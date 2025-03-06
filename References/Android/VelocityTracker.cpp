/*
 * Copyright (C) 2012 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#define LOG_TAG "VelocityTracker"

#include <android-base/logging.h>
#include <ftl/enum.h>
#include <inttypes.h>
#include <limits.h>
#include <math.h>
#include <array>
#include <optional>

#include <input/PrintTools.h>
#include <input/VelocityTracker.h>
#include <utils/BitSet.h>
#include <utils/Timers.h>

using std::literals::chrono_literals::operator""ms;

namespace android {

/**
 * Log debug messages about velocity tracking.
 * Enable this via "adb shell setprop log.tag.VelocityTrackerVelocity DEBUG" (requires restart)
 */
const bool DEBUG_VELOCITY =
        __android_log_is_loggable(ANDROID_LOG_DEBUG, LOG_TAG "Velocity", ANDROID_LOG_INFO);

/**
 * Log debug messages about the progress of the algorithm itself.
 * Enable this via "adb shell setprop log.tag.VelocityTrackerStrategy DEBUG" (requires restart)
 */
const bool DEBUG_STRATEGY =
        __android_log_is_loggable(ANDROID_LOG_DEBUG, LOG_TAG "Strategy", ANDROID_LOG_INFO);

/**
 * Log debug messages about the 'impulse' strategy.
 * Enable this via "adb shell setprop log.tag.VelocityTrackerImpulse DEBUG" (requires restart)
 */
const bool DEBUG_IMPULSE =
        __android_log_is_loggable(ANDROID_LOG_DEBUG, LOG_TAG "Impulse", ANDROID_LOG_INFO);

// Nanoseconds per milliseconds.
static const nsecs_t NANOS_PER_MS = 1000000;

// Seconds per nanosecond.
static const float SECONDS_PER_NANO = 1E-9;

// All axes supported for velocity tracking, mapped to their default strategies.
// Although other strategies are available for testing and comparison purposes,
// the default strategy is the one that applications will actually use.  Be very careful
// when adjusting the default strategy because it can dramatically affect
// (often in a bad way) the user experience.
static const std::map<int32_t, VelocityTracker::Strategy> DEFAULT_STRATEGY_BY_AXIS =
        {{AMOTION_EVENT_AXIS_X, VelocityTracker::Strategy::LSQ2},
         {AMOTION_EVENT_AXIS_Y, VelocityTracker::Strategy::LSQ2},
         {AMOTION_EVENT_AXIS_SCROLL, VelocityTracker::Strategy::IMPULSE}};

// Axes specifying location on a 2D plane (i.e. X and Y).
static const std::set<int32_t> PLANAR_AXES = {AMOTION_EVENT_AXIS_X, AMOTION_EVENT_AXIS_Y};

// Axes whose motion values are differential values (i.e. deltas).
static const std::set<int32_t> DIFFERENTIAL_AXES = {AMOTION_EVENT_AXIS_SCROLL};

// Threshold for determining that a pointer has stopped moving.
// Some input devices do not send ACTION_MOVE events in the case where a pointer has
// stopped.  We need to detect this case so that we can accurately predict the
// velocity after the pointer starts moving again.
static const std::chrono::duration ASSUME_POINTER_STOPPED_TIME = 40ms;

static std::string toString(std::chrono::nanoseconds t) {
    std::stringstream stream;
    stream.precision(1);
    stream << std::fixed << std::chrono::duration<float, std::milli>(t).count() << " ms";
    return stream.str();
}

static float vectorDot(const float* a, const float* b, uint32_t m) {
    float r = 0;
    for (size_t i = 0; i < m; i++) {
        r += *(a++) * *(b++);
    }
    return r;
}

static float vectorNorm(const float* a, uint32_t m) {
    float r = 0;
    for (size_t i = 0; i < m; i++) {
        float t = *(a++);
        r += t * t;
    }
    return sqrtf(r);
}

static std::string vectorToString(const float* a, uint32_t m) {
    std::string str;
    str += "[";
    for (size_t i = 0; i < m; i++) {
        if (i) {
            str += ",";
        }
        str += android::base::StringPrintf(" %f", *(a++));
    }
    str += " ]";
    return str;
}

static std::string vectorToString(const std::vector<float>& v) {
    return vectorToString(v.data(), v.size());
}

static std::string matrixToString(const float* a, uint32_t m, uint32_t n, bool rowMajor) {
    std::string str;
    str = "[";
    for (size_t i = 0; i < m; i++) {
        if (i) {
            str += ",";
        }
        str += " [";
        for (size_t j = 0; j < n; j++) {
            if (j) {
                str += ",";
            }
            str += android::base::StringPrintf(" %f", a[rowMajor ? i * n + j : j * m + i]);
        }
        str += " ]";
    }
    str += " ]";
    return str;
}


// --- VelocityTracker ---

VelocityTracker::VelocityTracker(const Strategy strategy)
      : mLastEventTime(0), mCurrentPointerIdBits(0), mOverrideStrategy(strategy) {}

bool VelocityTracker::isAxisSupported(int32_t axis) {
    return DEFAULT_STRATEGY_BY_AXIS.find(axis) != DEFAULT_STRATEGY_BY_AXIS.end();
}

void VelocityTracker::configureStrategy(int32_t axis) {
    const bool isDifferentialAxis = DIFFERENTIAL_AXES.find(axis) != DIFFERENTIAL_AXES.end();
    if (isDifferentialAxis || mOverrideStrategy == VelocityTracker::Strategy::DEFAULT) {
        // Do not allow overrides of strategies for differential axes, for now.
        mConfiguredStrategies[axis] = createStrategy(DEFAULT_STRATEGY_BY_AXIS.at(axis),
                                                     /*deltaValues=*/isDifferentialAxis);
    } else {
        mConfiguredStrategies[axis] = createStrategy(mOverrideStrategy, /*deltaValues=*/false);
    }
}

std::unique_ptr<VelocityTrackerStrategy> VelocityTracker::createStrategy(
        VelocityTracker::Strategy strategy, bool deltaValues) {
    switch (strategy) {
        case VelocityTracker::Strategy::IMPULSE:
            ALOGI_IF(DEBUG_STRATEGY, "Initializing impulse strategy");
            return std::make_unique<ImpulseVelocityTrackerStrategy>(deltaValues);

        case VelocityTracker::Strategy::LSQ1:
            return std::make_unique<LeastSquaresVelocityTrackerStrategy>(1);

        case VelocityTracker::Strategy::LSQ2:
            ALOGI_IF(DEBUG_STRATEGY && !DEBUG_IMPULSE, "Initializing lsq2 strategy");
            return std::make_unique<LeastSquaresVelocityTrackerStrategy>(2);

        case VelocityTracker::Strategy::LSQ3:
            return std::make_unique<LeastSquaresVelocityTrackerStrategy>(3);

        case VelocityTracker::Strategy::WLSQ2_DELTA:
            return std::make_unique<
                    LeastSquaresVelocityTrackerStrategy>(2,
                                                         LeastSquaresVelocityTrackerStrategy::
                                                                 Weighting::DELTA);
        case VelocityTracker::Strategy::WLSQ2_CENTRAL:
            return std::make_unique<
                    LeastSquaresVelocityTrackerStrategy>(2,
                                                         LeastSquaresVelocityTrackerStrategy::
                                                                 Weighting::CENTRAL);
        case VelocityTracker::Strategy::WLSQ2_RECENT:
            return std::make_unique<
                    LeastSquaresVelocityTrackerStrategy>(2,
                                                         LeastSquaresVelocityTrackerStrategy::
                                                                 Weighting::RECENT);

        case VelocityTracker::Strategy::INT1:
            return std::make_unique<IntegratingVelocityTrackerStrategy>(1);

        case VelocityTracker::Strategy::INT2:
            return std::make_unique<IntegratingVelocityTrackerStrategy>(2);

        case VelocityTracker::Strategy::LEGACY:
            return std::make_unique<LegacyVelocityTrackerStrategy>();

        default:
            break;
    }
    LOG(FATAL) << "Invalid strategy: " << ftl::enum_string(strategy)
               << ", deltaValues = " << deltaValues;

    return nullptr;
}

void VelocityTracker::clear() {
    mCurrentPointerIdBits.clear();
    mActivePointerId = std::nullopt;
    mConfiguredStrategies.clear();
}

void VelocityTracker::clearPointer(int32_t pointerId) {
    mCurrentPointerIdBits.clearBit(pointerId);

    if (mActivePointerId && *mActivePointerId == pointerId) {
        // The active pointer id is being removed. Mark it invalid and try to find a new one
        // from the remaining pointers.
        mActivePointerId = std::nullopt;
        if (!mCurrentPointerIdBits.isEmpty()) {
            mActivePointerId = mCurrentPointerIdBits.firstMarkedBit();
        }
    }

    for (const auto& [_, strategy] : mConfiguredStrategies) {
        strategy->clearPointer(pointerId);
    }
}

void VelocityTracker::addMovement(nsecs_t eventTime, int32_t pointerId, int32_t axis,
                                  float position) {
    if (pointerId < 0 || pointerId > MAX_POINTER_ID) {
        LOG(FATAL) << "Invalid pointer ID " << pointerId << " for axis "
                   << MotionEvent::getLabel(axis);
    }

    if (mCurrentPointerIdBits.hasBit(pointerId) &&
        std::chrono::nanoseconds(eventTime - mLastEventTime) > ASSUME_POINTER_STOPPED_TIME) {
        ALOGD_IF(DEBUG_VELOCITY, "VelocityTracker: stopped for %s, clearing state.",
                 toString(std::chrono::nanoseconds(eventTime - mLastEventTime)).c_str());

        // We have not received any movements for too long.  Assume that all pointers
        // have stopped.
        mConfiguredStrategies.clear();
    }
    mLastEventTime = eventTime;

    mCurrentPointerIdBits.markBit(pointerId);
    if (!mActivePointerId) {
        // Let this be the new active pointer if no active pointer is currently set
        mActivePointerId = pointerId;
    }

    if (mConfiguredStrategies.find(axis) == mConfiguredStrategies.end()) {
        configureStrategy(axis);
    }
    mConfiguredStrategies[axis]->addMovement(eventTime, pointerId, position);

    if (DEBUG_VELOCITY) {
        LOG(INFO) << "VelocityTracker: addMovement axis=" << MotionEvent::getLabel(axis)
                  << ", eventTime=" << eventTime << ", pointerId=" << pointerId
                  << ", activePointerId=" << toString(mActivePointerId) << ", position=" << position
                  << ", velocity=" << toString(getVelocity(axis, pointerId));
    }
}

void VelocityTracker::addMovement(const MotionEvent& event) {
    // Stores data about which axes to process based on the incoming motion event.
    std::set<int32_t> axesToProcess;
    int32_t actionMasked = event.getActionMasked();

    switch (actionMasked) {
        case AMOTION_EVENT_ACTION_DOWN:
        case AMOTION_EVENT_ACTION_HOVER_ENTER:
            // Clear all pointers on down before adding the new movement.
            clear();
            axesToProcess.insert(PLANAR_AXES.begin(), PLANAR_AXES.end());
            break;
        case AMOTION_EVENT_ACTION_POINTER_DOWN: {
            // Start a new movement trace for a pointer that just went down.
            // We do this on down instead of on up because the client may want to query the
            // final velocity for a pointer that just went up.
            clearPointer(event.getPointerId(event.getActionIndex()));
            axesToProcess.insert(PLANAR_AXES.begin(), PLANAR_AXES.end());
            break;
        }
        case AMOTION_EVENT_ACTION_MOVE:
        case AMOTION_EVENT_ACTION_HOVER_MOVE:
            axesToProcess.insert(PLANAR_AXES.begin(), PLANAR_AXES.end());
            break;
        case AMOTION_EVENT_ACTION_POINTER_UP:
            if (event.getFlags() & AMOTION_EVENT_FLAG_CANCELED) {
                clearPointer(event.getPointerId(event.getActionIndex()));
                return;
            }
            // Continue to ACTION_UP to ensure that the POINTER_STOPPED logic is triggered.
            [[fallthrough]];
        case AMOTION_EVENT_ACTION_UP: {
            std::chrono::nanoseconds delaySinceLastEvent(event.getEventTime() - mLastEventTime);
            if (delaySinceLastEvent > ASSUME_POINTER_STOPPED_TIME) {
                ALOGD_IF(DEBUG_VELOCITY,
                         "VelocityTracker: stopped for %s, clearing state upon pointer liftoff.",
                         toString(delaySinceLastEvent).c_str());
                // We have not received any movements for too long.  Assume that all pointers
                // have stopped.
                for (int32_t axis : PLANAR_AXES) {
                    mConfiguredStrategies.erase(axis);
                }
            }
            // These actions because they do not convey any new information about
            // pointer movement.  We also want to preserve the last known velocity of the pointers.
            // Note that ACTION_UP and ACTION_POINTER_UP always report the last known position
            // of the pointers that went up.  ACTION_POINTER_UP does include the new position of
            // pointers that remained down but we will also receive an ACTION_MOVE with this
            // information if any of them actually moved.  Since we don't know how many pointers
            // will be going up at once it makes sense to just wait for the following ACTION_MOVE
            // before adding the movement.
            return;
        }
        case AMOTION_EVENT_ACTION_SCROLL:
            axesToProcess.insert(AMOTION_EVENT_AXIS_SCROLL);
            break;
        case AMOTION_EVENT_ACTION_CANCEL: {
            clear();
            return;
        }

        default:
            // Ignore all other actions.
            return;
    }

    const size_t historySize = event.getHistorySize();
    for (size_t h = 0; h <= historySize; h++) {
        const nsecs_t eventTime = event.getHistoricalEventTime(h);
        for (size_t i = 0; i < event.getPointerCount(); i++) {
            if (event.isResampled(i, h)) {
                continue; // skip resampled samples
            }
            const int32_t pointerId = event.getPointerId(i);
            for (int32_t axis : axesToProcess) {
                const float position = event.getHistoricalAxisValue(axis, i, h);
                addMovement(eventTime, pointerId, axis, position);
            }
        }
    }
}

std::optional<float> VelocityTracker::getVelocity(int32_t axis, int32_t pointerId) const {
    const auto& it = mConfiguredStrategies.find(axis);
    if (it != mConfiguredStrategies.end()) {
        return it->second->getVelocity(pointerId);
    }
    return {};
}

VelocityTracker::ComputedVelocity VelocityTracker::getComputedVelocity(int32_t units,
                                                                       float maxVelocity) {
    ComputedVelocity computedVelocity;
    for (const auto& [axis, _] : mConfiguredStrategies) {
        BitSet32 copyIdBits = BitSet32(mCurrentPointerIdBits);
        while (!copyIdBits.isEmpty()) {
            uint32_t id = copyIdBits.clearFirstMarkedBit();
            std::optional<float> velocity = getVelocity(axis, id);
            if (velocity) {
                float adjustedVelocity =
                        std::clamp(*velocity * units / 1000, -maxVelocity, maxVelocity);
                computedVelocity.addVelocity(axis, id, adjustedVelocity);
            }
        }
    }
    return computedVelocity;
}

AccumulatingVelocityTrackerStrategy::AccumulatingVelocityTrackerStrategy(
        nsecs_t horizonNanos, bool maintainHorizonDuringAdd)
      : mHorizonNanos(horizonNanos), mMaintainHorizonDuringAdd(maintainHorizonDuringAdd) {}

void AccumulatingVelocityTrackerStrategy::clearPointer(int32_t pointerId) {
    mMovements.erase(pointerId);
}

void AccumulatingVelocityTrackerStrategy::addMovement(nsecs_t eventTime, int32_t pointerId,
                                                      float position) {
    auto [ringBufferIt, _] = mMovements.try_emplace(pointerId, HISTORY_SIZE);
    RingBuffer<Movement>& movements = ringBufferIt->second;
    const size_t size = movements.size();

    if (size != 0 && movements[size - 1].eventTime == eventTime) {
        // When ACTION_POINTER_DOWN happens, we will first receive ACTION_MOVE with the coordinates
        // of the existing pointers, and then ACTION_POINTER_DOWN with the coordinates that include
        // the new pointer. If the eventtimes for both events are identical, just update the data
        // for this time (i.e. pop out the last element, and insert the updated movement).
        // We only compare against the last value, as it is likely that addMovement is called
        // in chronological order as events occur.
        movements.popBack();
    }

    movements.pushBack({eventTime, position});

    // Clear movements that do not fall within `mHorizonNanos` of the latest movement.
    // Note that, if in the future we decide to use more movements (i.e. increase HISTORY_SIZE),
    // we can consider making this step binary-search based, which will give us some improvement.
    if (mMaintainHorizonDuringAdd) {
        while (eventTime - movements[0].eventTime > mHorizonNanos) {
            movements.popFront();
        }
    }
}

// --- LeastSquaresVelocityTrackerStrategy ---

LeastSquaresVelocityTrackerStrategy::LeastSquaresVelocityTrackerStrategy(uint32_t degree,
                                                                         Weighting weighting)
      : AccumulatingVelocityTrackerStrategy(HORIZON /*horizonNanos*/,
                                            true /*maintainHorizonDuringAdd*/),
        mDegree(degree),
        mWeighting(weighting) {}

LeastSquaresVelocityTrackerStrategy::~LeastSquaresVelocityTrackerStrategy() {}

/**
 * Solves a linear least squares problem to obtain a N degree polynomial that fits
 * the specified input data as nearly as possible.
 *
 * Returns true if a solution is found, false otherwise.
 *
 * The input consists of two vectors of data points X and Y with indices 0..m-1
 * along with a weight vector W of the same size.
 *
 * The output is a vector B with indices 0..n that describes a polynomial
 * that fits the data, such the sum of W[i] * W[i] * abs(Y[i] - (B[0] + B[1] X[i]
 * + B[2] X[i]^2 ... B[n] X[i]^n)) for all i between 0 and m-1 is minimized.
 *
 * Accordingly, the weight vector W should be initialized by the caller with the
 * reciprocal square root of the variance of the error in each input data point.
 * In other words, an ideal choice for W would be W[i] = 1 / var(Y[i]) = 1 / stddev(Y[i]).
 * The weights express the relative importance of each data point.  If the weights are
 * all 1, then the data points are considered to be of equal importance when fitting
 * the polynomial.  It is a good idea to choose weights that diminish the importance
 * of data points that may have higher than usual error margins.
 *
 * Errors among data points are assumed to be independent.  W is represented here
 * as a vector although in the literature it is typically taken to be a diagonal matrix.
 *
 * That is to say, the function that generated the input data can be approximated
 * by y(x) ~= B[0] + B[1] x + B[2] x^2 + ... + B[n] x^n.
 *
 * The coefficient of determination (R^2) is also returned to describe the goodness
 * of fit of the model for the given data.  It is a value between 0 and 1, where 1
 * indicates perfect correspondence.
 *
 * This function first expands the X vector to a m by n matrix A such that
 * A[i][0] = 1, A[i][1] = X[i], A[i][2] = X[i]^2, ..., A[i][n] = X[i]^n, then
 * multiplies it by w[i]./
 *
 * Then it calculates the QR decomposition of A yielding an m by m orthonormal matrix Q
 * and an m by n upper triangular matrix R.  Because R is upper triangular (lower
 * part is all zeroes), we can simplify the decomposition into an m by n matrix
 * Q1 and a n by n matrix R1 such that A = Q1 R1.
 *
 * Finally we solve the system of linear equations given by R1 B = (Qtranspose W Y)
 * to find B.
 *
 * For efficiency, we lay out A and Q column-wise in memory because we frequently
 * operate on the column vectors.  Conversely, we lay out R row-wise.
 *
 * http://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares
 * http://en.wikipedia.org/wiki/Gram-Schmidt
 */
static std::optional<float> solveLeastSquares(const std::vector<float>& x,
                                              const std::vector<float>& y,
                                              const std::vector<float>& w, uint32_t n) {
    const size_t m = x.size();

    ALOGD_IF(DEBUG_STRATEGY, "solveLeastSquares: m=%d, n=%d, x=%s, y=%s, w=%s", int(m), int(n),
             vectorToString(x).c_str(), vectorToString(y).c_str(), vectorToString(w).c_str());

    LOG_ALWAYS_FATAL_IF(m != y.size() || m != w.size(), "Mismatched vector sizes");

    // Expand the X vector to a matrix A, pre-multiplied by the weights.
    float a[n][m]; // column-major order
    for (uint32_t h = 0; h < m; h++) {
        a[0][h] = w[h];
        for (uint32_t i = 1; i < n; i++) {
            a[i][h] = a[i - 1][h] * x[h];
        }
    }

    ALOGD_IF(DEBUG_STRATEGY, "  - a=%s",
             matrixToString(&a[0][0], m, n, /*rowMajor=*/false).c_str());

    // Apply the Gram-Schmidt process to A to obtain its QR decomposition.
    float q[n][m]; // orthonormal basis, column-major order
    float r[n][n]; // upper triangular matrix, row-major order
    for (uint32_t j = 0; j < n; j++) {
        for (uint32_t h = 0; h < m; h++) {
            q[j][h] = a[j][h];
        }
        for (uint32_t i = 0; i < j; i++) {
            float dot = vectorDot(&q[j][0], &q[i][0], m);
            for (uint32_t h = 0; h < m; h++) {
                q[j][h] -= dot * q[i][h];
            }
        }

        float norm = vectorNorm(&q[j][0], m);
        if (norm < 0.000001f) {
            // vectors are linearly dependent or zero so no solution
            ALOGD_IF(DEBUG_STRATEGY, "  - no solution, norm=%f", norm);
            return {};
        }

        float invNorm = 1.0f / norm;
        for (uint32_t h = 0; h < m; h++) {
            q[j][h] *= invNorm;
        }
        for (uint32_t i = 0; i < n; i++) {
            r[j][i] = i < j ? 0 : vectorDot(&q[j][0], &a[i][0], m);
        }
    }
    if (DEBUG_STRATEGY) {
        ALOGD("  - q=%s", matrixToString(&q[0][0], m, n, /*rowMajor=*/false).c_str());
        ALOGD("  - r=%s", matrixToString(&r[0][0], n, n, /*rowMajor=*/true).c_str());

        // calculate QR, if we factored A correctly then QR should equal A
        float qr[n][m];
        for (uint32_t h = 0; h < m; h++) {
            for (uint32_t i = 0; i < n; i++) {
                qr[i][h] = 0;
                for (uint32_t j = 0; j < n; j++) {
                    qr[i][h] += q[j][h] * r[j][i];
                }
            }
        }
        ALOGD("  - qr=%s", matrixToString(&qr[0][0], m, n, /*rowMajor=*/false).c_str());
    }

    // Solve R B = Qt W Y to find B.  This is easy because R is upper triangular.
    // We just work from bottom-right to top-left calculating B's coefficients.
    float wy[m];
    for (uint32_t h = 0; h < m; h++) {
        wy[h] = y[h] * w[h];
    }
    std::array<float, VelocityTracker::MAX_DEGREE + 1> outB;
    for (uint32_t i = n; i != 0; ) {
        i--;
        outB[i] = vectorDot(&q[i][0], wy, m);
        for (uint32_t j = n - 1; j > i; j--) {
            outB[i] -= r[i][j] * outB[j];
        }
        outB[i] /= r[i][i];
    }

    ALOGD_IF(DEBUG_STRATEGY, "  - b=%s", vectorToString(outB.data(), n).c_str());

    // Calculate the coefficient of determination as 1 - (SSerr / SStot) where
    // SSerr is the residual sum of squares (variance of the error),
    // and SStot is the total sum of squares (variance of the data) where each
    // has been weighted.
    float ymean = 0;
    for (uint32_t h = 0; h < m; h++) {
        ymean += y[h];
    }
    ymean /= m;

    if (DEBUG_STRATEGY) {
        float sserr = 0;
        float sstot = 0;
        for (uint32_t h = 0; h < m; h++) {
            float err = y[h] - outB[0];
            float term = 1;
            for (uint32_t i = 1; i < n; i++) {
                term *= x[h];
                err -= term * outB[i];
            }
            sserr += w[h] * w[h] * err * err;
            float var = y[h] - ymean;
            sstot += w[h] * w[h] * var * var;
        }
        ALOGD("  - sserr=%f", sserr);
        ALOGD("  - sstot=%f", sstot);
    }

    return outB[1];
}

/*
 * Optimized unweighted second-order least squares fit. About 2x speed improvement compared to
 * the default implementation
 */
std::optional<float> LeastSquaresVelocityTrackerStrategy::solveUnweightedLeastSquaresDeg2(
        const RingBuffer<Movement>& movements) const {
    // Solving y = a*x^2 + b*x + c, where
    //      - "x" is age (i.e. duration since latest movement) of the movemnets
    //      - "y" is positions of the movements.
    float sxi = 0, sxiyi = 0, syi = 0, sxi2 = 0, sxi3 = 0, sxi2yi = 0, sxi4 = 0;

    const size_t count = movements.size();
    const Movement& newestMovement = movements[count - 1];
    for (size_t i = 0; i < count; i++) {
        const Movement& movement = movements[i];
        nsecs_t age = newestMovement.eventTime - movement.eventTime;
        float xi = -age * SECONDS_PER_NANO;
        float yi = movement.position;

        float xi2 = xi*xi;
        float xi3 = xi2*xi;
        float xi4 = xi3*xi;
        float xiyi = xi*yi;
        float xi2yi = xi2*yi;

        sxi += xi;
        sxi2 += xi2;
        sxiyi += xiyi;
        sxi2yi += xi2yi;
        syi += yi;
        sxi3 += xi3;
        sxi4 += xi4;
    }

    float Sxx = sxi2 - sxi*sxi / count;
    float Sxy = sxiyi - sxi*syi / count;
    float Sxx2 = sxi3 - sxi*sxi2 / count;
    float Sx2y = sxi2yi - sxi2*syi / count;
    float Sx2x2 = sxi4 - sxi2*sxi2 / count;

    float denominator = Sxx*Sx2x2 - Sxx2*Sxx2;
    if (denominator == 0) {
        ALOGW("division by 0 when computing velocity, Sxx=%f, Sx2x2=%f, Sxx2=%f", Sxx, Sx2x2, Sxx2);
        return std::nullopt;
    }

    return (Sxy * Sx2x2 - Sx2y * Sxx2) / denominator;
}

std::optional<float> LeastSquaresVelocityTrackerStrategy::getVelocity(int32_t pointerId) const {
    const auto movementIt = mMovements.find(pointerId);
    if (movementIt == mMovements.end()) {
        return std::nullopt; // no data
    }

    const RingBuffer<Movement>& movements = movementIt->second;
    const size_t size = movements.size();
    if (size == 0) {
        return std::nullopt; // no data
    }

    uint32_t degree = mDegree;
    if (degree > size - 1) {
        degree = size - 1;
    }

    if (degree <= 0) {
        return std::nullopt;
    }

    if (degree == 2 && mWeighting == Weighting::NONE) {
        // Optimize unweighted, quadratic polynomial fit
        return solveUnweightedLeastSquaresDeg2(movements);
    }

    // Iterate over movement samples in reverse time order and collect samples.
    std::vector<float> positions;
    std::vector<float> w;
    std::vector<float> time;

    const Movement& newestMovement = movements[size - 1];
    for (ssize_t i = size - 1; i >= 0; i--) {
        const Movement& movement = movements[i];
        nsecs_t age = newestMovement.eventTime - movement.eventTime;
        positions.push_back(movement.position);
        w.push_back(chooseWeight(pointerId, i));
        time.push_back(-age * 0.000000001f);
    }

    // General case for an Nth degree polynomial fit
    return solveLeastSquares(time, positions, w, degree + 1);
}

float LeastSquaresVelocityTrackerStrategy::chooseWeight(int32_t pointerId, uint32_t index) const {
    const RingBuffer<Movement>& movements = mMovements.at(pointerId);
    const size_t size = movements.size();
    switch (mWeighting) {
        case Weighting::DELTA: {
            // Weight points based on how much time elapsed between them and the next
            // point so that points that "cover" a shorter time span are weighed less.
            //   delta  0ms: 0.5
            //   delta 10ms: 1.0
            if (index == size - 1) {
                return 1.0f;
            }
            float deltaMillis =
                    (movements[index + 1].eventTime - movements[index].eventTime) * 0.000001f;
            if (deltaMillis < 0) {
                return 0.5f;
            }
            if (deltaMillis < 10) {
                return 0.5f + deltaMillis * 0.05;
            }
            return 1.0f;
        }

        case Weighting::CENTRAL: {
            // Weight points based on their age, weighing very recent and very old points less.
            //   age  0ms: 0.5
            //   age 10ms: 1.0
            //   age 50ms: 1.0
            //   age 60ms: 0.5
            float ageMillis =
                    (movements[size - 1].eventTime - movements[index].eventTime) * 0.000001f;
            if (ageMillis < 0) {
                return 0.5f;
            }
            if (ageMillis < 10) {
                return 0.5f + ageMillis * 0.05;
            }
            if (ageMillis < 50) {
                return 1.0f;
            }
            if (ageMillis < 60) {
                return 0.5f + (60 - ageMillis) * 0.05;
            }
            return 0.5f;
        }

        case Weighting::RECENT: {
            // Weight points based on their age, weighing older points less.
            //   age   0ms: 1.0
            //   age  50ms: 1.0
            //   age 100ms: 0.5
            float ageMillis =
                    (movements[size - 1].eventTime - movements[index].eventTime) * 0.000001f;
            if (ageMillis < 50) {
                return 1.0f;
            }
            if (ageMillis < 100) {
                return 0.5f + (100 - ageMillis) * 0.01f;
            }
            return 0.5f;
        }

        case Weighting::NONE:
            return 1.0f;
    }
}

// --- IntegratingVelocityTrackerStrategy ---

IntegratingVelocityTrackerStrategy::IntegratingVelocityTrackerStrategy(uint32_t degree) :
        mDegree(degree) {
}

IntegratingVelocityTrackerStrategy::~IntegratingVelocityTrackerStrategy() {
}

void IntegratingVelocityTrackerStrategy::clearPointer(int32_t pointerId) {
    mPointerIdBits.clearBit(pointerId);
}

void IntegratingVelocityTrackerStrategy::addMovement(nsecs_t eventTime, int32_t pointerId,
                                                     float position) {
    State& state = mPointerState[pointerId];
    if (mPointerIdBits.hasBit(pointerId)) {
        updateState(state, eventTime, position);
    } else {
        initState(state, eventTime, position);
    }

    mPointerIdBits.markBit(pointerId);
}

std::optional<float> IntegratingVelocityTrackerStrategy::getVelocity(int32_t pointerId) const {
    if (mPointerIdBits.hasBit(pointerId)) {
        return mPointerState[pointerId].vel;
    }

    return std::nullopt;
}

void IntegratingVelocityTrackerStrategy::initState(State& state, nsecs_t eventTime,
                                                   float pos) const {
    state.updateTime = eventTime;
    state.degree = 0;

    state.pos = pos;
    state.accel = 0;
    state.vel = 0;
}

void IntegratingVelocityTrackerStrategy::updateState(State& state, nsecs_t eventTime,
                                                     float pos) const {
    const nsecs_t MIN_TIME_DELTA = 2 * NANOS_PER_MS;
    const float FILTER_TIME_CONSTANT = 0.010f; // 10 milliseconds

    if (eventTime <= state.updateTime + MIN_TIME_DELTA) {
        return;
    }

    float dt = (eventTime - state.updateTime) * 0.000000001f;
    state.updateTime = eventTime;

    float vel = (pos - state.pos) / dt;
    if (state.degree == 0) {
        state.vel = vel;
        state.degree = 1;
    } else {
        float alpha = dt / (FILTER_TIME_CONSTANT + dt);
        if (mDegree == 1) {
            state.vel += (vel - state.vel) * alpha;
        } else {
            float accel = (vel - state.vel) / dt;
            if (state.degree == 1) {
                state.accel = accel;
                state.degree = 2;
            } else {
                state.accel += (accel - state.accel) * alpha;
            }
            state.vel += (state.accel * dt) * alpha;
        }
    }
    state.pos = pos;
}

// --- LegacyVelocityTrackerStrategy ---

LegacyVelocityTrackerStrategy::LegacyVelocityTrackerStrategy()
      : AccumulatingVelocityTrackerStrategy(HORIZON /*horizonNanos*/,
                                            false /*maintainHorizonDuringAdd*/) {}

LegacyVelocityTrackerStrategy::~LegacyVelocityTrackerStrategy() {
}

std::optional<float> LegacyVelocityTrackerStrategy::getVelocity(int32_t pointerId) const {
    const auto movementIt = mMovements.find(pointerId);
    if (movementIt == mMovements.end()) {
        return std::nullopt; // no data
    }

    const RingBuffer<Movement>& movements = movementIt->second;
    const size_t size = movements.size();
    if (size == 0) {
        return std::nullopt; // no data
    }

    const Movement& newestMovement = movements[size - 1];

    // Find the oldest sample that contains the pointer and that is not older than HORIZON.
    nsecs_t minTime = newestMovement.eventTime - HORIZON;
    uint32_t oldestIndex = size - 1;
    for (ssize_t i = size - 1; i >= 0; i--) {
        const Movement& nextOldestMovement = movements[i];
        if (nextOldestMovement.eventTime < minTime) {
            break;
        }
        oldestIndex = i;
    }

    // Calculate an exponentially weighted moving average of the velocity estimate
    // at different points in time measured relative to the oldest sample.
    // This is essentially an IIR filter.  Newer samples are weighted more heavily
    // than older samples.  Samples at equal time points are weighted more or less
    // equally.
    //
    // One tricky problem is that the sample data may be poorly conditioned.
    // Sometimes samples arrive very close together in time which can cause us to
    // overestimate the velocity at that time point.  Most samples might be measured
    // 16ms apart but some consecutive samples could be only 0.5sm apart because
    // the hardware or driver reports them irregularly or in bursts.
    float accumV = 0;
    uint32_t samplesUsed = 0;
    const Movement& oldestMovement = movements[oldestIndex];
    float oldestPosition = oldestMovement.position;
    nsecs_t lastDuration = 0;

    for (size_t i = oldestIndex; i < size; i++) {
        const Movement& movement = movements[i];
        nsecs_t duration = movement.eventTime - oldestMovement.eventTime;

        // If the duration between samples is small, we may significantly overestimate
        // the velocity.  Consequently, we impose a minimum duration constraint on the
        // samples that we include in the calculation.
        if (duration >= MIN_DURATION) {
            float position = movement.position;
            float scale = 1000000000.0f / duration; // one over time delta in seconds
            float v = (position - oldestPosition) * scale;
            accumV = (accumV * lastDuration + v * duration) / (duration + lastDuration);
            lastDuration = duration;
            samplesUsed += 1;
        }
    }

    if (samplesUsed) {
        return accumV;
    }
    return std::nullopt;
}

// --- ImpulseVelocityTrackerStrategy ---

ImpulseVelocityTrackerStrategy::ImpulseVelocityTrackerStrategy(bool deltaValues)
      : AccumulatingVelocityTrackerStrategy(HORIZON /*horizonNanos*/,
                                            true /*maintainHorizonDuringAdd*/),
        mDeltaValues(deltaValues) {}

ImpulseVelocityTrackerStrategy::~ImpulseVelocityTrackerStrategy() {
}

/**
 * Calculate the total impulse provided to the screen and the resulting velocity.
 *
 * The touchscreen is modeled as a physical object.
 * Initial condition is discussed below, but for now suppose that v(t=0) = 0
 *
 * The kinetic energy of the object at the release is E=0.5*m*v^2
 * Then vfinal = sqrt(2E/m). The goal is to calculate E.
 *
 * The kinetic energy at the release is equal to the total work done on the object by the finger.
 * The total work W is the sum of all dW along the path.
 *
 * dW = F*dx, where dx is the piece of path traveled.
 * Force is change of momentum over time, F = dp/dt = m dv/dt.
 * Then substituting:
 * dW = m (dv/dt) * dx = m * v * dv
 *
 * Summing along the path, we get:
 * W = sum(dW) = sum(m * v * dv) = m * sum(v * dv)
 * Since the mass stays constant, the equation for final velocity is:
 * vfinal = sqrt(2*sum(v * dv))
 *
 * Here,
 * dv : change of velocity = (v[i+1]-v[i])
 * dx : change of distance = (x[i+1]-x[i])
 * dt : change of time = (t[i+1]-t[i])
 * v : instantaneous velocity = dx/dt
 *
 * The final formula is:
 * vfinal = sqrt(2) * sqrt(sum((v[i]-v[i-1])*|v[i]|)) for all i
 * The absolute value is needed to properly account for the sign. If the velocity over a
 * particular segment descreases, then this indicates braking, which means that negative
 * work was done. So for two positive, but decreasing, velocities, this contribution would be
 * negative and will cause a smaller final velocity.
 *
 * Initial condition
 * There are two ways to deal with initial condition:
 * 1) Assume that v(0) = 0, which would mean that the screen is initially at rest.
 * This is not entirely accurate. We are only taking the past X ms of touch data, where X is
 * currently equal to 100. However, a touch event that created a fling probably lasted for longer
 * than that, which would mean that the user has already been interacting with the touchscreen
 * and it has probably already been moving.
 * 2) Assume that the touchscreen has already been moving at a certain velocity, calculate this
 * initial velocity and the equivalent energy, and start with this initial energy.
 * Consider an example where we have the following data, consisting of 3 points:
 *                 time: t0, t1, t2
 *                 x   : x0, x1, x2
 *                 v   : 0 , v1, v2
 * Here is what will happen in each of these scenarios:
 * 1) By directly applying the formula above with the v(0) = 0 boundary condition, we will get
 * vfinal = sqrt(2*(|v1|*(v1-v0) + |v2|*(v2-v1))). This can be simplified since v0=0
 * vfinal = sqrt(2*(|v1|*v1 + |v2|*(v2-v1))) = sqrt(2*(v1^2 + |v2|*(v2 - v1)))
 * since velocity is a real number
 * 2) If we treat the screen as already moving, then it must already have an energy (per mass)
 * equal to 1/2*v1^2. Then the initial energy should be 1/2*v1*2, and only the second segment
 * will contribute to the total kinetic energy (since we can effectively consider that v0=v1).
 * This will give the following expression for the final velocity:
 * vfinal = sqrt(2*(1/2*v1^2 + |v2|*(v2-v1)))
 * This analysis can be generalized to an arbitrary number of samples.
 *
 *
 * Comparing the two equations above, we see that the only mathematical difference
 * is the factor of 1/2 in front of the first velocity term.
 * This boundary condition would allow for the "proper" calculation of the case when all of the
 * samples are equally spaced in time and distance, which should suggest a constant velocity.
 *
 * Note that approach 2) is sensitive to the proper ordering of the data in time, since
 * the boundary condition must be applied to the oldest sample to be accurate.
 */
static float kineticEnergyToVelocity(float work) {
    static constexpr float sqrt2 = 1.41421356237;
    return (work < 0 ? -1.0 : 1.0) * sqrtf(fabsf(work)) * sqrt2;
}

std::optional<float> ImpulseVelocityTrackerStrategy::getVelocity(int32_t pointerId) const {
    const auto movementIt = mMovements.find(pointerId);
    if (movementIt == mMovements.end()) {
        return std::nullopt; // no data
    }

    const RingBuffer<Movement>& movements = movementIt->second;
    const size_t size = movements.size();
    if (size == 0) {
        return std::nullopt; // no data
    }

    float work = 0;
    for (size_t i = 0; i < size - 1; i++) {
        const Movement& mvt = movements[i];
        const Movement& nextMvt = movements[i + 1];

        float vprev = kineticEnergyToVelocity(work);
        float delta = mDeltaValues ? nextMvt.position : nextMvt.position - mvt.position;
        float vcurr = delta / (SECONDS_PER_NANO * (nextMvt.eventTime - mvt.eventTime));
        work += (vcurr - vprev) * fabsf(vcurr);

        if (i == 0) {
            work *= 0.5; // initial condition, case 2) above
        }
    }

    const float velocity = kineticEnergyToVelocity(work);
    ALOGD_IF(DEBUG_STRATEGY, "velocity: %.1f", velocity);

    if (DEBUG_IMPULSE) {
        // TODO(b/134179997): delete this block once the switch to 'impulse' is complete.
        // Calculate the lsq2 velocity for the same inputs to allow runtime comparisons.
        // X axis chosen arbitrarily for velocity comparisons.
        VelocityTracker lsq2(VelocityTracker::Strategy::LSQ2);
        for (size_t i = 0; i < size; i++) {
            const Movement& mvt = movements[i];
            lsq2.addMovement(mvt.eventTime, pointerId, AMOTION_EVENT_AXIS_X, mvt.position);
        }
        std::optional<float> v = lsq2.getVelocity(AMOTION_EVENT_AXIS_X, pointerId);
        if (v) {
            ALOGD("lsq2 velocity: %.1f", *v);
        } else {
            ALOGD("lsq2 velocity: could not compute velocity");
        }
    }
    return velocity;
}

} // namespace android
