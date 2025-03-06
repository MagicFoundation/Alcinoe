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

#pragma once

#include <android/os/IInputConstants.h>
#include <input/Input.h>
#include <input/RingBuffer.h>
#include <utils/BitSet.h>
#include <utils/Timers.h>
#include <map>
#include <set>

namespace android {

class VelocityTrackerStrategy;

/*
 * Calculates the velocity of pointer movements over time.
 */
class VelocityTracker {
public:
    static const size_t MAX_DEGREE = 4;

    enum class Strategy : int32_t {
        DEFAULT = android::os::IInputConstants::VELOCITY_TRACKER_STRATEGY_DEFAULT,
        IMPULSE = android::os::IInputConstants::VELOCITY_TRACKER_STRATEGY_IMPULSE,
        LSQ1 = android::os::IInputConstants::VELOCITY_TRACKER_STRATEGY_LSQ1,
        LSQ2 = android::os::IInputConstants::VELOCITY_TRACKER_STRATEGY_LSQ2,
        LSQ3 = android::os::IInputConstants::VELOCITY_TRACKER_STRATEGY_LSQ3,
        WLSQ2_DELTA = android::os::IInputConstants::VELOCITY_TRACKER_STRATEGY_WLSQ2_DELTA,
        WLSQ2_CENTRAL = android::os::IInputConstants::VELOCITY_TRACKER_STRATEGY_WLSQ2_CENTRAL,
        WLSQ2_RECENT = android::os::IInputConstants::VELOCITY_TRACKER_STRATEGY_WLSQ2_RECENT,
        INT1 = android::os::IInputConstants::VELOCITY_TRACKER_STRATEGY_INT1,
        INT2 = android::os::IInputConstants::VELOCITY_TRACKER_STRATEGY_INT2,
        LEGACY = android::os::IInputConstants::VELOCITY_TRACKER_STRATEGY_LEGACY,
        MIN = IMPULSE,
        MAX = LEGACY,
        ftl_last = LEGACY,
    };

    /*
     * Contains all available velocity data from a VelocityTracker.
     */
    struct ComputedVelocity {
        inline std::optional<float> getVelocity(int32_t axis, int32_t id) const {
            const auto& axisVelocities = mVelocities.find(axis);
            if (axisVelocities == mVelocities.end()) {
                return {};
            }

            const auto& axisIdVelocity = axisVelocities->second.find(id);
            if (axisIdVelocity == axisVelocities->second.end()) {
                return {};
            }

            return axisIdVelocity->second;
        }

        inline void addVelocity(int32_t axis, int32_t id, float velocity) {
            mVelocities[axis][id] = velocity;
        }

    private:
        std::map<int32_t /*axis*/, std::map<int32_t /*pointerId*/, float /*velocity*/>> mVelocities;
    };

    // Creates a velocity tracker using the specified strategy for each supported axis.
    // If strategy is not provided, uses the default strategy for the platform.
    // TODO(b/32830165): support axis-specific strategies.
    VelocityTracker(const Strategy strategy = Strategy::DEFAULT);

    /** Return true if the axis is supported for velocity tracking, false otherwise. */
    static bool isAxisSupported(int32_t axis);

    // Resets the velocity tracker state.
    void clear();

    // Resets the velocity tracker state for a specific pointer.
    // Call this method when some pointers have changed and may be reusing
    // an id that was assigned to a different pointer earlier.
    void clearPointer(int32_t pointerId);

    // Adds movement information for a pointer for a specific axis
    void addMovement(nsecs_t eventTime, int32_t pointerId, int32_t axis, float position);

    // Adds movement information for all pointers in a MotionEvent, including historical samples.
    void addMovement(const MotionEvent& event);

    // Returns the velocity of the specified pointer id and axis in position units per second.
    // Returns empty optional if there is insufficient movement information for the pointer, or if
    // the given axis is not supported for velocity tracking.
    std::optional<float> getVelocity(int32_t axis, int32_t pointerId) const;

    // Returns a ComputedVelocity instance with all available velocity data, using the given units
    // (reference: units == 1 means "per millisecond"), and clamping each velocity between
    // [-maxVelocity, maxVelocity], inclusive.
    ComputedVelocity getComputedVelocity(int32_t units, float maxVelocity);

    // Gets the active pointer id, or -1 if none.
    inline int32_t getActivePointerId() const { return mActivePointerId.value_or(-1); }

private:
    nsecs_t mLastEventTime;
    BitSet32 mCurrentPointerIdBits;
    std::optional<int32_t> mActivePointerId;

    // An override strategy passed in the constructor to be used for all axes.
    // This strategy will apply to all axes, unless the default strategy is specified here.
    // When default strategy is specified, then each axis will use a potentially different strategy
    // based on a hardcoded mapping.
    const Strategy mOverrideStrategy;
    // Maps axes to their respective VelocityTrackerStrategy instances.
    // Note that, only axes that have had MotionEvents (and not all supported axes) will be here.
    std::map<int32_t /*axis*/, std::unique_ptr<VelocityTrackerStrategy>> mConfiguredStrategies;

    void configureStrategy(int32_t axis);

    // Generates a VelocityTrackerStrategy instance for the given Strategy type.
    // The `deltaValues` parameter indicates whether or not the created strategy should treat motion
    // values as deltas (and not as absolute values). This the parameter is applicable only for
    // strategies that support differential axes.
    static std::unique_ptr<VelocityTrackerStrategy> createStrategy(const Strategy strategy,
                                                                   bool deltaValues);
};


/*
 * Implements a particular velocity tracker algorithm.
 */
class VelocityTrackerStrategy {
protected:
    VelocityTrackerStrategy() { }

public:
    virtual ~VelocityTrackerStrategy() { }

    virtual void clearPointer(int32_t pointerId) = 0;
    virtual void addMovement(nsecs_t eventTime, int32_t pointerId, float position) = 0;
    virtual std::optional<float> getVelocity(int32_t pointerId) const = 0;
};

/**
 * A `VelocityTrackerStrategy` that accumulates added data points and processes the accumulated data
 * points when getting velocity.
 */
class AccumulatingVelocityTrackerStrategy : public VelocityTrackerStrategy {
public:
    AccumulatingVelocityTrackerStrategy(nsecs_t horizonNanos, bool maintainHorizonDuringAdd);

    void addMovement(nsecs_t eventTime, int32_t pointerId, float position) override;
    void clearPointer(int32_t pointerId) override;

protected:
    struct Movement {
        nsecs_t eventTime;
        float position;
    };

    // Number of samples to keep.
    // If different strategies would like to maintain different history size, we can make this a
    // protected const field.
    static constexpr uint32_t HISTORY_SIZE = 20;

    /**
     * Duration, in nanoseconds, since the latest movement where a movement may be considered for
     * velocity calculation.
     */
    const nsecs_t mHorizonNanos;
    /**
     * If true, data points outside of horizon (see `mHorizonNanos`) will be cleared after each
     * addition of a new movement.
     */
    const bool mMaintainHorizonDuringAdd;
    std::map<int32_t /*pointerId*/, RingBuffer<Movement>> mMovements;
};

/*
 * Velocity tracker algorithm based on least-squares linear regression.
 */
class LeastSquaresVelocityTrackerStrategy : public AccumulatingVelocityTrackerStrategy {
public:
    enum class Weighting {
        // No weights applied.  All data points are equally reliable.
        NONE,

        // Weight by time delta.  Data points clustered together are weighted less.
        DELTA,

        // Weight such that points within a certain horizon are weighed more than those
        // outside of that horizon.
        CENTRAL,

        // Weight such that points older than a certain amount are weighed less.
        RECENT,
    };

    // Degree must be no greater than VelocityTracker::MAX_DEGREE.
    LeastSquaresVelocityTrackerStrategy(uint32_t degree, Weighting weighting = Weighting::NONE);
    ~LeastSquaresVelocityTrackerStrategy() override;

    std::optional<float> getVelocity(int32_t pointerId) const override;

private:
    // Sample horizon.
    // We don't use too much history by default since we want to react to quick
    // changes in direction.
    static const nsecs_t HORIZON = 100 * 1000000; // 100 ms

    float chooseWeight(int32_t pointerId, uint32_t index) const;
    /**
     * An optimized least-squares solver for degree 2 and no weight (i.e. `Weighting.NONE`).
     * The provided container of movements shall NOT be empty, and shall have the movements in
     * chronological order.
     */
    std::optional<float> solveUnweightedLeastSquaresDeg2(
            const RingBuffer<Movement>& movements) const;

    const uint32_t mDegree;
    const Weighting mWeighting;
};

/*
 * Velocity tracker algorithm that uses an IIR filter.
 */
class IntegratingVelocityTrackerStrategy : public VelocityTrackerStrategy {
public:
    // Degree must be 1 or 2.
    IntegratingVelocityTrackerStrategy(uint32_t degree);
    ~IntegratingVelocityTrackerStrategy() override;

    void clearPointer(int32_t pointerId) override;
    void addMovement(nsecs_t eventTime, int32_t pointerId, float positions) override;
    std::optional<float> getVelocity(int32_t pointerId) const override;

private:
    // Current state estimate for a particular pointer.
    struct State {
        nsecs_t updateTime;
        uint32_t degree;

        float pos, vel, accel;
    };

    const uint32_t mDegree;
    BitSet32 mPointerIdBits;
    State mPointerState[MAX_POINTER_ID + 1];

    void initState(State& state, nsecs_t eventTime, float pos) const;
    void updateState(State& state, nsecs_t eventTime, float pos) const;
};


/*
 * Velocity tracker strategy used prior to ICS.
 */
class LegacyVelocityTrackerStrategy : public AccumulatingVelocityTrackerStrategy {
public:
    LegacyVelocityTrackerStrategy();
    ~LegacyVelocityTrackerStrategy() override;

    std::optional<float> getVelocity(int32_t pointerId) const override;

private:
    // Oldest sample to consider when calculating the velocity.
    static const nsecs_t HORIZON = 200 * 1000000; // 100 ms

    // The minimum duration between samples when estimating velocity.
    static const nsecs_t MIN_DURATION = 10 * 1000000; // 10 ms
};

class ImpulseVelocityTrackerStrategy : public AccumulatingVelocityTrackerStrategy {
public:
    ImpulseVelocityTrackerStrategy(bool deltaValues);
    ~ImpulseVelocityTrackerStrategy() override;

    std::optional<float> getVelocity(int32_t pointerId) const override;

private:
    // Sample horizon.
    // We don't use too much history by default since we want to react to quick
    // changes in direction.
    static constexpr nsecs_t HORIZON = 100 * 1000000; // 100 ms

    // Whether or not the input movement values for the strategy come in the form of delta values.
    // If the input values are not deltas, the strategy needs to calculate deltas as part of its
    // velocity calculation.
    const bool mDeltaValues;
};

} // namespace android
