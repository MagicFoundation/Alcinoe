/*
 * Copyright (C) 2011 The Android Open Source Project
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

package com.alcinoe.util;

import java.util.Collection;

/**
 * Simple static methods to be called at the start of your own methods to verify
 * correct arguments and state.
 */
public class Preconditions {

    public static void checkArgument(boolean expression) {
        if (!expression) {
            throw new IllegalArgumentException();
        }
    }

    /**
     * Ensures that an object reference passed as a parameter to the calling
     * method is not null.
     *
     * @param reference an object reference
     * @return the non-null reference that was validated
     * @throws NullPointerException if {@code reference} is null
     */
    public static <T> T checkNotNull(final T reference) {
        if (reference == null) {
            throw new NullPointerException();
        }
        return reference;
    }

    /**
     * Ensures that an object reference passed as a parameter to the calling
     * method is not null.
     *
     * @param reference an object reference
     * @param errorMessage the exception message to use if the check fails; will
     *     be converted to a string using {@link String#valueOf(Object)}
     * @return the non-null reference that was validated
     * @throws NullPointerException if {@code reference} is null
     */
    public static <T> T checkNotNull(final T reference, final Object errorMessage) {
        if (reference == null) {
            throw new NullPointerException(String.valueOf(errorMessage));
        }
        return reference;
    }

    /**
     * Ensures the truth of an expression involving the state of the calling
     * instance, but not involving any parameters to the calling method.
     *
     * @param expression a boolean expression
     * @throws IllegalStateException if {@code expression} is false
     */
    public static void checkState(final boolean expression) {
        if (!expression) {
            throw new IllegalStateException();
        }
    }

    /**
     * Check the requested flags, throwing if any requested flags are outside
     * the allowed set.
     */
    public static void checkFlagsArgument(final int requestedFlags, final int allowedFlags) {
        if ((requestedFlags & allowedFlags) != requestedFlags) {
            throw new IllegalArgumentException("Requested flags 0x"
                    + Integer.toHexString(requestedFlags) + ", but only 0x"
                    + Integer.toHexString(allowedFlags) + " are allowed");
        }
    }

    /**
     * Ensures that that the argument numeric value is non-negative.
     *
     * @param value a numeric int value
     * @param errorMessage the exception message to use if the check fails
     * @return the validated numeric value
     * @throws IllegalArgumentException if {@code value} was negative
     */
    public static int checkArgumentNonnegative(final int value, final String errorMessage) {
        if (value < 0) {
            throw new IllegalArgumentException(errorMessage);
        }

        return value;
    }

    /**
     * Ensures that that the argument numeric value is non-negative.
     *
     * @param value a numeric long value
     * @param errorMessage the exception message to use if the check fails
     * @return the validated numeric value
     * @throws IllegalArgumentException if {@code value} was negative
     */
    public static long checkArgumentNonnegative(final long value, final String errorMessage) {
        if (value < 0) {
            throw new IllegalArgumentException(errorMessage);
        }

        return value;
    }

    /**
     * Ensures that that the argument numeric value is positive.
     *
     * @param value a numeric int value
     * @param errorMessage the exception message to use if the check fails
     * @return the validated numeric value
     * @throws IllegalArgumentException if {@code value} was not positive
     */
    public static int checkArgumentPositive(final int value, final String errorMessage) {
        if (value <= 0) {
            throw new IllegalArgumentException(errorMessage);
        }

        return value;
    }

    /**
     * Ensures that the argument floating point value is a finite number.
     *
     * <p>A finite number is defined to be both representable (that is, not NaN) and
     * not infinite (that is neither positive or negative infinity).</p>
     *
     * @param value a floating point value
     * @param valueName the name of the argument to use if the check fails
     *
     * @return the validated floating point value
     *
     * @throws IllegalArgumentException if {@code value} was not finite
     */
    public static float checkArgumentFinite(final float value, final String valueName) {
        if (Float.isNaN(value)) {
            throw new IllegalArgumentException(valueName + " must not be NaN");
        } else if (Float.isInfinite(value)) {
            throw new IllegalArgumentException(valueName + " must not be infinite");
        }

        return value;
    }

    /**
     * Ensures that the argument floating point value is within the inclusive range.
     *
     * <p>While this can be used to range check against +/- infinity, note that all NaN numbers
     * will always be out of range.</p>
     *
     * @param value a floating point value
     * @param lower the lower endpoint of the inclusive range
     * @param upper the upper endpoint of the inclusive range
     * @param valueName the name of the argument to use if the check fails
     *
     * @return the validated floating point value
     *
     * @throws IllegalArgumentException if {@code value} was not within the range
     */
    public static float checkArgumentInRange(float value, float lower, float upper,
            String valueName) {
        if (Float.isNaN(value)) {
            throw new IllegalArgumentException(valueName + " must not be NaN");
        } else if (value < lower) {
            throw new IllegalArgumentException(
                    String.format(
                            "%s is out of range of [%f, %f] (too low)", valueName, lower, upper));
        } else if (value > upper) {
            throw new IllegalArgumentException(
                    String.format(
                            "%s is out of range of [%f, %f] (too high)", valueName, lower, upper));
        }

        return value;
    }

    /**
     * Ensures that the argument int value is within the inclusive range.
     *
     * @param value a int value
     * @param lower the lower endpoint of the inclusive range
     * @param upper the upper endpoint of the inclusive range
     * @param valueName the name of the argument to use if the check fails
     *
     * @return the validated int value
     *
     * @throws IllegalArgumentException if {@code value} was not within the range
     */
    public static int checkArgumentInRange(int value, int lower, int upper,
            String valueName) {
        if (value < lower) {
            throw new IllegalArgumentException(
                    String.format(
                            "%s is out of range of [%d, %d] (too low)", valueName, lower, upper));
        } else if (value > upper) {
            throw new IllegalArgumentException(
                    String.format(
                            "%s is out of range of [%d, %d] (too high)", valueName, lower, upper));
        }

        return value;
    }

    /**
     * Ensures that the array is not {@code null}, and none of its elements are {@code null}.
     *
     * @param value an array of boxed objects
     * @param valueName the name of the argument to use if the check fails
     *
     * @return the validated array
     *
     * @throws NullPointerException if the {@code value} or any of its elements were {@code null}
     */
    public static <T> T[] checkArrayElementsNotNull(final T[] value, final String valueName) {
        if (value == null) {
            throw new NullPointerException(valueName + " must not be null");
        }

        for (int i = 0; i < value.length; ++i) {
            if (value[i] == null) {
                throw new NullPointerException(
                        String.format("%s[%d] must not be null", valueName, i));
            }
        }

        return value;
    }

    /**
     * Ensures that the {@link Collection} is not {@code null}, and none of its elements are
     * {@code null}.
     *
     * @param value a {@link Collection} of boxed objects
     * @param valueName the name of the argument to use if the check fails
     *
     * @return the validated {@link Collection}
     *
     * @throws NullPointerException if the {@code value} or any of its elements were {@code null}
     */
    public static <T> Collection<T> checkCollectionElementsNotNull(final Collection<T> value,
            final String valueName) {
        if (value == null) {
            throw new NullPointerException(valueName + " must not be null");
        }

        long ctr = 0;
        for (T elem : value) {
            if (elem == null) {
                throw new NullPointerException(
                        String.format("%s[%d] must not be null", valueName, ctr));
            }
            ++ctr;
        }

        return value;
    }

    /**
     * Ensures that the {@link Collection} is not {@code null}, and contains at least one element.
     *
     * @param value a {@link Collection} of boxed elements.
     * @param valueName the name of the argument to use if the check fails.

     * @return the validated {@link Collection}
     *
     * @throws NullPointerException if the {@code value} was {@code null}
     * @throws IllegalArgumentException if the {@code value} was empty
     */
    public static <T> Collection<T> checkCollectionNotEmpty(final Collection<T> value,
            final String valueName) {
        if (value == null) {
            throw new NullPointerException(valueName + " must not be null");
        }
        if (value.isEmpty()) {
            throw new IllegalArgumentException(valueName + " is empty");
        }
        return value;
    }

    /**
     * Ensures that all elements in the argument floating point array are within the inclusive range
     *
     * <p>While this can be used to range check against +/- infinity, note that all NaN numbers
     * will always be out of range.</p>
     *
     * @param value a floating point array of values
     * @param lower the lower endpoint of the inclusive range
     * @param upper the upper endpoint of the inclusive range
     * @param valueName the name of the argument to use if the check fails
     *
     * @return the validated floating point value
     *
     * @throws IllegalArgumentException if any of the elements in {@code value} were out of range
     * @throws NullPointerException if the {@code value} was {@code null}
     */
    public static float[] checkArrayElementsInRange(float[] value, float lower, float upper,
            String valueName) {
        checkNotNull(value, valueName + " must not be null");

        for (int i = 0; i < value.length; ++i) {
            float v = value[i];

            if (Float.isNaN(v)) {
                throw new IllegalArgumentException(valueName + "[" + i + "] must not be NaN");
            } else if (v < lower) {
                throw new IllegalArgumentException(
                        String.format("%s[%d] is out of range of [%f, %f] (too low)",
                                valueName, i, lower, upper));
            } else if (v > upper) {
                throw new IllegalArgumentException(
                        String.format("%s[%d] is out of range of [%f, %f] (too high)",
                                valueName, i, lower, upper));
            }
        }

        return value;
    }
}
