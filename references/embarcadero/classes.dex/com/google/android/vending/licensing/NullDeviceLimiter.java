package com.google.android.vending.licensing;

public class NullDeviceLimiter implements DeviceLimiter {
    public int isDeviceAllowed(String userId) {
        return Policy.LICENSED;
    }
}
