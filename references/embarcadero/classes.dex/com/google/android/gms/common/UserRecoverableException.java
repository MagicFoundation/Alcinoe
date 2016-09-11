package com.google.android.gms.common;

import android.content.Intent;

public class UserRecoverableException extends Exception {
    private final Intent mIntent;

    public UserRecoverableException(String msg, Intent intent) {
        super(msg);
        this.mIntent = intent;
    }

    public Intent getIntent() {
        return new Intent(this.mIntent);
    }
}
