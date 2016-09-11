package com.google.android.gms.auth;

import android.content.Intent;

public class UserRecoverableAuthException extends GoogleAuthException {
    private final Intent mIntent;

    public UserRecoverableAuthException(String msg, Intent intent) {
        super(msg);
        this.mIntent = intent;
    }

    public Intent getIntent() {
        return this.mIntent == null ? null : new Intent(this.mIntent);
    }
}
