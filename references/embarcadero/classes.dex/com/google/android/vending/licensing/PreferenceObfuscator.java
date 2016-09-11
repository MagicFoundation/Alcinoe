package com.google.android.vending.licensing;

import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.util.Log;

public class PreferenceObfuscator {
    private static final String TAG = "PreferenceObfuscator";
    private Editor mEditor;
    private final Obfuscator mObfuscator;
    private final SharedPreferences mPreferences;

    public PreferenceObfuscator(SharedPreferences sp, Obfuscator o) {
        this.mPreferences = sp;
        this.mObfuscator = o;
        this.mEditor = null;
    }

    public void putString(String key, String value) {
        if (this.mEditor == null) {
            this.mEditor = this.mPreferences.edit();
        }
        this.mEditor.putString(key, this.mObfuscator.obfuscate(value, key));
    }

    public String getString(String key, String defValue) {
        String value = this.mPreferences.getString(key, null);
        if (value == null) {
            return defValue;
        }
        try {
            return this.mObfuscator.unobfuscate(value, key);
        } catch (ValidationException e) {
            Log.w(TAG, "Validation error while reading preference: " + key);
            return defValue;
        }
    }

    public void commit() {
        if (this.mEditor != null) {
            this.mEditor.commit();
            this.mEditor = null;
        }
    }
}
