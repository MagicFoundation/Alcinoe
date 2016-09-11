package com.google.analytics.tracking.android;

import android.content.Context;
import android.text.TextUtils;

class ParameterLoaderImpl implements ParameterLoader {
    private final Context mContext;

    public ParameterLoaderImpl(Context context) {
        if (context == null) {
            throw new NullPointerException("Context cannot be null");
        }
        this.mContext = context.getApplicationContext();
    }

    private int getResourceIdForType(String key, String type) {
        if (this.mContext == null) {
            return 0;
        }
        return this.mContext.getResources().getIdentifier(key, type, this.mContext.getPackageName());
    }

    public String getString(String key) {
        int id = getResourceIdForType(key, "string");
        if (id == 0) {
            return null;
        }
        return this.mContext.getString(id);
    }

    public boolean getBoolean(String key) {
        int id = getResourceIdForType(key, "bool");
        if (id == 0) {
            return false;
        }
        return "true".equalsIgnoreCase(this.mContext.getString(id));
    }

    public int getInt(String key, int defaultValue) {
        int id = getResourceIdForType(key, "integer");
        if (id != 0) {
            try {
                defaultValue = Integer.parseInt(this.mContext.getString(id));
            } catch (NumberFormatException e) {
                Log.w("NumberFormatException parsing " + this.mContext.getString(id));
            }
        }
        return defaultValue;
    }

    public boolean isBooleanKeyPresent(String key) {
        return getResourceIdForType(key, "bool") != 0;
    }

    public Double getDoubleFromString(String key) {
        Double d = null;
        String value = getString(key);
        if (!TextUtils.isEmpty(value)) {
            try {
                d = Double.valueOf(Double.parseDouble(value));
            } catch (NumberFormatException e) {
                Log.w("NumberFormatException parsing " + value);
            }
        }
        return d;
    }
}
