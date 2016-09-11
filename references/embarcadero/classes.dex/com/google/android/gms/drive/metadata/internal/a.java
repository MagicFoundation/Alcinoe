package com.google.android.gms.drive.metadata.internal;

import android.os.Bundle;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.drive.metadata.MetadataField;

public class a extends MetadataField<Boolean> {
    public a(String str, int i) {
        super(str, i);
    }

    protected void a(Bundle bundle, Boolean bool) {
        bundle.putBoolean(getName(), bool.booleanValue());
    }

    protected /* synthetic */ Object b(DataHolder dataHolder, int i, int i2) {
        return d(dataHolder, i, i2);
    }

    protected Boolean d(DataHolder dataHolder, int i, int i2) {
        return Boolean.valueOf(dataHolder.getBoolean(getName(), i, i2));
    }

    protected /* synthetic */ Object e(Bundle bundle) {
        return f(bundle);
    }

    protected Boolean f(Bundle bundle) {
        return Boolean.valueOf(bundle.getBoolean(getName()));
    }
}
