package com.google.android.gms.drive.metadata.internal;

import android.os.Bundle;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.drive.metadata.MetadataField;

public final class j extends MetadataField<String> {
    public j(String str, int i) {
        super(str, i);
    }

    protected void a(Bundle bundle, String str) {
        bundle.putString(getName(), str);
    }

    protected /* synthetic */ Object b(DataHolder dataHolder, int i, int i2) {
        return h(dataHolder, i, i2);
    }

    protected /* synthetic */ Object e(Bundle bundle) {
        return l(bundle);
    }

    protected String h(DataHolder dataHolder, int i, int i2) {
        return dataHolder.getString(getName(), i, i2);
    }

    protected String l(Bundle bundle) {
        return bundle.getString(getName());
    }
}
