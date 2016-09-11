package com.google.android.gms.drive.metadata.internal;

import android.os.Bundle;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.drive.metadata.MetadataField;

public class d extends MetadataField<Integer> {
    public d(String str, int i) {
        super(str, i);
    }

    protected void a(Bundle bundle, Integer num) {
        bundle.putInt(getName(), num.intValue());
    }

    protected /* synthetic */ Object b(DataHolder dataHolder, int i, int i2) {
        return f(dataHolder, i, i2);
    }

    protected /* synthetic */ Object e(Bundle bundle) {
        return h(bundle);
    }

    protected Integer f(DataHolder dataHolder, int i, int i2) {
        return Integer.valueOf(dataHolder.getInteger(getName(), i, i2));
    }

    protected Integer h(Bundle bundle) {
        return Integer.valueOf(bundle.getInt(getName()));
    }
}
