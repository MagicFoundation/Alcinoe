package com.google.android.gms.drive.metadata.internal;

import android.os.Bundle;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.drive.metadata.MetadataField;

public class e extends MetadataField<Long> {
    public e(String str, int i) {
        super(str, i);
    }

    protected void a(Bundle bundle, Long l) {
        bundle.putLong(getName(), l.longValue());
    }

    protected /* synthetic */ Object b(DataHolder dataHolder, int i, int i2) {
        return g(dataHolder, i, i2);
    }

    protected /* synthetic */ Object e(Bundle bundle) {
        return i(bundle);
    }

    protected Long g(DataHolder dataHolder, int i, int i2) {
        return Long.valueOf(dataHolder.getLong(getName(), i, i2));
    }

    protected Long i(Bundle bundle) {
        return Long.valueOf(bundle.getLong(getName()));
    }
}
