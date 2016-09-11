package com.google.android.gms.drive.metadata.internal;

import android.os.Bundle;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.drive.metadata.OrderedMetadataField;
import java.util.Date;

public class b extends OrderedMetadataField<Date> {
    public b(String str, int i) {
        super(str, i);
    }

    protected void a(Bundle bundle, Date date) {
        bundle.putLong(getName(), date.getTime());
    }

    protected /* synthetic */ Object b(DataHolder dataHolder, int i, int i2) {
        return e(dataHolder, i, i2);
    }

    protected /* synthetic */ Object e(Bundle bundle) {
        return g(bundle);
    }

    protected Date e(DataHolder dataHolder, int i, int i2) {
        return new Date(dataHolder.getLong(getName(), i, i2));
    }

    protected Date g(Bundle bundle) {
        return new Date(bundle.getLong(getName()));
    }
}
