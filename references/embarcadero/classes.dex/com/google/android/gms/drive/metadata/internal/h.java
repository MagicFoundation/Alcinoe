package com.google.android.gms.drive.metadata.internal;

import android.os.Bundle;
import android.os.Parcelable;
import com.google.android.gms.drive.metadata.MetadataField;
import java.util.Collection;

public abstract class h<T extends Parcelable> extends MetadataField<T> {
    public h(String str, Collection<String> collection, int i) {
        super(str, collection, i);
    }

    protected void a(Bundle bundle, T t) {
        bundle.putParcelable(getName(), t);
    }

    protected /* synthetic */ Object e(Bundle bundle) {
        return k(bundle);
    }

    protected T k(Bundle bundle) {
        return bundle.getParcelable(getName());
    }
}
