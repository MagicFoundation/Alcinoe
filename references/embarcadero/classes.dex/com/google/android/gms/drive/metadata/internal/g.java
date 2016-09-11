package com.google.android.gms.drive.metadata.internal;

import android.os.Bundle;
import android.os.Parcelable;
import com.google.android.gms.drive.metadata.CollectionMetadataField;
import java.util.ArrayList;
import java.util.Collection;

public class g<T extends Parcelable> extends CollectionMetadataField<T> {
    public g(String str, int i) {
        super(str, i);
    }

    protected void a(Bundle bundle, Collection<T> collection) {
        bundle.putParcelableArrayList(getName(), new ArrayList(collection));
    }

    protected /* synthetic */ Object e(Bundle bundle) {
        return j(bundle);
    }

    protected Collection<T> j(Bundle bundle) {
        return bundle.getParcelableArrayList(getName());
    }
}
