package com.google.android.gms.drive.metadata;

import android.os.Bundle;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;
import com.google.android.gms.internal.er;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public abstract class MetadataField<T> {
    private final String Eg;
    private final Set<String> Eh;
    private final int Ei;

    protected MetadataField(String fieldName, int versionAdded) {
        this.Eg = (String) er.b((Object) fieldName, (Object) "fieldName");
        this.Eh = Collections.singleton(fieldName);
        this.Ei = versionAdded;
    }

    protected MetadataField(String fieldName, Collection<String> dataHolderFieldNames, int versionAdded) {
        this.Eg = (String) er.b((Object) fieldName, (Object) "fieldName");
        this.Eh = Collections.unmodifiableSet(new HashSet(dataHolderFieldNames));
        this.Ei = versionAdded;
    }

    protected abstract void a(Bundle bundle, T t);

    public final void a(DataHolder dataHolder, MetadataBundle metadataBundle, int i, int i2) {
        er.b((Object) dataHolder, (Object) "dataHolder");
        er.b((Object) metadataBundle, (Object) "bundle");
        metadataBundle.b(this, c(dataHolder, i, i2));
    }

    public final void a(T t, Bundle bundle) {
        er.b((Object) bundle, (Object) "bundle");
        if (t == null) {
            bundle.putString(getName(), null);
        } else {
            a(bundle, (Object) t);
        }
    }

    protected abstract T b(DataHolder dataHolder, int i, int i2);

    public final T c(DataHolder dataHolder, int i, int i2) {
        for (String hasNull : this.Eh) {
            if (dataHolder.hasNull(hasNull, i, i2)) {
                return null;
            }
        }
        return b(dataHolder, i, i2);
    }

    public final T d(Bundle bundle) {
        er.b((Object) bundle, (Object) "bundle");
        return bundle.get(getName()) != null ? e(bundle) : null;
    }

    protected abstract T e(Bundle bundle);

    public final Collection<String> ff() {
        return this.Eh;
    }

    public final String getName() {
        return this.Eg;
    }

    public final int getVersionAdded() {
        return this.Ei;
    }

    public String toString() {
        return this.Eg;
    }
}
