package com.google.android.gms.drive.metadata;

import com.google.android.gms.common.data.DataHolder;
import java.util.Collection;

public abstract class CollectionMetadataField<T> extends MetadataField<Collection<T>> {
    protected CollectionMetadataField(String fieldName, int versionAdded) {
        super(fieldName, versionAdded);
    }

    protected Collection<T> a(DataHolder dataHolder, int i, int i2) {
        throw new UnsupportedOperationException("Cannot read collections from a dataHolder.");
    }

    protected /* synthetic */ Object b(DataHolder dataHolder, int i, int i2) {
        return a(dataHolder, i, i2);
    }
}
