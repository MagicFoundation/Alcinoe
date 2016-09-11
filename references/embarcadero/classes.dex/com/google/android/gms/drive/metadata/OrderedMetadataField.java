package com.google.android.gms.drive.metadata;

public abstract class OrderedMetadataField<T extends Comparable<T>> extends MetadataField<T> {
    protected OrderedMetadataField(String fieldName, int versionAdded) {
        super(fieldName, versionAdded);
    }
}
