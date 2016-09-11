package com.google.android.gms.drive.internal;

import com.google.android.gms.drive.Metadata;
import com.google.android.gms.drive.metadata.MetadataField;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;

public final class j extends Metadata {
    private final MetadataBundle CZ;

    public j(MetadataBundle metadataBundle) {
        this.CZ = metadataBundle;
    }

    protected <T> T a(MetadataField<T> metadataField) {
        return this.CZ.a((MetadataField) metadataField);
    }

    public Metadata eQ() {
        return new j(MetadataBundle.a(this.CZ));
    }

    public /* synthetic */ Object freeze() {
        return eQ();
    }

    public boolean isDataValid() {
        return this.CZ != null;
    }

    public String toString() {
        return "Metadata [mImpl=" + this.CZ + "]";
    }
}
