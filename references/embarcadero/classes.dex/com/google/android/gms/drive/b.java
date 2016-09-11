package com.google.android.gms.drive;

import com.google.android.gms.drive.metadata.MetadataField;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;

public final class b extends Metadata {
    private final MetadataBundle CZ;

    public b(MetadataBundle metadataBundle) {
        this.CZ = metadataBundle;
    }

    protected <T> T a(MetadataField<T> metadataField) {
        return this.CZ.a((MetadataField) metadataField);
    }

    public Metadata eQ() {
        return new b(MetadataBundle.a(this.CZ));
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
