package com.google.android.gms.drive.query.internal;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.metadata.CollectionMetadataField;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;
import com.google.android.gms.drive.query.Filter;
import java.util.Collections;

public class InFilter<T> implements SafeParcelable, Filter {
    public static final e CREATOR;
    final MetadataBundle EP;
    private final CollectionMetadataField<T> EX;
    final int wj;

    static {
        CREATOR = new e();
    }

    InFilter(int versionCode, MetadataBundle value) {
        this.wj = versionCode;
        this.EP = value;
        this.EX = (CollectionMetadataField) d.b(value);
    }

    public InFilter(CollectionMetadataField<T> field, T value) {
        this(1, MetadataBundle.a(field, Collections.singleton(value)));
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel out, int flags) {
        e.a(this, out, flags);
    }
}
