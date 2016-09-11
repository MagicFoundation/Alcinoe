package com.google.android.gms.drive.query.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.query.Filter;

public class NotFilter implements SafeParcelable, Filter {
    public static final Creator<NotFilter> CREATOR;
    final FilterHolder EZ;
    final int wj;

    static {
        CREATOR = new g();
    }

    NotFilter(int versionCode, FilterHolder toNegate) {
        this.wj = versionCode;
        this.EZ = toNegate;
    }

    public NotFilter(Filter toNegate) {
        this(1, new FilterHolder(toNegate));
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel out, int flags) {
        g.a(this, out, flags);
    }
}
