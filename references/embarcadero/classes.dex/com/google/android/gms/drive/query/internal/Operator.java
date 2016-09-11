package com.google.android.gms.drive.query.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public class Operator implements SafeParcelable {
    public static final Creator<Operator> CREATOR;
    public static final Operator Fa;
    public static final Operator Fb;
    public static final Operator Fc;
    public static final Operator Fd;
    public static final Operator Fe;
    public static final Operator Ff;
    public static final Operator Fg;
    public static final Operator Fh;
    public static final Operator Fi;
    final String mTag;
    final int wj;

    static {
        CREATOR = new h();
        Fa = new Operator("=");
        Fb = new Operator("<");
        Fc = new Operator("<=");
        Fd = new Operator(">");
        Fe = new Operator(">=");
        Ff = new Operator("and");
        Fg = new Operator("or");
        Fh = new Operator("not");
        Fi = new Operator("contains");
    }

    Operator(int versionCode, String tag) {
        this.wj = versionCode;
        this.mTag = tag;
    }

    private Operator(String tag) {
        this(1, tag);
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        Operator operator = (Operator) obj;
        return this.mTag == null ? operator.mTag == null : this.mTag.equals(operator.mTag);
    }

    public int hashCode() {
        return (this.mTag == null ? 0 : this.mTag.hashCode()) + 31;
    }

    public void writeToParcel(Parcel out, int flags) {
        h.a(this, out, flags);
    }
}
