package com.google.android.gms.internal;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import java.util.List;

public final class hz implements SafeParcelable {
    public static final ia CREATOR;
    public final String Ov;
    public final String Ow;
    public final String Ox;
    public final List<String> Oy;
    public final String name;
    public final int versionCode;

    static {
        CREATOR = new ia();
    }

    public hz(int i, String str, String str2, String str3, String str4, List<String> list) {
        this.versionCode = i;
        this.name = str;
        this.Ov = str2;
        this.Ow = str3;
        this.Ox = str4;
        this.Oy = list;
    }

    public int describeContents() {
        ia iaVar = CREATOR;
        return 0;
    }

    public boolean equals(Object object) {
        if (this == object) {
            return true;
        }
        if (!(object instanceof hz)) {
            return false;
        }
        hz hzVar = (hz) object;
        return ep.equal(this.name, hzVar.name) && ep.equal(this.Ov, hzVar.Ov) && ep.equal(this.Ow, hzVar.Ow) && ep.equal(this.Ox, hzVar.Ox) && ep.equal(this.Oy, hzVar.Oy);
    }

    public int hashCode() {
        return ep.hashCode(this.name, this.Ov, this.Ow, this.Ox);
    }

    public String toString() {
        return ep.e(this).a("name", this.name).a("address", this.Ov).a("internationalPhoneNumber", this.Ow).a("regularOpenHours", this.Ox).a("attributions", this.Oy).toString();
    }

    public void writeToParcel(Parcel parcel, int flags) {
        ia iaVar = CREATOR;
        ia.a(this, parcel, flags);
    }
}
