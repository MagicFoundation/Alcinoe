package com.google.android.gms.internal;

import android.content.Context;
import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import java.util.Locale;

public class id implements SafeParcelable {
    public static final ie CREATOR;
    public final String OG;
    public final String OH;
    public final int versionCode;

    static {
        CREATOR = new ie();
    }

    public id(int i, String str, String str2) {
        this.versionCode = i;
        this.OG = str;
        this.OH = str2;
    }

    public id(Context context, Locale locale) {
        this.versionCode = 0;
        this.OG = context.getPackageName();
        this.OH = locale.toString();
    }

    public int describeContents() {
        ie ieVar = CREATOR;
        return 0;
    }

    public boolean equals(Object object) {
        if (this == object) {
            return true;
        }
        if (object == null || !(object instanceof id)) {
            return false;
        }
        id idVar = (id) object;
        return this.OH.equals(idVar.OH) && this.OG.equals(idVar.OG);
    }

    public int hashCode() {
        return ep.hashCode(this.OG, this.OH);
    }

    public String toString() {
        return ep.e(this).a("clientPackageName", this.OG).a("locale", this.OH).toString();
    }

    public void writeToParcel(Parcel out, int flags) {
        ie ieVar = CREATOR;
        ie.a(this, out, flags);
    }
}
