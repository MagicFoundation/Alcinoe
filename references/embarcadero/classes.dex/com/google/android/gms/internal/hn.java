package com.google.android.gms.internal;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public final class hn implements SafeParcelable {
    public static final ho CREATOR;
    final List<ht> LA;
    private final String LB;
    private final boolean LC;
    private final Set<ht> LD;
    final int wj;

    static {
        CREATOR = new ho();
    }

    hn(int i, List<ht> list, String str, boolean z) {
        this.wj = i;
        this.LA = list == null ? Collections.emptyList() : Collections.unmodifiableList(list);
        if (str == null) {
            str = "";
        }
        this.LB = str;
        this.LC = z;
        if (this.LA.isEmpty()) {
            this.LD = Collections.emptySet();
        } else {
            this.LD = Collections.unmodifiableSet(new HashSet(this.LA));
        }
    }

    public int describeContents() {
        ho hoVar = CREATOR;
        return 0;
    }

    public boolean equals(Object object) {
        if (this == object) {
            return true;
        }
        if (!(object instanceof hn)) {
            return false;
        }
        hn hnVar = (hn) object;
        return this.LD.equals(hnVar.LD) && this.LB == hnVar.LB && this.LC == hnVar.LC;
    }

    public String gr() {
        return this.LB;
    }

    public boolean gs() {
        return this.LC;
    }

    public int hashCode() {
        return ep.hashCode(this.LD, this.LB, Boolean.valueOf(this.LC));
    }

    public String toString() {
        return ep.e(this).a("types", this.LD).a("textQuery", this.LB).a("isOpenNowRequired", Boolean.valueOf(this.LC)).toString();
    }

    public void writeToParcel(Parcel parcel, int flags) {
        ho hoVar = CREATOR;
        ho.a(this, parcel, flags);
    }
}
