package com.google.android.gms.internal;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.fb.a;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.plus.model.moments.ItemScope;
import com.google.android.gms.plus.model.moments.Moment;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public final class io extends fb implements SafeParcelable, Moment {
    public static final ip CREATOR;
    private static final HashMap<String, a<?, ?>> RL;
    private String Oc;
    private final Set<Integer> RM;
    private im SH;
    private im SI;
    private String Sz;
    private String uS;
    private final int wj;

    static {
        CREATOR = new ip();
        RL = new HashMap();
        RL.put("id", a.j("id", 2));
        RL.put("result", a.a("result", 4, im.class));
        RL.put("startDate", a.j("startDate", 5));
        RL.put("target", a.a("target", 6, im.class));
        RL.put("type", a.j("type", 7));
    }

    public io() {
        this.wj = 1;
        this.RM = new HashSet();
    }

    io(Set<Integer> set, int i, String str, im imVar, String str2, im imVar2, String str3) {
        this.RM = set;
        this.wj = i;
        this.uS = str;
        this.SH = imVar;
        this.Sz = str2;
        this.SI = imVar2;
        this.Oc = str3;
    }

    public io(Set<Integer> set, String str, im imVar, String str2, im imVar2, String str3) {
        this.RM = set;
        this.wj = 1;
        this.uS = str;
        this.SH = imVar;
        this.Sz = str2;
        this.SI = imVar2;
        this.Oc = str3;
    }

    protected boolean a(a aVar) {
        return this.RM.contains(Integer.valueOf(aVar.eu()));
    }

    protected Object ak(String str) {
        return null;
    }

    protected boolean al(String str) {
        return false;
    }

    protected Object b(a aVar) {
        switch (aVar.eu()) {
            case DetectedActivity.ON_FOOT /*2*/:
                return this.uS;
            case DetectedActivity.UNKNOWN /*4*/:
                return this.SH;
            case DetectedActivity.TILTING /*5*/:
                return this.Sz;
            case Participant.STATUS_UNRESPONSIVE /*6*/:
                return this.SI;
            case Error.AVS_DECLINE /*7*/:
                return this.Oc;
            default:
                throw new IllegalStateException("Unknown safe parcelable id=" + aVar.eu());
        }
    }

    public int describeContents() {
        ip ipVar = CREATOR;
        return 0;
    }

    public HashMap<String, a<?, ?>> en() {
        return RL;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof io)) {
            return false;
        }
        if (this == obj) {
            return true;
        }
        io ioVar = (io) obj;
        for (a aVar : RL.values()) {
            if (a(aVar)) {
                if (!ioVar.a(aVar)) {
                    return false;
                }
                if (!b(aVar).equals(ioVar.b(aVar))) {
                    return false;
                }
            } else if (ioVar.a(aVar)) {
                return false;
            }
        }
        return true;
    }

    public /* synthetic */ Object freeze() {
        return hU();
    }

    public String getId() {
        return this.uS;
    }

    public ItemScope getResult() {
        return this.SH;
    }

    public String getStartDate() {
        return this.Sz;
    }

    public ItemScope getTarget() {
        return this.SI;
    }

    public String getType() {
        return this.Oc;
    }

    int getVersionCode() {
        return this.wj;
    }

    Set<Integer> hB() {
        return this.RM;
    }

    im hS() {
        return this.SH;
    }

    im hT() {
        return this.SI;
    }

    public io hU() {
        return this;
    }

    public boolean hasId() {
        return this.RM.contains(Integer.valueOf(2));
    }

    public boolean hasResult() {
        return this.RM.contains(Integer.valueOf(4));
    }

    public boolean hasStartDate() {
        return this.RM.contains(Integer.valueOf(5));
    }

    public boolean hasTarget() {
        return this.RM.contains(Integer.valueOf(6));
    }

    public boolean hasType() {
        return this.RM.contains(Integer.valueOf(7));
    }

    public int hashCode() {
        int i = 0;
        for (a aVar : RL.values()) {
            int hashCode;
            if (a(aVar)) {
                hashCode = b(aVar).hashCode() + (i + aVar.eu());
            } else {
                hashCode = i;
            }
            i = hashCode;
        }
        return i;
    }

    public boolean isDataValid() {
        return true;
    }

    public void writeToParcel(Parcel out, int flags) {
        ip ipVar = CREATOR;
        ip.a(this, out, flags);
    }
}
