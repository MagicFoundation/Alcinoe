package com.google.android.gms.internal;

import android.os.Bundle;
import android.os.Parcel;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.fb.a;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;

public class fh extends fb implements SafeParcelable {
    public static final fi CREATOR;
    private final fe CC;
    private final Parcel CJ;
    private final int CK;
    private int CL;
    private int CM;
    private final String mClassName;
    private final int wj;

    static {
        CREATOR = new fi();
    }

    fh(int i, Parcel parcel, fe feVar) {
        this.wj = i;
        this.CJ = (Parcel) er.f(parcel);
        this.CK = 2;
        this.CC = feVar;
        if (this.CC == null) {
            this.mClassName = null;
        } else {
            this.mClassName = this.CC.eD();
        }
        this.CL = 2;
    }

    private fh(SafeParcelable safeParcelable, fe feVar, String str) {
        this.wj = 1;
        this.CJ = Parcel.obtain();
        safeParcelable.writeToParcel(this.CJ, 0);
        this.CK = 1;
        this.CC = (fe) er.f(feVar);
        this.mClassName = (String) er.f(str);
        this.CL = 2;
    }

    public static <T extends fb & SafeParcelable> fh a(T t) {
        String canonicalName = t.getClass().getCanonicalName();
        return new fh((SafeParcelable) t, b(t), canonicalName);
    }

    private static void a(fe feVar, fb fbVar) {
        Class cls = fbVar.getClass();
        if (!feVar.b(cls)) {
            HashMap en = fbVar.en();
            feVar.a(cls, fbVar.en());
            for (String str : en.keySet()) {
                a aVar = (a) en.get(str);
                Class ev = aVar.ev();
                if (ev != null) {
                    try {
                        a(feVar, (fb) ev.newInstance());
                    } catch (Throwable e) {
                        throw new IllegalStateException("Could not instantiate an object of type " + aVar.ev().getCanonicalName(), e);
                    } catch (Throwable e2) {
                        throw new IllegalStateException("Could not access object of type " + aVar.ev().getCanonicalName(), e2);
                    }
                }
            }
        }
    }

    private void a(StringBuilder stringBuilder, int i, Object obj) {
        switch (i) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
            case DetectedActivity.ON_FOOT /*2*/:
            case DetectedActivity.STILL /*3*/:
            case DetectedActivity.UNKNOWN /*4*/:
            case DetectedActivity.TILTING /*5*/:
            case Participant.STATUS_UNRESPONSIVE /*6*/:
                stringBuilder.append(obj);
            case Error.AVS_DECLINE /*7*/:
                stringBuilder.append("\"").append(fp.ap(obj.toString())).append("\"");
            case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                stringBuilder.append("\"").append(fk.d((byte[]) obj)).append("\"");
            case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                stringBuilder.append("\"").append(fk.e((byte[]) obj));
                stringBuilder.append("\"");
            case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                fq.a(stringBuilder, (HashMap) obj);
            case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                throw new IllegalArgumentException("Method does not accept concrete type.");
            default:
                throw new IllegalArgumentException("Unknown type = " + i);
        }
    }

    private void a(StringBuilder stringBuilder, a<?, ?> aVar, Parcel parcel, int i) {
        switch (aVar.em()) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                b(stringBuilder, (a) aVar, a(aVar, Integer.valueOf(com.google.android.gms.common.internal.safeparcel.a.g(parcel, i))));
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                b(stringBuilder, (a) aVar, a(aVar, com.google.android.gms.common.internal.safeparcel.a.i(parcel, i)));
            case DetectedActivity.ON_FOOT /*2*/:
                b(stringBuilder, (a) aVar, a(aVar, Long.valueOf(com.google.android.gms.common.internal.safeparcel.a.h(parcel, i))));
            case DetectedActivity.STILL /*3*/:
                b(stringBuilder, (a) aVar, a(aVar, Float.valueOf(com.google.android.gms.common.internal.safeparcel.a.j(parcel, i))));
            case DetectedActivity.UNKNOWN /*4*/:
                b(stringBuilder, (a) aVar, a(aVar, Double.valueOf(com.google.android.gms.common.internal.safeparcel.a.k(parcel, i))));
            case DetectedActivity.TILTING /*5*/:
                b(stringBuilder, (a) aVar, a(aVar, com.google.android.gms.common.internal.safeparcel.a.l(parcel, i)));
            case Participant.STATUS_UNRESPONSIVE /*6*/:
                b(stringBuilder, (a) aVar, a(aVar, Boolean.valueOf(com.google.android.gms.common.internal.safeparcel.a.c(parcel, i))));
            case Error.AVS_DECLINE /*7*/:
                b(stringBuilder, (a) aVar, a(aVar, com.google.android.gms.common.internal.safeparcel.a.m(parcel, i)));
            case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
            case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                b(stringBuilder, (a) aVar, a(aVar, com.google.android.gms.common.internal.safeparcel.a.p(parcel, i)));
            case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                b(stringBuilder, (a) aVar, a(aVar, c(com.google.android.gms.common.internal.safeparcel.a.o(parcel, i))));
            case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                throw new IllegalArgumentException("Method does not accept concrete type.");
            default:
                throw new IllegalArgumentException("Unknown field out type = " + aVar.em());
        }
    }

    private void a(StringBuilder stringBuilder, String str, a<?, ?> aVar, Parcel parcel, int i) {
        stringBuilder.append("\"").append(str).append("\":");
        if (aVar.ex()) {
            a(stringBuilder, aVar, parcel, i);
        } else {
            b(stringBuilder, aVar, parcel, i);
        }
    }

    private void a(StringBuilder stringBuilder, HashMap<String, a<?, ?>> hashMap, Parcel parcel) {
        HashMap c = c((HashMap) hashMap);
        stringBuilder.append('{');
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        Object obj = null;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            Entry entry = (Entry) c.get(Integer.valueOf(com.google.android.gms.common.internal.safeparcel.a.S(n)));
            if (entry != null) {
                if (obj != null) {
                    stringBuilder.append(",");
                }
                a(stringBuilder, (String) entry.getKey(), (a) entry.getValue(), parcel, n);
                obj = 1;
            }
        }
        if (parcel.dataPosition() != o) {
            throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
        }
        stringBuilder.append('}');
    }

    private static fe b(fb fbVar) {
        fe feVar = new fe(fbVar.getClass());
        a(feVar, fbVar);
        feVar.eB();
        feVar.eA();
        return feVar;
    }

    private void b(StringBuilder stringBuilder, a<?, ?> aVar, Parcel parcel, int i) {
        if (aVar.es()) {
            stringBuilder.append("[");
            switch (aVar.em()) {
                case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                    fj.a(stringBuilder, com.google.android.gms.common.internal.safeparcel.a.r(parcel, i));
                    break;
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    fj.a(stringBuilder, com.google.android.gms.common.internal.safeparcel.a.t(parcel, i));
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    fj.a(stringBuilder, com.google.android.gms.common.internal.safeparcel.a.s(parcel, i));
                    break;
                case DetectedActivity.STILL /*3*/:
                    fj.a(stringBuilder, com.google.android.gms.common.internal.safeparcel.a.u(parcel, i));
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    fj.a(stringBuilder, com.google.android.gms.common.internal.safeparcel.a.v(parcel, i));
                    break;
                case DetectedActivity.TILTING /*5*/:
                    fj.a(stringBuilder, com.google.android.gms.common.internal.safeparcel.a.w(parcel, i));
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    fj.a(stringBuilder, com.google.android.gms.common.internal.safeparcel.a.q(parcel, i));
                    break;
                case Error.AVS_DECLINE /*7*/:
                    fj.a(stringBuilder, com.google.android.gms.common.internal.safeparcel.a.x(parcel, i));
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    throw new UnsupportedOperationException("List of type BASE64, BASE64_URL_SAFE, or STRING_MAP is not supported");
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    Parcel[] A = com.google.android.gms.common.internal.safeparcel.a.A(parcel, i);
                    int length = A.length;
                    for (int i2 = 0; i2 < length; i2++) {
                        if (i2 > 0) {
                            stringBuilder.append(",");
                        }
                        A[i2].setDataPosition(0);
                        a(stringBuilder, aVar.ez(), A[i2]);
                    }
                    break;
                default:
                    throw new IllegalStateException("Unknown field type out.");
            }
            stringBuilder.append("]");
            return;
        }
        switch (aVar.em()) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                stringBuilder.append(com.google.android.gms.common.internal.safeparcel.a.g(parcel, i));
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                stringBuilder.append(com.google.android.gms.common.internal.safeparcel.a.i(parcel, i));
            case DetectedActivity.ON_FOOT /*2*/:
                stringBuilder.append(com.google.android.gms.common.internal.safeparcel.a.h(parcel, i));
            case DetectedActivity.STILL /*3*/:
                stringBuilder.append(com.google.android.gms.common.internal.safeparcel.a.j(parcel, i));
            case DetectedActivity.UNKNOWN /*4*/:
                stringBuilder.append(com.google.android.gms.common.internal.safeparcel.a.k(parcel, i));
            case DetectedActivity.TILTING /*5*/:
                stringBuilder.append(com.google.android.gms.common.internal.safeparcel.a.l(parcel, i));
            case Participant.STATUS_UNRESPONSIVE /*6*/:
                stringBuilder.append(com.google.android.gms.common.internal.safeparcel.a.c(parcel, i));
            case Error.AVS_DECLINE /*7*/:
                stringBuilder.append("\"").append(fp.ap(com.google.android.gms.common.internal.safeparcel.a.m(parcel, i))).append("\"");
            case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                stringBuilder.append("\"").append(fk.d(com.google.android.gms.common.internal.safeparcel.a.p(parcel, i))).append("\"");
            case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                stringBuilder.append("\"").append(fk.e(com.google.android.gms.common.internal.safeparcel.a.p(parcel, i)));
                stringBuilder.append("\"");
            case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                Bundle o = com.google.android.gms.common.internal.safeparcel.a.o(parcel, i);
                Set<String> keySet = o.keySet();
                keySet.size();
                stringBuilder.append("{");
                int i3 = 1;
                for (String str : keySet) {
                    if (i3 == 0) {
                        stringBuilder.append(",");
                    }
                    stringBuilder.append("\"").append(str).append("\"");
                    stringBuilder.append(":");
                    stringBuilder.append("\"").append(fp.ap(o.getString(str))).append("\"");
                    i3 = 0;
                }
                stringBuilder.append("}");
            case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                Parcel z = com.google.android.gms.common.internal.safeparcel.a.z(parcel, i);
                z.setDataPosition(0);
                a(stringBuilder, aVar.ez(), z);
            default:
                throw new IllegalStateException("Unknown field type out");
        }
    }

    private void b(StringBuilder stringBuilder, a<?, ?> aVar, Object obj) {
        if (aVar.er()) {
            b(stringBuilder, (a) aVar, (ArrayList) obj);
        } else {
            a(stringBuilder, aVar.el(), obj);
        }
    }

    private void b(StringBuilder stringBuilder, a<?, ?> aVar, ArrayList<?> arrayList) {
        stringBuilder.append("[");
        int size = arrayList.size();
        for (int i = 0; i < size; i++) {
            if (i != 0) {
                stringBuilder.append(",");
            }
            a(stringBuilder, aVar.el(), arrayList.get(i));
        }
        stringBuilder.append("]");
    }

    public static HashMap<String, String> c(Bundle bundle) {
        HashMap<String, String> hashMap = new HashMap();
        for (String str : bundle.keySet()) {
            hashMap.put(str, bundle.getString(str));
        }
        return hashMap;
    }

    private static HashMap<Integer, Entry<String, a<?, ?>>> c(HashMap<String, a<?, ?>> hashMap) {
        HashMap<Integer, Entry<String, a<?, ?>>> hashMap2 = new HashMap();
        for (Entry entry : hashMap.entrySet()) {
            hashMap2.put(Integer.valueOf(((a) entry.getValue()).eu()), entry);
        }
        return hashMap2;
    }

    protected Object ak(String str) {
        throw new UnsupportedOperationException("Converting to JSON does not require this method.");
    }

    protected boolean al(String str) {
        throw new UnsupportedOperationException("Converting to JSON does not require this method.");
    }

    public int describeContents() {
        fi fiVar = CREATOR;
        return 0;
    }

    public Parcel eF() {
        switch (this.CL) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                this.CM = b.p(this.CJ);
                b.D(this.CJ, this.CM);
                this.CL = 2;
                break;
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                b.D(this.CJ, this.CM);
                this.CL = 2;
                break;
        }
        return this.CJ;
    }

    fe eG() {
        switch (this.CK) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                return null;
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return this.CC;
            case DetectedActivity.ON_FOOT /*2*/:
                return this.CC;
            default:
                throw new IllegalStateException("Invalid creation type: " + this.CK);
        }
    }

    public HashMap<String, a<?, ?>> en() {
        return this.CC == null ? null : this.CC.ao(this.mClassName);
    }

    public int getVersionCode() {
        return this.wj;
    }

    public String toString() {
        er.b(this.CC, (Object) "Cannot convert to JSON on client side.");
        Parcel eF = eF();
        eF.setDataPosition(0);
        StringBuilder stringBuilder = new StringBuilder(100);
        a(stringBuilder, this.CC.ao(this.mClassName), eF);
        return stringBuilder.toString();
    }

    public void writeToParcel(Parcel out, int flags) {
        fi fiVar = CREATOR;
        fi.a(this, out, flags);
    }
}
