package com.google.android.gms.internal;

import com.google.ads.AdRequest.ErrorCode;
import com.google.ads.AdRequest.Gender;
import com.google.ads.AdSize;
import com.google.ads.mediation.MediationAdRequest;
import com.google.android.gms.ads.a;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.Date;
import java.util.HashSet;

public final class bk {

    /* renamed from: com.google.android.gms.internal.bk.1 */
    static /* synthetic */ class AnonymousClass1 {
        static final /* synthetic */ int[] mW;
        static final /* synthetic */ int[] mX;

        static {
            mX = new int[ErrorCode.values().length];
            try {
                mX[ErrorCode.INTERNAL_ERROR.ordinal()] = 1;
            } catch (NoSuchFieldError e) {
            }
            try {
                mX[ErrorCode.INVALID_REQUEST.ordinal()] = 2;
            } catch (NoSuchFieldError e2) {
            }
            try {
                mX[ErrorCode.NETWORK_ERROR.ordinal()] = 3;
            } catch (NoSuchFieldError e3) {
            }
            try {
                mX[ErrorCode.NO_FILL.ordinal()] = 4;
            } catch (NoSuchFieldError e4) {
            }
            mW = new int[Gender.values().length];
            try {
                mW[Gender.FEMALE.ordinal()] = 1;
            } catch (NoSuchFieldError e5) {
            }
            try {
                mW[Gender.MALE.ordinal()] = 2;
            } catch (NoSuchFieldError e6) {
            }
            try {
                mW[Gender.UNKNOWN.ordinal()] = 3;
            } catch (NoSuchFieldError e7) {
            }
        }
    }

    public static int a(ErrorCode errorCode) {
        switch (AnonymousClass1.mX[errorCode.ordinal()]) {
            case DetectedActivity.ON_FOOT /*2*/:
                return 1;
            case DetectedActivity.STILL /*3*/:
                return 2;
            case DetectedActivity.UNKNOWN /*4*/:
                return 3;
            default:
                return 0;
        }
    }

    public static int a(Gender gender) {
        switch (AnonymousClass1.mW[gender.ordinal()]) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return 2;
            case DetectedActivity.ON_FOOT /*2*/:
                return 1;
            default:
                return 0;
        }
    }

    public static AdSize b(ab abVar) {
        return new AdSize(a.a(abVar.width, abVar.height, abVar.ln));
    }

    public static MediationAdRequest e(z zVar) {
        return new MediationAdRequest(new Date(zVar.le), g(zVar.lf), zVar.lg != null ? new HashSet(zVar.lg) : null, zVar.lh);
    }

    public static Gender g(int i) {
        switch (i) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return Gender.MALE;
            case DetectedActivity.ON_FOOT /*2*/:
                return Gender.FEMALE;
            default:
                return Gender.UNKNOWN;
        }
    }

    public static final ErrorCode h(int i) {
        switch (i) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return ErrorCode.INVALID_REQUEST;
            case DetectedActivity.ON_FOOT /*2*/:
                return ErrorCode.NETWORK_ERROR;
            case DetectedActivity.STILL /*3*/:
                return ErrorCode.NO_FILL;
            default:
                return ErrorCode.INTERNAL_ERROR;
        }
    }
}
