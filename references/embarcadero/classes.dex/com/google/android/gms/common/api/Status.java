package com.google.android.gms.common.api;

import android.app.Activity;
import android.app.PendingIntent;
import android.content.IntentSender.SendIntentException;
import android.os.Parcel;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.ep;

public final class Status implements Result, SafeParcelable {
    public static final StatusCreator CREATOR;
    public static final Status zQ;
    public static final Status zR;
    public static final Status zS;
    private final PendingIntent mPendingIntent;
    private final int wj;
    private final int yJ;
    private final String zT;

    static {
        zQ = new Status(0, null, null);
        zR = new Status(14, null, null);
        zS = new Status(15, null, null);
        CREATOR = new StatusCreator();
    }

    public Status(int statusCode) {
        this(1, statusCode, null, null);
    }

    Status(int versionCode, int statusCode, String statusMessage, PendingIntent pendingIntent) {
        this.wj = versionCode;
        this.yJ = statusCode;
        this.zT = statusMessage;
        this.mPendingIntent = pendingIntent;
    }

    public Status(int statusCode, String statusMessage, PendingIntent pendingIntent) {
        this(1, statusCode, statusMessage, pendingIntent);
    }

    private String dn() {
        return this.zT != null ? this.zT : CommonStatusCodes.getStatusCodeString(this.yJ);
    }

    PendingIntent dE() {
        return this.mPendingIntent;
    }

    String dF() {
        return this.zT;
    }

    @Deprecated
    public ConnectionResult dG() {
        return new ConnectionResult(this.yJ, this.mPendingIntent);
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof Status)) {
            return false;
        }
        Status status = (Status) obj;
        return this.wj == status.wj && this.yJ == status.yJ && ep.equal(this.zT, status.zT) && ep.equal(this.mPendingIntent, status.mPendingIntent);
    }

    public PendingIntent getResolution() {
        return this.mPendingIntent;
    }

    public Status getStatus() {
        return this;
    }

    public int getStatusCode() {
        return this.yJ;
    }

    int getVersionCode() {
        return this.wj;
    }

    public boolean hasResolution() {
        return this.mPendingIntent != null;
    }

    public int hashCode() {
        return ep.hashCode(Integer.valueOf(this.wj), Integer.valueOf(this.yJ), this.zT, this.mPendingIntent);
    }

    public boolean isInterrupted() {
        return this.yJ == 14;
    }

    public boolean isSuccess() {
        return this.yJ <= 0;
    }

    public void startResolutionForResult(Activity activity, int requestCode) throws SendIntentException {
        if (hasResolution()) {
            activity.startIntentSenderForResult(this.mPendingIntent.getIntentSender(), requestCode, null, 0, 0, 0);
        }
    }

    public String toString() {
        return ep.e(this).a("statusCode", dn()).a("resolution", this.mPendingIntent).toString();
    }

    public void writeToParcel(Parcel out, int flags) {
        StatusCreator.a(this, out, flags);
    }
}
