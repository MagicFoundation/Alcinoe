package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.text.TextUtils;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.er;

public final class NotifyTransactionStatusRequest implements SafeParcelable {
    public static final Creator<NotifyTransactionStatusRequest> CREATOR;
    String Yk;
    String Zk;
    int status;
    final int wj;

    public final class Builder {
        final /* synthetic */ NotifyTransactionStatusRequest Zl;

        private Builder(NotifyTransactionStatusRequest notifyTransactionStatusRequest) {
            this.Zl = notifyTransactionStatusRequest;
        }

        public NotifyTransactionStatusRequest build() {
            boolean z = true;
            er.b(!TextUtils.isEmpty(this.Zl.Yk), (Object) "googleTransactionId is required");
            if (this.Zl.status < 1 || this.Zl.status > 8) {
                z = false;
            }
            er.b(z, (Object) "status is an unrecognized value");
            return this.Zl;
        }

        public Builder setDetailedReason(String detailedReason) {
            this.Zl.Zk = detailedReason;
            return this;
        }

        public Builder setGoogleTransactionId(String googleTransactionId) {
            this.Zl.Yk = googleTransactionId;
            return this;
        }

        public Builder setStatus(int status) {
            this.Zl.status = status;
            return this;
        }
    }

    public interface Status {
        public static final int SUCCESS = 1;

        public interface Error {
            public static final int AVS_DECLINE = 7;
            public static final int BAD_CARD = 4;
            public static final int BAD_CVC = 3;
            public static final int DECLINED = 5;
            public static final int FRAUD_DECLINE = 8;
            public static final int OTHER = 6;
            public static final int UNKNOWN = 2;
        }
    }

    static {
        CREATOR = new m();
    }

    NotifyTransactionStatusRequest() {
        this.wj = 1;
    }

    NotifyTransactionStatusRequest(int versionCode, String googleTransactionId, int status, String detailedReason) {
        this.wj = versionCode;
        this.Yk = googleTransactionId;
        this.status = status;
        this.Zk = detailedReason;
    }

    public static Builder newBuilder() {
        NotifyTransactionStatusRequest notifyTransactionStatusRequest = new NotifyTransactionStatusRequest();
        notifyTransactionStatusRequest.getClass();
        return new Builder(null);
    }

    public int describeContents() {
        return 0;
    }

    public String getDetailedReason() {
        return this.Zk;
    }

    public String getGoogleTransactionId() {
        return this.Yk;
    }

    public int getStatus() {
        return this.status;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel out, int flags) {
        m.a(this, out, flags);
    }
}
