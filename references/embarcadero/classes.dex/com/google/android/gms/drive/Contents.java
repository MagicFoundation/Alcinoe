package com.google.android.gms.drive;

import android.os.Parcel;
import android.os.ParcelFileDescriptor;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

public class Contents implements SafeParcelable {
    public static final Creator<Contents> CREATOR;
    final ParcelFileDescriptor AC;
    final int CQ;
    final int CR;
    final DriveId CS;
    private boolean CT;
    private boolean CU;
    private boolean mClosed;
    final int wj;

    static {
        CREATOR = new a();
    }

    Contents(int versionCode, ParcelFileDescriptor parcelFileDescriptor, int requestId, int mode, DriveId driveId) {
        this.mClosed = false;
        this.CT = false;
        this.CU = false;
        this.wj = versionCode;
        this.AC = parcelFileDescriptor;
        this.CQ = requestId;
        this.CR = mode;
        this.CS = driveId;
    }

    public void close() {
        this.mClosed = true;
    }

    public int describeContents() {
        return 0;
    }

    public int eP() {
        return this.CQ;
    }

    public DriveId getDriveId() {
        return this.CS;
    }

    public InputStream getInputStream() {
        if (this.mClosed) {
            throw new IllegalStateException("Contents have been closed, cannot access the input stream.");
        } else if (this.CR != DriveFile.MODE_READ_ONLY) {
            throw new IllegalStateException("getInputStream() can only be used with contents opened with MODE_READ_ONLY.");
        } else if (this.CT) {
            throw new IllegalStateException("getInputStream() can only be called once per Contents instance.");
        } else {
            this.CT = true;
            return new FileInputStream(this.AC.getFileDescriptor());
        }
    }

    public int getMode() {
        return this.CR;
    }

    public OutputStream getOutputStream() {
        if (this.mClosed) {
            throw new IllegalStateException("Contents have been closed, cannot access the output stream.");
        } else if (this.CR != DriveFile.MODE_WRITE_ONLY) {
            throw new IllegalStateException("getOutputStream() can only be used with contents opened with MODE_WRITE_ONLY.");
        } else if (this.CU) {
            throw new IllegalStateException("getOutputStream() can only be called once per Contents instance.");
        } else {
            this.CU = true;
            return new FileOutputStream(this.AC.getFileDescriptor());
        }
    }

    public ParcelFileDescriptor getParcelFileDescriptor() {
        if (!this.mClosed) {
            return this.AC;
        }
        throw new IllegalStateException("Contents have been closed, cannot access the output stream.");
    }

    public void writeToParcel(Parcel dest, int flags) {
        a.a(this, dest, flags);
    }
}
