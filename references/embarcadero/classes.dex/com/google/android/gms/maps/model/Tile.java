package com.google.android.gms.maps.model;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.maps.internal.r;

public final class Tile implements SafeParcelable {
    public static final TileCreator CREATOR;
    public final byte[] data;
    public final int height;
    public final int width;
    private final int wj;

    static {
        CREATOR = new TileCreator();
    }

    Tile(int versionCode, int width, int height, byte[] data) {
        this.wj = versionCode;
        this.width = width;
        this.height = height;
        this.data = data;
    }

    public Tile(int width, int height, byte[] data) {
        this(1, width, height, data);
    }

    public int describeContents() {
        return 0;
    }

    int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel out, int flags) {
        if (r.hc()) {
            i.a(this, out, flags);
        } else {
            TileCreator.a(this, out, flags);
        }
    }
}
