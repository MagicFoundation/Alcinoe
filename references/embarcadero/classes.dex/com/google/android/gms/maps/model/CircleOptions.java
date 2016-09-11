package com.google.android.gms.maps.model;

import android.os.Parcel;
import android.support.v4.view.ViewCompat;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.maps.internal.r;

public final class CircleOptions implements SafeParcelable {
    public static final CircleOptionsCreator CREATOR;
    private LatLng PK;
    private double PL;
    private float PM;
    private int PN;
    private int PO;
    private float PP;
    private boolean PQ;
    private final int wj;

    static {
        CREATOR = new CircleOptionsCreator();
    }

    public CircleOptions() {
        this.PK = null;
        this.PL = 0.0d;
        this.PM = 10.0f;
        this.PN = ViewCompat.MEASURED_STATE_MASK;
        this.PO = 0;
        this.PP = 0.0f;
        this.PQ = true;
        this.wj = 1;
    }

    CircleOptions(int versionCode, LatLng center, double radius, float strokeWidth, int strokeColor, int fillColor, float zIndex, boolean visible) {
        this.PK = null;
        this.PL = 0.0d;
        this.PM = 10.0f;
        this.PN = ViewCompat.MEASURED_STATE_MASK;
        this.PO = 0;
        this.PP = 0.0f;
        this.PQ = true;
        this.wj = versionCode;
        this.PK = center;
        this.PL = radius;
        this.PM = strokeWidth;
        this.PN = strokeColor;
        this.PO = fillColor;
        this.PP = zIndex;
        this.PQ = visible;
    }

    public CircleOptions center(LatLng center) {
        this.PK = center;
        return this;
    }

    public int describeContents() {
        return 0;
    }

    public CircleOptions fillColor(int color) {
        this.PO = color;
        return this;
    }

    public LatLng getCenter() {
        return this.PK;
    }

    public int getFillColor() {
        return this.PO;
    }

    public double getRadius() {
        return this.PL;
    }

    public int getStrokeColor() {
        return this.PN;
    }

    public float getStrokeWidth() {
        return this.PM;
    }

    int getVersionCode() {
        return this.wj;
    }

    public float getZIndex() {
        return this.PP;
    }

    public boolean isVisible() {
        return this.PQ;
    }

    public CircleOptions radius(double radius) {
        this.PL = radius;
        return this;
    }

    public CircleOptions strokeColor(int color) {
        this.PN = color;
        return this;
    }

    public CircleOptions strokeWidth(float width) {
        this.PM = width;
        return this;
    }

    public CircleOptions visible(boolean visible) {
        this.PQ = visible;
        return this;
    }

    public void writeToParcel(Parcel out, int flags) {
        if (r.hc()) {
            b.a(this, out, flags);
        } else {
            CircleOptionsCreator.a(this, out, flags);
        }
    }

    public CircleOptions zIndex(float zIndex) {
        this.PP = zIndex;
        return this;
    }
}
