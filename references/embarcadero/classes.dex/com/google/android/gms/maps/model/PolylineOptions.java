package com.google.android.gms.maps.model;

import android.os.Parcel;
import android.support.v4.view.ViewCompat;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.maps.internal.r;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class PolylineOptions implements SafeParcelable {
    public static final PolylineOptionsCreator CREATOR;
    private float PP;
    private boolean PQ;
    private float PU;
    private final List<LatLng> Qo;
    private boolean Qq;
    private final int wj;
    private int yX;

    static {
        CREATOR = new PolylineOptionsCreator();
    }

    public PolylineOptions() {
        this.PU = 10.0f;
        this.yX = ViewCompat.MEASURED_STATE_MASK;
        this.PP = 0.0f;
        this.PQ = true;
        this.Qq = false;
        this.wj = 1;
        this.Qo = new ArrayList();
    }

    PolylineOptions(int versionCode, List points, float width, int color, float zIndex, boolean visible, boolean geodesic) {
        this.PU = 10.0f;
        this.yX = ViewCompat.MEASURED_STATE_MASK;
        this.PP = 0.0f;
        this.PQ = true;
        this.Qq = false;
        this.wj = versionCode;
        this.Qo = points;
        this.PU = width;
        this.yX = color;
        this.PP = zIndex;
        this.PQ = visible;
        this.Qq = geodesic;
    }

    public PolylineOptions add(LatLng point) {
        this.Qo.add(point);
        return this;
    }

    public PolylineOptions add(LatLng... points) {
        this.Qo.addAll(Arrays.asList(points));
        return this;
    }

    public PolylineOptions addAll(Iterable<LatLng> points) {
        for (LatLng add : points) {
            this.Qo.add(add);
        }
        return this;
    }

    public PolylineOptions color(int color) {
        this.yX = color;
        return this;
    }

    public int describeContents() {
        return 0;
    }

    public PolylineOptions geodesic(boolean geodesic) {
        this.Qq = geodesic;
        return this;
    }

    public int getColor() {
        return this.yX;
    }

    public List<LatLng> getPoints() {
        return this.Qo;
    }

    int getVersionCode() {
        return this.wj;
    }

    public float getWidth() {
        return this.PU;
    }

    public float getZIndex() {
        return this.PP;
    }

    public boolean isGeodesic() {
        return this.Qq;
    }

    public boolean isVisible() {
        return this.PQ;
    }

    public PolylineOptions visible(boolean visible) {
        this.PQ = visible;
        return this;
    }

    public PolylineOptions width(float width) {
        this.PU = width;
        return this;
    }

    public void writeToParcel(Parcel out, int flags) {
        if (r.hc()) {
            h.a(this, out, flags);
        } else {
            PolylineOptionsCreator.a(this, out, flags);
        }
    }

    public PolylineOptions zIndex(float zIndex) {
        this.PP = zIndex;
        return this;
    }
}
