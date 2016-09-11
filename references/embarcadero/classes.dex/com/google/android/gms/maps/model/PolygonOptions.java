package com.google.android.gms.maps.model;

import android.os.Parcel;
import android.support.v4.view.ViewCompat;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.maps.internal.r;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class PolygonOptions implements SafeParcelable {
    public static final PolygonOptionsCreator CREATOR;
    private float PM;
    private int PN;
    private int PO;
    private float PP;
    private boolean PQ;
    private final List<LatLng> Qo;
    private final List<List<LatLng>> Qp;
    private boolean Qq;
    private final int wj;

    static {
        CREATOR = new PolygonOptionsCreator();
    }

    public PolygonOptions() {
        this.PM = 10.0f;
        this.PN = ViewCompat.MEASURED_STATE_MASK;
        this.PO = 0;
        this.PP = 0.0f;
        this.PQ = true;
        this.Qq = false;
        this.wj = 1;
        this.Qo = new ArrayList();
        this.Qp = new ArrayList();
    }

    PolygonOptions(int versionCode, List<LatLng> points, List holes, float strokeWidth, int strokeColor, int fillColor, float zIndex, boolean visible, boolean geodesic) {
        this.PM = 10.0f;
        this.PN = ViewCompat.MEASURED_STATE_MASK;
        this.PO = 0;
        this.PP = 0.0f;
        this.PQ = true;
        this.Qq = false;
        this.wj = versionCode;
        this.Qo = points;
        this.Qp = holes;
        this.PM = strokeWidth;
        this.PN = strokeColor;
        this.PO = fillColor;
        this.PP = zIndex;
        this.PQ = visible;
        this.Qq = geodesic;
    }

    public PolygonOptions add(LatLng point) {
        this.Qo.add(point);
        return this;
    }

    public PolygonOptions add(LatLng... points) {
        this.Qo.addAll(Arrays.asList(points));
        return this;
    }

    public PolygonOptions addAll(Iterable<LatLng> points) {
        for (LatLng add : points) {
            this.Qo.add(add);
        }
        return this;
    }

    public PolygonOptions addHole(Iterable<LatLng> points) {
        ArrayList arrayList = new ArrayList();
        for (LatLng add : points) {
            arrayList.add(add);
        }
        this.Qp.add(arrayList);
        return this;
    }

    public int describeContents() {
        return 0;
    }

    public PolygonOptions fillColor(int color) {
        this.PO = color;
        return this;
    }

    public PolygonOptions geodesic(boolean geodesic) {
        this.Qq = geodesic;
        return this;
    }

    public int getFillColor() {
        return this.PO;
    }

    public List<List<LatLng>> getHoles() {
        return this.Qp;
    }

    public List<LatLng> getPoints() {
        return this.Qo;
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

    List hg() {
        return this.Qp;
    }

    public boolean isGeodesic() {
        return this.Qq;
    }

    public boolean isVisible() {
        return this.PQ;
    }

    public PolygonOptions strokeColor(int color) {
        this.PN = color;
        return this;
    }

    public PolygonOptions strokeWidth(float width) {
        this.PM = width;
        return this;
    }

    public PolygonOptions visible(boolean visible) {
        this.PQ = visible;
        return this;
    }

    public void writeToParcel(Parcel out, int flags) {
        if (r.hc()) {
            g.a(this, out, flags);
        } else {
            PolygonOptionsCreator.a(this, out, flags);
        }
    }

    public PolygonOptions zIndex(float zIndex) {
        this.PP = zIndex;
        return this;
    }
}
