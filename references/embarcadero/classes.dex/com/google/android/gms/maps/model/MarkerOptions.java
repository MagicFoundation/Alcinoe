package com.google.android.gms.maps.model;

import android.os.IBinder;
import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.dynamic.b.a;
import com.google.android.gms.maps.internal.r;

public final class MarkerOptions implements SafeParcelable {
    public static final MarkerOptionsCreator CREATOR;
    private String CX;
    private boolean PQ;
    private float PY;
    private float PZ;
    private LatLng Qf;
    private String Qg;
    private BitmapDescriptor Qh;
    private boolean Qi;
    private boolean Qj;
    private float Qk;
    private float Ql;
    private float Qm;
    private float mAlpha;
    private final int wj;

    static {
        CREATOR = new MarkerOptionsCreator();
    }

    public MarkerOptions() {
        this.PY = 0.5f;
        this.PZ = 1.0f;
        this.PQ = true;
        this.Qj = false;
        this.Qk = 0.0f;
        this.Ql = 0.5f;
        this.Qm = 0.0f;
        this.mAlpha = 1.0f;
        this.wj = 1;
    }

    MarkerOptions(int versionCode, LatLng position, String title, String snippet, IBinder wrappedIcon, float anchorU, float anchorV, boolean draggable, boolean visible, boolean flat, float rotation, float infoWindowAnchorU, float infoWindowAnchorV, float alpha) {
        this.PY = 0.5f;
        this.PZ = 1.0f;
        this.PQ = true;
        this.Qj = false;
        this.Qk = 0.0f;
        this.Ql = 0.5f;
        this.Qm = 0.0f;
        this.mAlpha = 1.0f;
        this.wj = versionCode;
        this.Qf = position;
        this.CX = title;
        this.Qg = snippet;
        this.Qh = wrappedIcon == null ? null : new BitmapDescriptor(a.G(wrappedIcon));
        this.PY = anchorU;
        this.PZ = anchorV;
        this.Qi = draggable;
        this.PQ = visible;
        this.Qj = flat;
        this.Qk = rotation;
        this.Ql = infoWindowAnchorU;
        this.Qm = infoWindowAnchorV;
        this.mAlpha = alpha;
    }

    public MarkerOptions alpha(float alpha) {
        this.mAlpha = alpha;
        return this;
    }

    public MarkerOptions anchor(float u, float v) {
        this.PY = u;
        this.PZ = v;
        return this;
    }

    public int describeContents() {
        return 0;
    }

    public MarkerOptions draggable(boolean draggable) {
        this.Qi = draggable;
        return this;
    }

    public MarkerOptions flat(boolean flat) {
        this.Qj = flat;
        return this;
    }

    public float getAlpha() {
        return this.mAlpha;
    }

    public float getAnchorU() {
        return this.PY;
    }

    public float getAnchorV() {
        return this.PZ;
    }

    public BitmapDescriptor getIcon() {
        return this.Qh;
    }

    public float getInfoWindowAnchorU() {
        return this.Ql;
    }

    public float getInfoWindowAnchorV() {
        return this.Qm;
    }

    public LatLng getPosition() {
        return this.Qf;
    }

    public float getRotation() {
        return this.Qk;
    }

    public String getSnippet() {
        return this.Qg;
    }

    public String getTitle() {
        return this.CX;
    }

    int getVersionCode() {
        return this.wj;
    }

    IBinder hf() {
        return this.Qh == null ? null : this.Qh.gK().asBinder();
    }

    public MarkerOptions icon(BitmapDescriptor icon) {
        this.Qh = icon;
        return this;
    }

    public MarkerOptions infoWindowAnchor(float u, float v) {
        this.Ql = u;
        this.Qm = v;
        return this;
    }

    public boolean isDraggable() {
        return this.Qi;
    }

    public boolean isFlat() {
        return this.Qj;
    }

    public boolean isVisible() {
        return this.PQ;
    }

    public MarkerOptions position(LatLng position) {
        this.Qf = position;
        return this;
    }

    public MarkerOptions rotation(float rotation) {
        this.Qk = rotation;
        return this;
    }

    public MarkerOptions snippet(String snippet) {
        this.Qg = snippet;
        return this;
    }

    public MarkerOptions title(String title) {
        this.CX = title;
        return this;
    }

    public MarkerOptions visible(boolean visible) {
        this.PQ = visible;
        return this;
    }

    public void writeToParcel(Parcel out, int flags) {
        if (r.hc()) {
            f.a(this, out, flags);
        } else {
            MarkerOptionsCreator.a(this, out, flags);
        }
    }
}
