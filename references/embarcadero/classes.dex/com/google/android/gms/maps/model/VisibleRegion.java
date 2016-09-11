package com.google.android.gms.maps.model;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.ep;
import com.google.android.gms.maps.internal.r;

public final class VisibleRegion implements SafeParcelable {
    public static final VisibleRegionCreator CREATOR;
    public final LatLng farLeft;
    public final LatLng farRight;
    public final LatLngBounds latLngBounds;
    public final LatLng nearLeft;
    public final LatLng nearRight;
    private final int wj;

    static {
        CREATOR = new VisibleRegionCreator();
    }

    VisibleRegion(int versionCode, LatLng nearLeft, LatLng nearRight, LatLng farLeft, LatLng farRight, LatLngBounds latLngBounds) {
        this.wj = versionCode;
        this.nearLeft = nearLeft;
        this.nearRight = nearRight;
        this.farLeft = farLeft;
        this.farRight = farRight;
        this.latLngBounds = latLngBounds;
    }

    public VisibleRegion(LatLng nearLeft, LatLng nearRight, LatLng farLeft, LatLng farRight, LatLngBounds latLngBounds) {
        this(1, nearLeft, nearRight, farLeft, farRight, latLngBounds);
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof VisibleRegion)) {
            return false;
        }
        VisibleRegion visibleRegion = (VisibleRegion) o;
        return this.nearLeft.equals(visibleRegion.nearLeft) && this.nearRight.equals(visibleRegion.nearRight) && this.farLeft.equals(visibleRegion.farLeft) && this.farRight.equals(visibleRegion.farRight) && this.latLngBounds.equals(visibleRegion.latLngBounds);
    }

    int getVersionCode() {
        return this.wj;
    }

    public int hashCode() {
        return ep.hashCode(this.nearLeft, this.nearRight, this.farLeft, this.farRight, this.latLngBounds);
    }

    public String toString() {
        return ep.e(this).a("nearLeft", this.nearLeft).a("nearRight", this.nearRight).a("farLeft", this.farLeft).a("farRight", this.farRight).a("latLngBounds", this.latLngBounds).toString();
    }

    public void writeToParcel(Parcel out, int flags) {
        if (r.hc()) {
            k.a(this, out, flags);
        } else {
            VisibleRegionCreator.a(this, out, flags);
        }
    }
}
