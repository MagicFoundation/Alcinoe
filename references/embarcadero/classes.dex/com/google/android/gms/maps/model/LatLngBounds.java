package com.google.android.gms.maps.model;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.ep;
import com.google.android.gms.internal.er;
import com.google.android.gms.maps.internal.r;

public final class LatLngBounds implements SafeParcelable {
    public static final LatLngBoundsCreator CREATOR;
    public final LatLng northeast;
    public final LatLng southwest;
    private final int wj;

    public static final class Builder {
        private double Qa;
        private double Qb;
        private double Qc;
        private double Qd;

        public Builder() {
            this.Qa = Double.POSITIVE_INFINITY;
            this.Qb = Double.NEGATIVE_INFINITY;
            this.Qc = Double.NaN;
            this.Qd = Double.NaN;
        }

        private boolean d(double d) {
            boolean z = false;
            if (this.Qc <= this.Qd) {
                return this.Qc <= d && d <= this.Qd;
            } else {
                if (this.Qc <= d || d <= this.Qd) {
                    z = true;
                }
                return z;
            }
        }

        public LatLngBounds build() {
            er.a(!Double.isNaN(this.Qc), "no included points");
            return new LatLngBounds(new LatLng(this.Qa, this.Qc), new LatLng(this.Qb, this.Qd));
        }

        public Builder include(LatLng point) {
            this.Qa = Math.min(this.Qa, point.latitude);
            this.Qb = Math.max(this.Qb, point.latitude);
            double d = point.longitude;
            if (Double.isNaN(this.Qc)) {
                this.Qc = d;
                this.Qd = d;
            } else if (!d(d)) {
                if (LatLngBounds.b(this.Qc, d) < LatLngBounds.c(this.Qd, d)) {
                    this.Qc = d;
                } else {
                    this.Qd = d;
                }
            }
            return this;
        }
    }

    static {
        CREATOR = new LatLngBoundsCreator();
    }

    LatLngBounds(int versionCode, LatLng southwest, LatLng northeast) {
        er.b((Object) southwest, (Object) "null southwest");
        er.b((Object) northeast, (Object) "null northeast");
        er.a(northeast.latitude >= southwest.latitude, "southern latitude exceeds northern latitude (%s > %s)", Double.valueOf(southwest.latitude), Double.valueOf(northeast.latitude));
        this.wj = versionCode;
        this.southwest = southwest;
        this.northeast = northeast;
    }

    public LatLngBounds(LatLng southwest, LatLng northeast) {
        this(1, southwest, northeast);
    }

    private static double b(double d, double d2) {
        return ((d - d2) + 360.0d) % 360.0d;
    }

    public static Builder builder() {
        return new Builder();
    }

    private static double c(double d, double d2) {
        return ((d2 - d) + 360.0d) % 360.0d;
    }

    private boolean c(double d) {
        return this.southwest.latitude <= d && d <= this.northeast.latitude;
    }

    private boolean d(double d) {
        boolean z = false;
        if (this.southwest.longitude <= this.northeast.longitude) {
            return this.southwest.longitude <= d && d <= this.northeast.longitude;
        } else {
            if (this.southwest.longitude <= d || d <= this.northeast.longitude) {
                z = true;
            }
            return z;
        }
    }

    public boolean contains(LatLng point) {
        return c(point.latitude) && d(point.longitude);
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof LatLngBounds)) {
            return false;
        }
        LatLngBounds latLngBounds = (LatLngBounds) o;
        return this.southwest.equals(latLngBounds.southwest) && this.northeast.equals(latLngBounds.northeast);
    }

    public LatLng getCenter() {
        double d = (this.southwest.latitude + this.northeast.latitude) / 2.0d;
        double d2 = this.northeast.longitude;
        double d3 = this.southwest.longitude;
        return new LatLng(d, d3 <= d2 ? (d2 + d3) / 2.0d : ((d2 + 360.0d) + d3) / 2.0d);
    }

    int getVersionCode() {
        return this.wj;
    }

    public int hashCode() {
        return ep.hashCode(this.southwest, this.northeast);
    }

    public LatLngBounds including(LatLng point) {
        double min = Math.min(this.southwest.latitude, point.latitude);
        double max = Math.max(this.northeast.latitude, point.latitude);
        double d = this.northeast.longitude;
        double d2 = this.southwest.longitude;
        double d3 = point.longitude;
        if (d(d3)) {
            d3 = d2;
            d2 = d;
        } else if (b(d2, d3) < c(d, d3)) {
            d2 = d;
        } else {
            double d4 = d2;
            d2 = d3;
            d3 = d4;
        }
        return new LatLngBounds(new LatLng(min, d3), new LatLng(max, d2));
    }

    public String toString() {
        return ep.e(this).a("southwest", this.southwest).a("northeast", this.northeast).toString();
    }

    public void writeToParcel(Parcel out, int flags) {
        if (r.hc()) {
            d.a(this, out, flags);
        } else {
            LatLngBoundsCreator.a(this, out, flags);
        }
    }
}
