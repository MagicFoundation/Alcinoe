package com.google.android.gms.maps.model;

import android.content.Context;
import android.content.res.TypedArray;
import android.os.Parcel;
import android.util.AttributeSet;
import com.google.android.gms.R;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.ep;
import com.google.android.gms.internal.er;
import com.google.android.gms.maps.internal.r;

public final class CameraPosition implements SafeParcelable {
    public static final CameraPositionCreator CREATOR;
    public final float bearing;
    public final LatLng target;
    public final float tilt;
    private final int wj;
    public final float zoom;

    public static final class Builder {
        private LatLng PF;
        private float PG;
        private float PH;
        private float PI;

        public Builder(CameraPosition previous) {
            this.PF = previous.target;
            this.PG = previous.zoom;
            this.PH = previous.tilt;
            this.PI = previous.bearing;
        }

        public Builder bearing(float bearing) {
            this.PI = bearing;
            return this;
        }

        public CameraPosition build() {
            return new CameraPosition(this.PF, this.PG, this.PH, this.PI);
        }

        public Builder target(LatLng location) {
            this.PF = location;
            return this;
        }

        public Builder tilt(float tilt) {
            this.PH = tilt;
            return this;
        }

        public Builder zoom(float zoom) {
            this.PG = zoom;
            return this;
        }
    }

    static {
        CREATOR = new CameraPositionCreator();
    }

    CameraPosition(int versionCode, LatLng target, float zoom, float tilt, float bearing) {
        er.b((Object) target, (Object) "null camera target");
        boolean z = 0.0f <= tilt && tilt <= 90.0f;
        er.b(z, (Object) "Tilt needs to be between 0 and 90 inclusive");
        this.wj = versionCode;
        this.target = target;
        this.zoom = zoom;
        this.tilt = tilt + 0.0f;
        if (((double) bearing) <= 0.0d) {
            bearing = (bearing % 360.0f) + 360.0f;
        }
        this.bearing = bearing % 360.0f;
    }

    public CameraPosition(LatLng target, float zoom, float tilt, float bearing) {
        this(1, target, zoom, tilt, bearing);
    }

    public static Builder builder() {
        return new Builder();
    }

    public static Builder builder(CameraPosition camera) {
        return new Builder(camera);
    }

    public static CameraPosition createFromAttributes(Context context, AttributeSet attrs) {
        if (attrs == null) {
            return null;
        }
        TypedArray obtainAttributes = context.getResources().obtainAttributes(attrs, R.styleable.MapAttrs);
        LatLng latLng = new LatLng((double) (obtainAttributes.hasValue(2) ? obtainAttributes.getFloat(2, 0.0f) : 0.0f), (double) (obtainAttributes.hasValue(3) ? obtainAttributes.getFloat(3, 0.0f) : 0.0f));
        Builder builder = builder();
        builder.target(latLng);
        if (obtainAttributes.hasValue(5)) {
            builder.zoom(obtainAttributes.getFloat(5, 0.0f));
        }
        if (obtainAttributes.hasValue(1)) {
            builder.bearing(obtainAttributes.getFloat(1, 0.0f));
        }
        if (obtainAttributes.hasValue(4)) {
            builder.tilt(obtainAttributes.getFloat(4, 0.0f));
        }
        return builder.build();
    }

    public static final CameraPosition fromLatLngZoom(LatLng target, float zoom) {
        return new CameraPosition(target, zoom, 0.0f, 0.0f);
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof CameraPosition)) {
            return false;
        }
        CameraPosition cameraPosition = (CameraPosition) o;
        return this.target.equals(cameraPosition.target) && Float.floatToIntBits(this.zoom) == Float.floatToIntBits(cameraPosition.zoom) && Float.floatToIntBits(this.tilt) == Float.floatToIntBits(cameraPosition.tilt) && Float.floatToIntBits(this.bearing) == Float.floatToIntBits(cameraPosition.bearing);
    }

    int getVersionCode() {
        return this.wj;
    }

    public int hashCode() {
        return ep.hashCode(this.target, Float.valueOf(this.zoom), Float.valueOf(this.tilt), Float.valueOf(this.bearing));
    }

    public String toString() {
        return ep.e(this).a("target", this.target).a("zoom", Float.valueOf(this.zoom)).a("tilt", Float.valueOf(this.tilt)).a("bearing", Float.valueOf(this.bearing)).toString();
    }

    public void writeToParcel(Parcel out, int flags) {
        if (r.hc()) {
            a.a(this, out, flags);
        } else {
            CameraPositionCreator.a(this, out, flags);
        }
    }
}
