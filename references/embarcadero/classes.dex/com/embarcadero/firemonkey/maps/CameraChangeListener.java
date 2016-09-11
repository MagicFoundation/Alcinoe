package com.embarcadero.firemonkey.maps;

import com.google.android.gms.maps.GoogleMap.OnCameraChangeListener;
import com.google.android.gms.maps.model.CameraPosition;

public class CameraChangeListener implements OnCameraChangeListener {
    private Callback mCallback;
    private CameraPosition mPosition;

    public interface Callback {
        void onCameraChange(CameraChangeListener cameraChangeListener);
    }

    public void onCameraChange(CameraPosition position) {
        this.mPosition = position;
        if (this.mCallback != null) {
            this.mCallback.onCameraChange(this);
        }
    }

    public void setCallback(Callback callback) {
        this.mCallback = callback;
    }

    public float getTilt() {
        return this.mPosition == null ? 0.0f : this.mPosition.tilt;
    }

    public float getZoom() {
        return this.mPosition == null ? 0.0f : this.mPosition.zoom;
    }

    public float getBearing() {
        return this.mPosition == null ? 0.0f : this.mPosition.bearing;
    }

    public double getLatitude() {
        return this.mPosition == null ? 0.0d : this.mPosition.target.latitude;
    }

    public double getLongitude() {
        return this.mPosition == null ? 0.0d : this.mPosition.target.longitude;
    }
}
