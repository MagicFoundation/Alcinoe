package com.embarcadero.firemonkey.maps;

import android.content.Context;
import android.view.GestureDetector;
import android.view.GestureDetector.SimpleOnGestureListener;
import android.view.MotionEvent;
import com.google.android.gms.maps.GoogleMapOptions;
import com.google.android.gms.maps.MapView;

public class MapViewWithGestures extends MapView {
    private GestureDetector mDetector;

    public MapViewWithGestures(Context context, GoogleMapOptions options) {
        super(context, options);
        this.mDetector = new GestureDetector(context, new SimpleOnGestureListener(), getHandler());
    }

    public GestureDetector getGestureDetector() {
        return this.mDetector;
    }

    public boolean dispatchTouchEvent(MotionEvent ev) {
        return this.mDetector.onTouchEvent(ev) || super.dispatchTouchEvent(ev);
    }
}
