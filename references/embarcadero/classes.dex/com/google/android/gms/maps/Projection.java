package com.google.android.gms.maps;

import android.graphics.Point;
import android.os.RemoteException;
import com.google.android.gms.dynamic.c;
import com.google.android.gms.maps.internal.IProjectionDelegate;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.RuntimeRemoteException;
import com.google.android.gms.maps.model.VisibleRegion;

public final class Projection {
    private final IProjectionDelegate Px;

    Projection(IProjectionDelegate delegate) {
        this.Px = delegate;
    }

    public LatLng fromScreenLocation(Point point) {
        try {
            return this.Px.fromScreenLocation(c.h(point));
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public VisibleRegion getVisibleRegion() {
        try {
            return this.Px.getVisibleRegion();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public Point toScreenLocation(LatLng location) {
        try {
            return (Point) c.b(this.Px.toScreenLocation(location));
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }
}
