package com.google.android.gms.maps;

import android.os.RemoteException;
import com.google.android.gms.maps.internal.IUiSettingsDelegate;
import com.google.android.gms.maps.model.RuntimeRemoteException;

public final class UiSettings {
    private final IUiSettingsDelegate PA;

    UiSettings(IUiSettingsDelegate delegate) {
        this.PA = delegate;
    }

    public boolean isCompassEnabled() {
        try {
            return this.PA.isCompassEnabled();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public boolean isMyLocationButtonEnabled() {
        try {
            return this.PA.isMyLocationButtonEnabled();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public boolean isRotateGesturesEnabled() {
        try {
            return this.PA.isRotateGesturesEnabled();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public boolean isScrollGesturesEnabled() {
        try {
            return this.PA.isScrollGesturesEnabled();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public boolean isTiltGesturesEnabled() {
        try {
            return this.PA.isTiltGesturesEnabled();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public boolean isZoomControlsEnabled() {
        try {
            return this.PA.isZoomControlsEnabled();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public boolean isZoomGesturesEnabled() {
        try {
            return this.PA.isZoomGesturesEnabled();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public void setAllGesturesEnabled(boolean enabled) {
        try {
            this.PA.setAllGesturesEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public void setCompassEnabled(boolean enabled) {
        try {
            this.PA.setCompassEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public void setMyLocationButtonEnabled(boolean enabled) {
        try {
            this.PA.setMyLocationButtonEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public void setRotateGesturesEnabled(boolean enabled) {
        try {
            this.PA.setRotateGesturesEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public void setScrollGesturesEnabled(boolean enabled) {
        try {
            this.PA.setScrollGesturesEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public void setTiltGesturesEnabled(boolean enabled) {
        try {
            this.PA.setTiltGesturesEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public void setZoomControlsEnabled(boolean enabled) {
        try {
            this.PA.setZoomControlsEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public void setZoomGesturesEnabled(boolean enabled) {
        try {
            this.PA.setZoomGesturesEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }
}
