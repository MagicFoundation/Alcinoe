package com.google.android.gms.cast;

import android.os.Bundle;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.images.WebImage;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.dr;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class CastDevice implements SafeParcelable {
    public static final Creator<CastDevice> CREATOR;
    private String wC;
    String wD;
    private Inet4Address wE;
    private String wF;
    private String wG;
    private String wH;
    private int wI;
    private List<WebImage> wJ;
    private final int wj;

    static {
        CREATOR = new b();
    }

    private CastDevice() {
        this(1, null, null, null, null, null, -1, new ArrayList());
    }

    CastDevice(int versionCode, String deviceId, String hostAddress, String friendlyName, String modelName, String deviceVersion, int servicePort, List<WebImage> icons) {
        this.wj = versionCode;
        this.wC = deviceId;
        this.wD = hostAddress;
        if (this.wD != null) {
            try {
                InetAddress byName = InetAddress.getByName(this.wD);
                if (byName instanceof Inet4Address) {
                    this.wE = (Inet4Address) byName;
                }
            } catch (UnknownHostException e) {
                this.wE = null;
            }
        }
        this.wF = friendlyName;
        this.wG = modelName;
        this.wH = deviceVersion;
        this.wI = servicePort;
        this.wJ = icons;
    }

    public static CastDevice getFromBundle(Bundle extras) {
        if (extras == null) {
            return null;
        }
        extras.setClassLoader(CastDevice.class.getClassLoader());
        return (CastDevice) extras.getParcelable("com.google.android.gms.cast.EXTRA_CAST_DEVICE");
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (!(obj instanceof CastDevice)) {
            return false;
        }
        CastDevice castDevice = (CastDevice) obj;
        return getDeviceId() == null ? castDevice.getDeviceId() == null : dr.a(this.wC, castDevice.wC) && dr.a(this.wE, castDevice.wE) && dr.a(this.wG, castDevice.wG) && dr.a(this.wF, castDevice.wF) && dr.a(this.wH, castDevice.wH) && this.wI == castDevice.wI && dr.a(this.wJ, castDevice.wJ);
    }

    public String getDeviceId() {
        return this.wC;
    }

    public String getDeviceVersion() {
        return this.wH;
    }

    public String getFriendlyName() {
        return this.wF;
    }

    public WebImage getIcon(int preferredWidth, int preferredHeight) {
        WebImage webImage = null;
        if (this.wJ.isEmpty()) {
            return null;
        }
        if (preferredWidth <= 0 || preferredHeight <= 0) {
            return (WebImage) this.wJ.get(0);
        }
        WebImage webImage2 = null;
        for (WebImage webImage3 : this.wJ) {
            WebImage webImage32;
            int width = webImage32.getWidth();
            int height = webImage32.getHeight();
            if (width < preferredWidth || height < preferredHeight) {
                if (width < preferredWidth && height < preferredHeight && (webImage == null || (webImage.getWidth() < width && webImage.getHeight() < height))) {
                    webImage = webImage2;
                }
                webImage32 = webImage;
                webImage = webImage2;
            } else {
                if (webImage2 == null || (webImage2.getWidth() > width && webImage2.getHeight() > height)) {
                    WebImage webImage4 = webImage;
                    webImage = webImage32;
                    webImage32 = webImage4;
                }
                webImage32 = webImage;
                webImage = webImage2;
            }
            webImage2 = webImage;
            webImage = webImage32;
        }
        if (webImage2 == null) {
            webImage2 = webImage != null ? webImage : (WebImage) this.wJ.get(0);
        }
        return webImage2;
    }

    public List<WebImage> getIcons() {
        return Collections.unmodifiableList(this.wJ);
    }

    public Inet4Address getIpAddress() {
        return this.wE;
    }

    public String getModelName() {
        return this.wG;
    }

    public int getServicePort() {
        return this.wI;
    }

    int getVersionCode() {
        return this.wj;
    }

    public boolean hasIcons() {
        return !this.wJ.isEmpty();
    }

    public int hashCode() {
        return this.wC == null ? 0 : this.wC.hashCode();
    }

    public boolean isSameDevice(CastDevice castDevice) {
        if (castDevice == null) {
            return false;
        }
        if (getDeviceId() == null) {
            return castDevice.getDeviceId() == null;
        } else {
            return dr.a(getDeviceId(), castDevice.getDeviceId());
        }
    }

    public void putInBundle(Bundle bundle) {
        if (bundle != null) {
            bundle.putParcelable("com.google.android.gms.cast.EXTRA_CAST_DEVICE", this);
        }
    }

    public String toString() {
        return String.format("\"%s\" (%s)", new Object[]{this.wF, this.wC});
    }

    public void writeToParcel(Parcel out, int flags) {
        b.a(this, out, flags);
    }
}
