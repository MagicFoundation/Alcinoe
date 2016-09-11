package com.google.android.gms.common.images;

import android.net.Uri;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.ep;
import com.google.android.gms.plus.PlusShare;
import org.json.JSONException;
import org.json.JSONObject;

public final class WebImage implements SafeParcelable {
    public static final Creator<WebImage> CREATOR;
    private final Uri AR;
    private final int v;
    private final int w;
    private final int wj;

    static {
        CREATOR = new b();
    }

    WebImage(int versionCode, Uri url, int width, int height) {
        this.wj = versionCode;
        this.AR = url;
        this.w = width;
        this.v = height;
    }

    public WebImage(Uri url) throws IllegalArgumentException {
        this(url, 0, 0);
    }

    public WebImage(Uri url, int width, int height) throws IllegalArgumentException {
        this(1, url, width, height);
        if (url == null) {
            throw new IllegalArgumentException("url cannot be null");
        } else if (width < 0 || height < 0) {
            throw new IllegalArgumentException("width and height must not be negative");
        }
    }

    public WebImage(JSONObject json) throws IllegalArgumentException {
        this(c(json), json.optInt("width", 0), json.optInt("height", 0));
    }

    private static Uri c(JSONObject jSONObject) {
        Uri uri = null;
        if (jSONObject.has(PlusShare.KEY_CALL_TO_ACTION_URL)) {
            try {
                uri = Uri.parse(jSONObject.getString(PlusShare.KEY_CALL_TO_ACTION_URL));
            } catch (JSONException e) {
            }
        }
        return uri;
    }

    public JSONObject cT() {
        JSONObject jSONObject = new JSONObject();
        try {
            jSONObject.put(PlusShare.KEY_CALL_TO_ACTION_URL, this.AR.toString());
            jSONObject.put("width", this.w);
            jSONObject.put("height", this.v);
        } catch (JSONException e) {
        }
        return jSONObject;
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (other == null || !(other instanceof WebImage)) {
            return false;
        }
        WebImage webImage = (WebImage) other;
        return ep.equal(this.AR, webImage.AR) && this.w == webImage.w && this.v == webImage.v;
    }

    public int getHeight() {
        return this.v;
    }

    public Uri getUrl() {
        return this.AR;
    }

    int getVersionCode() {
        return this.wj;
    }

    public int getWidth() {
        return this.w;
    }

    public int hashCode() {
        return ep.hashCode(this.AR, Integer.valueOf(this.w), Integer.valueOf(this.v));
    }

    public String toString() {
        return String.format("Image %dx%d %s", new Object[]{Integer.valueOf(this.w), Integer.valueOf(this.v), this.AR.toString()});
    }

    public void writeToParcel(Parcel out, int flags) {
        b.a(this, out, flags);
    }
}
