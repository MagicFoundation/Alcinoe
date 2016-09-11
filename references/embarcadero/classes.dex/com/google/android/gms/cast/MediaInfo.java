package com.google.android.gms.cast;

import android.text.TextUtils;
import com.google.android.gms.internal.dr;
import com.google.android.gms.internal.ep;
import com.google.android.gms.internal.fp;
import org.json.JSONException;
import org.json.JSONObject;

public final class MediaInfo {
    public static final int STREAM_TYPE_BUFFERED = 1;
    public static final int STREAM_TYPE_INVALID = -1;
    public static final int STREAM_TYPE_LIVE = 2;
    public static final int STREAM_TYPE_NONE = 0;
    private final String wK;
    private int wL;
    private String wM;
    private MediaMetadata wN;
    private long wO;
    private JSONObject wP;

    public static class Builder {
        private final MediaInfo wQ;

        public Builder(String contentId) throws IllegalArgumentException {
            if (TextUtils.isEmpty(contentId)) {
                throw new IllegalArgumentException("Content ID cannot be empty");
            }
            this.wQ = new MediaInfo(contentId);
        }

        public MediaInfo build() throws IllegalArgumentException {
            this.wQ.cS();
            return this.wQ;
        }

        public Builder setContentType(String contentType) throws IllegalArgumentException {
            this.wQ.setContentType(contentType);
            return this;
        }

        public Builder setCustomData(JSONObject customData) {
            this.wQ.a(customData);
            return this;
        }

        public Builder setMetadata(MediaMetadata metadata) {
            this.wQ.a(metadata);
            return this;
        }

        public Builder setStreamDuration(long duration) throws IllegalArgumentException {
            this.wQ.j(duration);
            return this;
        }

        public Builder setStreamType(int streamType) throws IllegalArgumentException {
            this.wQ.setStreamType(streamType);
            return this;
        }
    }

    MediaInfo(String contentId) throws IllegalArgumentException {
        if (TextUtils.isEmpty(contentId)) {
            throw new IllegalArgumentException("content ID cannot be null or empty");
        }
        this.wK = contentId;
        this.wL = STREAM_TYPE_INVALID;
    }

    MediaInfo(JSONObject json) throws JSONException {
        this.wK = json.getString("contentId");
        String string = json.getString("streamType");
        if ("NONE".equals(string)) {
            this.wL = 0;
        } else if ("BUFFERED".equals(string)) {
            this.wL = STREAM_TYPE_BUFFERED;
        } else if ("LIVE".equals(string)) {
            this.wL = STREAM_TYPE_LIVE;
        } else {
            this.wL = STREAM_TYPE_INVALID;
        }
        this.wM = json.getString("contentType");
        if (json.has("metadata")) {
            JSONObject jSONObject = json.getJSONObject("metadata");
            this.wN = new MediaMetadata(jSONObject.getInt("metadataType"));
            this.wN.b(jSONObject);
        }
        this.wO = dr.b(json.optDouble("duration", 0.0d));
        this.wP = json.optJSONObject("customData");
    }

    void a(MediaMetadata mediaMetadata) {
        this.wN = mediaMetadata;
    }

    void a(JSONObject jSONObject) {
        this.wP = jSONObject;
    }

    void cS() throws IllegalArgumentException {
        if (TextUtils.isEmpty(this.wK)) {
            throw new IllegalArgumentException("content ID cannot be null or empty");
        } else if (TextUtils.isEmpty(this.wM)) {
            throw new IllegalArgumentException("content type cannot be null or empty");
        } else if (this.wL == STREAM_TYPE_INVALID) {
            throw new IllegalArgumentException("a valid stream type must be specified");
        }
    }

    public JSONObject cT() {
        JSONObject jSONObject = new JSONObject();
        try {
            Object obj;
            jSONObject.put("contentId", this.wK);
            switch (this.wL) {
                case STREAM_TYPE_BUFFERED /*1*/:
                    obj = "BUFFERED";
                    break;
                case STREAM_TYPE_LIVE /*2*/:
                    obj = "LIVE";
                    break;
                default:
                    obj = "NONE";
                    break;
            }
            jSONObject.put("streamType", obj);
            if (this.wM != null) {
                jSONObject.put("contentType", this.wM);
            }
            if (this.wN != null) {
                jSONObject.put("metadata", this.wN.cT());
            }
            jSONObject.put("duration", dr.l(this.wO));
            if (this.wP != null) {
                jSONObject.put("customData", this.wP);
            }
        } catch (JSONException e) {
        }
        return jSONObject;
    }

    public boolean equals(Object other) {
        boolean z = true;
        if (this == other) {
            return true;
        }
        if (!(other instanceof MediaInfo)) {
            return false;
        }
        MediaInfo mediaInfo = (MediaInfo) other;
        if ((this.wP == null ? STREAM_TYPE_BUFFERED : false) != (mediaInfo.wP == null ? STREAM_TYPE_BUFFERED : false)) {
            return false;
        }
        if (this.wP != null && mediaInfo.wP != null && !fp.d(this.wP, mediaInfo.wP)) {
            return false;
        }
        if (!(dr.a(this.wK, mediaInfo.wK) && this.wL == mediaInfo.wL && dr.a(this.wM, mediaInfo.wM) && dr.a(this.wN, mediaInfo.wN) && this.wO == mediaInfo.wO)) {
            z = false;
        }
        return z;
    }

    public String getContentId() {
        return this.wK;
    }

    public String getContentType() {
        return this.wM;
    }

    public JSONObject getCustomData() {
        return this.wP;
    }

    public MediaMetadata getMetadata() {
        return this.wN;
    }

    public long getStreamDuration() {
        return this.wO;
    }

    public int getStreamType() {
        return this.wL;
    }

    public int hashCode() {
        return ep.hashCode(this.wK, Integer.valueOf(this.wL), this.wM, this.wN, Long.valueOf(this.wO), String.valueOf(this.wP));
    }

    void j(long j) throws IllegalArgumentException {
        if (j < 0) {
            throw new IllegalArgumentException("Stream duration cannot be negative");
        }
        this.wO = j;
    }

    void setContentType(String contentType) throws IllegalArgumentException {
        if (TextUtils.isEmpty(contentType)) {
            throw new IllegalArgumentException("content type cannot be null or empty");
        }
        this.wM = contentType;
    }

    void setStreamType(int streamType) throws IllegalArgumentException {
        if (streamType < STREAM_TYPE_INVALID || streamType > STREAM_TYPE_LIVE) {
            throw new IllegalArgumentException("invalid stream type");
        }
        this.wL = streamType;
    }
}
