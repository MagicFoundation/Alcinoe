package com.google.android.gms.cast;

import com.google.android.gms.internal.dr;
import org.json.JSONException;
import org.json.JSONObject;

public final class MediaStatus {
    public static final long COMMAND_PAUSE = 1;
    public static final long COMMAND_SEEK = 2;
    public static final long COMMAND_SET_VOLUME = 4;
    public static final long COMMAND_SKIP_BACKWARD = 32;
    public static final long COMMAND_SKIP_FORWARD = 16;
    public static final long COMMAND_TOGGLE_MUTE = 8;
    public static final int IDLE_REASON_CANCELED = 2;
    public static final int IDLE_REASON_ERROR = 4;
    public static final int IDLE_REASON_FINISHED = 1;
    public static final int IDLE_REASON_INTERRUPTED = 3;
    public static final int IDLE_REASON_NONE = 0;
    public static final int PLAYER_STATE_BUFFERING = 4;
    public static final int PLAYER_STATE_IDLE = 1;
    public static final int PLAYER_STATE_PAUSED = 3;
    public static final int PLAYER_STATE_PLAYING = 2;
    public static final int PLAYER_STATE_UNKNOWN = 0;
    private JSONObject wP;
    private MediaInfo wQ;
    private long wY;
    private double wZ;
    private int xa;
    private int xb;
    private long xc;
    private long xd;
    private double xe;
    private boolean xf;

    public MediaStatus(JSONObject json) throws JSONException {
        a(json, IDLE_REASON_NONE);
    }

    public int a(JSONObject jSONObject, int i) throws JSONException {
        int i2;
        long b;
        int i3 = PLAYER_STATE_PLAYING;
        long j = jSONObject.getLong("mediaSessionId");
        if (j != this.wY) {
            this.wY = j;
            i2 = PLAYER_STATE_IDLE;
        } else {
            i2 = IDLE_REASON_NONE;
        }
        if (jSONObject.has("playerState")) {
            String string = jSONObject.getString("playerState");
            int i4 = string.equals("IDLE") ? PLAYER_STATE_IDLE : string.equals("PLAYING") ? PLAYER_STATE_PLAYING : string.equals("PAUSED") ? PLAYER_STATE_PAUSED : string.equals("BUFFERING") ? PLAYER_STATE_BUFFERING : IDLE_REASON_NONE;
            if (i4 != this.xa) {
                this.xa = i4;
                i2 |= PLAYER_STATE_PLAYING;
            }
            if (i4 == PLAYER_STATE_IDLE && jSONObject.has("idleReason")) {
                string = jSONObject.getString("idleReason");
                if (!string.equals("CANCELLED")) {
                    i3 = string.equals("INTERRUPTED") ? PLAYER_STATE_PAUSED : string.equals("FINISHED") ? PLAYER_STATE_IDLE : string.equals("ERROR") ? PLAYER_STATE_BUFFERING : IDLE_REASON_NONE;
                }
                if (i3 != this.xb) {
                    this.xb = i3;
                    i2 |= PLAYER_STATE_PLAYING;
                }
            }
        }
        if (jSONObject.has("playbackRate")) {
            double d = jSONObject.getDouble("playbackRate");
            if (this.wZ != d) {
                this.wZ = d;
                i2 |= PLAYER_STATE_PLAYING;
            }
        }
        if (jSONObject.has("currentTime") && (i & PLAYER_STATE_PLAYING) == 0) {
            b = dr.b(jSONObject.getDouble("currentTime"));
            if (b != this.xc) {
                this.xc = b;
                i2 |= PLAYER_STATE_PLAYING;
            }
        }
        if (jSONObject.has("supportedMediaCommands")) {
            b = jSONObject.getLong("supportedMediaCommands");
            if (b != this.xd) {
                this.xd = b;
                i2 |= PLAYER_STATE_PLAYING;
            }
        }
        if (jSONObject.has("volume") && (i & PLAYER_STATE_IDLE) == 0) {
            JSONObject jSONObject2 = jSONObject.getJSONObject("volume");
            double d2 = jSONObject2.getDouble("level");
            if (d2 != this.xe) {
                this.xe = d2;
                i2 |= PLAYER_STATE_PLAYING;
            }
            boolean z = jSONObject2.getBoolean("muted");
            if (z != this.xf) {
                this.xf = z;
                i2 |= PLAYER_STATE_PLAYING;
            }
        }
        if (jSONObject.has("customData")) {
            this.wP = jSONObject.getJSONObject("customData");
            i2 |= PLAYER_STATE_PLAYING;
        }
        if (!jSONObject.has("media")) {
            return i2;
        }
        jSONObject2 = jSONObject.getJSONObject("media");
        this.wQ = new MediaInfo(jSONObject2);
        i2 |= PLAYER_STATE_PLAYING;
        return jSONObject2.has("metadata") ? i2 | PLAYER_STATE_BUFFERING : i2;
    }

    public long cU() {
        return this.wY;
    }

    public JSONObject getCustomData() {
        return this.wP;
    }

    public int getIdleReason() {
        return this.xb;
    }

    public MediaInfo getMediaInfo() {
        return this.wQ;
    }

    public double getPlaybackRate() {
        return this.wZ;
    }

    public int getPlayerState() {
        return this.xa;
    }

    public long getStreamPosition() {
        return this.xc;
    }

    public double getStreamVolume() {
        return this.xe;
    }

    public boolean isMediaCommandSupported(long mediaCommand) {
        return (this.xd & mediaCommand) != 0;
    }

    public boolean isMute() {
        return this.xf;
    }
}
