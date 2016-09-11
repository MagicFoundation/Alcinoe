package com.google.android.gms.cast;

import com.google.android.gms.cast.Cast.MessageReceivedCallback;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.internal.dq;
import com.google.android.gms.internal.dv;
import com.google.android.gms.internal.dw;
import com.google.android.gms.internal.dx;
import java.io.IOException;
import org.json.JSONObject;

public class RemoteMediaPlayer implements MessageReceivedCallback {
    public static final int RESUME_STATE_PAUSE = 2;
    public static final int RESUME_STATE_PLAY = 1;
    public static final int RESUME_STATE_UNCHANGED = 0;
    public static final int STATUS_CANCELED = 2;
    public static final int STATUS_FAILED = 1;
    public static final int STATUS_REPLACED = 4;
    public static final int STATUS_SUCCEEDED = 0;
    public static final int STATUS_TIMED_OUT = 3;
    private final Object mg;
    private final dv xg;
    private final a xh;
    private OnMetadataUpdatedListener xi;
    private OnStatusUpdatedListener xj;

    public interface OnMetadataUpdatedListener {
        void onMetadataUpdated();
    }

    public interface OnStatusUpdatedListener {
        void onStatusUpdated();
    }

    public interface MediaChannelResult extends Result {
    }

    private class a implements dw {
        final /* synthetic */ RemoteMediaPlayer xk;
        private GoogleApiClient xu;
        private long xv;

        private final class a implements ResultCallback<Status> {
            private final long xw;
            final /* synthetic */ a xx;

            a(a aVar, long j) {
                this.xx = aVar;
                this.xw = j;
            }

            public void i(Status status) {
                if (!status.isSuccess()) {
                    this.xx.xk.xg.a(this.xw, status.getStatusCode());
                }
            }

            public /* synthetic */ void onResult(Result x0) {
                i((Status) x0);
            }
        }

        public a(RemoteMediaPlayer remoteMediaPlayer) {
            this.xk = remoteMediaPlayer;
            this.xv = 0;
        }

        public void a(String str, String str2, long j, String str3) throws IOException {
            if (this.xu == null) {
                throw new IOException("No GoogleApiClient available");
            }
            Cast.CastApi.sendMessage(this.xu, str, str2).setResultCallback(new a(this, j));
        }

        public void b(GoogleApiClient googleApiClient) {
            this.xu = googleApiClient;
        }

        public long cV() {
            long j = this.xv + 1;
            this.xv = j;
            return j;
        }
    }

    private static final class c implements MediaChannelResult {
        private final Status vl;
        private final JSONObject wP;

        c(Status status, JSONObject jSONObject) {
            this.vl = status;
            this.wP = jSONObject;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    private static abstract class b extends a<MediaChannelResult> {
        dx xy;

        /* renamed from: com.google.android.gms.cast.RemoteMediaPlayer.b.2 */
        class AnonymousClass2 implements MediaChannelResult {
            final /* synthetic */ Status vb;
            final /* synthetic */ b xz;

            AnonymousClass2(b bVar, Status status) {
                this.xz = bVar;
                this.vb = status;
            }

            public Status getStatus() {
                return this.vb;
            }
        }

        b() {
            this.xy = new dx() {
                final /* synthetic */ b xz;

                {
                    this.xz = r1;
                }

                public void a(long j, int i, JSONObject jSONObject) {
                    this.xz.a(new c(new Status(i), jSONObject));
                }

                public void k(long j) {
                    this.xz.a(this.xz.j(new Status(RemoteMediaPlayer.STATUS_REPLACED)));
                }
            };
        }

        public /* synthetic */ Result d(Status status) {
            return j(status);
        }

        public MediaChannelResult j(Status status) {
            return new AnonymousClass2(this, status);
        }
    }

    /* renamed from: com.google.android.gms.cast.RemoteMediaPlayer.2 */
    class AnonymousClass2 extends b {
        final /* synthetic */ RemoteMediaPlayer xk;
        final /* synthetic */ GoogleApiClient xl;
        final /* synthetic */ MediaInfo xm;
        final /* synthetic */ boolean xn;
        final /* synthetic */ long xo;
        final /* synthetic */ JSONObject xp;

        AnonymousClass2(RemoteMediaPlayer remoteMediaPlayer, GoogleApiClient googleApiClient, MediaInfo mediaInfo, boolean z, long j, JSONObject jSONObject) {
            this.xk = remoteMediaPlayer;
            this.xl = googleApiClient;
            this.xm = mediaInfo;
            this.xn = z;
            this.xo = j;
            this.xp = jSONObject;
        }

        protected void a(dq dqVar) {
            synchronized (this.xk.mg) {
                this.xk.xh.b(this.xl);
                try {
                    this.xk.xg.a(this.xy, this.xm, this.xn, this.xo, this.xp);
                    this.xk.xh.b(null);
                } catch (IOException e) {
                    a(j(new Status(RemoteMediaPlayer.STATUS_FAILED)));
                    this.xk.xh.b(null);
                } catch (Throwable th) {
                    this.xk.xh.b(null);
                }
            }
        }
    }

    /* renamed from: com.google.android.gms.cast.RemoteMediaPlayer.3 */
    class AnonymousClass3 extends b {
        final /* synthetic */ RemoteMediaPlayer xk;
        final /* synthetic */ GoogleApiClient xl;
        final /* synthetic */ JSONObject xp;

        AnonymousClass3(RemoteMediaPlayer remoteMediaPlayer, GoogleApiClient googleApiClient, JSONObject jSONObject) {
            this.xk = remoteMediaPlayer;
            this.xl = googleApiClient;
            this.xp = jSONObject;
        }

        protected void a(dq dqVar) {
            synchronized (this.xk.mg) {
                this.xk.xh.b(this.xl);
                try {
                    this.xk.xg.a(this.xy, this.xp);
                    this.xk.xh.b(null);
                } catch (IOException e) {
                    a(j(new Status(RemoteMediaPlayer.STATUS_FAILED)));
                    this.xk.xh.b(null);
                } catch (Throwable th) {
                    this.xk.xh.b(null);
                }
            }
        }
    }

    /* renamed from: com.google.android.gms.cast.RemoteMediaPlayer.4 */
    class AnonymousClass4 extends b {
        final /* synthetic */ RemoteMediaPlayer xk;
        final /* synthetic */ GoogleApiClient xl;
        final /* synthetic */ JSONObject xp;

        AnonymousClass4(RemoteMediaPlayer remoteMediaPlayer, GoogleApiClient googleApiClient, JSONObject jSONObject) {
            this.xk = remoteMediaPlayer;
            this.xl = googleApiClient;
            this.xp = jSONObject;
        }

        protected void a(dq dqVar) {
            synchronized (this.xk.mg) {
                this.xk.xh.b(this.xl);
                try {
                    this.xk.xg.b(this.xy, this.xp);
                    this.xk.xh.b(null);
                } catch (IOException e) {
                    a(j(new Status(RemoteMediaPlayer.STATUS_FAILED)));
                    this.xk.xh.b(null);
                } catch (Throwable th) {
                    this.xk.xh.b(null);
                }
            }
        }
    }

    /* renamed from: com.google.android.gms.cast.RemoteMediaPlayer.5 */
    class AnonymousClass5 extends b {
        final /* synthetic */ RemoteMediaPlayer xk;
        final /* synthetic */ GoogleApiClient xl;
        final /* synthetic */ JSONObject xp;

        AnonymousClass5(RemoteMediaPlayer remoteMediaPlayer, GoogleApiClient googleApiClient, JSONObject jSONObject) {
            this.xk = remoteMediaPlayer;
            this.xl = googleApiClient;
            this.xp = jSONObject;
        }

        protected void a(dq dqVar) {
            synchronized (this.xk.mg) {
                this.xk.xh.b(this.xl);
                try {
                    this.xk.xg.c(this.xy, this.xp);
                    this.xk.xh.b(null);
                } catch (IOException e) {
                    a(j(new Status(RemoteMediaPlayer.STATUS_FAILED)));
                    this.xk.xh.b(null);
                } catch (Throwable th) {
                    this.xk.xh.b(null);
                }
            }
        }
    }

    /* renamed from: com.google.android.gms.cast.RemoteMediaPlayer.6 */
    class AnonymousClass6 extends b {
        final /* synthetic */ RemoteMediaPlayer xk;
        final /* synthetic */ GoogleApiClient xl;
        final /* synthetic */ JSONObject xp;
        final /* synthetic */ long xq;
        final /* synthetic */ int xr;

        AnonymousClass6(RemoteMediaPlayer remoteMediaPlayer, GoogleApiClient googleApiClient, long j, int i, JSONObject jSONObject) {
            this.xk = remoteMediaPlayer;
            this.xl = googleApiClient;
            this.xq = j;
            this.xr = i;
            this.xp = jSONObject;
        }

        protected void a(dq dqVar) {
            synchronized (this.xk.mg) {
                this.xk.xh.b(this.xl);
                try {
                    this.xk.xg.a(this.xy, this.xq, this.xr, this.xp);
                    this.xk.xh.b(null);
                } catch (IOException e) {
                    a(j(new Status(RemoteMediaPlayer.STATUS_FAILED)));
                    this.xk.xh.b(null);
                } catch (Throwable th) {
                    this.xk.xh.b(null);
                }
            }
        }
    }

    /* renamed from: com.google.android.gms.cast.RemoteMediaPlayer.7 */
    class AnonymousClass7 extends b {
        final /* synthetic */ RemoteMediaPlayer xk;
        final /* synthetic */ GoogleApiClient xl;
        final /* synthetic */ JSONObject xp;
        final /* synthetic */ double xs;

        AnonymousClass7(RemoteMediaPlayer remoteMediaPlayer, GoogleApiClient googleApiClient, double d, JSONObject jSONObject) {
            this.xk = remoteMediaPlayer;
            this.xl = googleApiClient;
            this.xs = d;
            this.xp = jSONObject;
        }

        protected void a(dq dqVar) {
            synchronized (this.xk.mg) {
                this.xk.xh.b(this.xl);
                try {
                    this.xk.xg.a(this.xy, this.xs, this.xp);
                    this.xk.xh.b(null);
                } catch (IllegalStateException e) {
                    a(j(new Status(RemoteMediaPlayer.STATUS_FAILED)));
                    this.xk.xh.b(null);
                } catch (IllegalArgumentException e2) {
                    a(j(new Status(RemoteMediaPlayer.STATUS_FAILED)));
                    this.xk.xh.b(null);
                } catch (IOException e3) {
                    a(j(new Status(RemoteMediaPlayer.STATUS_FAILED)));
                    this.xk.xh.b(null);
                } catch (Throwable th) {
                    this.xk.xh.b(null);
                }
            }
        }
    }

    /* renamed from: com.google.android.gms.cast.RemoteMediaPlayer.8 */
    class AnonymousClass8 extends b {
        final /* synthetic */ RemoteMediaPlayer xk;
        final /* synthetic */ GoogleApiClient xl;
        final /* synthetic */ JSONObject xp;
        final /* synthetic */ boolean xt;

        AnonymousClass8(RemoteMediaPlayer remoteMediaPlayer, GoogleApiClient googleApiClient, boolean z, JSONObject jSONObject) {
            this.xk = remoteMediaPlayer;
            this.xl = googleApiClient;
            this.xt = z;
            this.xp = jSONObject;
        }

        protected void a(dq dqVar) {
            synchronized (this.xk.mg) {
                this.xk.xh.b(this.xl);
                try {
                    this.xk.xg.a(this.xy, this.xt, this.xp);
                    this.xk.xh.b(null);
                } catch (IllegalStateException e) {
                    a(j(new Status(RemoteMediaPlayer.STATUS_FAILED)));
                    this.xk.xh.b(null);
                } catch (IOException e2) {
                    a(j(new Status(RemoteMediaPlayer.STATUS_FAILED)));
                    this.xk.xh.b(null);
                } catch (Throwable th) {
                    this.xk.xh.b(null);
                }
            }
        }
    }

    /* renamed from: com.google.android.gms.cast.RemoteMediaPlayer.9 */
    class AnonymousClass9 extends b {
        final /* synthetic */ RemoteMediaPlayer xk;
        final /* synthetic */ GoogleApiClient xl;

        AnonymousClass9(RemoteMediaPlayer remoteMediaPlayer, GoogleApiClient googleApiClient) {
            this.xk = remoteMediaPlayer;
            this.xl = googleApiClient;
        }

        protected void a(dq dqVar) {
            synchronized (this.xk.mg) {
                this.xk.xh.b(this.xl);
                try {
                    this.xk.xg.a(this.xy);
                    this.xk.xh.b(null);
                } catch (IOException e) {
                    a(j(new Status(RemoteMediaPlayer.STATUS_FAILED)));
                    this.xk.xh.b(null);
                } catch (Throwable th) {
                    this.xk.xh.b(null);
                }
            }
        }
    }

    public RemoteMediaPlayer() {
        this.mg = new Object();
        this.xh = new a(this);
        this.xg = new dv() {
            final /* synthetic */ RemoteMediaPlayer xk;

            {
                this.xk = r1;
            }

            protected void onMetadataUpdated() {
                this.xk.onMetadataUpdated();
            }

            protected void onStatusUpdated() {
                this.xk.onStatusUpdated();
            }
        };
        this.xg.a(this.xh);
    }

    private void onMetadataUpdated() {
        if (this.xi != null) {
            this.xi.onMetadataUpdated();
        }
    }

    private void onStatusUpdated() {
        if (this.xj != null) {
            this.xj.onStatusUpdated();
        }
    }

    public long getApproximateStreamPosition() {
        long approximateStreamPosition;
        synchronized (this.mg) {
            approximateStreamPosition = this.xg.getApproximateStreamPosition();
        }
        return approximateStreamPosition;
    }

    public MediaInfo getMediaInfo() {
        MediaInfo mediaInfo;
        synchronized (this.mg) {
            mediaInfo = this.xg.getMediaInfo();
        }
        return mediaInfo;
    }

    public MediaStatus getMediaStatus() {
        MediaStatus mediaStatus;
        synchronized (this.mg) {
            mediaStatus = this.xg.getMediaStatus();
        }
        return mediaStatus;
    }

    public String getNamespace() {
        return this.xg.getNamespace();
    }

    public long getStreamDuration() {
        long streamDuration;
        synchronized (this.mg) {
            streamDuration = this.xg.getStreamDuration();
        }
        return streamDuration;
    }

    public PendingResult<MediaChannelResult> load(GoogleApiClient apiClient, MediaInfo mediaInfo) {
        return load(apiClient, mediaInfo, true, 0, null);
    }

    public PendingResult<MediaChannelResult> load(GoogleApiClient apiClient, MediaInfo mediaInfo, boolean autoplay) {
        return load(apiClient, mediaInfo, autoplay, 0, null);
    }

    public PendingResult<MediaChannelResult> load(GoogleApiClient apiClient, MediaInfo mediaInfo, boolean autoplay, long playPosition) {
        return load(apiClient, mediaInfo, autoplay, playPosition, null);
    }

    public PendingResult<MediaChannelResult> load(GoogleApiClient apiClient, MediaInfo mediaInfo, boolean autoplay, long playPosition, JSONObject customData) {
        return apiClient.b(new AnonymousClass2(this, apiClient, mediaInfo, autoplay, playPosition, customData));
    }

    public void onMessageReceived(CastDevice castDevice, String namespace, String message) {
        this.xg.P(message);
    }

    public PendingResult<MediaChannelResult> pause(GoogleApiClient apiClient) {
        return pause(apiClient, null);
    }

    public PendingResult<MediaChannelResult> pause(GoogleApiClient apiClient, JSONObject customData) {
        return apiClient.b(new AnonymousClass3(this, apiClient, customData));
    }

    public PendingResult<MediaChannelResult> play(GoogleApiClient apiClient) {
        return play(apiClient, null);
    }

    public PendingResult<MediaChannelResult> play(GoogleApiClient apiClient, JSONObject customData) {
        return apiClient.b(new AnonymousClass5(this, apiClient, customData));
    }

    public PendingResult<MediaChannelResult> requestStatus(GoogleApiClient apiClient) {
        return apiClient.b(new AnonymousClass9(this, apiClient));
    }

    public PendingResult<MediaChannelResult> seek(GoogleApiClient apiClient, long position) {
        return seek(apiClient, position, STATUS_SUCCEEDED, null);
    }

    public PendingResult<MediaChannelResult> seek(GoogleApiClient apiClient, long position, int resumeState) {
        return seek(apiClient, position, resumeState, null);
    }

    public PendingResult<MediaChannelResult> seek(GoogleApiClient apiClient, long position, int resumeState, JSONObject customData) {
        return apiClient.b(new AnonymousClass6(this, apiClient, position, resumeState, customData));
    }

    public void setOnMetadataUpdatedListener(OnMetadataUpdatedListener listener) {
        this.xi = listener;
    }

    public void setOnStatusUpdatedListener(OnStatusUpdatedListener listener) {
        this.xj = listener;
    }

    public PendingResult<MediaChannelResult> setStreamMute(GoogleApiClient apiClient, boolean muteState) {
        return setStreamMute(apiClient, muteState, null);
    }

    public PendingResult<MediaChannelResult> setStreamMute(GoogleApiClient apiClient, boolean muteState, JSONObject customData) {
        return apiClient.b(new AnonymousClass8(this, apiClient, muteState, customData));
    }

    public PendingResult<MediaChannelResult> setStreamVolume(GoogleApiClient apiClient, double volume) throws IllegalArgumentException {
        return setStreamVolume(apiClient, volume, null);
    }

    public PendingResult<MediaChannelResult> setStreamVolume(GoogleApiClient apiClient, double volume, JSONObject customData) throws IllegalArgumentException {
        if (!Double.isInfinite(volume) && !Double.isNaN(volume)) {
            return apiClient.b(new AnonymousClass7(this, apiClient, volume, customData));
        }
        throw new IllegalArgumentException("Volume cannot be " + volume);
    }

    public PendingResult<MediaChannelResult> stop(GoogleApiClient apiClient) {
        return stop(apiClient, null);
    }

    public PendingResult<MediaChannelResult> stop(GoogleApiClient apiClient, JSONObject customData) {
        return apiClient.b(new AnonymousClass4(this, apiClient, customData));
    }
}
