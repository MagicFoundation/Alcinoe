package com.google.android.gms.games;

import android.database.CharArrayBuffer;
import android.net.Uri;
import android.os.Parcel;
import android.text.TextUtils;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.common.data.b;

public final class d extends b implements Player {
    private final a Gl;

    private static final class a {
        public final String Gm;
        public final String Gn;
        public final String Go;
        public final String Gp;
        public final String Gq;
        public final String Gr;
        public final String Gs;
        public final String Gt;
        public final String Gu;

        public a(String str) {
            if (TextUtils.isEmpty(str)) {
                this.Gm = "external_player_id";
                this.Gn = "profile_name";
                this.Go = "profile_icon_image_uri";
                this.Gp = "profile_icon_image_url";
                this.Gq = "profile_hi_res_image_uri";
                this.Gr = "profile_hi_res_image_url";
                this.Gs = "last_updated";
                this.Gt = "is_in_circles";
                this.Gu = "played_with_timestamp";
                return;
            }
            this.Gm = str + "external_player_id";
            this.Gn = str + "profile_name";
            this.Go = str + "profile_icon_image_uri";
            this.Gp = str + "profile_icon_image_url";
            this.Gq = str + "profile_hi_res_image_uri";
            this.Gr = str + "profile_hi_res_image_url";
            this.Gs = str + "last_updated";
            this.Gt = str + "is_in_circles";
            this.Gu = str + "played_with_timestamp";
        }
    }

    public d(DataHolder dataHolder, int i) {
        this(dataHolder, i, null);
    }

    public d(DataHolder dataHolder, int i, String str) {
        super(dataHolder, i);
        this.Gl = new a(str);
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        return PlayerEntity.a(this, obj);
    }

    public int fl() {
        return getInteger(this.Gl.Gt);
    }

    public Player freeze() {
        return new PlayerEntity(this);
    }

    public String getDisplayName() {
        return getString(this.Gl.Gn);
    }

    public void getDisplayName(CharArrayBuffer dataOut) {
        a(this.Gl.Gn, dataOut);
    }

    public Uri getHiResImageUri() {
        return aa(this.Gl.Gq);
    }

    public String getHiResImageUrl() {
        return getString(this.Gl.Gr);
    }

    public Uri getIconImageUri() {
        return aa(this.Gl.Go);
    }

    public String getIconImageUrl() {
        return getString(this.Gl.Gp);
    }

    public long getLastPlayedWithTimestamp() {
        return !hasColumn(this.Gl.Gu) ? -1 : getLong(this.Gl.Gu);
    }

    public String getPlayerId() {
        return getString(this.Gl.Gm);
    }

    public long getRetrievedTimestamp() {
        return getLong(this.Gl.Gs);
    }

    public boolean hasHiResImage() {
        return getHiResImageUri() != null;
    }

    public boolean hasIconImage() {
        return getIconImageUri() != null;
    }

    public int hashCode() {
        return PlayerEntity.a(this);
    }

    public String toString() {
        return PlayerEntity.b((Player) this);
    }

    public void writeToParcel(Parcel dest, int flags) {
        ((PlayerEntity) freeze()).writeToParcel(dest, flags);
    }
}
