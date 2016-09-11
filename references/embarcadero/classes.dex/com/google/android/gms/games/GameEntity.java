package com.google.android.gms.games;

import android.database.CharArrayBuffer;
import android.net.Uri;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.internal.eg;
import com.google.android.gms.internal.ep;
import com.google.android.gms.internal.fm;
import com.google.android.gms.internal.fy;

public final class GameEntity extends fy implements Game {
    public static final Creator<GameEntity> CREATOR;
    private final String FE;
    private final String FF;
    private final String FG;
    private final String FH;
    private final String FI;
    private final Uri FJ;
    private final Uri FK;
    private final Uri FL;
    private final boolean FM;
    private final boolean FN;
    private final String FO;
    private final int FP;
    private final int FQ;
    private final int FR;
    private final boolean FS;
    private final boolean FT;
    private final String FU;
    private final String FV;
    private final String FW;
    private final boolean FX;
    private final int wj;
    private final String wk;

    static final class a extends a {
        a() {
        }

        public GameEntity aj(Parcel parcel) {
            if (fy.c(eg.dY()) || eg.ae(GameEntity.class.getCanonicalName())) {
                return super.aj(parcel);
            }
            String readString = parcel.readString();
            String readString2 = parcel.readString();
            String readString3 = parcel.readString();
            String readString4 = parcel.readString();
            String readString5 = parcel.readString();
            String readString6 = parcel.readString();
            String readString7 = parcel.readString();
            Uri parse = readString7 == null ? null : Uri.parse(readString7);
            readString7 = parcel.readString();
            Uri parse2 = readString7 == null ? null : Uri.parse(readString7);
            readString7 = parcel.readString();
            return new GameEntity(2, readString, readString2, readString3, readString4, readString5, readString6, parse, parse2, readString7 == null ? null : Uri.parse(readString7), parcel.readInt() > 0, parcel.readInt() > 0, parcel.readString(), parcel.readInt(), parcel.readInt(), parcel.readInt(), false, false, null, null, null, false);
        }

        public /* synthetic */ Object createFromParcel(Parcel x0) {
            return aj(x0);
        }
    }

    static {
        CREATOR = new a();
    }

    GameEntity(int versionCode, String applicationId, String displayName, String primaryCategory, String secondaryCategory, String description, String developerName, Uri iconImageUri, Uri hiResImageUri, Uri featuredImageUri, boolean playEnabledGame, boolean instanceInstalled, String instancePackageName, int gameplayAclStatus, int achievementTotalCount, int leaderboardCount, boolean realTimeEnabled, boolean turnBasedEnabled, String iconImageUrl, String hiResImageUrl, String featuredImageUrl, boolean muted) {
        this.wj = versionCode;
        this.wk = applicationId;
        this.FE = displayName;
        this.FF = primaryCategory;
        this.FG = secondaryCategory;
        this.FH = description;
        this.FI = developerName;
        this.FJ = iconImageUri;
        this.FU = iconImageUrl;
        this.FK = hiResImageUri;
        this.FV = hiResImageUrl;
        this.FL = featuredImageUri;
        this.FW = featuredImageUrl;
        this.FM = playEnabledGame;
        this.FN = instanceInstalled;
        this.FO = instancePackageName;
        this.FP = gameplayAclStatus;
        this.FQ = achievementTotalCount;
        this.FR = leaderboardCount;
        this.FS = realTimeEnabled;
        this.FT = turnBasedEnabled;
        this.FX = muted;
    }

    public GameEntity(Game game) {
        this.wj = 2;
        this.wk = game.getApplicationId();
        this.FF = game.getPrimaryCategory();
        this.FG = game.getSecondaryCategory();
        this.FH = game.getDescription();
        this.FI = game.getDeveloperName();
        this.FE = game.getDisplayName();
        this.FJ = game.getIconImageUri();
        this.FU = game.getIconImageUrl();
        this.FK = game.getHiResImageUri();
        this.FV = game.getHiResImageUrl();
        this.FL = game.getFeaturedImageUri();
        this.FW = game.getFeaturedImageUrl();
        this.FM = game.isPlayEnabledGame();
        this.FN = game.isInstanceInstalled();
        this.FO = game.getInstancePackageName();
        this.FP = game.getGameplayAclStatus();
        this.FQ = game.getAchievementTotalCount();
        this.FR = game.getLeaderboardCount();
        this.FS = game.isRealTimeMultiplayerEnabled();
        this.FT = game.isTurnBasedMultiplayerEnabled();
        this.FX = game.isMuted();
    }

    static int a(Game game) {
        return ep.hashCode(game.getApplicationId(), game.getDisplayName(), game.getPrimaryCategory(), game.getSecondaryCategory(), game.getDescription(), game.getDeveloperName(), game.getIconImageUri(), game.getHiResImageUri(), game.getFeaturedImageUri(), Boolean.valueOf(game.isPlayEnabledGame()), Boolean.valueOf(game.isInstanceInstalled()), game.getInstancePackageName(), Integer.valueOf(game.getGameplayAclStatus()), Integer.valueOf(game.getAchievementTotalCount()), Integer.valueOf(game.getLeaderboardCount()), Boolean.valueOf(game.isRealTimeMultiplayerEnabled()), Boolean.valueOf(game.isTurnBasedMultiplayerEnabled()), Boolean.valueOf(game.isMuted()));
    }

    static boolean a(Game game, Object obj) {
        if (!(obj instanceof Game)) {
            return false;
        }
        if (game == obj) {
            return true;
        }
        Game game2 = (Game) obj;
        if (ep.equal(game2.getApplicationId(), game.getApplicationId()) && ep.equal(game2.getDisplayName(), game.getDisplayName()) && ep.equal(game2.getPrimaryCategory(), game.getPrimaryCategory()) && ep.equal(game2.getSecondaryCategory(), game.getSecondaryCategory()) && ep.equal(game2.getDescription(), game.getDescription()) && ep.equal(game2.getDeveloperName(), game.getDeveloperName()) && ep.equal(game2.getIconImageUri(), game.getIconImageUri()) && ep.equal(game2.getHiResImageUri(), game.getHiResImageUri()) && ep.equal(game2.getFeaturedImageUri(), game.getFeaturedImageUri()) && ep.equal(Boolean.valueOf(game2.isPlayEnabledGame()), Boolean.valueOf(game.isPlayEnabledGame())) && ep.equal(Boolean.valueOf(game2.isInstanceInstalled()), Boolean.valueOf(game.isInstanceInstalled())) && ep.equal(game2.getInstancePackageName(), game.getInstancePackageName()) && ep.equal(Integer.valueOf(game2.getGameplayAclStatus()), Integer.valueOf(game.getGameplayAclStatus())) && ep.equal(Integer.valueOf(game2.getAchievementTotalCount()), Integer.valueOf(game.getAchievementTotalCount())) && ep.equal(Integer.valueOf(game2.getLeaderboardCount()), Integer.valueOf(game.getLeaderboardCount())) && ep.equal(Boolean.valueOf(game2.isRealTimeMultiplayerEnabled()), Boolean.valueOf(game.isRealTimeMultiplayerEnabled()))) {
            Boolean valueOf = Boolean.valueOf(game2.isTurnBasedMultiplayerEnabled());
            boolean z = game.isTurnBasedMultiplayerEnabled() && ep.equal(Boolean.valueOf(game2.isMuted()), Boolean.valueOf(game.isMuted()));
            if (ep.equal(valueOf, Boolean.valueOf(z))) {
                return true;
            }
        }
        return false;
    }

    static String b(Game game) {
        return ep.e(game).a("ApplicationId", game.getApplicationId()).a("DisplayName", game.getDisplayName()).a("PrimaryCategory", game.getPrimaryCategory()).a("SecondaryCategory", game.getSecondaryCategory()).a("Description", game.getDescription()).a("DeveloperName", game.getDeveloperName()).a("IconImageUri", game.getIconImageUri()).a("IconImageUrl", game.getIconImageUrl()).a("HiResImageUri", game.getHiResImageUri()).a("HiResImageUrl", game.getHiResImageUrl()).a("FeaturedImageUri", game.getFeaturedImageUri()).a("FeaturedImageUrl", game.getFeaturedImageUrl()).a("PlayEnabledGame", Boolean.valueOf(game.isPlayEnabledGame())).a("InstanceInstalled", Boolean.valueOf(game.isInstanceInstalled())).a("InstancePackageName", game.getInstancePackageName()).a("AchievementTotalCount", Integer.valueOf(game.getAchievementTotalCount())).a("LeaderboardCount", Integer.valueOf(game.getLeaderboardCount())).a("RealTimeMultiplayerEnabled", Boolean.valueOf(game.isRealTimeMultiplayerEnabled())).a("TurnBasedMultiplayerEnabled", Boolean.valueOf(game.isTurnBasedMultiplayerEnabled())).toString();
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        return a(this, obj);
    }

    public Game freeze() {
        return this;
    }

    public int getAchievementTotalCount() {
        return this.FQ;
    }

    public String getApplicationId() {
        return this.wk;
    }

    public String getDescription() {
        return this.FH;
    }

    public void getDescription(CharArrayBuffer dataOut) {
        fm.b(this.FH, dataOut);
    }

    public String getDeveloperName() {
        return this.FI;
    }

    public void getDeveloperName(CharArrayBuffer dataOut) {
        fm.b(this.FI, dataOut);
    }

    public String getDisplayName() {
        return this.FE;
    }

    public void getDisplayName(CharArrayBuffer dataOut) {
        fm.b(this.FE, dataOut);
    }

    public Uri getFeaturedImageUri() {
        return this.FL;
    }

    public String getFeaturedImageUrl() {
        return this.FW;
    }

    public int getGameplayAclStatus() {
        return this.FP;
    }

    public Uri getHiResImageUri() {
        return this.FK;
    }

    public String getHiResImageUrl() {
        return this.FV;
    }

    public Uri getIconImageUri() {
        return this.FJ;
    }

    public String getIconImageUrl() {
        return this.FU;
    }

    public String getInstancePackageName() {
        return this.FO;
    }

    public int getLeaderboardCount() {
        return this.FR;
    }

    public String getPrimaryCategory() {
        return this.FF;
    }

    public String getSecondaryCategory() {
        return this.FG;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public int hashCode() {
        return a(this);
    }

    public boolean isDataValid() {
        return true;
    }

    public boolean isInstanceInstalled() {
        return this.FN;
    }

    public boolean isMuted() {
        return this.FX;
    }

    public boolean isPlayEnabledGame() {
        return this.FM;
    }

    public boolean isRealTimeMultiplayerEnabled() {
        return this.FS;
    }

    public boolean isTurnBasedMultiplayerEnabled() {
        return this.FT;
    }

    public String toString() {
        return b((Game) this);
    }

    public void writeToParcel(Parcel dest, int flags) {
        int i = 1;
        String str = null;
        if (dZ()) {
            dest.writeString(this.wk);
            dest.writeString(this.FE);
            dest.writeString(this.FF);
            dest.writeString(this.FG);
            dest.writeString(this.FH);
            dest.writeString(this.FI);
            dest.writeString(this.FJ == null ? null : this.FJ.toString());
            dest.writeString(this.FK == null ? null : this.FK.toString());
            if (this.FL != null) {
                str = this.FL.toString();
            }
            dest.writeString(str);
            dest.writeInt(this.FM ? 1 : 0);
            if (!this.FN) {
                i = 0;
            }
            dest.writeInt(i);
            dest.writeString(this.FO);
            dest.writeInt(this.FP);
            dest.writeInt(this.FQ);
            dest.writeInt(this.FR);
            return;
        }
        a.a(this, dest, flags);
    }
}
