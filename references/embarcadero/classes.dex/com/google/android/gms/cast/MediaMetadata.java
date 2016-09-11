package com.google.android.gms.cast;

import android.os.Bundle;
import android.text.TextUtils;
import com.google.android.gms.common.images.WebImage;
import com.google.android.gms.internal.dz;
import com.google.android.gms.plus.PlusShare;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.json.JSONException;
import org.json.JSONObject;

public class MediaMetadata {
    public static final String KEY_ALBUM_ARTIST = "com.google.android.gms.cast.metadata.ALBUM_ARTIST";
    public static final String KEY_ALBUM_TITLE = "com.google.android.gms.cast.metadata.ALBUM_TITLE";
    public static final String KEY_ARTIST = "com.google.android.gms.cast.metadata.ARTIST";
    public static final String KEY_BROADCAST_DATE = "com.google.android.gms.cast.metadata.BROADCAST_DATE";
    public static final String KEY_COMPOSER = "com.google.android.gms.cast.metadata.COMPOSER";
    public static final String KEY_CREATION_DATE = "com.google.android.gms.cast.metadata.CREATION_DATE";
    public static final String KEY_DISC_NUMBER = "com.google.android.gms.cast.metadata.DISC_NUMBER";
    public static final String KEY_EPISODE_NUMBER = "com.google.android.gms.cast.metadata.EPISODE_NUMBER";
    public static final String KEY_HEIGHT = "com.google.android.gms.cast.metadata.HEIGHT";
    public static final String KEY_LOCATION_LATITUDE = "com.google.android.gms.cast.metadata.LOCATION_LATITUDE";
    public static final String KEY_LOCATION_LONGITUDE = "com.google.android.gms.cast.metadata.LOCATION_LONGITUDE";
    public static final String KEY_LOCATION_NAME = "com.google.android.gms.cast.metadata.LOCATION_NAME";
    public static final String KEY_RELEASE_DATE = "com.google.android.gms.cast.metadata.RELEASE_DATE";
    public static final String KEY_SEASON_NUMBER = "com.google.android.gms.cast.metadata.SEASON_NUMBER";
    public static final String KEY_SERIES_TITLE = "com.google.android.gms.cast.metadata.SERIES_TITLE";
    public static final String KEY_STUDIO = "com.google.android.gms.cast.metadata.STUDIO";
    public static final String KEY_SUBTITLE = "com.google.android.gms.cast.metadata.SUBTITLE";
    public static final String KEY_TITLE = "com.google.android.gms.cast.metadata.TITLE";
    public static final String KEY_TRACK_NUMBER = "com.google.android.gms.cast.metadata.TRACK_NUMBER";
    public static final String KEY_WIDTH = "com.google.android.gms.cast.metadata.WIDTH";
    public static final int MEDIA_TYPE_GENERIC = 0;
    public static final int MEDIA_TYPE_MOVIE = 1;
    public static final int MEDIA_TYPE_MUSIC_TRACK = 3;
    public static final int MEDIA_TYPE_PHOTO = 4;
    public static final int MEDIA_TYPE_TV_SHOW = 2;
    public static final int MEDIA_TYPE_USER = 100;
    private static final String[] wR;
    private static final a wS;
    private final Bundle wT;
    private int wU;
    private final List<WebImage> wl;

    private static class a {
        private final Map<String, String> wV;
        private final Map<String, String> wW;
        private final Map<String, Integer> wX;

        public a() {
            this.wV = new HashMap();
            this.wW = new HashMap();
            this.wX = new HashMap();
        }

        public String M(String str) {
            return (String) this.wV.get(str);
        }

        public String N(String str) {
            return (String) this.wW.get(str);
        }

        public int O(String str) {
            Integer num = (Integer) this.wX.get(str);
            return num != null ? num.intValue() : MediaMetadata.MEDIA_TYPE_GENERIC;
        }

        public a a(String str, String str2, int i) {
            this.wV.put(str, str2);
            this.wW.put(str2, str);
            this.wX.put(str, Integer.valueOf(i));
            return this;
        }
    }

    static {
        wR = new String[]{null, "String", "int", "double", "ISO-8601 date String"};
        wS = new a().a(KEY_CREATION_DATE, "creationDateTime", MEDIA_TYPE_PHOTO).a(KEY_RELEASE_DATE, "releaseDate", MEDIA_TYPE_PHOTO).a(KEY_BROADCAST_DATE, "originalAirdate", MEDIA_TYPE_PHOTO).a(KEY_TITLE, PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_TITLE, MEDIA_TYPE_MOVIE).a(KEY_SUBTITLE, "subtitle", MEDIA_TYPE_MOVIE).a(KEY_ARTIST, "artist", MEDIA_TYPE_MOVIE).a(KEY_ALBUM_ARTIST, "albumArtist", MEDIA_TYPE_MOVIE).a(KEY_ALBUM_TITLE, "albumName", MEDIA_TYPE_MOVIE).a(KEY_COMPOSER, "composer", MEDIA_TYPE_MOVIE).a(KEY_DISC_NUMBER, "discNumber", MEDIA_TYPE_TV_SHOW).a(KEY_TRACK_NUMBER, "trackNumber", MEDIA_TYPE_TV_SHOW).a(KEY_SEASON_NUMBER, "season", MEDIA_TYPE_TV_SHOW).a(KEY_EPISODE_NUMBER, "episode", MEDIA_TYPE_TV_SHOW).a(KEY_SERIES_TITLE, "seriesTitle", MEDIA_TYPE_MOVIE).a(KEY_STUDIO, "studio", MEDIA_TYPE_MOVIE).a(KEY_WIDTH, "width", MEDIA_TYPE_TV_SHOW).a(KEY_HEIGHT, "height", MEDIA_TYPE_TV_SHOW).a(KEY_LOCATION_NAME, "location", MEDIA_TYPE_MOVIE).a(KEY_LOCATION_LATITUDE, "latitude", MEDIA_TYPE_MUSIC_TRACK).a(KEY_LOCATION_LONGITUDE, "longitude", MEDIA_TYPE_MUSIC_TRACK);
    }

    public MediaMetadata() {
        this(MEDIA_TYPE_GENERIC);
    }

    public MediaMetadata(int mediaType) {
        this.wl = new ArrayList();
        this.wT = new Bundle();
        this.wU = mediaType;
    }

    private void a(JSONObject jSONObject, String... strArr) {
        try {
            int length = strArr.length;
            for (int i = MEDIA_TYPE_GENERIC; i < length; i += MEDIA_TYPE_MOVIE) {
                String str = strArr[i];
                if (this.wT.containsKey(str)) {
                    switch (wS.O(str)) {
                        case MEDIA_TYPE_MOVIE /*1*/:
                        case MEDIA_TYPE_PHOTO /*4*/:
                            jSONObject.put(wS.M(str), this.wT.getString(str));
                            break;
                        case MEDIA_TYPE_TV_SHOW /*2*/:
                            jSONObject.put(wS.M(str), this.wT.getInt(str));
                            break;
                        case MEDIA_TYPE_MUSIC_TRACK /*3*/:
                            jSONObject.put(wS.M(str), this.wT.getDouble(str));
                            break;
                        default:
                            break;
                    }
                }
            }
            for (String str2 : this.wT.keySet()) {
                if (!str2.startsWith("com.google.")) {
                    Object obj = this.wT.get(str2);
                    if (obj instanceof String) {
                        jSONObject.put(str2, obj);
                    } else if (obj instanceof Integer) {
                        jSONObject.put(str2, obj);
                    } else if (obj instanceof Double) {
                        jSONObject.put(str2, obj);
                    }
                }
            }
        } catch (JSONException e) {
        }
    }

    private boolean a(Bundle bundle, Bundle bundle2) {
        if (bundle.size() != bundle2.size()) {
            return false;
        }
        for (String str : bundle.keySet()) {
            Object obj = bundle.get(str);
            Object obj2 = bundle2.get(str);
            if ((obj instanceof Bundle) && (obj2 instanceof Bundle) && !a((Bundle) obj, (Bundle) obj2)) {
                return false;
            }
            if (obj == null) {
                if (obj2 != null || !bundle2.containsKey(str)) {
                    return false;
                }
            } else if (!obj.equals(obj2)) {
                return false;
            }
        }
        return true;
    }

    private void b(JSONObject jSONObject, String... strArr) {
        Set hashSet = new HashSet(Arrays.asList(strArr));
        try {
            Iterator keys = jSONObject.keys();
            while (keys.hasNext()) {
                String str = (String) keys.next();
                if (!"metadataType".equals(str)) {
                    String N = wS.N(str);
                    if (N == null) {
                        Object obj = jSONObject.get(str);
                        if (obj instanceof String) {
                            this.wT.putString(str, (String) obj);
                        } else if (obj instanceof Integer) {
                            this.wT.putInt(str, ((Integer) obj).intValue());
                        } else if (obj instanceof Double) {
                            this.wT.putDouble(str, ((Double) obj).doubleValue());
                        }
                    } else if (hashSet.contains(N)) {
                        try {
                            Object obj2 = jSONObject.get(str);
                            if (obj2 != null) {
                                switch (wS.O(N)) {
                                    case MEDIA_TYPE_MOVIE /*1*/:
                                        if (!(obj2 instanceof String)) {
                                            break;
                                        }
                                        this.wT.putString(N, (String) obj2);
                                        break;
                                    case MEDIA_TYPE_TV_SHOW /*2*/:
                                        if (!(obj2 instanceof Integer)) {
                                            break;
                                        }
                                        this.wT.putInt(N, ((Integer) obj2).intValue());
                                        break;
                                    case MEDIA_TYPE_MUSIC_TRACK /*3*/:
                                        if (!(obj2 instanceof Double)) {
                                            break;
                                        }
                                        this.wT.putDouble(N, ((Double) obj2).doubleValue());
                                        break;
                                    case MEDIA_TYPE_PHOTO /*4*/:
                                        if (!(obj2 instanceof String)) {
                                            break;
                                        }
                                        if (dz.V((String) obj2) == null) {
                                            break;
                                        }
                                        this.wT.putString(N, (String) obj2);
                                        break;
                                    default:
                                        break;
                                }
                            }
                        } catch (JSONException e) {
                        }
                    }
                }
            }
        } catch (JSONException e2) {
        }
    }

    private void d(String str, int i) throws IllegalArgumentException {
        if (TextUtils.isEmpty(str)) {
            throw new IllegalArgumentException("null and empty keys are not allowed");
        }
        int O = wS.O(str);
        if (O != i && O != 0) {
            throw new IllegalArgumentException("Value for " + str + " must be a " + wR[i]);
        }
    }

    public void addImage(WebImage image) {
        this.wl.add(image);
    }

    public void b(JSONObject jSONObject) {
        clear();
        this.wU = MEDIA_TYPE_GENERIC;
        try {
            this.wU = jSONObject.getInt("metadataType");
        } catch (JSONException e) {
        }
        dz.a(this.wl, jSONObject);
        String[] strArr;
        switch (this.wU) {
            case MEDIA_TYPE_GENERIC /*0*/:
                strArr = new String[MEDIA_TYPE_PHOTO];
                strArr[MEDIA_TYPE_GENERIC] = KEY_TITLE;
                strArr[MEDIA_TYPE_MOVIE] = KEY_ARTIST;
                strArr[MEDIA_TYPE_TV_SHOW] = KEY_SUBTITLE;
                strArr[MEDIA_TYPE_MUSIC_TRACK] = KEY_RELEASE_DATE;
                b(jSONObject, strArr);
            case MEDIA_TYPE_MOVIE /*1*/:
                strArr = new String[MEDIA_TYPE_PHOTO];
                strArr[MEDIA_TYPE_GENERIC] = KEY_TITLE;
                strArr[MEDIA_TYPE_MOVIE] = KEY_STUDIO;
                strArr[MEDIA_TYPE_TV_SHOW] = KEY_SUBTITLE;
                strArr[MEDIA_TYPE_MUSIC_TRACK] = KEY_RELEASE_DATE;
                b(jSONObject, strArr);
            case MEDIA_TYPE_TV_SHOW /*2*/:
                b(jSONObject, KEY_TITLE, KEY_SERIES_TITLE, KEY_SEASON_NUMBER, KEY_EPISODE_NUMBER, KEY_BROADCAST_DATE);
            case MEDIA_TYPE_MUSIC_TRACK /*3*/:
                b(jSONObject, KEY_TITLE, KEY_ALBUM_TITLE, KEY_ARTIST, KEY_ALBUM_ARTIST, KEY_COMPOSER, KEY_TRACK_NUMBER, KEY_DISC_NUMBER, KEY_RELEASE_DATE);
            case MEDIA_TYPE_PHOTO /*4*/:
                b(jSONObject, KEY_TITLE, KEY_ARTIST, KEY_LOCATION_NAME, KEY_LOCATION_LATITUDE, KEY_LOCATION_LONGITUDE, KEY_WIDTH, KEY_HEIGHT, KEY_CREATION_DATE);
            default:
                b(jSONObject, new String[MEDIA_TYPE_GENERIC]);
        }
    }

    public JSONObject cT() {
        JSONObject jSONObject = new JSONObject();
        try {
            jSONObject.put("metadataType", this.wU);
        } catch (JSONException e) {
        }
        dz.a(jSONObject, this.wl);
        String[] strArr;
        switch (this.wU) {
            case MEDIA_TYPE_GENERIC /*0*/:
                strArr = new String[MEDIA_TYPE_PHOTO];
                strArr[MEDIA_TYPE_GENERIC] = KEY_TITLE;
                strArr[MEDIA_TYPE_MOVIE] = KEY_ARTIST;
                strArr[MEDIA_TYPE_TV_SHOW] = KEY_SUBTITLE;
                strArr[MEDIA_TYPE_MUSIC_TRACK] = KEY_RELEASE_DATE;
                a(jSONObject, strArr);
                break;
            case MEDIA_TYPE_MOVIE /*1*/:
                strArr = new String[MEDIA_TYPE_PHOTO];
                strArr[MEDIA_TYPE_GENERIC] = KEY_TITLE;
                strArr[MEDIA_TYPE_MOVIE] = KEY_STUDIO;
                strArr[MEDIA_TYPE_TV_SHOW] = KEY_SUBTITLE;
                strArr[MEDIA_TYPE_MUSIC_TRACK] = KEY_RELEASE_DATE;
                a(jSONObject, strArr);
                break;
            case MEDIA_TYPE_TV_SHOW /*2*/:
                a(jSONObject, KEY_TITLE, KEY_SERIES_TITLE, KEY_SEASON_NUMBER, KEY_EPISODE_NUMBER, KEY_BROADCAST_DATE);
                break;
            case MEDIA_TYPE_MUSIC_TRACK /*3*/:
                a(jSONObject, KEY_TITLE, KEY_ARTIST, KEY_ALBUM_TITLE, KEY_ALBUM_ARTIST, KEY_COMPOSER, KEY_TRACK_NUMBER, KEY_DISC_NUMBER, KEY_RELEASE_DATE);
                break;
            case MEDIA_TYPE_PHOTO /*4*/:
                a(jSONObject, KEY_TITLE, KEY_ARTIST, KEY_LOCATION_NAME, KEY_LOCATION_LATITUDE, KEY_LOCATION_LONGITUDE, KEY_WIDTH, KEY_HEIGHT, KEY_CREATION_DATE);
                break;
            default:
                a(jSONObject, new String[MEDIA_TYPE_GENERIC]);
                break;
        }
        return jSONObject;
    }

    public void clear() {
        this.wT.clear();
        this.wl.clear();
    }

    public void clearImages() {
        this.wl.clear();
    }

    public boolean containsKey(String key) {
        return this.wT.containsKey(key);
    }

    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof MediaMetadata)) {
            return false;
        }
        MediaMetadata mediaMetadata = (MediaMetadata) other;
        return a(this.wT, mediaMetadata.wT) && this.wl.equals(mediaMetadata.wl);
    }

    public Calendar getDate(String key) {
        d(key, MEDIA_TYPE_PHOTO);
        String string = this.wT.getString(key);
        return string != null ? dz.V(string) : null;
    }

    public String getDateAsString(String key) {
        d(key, MEDIA_TYPE_PHOTO);
        return this.wT.getString(key);
    }

    public double getDouble(String key) {
        d(key, MEDIA_TYPE_MUSIC_TRACK);
        return this.wT.getDouble(key);
    }

    public List<WebImage> getImages() {
        return this.wl;
    }

    public int getInt(String key) {
        d(key, MEDIA_TYPE_TV_SHOW);
        return this.wT.getInt(key);
    }

    public int getMediaType() {
        return this.wU;
    }

    public String getString(String key) {
        d(key, MEDIA_TYPE_MOVIE);
        return this.wT.getString(key);
    }

    public boolean hasImages() {
        return (this.wl == null || this.wl.isEmpty()) ? false : true;
    }

    public int hashCode() {
        int i = 17;
        for (String str : this.wT.keySet()) {
            i *= 31;
            i = this.wT.get(str).hashCode() + i;
        }
        return (i * 31) + this.wl.hashCode();
    }

    public Set<String> keySet() {
        return this.wT.keySet();
    }

    public void putDate(String key, Calendar value) {
        d(key, MEDIA_TYPE_PHOTO);
        this.wT.putString(key, dz.a(value));
    }

    public void putDouble(String key, double value) {
        d(key, MEDIA_TYPE_MUSIC_TRACK);
        this.wT.putDouble(key, value);
    }

    public void putInt(String key, int value) {
        d(key, MEDIA_TYPE_TV_SHOW);
        this.wT.putInt(key, value);
    }

    public void putString(String key, String value) {
        d(key, MEDIA_TYPE_MOVIE);
        this.wT.putString(key, value);
    }
}
