package com.google.android.gms.internal;

import android.content.Context;
import android.location.Location;
import android.os.Bundle;
import android.os.Parcel;
import com.google.android.gms.ads.mediation.admob.AdMobExtras;
import com.google.android.gms.ads.search.SearchAdRequest;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

public final class z implements SafeParcelable {
    public static final aa CREATOR;
    public final Bundle extras;
    public final long le;
    public final int lf;
    public final List<String> lg;
    public final boolean lh;
    public final boolean li;
    public final String lj;
    public final am lk;
    public final Location ll;
    public final String lm;
    public final int tagForChildDirectedTreatment;
    public final int versionCode;

    static {
        CREATOR = new aa();
    }

    z(int i, long j, Bundle bundle, int i2, List<String> list, boolean z, int i3, boolean z2, String str, am amVar, Location location, String str2) {
        this.versionCode = i;
        this.le = j;
        this.extras = bundle;
        this.lf = i2;
        this.lg = list;
        this.lh = z;
        this.tagForChildDirectedTreatment = i3;
        this.li = z2;
        this.lj = str;
        this.lk = amVar;
        this.ll = location;
        this.lm = str2;
    }

    public z(Context context, aj ajVar) {
        am amVar = null;
        this.versionCode = 3;
        Date birthday = ajVar.getBirthday();
        this.le = birthday != null ? birthday.getTime() : -1;
        this.lm = ajVar.getContentUrl();
        this.lf = ajVar.getGender();
        Collection keywords = ajVar.getKeywords();
        this.lg = !keywords.isEmpty() ? Collections.unmodifiableList(new ArrayList(keywords)) : null;
        this.lh = ajVar.isTestDevice(context);
        this.tagForChildDirectedTreatment = ajVar.al();
        this.ll = ajVar.getLocation();
        AdMobExtras adMobExtras = (AdMobExtras) ajVar.getNetworkExtras(AdMobExtras.class);
        this.extras = adMobExtras != null ? adMobExtras.getExtras() : null;
        this.li = ajVar.getManualImpressionsEnabled();
        this.lj = ajVar.getPublisherProvidedId();
        SearchAdRequest aj = ajVar.aj();
        if (aj != null) {
            amVar = new am(aj);
        }
        this.lk = amVar;
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel out, int flags) {
        aa.a(this, out, flags);
    }
}
