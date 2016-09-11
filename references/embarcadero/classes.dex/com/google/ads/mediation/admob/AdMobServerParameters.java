package com.google.ads.mediation.admob;

import com.google.ads.mediation.MediationServerParameters;

public final class AdMobServerParameters extends MediationServerParameters {
    public String adJson;
    @Parameter(name = "pubid")
    public String adUnitId;
    @Parameter(name = "mad_hac", required = false)
    public String allowHouseAds;
    public int tagForChildDirectedTreatment;

    public AdMobServerParameters() {
        this.allowHouseAds = null;
        this.tagForChildDirectedTreatment = -1;
    }
}
