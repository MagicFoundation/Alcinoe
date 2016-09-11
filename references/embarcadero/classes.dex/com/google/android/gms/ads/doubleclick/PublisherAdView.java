package com.google.android.gms.ads.doubleclick;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;
import com.google.android.gms.ads.AdListener;
import com.google.android.gms.ads.AdSize;
import com.google.android.gms.internal.ak;

public final class PublisherAdView extends ViewGroup {
    private final ak kD;

    public PublisherAdView(Context context) {
        super(context);
        this.kD = new ak(this);
    }

    public PublisherAdView(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.kD = new ak(this, attrs, true);
    }

    public PublisherAdView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        this.kD = new ak(this, attrs, true);
    }

    public void destroy() {
        this.kD.destroy();
    }

    public AdListener getAdListener() {
        return this.kD.getAdListener();
    }

    public AdSize getAdSize() {
        return this.kD.getAdSize();
    }

    public AdSize[] getAdSizes() {
        return this.kD.getAdSizes();
    }

    public String getAdUnitId() {
        return this.kD.getAdUnitId();
    }

    public AppEventListener getAppEventListener() {
        return this.kD.getAppEventListener();
    }

    public void loadAd(PublisherAdRequest publisherAdRequest) {
        this.kD.a(publisherAdRequest.N());
    }

    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        View childAt = getChildAt(0);
        if (childAt != null && childAt.getVisibility() != 8) {
            int measuredWidth = childAt.getMeasuredWidth();
            int measuredHeight = childAt.getMeasuredHeight();
            int i = ((right - left) - measuredWidth) / 2;
            int i2 = ((bottom - top) - measuredHeight) / 2;
            childAt.layout(i, i2, measuredWidth + i, measuredHeight + i2);
        }
    }

    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        int measuredWidth;
        int i = 0;
        View childAt = getChildAt(0);
        AdSize adSize = getAdSize();
        if (childAt != null && childAt.getVisibility() != 8) {
            measureChild(childAt, widthMeasureSpec, heightMeasureSpec);
            measuredWidth = childAt.getMeasuredWidth();
            i = childAt.getMeasuredHeight();
        } else if (adSize != null) {
            Context context = getContext();
            measuredWidth = adSize.getWidthInPixels(context);
            i = adSize.getHeightInPixels(context);
        } else {
            measuredWidth = 0;
        }
        setMeasuredDimension(View.resolveSize(Math.max(measuredWidth, getSuggestedMinimumWidth()), widthMeasureSpec), View.resolveSize(Math.max(i, getSuggestedMinimumHeight()), heightMeasureSpec));
    }

    public void pause() {
        this.kD.pause();
    }

    public void recordManualImpression() {
        this.kD.recordManualImpression();
    }

    public void resume() {
        this.kD.resume();
    }

    public void setAdListener(AdListener adListener) {
        this.kD.setAdListener(adListener);
    }

    public void setAdSizes(AdSize... adSizes) {
        if (adSizes == null || adSizes.length < 1) {
            throw new IllegalArgumentException("The supported ad sizes must contain at least one valid ad size.");
        }
        this.kD.a(adSizes);
    }

    public void setAdUnitId(String adUnitId) {
        this.kD.setAdUnitId(adUnitId);
    }

    public void setAppEventListener(AppEventListener appEventListener) {
        this.kD.setAppEventListener(appEventListener);
    }
}
