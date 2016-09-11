package com.google.android.gms.ads.search;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;
import com.google.android.gms.ads.AdListener;
import com.google.android.gms.ads.AdSize;
import com.google.android.gms.internal.ak;

public final class SearchAdView extends ViewGroup {
    private final ak kD;

    public SearchAdView(Context context) {
        super(context);
        this.kD = new ak(this);
    }

    public SearchAdView(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.kD = new ak(this, attrs, false);
    }

    public SearchAdView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        this.kD = new ak(this, attrs, false);
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

    public String getAdUnitId() {
        return this.kD.getAdUnitId();
    }

    public void loadAd(SearchAdRequest searchAdRequest) {
        this.kD.a(searchAdRequest.N());
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

    public void resume() {
        this.kD.resume();
    }

    public void setAdListener(AdListener adListener) {
        this.kD.setAdListener(adListener);
    }

    public void setAdSize(AdSize adSize) {
        this.kD.setAdSizes(adSize);
    }

    public void setAdUnitId(String adUnitId) {
        this.kD.setAdUnitId(adUnitId);
    }
}
