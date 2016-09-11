package com.google.android.gms.analytics;

import android.content.Context;
import com.google.android.gms.analytics.HitBuilders.ExceptionBuilder;
import java.lang.Thread.UncaughtExceptionHandler;
import java.util.ArrayList;

public class ExceptionReporter implements UncaughtExceptionHandler {
    private final Context mContext;
    private final UncaughtExceptionHandler rd;
    private final Tracker re;
    private ExceptionParser rf;

    public ExceptionReporter(Tracker tracker, UncaughtExceptionHandler originalHandler, Context context) {
        if (tracker == null) {
            throw new NullPointerException("tracker cannot be null");
        } else if (context == null) {
            throw new NullPointerException("context cannot be null");
        } else {
            this.rd = originalHandler;
            this.re = tracker;
            this.rf = new StandardExceptionParser(context, new ArrayList());
            this.mContext = context.getApplicationContext();
            aa.v("ExceptionReporter created, original handler is " + (originalHandler == null ? "null" : originalHandler.getClass().getName()));
        }
    }

    public ExceptionParser getExceptionParser() {
        return this.rf;
    }

    public void setExceptionParser(ExceptionParser exceptionParser) {
        this.rf = exceptionParser;
    }

    public void uncaughtException(Thread t, Throwable e) {
        String str = "UncaughtException";
        if (this.rf != null) {
            str = this.rf.getDescription(t != null ? t.getName() : null, e);
        }
        aa.v("Tracking Exception: " + str);
        this.re.send(new ExceptionBuilder().setDescription(str).setFatal(true).build());
        GoogleAnalytics.getInstance(this.mContext).dispatchLocalHits();
        if (this.rd != null) {
            aa.v("Passing exception to original handler.");
            this.rd.uncaughtException(t, e);
        }
    }
}
