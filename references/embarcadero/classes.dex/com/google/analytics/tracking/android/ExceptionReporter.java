package com.google.analytics.tracking.android;

import android.content.Context;
import java.lang.Thread.UncaughtExceptionHandler;
import java.util.ArrayList;

public class ExceptionReporter implements UncaughtExceptionHandler {
    static final String DEFAULT_DESCRIPTION = "UncaughtException";
    private ExceptionParser mExceptionParser;
    private final UncaughtExceptionHandler mOriginalHandler;
    private final ServiceManager mServiceManager;
    private final Tracker mTracker;

    public ExceptionReporter(Tracker tracker, ServiceManager serviceManager, UncaughtExceptionHandler originalHandler, Context context) {
        if (tracker == null) {
            throw new NullPointerException("tracker cannot be null");
        } else if (serviceManager == null) {
            throw new NullPointerException("serviceManager cannot be null");
        } else {
            this.mOriginalHandler = originalHandler;
            this.mTracker = tracker;
            this.mServiceManager = serviceManager;
            this.mExceptionParser = new StandardExceptionParser(context, new ArrayList());
            Log.iDebug("ExceptionReporter created, original handler is " + (originalHandler == null ? "null" : originalHandler.getClass().getName()));
        }
    }

    public ExceptionParser getExceptionParser() {
        return this.mExceptionParser;
    }

    public void setExceptionParser(ExceptionParser exceptionParser) {
        this.mExceptionParser = exceptionParser;
    }

    public void uncaughtException(Thread t, Throwable e) {
        String description = DEFAULT_DESCRIPTION;
        if (this.mExceptionParser != null) {
            description = this.mExceptionParser.getDescription(t != null ? t.getName() : null, e);
        }
        Log.iDebug("Tracking Exception: " + description);
        this.mTracker.sendException(description, true);
        this.mServiceManager.dispatch();
        if (this.mOriginalHandler != null) {
            Log.iDebug("Passing exception to original handler.");
            this.mOriginalHandler.uncaughtException(t, e);
        }
    }
}
