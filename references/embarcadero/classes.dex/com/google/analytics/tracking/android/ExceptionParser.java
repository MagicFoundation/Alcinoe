package com.google.analytics.tracking.android;

public interface ExceptionParser {
    String getDescription(String str, Throwable th);
}
