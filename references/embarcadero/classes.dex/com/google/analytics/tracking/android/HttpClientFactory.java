package com.google.analytics.tracking.android;

import org.apache.http.client.HttpClient;

interface HttpClientFactory {
    HttpClient newInstance();
}
