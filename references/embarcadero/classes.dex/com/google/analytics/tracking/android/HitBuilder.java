package com.google.analytics.tracking.android;

import com.google.analytics.tracking.android.MetaModel.MetaInfo;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

class HitBuilder {
    HitBuilder() {
    }

    static Map<String, String> generateHitParams(MetaModel metaModel, Map<String, String> hit) {
        Map<String, String> params = new HashMap();
        for (Entry<String, String> entry : hit.entrySet()) {
            MetaInfo metaInfo = metaModel.getMetaInfo((String) entry.getKey());
            if (metaInfo != null) {
                String urlParam = metaInfo.getUrlParam((String) entry.getKey());
                if (urlParam != null) {
                    String value = (String) entry.getValue();
                    if (metaInfo.getFormatter() != null) {
                        value = metaInfo.getFormatter().format(value);
                    }
                    if (!(value == null || value.equals(metaInfo.getDefaultValue()))) {
                        params.put(urlParam, value);
                    }
                }
            }
        }
        return params;
    }

    static String postProcessHit(Hit hit, long currentTimeMillis) {
        StringBuilder builder = new StringBuilder();
        builder.append(hit.getHitParams());
        if (hit.getHitTime() > 0) {
            long queueTime = currentTimeMillis - hit.getHitTime();
            if (queueTime >= 0) {
                builder.append("&").append(ModelFields.QUEUE_TIME).append("=").append(queueTime);
            }
        }
        builder.append("&").append(ModelFields.CACHE_BUSTER).append("=").append(hit.getHitId());
        return builder.toString();
    }

    static String encode(String input) {
        try {
            return URLEncoder.encode(input, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new AssertionError("URL encoding failed for: " + input);
        }
    }
}
