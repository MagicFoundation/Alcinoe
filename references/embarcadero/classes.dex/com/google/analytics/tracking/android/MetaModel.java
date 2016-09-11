package com.google.analytics.tracking.android;

import com.google.android.gms.common.util.VisibleForTesting;
import java.util.HashMap;
import java.util.Map;

class MetaModel {
    private Map<String, MetaInfo> mMetaInfos;

    public interface Formatter {
        String format(String str);
    }

    public static class MetaInfo {
        private final String mDefaultValue;
        private final Formatter mFormatter;
        private final String mUrlParam;

        public MetaInfo(String urlParam, String defaultValue, Formatter formatter) {
            this.mUrlParam = urlParam;
            this.mDefaultValue = defaultValue;
            this.mFormatter = formatter;
        }

        public String getUrlParam(String actualKey) {
            if (!actualKey.contains("*")) {
                return this.mUrlParam;
            }
            String param = this.mUrlParam;
            String[] splits = actualKey.split("\\*");
            if (splits.length <= 1) {
                return null;
            }
            try {
                return param + Integer.parseInt(splits[1]);
            } catch (NumberFormatException e) {
                Log.w("Unable to parse slot for url parameter " + param);
                return null;
            }
        }

        public String getDefaultValue() {
            return this.mDefaultValue;
        }

        public Formatter getFormatter() {
            return this.mFormatter;
        }

        @VisibleForTesting
        String getUrlParam() {
            return this.mUrlParam;
        }
    }

    MetaModel() {
        this.mMetaInfos = new HashMap();
    }

    MetaInfo getMetaInfo(String key) {
        if (key.startsWith("&")) {
            return new MetaInfo(key.substring(1), null, null);
        }
        String tmpKey = key;
        if (key.contains("*")) {
            tmpKey = key.substring(0, key.indexOf("*"));
        }
        return (MetaInfo) this.mMetaInfos.get(tmpKey);
    }

    public void addField(String key, String urlParam, String defaultValue, Formatter formatter) {
        this.mMetaInfos.put(key, new MetaInfo(urlParam, defaultValue, formatter));
    }
}
