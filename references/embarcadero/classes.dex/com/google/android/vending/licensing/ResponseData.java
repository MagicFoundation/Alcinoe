package com.google.android.vending.licensing;

import android.text.TextUtils;
import java.util.regex.Pattern;

public class ResponseData {
    public String extra;
    public int nonce;
    public String packageName;
    public int responseCode;
    public long timestamp;
    public String userId;
    public String versionCode;

    public static ResponseData parse(String responseData) {
        String mainData;
        String extraData;
        int index = responseData.indexOf(58);
        if (-1 == index) {
            mainData = responseData;
            extraData = "";
        } else {
            mainData = responseData.substring(0, index);
            extraData = index >= responseData.length() ? "" : responseData.substring(index + 1);
        }
        String[] fields = TextUtils.split(mainData, Pattern.quote("|"));
        if (fields.length < 6) {
            throw new IllegalArgumentException("Wrong number of fields.");
        }
        ResponseData data = new ResponseData();
        data.extra = extraData;
        data.responseCode = Integer.parseInt(fields[0]);
        data.nonce = Integer.parseInt(fields[1]);
        data.packageName = fields[2];
        data.versionCode = fields[3];
        data.userId = fields[4];
        data.timestamp = Long.parseLong(fields[5]);
        return data;
    }

    public String toString() {
        return TextUtils.join("|", new Object[]{Integer.valueOf(this.responseCode), Integer.valueOf(this.nonce), this.packageName, this.versionCode, this.userId, Long.valueOf(this.timestamp)});
    }
}
