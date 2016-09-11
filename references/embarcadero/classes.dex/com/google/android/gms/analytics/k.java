package com.google.android.gms.analytics;

import android.content.Context;
import android.content.res.Resources.NotFoundException;
import android.content.res.XmlResourceParser;
import android.text.TextUtils;
import java.io.IOException;
import org.xmlpull.v1.XmlPullParserException;

abstract class k<T extends j> {
    Context mContext;
    a<T> rb;

    public interface a<U extends j> {
        void a(String str, int i);

        void a(String str, String str2);

        void b(String str, String str2);

        U bz();

        void c(String str, boolean z);
    }

    public k(Context context, a<T> aVar) {
        this.mContext = context;
        this.rb = aVar;
    }

    private T a(XmlResourceParser xmlResourceParser) {
        try {
            xmlResourceParser.next();
            int eventType = xmlResourceParser.getEventType();
            while (eventType != 1) {
                if (xmlResourceParser.getEventType() == 2) {
                    String toLowerCase = xmlResourceParser.getName().toLowerCase();
                    String trim;
                    if (toLowerCase.equals("screenname")) {
                        toLowerCase = xmlResourceParser.getAttributeValue(null, "name");
                        trim = xmlResourceParser.nextText().trim();
                        if (!(TextUtils.isEmpty(toLowerCase) || TextUtils.isEmpty(trim))) {
                            this.rb.a(toLowerCase, trim);
                        }
                    } else if (toLowerCase.equals("string")) {
                        r0 = xmlResourceParser.getAttributeValue(null, "name");
                        trim = xmlResourceParser.nextText().trim();
                        if (!(TextUtils.isEmpty(r0) || trim == null)) {
                            this.rb.b(r0, trim);
                        }
                    } else if (toLowerCase.equals("bool")) {
                        r0 = xmlResourceParser.getAttributeValue(null, "name");
                        trim = xmlResourceParser.nextText().trim();
                        if (!(TextUtils.isEmpty(r0) || TextUtils.isEmpty(trim))) {
                            try {
                                this.rb.c(r0, Boolean.parseBoolean(trim));
                            } catch (NumberFormatException e) {
                                aa.t("Error parsing bool configuration value: " + trim);
                            }
                        }
                    } else if (toLowerCase.equals("integer")) {
                        toLowerCase = xmlResourceParser.getAttributeValue(null, "name");
                        trim = xmlResourceParser.nextText().trim();
                        if (!(TextUtils.isEmpty(toLowerCase) || TextUtils.isEmpty(trim))) {
                            try {
                                this.rb.a(toLowerCase, Integer.parseInt(trim));
                            } catch (NumberFormatException e2) {
                                aa.t("Error parsing int configuration value: " + trim);
                            }
                        }
                    } else {
                        continue;
                    }
                }
                eventType = xmlResourceParser.next();
            }
        } catch (XmlPullParserException e3) {
            aa.t("Error parsing tracker configuration file: " + e3);
        } catch (IOException e4) {
            aa.t("Error parsing tracker configuration file: " + e4);
        }
        return this.rb.bz();
    }

    public T p(int i) {
        try {
            return a(this.mContext.getResources().getXml(i));
        } catch (NotFoundException e) {
            aa.w("inflate() called with unknown resourceId: " + e);
            return null;
        }
    }
}
