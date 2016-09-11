package com.google.android.gms.internal;

import android.content.Context;
import android.content.res.Resources.NotFoundException;
import android.util.AttributeSet;
import android.util.Log;
import android.util.TypedValue;

public class eu {
    public static String a(String str, String str2, Context context, AttributeSet attributeSet, boolean z, boolean z2, String str3) {
        String attributeValue = attributeSet == null ? null : attributeSet.getAttributeValue(str, str2);
        if (attributeValue != null && attributeValue.startsWith("@string/") && z) {
            String substring = attributeValue.substring("@string/".length());
            String packageName = context.getPackageName();
            TypedValue typedValue = new TypedValue();
            try {
                context.getResources().getValue(packageName + ":string/" + substring, typedValue, true);
            } catch (NotFoundException e) {
                Log.w(str3, "Could not find resource for " + str2 + ": " + attributeValue);
            }
            if (typedValue.string != null) {
                attributeValue = typedValue.string.toString();
            } else {
                Log.w(str3, "Resource " + str2 + " was not a string: " + typedValue);
            }
        }
        if (z2 && attributeValue == null) {
            Log.w(str3, "Required XML attribute \"" + str2 + "\" missing");
        }
        return attributeValue;
    }
}
