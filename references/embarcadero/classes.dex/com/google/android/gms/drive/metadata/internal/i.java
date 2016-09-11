package com.google.android.gms.drive.metadata.internal;

import android.os.Bundle;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.drive.metadata.CollectionMetadataField;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import org.json.JSONArray;
import org.json.JSONException;

public class i extends CollectionMetadataField<String> {
    public i(String str, int i) {
        super(str, i);
    }

    public static final Collection<String> as(String str) throws JSONException {
        if (str == null) {
            return null;
        }
        Collection arrayList = new ArrayList();
        JSONArray jSONArray = new JSONArray(str);
        for (int i = 0; i < jSONArray.length(); i++) {
            arrayList.add(jSONArray.getString(i));
        }
        return Collections.unmodifiableCollection(arrayList);
    }

    protected Collection<String> a(DataHolder dataHolder, int i, int i2) {
        try {
            return as(dataHolder.getString(getName(), i, i2));
        } catch (Throwable e) {
            throw new IllegalStateException("DataHolder supplied invalid JSON", e);
        }
    }

    protected void a(Bundle bundle, Collection<String> collection) {
        bundle.putStringArrayList(getName(), new ArrayList(collection));
    }

    protected /* synthetic */ Object b(DataHolder dataHolder, int i, int i2) {
        return a(dataHolder, i, i2);
    }

    protected /* synthetic */ Object e(Bundle bundle) {
        return j(bundle);
    }

    protected Collection<String> j(Bundle bundle) {
        return bundle.getStringArrayList(getName());
    }
}
