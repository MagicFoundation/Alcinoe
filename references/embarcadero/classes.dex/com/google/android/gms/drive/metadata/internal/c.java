package com.google.android.gms.drive.metadata.internal;

import com.google.android.gms.drive.metadata.MetadataField;
import com.google.android.gms.internal.fs;
import com.google.android.gms.internal.ft;
import com.google.android.gms.internal.fv;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public final class c {
    private static Map<String, MetadataField<?>> Ej;

    static {
        Ej = new HashMap();
        b(fs.El);
        b(fs.TITLE);
        b(fs.MIME_TYPE);
        b(fs.STARRED);
        b(fs.TRASHED);
        b(fs.Em);
        b(fs.IS_PINNED);
        b(fs.En);
        b(fs.Eo);
        b(fs.PARENTS);
        b(fs.Ep);
        b(fs.Eq);
        b(fs.Er);
        b(fs.Es);
        b(fs.Et);
        b(fs.Eu);
        b(fs.Ev);
        b(fs.Ew);
        b(fs.Ex);
        b(fs.Ey);
        b(fs.Ez);
        b(fs.EA);
        b(fs.EB);
        b(fs.EC);
        b(fs.ED);
        b(ft.EG);
        b(ft.EE);
        b(ft.EF);
        b(ft.EH);
        b(ft.LAST_VIEWED_BY_ME);
        b(fv.EJ);
        b(fv.EK);
    }

    public static MetadataField<?> ar(String str) {
        return (MetadataField) Ej.get(str);
    }

    private static void b(MetadataField<?> metadataField) {
        if (Ej.containsKey(metadataField.getName())) {
            throw new IllegalArgumentException("Duplicate field name registered: " + metadataField.getName());
        }
        Ej.put(metadataField.getName(), metadataField);
    }

    public static Collection<MetadataField<?>> fg() {
        return Collections.unmodifiableCollection(Ej.values());
    }
}
