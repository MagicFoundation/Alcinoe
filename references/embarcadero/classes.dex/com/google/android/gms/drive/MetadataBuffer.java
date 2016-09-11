package com.google.android.gms.drive;

import com.google.android.gms.common.data.DataBuffer;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.drive.metadata.MetadataField;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;
import com.google.android.gms.drive.metadata.internal.c;
import java.util.ArrayList;
import java.util.List;

public final class MetadataBuffer extends DataBuffer<Metadata> {
    private static final String[] Dg;
    private final String Dh;

    private static class a extends Metadata {
        private final int Di;
        private final DataHolder zU;
        private final int zX;

        public a(DataHolder dataHolder, int i) {
            this.zU = dataHolder;
            this.Di = i;
            this.zX = dataHolder.I(i);
        }

        protected <T> T a(MetadataField<T> metadataField) {
            return metadataField.c(this.zU, this.Di, this.zX);
        }

        public Metadata eQ() {
            MetadataBundle fh = MetadataBundle.fh();
            for (MetadataField a : c.fg()) {
                a.a(this.zU, fh, this.Di, this.zX);
            }
            return new b(fh);
        }

        public /* synthetic */ Object freeze() {
            return eQ();
        }

        public boolean isDataValid() {
            return !this.zU.isClosed();
        }
    }

    static {
        List arrayList = new ArrayList();
        for (MetadataField ff : c.fg()) {
            arrayList.addAll(ff.ff());
        }
        Dg = (String[]) arrayList.toArray(new String[0]);
    }

    public MetadataBuffer(DataHolder dataHolder, String nextPageToken) {
        super(dataHolder);
        this.Dh = nextPageToken;
    }

    public Metadata get(int row) {
        return new a(this.zU, row);
    }

    public String getNextPageToken() {
        return this.Dh;
    }
}
