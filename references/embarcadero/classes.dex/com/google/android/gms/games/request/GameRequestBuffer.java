package com.google.android.gms.games.request;

import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.common.data.d;

public final class GameRequestBuffer extends d<GameRequest> {
    public GameRequestBuffer(DataHolder dataHolder) {
        super(dataHolder);
    }

    protected /* synthetic */ Object c(int i, int i2) {
        return getEntry(i, i2);
    }

    protected GameRequest getEntry(int rowIndex, int numChildren) {
        return new a(this.zU, rowIndex, numChildren);
    }

    protected String getPrimaryDataMarkerColumn() {
        return "external_request_id";
    }
}
