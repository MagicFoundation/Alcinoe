package com.google.android.gms.games.multiplayer.realtime;

import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.common.data.d;

public final class a extends d<Room> {
    public a(DataHolder dataHolder) {
        super(dataHolder);
    }

    protected /* synthetic */ Object c(int i, int i2) {
        return d(i, i2);
    }

    protected Room d(int i, int i2) {
        return new c(this.zU, i, i2);
    }

    protected String getPrimaryDataMarkerColumn() {
        return "external_match_id";
    }
}
