package com.google.android.gms.appstate;

import com.google.android.gms.common.data.DataBuffer;
import com.google.android.gms.common.data.DataHolder;

public final class AppStateBuffer extends DataBuffer<AppState> {
    public AppStateBuffer(DataHolder dataHolder) {
        super(dataHolder);
    }

    public AppState get(int position) {
        return new b(this.zU, position);
    }
}
