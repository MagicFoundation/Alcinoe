package com.google.android.gms.plus.model.moments;

import com.google.android.gms.common.data.DataBuffer;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.internal.iq;

public final class MomentBuffer extends DataBuffer<Moment> {
    public MomentBuffer(DataHolder dataHolder) {
        super(dataHolder);
    }

    public Moment get(int position) {
        return new iq(this.zU, position);
    }
}
