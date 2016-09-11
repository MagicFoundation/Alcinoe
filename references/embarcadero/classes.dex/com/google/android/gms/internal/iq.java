package com.google.android.gms.internal;

import android.os.Parcel;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.common.data.b;
import com.google.android.gms.plus.model.moments.ItemScope;
import com.google.android.gms.plus.model.moments.Moment;

public final class iq extends b implements Moment {
    private io SJ;

    public iq(DataHolder dataHolder, int i) {
        super(dataHolder, i);
    }

    private io hV() {
        synchronized (this) {
            if (this.SJ == null) {
                byte[] byteArray = getByteArray("momentImpl");
                Parcel obtain = Parcel.obtain();
                obtain.unmarshall(byteArray, 0, byteArray.length);
                obtain.setDataPosition(0);
                this.SJ = io.CREATOR.aH(obtain);
                obtain.recycle();
            }
        }
        return this.SJ;
    }

    public /* synthetic */ Object freeze() {
        return hU();
    }

    public String getId() {
        return hV().getId();
    }

    public ItemScope getResult() {
        return hV().getResult();
    }

    public String getStartDate() {
        return hV().getStartDate();
    }

    public ItemScope getTarget() {
        return hV().getTarget();
    }

    public String getType() {
        return hV().getType();
    }

    public io hU() {
        return hV();
    }

    public boolean hasId() {
        return hV().hasId();
    }

    public boolean hasResult() {
        return hV().hasId();
    }

    public boolean hasStartDate() {
        return hV().hasStartDate();
    }

    public boolean hasTarget() {
        return hV().hasTarget();
    }

    public boolean hasType() {
        return hV().hasType();
    }
}
