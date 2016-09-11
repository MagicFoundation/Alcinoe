package com.google.android.gms.plus.model.people;

import com.google.android.gms.common.data.DataBuffer;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.common.data.c;
import com.google.android.gms.internal.ir;
import com.google.android.gms.internal.jc;

public final class PersonBuffer extends DataBuffer<Person> {
    private final c<ir> Tu;

    public PersonBuffer(DataHolder dataHolder) {
        super(dataHolder);
        if (dataHolder.getMetadata() == null || !dataHolder.getMetadata().getBoolean("com.google.android.gms.plus.IsSafeParcelable", false)) {
            this.Tu = null;
        } else {
            this.Tu = new c(dataHolder, ir.CREATOR);
        }
    }

    public Person get(int position) {
        return this.Tu != null ? (Person) this.Tu.H(position) : new jc(this.zU, position);
    }
}
