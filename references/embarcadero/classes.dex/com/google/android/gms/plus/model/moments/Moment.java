package com.google.android.gms.plus.model.moments;

import com.google.android.gms.common.data.Freezable;
import com.google.android.gms.internal.im;
import com.google.android.gms.internal.io;
import java.util.HashSet;
import java.util.Set;

public interface Moment extends Freezable<Moment> {

    public static class Builder {
        private String Oc;
        private final Set<Integer> RM;
        private im SH;
        private im SI;
        private String Sz;
        private String uS;

        public Builder() {
            this.RM = new HashSet();
        }

        public Moment build() {
            return new io(this.RM, this.uS, this.SH, this.Sz, this.SI, this.Oc);
        }

        public Builder setId(String id) {
            this.uS = id;
            this.RM.add(Integer.valueOf(2));
            return this;
        }

        public Builder setResult(ItemScope result) {
            this.SH = (im) result;
            this.RM.add(Integer.valueOf(4));
            return this;
        }

        public Builder setStartDate(String startDate) {
            this.Sz = startDate;
            this.RM.add(Integer.valueOf(5));
            return this;
        }

        public Builder setTarget(ItemScope target) {
            this.SI = (im) target;
            this.RM.add(Integer.valueOf(6));
            return this;
        }

        public Builder setType(String type) {
            this.Oc = type;
            this.RM.add(Integer.valueOf(7));
            return this;
        }
    }

    String getId();

    ItemScope getResult();

    String getStartDate();

    ItemScope getTarget();

    String getType();

    boolean hasId();

    boolean hasResult();

    boolean hasStartDate();

    boolean hasTarget();

    boolean hasType();
}
