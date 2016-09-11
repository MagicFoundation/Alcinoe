package com.google.android.gms.common.data;

import java.util.ArrayList;

public abstract class d<T> extends DataBuffer<T> {
    private boolean Ap;
    private ArrayList<Integer> Aq;

    protected d(DataHolder dataHolder) {
        super(dataHolder);
        this.Ap = false;
    }

    private int K(int i) {
        return (i < 0 || i == this.Aq.size()) ? 0 : i == this.Aq.size() + -1 ? this.zU.getCount() - ((Integer) this.Aq.get(i)).intValue() : ((Integer) this.Aq.get(i + 1)).intValue() - ((Integer) this.Aq.get(i)).intValue();
    }

    private void dK() {
        synchronized (this) {
            if (!this.Ap) {
                int count = this.zU.getCount();
                this.Aq = new ArrayList();
                if (count > 0) {
                    this.Aq.add(Integer.valueOf(0));
                    String primaryDataMarkerColumn = getPrimaryDataMarkerColumn();
                    String string = this.zU.getString(primaryDataMarkerColumn, 0, this.zU.I(0));
                    int i = 1;
                    while (i < count) {
                        String string2 = this.zU.getString(primaryDataMarkerColumn, i, this.zU.I(i));
                        if (string2.equals(string)) {
                            string2 = string;
                        } else {
                            this.Aq.add(Integer.valueOf(i));
                        }
                        i++;
                        string = string2;
                    }
                }
                this.Ap = true;
            }
        }
    }

    int J(int i) {
        if (i >= 0 && i < this.Aq.size()) {
            return ((Integer) this.Aq.get(i)).intValue();
        }
        throw new IllegalArgumentException("Position " + i + " is out of bounds for this buffer");
    }

    protected abstract T c(int i, int i2);

    public final T get(int position) {
        dK();
        return c(J(position), K(position));
    }

    public int getCount() {
        dK();
        return this.Aq.size();
    }

    protected abstract String getPrimaryDataMarkerColumn();
}
