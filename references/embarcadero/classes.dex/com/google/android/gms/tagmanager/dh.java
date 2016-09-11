package com.google.android.gms.tagmanager;

class dh extends Number implements Comparable<dh> {
    private double XF;
    private long XG;
    private boolean XH;

    private dh(double d) {
        this.XF = d;
        this.XH = false;
    }

    private dh(long j) {
        this.XG = j;
        this.XH = true;
    }

    public static dh a(Double d) {
        return new dh(d.doubleValue());
    }

    public static dh bH(String str) throws NumberFormatException {
        try {
            return new dh(Long.parseLong(str));
        } catch (NumberFormatException e) {
            try {
                return new dh(Double.parseDouble(str));
            } catch (NumberFormatException e2) {
                throw new NumberFormatException(str + " is not a valid TypedNumber");
            }
        }
    }

    public static dh v(long j) {
        return new dh(j);
    }

    public int a(dh dhVar) {
        return (kk() && dhVar.kk()) ? new Long(this.XG).compareTo(Long.valueOf(dhVar.XG)) : Double.compare(doubleValue(), dhVar.doubleValue());
    }

    public byte byteValue() {
        return (byte) ((int) longValue());
    }

    public /* synthetic */ int compareTo(Object x0) {
        return a((dh) x0);
    }

    public double doubleValue() {
        return kk() ? (double) this.XG : this.XF;
    }

    public boolean equals(Object other) {
        return (other instanceof dh) && a((dh) other) == 0;
    }

    public float floatValue() {
        return (float) doubleValue();
    }

    public int hashCode() {
        return new Long(longValue()).hashCode();
    }

    public int intValue() {
        return km();
    }

    public boolean kj() {
        return !kk();
    }

    public boolean kk() {
        return this.XH;
    }

    public long kl() {
        return kk() ? this.XG : (long) this.XF;
    }

    public int km() {
        return (int) longValue();
    }

    public short kn() {
        return (short) ((int) longValue());
    }

    public long longValue() {
        return kl();
    }

    public short shortValue() {
        return kn();
    }

    public String toString() {
        return kk() ? Long.toString(this.XG) : Double.toString(this.XF);
    }
}
