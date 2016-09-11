package com.google.android.gms.games;

public final class PageDirection {
    public static final int NEXT = 0;
    public static final int NONE = -1;
    public static final int PREV = 1;

    private PageDirection() {
        throw new AssertionError("Uninstantiable");
    }
}
