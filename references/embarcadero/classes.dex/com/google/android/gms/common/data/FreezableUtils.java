package com.google.android.gms.common.data;

import java.util.ArrayList;

public final class FreezableUtils {
    public static <T, E extends Freezable<T>> ArrayList<T> freeze(ArrayList<E> list) {
        ArrayList<T> arrayList = new ArrayList(list.size());
        int size = list.size();
        for (int i = 0; i < size; i++) {
            arrayList.add(((Freezable) list.get(i)).freeze());
        }
        return arrayList;
    }

    public static <T, E extends Freezable<T>> ArrayList<T> freeze(E[] array) {
        ArrayList<T> arrayList = new ArrayList(array.length);
        for (Freezable freeze : array) {
            arrayList.add(freeze.freeze());
        }
        return arrayList;
    }
}
