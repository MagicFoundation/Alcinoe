package com.embarcadero.firemonkey.pickers;

public class GeneratorPickerID {
    protected static int mID;

    static {
        mID = 0;
    }

    public static int getUniqueID() {
        int i = mID;
        mID = i + 1;
        return i;
    }
}
