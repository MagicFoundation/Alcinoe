package com.google.android.gms.appstate;

import com.google.android.gms.common.data.Freezable;

public interface AppState extends Freezable<AppState> {
    byte[] getConflictData();

    String getConflictVersion();

    int getKey();

    byte[] getLocalData();

    String getLocalVersion();

    boolean hasConflict();
}
