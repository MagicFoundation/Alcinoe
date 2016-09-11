package com.google.android.gms.plus;

import android.net.Uri;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Releasable;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.plus.model.moments.Moment;
import com.google.android.gms.plus.model.moments.MomentBuffer;

public interface Moments {

    public interface LoadMomentsResult extends Releasable, Result {
        MomentBuffer getMomentBuffer();

        String getNextPageToken();

        String getUpdated();
    }

    PendingResult<LoadMomentsResult> load(GoogleApiClient googleApiClient);

    PendingResult<LoadMomentsResult> load(GoogleApiClient googleApiClient, int i, String str, Uri uri, String str2, String str3);

    PendingResult<Status> remove(GoogleApiClient googleApiClient, String str);

    PendingResult<Status> write(GoogleApiClient googleApiClient, Moment moment);
}
