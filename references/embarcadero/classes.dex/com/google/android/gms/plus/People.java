package com.google.android.gms.plus;

import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Releasable;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.plus.model.people.Person;
import com.google.android.gms.plus.model.people.PersonBuffer;
import java.util.Collection;

public interface People {

    public interface OrderBy {
        public static final int ALPHABETICAL = 0;
        public static final int BEST = 1;
    }

    public interface LoadPeopleResult extends Releasable, Result {
        String getNextPageToken();

        PersonBuffer getPersonBuffer();
    }

    Person getCurrentPerson(GoogleApiClient googleApiClient);

    PendingResult<LoadPeopleResult> load(GoogleApiClient googleApiClient, Collection<String> collection);

    PendingResult<LoadPeopleResult> load(GoogleApiClient googleApiClient, String... strArr);

    PendingResult<LoadPeopleResult> loadConnected(GoogleApiClient googleApiClient);

    PendingResult<LoadPeopleResult> loadVisible(GoogleApiClient googleApiClient, int i, String str);

    PendingResult<LoadPeopleResult> loadVisible(GoogleApiClient googleApiClient, String str);
}
