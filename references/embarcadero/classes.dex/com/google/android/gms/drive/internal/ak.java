package com.google.android.gms.drive.internal;

import android.os.RemoteException;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.api.a.c;

public class ak extends c {
    private final c<Status> vj;

    public ak(c<Status> cVar) {
        this.vj = cVar;
    }

    public void l(Status status) throws RemoteException {
        this.vj.b(status);
    }

    public void onSuccess() throws RemoteException {
        this.vj.b(Status.zQ);
    }
}
