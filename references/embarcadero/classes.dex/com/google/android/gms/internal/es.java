package com.google.android.gms.internal;

import android.content.Context;
import android.os.IBinder;
import android.view.View;
import com.google.android.gms.dynamic.c;
import com.google.android.gms.dynamic.e;
import com.google.android.gms.dynamic.e.a;

public final class es extends e<eo> {
    private static final es Cg;

    static {
        Cg = new es();
    }

    private es() {
        super("com.google.android.gms.common.ui.SignInButtonCreatorImpl");
    }

    public static View d(Context context, int i, int i2) throws a {
        return Cg.e(context, i, i2);
    }

    private View e(Context context, int i, int i2) throws a {
        try {
            return (View) c.b(((eo) z(context)).a(c.h(context), i, i2));
        } catch (Throwable e) {
            throw new a("Could not get button with size " + i + " and color " + i2, e);
        }
    }

    public eo B(IBinder iBinder) {
        return eo.a.A(iBinder);
    }

    public /* synthetic */ Object d(IBinder iBinder) {
        return B(iBinder);
    }
}
