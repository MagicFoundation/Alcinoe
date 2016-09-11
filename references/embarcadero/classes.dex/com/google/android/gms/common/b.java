package com.google.android.gms.common;

import android.app.Dialog;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.support.v4.app.FragmentManager;
import com.google.android.gms.internal.er;

public class b extends DialogFragment {
    private Dialog mDialog;
    private OnCancelListener yK;

    public b() {
        this.mDialog = null;
        this.yK = null;
    }

    public static b a(Dialog dialog, OnCancelListener onCancelListener) {
        b bVar = new b();
        Dialog dialog2 = (Dialog) er.b((Object) dialog, (Object) "Cannot display null dialog");
        dialog2.setOnCancelListener(null);
        dialog2.setOnDismissListener(null);
        bVar.mDialog = dialog2;
        if (onCancelListener != null) {
            bVar.yK = onCancelListener;
        }
        return bVar;
    }

    public void onCancel(DialogInterface dialog) {
        if (this.yK != null) {
            this.yK.onCancel(dialog);
        }
    }

    public Dialog onCreateDialog(Bundle savedInstanceState) {
        return this.mDialog;
    }

    public void show(FragmentManager manager, String tag) {
        super.show(manager, tag);
    }
}
