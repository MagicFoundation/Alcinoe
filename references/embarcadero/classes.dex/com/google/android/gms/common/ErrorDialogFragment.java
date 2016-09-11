package com.google.android.gms.common;

import android.app.Dialog;
import android.app.DialogFragment;
import android.app.FragmentManager;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.os.Bundle;
import com.google.android.gms.internal.er;

public class ErrorDialogFragment extends DialogFragment {
    private Dialog mDialog;
    private OnCancelListener yK;

    public ErrorDialogFragment() {
        this.mDialog = null;
        this.yK = null;
    }

    public static ErrorDialogFragment newInstance(Dialog dialog) {
        return newInstance(dialog, null);
    }

    public static ErrorDialogFragment newInstance(Dialog dialog, OnCancelListener cancelListener) {
        ErrorDialogFragment errorDialogFragment = new ErrorDialogFragment();
        Dialog dialog2 = (Dialog) er.b((Object) dialog, (Object) "Cannot display null dialog");
        dialog2.setOnCancelListener(null);
        dialog2.setOnDismissListener(null);
        errorDialogFragment.mDialog = dialog2;
        if (cancelListener != null) {
            errorDialogFragment.yK = cancelListener;
        }
        return errorDialogFragment;
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
