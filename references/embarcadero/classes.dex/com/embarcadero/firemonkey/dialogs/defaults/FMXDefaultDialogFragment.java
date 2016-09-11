package com.embarcadero.firemonkey.dialogs.defaults;

import android.annotation.TargetApi;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;

@TargetApi(11)
public class FMXDefaultDialogFragment extends DialogFragment {
    private FMXDefaultStandardDialog mDialog;

    public FMXDefaultDialogFragment() {
        this.mDialog = null;
    }

    public void setDialog(FMXDefaultStandardDialog dialog) {
        this.mDialog = dialog;
    }

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        this.mDialog.getRealDialog().getWindow().setSoftInputMode(5);
    }

    public Dialog onCreateDialog(Bundle savedInstanceState) {
        return this.mDialog.getRealDialog();
    }

    public void onDismiss(DialogInterface dialog) {
        if (this.mDialog.getListener() != null) {
            int result = this.mDialog.getModalResult();
            String[] values = null;
            if (FMXDefaultInputQueryDialog.class.isInstance(this.mDialog)) {
                values = ((FMXDefaultInputQueryDialog) this.mDialog).getValues();
            }
            this.mDialog.getListener().onDialogClosed(result, values);
        }
        super.onDismiss(dialog);
    }
}
