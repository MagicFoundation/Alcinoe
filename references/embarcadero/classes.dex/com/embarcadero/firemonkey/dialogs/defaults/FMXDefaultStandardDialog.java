package com.embarcadero.firemonkey.dialogs.defaults;

import android.annotation.TargetApi;
import android.app.Dialog;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.dialogs.FMXStandardDialog;

@TargetApi(11)
public class FMXDefaultStandardDialog extends FMXStandardDialog {
    private FMXDefaultDialogFragment dialogFragment;

    public FMXDefaultStandardDialog(FMXNativeActivity aActivity) {
        this.dialogFragment = new FMXDefaultDialogFragment();
        this.activity = aActivity;
    }

    protected void setFragmentDialog(Dialog dialog) {
        this.mdialog = dialog;
        this.dialogFragment.setDialog(this);
    }

    public void hide() {
        if (this.dialogFragment != null) {
            this.dialogFragment.dismiss();
        }
    }

    public boolean isShown() {
        if (this.dialogFragment != null) {
            return this.dialogFragment.isVisible();
        }
        return false;
    }

    public void show() {
        if (this.activity != null) {
            this.dialogFragment.show(this.activity.getFragmentManager(), "FMXDialog");
        }
    }
}
