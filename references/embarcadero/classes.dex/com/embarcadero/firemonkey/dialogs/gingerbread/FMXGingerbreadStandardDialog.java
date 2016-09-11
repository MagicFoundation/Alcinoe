package com.embarcadero.firemonkey.dialogs.gingerbread;

import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.dialogs.FMXStandardDialog;

public class FMXGingerbreadStandardDialog extends FMXStandardDialog {
    public FMXGingerbreadStandardDialog(FMXNativeActivity aActivity) {
        this.activity = aActivity;
    }

    public void setModalResult(int result) {
        super.setModalResult(result);
        if (this.mlistener != null) {
            String[] values = null;
            if (FMXGingerbreadInputQueryDialog.class.isInstance(this)) {
                values = ((FMXGingerbreadInputQueryDialog) this).getValues();
            }
            this.mlistener.onDialogClosed(this.modalResult, values);
        }
    }

    public void hide() {
        getRealDialog().dismiss();
    }

    public boolean isShown() {
        if (this.mdialog != null) {
            return this.mdialog.isShowing();
        }
        return false;
    }

    public void show() {
        if (this.activity != null) {
            this.mdialog.show();
        }
    }
}
