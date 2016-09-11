package com.embarcadero.firemonkey.dialogs;

import android.app.Dialog;
import com.embarcadero.firemonkey.FMXNativeActivity;

public abstract class FMXStandardDialog {
    protected FMXNativeActivity activity;
    protected Dialog mdialog;
    protected FMXDialogListener mlistener;
    protected int modalResult;

    public abstract void hide();

    public abstract boolean isShown();

    public abstract void show();

    public FMXStandardDialog() {
        this.activity = null;
        this.modalResult = -1;
    }

    protected void setModalResult(int result) {
        this.modalResult = result;
    }

    public Dialog getRealDialog() {
        return this.mdialog;
    }

    public int getModalResult() {
        return this.modalResult;
    }

    public void setListener(FMXDialogListener listener) {
        this.mlistener = listener;
    }

    public FMXDialogListener getListener() {
        return this.mlistener;
    }
}
