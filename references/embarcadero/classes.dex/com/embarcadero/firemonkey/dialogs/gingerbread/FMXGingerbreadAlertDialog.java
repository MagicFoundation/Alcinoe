package com.embarcadero.firemonkey.dialogs.gingerbread;

import android.app.AlertDialog.Builder;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.dialogs.FMXDialogHelpers;

public class FMXGingerbreadAlertDialog extends FMXGingerbreadStandardDialog {
    public FMXGingerbreadAlertDialog(FMXNativeActivity activity, int theme, String msg, int dlgType, String[] captions, int posButton, int negButton, int neutralButton) {
        super(activity);
        Builder builder = new Builder(activity);
        FMXDialogHelpers.generateAlertDialog(activity, builder, msg, dlgType, captions, posButton, negButton, neutralButton, this);
        this.mdialog = builder.create();
    }
}
