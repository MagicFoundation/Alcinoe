package com.embarcadero.firemonkey.dialogs.defaults;

import android.annotation.TargetApi;
import android.app.AlertDialog.Builder;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.dialogs.FMXDialogHelpers;

@TargetApi(11)
public class FMXDefaultAlertDialog extends FMXDefaultStandardDialog {
    public FMXDefaultAlertDialog(FMXNativeActivity activity, int theme, String msg, int dlgType, String[] captions, int posButton, int negButton, int neutralButton) {
        super(activity);
        Builder builder = new Builder(activity, theme);
        FMXDialogHelpers.generateAlertDialog(activity, builder, msg, dlgType, captions, posButton, negButton, neutralButton, this);
        setFragmentDialog(builder.create());
    }
}
