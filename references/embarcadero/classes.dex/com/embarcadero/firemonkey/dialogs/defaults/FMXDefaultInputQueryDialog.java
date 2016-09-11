package com.embarcadero.firemonkey.dialogs.defaults;

import android.annotation.TargetApi;
import android.app.AlertDialog.Builder;
import android.widget.EditText;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.dialogs.FMXDialogHelpers;

@TargetApi(11)
public class FMXDefaultInputQueryDialog extends FMXDefaultStandardDialog {
    private EditText[] mValueEdits;

    public FMXDefaultInputQueryDialog(FMXNativeActivity activity, int theme, String title, String[] prompts, String[] values, String[] captions) {
        super(activity);
        Builder builder = new Builder(activity, theme);
        this.mValueEdits = FMXDialogHelpers.generateInputQuery(activity, builder, title, prompts, values, captions, this);
        setFragmentDialog(builder.create());
    }

    public String[] getValues() {
        String[] values = new String[this.mValueEdits.length];
        for (int i = 0; i < this.mValueEdits.length; i++) {
            values[i] = this.mValueEdits[i].getText().toString();
        }
        return values;
    }
}
