package com.embarcadero.firemonkey.dialogs.gingerbread;

import android.app.AlertDialog.Builder;
import android.widget.EditText;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.dialogs.FMXDialogHelpers;

public class FMXGingerbreadInputQueryDialog extends FMXGingerbreadStandardDialog {
    private EditText[] mValueEdits;

    public FMXGingerbreadInputQueryDialog(FMXNativeActivity activity, int theme, String title, String[] prompts, String[] values, String[] captions) {
        super(activity);
        Builder builder = new Builder(activity);
        this.mValueEdits = FMXDialogHelpers.generateInputQuery(activity, builder, title, prompts, values, captions, this);
        this.mdialog = builder.create();
    }

    public String[] getValues() {
        String[] values = new String[this.mValueEdits.length];
        for (int i = 0; i < this.mValueEdits.length; i++) {
            values[i] = this.mValueEdits[i].getText().toString();
        }
        return values;
    }
}
