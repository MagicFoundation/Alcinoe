package com.embarcadero.firemonkey.dialogs;

import android.app.AlertDialog.Builder;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.support.v4.media.TransportMediator;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class FMXDialogHelpers {

    /* renamed from: com.embarcadero.firemonkey.dialogs.FMXDialogHelpers.1 */
    static class AnonymousClass1 implements OnClickListener {
        final /* synthetic */ FMXStandardDialog val$fmxdialog;
        final /* synthetic */ int val$posButton;

        AnonymousClass1(FMXStandardDialog fMXStandardDialog, int i) {
            this.val$fmxdialog = fMXStandardDialog;
            this.val$posButton = i;
        }

        public void onClick(DialogInterface dialog, int id) {
            this.val$fmxdialog.setModalResult(this.val$posButton);
        }
    }

    /* renamed from: com.embarcadero.firemonkey.dialogs.FMXDialogHelpers.2 */
    static class AnonymousClass2 implements OnClickListener {
        final /* synthetic */ FMXStandardDialog val$fmxdialog;
        final /* synthetic */ int val$negButton;

        AnonymousClass2(FMXStandardDialog fMXStandardDialog, int i) {
            this.val$fmxdialog = fMXStandardDialog;
            this.val$negButton = i;
        }

        public void onClick(DialogInterface dialog, int id) {
            this.val$fmxdialog.setModalResult(this.val$negButton);
        }
    }

    /* renamed from: com.embarcadero.firemonkey.dialogs.FMXDialogHelpers.3 */
    static class AnonymousClass3 implements OnClickListener {
        final /* synthetic */ FMXStandardDialog val$fmxdialog;
        final /* synthetic */ int val$neutralButton;

        AnonymousClass3(FMXStandardDialog fMXStandardDialog, int i) {
            this.val$fmxdialog = fMXStandardDialog;
            this.val$neutralButton = i;
        }

        public void onClick(DialogInterface dialog, int id) {
            this.val$fmxdialog.setModalResult(this.val$neutralButton);
        }
    }

    /* renamed from: com.embarcadero.firemonkey.dialogs.FMXDialogHelpers.4 */
    static class AnonymousClass4 implements OnClickListener {
        final /* synthetic */ FMXStandardDialog val$fmxdialog;

        AnonymousClass4(FMXStandardDialog fMXStandardDialog) {
            this.val$fmxdialog = fMXStandardDialog;
        }

        public void onClick(DialogInterface dialog, int id) {
            this.val$fmxdialog.setModalResult(1);
        }
    }

    /* renamed from: com.embarcadero.firemonkey.dialogs.FMXDialogHelpers.5 */
    static class AnonymousClass5 implements OnClickListener {
        final /* synthetic */ FMXStandardDialog val$fmxdialog;

        AnonymousClass5(FMXStandardDialog fMXStandardDialog) {
            this.val$fmxdialog = fMXStandardDialog;
        }

        public void onClick(DialogInterface dialog, int id) {
            this.val$fmxdialog.setModalResult(2);
        }
    }

    public static final void generateAlertDialog(FMXNativeActivity activity, Builder builder, String msg, int dlgType, String[] captions, int posButton, int negButton, int neutralButton, FMXStandardDialog fmxdialog) {
        switch (dlgType) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                builder.setIcon(17301543);
                break;
            case DetectedActivity.ON_FOOT /*2*/:
                builder.setIcon(17301659);
                break;
        }
        if (posButton >= 0) {
            builder.setPositiveButton(captions[0], new AnonymousClass1(fmxdialog, posButton));
        }
        if (negButton >= 0) {
            builder.setNegativeButton(captions[1], new AnonymousClass2(fmxdialog, negButton));
        }
        if (neutralButton >= 0) {
            builder.setNeutralButton(captions[2], new AnonymousClass3(fmxdialog, neutralButton));
        }
        builder.setMessage(msg);
    }

    public static final EditText[] generateInputQuery(FMXNativeActivity activity, Builder builder, String title, String[] prompts, String[] values, String[] captions, FMXStandardDialog fmxdialog) {
        builder.setTitle(title);
        LinearLayout layout = new LinearLayout(activity);
        layout.setOrientation(1);
        TextView[] labels = new TextView[prompts.length];
        EditText[] ValueEdits = new EditText[values.length];
        int i = 0;
        while (i < ValueEdits.length) {
            int inputType = 1;
            if (i < labels.length) {
                String labelText = prompts[i];
                labels[i] = new TextView(activity);
                if (prompts[i].length() > 0 && prompts[i].charAt(0) < '2') {
                    inputType = 1 | TransportMediator.FLAG_KEY_MEDIA_NEXT;
                    labelText = prompts[i].substring(1);
                }
                labels[i].setText(labelText);
                layout.addView(labels[i]);
            }
            ValueEdits[i] = new EditText(activity);
            ValueEdits[i].setInputType(inputType);
            ValueEdits[i].getEditableText().append(values[i]);
            if (i == 0) {
                ValueEdits[i].selectAll();
            }
            layout.addView(ValueEdits[i]);
            i++;
        }
        builder.setView(layout);
        builder.setPositiveButton(captions[0], new AnonymousClass4(fmxdialog));
        builder.setNegativeButton(captions[1], new AnonymousClass5(fmxdialog));
        return ValueEdits;
    }
}
