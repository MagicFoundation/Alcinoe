package com.embarcadero.firemonkey.pickers.gingerbread;

import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.content.DialogInterface.OnDismissListener;
import android.content.DialogInterface.OnShowListener;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.BaseListPicker;
import com.embarcadero.firemonkey.pickers.GeneratorPickerID;

public class GingerbreadListPicker extends BaseListPicker implements OnDismissListener, OnShowListener {
    private FMXNativeActivity mActivity;
    private int mId;
    private AlertDialog mPicker;

    public GingerbreadListPicker(FMXNativeActivity activity) {
        this.mActivity = null;
        this.mPicker = null;
        this.mId = 0;
        this.mActivity = activity;
    }

    public void createNativePicker() {
        Builder builder = new Builder(this.mActivity);
        builder.setSingleChoiceItems(this.mItems, this.mItemIndex, new OnClickListener() {
            public void onClick(DialogInterface dialog, int which) {
                if (GingerbreadListPicker.this.mListener != null) {
                    GingerbreadListPicker.this.mListener.onItemChanged(which);
                    dialog.dismiss();
                }
            }
        });
        this.mPicker = builder.create();
        this.mPicker.setOnDismissListener(this);
        this.mPicker.setOnShowListener(this);
    }

    public void show() {
        if (this.mActivity != null) {
            this.mId = GeneratorPickerID.getUniqueID();
            createNativePicker();
            this.mActivity.showDialog(this.mId, this.mPicker);
        }
    }

    public void hide() {
        if (this.mPicker != null) {
            this.mPicker.dismiss();
            this.mActivity.removeDialog(this.mId);
        }
    }

    public boolean isShown() {
        if (this.mPicker != null) {
            return this.mPicker.isShowing();
        }
        return false;
    }

    public void onDismiss(DialogInterface dialog) {
        if (hasListener()) {
            this.mListener.onHide();
        }
    }

    public void onShow(DialogInterface dialog) {
        if (hasListener()) {
            this.mListener.onShow();
        }
    }
}
