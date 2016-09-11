package com.embarcadero.firemonkey.pickers.gingerbread;

import android.app.DatePickerDialog;
import android.app.DatePickerDialog.OnDateSetListener;
import android.content.DialogInterface;
import android.content.DialogInterface.OnDismissListener;
import android.content.DialogInterface.OnShowListener;
import android.widget.DatePicker;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.BaseDateTimePicker;
import com.embarcadero.firemonkey.pickers.GeneratorPickerID;

public class GingerbreadDatePicker extends BaseDateTimePicker implements OnDateSetListener, OnDismissListener, OnShowListener {
    private FMXNativeActivity mActivity;
    private int mId;
    private DatePickerDialog mPicker;

    public GingerbreadDatePicker(FMXNativeActivity activity) {
        this.mActivity = null;
        this.mPicker = null;
        this.mId = 0;
        this.mActivity = activity;
        this.mPicker = new DatePickerDialog(this.mActivity, this, this.mYear, this.mMonth, this.mDay);
        this.mPicker.setOnDismissListener(this);
        this.mPicker.setOnShowListener(this);
    }

    public void show() {
        if (this.mActivity != null) {
            this.mId = GeneratorPickerID.getUniqueID();
            this.mPicker.updateDate(this.mYear, this.mMonth, this.mDay);
            this.mActivity.showDialog(this.mId, this.mPicker);
        }
    }

    public void hide() {
        if (isShown()) {
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

    public void onDateSet(DatePicker view, int year, int monthOfYear, int dayOfMonth) {
        if (hasListener()) {
            this.mYear = year;
            this.mMonth = monthOfYear;
            this.mDay = dayOfMonth;
            this.mListener.onDateChanged(getDate());
        }
    }

    public void onShow(DialogInterface dialog) {
        if (hasListener()) {
            this.mListener.onShow();
        }
    }

    public void onDismiss(DialogInterface dialog) {
        if (hasListener()) {
            this.mListener.onHide();
        }
    }
}
