package com.embarcadero.firemonkey.pickers.gingerbread;

import android.app.TimePickerDialog;
import android.app.TimePickerDialog.OnTimeSetListener;
import android.content.DialogInterface;
import android.content.DialogInterface.OnDismissListener;
import android.content.DialogInterface.OnShowListener;
import android.text.format.DateFormat;
import android.widget.TimePicker;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.BaseDateTimePicker;
import com.embarcadero.firemonkey.pickers.GeneratorPickerID;

public class GingerbreadTimePicker extends BaseDateTimePicker implements OnTimeSetListener, OnDismissListener, OnShowListener {
    private FMXNativeActivity mActivity;
    private int mId;
    private TimePickerDialog mPicker;

    public GingerbreadTimePicker(FMXNativeActivity activity) {
        this.mActivity = null;
        this.mPicker = null;
        this.mId = 0;
        this.mActivity = activity;
        this.mPicker = new TimePickerDialog(this.mActivity, this, 0, 0, DateFormat.is24HourFormat(this.mActivity));
        this.mPicker.setOnDismissListener(this);
        this.mPicker.setOnShowListener(this);
    }

    public void show() {
        if (this.mActivity != null) {
            this.mId = GeneratorPickerID.getUniqueID();
            this.mPicker.updateTime(this.mHour, this.mMinute);
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
        return this.mPicker.isShowing();
    }

    public void onTimeSet(TimePicker view, int hourOfDay, int minute) {
        if (this.mListener != null) {
            this.mHour = hourOfDay;
            this.mMinute = minute;
            this.mListener.onDateChanged(getTime());
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
