package com.embarcadero.firemonkey.pickers.defaults;

import android.annotation.TargetApi;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.BaseDateTimePicker;
import com.embarcadero.firemonkey.pickers.OnDateTimeChangedListener;

public class DefaultTimePicker extends BaseDateTimePicker {
    private FMXNativeActivity mActivity;
    private TimePickerFragment mPicker;

    public DefaultTimePicker(FMXNativeActivity activity) {
        this.mActivity = null;
        this.mPicker = new TimePickerFragment(this.mHour, this.mMinute);
        this.mActivity = activity;
    }

    public void setListener(OnDateTimeChangedListener listener) {
        this.mPicker.setListener(listener);
    }

    public void hide() {
        if (this.mPicker != null) {
            this.mPicker.dismiss();
        }
    }

    public boolean isShown() {
        if (this.mPicker != null) {
            return this.mPicker.isVisible();
        }
        return false;
    }

    public void show() {
        if (this.mActivity != null && !this.mPicker.isAdded()) {
            this.mPicker.setTime(this.mHour, this.mMinute);
            this.mPicker.show(this.mActivity.getFragmentManager(), "TimePicker");
        }
    }

    @TargetApi(11)
    public void setTheme(int theme) {
        super.setTheme(theme);
        this.mPicker.setTheme(this.mTheme);
    }
}
