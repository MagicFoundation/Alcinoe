package com.embarcadero.firemonkey.pickers.defaults;

import android.annotation.TargetApi;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.BaseDateTimePicker;
import com.embarcadero.firemonkey.pickers.OnDateTimeChangedListener;

public class DefaultDatePicker extends BaseDateTimePicker {
    private FMXNativeActivity mActivity;
    private DatePickerFragment mPicker;

    public DefaultDatePicker(FMXNativeActivity activity) {
        this.mActivity = null;
        this.mPicker = new DatePickerFragment(this.mYear, this.mMonth, this.mDay);
        this.mActivity = activity;
    }

    public void setListener(OnDateTimeChangedListener listener) {
        if (this.mPicker != null) {
            this.mPicker.setListener(listener);
        }
    }

    public void hide() {
        if (isShown()) {
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
            this.mPicker.setDate(this.mYear, this.mMonth, this.mDay);
            this.mPicker.show(this.mActivity.getFragmentManager(), "DatePicker");
        }
    }

    @TargetApi(11)
    public void setTheme(int theme) {
        super.setTheme(theme);
        this.mPicker.setTheme(this.mTheme);
    }
}
