package com.embarcadero.firemonkey.pickers.defaults;

import android.app.Dialog;
import android.app.DialogFragment;
import android.app.TimePickerDialog;
import android.app.TimePickerDialog.OnTimeSetListener;
import android.os.Bundle;
import android.text.format.DateFormat;
import android.widget.TimePicker;
import com.embarcadero.firemonkey.pickers.BaseDateTimePicker;
import com.embarcadero.firemonkey.pickers.OnDateTimeChangedListener;
import java.util.Calendar;

public class TimePickerFragment extends DialogFragment implements OnTimeSetListener {
    private int mHour;
    private OnDateTimeChangedListener mListener;
    private int mMin;
    private int mTheme;

    public TimePickerFragment(int hour, int min) {
        this.mHour = 0;
        this.mMin = 0;
        this.mTheme = 0;
        this.mListener = null;
        setTime(hour, min);
    }

    public void setTime(int hour, int min) {
        this.mHour = hour;
        this.mMin = min;
    }

    public void setListener(OnDateTimeChangedListener listener) {
        this.mListener = listener;
    }

    public Dialog onCreateDialog(Bundle savedInstanceState) {
        return new TimePickerDialog(getActivity(), this.mTheme, this, this.mHour, this.mMin, DateFormat.is24HourFormat(getActivity()));
    }

    public void onTimeSet(TimePicker view, int hourOfDay, int minute) {
        if (hasListener()) {
            Calendar c = Calendar.getInstance(BaseDateTimePicker.getGMTTimeZone());
            c.set(11, hourOfDay);
            c.set(12, minute);
            this.mListener.onDateChanged(c.getTime());
        }
    }

    public void onStart() {
        super.onStart();
        if (hasListener()) {
            this.mListener.onShow();
        }
    }

    public void onStop() {
        super.onStop();
        if (hasListener()) {
            this.mListener.onHide();
        }
    }

    public void setTheme(int theme) {
        this.mTheme = theme;
    }

    private boolean hasListener() {
        return this.mListener != null;
    }
}
