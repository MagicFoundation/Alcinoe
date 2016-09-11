package com.embarcadero.firemonkey.pickers.defaults;

import android.app.DatePickerDialog;
import android.app.DatePickerDialog.OnDateSetListener;
import android.app.Dialog;
import android.app.DialogFragment;
import android.os.Bundle;
import android.widget.DatePicker;
import com.embarcadero.firemonkey.pickers.BaseDateTimePicker;
import com.embarcadero.firemonkey.pickers.OnDateTimeChangedListener;
import java.util.Calendar;
import java.util.Date;

public class DatePickerFragment extends DialogFragment implements OnDateSetListener {
    private int mDay;
    private boolean mHasDateConstraints;
    private OnDateTimeChangedListener mListener;
    private long mMaxDate;
    private long mMinDate;
    private int mMonth;
    private DatePickerDialog mPickerDialog;
    private int mTheme;
    private int mYear;

    public DatePickerFragment(int year, int month, int day) {
        this.mHasDateConstraints = false;
        this.mTheme = 0;
        this.mListener = null;
        setDate(year, month, day);
    }

    public void setMinDate(Date date) {
        this.mMinDate = Math.min(date.getTime(), this.mMaxDate);
    }

    public void setMaxDate(Date date) {
        this.mMaxDate = Math.max(date.getTime(), this.mMinDate);
    }

    public void setHasDateConstraints(boolean hasDateConstraints) {
        this.mHasDateConstraints = hasDateConstraints;
    }

    public void setDate(int year, int month, int day) {
        this.mYear = year;
        this.mMonth = month;
        this.mDay = day;
    }

    public void setListener(OnDateTimeChangedListener listener) {
        this.mListener = listener;
    }

    public Dialog onCreateDialog(Bundle savedInstanceState) {
        this.mPickerDialog = new DatePickerDialog(getActivity(), this.mTheme, this, this.mYear, this.mMonth, this.mDay);
        if (this.mHasDateConstraints) {
            this.mPickerDialog.getDatePicker().setMinDate(this.mMinDate);
            this.mPickerDialog.getDatePicker().setMaxDate(this.mMaxDate);
        }
        return this.mPickerDialog;
    }

    public void onDateSet(DatePicker view, int year, int month, int day) {
        if (hasListener()) {
            Calendar c = Calendar.getInstance(BaseDateTimePicker.getGMTTimeZone());
            c.set(1, year);
            c.set(2, month);
            c.set(5, day);
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

    private boolean hasListener() {
        return this.mListener != null;
    }

    public void setTheme(int theme) {
        this.mTheme = theme;
    }
}
