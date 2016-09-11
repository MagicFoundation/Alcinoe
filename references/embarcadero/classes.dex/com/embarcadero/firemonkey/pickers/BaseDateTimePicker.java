package com.embarcadero.firemonkey.pickers;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

public abstract class BaseDateTimePicker extends BasePicker {
    protected static TimeZone mGMTTimeZone;
    protected int mDay;
    protected int mHour;
    protected OnDateTimeChangedListener mListener;
    protected int mMinute;
    protected int mMonth;
    protected int mYear;

    static {
        mGMTTimeZone = TimeZone.getTimeZone("GMT+00:00");
    }

    public BaseDateTimePicker() {
        this.mListener = null;
        this.mYear = 0;
        this.mMonth = 0;
        this.mDay = 0;
        this.mHour = 0;
        this.mMinute = 0;
        setDate(System.currentTimeMillis());
    }

    public void setListener(OnDateTimeChangedListener listener) {
        this.mListener = listener;
    }

    public void setDate(long timeInMillis) {
        Calendar c = Calendar.getInstance(mGMTTimeZone);
        c.setTimeInMillis(timeInMillis);
        this.mYear = c.get(1);
        this.mMonth = c.get(2);
        this.mDay = c.get(5);
        this.mHour = c.get(11);
        this.mMinute = c.get(12);
    }

    public Date getDate() {
        Calendar c = Calendar.getInstance(mGMTTimeZone);
        c.set(1, this.mYear);
        c.set(2, this.mMonth);
        c.set(5, this.mDay);
        return c.getTime();
    }

    public Date getTime() {
        Calendar c = Calendar.getInstance(mGMTTimeZone);
        c.set(11, this.mHour);
        c.set(12, this.mMinute);
        return c.getTime();
    }

    public boolean hasListener() {
        return this.mListener != null;
    }

    public static TimeZone getGMTTimeZone() {
        return mGMTTimeZone;
    }
}
