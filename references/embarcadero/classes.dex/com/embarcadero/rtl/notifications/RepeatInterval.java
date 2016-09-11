package com.embarcadero.rtl.notifications;

import java.util.Calendar;

public class RepeatInterval {
    public static final int REPEAT_INTERVAL_DAY = 4;
    public static final int REPEAT_INTERVAL_ERA = 10;
    public static final int REPEAT_INTERVAL_HOUR = 3;
    public static final int REPEAT_INTERVAL_MINUTE = 2;
    public static final int REPEAT_INTERVAL_MONTH = 7;
    public static final int REPEAT_INTERVAL_NONE = 0;
    public static final int REPEAT_INTERVAL_QUAERTER = 8;
    public static final int REPEAT_INTERVAL_SECOND = 1;
    public static final int REPEAT_INTERVAL_WEEK = 5;
    public static final int REPEAT_INTERVAL_WEEKDAYS = 6;
    public static final int REPEAT_INTERVAL_YEAR = 9;

    public static long getRepeatIntervalMSsec(int repeatIntervalType) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeInMillis(System.currentTimeMillis());
        switch (repeatIntervalType) {
            case REPEAT_INTERVAL_SECOND /*1*/:
                calendar.add(13, REPEAT_INTERVAL_SECOND);
                return calendar.getTimeInMillis();
            case REPEAT_INTERVAL_MINUTE /*2*/:
                calendar.add(12, REPEAT_INTERVAL_SECOND);
                return calendar.getTimeInMillis();
            case REPEAT_INTERVAL_HOUR /*3*/:
                calendar.add(REPEAT_INTERVAL_ERA, REPEAT_INTERVAL_SECOND);
                return calendar.getTimeInMillis();
            case REPEAT_INTERVAL_DAY /*4*/:
                calendar.add(REPEAT_INTERVAL_WEEK, REPEAT_INTERVAL_SECOND);
                return calendar.getTimeInMillis();
            case REPEAT_INTERVAL_WEEK /*5*/:
                calendar.add(REPEAT_INTERVAL_WEEK, REPEAT_INTERVAL_MONTH);
                return calendar.getTimeInMillis();
            case REPEAT_INTERVAL_MONTH /*7*/:
                calendar.add(REPEAT_INTERVAL_MINUTE, REPEAT_INTERVAL_SECOND);
                return calendar.getTimeInMillis();
            case REPEAT_INTERVAL_QUAERTER /*8*/:
                calendar.add(REPEAT_INTERVAL_MINUTE, REPEAT_INTERVAL_HOUR);
                return calendar.getTimeInMillis();
            case REPEAT_INTERVAL_YEAR /*9*/:
                calendar.add(REPEAT_INTERVAL_SECOND, REPEAT_INTERVAL_SECOND);
                return calendar.getTimeInMillis();
            case REPEAT_INTERVAL_ERA /*10*/:
                calendar.add(REPEAT_INTERVAL_NONE, REPEAT_INTERVAL_SECOND);
                return calendar.getTimeInMillis();
            default:
                return 0;
        }
    }
}
