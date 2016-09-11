package com.embarcadero.firemonkey.pickers;

import android.os.Build.VERSION;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.defaults.DefaultPickersFactory;
import com.embarcadero.firemonkey.pickers.gingerbread.GingerbreadPickersFactory;

public abstract class BasePickersFactory {
    private static BasePickersFactory mFactory;

    public abstract BaseDateTimePicker createDatePicker(FMXNativeActivity fMXNativeActivity);

    public abstract BaseListPicker createListPicker(FMXNativeActivity fMXNativeActivity);

    public abstract BaseDateTimePicker createTimePicker(FMXNativeActivity fMXNativeActivity);

    static {
        mFactory = null;
    }

    public static BasePickersFactory getFactory() {
        if (mFactory == null) {
            if (VERSION.SDK_INT == 9 || VERSION.SDK_INT == 10) {
                mFactory = new GingerbreadPickersFactory();
            } else {
                mFactory = new DefaultPickersFactory();
            }
        }
        return mFactory;
    }
}
