package com.embarcadero.firemonkey.pickers.defaults;

import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.BaseDateTimePicker;
import com.embarcadero.firemonkey.pickers.BaseListPicker;
import com.embarcadero.firemonkey.pickers.BasePickersFactory;

public class DefaultPickersFactory extends BasePickersFactory {
    public BaseDateTimePicker createDatePicker(FMXNativeActivity activity) {
        return new DefaultDatePicker(activity);
    }

    public BaseDateTimePicker createTimePicker(FMXNativeActivity activity) {
        return new DefaultTimePicker(activity);
    }

    public BaseListPicker createListPicker(FMXNativeActivity activity) {
        return new DefaultListPicker(activity);
    }
}
