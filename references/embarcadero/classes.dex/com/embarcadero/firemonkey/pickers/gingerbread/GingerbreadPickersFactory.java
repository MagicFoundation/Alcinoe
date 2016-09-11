package com.embarcadero.firemonkey.pickers.gingerbread;

import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.BaseDateTimePicker;
import com.embarcadero.firemonkey.pickers.BaseListPicker;
import com.embarcadero.firemonkey.pickers.BasePickersFactory;

public class GingerbreadPickersFactory extends BasePickersFactory {
    public BaseDateTimePicker createDatePicker(FMXNativeActivity activity) {
        return new GingerbreadDatePicker(activity);
    }

    public BaseDateTimePicker createTimePicker(FMXNativeActivity activity) {
        return new GingerbreadTimePicker(activity);
    }

    public BaseListPicker createListPicker(FMXNativeActivity activity) {
        return new GingerbreadListPicker(activity);
    }
}
