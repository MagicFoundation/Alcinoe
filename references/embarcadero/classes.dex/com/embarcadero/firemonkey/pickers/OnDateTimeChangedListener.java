package com.embarcadero.firemonkey.pickers;

import java.util.Date;

public interface OnDateTimeChangedListener {
    void onDateChanged(Date date);

    void onHide();

    void onShow();
}
