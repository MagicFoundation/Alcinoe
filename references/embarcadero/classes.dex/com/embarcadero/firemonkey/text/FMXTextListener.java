package com.embarcadero.firemonkey.text;

import android.view.KeyEvent;

public interface FMXTextListener {
    void onComposingText(int i, int i2);

    void onSkipKeyEvent(KeyEvent keyEvent);

    void onTextUpdated(CharSequence charSequence, int i);
}
