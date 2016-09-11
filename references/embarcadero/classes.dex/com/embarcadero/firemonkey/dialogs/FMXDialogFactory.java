package com.embarcadero.firemonkey.dialogs;

import android.os.Build.VERSION;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.dialogs.defaults.FMXDefaultDialogFactory;
import com.embarcadero.firemonkey.dialogs.gingerbread.FMXGingerbreadDialogFactory;

public abstract class FMXDialogFactory {
    private static FMXDialogFactory factory;

    public abstract FMXStandardDialog createInputQueryDialog(FMXNativeActivity fMXNativeActivity, int i, String str, String[] strArr, String[] strArr2, String[] strArr3);

    public abstract FMXStandardDialog createMessageDialog(FMXNativeActivity fMXNativeActivity, int i, String str, int i2, String[] strArr, int i3, int i4, int i5);

    static {
        factory = null;
    }

    public static FMXDialogFactory getFactory() {
        if (factory == null) {
            if (VERSION.SDK_INT == 9 || VERSION.SDK_INT == 10) {
                factory = new FMXGingerbreadDialogFactory();
            } else {
                factory = new FMXDefaultDialogFactory();
            }
        }
        return factory;
    }
}
