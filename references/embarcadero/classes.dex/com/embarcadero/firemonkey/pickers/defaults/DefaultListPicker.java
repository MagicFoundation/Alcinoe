package com.embarcadero.firemonkey.pickers.defaults;

import android.annotation.TargetApi;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.BaseListPicker;
import com.embarcadero.firemonkey.pickers.OnItemChangedListener;

public class DefaultListPicker extends BaseListPicker {
    private FMXNativeActivity mActivity;
    private ListPickerFragment mPicker;

    public DefaultListPicker(FMXNativeActivity activity) {
        this.mPicker = new ListPickerFragment();
        this.mActivity = null;
        this.mActivity = activity;
    }

    public void setItemIndex(int itemIndex) {
        this.mPicker.setItemIndex(itemIndex);
    }

    public void setItems(CharSequence[] items) {
        this.mPicker.setItems(items);
    }

    public void setListener(OnItemChangedListener listener) {
        this.mPicker.setListener(listener);
    }

    public void hide() {
        if (this.mPicker != null) {
            this.mPicker.dismiss();
        }
    }

    public boolean isShown() {
        if (this.mPicker != null) {
            return this.mPicker.isVisible();
        }
        return false;
    }

    public void show() {
        if (this.mPicker.getItems().length > 0 && this.mActivity != null && !this.mPicker.isAdded()) {
            this.mPicker.show(this.mActivity.getFragmentManager(), "ListPicker");
        }
    }

    @TargetApi(11)
    public void setTheme(int theme) {
        super.setTheme(theme);
        this.mPicker.setTheme(this.mTheme);
    }
}
