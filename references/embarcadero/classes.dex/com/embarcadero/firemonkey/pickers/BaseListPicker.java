package com.embarcadero.firemonkey.pickers;

public abstract class BaseListPicker extends BasePicker {
    protected int mItemIndex;
    protected CharSequence[] mItems;
    protected OnItemChangedListener mListener;

    public BaseListPicker() {
        this.mItemIndex = 1;
        this.mItems = null;
        this.mListener = null;
    }

    public void setItems(CharSequence[] items) {
        this.mItems = items;
    }

    public void setItemIndex(int itemIndex) {
        this.mItemIndex = itemIndex;
    }

    public void setListener(OnItemChangedListener listener) {
        this.mListener = listener;
    }

    public boolean hasListener() {
        return this.mListener != null;
    }
}
