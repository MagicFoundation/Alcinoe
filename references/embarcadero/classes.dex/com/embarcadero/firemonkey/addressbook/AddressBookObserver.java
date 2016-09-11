package com.embarcadero.firemonkey.addressbook;

import android.database.ContentObserver;

public class AddressBookObserver extends ContentObserver {
    private OnAddressBookChangesListener mListener;

    public AddressBookObserver(OnAddressBookChangesListener listener) {
        super(null);
        this.mListener = listener;
    }

    public void onChange(boolean selfChange) {
        super.onChange(selfChange);
        if (this.mListener != null) {
            this.mListener.onChanged(selfChange);
        }
    }
}
