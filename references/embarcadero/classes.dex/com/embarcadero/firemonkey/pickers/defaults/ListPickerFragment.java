package com.embarcadero.firemonkey.pickers.defaults;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.os.Bundle;
import com.embarcadero.firemonkey.pickers.OnItemChangedListener;

@SuppressLint({"NewApi"})
public class ListPickerFragment extends DialogFragment {
    private AlertDialog mDialog;
    private int mItemIndex;
    private CharSequence[] mItems;
    private OnItemChangedListener mListener;
    private int mTheme;

    public ListPickerFragment() {
        this.mDialog = null;
        this.mItems = null;
        this.mItemIndex = -1;
        this.mTheme = 0;
        this.mListener = null;
    }

    void setItems(CharSequence[] items) {
        this.mItems = items;
    }

    CharSequence[] getItems() {
        return this.mItems;
    }

    void setItemIndex(int itemIndex) {
        this.mItemIndex = itemIndex;
    }

    void setListener(OnItemChangedListener listener) {
        this.mListener = listener;
    }

    @SuppressLint({"NewApi"})
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        Builder builder = new Builder(getActivity(), this.mTheme);
        builder.setSingleChoiceItems(this.mItems, this.mItemIndex, new OnClickListener() {
            public void onClick(DialogInterface dialog, int which) {
                if (ListPickerFragment.this.mListener != null) {
                    ListPickerFragment.this.mListener.onItemChanged(which);
                    dialog.dismiss();
                }
            }
        });
        this.mDialog = builder.create();
        return this.mDialog;
    }

    @SuppressLint({"NewApi"})
    public void onStart() {
        super.onStart();
        if (this.mListener != null) {
            this.mListener.onShow();
        }
    }

    @SuppressLint({"NewApi"})
    public void onDismiss(DialogInterface dialog) {
        super.onDismiss(dialog);
        if (this.mListener != null) {
            this.mListener.onHide();
        }
    }

    public void setTheme(int theme) {
        this.mTheme = theme;
    }
}
