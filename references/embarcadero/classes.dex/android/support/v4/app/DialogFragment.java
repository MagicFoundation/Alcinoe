package android.support.v4.app;

import android.app.Activity;
import android.app.Dialog;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.content.DialogInterface.OnDismissListener;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

public class DialogFragment extends Fragment implements OnCancelListener, OnDismissListener {
    private static final String SAVED_BACK_STACK_ID = "android:backStackId";
    private static final String SAVED_CANCELABLE = "android:cancelable";
    private static final String SAVED_DIALOG_STATE_TAG = "android:savedDialogState";
    private static final String SAVED_SHOWS_DIALOG = "android:showsDialog";
    private static final String SAVED_STYLE = "android:style";
    private static final String SAVED_THEME = "android:theme";
    public static final int STYLE_NORMAL = 0;
    public static final int STYLE_NO_FRAME = 2;
    public static final int STYLE_NO_INPUT = 3;
    public static final int STYLE_NO_TITLE = 1;
    int mBackStackId;
    boolean mCancelable;
    Dialog mDialog;
    boolean mDismissed;
    boolean mShownByMe;
    boolean mShowsDialog;
    int mStyle;
    int mTheme;
    boolean mViewDestroyed;

    public DialogFragment() {
        this.mStyle = STYLE_NORMAL;
        this.mTheme = STYLE_NORMAL;
        this.mCancelable = true;
        this.mShowsDialog = true;
        this.mBackStackId = -1;
    }

    public void setStyle(int style, int theme) {
        this.mStyle = style;
        if (this.mStyle == STYLE_NO_FRAME || this.mStyle == STYLE_NO_INPUT) {
            this.mTheme = 16973913;
        }
        if (theme != 0) {
            this.mTheme = theme;
        }
    }

    public void show(FragmentManager manager, String tag) {
        this.mDismissed = false;
        this.mShownByMe = true;
        FragmentTransaction ft = manager.beginTransaction();
        ft.add((Fragment) this, tag);
        ft.commit();
    }

    public int show(FragmentTransaction transaction, String tag) {
        this.mDismissed = false;
        this.mShownByMe = true;
        transaction.add((Fragment) this, tag);
        this.mViewDestroyed = false;
        this.mBackStackId = transaction.commit();
        return this.mBackStackId;
    }

    public void dismiss() {
        dismissInternal(false);
    }

    public void dismissAllowingStateLoss() {
        dismissInternal(true);
    }

    void dismissInternal(boolean allowStateLoss) {
        if (!this.mDismissed) {
            this.mDismissed = true;
            this.mShownByMe = false;
            if (this.mDialog != null) {
                this.mDialog.dismiss();
                this.mDialog = null;
            }
            this.mViewDestroyed = true;
            if (this.mBackStackId >= 0) {
                getFragmentManager().popBackStack(this.mBackStackId, (int) STYLE_NO_TITLE);
                this.mBackStackId = -1;
                return;
            }
            FragmentTransaction ft = getFragmentManager().beginTransaction();
            ft.remove(this);
            if (allowStateLoss) {
                ft.commitAllowingStateLoss();
            } else {
                ft.commit();
            }
        }
    }

    public Dialog getDialog() {
        return this.mDialog;
    }

    public int getTheme() {
        return this.mTheme;
    }

    public void setCancelable(boolean cancelable) {
        this.mCancelable = cancelable;
        if (this.mDialog != null) {
            this.mDialog.setCancelable(cancelable);
        }
    }

    public boolean isCancelable() {
        return this.mCancelable;
    }

    public void setShowsDialog(boolean showsDialog) {
        this.mShowsDialog = showsDialog;
    }

    public boolean getShowsDialog() {
        return this.mShowsDialog;
    }

    public void onAttach(Activity activity) {
        super.onAttach(activity);
        if (!this.mShownByMe) {
            this.mDismissed = false;
        }
    }

    public void onDetach() {
        super.onDetach();
        if (!this.mShownByMe && !this.mDismissed) {
            this.mDismissed = true;
        }
    }

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        this.mShowsDialog = this.mContainerId == 0;
        if (savedInstanceState != null) {
            this.mStyle = savedInstanceState.getInt(SAVED_STYLE, STYLE_NORMAL);
            this.mTheme = savedInstanceState.getInt(SAVED_THEME, STYLE_NORMAL);
            this.mCancelable = savedInstanceState.getBoolean(SAVED_CANCELABLE, true);
            this.mShowsDialog = savedInstanceState.getBoolean(SAVED_SHOWS_DIALOG, this.mShowsDialog);
            this.mBackStackId = savedInstanceState.getInt(SAVED_BACK_STACK_ID, -1);
        }
    }

    public LayoutInflater getLayoutInflater(Bundle savedInstanceState) {
        if (!this.mShowsDialog) {
            return super.getLayoutInflater(savedInstanceState);
        }
        this.mDialog = onCreateDialog(savedInstanceState);
        switch (this.mStyle) {
            case STYLE_NO_TITLE /*1*/:
            case STYLE_NO_FRAME /*2*/:
                break;
            case STYLE_NO_INPUT /*3*/:
                this.mDialog.getWindow().addFlags(24);
                break;
        }
        this.mDialog.requestWindowFeature(STYLE_NO_TITLE);
        if (this.mDialog != null) {
            return (LayoutInflater) this.mDialog.getContext().getSystemService("layout_inflater");
        }
        return (LayoutInflater) this.mActivity.getSystemService("layout_inflater");
    }

    public Dialog onCreateDialog(Bundle savedInstanceState) {
        return new Dialog(getActivity(), getTheme());
    }

    public void onCancel(DialogInterface dialog) {
    }

    public void onDismiss(DialogInterface dialog) {
        if (!this.mViewDestroyed) {
            dismissInternal(true);
        }
    }

    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        if (this.mShowsDialog) {
            View view = getView();
            if (view != null) {
                if (view.getParent() != null) {
                    throw new IllegalStateException("DialogFragment can not be attached to a container view");
                }
                this.mDialog.setContentView(view);
            }
            this.mDialog.setOwnerActivity(getActivity());
            this.mDialog.setCancelable(this.mCancelable);
            this.mDialog.setOnCancelListener(this);
            this.mDialog.setOnDismissListener(this);
            if (savedInstanceState != null) {
                Bundle dialogState = savedInstanceState.getBundle(SAVED_DIALOG_STATE_TAG);
                if (dialogState != null) {
                    this.mDialog.onRestoreInstanceState(dialogState);
                }
            }
        }
    }

    public void onStart() {
        super.onStart();
        if (this.mDialog != null) {
            this.mViewDestroyed = false;
            this.mDialog.show();
        }
    }

    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        if (this.mDialog != null) {
            Bundle dialogState = this.mDialog.onSaveInstanceState();
            if (dialogState != null) {
                outState.putBundle(SAVED_DIALOG_STATE_TAG, dialogState);
            }
        }
        if (this.mStyle != 0) {
            outState.putInt(SAVED_STYLE, this.mStyle);
        }
        if (this.mTheme != 0) {
            outState.putInt(SAVED_THEME, this.mTheme);
        }
        if (!this.mCancelable) {
            outState.putBoolean(SAVED_CANCELABLE, this.mCancelable);
        }
        if (!this.mShowsDialog) {
            outState.putBoolean(SAVED_SHOWS_DIALOG, this.mShowsDialog);
        }
        if (this.mBackStackId != -1) {
            outState.putInt(SAVED_BACK_STACK_ID, this.mBackStackId);
        }
    }

    public void onStop() {
        super.onStop();
        if (this.mDialog != null) {
            this.mDialog.hide();
        }
    }

    public void onDestroyView() {
        super.onDestroyView();
        if (this.mDialog != null) {
            this.mViewDestroyed = true;
            this.mDialog.dismiss();
            this.mDialog = null;
        }
    }
}
