package com.embarcadero.firemonkey.text;

import android.annotation.SuppressLint;
import android.content.ClipData;
import android.content.Context;
import android.os.Build.VERSION;
import android.os.Bundle;
import android.os.Parcel;
import android.os.Parcelable;
import android.os.Parcelable.Creator;
import android.os.ResultReceiver;
import android.support.v4.media.TransportMediator;
import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat;
import android.text.ClipboardManager;
import android.text.Editable;
import android.text.InputFilter;
import android.text.InputFilter.LengthFilter;
import android.text.Selection;
import android.text.SpanWatcher;
import android.text.Spannable;
import android.text.SpannableStringBuilder;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.BaseSavedState;
import android.view.inputmethod.BaseInputConnection;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;
import android.view.inputmethod.InputMethodManager;
import com.google.android.gms.drive.DriveFile;
import com.google.android.vending.licensing.Policy;
import java.util.ArrayList;
import java.util.Iterator;

public class FMXTextEditorProxy extends View {
    public static final int ACTION_DONE = 2;
    public static final int ACTION_ENTER = 0;
    public static final int ACTION_GO = 3;
    public static final int ACTION_NEXT = 1;
    public static final int ACTION_SEARCH = 4;
    public static final int ACTION_SEND = 5;
    public static final int INPUT_ALPHABET = 4;
    public static final int INPUT_EMAIL_ADDRESS = 7;
    public static final int INPUT_NAME_PHONE_PAD = 6;
    public static final int INPUT_NUMBER = 1;
    public static final int INPUT_NUMBER_AND_PUNCTUATION = 2;
    public static final int INPUT_PHONE = 3;
    public static final int INPUT_TEXT = 0;
    public static final int INPUT_URL = 5;
    private static final String TAG = "FMXTextEditorProxy";
    private ChangeWatcher mChangeWatcher;
    private Editable mEditable;
    private int mEnterAction;
    private ProxyInputConnection mInputConnection;
    private int mInputType;
    private ArrayList<FMXTextListener> mListeners;
    private int mMaxLength;
    private boolean mMultiline;
    private OnEnterActionListener mOnEnterActionListener;
    private boolean mPassword;
    private boolean mReadOnly;
    private boolean mSkipRestart;
    private ArrayList<VKStateChangeListener> mVKListeners;

    private class ChangeWatcher implements TextWatcher, SpanWatcher {
        private static final String TAG = "ChangeWatcher";
        private boolean hasComposing;
        private FMXTextEditorProxy mProxyView;

        public ChangeWatcher(FMXTextEditorProxy proxyView) {
            this.hasComposing = false;
            this.mProxyView = proxyView;
        }

        public void beforeTextChanged(CharSequence buffer, int start, int before, int after) {
        }

        public void onTextChanged(CharSequence buffer, int start, int before, int after) {
        }

        public void afterTextChanged(Editable buffer) {
        }

        public void onSpanChanged(Spannable buf, Object what, int s, int e, int st, int en) {
        }

        public void onSpanAdded(Spannable buf, Object what, int s, int e) {
            if ((buf.getSpanFlags(what) & Policy.LICENSED) != 0 && !this.hasComposing) {
                int composingStart = BaseInputConnection.getComposingSpanStart(buf);
                int composingEnd = BaseInputConnection.getComposingSpanEnd(buf);
                if (composingStart >= 0 && composingEnd >= 0) {
                    this.hasComposing = true;
                    if (buf != this.mProxyView.getText()) {
                        this.mProxyView.sendOnTextUpdated(this.mProxyView.packText(buf), Selection.getSelectionEnd(buf));
                    }
                    this.mProxyView.sendOnComposingText(composingStart, composingEnd);
                }
            }
        }

        public void onSpanRemoved(Spannable buf, Object what, int s, int e) {
            if (this.hasComposing) {
                int composingStart = BaseInputConnection.getComposingSpanStart(buf);
                int composingEnd = BaseInputConnection.getComposingSpanEnd(buf);
                if (composingStart < 0 || composingEnd < 0) {
                    this.hasComposing = false;
                    if (buf != this.mProxyView.getText()) {
                        this.mProxyView.sendOnTextUpdated(this.mProxyView.packText(buf), Selection.getSelectionEnd(buf));
                    }
                    this.mProxyView.sendOnComposingText(-1, -1);
                }
            }
        }
    }

    public interface OnEnterActionListener {
        boolean onAction(FMXTextEditorProxy fMXTextEditorProxy, int i, KeyEvent keyEvent);
    }

    private static class ProxyInputConnection extends BaseInputConnection {
        private static final String TAG = "ProxyInputConnection";
        private FMXTextEditorProxy mTargetView;

        public ProxyInputConnection(View targetView) {
            super(targetView, true);
            this.mTargetView = (FMXTextEditorProxy) targetView;
            this.mTargetView.setInputConnection(this);
        }

        public Editable getEditable() {
            return this.mTargetView.getEditable();
        }

        public boolean commitText(CharSequence text, int newCursorPosition) {
            if (this.mTargetView.mReadOnly) {
                return false;
            }
            this.mTargetView.allowRestart();
            boolean superResult = super.commitText(text, newCursorPosition);
            this.mTargetView.checkSpanMerge();
            this.mTargetView.sendOnTextUpdated(this.mTargetView.packText(getEditable()), Selection.getSelectionEnd(getEditable()));
            this.mTargetView.sendOnComposingText(BaseInputConnection.getComposingSpanStart(getEditable()), BaseInputConnection.getComposingSpanEnd(getEditable()));
            return superResult;
        }

        public boolean setComposingText(CharSequence text, int newCursorPosition) {
            int composingStart = BaseInputConnection.getComposingSpanStart(getEditable());
            int composingEnd = BaseInputConnection.getComposingSpanEnd(getEditable());
            boolean hasComposing;
            if (composingStart == composingEnd || (composingStart <= 0 && composingEnd <= 0)) {
                hasComposing = false;
            } else {
                hasComposing = true;
            }
            if (this.mTargetView.mReadOnly || (!hasComposing && text.length() <= 0)) {
                return false;
            }
            this.mTargetView.allowRestart();
            boolean superResult = super.setComposingText(text, newCursorPosition);
            this.mTargetView.checkSpanMerge();
            this.mTargetView.sendOnTextUpdated(this.mTargetView.packText(getEditable()), this.mTargetView.getSelectionEnd());
            this.mTargetView.sendOnComposingText(BaseInputConnection.getComposingSpanStart(getEditable()), BaseInputConnection.getComposingSpanEnd(getEditable()));
            return superResult;
        }

        public boolean deleteSurroundingText(int beforeLength, int afterLength) {
            if (this.mTargetView.mReadOnly) {
                return false;
            }
            boolean superResult = super.deleteSurroundingText(beforeLength, afterLength);
            this.mTargetView.checkSpanMerge();
            this.mTargetView.sendOnTextUpdated(this.mTargetView.packText(getEditable()), Selection.getSelectionEnd(getEditable()));
            return superResult;
        }

        public boolean sendKeyEvent(KeyEvent event) {
            boolean superResult = super.sendKeyEvent(event);
            if (event.getAction() == 0 && (event.getFlags() & 16) != 0) {
                removeComposingSpans(getEditable());
            } else if (event.getKeyCode() == 67 && !this.mTargetView.mReadOnly) {
                boolean deleteSelected;
                int a = this.mTargetView.getSelectionStart();
                int b = this.mTargetView.getSelectionEnd();
                if (a != b || a > 0) {
                    deleteSelected = true;
                } else {
                    deleteSelected = false;
                }
                if (event.getAction() == 0) {
                    getEditable().removeSpan(this.mTargetView.mChangeWatcher);
                    if (deleteSelected) {
                        int charsToDelete = FMXTextEditorProxy.INPUT_TEXT;
                        if (a != b) {
                            charsToDelete = b - a;
                            if (charsToDelete > 0) {
                                Selection.setSelection(getEditable(), a, a);
                                deleteSurroundingText(FMXTextEditorProxy.INPUT_TEXT, charsToDelete);
                            }
                        } else {
                            if (getEditable().length() > 0) {
                                if (Character.isLowSurrogate(getEditable().charAt(a - 1))) {
                                    charsToDelete = FMXTextEditorProxy.INPUT_NUMBER_AND_PUNCTUATION;
                                } else {
                                    charsToDelete = Character.charCount(Character.codePointAt(getEditable(), a - 1));
                                }
                            }
                            if (charsToDelete > 0) {
                                deleteSurroundingText(charsToDelete, FMXTextEditorProxy.INPUT_TEXT);
                            }
                        }
                    } else if (a == 0) {
                        deleteSelected = true;
                        deleteSurroundingText(FMXTextEditorProxy.INPUT_NUMBER, FMXTextEditorProxy.INPUT_TEXT);
                    }
                    this.mTargetView.createChangeWatcher();
                }
                if (!deleteSelected) {
                    return true;
                }
                this.mTargetView.sendSkipKeyEvent(new KeyEvent(event));
                return true;
            } else if (event.getAction() == 0) {
                this.mTargetView.skipRestart();
            }
            return superResult;
        }
    }

    static class SavedState extends BaseSavedState {
        public static final Creator<SavedState> CREATOR;
        int mSelEnd;
        int mSelStart;
        CharSequence mText;

        public SavedState(Parcelable superState, int selStart, int selEnd, CharSequence text) {
            super(superState);
            this.mSelStart = selStart;
            this.mSelEnd = selEnd;
            this.mText = text;
        }

        public void writeToParcel(Parcel out, int flags) {
            super.writeToParcel(out, flags);
            out.writeInt(this.mSelStart);
            out.writeInt(this.mSelEnd);
            TextUtils.writeToParcel(this.mText, out, flags);
        }

        private SavedState(Parcel in) {
            super(in);
            this.mSelStart = in.readInt();
            this.mSelEnd = in.readInt();
            this.mText = (CharSequence) TextUtils.CHAR_SEQUENCE_CREATOR.createFromParcel(in);
        }

        static {
            CREATOR = new Creator<SavedState>() {
                public SavedState createFromParcel(Parcel in) {
                    return new SavedState(null);
                }

                public SavedState[] newArray(int size) {
                    return new SavedState[size];
                }
            };
        }
    }

    private static class SyncResultReceiver extends ResultReceiver {
        private ArrayList<VKStateChangeListener> mListeners;

        public SyncResultReceiver(ArrayList<VKStateChangeListener> listeners) {
            super(null);
            this.mListeners = new ArrayList(listeners);
        }

        public void onReceiveResult(int result, Bundle data) {
            Iterator i$;
            switch (result) {
                case FMXTextEditorProxy.INPUT_TEXT /*0*/:
                case FMXTextEditorProxy.INPUT_NUMBER_AND_PUNCTUATION /*2*/:
                    i$ = this.mListeners.iterator();
                    while (i$.hasNext()) {
                        ((VKStateChangeListener) i$.next()).onVirtualKeyboardShown();
                    }
                case FMXTextEditorProxy.INPUT_NUMBER /*1*/:
                case FMXTextEditorProxy.INPUT_PHONE /*3*/:
                    i$ = this.mListeners.iterator();
                    while (i$.hasNext()) {
                        ((VKStateChangeListener) i$.next()).onVirtualKeyboardHidden();
                    }
                default:
            }
        }
    }

    public FMXTextEditorProxy(Context context) {
        super(context);
        this.mEditable = null;
        this.mInputConnection = null;
        this.mReadOnly = false;
        this.mPassword = false;
        this.mMultiline = true;
        this.mInputType = INPUT_TEXT;
        this.mEnterAction = INPUT_TEXT;
        this.mMaxLength = INPUT_TEXT;
        this.mChangeWatcher = null;
        this.mSkipRestart = false;
        init(context, null, INPUT_TEXT);
    }

    public FMXTextEditorProxy(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.mEditable = null;
        this.mInputConnection = null;
        this.mReadOnly = false;
        this.mPassword = false;
        this.mMultiline = true;
        this.mInputType = INPUT_TEXT;
        this.mEnterAction = INPUT_TEXT;
        this.mMaxLength = INPUT_TEXT;
        this.mChangeWatcher = null;
        this.mSkipRestart = false;
        init(context, attrs, INPUT_TEXT);
    }

    public FMXTextEditorProxy(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        this.mEditable = null;
        this.mInputConnection = null;
        this.mReadOnly = false;
        this.mPassword = false;
        this.mMultiline = true;
        this.mInputType = INPUT_TEXT;
        this.mEnterAction = INPUT_TEXT;
        this.mMaxLength = INPUT_TEXT;
        this.mChangeWatcher = null;
        this.mSkipRestart = false;
        init(context, attrs, defStyle);
    }

    private void init(Context context, AttributeSet attrs, int defStyle) {
        setFocusable(true);
        setFocusableInTouchMode(true);
        setClickable(true);
    }

    public CharSequence getText() {
        return getEditable();
    }

    private CharSequence unpackText(CharSequence text) {
        StringBuilder builder = new StringBuilder(text.length());
        if (text.length() > 0) {
            String textStr = text.toString();
            int headerBegin = textStr.indexOf("[");
            int headerEnd = textStr.indexOf("]");
            if (headerBegin >= 0 && headerEnd > 0 && (headerEnd - headerBegin) - 1 > 0) {
                String lineBreak = System.getProperty("line.separator");
                String[] header = textStr.substring(headerBegin + INPUT_NUMBER, headerEnd).split(",");
                int lengthBefore = INPUT_TEXT;
                for (int i = INPUT_TEXT; i < header.length; i += INPUT_NUMBER) {
                    if (i > 0) {
                        builder.append(lineBreak);
                    }
                    int lineLength = Integer.parseInt(header[i]);
                    if (lineLength > 0) {
                        int indexShift = (headerEnd + INPUT_NUMBER) + lengthBefore;
                        builder.append(textStr.substring(indexShift, indexShift + lineLength));
                    }
                    lengthBefore += lineLength;
                }
            }
        }
        return builder.toString();
    }

    public CharSequence packText(Spannable editable) {
        String result = "";
        TextLineSpan[] spans = (TextLineSpan[]) editable.getSpans(INPUT_TEXT, editable.length(), TextLineSpan.class);
        StringBuilder header = new StringBuilder(spans.length * INPUT_ALPHABET);
        StringBuilder body = new StringBuilder(editable.length());
        TextLineSpan[] arr$ = spans;
        int len$ = arr$.length;
        for (int i$ = INPUT_TEXT; i$ < len$; i$ += INPUT_NUMBER) {
            TextLineSpan span = arr$[i$];
            if (header.length() > 0) {
                header.append(',');
            }
            String line = editable.subSequence(editable.getSpanStart(span), editable.getSpanEnd(span)).toString();
            header.append(line.length());
            body.append(line);
        }
        if (header.length() > 0) {
            result = "[" + header.toString() + "]" + body.toString();
        }
        return result.subSequence(INPUT_TEXT, result.length());
    }

    private void setLineSpans(CharSequence text) {
        TextLineSpan[] arr$ = (TextLineSpan[]) this.mEditable.getSpans(INPUT_TEXT, this.mEditable.length(), TextLineSpan.class);
        int len$ = arr$.length;
        for (int i$ = INPUT_TEXT; i$ < len$; i$ += INPUT_NUMBER) {
            TextLineSpan span = arr$[i$];
            this.mEditable.removeSpan(span);
        }
        if (text.length() > 0) {
            String textStr = text.toString();
            int headerBegin = textStr.indexOf("[");
            int headerEnd = textStr.indexOf("]");
            if (headerBegin >= 0 && headerEnd > 0) {
                int lineBreakLength = System.getProperty("line.separator").length();
                String[] header = textStr.substring(headerBegin + INPUT_NUMBER, headerEnd).split(",");
                int lineNumber = INPUT_TEXT;
                int spanStart = INPUT_TEXT;
                int i = INPUT_TEXT;
                while (true) {
                    int length = header.length;
                    if (i < r0) {
                        int lineLength = Integer.parseInt(header[i]);
                        int lineNumber2 = lineNumber + INPUT_NUMBER;
                        this.mEditable.setSpan(new TextLineSpan(lineNumber), spanStart, spanStart + lineLength, 18);
                        spanStart += lineLength + lineBreakLength;
                        i += INPUT_NUMBER;
                        lineNumber = lineNumber2;
                    } else {
                        return;
                    }
                }
            }
            return;
        }
        this.mEditable.setSpan(new TextLineSpan(INPUT_TEXT), INPUT_TEXT, INPUT_TEXT, 18);
    }

    public void setText(CharSequence text) {
        CharSequence unpackedText = unpackText(text);
        InputMethodManager imm;
        if (this.mEditable == null) {
            createEditable(text);
            imm = getInputMethodManager();
            if (imm != null) {
                imm.restartInput(this);
                return;
            }
            return;
        }
        int selEnd = getSelectionEnd();
        if (this.mChangeWatcher != null) {
            this.mEditable.removeSpan(this.mChangeWatcher);
        }
        int composingStart = BaseInputConnection.getComposingSpanStart(this.mEditable);
        int composingEnd = BaseInputConnection.getComposingSpanEnd(this.mEditable);
        if (composingStart != composingEnd && (composingStart > 0 || composingEnd > 0)) {
            imm = getInputMethodManager();
            if (imm != null) {
                imm.restartInput(this);
            }
        }
        this.mEditable.replace(INPUT_TEXT, this.mEditable.length(), unpackedText);
        setLineSpans(text);
        Selection.setSelection(this.mEditable, Math.min(selEnd, this.mEditable.length()));
        createChangeWatcher();
    }

    public void setInputType(int inputType) {
        if (inputType != this.mInputType) {
            this.mInputType = inputType;
            InputMethodManager imm = getInputMethodManager();
            if (imm != null) {
                imm.restartInput(this);
            }
        }
    }

    public void setEnterAction(int enterAction) {
        if (this.mEnterAction != enterAction) {
            this.mEnterAction = enterAction;
            setText(getText());
        }
    }

    public void setMaxLength(int value) {
        if (value != this.mMaxLength) {
            this.mMaxLength = value;
            if (this.mEditable == null) {
                return;
            }
            if (this.mMaxLength > 0) {
                InputFilter[] FilterArray = new InputFilter[INPUT_NUMBER];
                FilterArray[INPUT_TEXT] = new LengthFilter(this.mMaxLength);
                this.mEditable.setFilters(FilterArray);
                return;
            }
            this.mEditable.setFilters(new InputFilter[INPUT_TEXT]);
        }
    }

    public void showSoftInput(boolean show) {
        SyncResultReceiver receiver = new SyncResultReceiver(this.mVKListeners);
        InputMethodManager imm = getInputMethodManager();
        if (show) {
            imm.showSoftInput(this, INPUT_TEXT, receiver);
        } else {
            imm.hideSoftInputFromWindow(getWindowToken(), INPUT_TEXT, receiver);
        }
    }

    public void addTextListener(FMXTextListener textListener) {
        if (this.mListeners == null) {
            this.mListeners = new ArrayList();
        }
        if (!this.mListeners.contains(textListener)) {
            this.mListeners.add(textListener);
        }
    }

    public void removeTextListener(FMXTextListener textListener) {
        if (this.mListeners != null && this.mListeners.contains(textListener)) {
            this.mListeners.remove(textListener);
        }
    }

    public void setOnEditorActionListener(OnEnterActionListener actionListener) {
        this.mOnEnterActionListener = actionListener;
    }

    public void addOnVKStateChangeListener(VKStateChangeListener vkListener) {
        if (this.mVKListeners == null) {
            this.mVKListeners = new ArrayList();
        }
        this.mVKListeners.add(vkListener);
    }

    public void removeOnVKStateChangeListener(VKStateChangeListener vkListener) {
        if (this.mVKListeners != null) {
            this.mVKListeners.remove(vkListener);
        }
    }

    public void setReadOnly(boolean readonly) {
        if (this.mReadOnly != readonly) {
            this.mReadOnly = readonly;
            InputMethodManager imm = getInputMethodManager();
            if (imm != null) {
                imm.restartInput(this);
            }
        }
    }

    public void setIsPassword(boolean isPassword) {
        if (this.mPassword != isPassword) {
            this.mPassword = isPassword;
            InputMethodManager imm = getInputMethodManager();
            if (imm != null) {
                imm.restartInput(this);
            }
        }
    }

    public void setMultiline(boolean multiline) {
        this.mMultiline = multiline;
    }

    public boolean onTouchEvent(MotionEvent event) {
        boolean touchIsFinished = true;
        int action = event.getActionMasked();
        boolean superResult = super.onTouchEvent(event);
        if (!(action == INPUT_NUMBER && isFocused())) {
            touchIsFinished = false;
        }
        if (touchIsFinished) {
            viewClicked();
        }
        return superResult;
    }

    public boolean onCheckIsTextEditor() {
        return true;
    }

    @SuppressLint({"InlinedApi"})
    public InputConnection onCreateInputConnection(EditorInfo outAttrs) {
        if (VERSION.SDK_INT >= 11) {
            outAttrs.imeOptions = 838860800;
        } else {
            outAttrs.imeOptions = DriveFile.MODE_READ_WRITE;
        }
        switch (this.mInputType) {
            case INPUT_NUMBER /*1*/:
                outAttrs.inputType = INPUT_NUMBER_AND_PUNCTUATION;
                break;
            case INPUT_NUMBER_AND_PUNCTUATION /*2*/:
                outAttrs.inputType = INPUT_NUMBER;
                break;
            case INPUT_PHONE /*3*/:
                outAttrs.inputType = INPUT_PHONE;
                break;
            case INPUT_ALPHABET /*4*/:
                outAttrs.inputType = 524289;
                if (!this.mPassword) {
                    outAttrs.inputType |= AccessibilityNodeInfoCompat.ACTION_COPY;
                    break;
                }
                break;
            case INPUT_URL /*5*/:
                outAttrs.inputType = 17;
                break;
            case INPUT_NAME_PHONE_PAD /*6*/:
                outAttrs.inputType = INPUT_PHONE;
                if (!this.mPassword) {
                    outAttrs.inputType |= AccessibilityNodeInfoCompat.ACTION_COPY;
                    break;
                }
                break;
            case INPUT_EMAIL_ADDRESS /*7*/:
                outAttrs.inputType = 33;
                break;
            default:
                outAttrs.inputType = INPUT_NUMBER;
                if (!this.mPassword) {
                    outAttrs.inputType |= AccessibilityNodeInfoCompat.ACTION_COPY;
                    break;
                }
                break;
        }
        if (this.mPassword) {
            if ((outAttrs.inputType & INPUT_NUMBER_AND_PUNCTUATION) == INPUT_NUMBER_AND_PUNCTUATION) {
                outAttrs.inputType |= 16;
            } else {
                outAttrs.inputType |= TransportMediator.FLAG_KEY_MEDIA_NEXT;
            }
        }
        switch (this.mEnterAction) {
            case INPUT_NUMBER /*1*/:
                outAttrs.imeOptions |= INPUT_URL;
                break;
            case INPUT_NUMBER_AND_PUNCTUATION /*2*/:
                outAttrs.imeOptions |= INPUT_NAME_PHONE_PAD;
                break;
            case INPUT_PHONE /*3*/:
                outAttrs.imeOptions |= INPUT_NUMBER_AND_PUNCTUATION;
                break;
            case INPUT_ALPHABET /*4*/:
                outAttrs.imeOptions |= INPUT_PHONE;
                break;
            case INPUT_URL /*5*/:
                outAttrs.imeOptions |= INPUT_ALPHABET;
                break;
        }
        outAttrs.imeOptions |= AccessibilityNodeInfoCompat.ACTION_SET_SELECTION;
        return new ProxyInputConnection(this);
    }

    private void createEditable(CharSequence text) {
        this.mEditable = new SpannableStringBuilder();
        this.mEditable.append(unpackText(text));
        setLineSpans(text);
        Selection.setSelection(this.mEditable, this.mEditable.length());
        createChangeWatcher();
    }

    private void createChangeWatcher() {
        if (this.mChangeWatcher == null) {
            this.mChangeWatcher = new ChangeWatcher(this);
        }
        this.mEditable.setSpan(this.mChangeWatcher, INPUT_TEXT, getEditable().length(), 18);
    }

    public Editable getEditable() {
        if (this.mEditable == null) {
            createEditable("");
        }
        return this.mEditable;
    }

    public void sendOnComposingText(int begin, int end) {
        if (this.mListeners != null) {
            Iterator i$ = this.mListeners.iterator();
            while (i$.hasNext()) {
                FMXTextListener L = (FMXTextListener) i$.next();
                if (L != null) {
                    L.onComposingText(begin, end);
                }
            }
        }
    }

    public void sendOnTextUpdated(CharSequence text, int position) {
        if (this.mListeners != null) {
            Iterator i$ = this.mListeners.iterator();
            while (i$.hasNext()) {
                FMXTextListener L = (FMXTextListener) i$.next();
                if (L != null) {
                    L.onTextUpdated(text, position);
                }
            }
        }
    }

    public void sendSkipKeyEvent(KeyEvent event) {
        if (this.mListeners != null) {
            Iterator i$ = this.mListeners.iterator();
            while (i$.hasNext()) {
                ((FMXTextListener) i$.next()).onSkipKeyEvent(event);
            }
        }
    }

    public int getCursorPosition() {
        int end = Selection.getSelectionEnd(getEditable());
        if (end > getEditable().length()) {
            end = getEditable().length();
        }
        if (end < 0) {
            return INPUT_TEXT;
        }
        return end;
    }

    public int getSelectionEnd() {
        return Selection.getSelectionEnd(getEditable());
    }

    public int getSelectionStart() {
        return Selection.getSelectionStart(getEditable());
    }

    public void setSelection(int start, int end) {
        if (start >= 0 && end >= 0 && start <= getEditable().length() && end <= getEditable().length()) {
            int s = Math.min(start, end);
            int e = Math.max(start, end);
            Selection.setSelection(getEditable(), s, e);
            if (this.mInputConnection != null) {
                this.mInputConnection.setSelection(s, e);
            }
        }
    }

    public void setCursorPosition(int position) {
        if (position >= 0 && position <= getEditable().length()) {
            boolean hasComposing;
            int composingStart = BaseInputConnection.getComposingSpanStart(this.mEditable);
            int composingEnd = BaseInputConnection.getComposingSpanEnd(this.mEditable);
            if (composingStart == composingEnd || (composingStart <= 0 && composingEnd <= 0)) {
                hasComposing = false;
            } else {
                hasComposing = true;
            }
            boolean selectionChanged;
            if (position == getSelectionStart() || position == getSelectionEnd()) {
                selectionChanged = false;
            } else {
                selectionChanged = true;
            }
            if ((hasComposing || selectionChanged) && !this.mSkipRestart) {
                getInputMethodManager().restartInput(this);
            }
            this.mSkipRestart = false;
            setSelection(position, position);
        }
    }

    public void skipRestart() {
        this.mSkipRestart = true;
    }

    public void allowRestart() {
        this.mSkipRestart = false;
    }

    @SuppressLint({"NewApi"})
    private void copyTextToClipboard(CharSequence text) {
        ClipboardManager service = getContext().getSystemService("clipboard");
        if (service == null) {
            return;
        }
        if ("android.content.ClipboardManager".equals(service.getClass().getName())) {
            android.content.ClipboardManager newManager = (android.content.ClipboardManager) service;
            ClipData data = ClipData.newPlainText("text", text);
            if (data != null) {
                newManager.setPrimaryClip(data);
            }
        } else if ("android.text.ClipboardManager".equals(service.getClass().getName())) {
            service.setText(text);
        }
    }

    public void copySelectedText() {
        int a = getSelectionStart();
        int b = getSelectionEnd();
        if (a != b) {
            copyTextToClipboard(getEditable().subSequence(a, b));
            setCursorPosition(b);
            getInputMethodManager().restartInput(this);
        }
    }

    public void cutSelectedText() {
        int a = getSelectionStart();
        int b = getSelectionEnd();
        if (a != b) {
            copyTextToClipboard(getEditable().subSequence(a, b));
            if (!this.mReadOnly) {
                this.mEditable.removeSpan(this.mChangeWatcher);
                getEditable().delete(a, b);
                checkSpanMerge();
                setCursorPosition(a);
                sendOnTextUpdated(packText(this.mEditable), getSelectionEnd());
                createChangeWatcher();
                getInputMethodManager().restartInput(this);
            }
        }
    }

    @SuppressLint({"NewApi"})
    public void pasteText() {
        if (!this.mReadOnly) {
            CharSequence text = null;
            ClipboardManager service = getContext().getSystemService("clipboard");
            if (service != null) {
                if ("android.content.ClipboardManager".equals(service.getClass().getName())) {
                    text = ((android.content.ClipboardManager) service).getPrimaryClip().getItemAt(INPUT_TEXT).getText();
                } else if ("android.text.ClipboardManager".equals(service.getClass().getName())) {
                    text = service.getText();
                }
            }
            if (text != null && text.length() > 0) {
                String textStr;
                TextLineSpan[] spans;
                int i;
                int length;
                String lineBreak = System.getProperty("line.separator");
                if (!this.mMultiline) {
                    textStr = text.toString();
                    if (textStr.contains(lineBreak)) {
                        text = textStr.split(lineBreak)[INPUT_TEXT];
                    }
                }
                BaseInputConnection.removeComposingSpans(getEditable());
                this.mEditable.removeSpan(this.mChangeWatcher);
                int selStart = getSelectionStart();
                int selEnd = getSelectionEnd();
                int newCursorPosition = selStart + text.length();
                if (selStart != selEnd) {
                    getEditable().delete(selStart, selEnd);
                    checkSpanMerge();
                }
                TextLineSpan line = null;
                textStr = text.toString();
                if (textStr.contains(lineBreak)) {
                    spans = (TextLineSpan[]) this.mEditable.getSpans(INPUT_TEXT, this.mEditable.length(), TextLineSpan.class);
                    i = INPUT_TEXT;
                    while (true) {
                        length = spans.length;
                        if (i >= r0) {
                            break;
                        }
                        int spanStart = this.mEditable.getSpanStart(spans[i]);
                        int spanEnd = this.mEditable.getSpanEnd(spans[i]);
                        if (selStart >= spanStart && selStart <= spanEnd) {
                            break;
                        }
                        i += INPUT_NUMBER;
                    }
                    line = spans[i];
                }
                getEditable().insert(selStart, text);
                if (line != null && textStr.contains(lineBreak)) {
                    int lineStart = this.mEditable.getSpanStart(line);
                    int lineEnd = this.mEditable.getSpanEnd(line);
                    spans = (TextLineSpan[]) this.mEditable.getSpans(lineEnd + INPUT_NUMBER, this.mEditable.length(), TextLineSpan.class);
                    int breakIndex = textStr.indexOf(lineBreak);
                    this.mEditable.setSpan(line, lineStart, selStart + breakIndex, 18);
                    selStart += lineBreak.length() + breakIndex;
                    int textStrShift = breakIndex + lineBreak.length();
                    while (textStr.indexOf(lineBreak, textStrShift) >= 0) {
                        breakIndex = textStr.indexOf(lineBreak, textStrShift);
                        if (breakIndex >= 0) {
                            this.mEditable.setSpan(new TextLineSpan(INPUT_TEXT), selStart, (selStart + breakIndex) - textStrShift, 18);
                            selStart += (breakIndex - textStrShift) + lineBreak.length();
                            textStrShift = breakIndex + lineBreak.length();
                        }
                    }
                    if (lineEnd - selStart > 0) {
                        this.mEditable.setSpan(new TextLineSpan(INPUT_TEXT), selStart, lineEnd, 18);
                    }
                    i = INPUT_TEXT;
                    while (true) {
                        length = spans.length;
                        if (i >= r0) {
                            break;
                        }
                        lineStart = this.mEditable.getSpanStart(spans[i]);
                        lineEnd = this.mEditable.getSpanEnd(spans[i]);
                        this.mEditable.removeSpan(spans[i]);
                        this.mEditable.setSpan(new TextLineSpan(INPUT_TEXT), lineStart, lineEnd, 18);
                        i += INPUT_NUMBER;
                    }
                    spans = (TextLineSpan[]) this.mEditable.getSpans(INPUT_TEXT, this.mEditable.length(), TextLineSpan.class);
                    i = INPUT_TEXT;
                    while (true) {
                        length = spans.length;
                        if (i >= r0) {
                            break;
                        }
                        spans[i].setLineNumber(i);
                        i += INPUT_NUMBER;
                    }
                }
                setCursorPosition(newCursorPosition);
                sendOnTextUpdated(packText(this.mEditable), getSelectionEnd());
                createChangeWatcher();
                getInputMethodManager().restartInput(this);
            }
        }
    }

    public void checkSpanMerge() {
        TextLineSpan[] spans = (TextLineSpan[]) getEditable().getSpans(INPUT_TEXT, getEditable().length(), TextLineSpan.class);
        if (spans.length > INPUT_NUMBER) {
            TextLineSpan prevLine = spans[INPUT_TEXT];
            int prevLineStart = getEditable().getSpanStart(prevLine);
            int prevLineEnd = getEditable().getSpanEnd(prevLine);
            int lineNumber = INPUT_NUMBER;
            int i = INPUT_NUMBER;
            while (i < spans.length) {
                TextLineSpan line = spans[i];
                int lineNumber2 = lineNumber + INPUT_NUMBER;
                line.setLineNumber(lineNumber);
                int lineStart = getEditable().getSpanStart(line);
                int lineEnd = getEditable().getSpanEnd(line);
                if (prevLineEnd >= lineStart) {
                    getEditable().removeSpan(line);
                    prevLineEnd = Math.max(lineEnd, prevLineEnd);
                    getEditable().setSpan(prevLine, prevLineStart, prevLineEnd, 18);
                } else {
                    prevLine = line;
                    prevLineStart = getEditable().getSpanStart(prevLine);
                    prevLineEnd = getEditable().getSpanEnd(prevLine);
                }
                i += INPUT_NUMBER;
                lineNumber = lineNumber2;
            }
        }
    }

    public void setInputConnection(ProxyInputConnection connection) {
        this.mInputConnection = connection;
    }

    public boolean onKeyDown(int keyCode, KeyEvent event) {
        boolean superResult = super.onKeyDown(keyCode, event);
        if (!((event.getFlags() & 16) == 0 || this.mOnEnterActionListener == null)) {
            this.mOnEnterActionListener.onAction(this, INPUT_TEXT, event);
        }
        return superResult;
    }

    public Parcelable onSaveInstanceState() {
        return new SavedState(super.onSaveInstanceState(), getSelectionStart(), getSelectionEnd(), getText());
    }

    public void onRestoreInstanceState(Parcelable state) {
        if (state instanceof SavedState) {
            SavedState ss = (SavedState) state;
            super.onRestoreInstanceState(ss.getSuperState());
            setText(ss.mText);
            setSelection(ss.mSelStart, ss.mSelEnd);
            return;
        }
        super.onRestoreInstanceState(state);
    }

    protected InputMethodManager getInputMethodManager() {
        return (InputMethodManager) getContext().getSystemService("input_method");
    }

    private void viewClicked() {
        showSoftInput(true);
    }
}
