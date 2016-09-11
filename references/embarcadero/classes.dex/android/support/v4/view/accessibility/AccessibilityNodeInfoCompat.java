package android.support.v4.view.accessibility;

import android.graphics.Rect;
import android.os.Build.VERSION;
import android.os.Bundle;
import android.view.View;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class AccessibilityNodeInfoCompat {
    public static final int ACTION_ACCESSIBILITY_FOCUS = 64;
    public static final String ACTION_ARGUMENT_EXTEND_SELECTION_BOOLEAN = "ACTION_ARGUMENT_EXTEND_SELECTION_BOOLEAN";
    public static final String ACTION_ARGUMENT_HTML_ELEMENT_STRING = "ACTION_ARGUMENT_HTML_ELEMENT_STRING";
    public static final String ACTION_ARGUMENT_MOVEMENT_GRANULARITY_INT = "ACTION_ARGUMENT_MOVEMENT_GRANULARITY_INT";
    public static final String ACTION_ARGUMENT_SELECTION_END_INT = "ACTION_ARGUMENT_SELECTION_END_INT";
    public static final String ACTION_ARGUMENT_SELECTION_START_INT = "ACTION_ARGUMENT_SELECTION_START_INT";
    public static final int ACTION_CLEAR_ACCESSIBILITY_FOCUS = 128;
    public static final int ACTION_CLEAR_FOCUS = 2;
    public static final int ACTION_CLEAR_SELECTION = 8;
    public static final int ACTION_CLICK = 16;
    public static final int ACTION_COPY = 16384;
    public static final int ACTION_CUT = 65536;
    public static final int ACTION_FOCUS = 1;
    public static final int ACTION_LONG_CLICK = 32;
    public static final int ACTION_NEXT_AT_MOVEMENT_GRANULARITY = 256;
    public static final int ACTION_NEXT_HTML_ELEMENT = 1024;
    public static final int ACTION_PASTE = 32768;
    public static final int ACTION_PREVIOUS_AT_MOVEMENT_GRANULARITY = 512;
    public static final int ACTION_PREVIOUS_HTML_ELEMENT = 2048;
    public static final int ACTION_SCROLL_BACKWARD = 8192;
    public static final int ACTION_SCROLL_FORWARD = 4096;
    public static final int ACTION_SELECT = 4;
    public static final int ACTION_SET_SELECTION = 131072;
    public static final int FOCUS_ACCESSIBILITY = 2;
    public static final int FOCUS_INPUT = 1;
    private static final AccessibilityNodeInfoImpl IMPL;
    public static final int MOVEMENT_GRANULARITY_CHARACTER = 1;
    public static final int MOVEMENT_GRANULARITY_LINE = 4;
    public static final int MOVEMENT_GRANULARITY_PAGE = 16;
    public static final int MOVEMENT_GRANULARITY_PARAGRAPH = 8;
    public static final int MOVEMENT_GRANULARITY_WORD = 2;
    private final Object mInfo;

    interface AccessibilityNodeInfoImpl {
        void addAction(Object obj, int i);

        void addChild(Object obj, View view);

        void addChild(Object obj, View view, int i);

        List<Object> findAccessibilityNodeInfosByText(Object obj, String str);

        Object findFocus(Object obj, int i);

        Object focusSearch(Object obj, int i);

        int getActions(Object obj);

        void getBoundsInParent(Object obj, Rect rect);

        void getBoundsInScreen(Object obj, Rect rect);

        Object getChild(Object obj, int i);

        int getChildCount(Object obj);

        CharSequence getClassName(Object obj);

        CharSequence getContentDescription(Object obj);

        int getLiveRegion(Object obj);

        int getMovementGranularities(Object obj);

        CharSequence getPackageName(Object obj);

        Object getParent(Object obj);

        CharSequence getText(Object obj);

        String getViewIdResourceName(Object obj);

        int getWindowId(Object obj);

        boolean isAccessibilityFocused(Object obj);

        boolean isCheckable(Object obj);

        boolean isChecked(Object obj);

        boolean isClickable(Object obj);

        boolean isEnabled(Object obj);

        boolean isFocusable(Object obj);

        boolean isFocused(Object obj);

        boolean isLongClickable(Object obj);

        boolean isPassword(Object obj);

        boolean isScrollable(Object obj);

        boolean isSelected(Object obj);

        boolean isVisibleToUser(Object obj);

        Object obtain();

        Object obtain(View view);

        Object obtain(View view, int i);

        Object obtain(Object obj);

        boolean performAction(Object obj, int i);

        boolean performAction(Object obj, int i, Bundle bundle);

        void recycle(Object obj);

        void setAccessibilityFocused(Object obj, boolean z);

        void setBoundsInParent(Object obj, Rect rect);

        void setBoundsInScreen(Object obj, Rect rect);

        void setCheckable(Object obj, boolean z);

        void setChecked(Object obj, boolean z);

        void setClassName(Object obj, CharSequence charSequence);

        void setClickable(Object obj, boolean z);

        void setContentDescription(Object obj, CharSequence charSequence);

        void setEnabled(Object obj, boolean z);

        void setFocusable(Object obj, boolean z);

        void setFocused(Object obj, boolean z);

        void setLiveRegion(Object obj, int i);

        void setLongClickable(Object obj, boolean z);

        void setMovementGranularities(Object obj, int i);

        void setPackageName(Object obj, CharSequence charSequence);

        void setParent(Object obj, View view);

        void setParent(Object obj, View view, int i);

        void setPassword(Object obj, boolean z);

        void setScrollable(Object obj, boolean z);

        void setSelected(Object obj, boolean z);

        void setSource(Object obj, View view);

        void setSource(Object obj, View view, int i);

        void setText(Object obj, CharSequence charSequence);

        void setViewIdResourceName(Object obj, String str);

        void setVisibleToUser(Object obj, boolean z);
    }

    static class AccessibilityNodeInfoStubImpl implements AccessibilityNodeInfoImpl {
        AccessibilityNodeInfoStubImpl() {
        }

        public Object obtain() {
            return null;
        }

        public Object obtain(View source) {
            return null;
        }

        public Object obtain(View root, int virtualDescendantId) {
            return null;
        }

        public Object obtain(Object info) {
            return null;
        }

        public void addAction(Object info, int action) {
        }

        public void addChild(Object info, View child) {
        }

        public void addChild(Object info, View child, int virtualDescendantId) {
        }

        public List<Object> findAccessibilityNodeInfosByText(Object info, String text) {
            return Collections.emptyList();
        }

        public int getActions(Object info) {
            return 0;
        }

        public void getBoundsInParent(Object info, Rect outBounds) {
        }

        public void getBoundsInScreen(Object info, Rect outBounds) {
        }

        public Object getChild(Object info, int index) {
            return null;
        }

        public int getChildCount(Object info) {
            return 0;
        }

        public CharSequence getClassName(Object info) {
            return null;
        }

        public CharSequence getContentDescription(Object info) {
            return null;
        }

        public CharSequence getPackageName(Object info) {
            return null;
        }

        public Object getParent(Object info) {
            return null;
        }

        public CharSequence getText(Object info) {
            return null;
        }

        public int getWindowId(Object info) {
            return 0;
        }

        public boolean isCheckable(Object info) {
            return false;
        }

        public boolean isChecked(Object info) {
            return false;
        }

        public boolean isClickable(Object info) {
            return false;
        }

        public boolean isEnabled(Object info) {
            return false;
        }

        public boolean isFocusable(Object info) {
            return false;
        }

        public boolean isFocused(Object info) {
            return false;
        }

        public boolean isVisibleToUser(Object info) {
            return false;
        }

        public boolean isAccessibilityFocused(Object info) {
            return false;
        }

        public boolean isLongClickable(Object info) {
            return false;
        }

        public boolean isPassword(Object info) {
            return false;
        }

        public boolean isScrollable(Object info) {
            return false;
        }

        public boolean isSelected(Object info) {
            return false;
        }

        public boolean performAction(Object info, int action) {
            return false;
        }

        public boolean performAction(Object info, int action, Bundle arguments) {
            return false;
        }

        public void setMovementGranularities(Object info, int granularities) {
        }

        public int getMovementGranularities(Object info) {
            return 0;
        }

        public void setBoundsInParent(Object info, Rect bounds) {
        }

        public void setBoundsInScreen(Object info, Rect bounds) {
        }

        public void setCheckable(Object info, boolean checkable) {
        }

        public void setChecked(Object info, boolean checked) {
        }

        public void setClassName(Object info, CharSequence className) {
        }

        public void setClickable(Object info, boolean clickable) {
        }

        public void setContentDescription(Object info, CharSequence contentDescription) {
        }

        public void setEnabled(Object info, boolean enabled) {
        }

        public void setFocusable(Object info, boolean focusable) {
        }

        public void setFocused(Object info, boolean focused) {
        }

        public void setVisibleToUser(Object info, boolean visibleToUser) {
        }

        public void setAccessibilityFocused(Object info, boolean focused) {
        }

        public void setLongClickable(Object info, boolean longClickable) {
        }

        public void setPackageName(Object info, CharSequence packageName) {
        }

        public void setParent(Object info, View parent) {
        }

        public void setPassword(Object info, boolean password) {
        }

        public void setScrollable(Object info, boolean scrollable) {
        }

        public void setSelected(Object info, boolean selected) {
        }

        public void setSource(Object info, View source) {
        }

        public void setSource(Object info, View root, int virtualDescendantId) {
        }

        public Object findFocus(Object info, int focus) {
            return null;
        }

        public Object focusSearch(Object info, int direction) {
            return null;
        }

        public void setText(Object info, CharSequence text) {
        }

        public void recycle(Object info) {
        }

        public void setParent(Object info, View root, int virtualDescendantId) {
        }

        public String getViewIdResourceName(Object info) {
            return null;
        }

        public void setViewIdResourceName(Object info, String viewId) {
        }

        public int getLiveRegion(Object info) {
            return 0;
        }

        public void setLiveRegion(Object info, int mode) {
        }
    }

    static class AccessibilityNodeInfoIcsImpl extends AccessibilityNodeInfoStubImpl {
        AccessibilityNodeInfoIcsImpl() {
        }

        public Object obtain() {
            return AccessibilityNodeInfoCompatIcs.obtain();
        }

        public Object obtain(View source) {
            return AccessibilityNodeInfoCompatIcs.obtain(source);
        }

        public Object obtain(Object info) {
            return AccessibilityNodeInfoCompatIcs.obtain(info);
        }

        public void addAction(Object info, int action) {
            AccessibilityNodeInfoCompatIcs.addAction(info, action);
        }

        public void addChild(Object info, View child) {
            AccessibilityNodeInfoCompatIcs.addChild(info, child);
        }

        public List<Object> findAccessibilityNodeInfosByText(Object info, String text) {
            return AccessibilityNodeInfoCompatIcs.findAccessibilityNodeInfosByText(info, text);
        }

        public int getActions(Object info) {
            return AccessibilityNodeInfoCompatIcs.getActions(info);
        }

        public void getBoundsInParent(Object info, Rect outBounds) {
            AccessibilityNodeInfoCompatIcs.getBoundsInParent(info, outBounds);
        }

        public void getBoundsInScreen(Object info, Rect outBounds) {
            AccessibilityNodeInfoCompatIcs.getBoundsInScreen(info, outBounds);
        }

        public Object getChild(Object info, int index) {
            return AccessibilityNodeInfoCompatIcs.getChild(info, index);
        }

        public int getChildCount(Object info) {
            return AccessibilityNodeInfoCompatIcs.getChildCount(info);
        }

        public CharSequence getClassName(Object info) {
            return AccessibilityNodeInfoCompatIcs.getClassName(info);
        }

        public CharSequence getContentDescription(Object info) {
            return AccessibilityNodeInfoCompatIcs.getContentDescription(info);
        }

        public CharSequence getPackageName(Object info) {
            return AccessibilityNodeInfoCompatIcs.getPackageName(info);
        }

        public Object getParent(Object info) {
            return AccessibilityNodeInfoCompatIcs.getParent(info);
        }

        public CharSequence getText(Object info) {
            return AccessibilityNodeInfoCompatIcs.getText(info);
        }

        public int getWindowId(Object info) {
            return AccessibilityNodeInfoCompatIcs.getWindowId(info);
        }

        public boolean isCheckable(Object info) {
            return AccessibilityNodeInfoCompatIcs.isCheckable(info);
        }

        public boolean isChecked(Object info) {
            return AccessibilityNodeInfoCompatIcs.isChecked(info);
        }

        public boolean isClickable(Object info) {
            return AccessibilityNodeInfoCompatIcs.isClickable(info);
        }

        public boolean isEnabled(Object info) {
            return AccessibilityNodeInfoCompatIcs.isEnabled(info);
        }

        public boolean isFocusable(Object info) {
            return AccessibilityNodeInfoCompatIcs.isFocusable(info);
        }

        public boolean isFocused(Object info) {
            return AccessibilityNodeInfoCompatIcs.isFocused(info);
        }

        public boolean isLongClickable(Object info) {
            return AccessibilityNodeInfoCompatIcs.isLongClickable(info);
        }

        public boolean isPassword(Object info) {
            return AccessibilityNodeInfoCompatIcs.isPassword(info);
        }

        public boolean isScrollable(Object info) {
            return AccessibilityNodeInfoCompatIcs.isScrollable(info);
        }

        public boolean isSelected(Object info) {
            return AccessibilityNodeInfoCompatIcs.isSelected(info);
        }

        public boolean performAction(Object info, int action) {
            return AccessibilityNodeInfoCompatIcs.performAction(info, action);
        }

        public void setBoundsInParent(Object info, Rect bounds) {
            AccessibilityNodeInfoCompatIcs.setBoundsInParent(info, bounds);
        }

        public void setBoundsInScreen(Object info, Rect bounds) {
            AccessibilityNodeInfoCompatIcs.setBoundsInScreen(info, bounds);
        }

        public void setCheckable(Object info, boolean checkable) {
            AccessibilityNodeInfoCompatIcs.setCheckable(info, checkable);
        }

        public void setChecked(Object info, boolean checked) {
            AccessibilityNodeInfoCompatIcs.setChecked(info, checked);
        }

        public void setClassName(Object info, CharSequence className) {
            AccessibilityNodeInfoCompatIcs.setClassName(info, className);
        }

        public void setClickable(Object info, boolean clickable) {
            AccessibilityNodeInfoCompatIcs.setClickable(info, clickable);
        }

        public void setContentDescription(Object info, CharSequence contentDescription) {
            AccessibilityNodeInfoCompatIcs.setContentDescription(info, contentDescription);
        }

        public void setEnabled(Object info, boolean enabled) {
            AccessibilityNodeInfoCompatIcs.setEnabled(info, enabled);
        }

        public void setFocusable(Object info, boolean focusable) {
            AccessibilityNodeInfoCompatIcs.setFocusable(info, focusable);
        }

        public void setFocused(Object info, boolean focused) {
            AccessibilityNodeInfoCompatIcs.setFocused(info, focused);
        }

        public void setLongClickable(Object info, boolean longClickable) {
            AccessibilityNodeInfoCompatIcs.setLongClickable(info, longClickable);
        }

        public void setPackageName(Object info, CharSequence packageName) {
            AccessibilityNodeInfoCompatIcs.setPackageName(info, packageName);
        }

        public void setParent(Object info, View parent) {
            AccessibilityNodeInfoCompatIcs.setParent(info, parent);
        }

        public void setPassword(Object info, boolean password) {
            AccessibilityNodeInfoCompatIcs.setPassword(info, password);
        }

        public void setScrollable(Object info, boolean scrollable) {
            AccessibilityNodeInfoCompatIcs.setScrollable(info, scrollable);
        }

        public void setSelected(Object info, boolean selected) {
            AccessibilityNodeInfoCompatIcs.setSelected(info, selected);
        }

        public void setSource(Object info, View source) {
            AccessibilityNodeInfoCompatIcs.setSource(info, source);
        }

        public void setText(Object info, CharSequence text) {
            AccessibilityNodeInfoCompatIcs.setText(info, text);
        }

        public void recycle(Object info) {
            AccessibilityNodeInfoCompatIcs.recycle(info);
        }
    }

    static class AccessibilityNodeInfoJellybeanImpl extends AccessibilityNodeInfoIcsImpl {
        AccessibilityNodeInfoJellybeanImpl() {
        }

        public Object obtain(View root, int virtualDescendantId) {
            return AccessibilityNodeInfoCompatJellyBean.obtain(root, virtualDescendantId);
        }

        public Object findFocus(Object info, int focus) {
            return AccessibilityNodeInfoCompatJellyBean.findFocus(info, focus);
        }

        public Object focusSearch(Object info, int direction) {
            return AccessibilityNodeInfoCompatJellyBean.focusSearch(info, direction);
        }

        public void addChild(Object info, View child, int virtualDescendantId) {
            AccessibilityNodeInfoCompatJellyBean.addChild(info, child, virtualDescendantId);
        }

        public void setSource(Object info, View root, int virtualDescendantId) {
            AccessibilityNodeInfoCompatJellyBean.setSource(info, root, virtualDescendantId);
        }

        public boolean isVisibleToUser(Object info) {
            return AccessibilityNodeInfoCompatJellyBean.isVisibleToUser(info);
        }

        public void setVisibleToUser(Object info, boolean visibleToUser) {
            AccessibilityNodeInfoCompatJellyBean.setVisibleToUser(info, visibleToUser);
        }

        public boolean isAccessibilityFocused(Object info) {
            return AccessibilityNodeInfoCompatJellyBean.isAccessibilityFocused(info);
        }

        public void setAccessibilityFocused(Object info, boolean focused) {
            AccessibilityNodeInfoCompatJellyBean.setAccesibilityFocused(info, focused);
        }

        public boolean performAction(Object info, int action, Bundle arguments) {
            return AccessibilityNodeInfoCompatJellyBean.performAction(info, action, arguments);
        }

        public void setMovementGranularities(Object info, int granularities) {
            AccessibilityNodeInfoCompatJellyBean.setMovementGranularities(info, granularities);
        }

        public int getMovementGranularities(Object info) {
            return AccessibilityNodeInfoCompatJellyBean.getMovementGranularities(info);
        }

        public void setParent(Object info, View root, int virtualDescendantId) {
            AccessibilityNodeInfoCompatJellyBean.setParent(info, root, virtualDescendantId);
        }
    }

    static class AccessibilityNodeInfoJellybeanMr2Impl extends AccessibilityNodeInfoJellybeanImpl {
        AccessibilityNodeInfoJellybeanMr2Impl() {
        }

        public String getViewIdResourceName(Object info) {
            return AccessibilityNodeInfoCompatJellybeanMr2.getViewIdResourceName(info);
        }

        public void setViewIdResourceName(Object info, String viewId) {
            AccessibilityNodeInfoCompatJellybeanMr2.setViewIdResourceName(info, viewId);
        }
    }

    static class AccessibilityNodeInfoKitKatImpl extends AccessibilityNodeInfoJellybeanMr2Impl {
        AccessibilityNodeInfoKitKatImpl() {
        }

        public int getLiveRegion(Object info) {
            return AccessibilityNodeInfoCompatKitKat.getLiveRegion(info);
        }

        public void setLiveRegion(Object info, int mode) {
            AccessibilityNodeInfoCompatKitKat.setLiveRegion(info, mode);
        }
    }

    static {
        if (VERSION.SDK_INT >= 19) {
            IMPL = new AccessibilityNodeInfoKitKatImpl();
        } else if (VERSION.SDK_INT >= 18) {
            IMPL = new AccessibilityNodeInfoJellybeanMr2Impl();
        } else if (VERSION.SDK_INT >= MOVEMENT_GRANULARITY_PAGE) {
            IMPL = new AccessibilityNodeInfoJellybeanImpl();
        } else if (VERSION.SDK_INT >= 14) {
            IMPL = new AccessibilityNodeInfoIcsImpl();
        } else {
            IMPL = new AccessibilityNodeInfoStubImpl();
        }
    }

    static AccessibilityNodeInfoCompat wrapNonNullInstance(Object object) {
        if (object != null) {
            return new AccessibilityNodeInfoCompat(object);
        }
        return null;
    }

    public AccessibilityNodeInfoCompat(Object info) {
        this.mInfo = info;
    }

    public Object getInfo() {
        return this.mInfo;
    }

    public static AccessibilityNodeInfoCompat obtain(View source) {
        return wrapNonNullInstance(IMPL.obtain(source));
    }

    public static AccessibilityNodeInfoCompat obtain(View root, int virtualDescendantId) {
        return wrapNonNullInstance(IMPL.obtain(root, virtualDescendantId));
    }

    public static AccessibilityNodeInfoCompat obtain() {
        return wrapNonNullInstance(IMPL.obtain());
    }

    public static AccessibilityNodeInfoCompat obtain(AccessibilityNodeInfoCompat info) {
        return wrapNonNullInstance(IMPL.obtain(info.mInfo));
    }

    public void setSource(View source) {
        IMPL.setSource(this.mInfo, source);
    }

    public void setSource(View root, int virtualDescendantId) {
        IMPL.setSource(this.mInfo, root, virtualDescendantId);
    }

    public AccessibilityNodeInfoCompat findFocus(int focus) {
        return wrapNonNullInstance(IMPL.findFocus(this.mInfo, focus));
    }

    public AccessibilityNodeInfoCompat focusSearch(int direction) {
        return wrapNonNullInstance(IMPL.focusSearch(this.mInfo, direction));
    }

    public int getWindowId() {
        return IMPL.getWindowId(this.mInfo);
    }

    public int getChildCount() {
        return IMPL.getChildCount(this.mInfo);
    }

    public AccessibilityNodeInfoCompat getChild(int index) {
        return wrapNonNullInstance(IMPL.getChild(this.mInfo, index));
    }

    public void addChild(View child) {
        IMPL.addChild(this.mInfo, child);
    }

    public void addChild(View root, int virtualDescendantId) {
        IMPL.addChild(this.mInfo, root, virtualDescendantId);
    }

    public int getActions() {
        return IMPL.getActions(this.mInfo);
    }

    public void addAction(int action) {
        IMPL.addAction(this.mInfo, action);
    }

    public boolean performAction(int action) {
        return IMPL.performAction(this.mInfo, action);
    }

    public boolean performAction(int action, Bundle arguments) {
        return IMPL.performAction(this.mInfo, action, arguments);
    }

    public void setMovementGranularities(int granularities) {
        IMPL.setMovementGranularities(this.mInfo, granularities);
    }

    public int getMovementGranularities() {
        return IMPL.getMovementGranularities(this.mInfo);
    }

    public List<AccessibilityNodeInfoCompat> findAccessibilityNodeInfosByText(String text) {
        List<AccessibilityNodeInfoCompat> result = new ArrayList();
        List<Object> infos = IMPL.findAccessibilityNodeInfosByText(this.mInfo, text);
        int infoCount = infos.size();
        for (int i = 0; i < infoCount; i += MOVEMENT_GRANULARITY_CHARACTER) {
            result.add(new AccessibilityNodeInfoCompat(infos.get(i)));
        }
        return result;
    }

    public AccessibilityNodeInfoCompat getParent() {
        return wrapNonNullInstance(IMPL.getParent(this.mInfo));
    }

    public void setParent(View parent) {
        IMPL.setParent(this.mInfo, parent);
    }

    public void setParent(View root, int virtualDescendantId) {
        IMPL.setParent(this.mInfo, root, virtualDescendantId);
    }

    public void getBoundsInParent(Rect outBounds) {
        IMPL.getBoundsInParent(this.mInfo, outBounds);
    }

    public void setBoundsInParent(Rect bounds) {
        IMPL.setBoundsInParent(this.mInfo, bounds);
    }

    public void getBoundsInScreen(Rect outBounds) {
        IMPL.getBoundsInScreen(this.mInfo, outBounds);
    }

    public void setBoundsInScreen(Rect bounds) {
        IMPL.setBoundsInScreen(this.mInfo, bounds);
    }

    public boolean isCheckable() {
        return IMPL.isCheckable(this.mInfo);
    }

    public void setCheckable(boolean checkable) {
        IMPL.setCheckable(this.mInfo, checkable);
    }

    public boolean isChecked() {
        return IMPL.isChecked(this.mInfo);
    }

    public void setChecked(boolean checked) {
        IMPL.setChecked(this.mInfo, checked);
    }

    public boolean isFocusable() {
        return IMPL.isFocusable(this.mInfo);
    }

    public void setFocusable(boolean focusable) {
        IMPL.setFocusable(this.mInfo, focusable);
    }

    public boolean isFocused() {
        return IMPL.isFocused(this.mInfo);
    }

    public void setFocused(boolean focused) {
        IMPL.setFocused(this.mInfo, focused);
    }

    public boolean isVisibleToUser() {
        return IMPL.isVisibleToUser(this.mInfo);
    }

    public void setVisibleToUser(boolean visibleToUser) {
        IMPL.setVisibleToUser(this.mInfo, visibleToUser);
    }

    public boolean isAccessibilityFocused() {
        return IMPL.isAccessibilityFocused(this.mInfo);
    }

    public void setAccessibilityFocused(boolean focused) {
        IMPL.setAccessibilityFocused(this.mInfo, focused);
    }

    public boolean isSelected() {
        return IMPL.isSelected(this.mInfo);
    }

    public void setSelected(boolean selected) {
        IMPL.setSelected(this.mInfo, selected);
    }

    public boolean isClickable() {
        return IMPL.isClickable(this.mInfo);
    }

    public void setClickable(boolean clickable) {
        IMPL.setClickable(this.mInfo, clickable);
    }

    public boolean isLongClickable() {
        return IMPL.isLongClickable(this.mInfo);
    }

    public void setLongClickable(boolean longClickable) {
        IMPL.setLongClickable(this.mInfo, longClickable);
    }

    public boolean isEnabled() {
        return IMPL.isEnabled(this.mInfo);
    }

    public void setEnabled(boolean enabled) {
        IMPL.setEnabled(this.mInfo, enabled);
    }

    public boolean isPassword() {
        return IMPL.isPassword(this.mInfo);
    }

    public void setPassword(boolean password) {
        IMPL.setPassword(this.mInfo, password);
    }

    public boolean isScrollable() {
        return IMPL.isScrollable(this.mInfo);
    }

    public void setScrollable(boolean scrollable) {
        IMPL.setScrollable(this.mInfo, scrollable);
    }

    public CharSequence getPackageName() {
        return IMPL.getPackageName(this.mInfo);
    }

    public void setPackageName(CharSequence packageName) {
        IMPL.setPackageName(this.mInfo, packageName);
    }

    public CharSequence getClassName() {
        return IMPL.getClassName(this.mInfo);
    }

    public void setClassName(CharSequence className) {
        IMPL.setClassName(this.mInfo, className);
    }

    public CharSequence getText() {
        return IMPL.getText(this.mInfo);
    }

    public void setText(CharSequence text) {
        IMPL.setText(this.mInfo, text);
    }

    public CharSequence getContentDescription() {
        return IMPL.getContentDescription(this.mInfo);
    }

    public void setContentDescription(CharSequence contentDescription) {
        IMPL.setContentDescription(this.mInfo, contentDescription);
    }

    public void recycle() {
        IMPL.recycle(this.mInfo);
    }

    public void setViewIdResourceName(String viewId) {
        IMPL.setViewIdResourceName(this.mInfo, viewId);
    }

    public String getViewIdResourceName() {
        return IMPL.getViewIdResourceName(this.mInfo);
    }

    public int getLiveRegion() {
        return IMPL.getLiveRegion(this.mInfo);
    }

    public void setLiveRegion(int mode) {
        IMPL.setLiveRegion(this.mInfo, mode);
    }

    public int hashCode() {
        return this.mInfo == null ? 0 : this.mInfo.hashCode();
    }

    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        AccessibilityNodeInfoCompat other = (AccessibilityNodeInfoCompat) obj;
        if (this.mInfo == null) {
            if (other.mInfo != null) {
                return false;
            }
            return true;
        } else if (this.mInfo.equals(other.mInfo)) {
            return true;
        } else {
            return false;
        }
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append(super.toString());
        Rect bounds = new Rect();
        getBoundsInParent(bounds);
        builder.append("; boundsInParent: " + bounds);
        getBoundsInScreen(bounds);
        builder.append("; boundsInScreen: " + bounds);
        builder.append("; packageName: ").append(getPackageName());
        builder.append("; className: ").append(getClassName());
        builder.append("; text: ").append(getText());
        builder.append("; contentDescription: ").append(getContentDescription());
        builder.append("; viewId: ").append(getViewIdResourceName());
        builder.append("; checkable: ").append(isCheckable());
        builder.append("; checked: ").append(isChecked());
        builder.append("; focusable: ").append(isFocusable());
        builder.append("; focused: ").append(isFocused());
        builder.append("; selected: ").append(isSelected());
        builder.append("; clickable: ").append(isClickable());
        builder.append("; longClickable: ").append(isLongClickable());
        builder.append("; enabled: ").append(isEnabled());
        builder.append("; password: ").append(isPassword());
        builder.append("; scrollable: " + isScrollable());
        builder.append("; [");
        int actionBits = getActions();
        while (actionBits != 0) {
            int action = MOVEMENT_GRANULARITY_CHARACTER << Integer.numberOfTrailingZeros(actionBits);
            actionBits &= action ^ -1;
            builder.append(getActionSymbolicName(action));
            if (actionBits != 0) {
                builder.append(", ");
            }
        }
        builder.append("]");
        return builder.toString();
    }

    private static String getActionSymbolicName(int action) {
        switch (action) {
            case MOVEMENT_GRANULARITY_CHARACTER /*1*/:
                return "ACTION_FOCUS";
            case MOVEMENT_GRANULARITY_WORD /*2*/:
                return "ACTION_CLEAR_FOCUS";
            case MOVEMENT_GRANULARITY_LINE /*4*/:
                return "ACTION_SELECT";
            case MOVEMENT_GRANULARITY_PARAGRAPH /*8*/:
                return "ACTION_CLEAR_SELECTION";
            case MOVEMENT_GRANULARITY_PAGE /*16*/:
                return "ACTION_CLICK";
            case ACTION_LONG_CLICK /*32*/:
                return "ACTION_LONG_CLICK";
            case ACTION_ACCESSIBILITY_FOCUS /*64*/:
                return "ACTION_ACCESSIBILITY_FOCUS";
            case ACTION_CLEAR_ACCESSIBILITY_FOCUS /*128*/:
                return "ACTION_CLEAR_ACCESSIBILITY_FOCUS";
            case ACTION_NEXT_AT_MOVEMENT_GRANULARITY /*256*/:
                return "ACTION_NEXT_AT_MOVEMENT_GRANULARITY";
            case ACTION_PREVIOUS_AT_MOVEMENT_GRANULARITY /*512*/:
                return "ACTION_PREVIOUS_AT_MOVEMENT_GRANULARITY";
            case ACTION_NEXT_HTML_ELEMENT /*1024*/:
                return "ACTION_NEXT_HTML_ELEMENT";
            case ACTION_PREVIOUS_HTML_ELEMENT /*2048*/:
                return "ACTION_PREVIOUS_HTML_ELEMENT";
            case ACTION_SCROLL_FORWARD /*4096*/:
                return "ACTION_SCROLL_FORWARD";
            case ACTION_SCROLL_BACKWARD /*8192*/:
                return "ACTION_SCROLL_BACKWARD";
            case ACTION_COPY /*16384*/:
                return "ACTION_COPY";
            case ACTION_PASTE /*32768*/:
                return "ACTION_PASTE";
            case ACTION_CUT /*65536*/:
                return "ACTION_CUT";
            case ACTION_SET_SELECTION /*131072*/:
                return "ACTION_SET_SELECTION";
            default:
                return "ACTION_UNKNOWN";
        }
    }
}
