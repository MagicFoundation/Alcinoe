package android.support.v4.view;

import android.os.Bundle;
import android.view.View;
import android.view.View.AccessibilityDelegate;
import android.view.ViewGroup;
import android.view.accessibility.AccessibilityEvent;
import android.view.accessibility.AccessibilityNodeInfo;
import android.view.accessibility.AccessibilityNodeProvider;

class AccessibilityDelegateCompatJellyBean {

    /* renamed from: android.support.v4.view.AccessibilityDelegateCompatJellyBean.1 */
    static class AnonymousClass1 extends AccessibilityDelegate {
        final /* synthetic */ AccessibilityDelegateBridgeJellyBean val$bridge;

        AnonymousClass1(AccessibilityDelegateBridgeJellyBean accessibilityDelegateBridgeJellyBean) {
            this.val$bridge = accessibilityDelegateBridgeJellyBean;
        }

        public boolean dispatchPopulateAccessibilityEvent(View host, AccessibilityEvent event) {
            return this.val$bridge.dispatchPopulateAccessibilityEvent(host, event);
        }

        public void onInitializeAccessibilityEvent(View host, AccessibilityEvent event) {
            this.val$bridge.onInitializeAccessibilityEvent(host, event);
        }

        public void onInitializeAccessibilityNodeInfo(View host, AccessibilityNodeInfo info) {
            this.val$bridge.onInitializeAccessibilityNodeInfo(host, info);
        }

        public void onPopulateAccessibilityEvent(View host, AccessibilityEvent event) {
            this.val$bridge.onPopulateAccessibilityEvent(host, event);
        }

        public boolean onRequestSendAccessibilityEvent(ViewGroup host, View child, AccessibilityEvent event) {
            return this.val$bridge.onRequestSendAccessibilityEvent(host, child, event);
        }

        public void sendAccessibilityEvent(View host, int eventType) {
            this.val$bridge.sendAccessibilityEvent(host, eventType);
        }

        public void sendAccessibilityEventUnchecked(View host, AccessibilityEvent event) {
            this.val$bridge.sendAccessibilityEventUnchecked(host, event);
        }

        public AccessibilityNodeProvider getAccessibilityNodeProvider(View host) {
            return (AccessibilityNodeProvider) this.val$bridge.getAccessibilityNodeProvider(host);
        }

        public boolean performAccessibilityAction(View host, int action, Bundle args) {
            return this.val$bridge.performAccessibilityAction(host, action, args);
        }
    }

    public interface AccessibilityDelegateBridgeJellyBean {
        boolean dispatchPopulateAccessibilityEvent(View view, AccessibilityEvent accessibilityEvent);

        Object getAccessibilityNodeProvider(View view);

        void onInitializeAccessibilityEvent(View view, AccessibilityEvent accessibilityEvent);

        void onInitializeAccessibilityNodeInfo(View view, Object obj);

        void onPopulateAccessibilityEvent(View view, AccessibilityEvent accessibilityEvent);

        boolean onRequestSendAccessibilityEvent(ViewGroup viewGroup, View view, AccessibilityEvent accessibilityEvent);

        boolean performAccessibilityAction(View view, int i, Bundle bundle);

        void sendAccessibilityEvent(View view, int i);

        void sendAccessibilityEventUnchecked(View view, AccessibilityEvent accessibilityEvent);
    }

    AccessibilityDelegateCompatJellyBean() {
    }

    public static Object newAccessibilityDelegateBridge(AccessibilityDelegateBridgeJellyBean bridge) {
        return new AnonymousClass1(bridge);
    }

    public static Object getAccessibilityNodeProvider(Object delegate, View host) {
        return ((AccessibilityDelegate) delegate).getAccessibilityNodeProvider(host);
    }

    public static boolean performAccessibilityAction(Object delegate, View host, int action, Bundle args) {
        return ((AccessibilityDelegate) delegate).performAccessibilityAction(host, action, args);
    }
}
