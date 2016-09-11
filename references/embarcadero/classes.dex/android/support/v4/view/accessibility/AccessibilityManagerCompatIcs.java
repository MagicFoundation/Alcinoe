package android.support.v4.view.accessibility;

import android.accessibilityservice.AccessibilityServiceInfo;
import android.view.accessibility.AccessibilityManager;
import android.view.accessibility.AccessibilityManager.AccessibilityStateChangeListener;
import java.util.List;

class AccessibilityManagerCompatIcs {

    /* renamed from: android.support.v4.view.accessibility.AccessibilityManagerCompatIcs.1 */
    static class AnonymousClass1 implements AccessibilityStateChangeListener {
        final /* synthetic */ AccessibilityStateChangeListenerBridge val$bridge;

        AnonymousClass1(AccessibilityStateChangeListenerBridge accessibilityStateChangeListenerBridge) {
            this.val$bridge = accessibilityStateChangeListenerBridge;
        }

        public void onAccessibilityStateChanged(boolean enabled) {
            this.val$bridge.onAccessibilityStateChanged(enabled);
        }
    }

    interface AccessibilityStateChangeListenerBridge {
        void onAccessibilityStateChanged(boolean z);
    }

    AccessibilityManagerCompatIcs() {
    }

    public static Object newAccessibilityStateChangeListener(AccessibilityStateChangeListenerBridge bridge) {
        return new AnonymousClass1(bridge);
    }

    public static boolean addAccessibilityStateChangeListener(AccessibilityManager manager, Object listener) {
        return manager.addAccessibilityStateChangeListener((AccessibilityStateChangeListener) listener);
    }

    public static boolean removeAccessibilityStateChangeListener(AccessibilityManager manager, Object listener) {
        return manager.removeAccessibilityStateChangeListener((AccessibilityStateChangeListener) listener);
    }

    public static List<AccessibilityServiceInfo> getEnabledAccessibilityServiceList(AccessibilityManager manager, int feedbackTypeFlags) {
        return manager.getEnabledAccessibilityServiceList(feedbackTypeFlags);
    }

    public static List<AccessibilityServiceInfo> getInstalledAccessibilityServiceList(AccessibilityManager manager) {
        return manager.getInstalledAccessibilityServiceList();
    }

    public static boolean isTouchExplorationEnabled(AccessibilityManager manager) {
        return manager.isTouchExplorationEnabled();
    }
}
