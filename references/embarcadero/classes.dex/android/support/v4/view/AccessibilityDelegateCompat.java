package android.support.v4.view;

import android.os.Build.VERSION;
import android.os.Bundle;
import android.support.v4.view.AccessibilityDelegateCompatIcs.AccessibilityDelegateBridge;
import android.support.v4.view.AccessibilityDelegateCompatJellyBean.AccessibilityDelegateBridgeJellyBean;
import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat;
import android.support.v4.view.accessibility.AccessibilityNodeProviderCompat;
import android.view.View;
import android.view.ViewGroup;
import android.view.accessibility.AccessibilityEvent;

public class AccessibilityDelegateCompat {
    private static final Object DEFAULT_DELEGATE;
    private static final AccessibilityDelegateImpl IMPL;
    final Object mBridge;

    interface AccessibilityDelegateImpl {
        boolean dispatchPopulateAccessibilityEvent(Object obj, View view, AccessibilityEvent accessibilityEvent);

        AccessibilityNodeProviderCompat getAccessibilityNodeProvider(Object obj, View view);

        Object newAccessiblityDelegateBridge(AccessibilityDelegateCompat accessibilityDelegateCompat);

        Object newAccessiblityDelegateDefaultImpl();

        void onInitializeAccessibilityEvent(Object obj, View view, AccessibilityEvent accessibilityEvent);

        void onInitializeAccessibilityNodeInfo(Object obj, View view, AccessibilityNodeInfoCompat accessibilityNodeInfoCompat);

        void onPopulateAccessibilityEvent(Object obj, View view, AccessibilityEvent accessibilityEvent);

        boolean onRequestSendAccessibilityEvent(Object obj, ViewGroup viewGroup, View view, AccessibilityEvent accessibilityEvent);

        boolean performAccessibilityAction(Object obj, View view, int i, Bundle bundle);

        void sendAccessibilityEvent(Object obj, View view, int i);

        void sendAccessibilityEventUnchecked(Object obj, View view, AccessibilityEvent accessibilityEvent);
    }

    static class AccessibilityDelegateStubImpl implements AccessibilityDelegateImpl {
        AccessibilityDelegateStubImpl() {
        }

        public Object newAccessiblityDelegateDefaultImpl() {
            return null;
        }

        public Object newAccessiblityDelegateBridge(AccessibilityDelegateCompat listener) {
            return null;
        }

        public boolean dispatchPopulateAccessibilityEvent(Object delegate, View host, AccessibilityEvent event) {
            return false;
        }

        public void onInitializeAccessibilityEvent(Object delegate, View host, AccessibilityEvent event) {
        }

        public void onInitializeAccessibilityNodeInfo(Object delegate, View host, AccessibilityNodeInfoCompat info) {
        }

        public void onPopulateAccessibilityEvent(Object delegate, View host, AccessibilityEvent event) {
        }

        public boolean onRequestSendAccessibilityEvent(Object delegate, ViewGroup host, View child, AccessibilityEvent event) {
            return true;
        }

        public void sendAccessibilityEvent(Object delegate, View host, int eventType) {
        }

        public void sendAccessibilityEventUnchecked(Object delegate, View host, AccessibilityEvent event) {
        }

        public AccessibilityNodeProviderCompat getAccessibilityNodeProvider(Object delegate, View host) {
            return null;
        }

        public boolean performAccessibilityAction(Object delegate, View host, int action, Bundle args) {
            return false;
        }
    }

    static class AccessibilityDelegateIcsImpl extends AccessibilityDelegateStubImpl {

        /* renamed from: android.support.v4.view.AccessibilityDelegateCompat.AccessibilityDelegateIcsImpl.1 */
        class AnonymousClass1 implements AccessibilityDelegateBridge {
            final /* synthetic */ AccessibilityDelegateCompat val$compat;

            AnonymousClass1(AccessibilityDelegateCompat accessibilityDelegateCompat) {
                this.val$compat = accessibilityDelegateCompat;
            }

            public boolean dispatchPopulateAccessibilityEvent(View host, AccessibilityEvent event) {
                return this.val$compat.dispatchPopulateAccessibilityEvent(host, event);
            }

            public void onInitializeAccessibilityEvent(View host, AccessibilityEvent event) {
                this.val$compat.onInitializeAccessibilityEvent(host, event);
            }

            public void onInitializeAccessibilityNodeInfo(View host, Object info) {
                this.val$compat.onInitializeAccessibilityNodeInfo(host, new AccessibilityNodeInfoCompat(info));
            }

            public void onPopulateAccessibilityEvent(View host, AccessibilityEvent event) {
                this.val$compat.onPopulateAccessibilityEvent(host, event);
            }

            public boolean onRequestSendAccessibilityEvent(ViewGroup host, View child, AccessibilityEvent event) {
                return this.val$compat.onRequestSendAccessibilityEvent(host, child, event);
            }

            public void sendAccessibilityEvent(View host, int eventType) {
                this.val$compat.sendAccessibilityEvent(host, eventType);
            }

            public void sendAccessibilityEventUnchecked(View host, AccessibilityEvent event) {
                this.val$compat.sendAccessibilityEventUnchecked(host, event);
            }
        }

        AccessibilityDelegateIcsImpl() {
        }

        public Object newAccessiblityDelegateDefaultImpl() {
            return AccessibilityDelegateCompatIcs.newAccessibilityDelegateDefaultImpl();
        }

        public Object newAccessiblityDelegateBridge(AccessibilityDelegateCompat compat) {
            return AccessibilityDelegateCompatIcs.newAccessibilityDelegateBridge(new AnonymousClass1(compat));
        }

        public boolean dispatchPopulateAccessibilityEvent(Object delegate, View host, AccessibilityEvent event) {
            return AccessibilityDelegateCompatIcs.dispatchPopulateAccessibilityEvent(delegate, host, event);
        }

        public void onInitializeAccessibilityEvent(Object delegate, View host, AccessibilityEvent event) {
            AccessibilityDelegateCompatIcs.onInitializeAccessibilityEvent(delegate, host, event);
        }

        public void onInitializeAccessibilityNodeInfo(Object delegate, View host, AccessibilityNodeInfoCompat info) {
            AccessibilityDelegateCompatIcs.onInitializeAccessibilityNodeInfo(delegate, host, info.getInfo());
        }

        public void onPopulateAccessibilityEvent(Object delegate, View host, AccessibilityEvent event) {
            AccessibilityDelegateCompatIcs.onPopulateAccessibilityEvent(delegate, host, event);
        }

        public boolean onRequestSendAccessibilityEvent(Object delegate, ViewGroup host, View child, AccessibilityEvent event) {
            return AccessibilityDelegateCompatIcs.onRequestSendAccessibilityEvent(delegate, host, child, event);
        }

        public void sendAccessibilityEvent(Object delegate, View host, int eventType) {
            AccessibilityDelegateCompatIcs.sendAccessibilityEvent(delegate, host, eventType);
        }

        public void sendAccessibilityEventUnchecked(Object delegate, View host, AccessibilityEvent event) {
            AccessibilityDelegateCompatIcs.sendAccessibilityEventUnchecked(delegate, host, event);
        }
    }

    static class AccessibilityDelegateJellyBeanImpl extends AccessibilityDelegateIcsImpl {

        /* renamed from: android.support.v4.view.AccessibilityDelegateCompat.AccessibilityDelegateJellyBeanImpl.1 */
        class AnonymousClass1 implements AccessibilityDelegateBridgeJellyBean {
            final /* synthetic */ AccessibilityDelegateCompat val$compat;

            AnonymousClass1(AccessibilityDelegateCompat accessibilityDelegateCompat) {
                this.val$compat = accessibilityDelegateCompat;
            }

            public boolean dispatchPopulateAccessibilityEvent(View host, AccessibilityEvent event) {
                return this.val$compat.dispatchPopulateAccessibilityEvent(host, event);
            }

            public void onInitializeAccessibilityEvent(View host, AccessibilityEvent event) {
                this.val$compat.onInitializeAccessibilityEvent(host, event);
            }

            public void onInitializeAccessibilityNodeInfo(View host, Object info) {
                this.val$compat.onInitializeAccessibilityNodeInfo(host, new AccessibilityNodeInfoCompat(info));
            }

            public void onPopulateAccessibilityEvent(View host, AccessibilityEvent event) {
                this.val$compat.onPopulateAccessibilityEvent(host, event);
            }

            public boolean onRequestSendAccessibilityEvent(ViewGroup host, View child, AccessibilityEvent event) {
                return this.val$compat.onRequestSendAccessibilityEvent(host, child, event);
            }

            public void sendAccessibilityEvent(View host, int eventType) {
                this.val$compat.sendAccessibilityEvent(host, eventType);
            }

            public void sendAccessibilityEventUnchecked(View host, AccessibilityEvent event) {
                this.val$compat.sendAccessibilityEventUnchecked(host, event);
            }

            public Object getAccessibilityNodeProvider(View host) {
                AccessibilityNodeProviderCompat provider = this.val$compat.getAccessibilityNodeProvider(host);
                return provider != null ? provider.getProvider() : null;
            }

            public boolean performAccessibilityAction(View host, int action, Bundle args) {
                return this.val$compat.performAccessibilityAction(host, action, args);
            }
        }

        AccessibilityDelegateJellyBeanImpl() {
        }

        public Object newAccessiblityDelegateBridge(AccessibilityDelegateCompat compat) {
            return AccessibilityDelegateCompatJellyBean.newAccessibilityDelegateBridge(new AnonymousClass1(compat));
        }

        public AccessibilityNodeProviderCompat getAccessibilityNodeProvider(Object delegate, View host) {
            Object provider = AccessibilityDelegateCompatJellyBean.getAccessibilityNodeProvider(delegate, host);
            if (provider != null) {
                return new AccessibilityNodeProviderCompat(provider);
            }
            return null;
        }

        public boolean performAccessibilityAction(Object delegate, View host, int action, Bundle args) {
            return AccessibilityDelegateCompatJellyBean.performAccessibilityAction(delegate, host, action, args);
        }
    }

    static {
        if (VERSION.SDK_INT >= 16) {
            IMPL = new AccessibilityDelegateJellyBeanImpl();
        } else if (VERSION.SDK_INT >= 14) {
            IMPL = new AccessibilityDelegateIcsImpl();
        } else {
            IMPL = new AccessibilityDelegateStubImpl();
        }
        DEFAULT_DELEGATE = IMPL.newAccessiblityDelegateDefaultImpl();
    }

    public AccessibilityDelegateCompat() {
        this.mBridge = IMPL.newAccessiblityDelegateBridge(this);
    }

    Object getBridge() {
        return this.mBridge;
    }

    public void sendAccessibilityEvent(View host, int eventType) {
        IMPL.sendAccessibilityEvent(DEFAULT_DELEGATE, host, eventType);
    }

    public void sendAccessibilityEventUnchecked(View host, AccessibilityEvent event) {
        IMPL.sendAccessibilityEventUnchecked(DEFAULT_DELEGATE, host, event);
    }

    public boolean dispatchPopulateAccessibilityEvent(View host, AccessibilityEvent event) {
        return IMPL.dispatchPopulateAccessibilityEvent(DEFAULT_DELEGATE, host, event);
    }

    public void onPopulateAccessibilityEvent(View host, AccessibilityEvent event) {
        IMPL.onPopulateAccessibilityEvent(DEFAULT_DELEGATE, host, event);
    }

    public void onInitializeAccessibilityEvent(View host, AccessibilityEvent event) {
        IMPL.onInitializeAccessibilityEvent(DEFAULT_DELEGATE, host, event);
    }

    public void onInitializeAccessibilityNodeInfo(View host, AccessibilityNodeInfoCompat info) {
        IMPL.onInitializeAccessibilityNodeInfo(DEFAULT_DELEGATE, host, info);
    }

    public boolean onRequestSendAccessibilityEvent(ViewGroup host, View child, AccessibilityEvent event) {
        return IMPL.onRequestSendAccessibilityEvent(DEFAULT_DELEGATE, host, child, event);
    }

    public AccessibilityNodeProviderCompat getAccessibilityNodeProvider(View host) {
        return IMPL.getAccessibilityNodeProvider(DEFAULT_DELEGATE, host);
    }

    public boolean performAccessibilityAction(View host, int action, Bundle args) {
        return IMPL.performAccessibilityAction(DEFAULT_DELEGATE, host, action, args);
    }
}
