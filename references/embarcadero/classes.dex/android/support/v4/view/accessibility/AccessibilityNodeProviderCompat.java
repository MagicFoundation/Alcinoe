package android.support.v4.view.accessibility;

import android.os.Build.VERSION;
import android.os.Bundle;
import java.util.ArrayList;
import java.util.List;

public class AccessibilityNodeProviderCompat {
    private static final AccessibilityNodeProviderImpl IMPL;
    private final Object mProvider;

    interface AccessibilityNodeProviderImpl {
        Object newAccessibilityNodeProviderBridge(AccessibilityNodeProviderCompat accessibilityNodeProviderCompat);
    }

    static class AccessibilityNodeProviderStubImpl implements AccessibilityNodeProviderImpl {
        AccessibilityNodeProviderStubImpl() {
        }

        public Object newAccessibilityNodeProviderBridge(AccessibilityNodeProviderCompat compat) {
            return null;
        }
    }

    static class AccessibilityNodeProviderJellyBeanImpl extends AccessibilityNodeProviderStubImpl {

        /* renamed from: android.support.v4.view.accessibility.AccessibilityNodeProviderCompat.AccessibilityNodeProviderJellyBeanImpl.1 */
        class AnonymousClass1 implements AccessibilityNodeInfoBridge {
            final /* synthetic */ AccessibilityNodeProviderCompat val$compat;

            AnonymousClass1(AccessibilityNodeProviderCompat accessibilityNodeProviderCompat) {
                this.val$compat = accessibilityNodeProviderCompat;
            }

            public boolean performAction(int virtualViewId, int action, Bundle arguments) {
                return this.val$compat.performAction(virtualViewId, action, arguments);
            }

            public List<Object> findAccessibilityNodeInfosByText(String text, int virtualViewId) {
                List<AccessibilityNodeInfoCompat> compatInfos = this.val$compat.findAccessibilityNodeInfosByText(text, virtualViewId);
                List<Object> infos = new ArrayList();
                int infoCount = compatInfos.size();
                for (int i = 0; i < infoCount; i++) {
                    infos.add(((AccessibilityNodeInfoCompat) compatInfos.get(i)).getInfo());
                }
                return infos;
            }

            public Object createAccessibilityNodeInfo(int virtualViewId) {
                AccessibilityNodeInfoCompat compatInfo = this.val$compat.createAccessibilityNodeInfo(virtualViewId);
                if (compatInfo == null) {
                    return null;
                }
                return compatInfo.getInfo();
            }
        }

        AccessibilityNodeProviderJellyBeanImpl() {
        }

        public Object newAccessibilityNodeProviderBridge(AccessibilityNodeProviderCompat compat) {
            return AccessibilityNodeProviderCompatJellyBean.newAccessibilityNodeProviderBridge(new AnonymousClass1(compat));
        }
    }

    static class AccessibilityNodeProviderKitKatImpl extends AccessibilityNodeProviderStubImpl {

        /* renamed from: android.support.v4.view.accessibility.AccessibilityNodeProviderCompat.AccessibilityNodeProviderKitKatImpl.1 */
        class AnonymousClass1 implements AccessibilityNodeInfoBridge {
            final /* synthetic */ AccessibilityNodeProviderCompat val$compat;

            AnonymousClass1(AccessibilityNodeProviderCompat accessibilityNodeProviderCompat) {
                this.val$compat = accessibilityNodeProviderCompat;
            }

            public boolean performAction(int virtualViewId, int action, Bundle arguments) {
                return this.val$compat.performAction(virtualViewId, action, arguments);
            }

            public List<Object> findAccessibilityNodeInfosByText(String text, int virtualViewId) {
                List<AccessibilityNodeInfoCompat> compatInfos = this.val$compat.findAccessibilityNodeInfosByText(text, virtualViewId);
                List<Object> infos = new ArrayList();
                int infoCount = compatInfos.size();
                for (int i = 0; i < infoCount; i++) {
                    infos.add(((AccessibilityNodeInfoCompat) compatInfos.get(i)).getInfo());
                }
                return infos;
            }

            public Object createAccessibilityNodeInfo(int virtualViewId) {
                AccessibilityNodeInfoCompat compatInfo = this.val$compat.createAccessibilityNodeInfo(virtualViewId);
                if (compatInfo == null) {
                    return null;
                }
                return compatInfo.getInfo();
            }

            public Object findFocus(int focus) {
                AccessibilityNodeInfoCompat compatInfo = this.val$compat.findFocus(focus);
                if (compatInfo == null) {
                    return null;
                }
                return compatInfo.getInfo();
            }
        }

        AccessibilityNodeProviderKitKatImpl() {
        }

        public Object newAccessibilityNodeProviderBridge(AccessibilityNodeProviderCompat compat) {
            return AccessibilityNodeProviderCompatKitKat.newAccessibilityNodeProviderBridge(new AnonymousClass1(compat));
        }
    }

    static {
        if (VERSION.SDK_INT >= 19) {
            IMPL = new AccessibilityNodeProviderKitKatImpl();
        } else if (VERSION.SDK_INT >= 16) {
            IMPL = new AccessibilityNodeProviderJellyBeanImpl();
        } else {
            IMPL = new AccessibilityNodeProviderStubImpl();
        }
    }

    public AccessibilityNodeProviderCompat() {
        this.mProvider = IMPL.newAccessibilityNodeProviderBridge(this);
    }

    public AccessibilityNodeProviderCompat(Object provider) {
        this.mProvider = provider;
    }

    public Object getProvider() {
        return this.mProvider;
    }

    public AccessibilityNodeInfoCompat createAccessibilityNodeInfo(int virtualViewId) {
        return null;
    }

    public boolean performAction(int virtualViewId, int action, Bundle arguments) {
        return false;
    }

    public List<AccessibilityNodeInfoCompat> findAccessibilityNodeInfosByText(String text, int virtualViewId) {
        return null;
    }

    public AccessibilityNodeInfoCompat findFocus(int focus) {
        return null;
    }
}
