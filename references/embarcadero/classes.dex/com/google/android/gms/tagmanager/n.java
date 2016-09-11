package com.google.android.gms.tagmanager;

import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.tagmanager.ContainerHolder.ContainerAvailableListener;
import com.google.android.vending.licensing.APKExpansionPolicy;

class n implements ContainerHolder {
    private Container TU;
    private Container TV;
    private b TW;
    private a TX;
    private TagManager TY;
    private Status vl;
    private boolean zk;
    private final Looper zs;

    public interface a {
        void bc(String str);

        String iF();

        void iH();
    }

    private class b extends Handler {
        private final ContainerAvailableListener TZ;
        final /* synthetic */ n Ua;

        public b(n nVar, ContainerAvailableListener containerAvailableListener, Looper looper) {
            this.Ua = nVar;
            super(looper);
            this.TZ = containerAvailableListener;
        }

        public void bd(String str) {
            sendMessage(obtainMessage(1, str));
        }

        protected void be(String str) {
            this.TZ.onContainerAvailable(this.Ua, str);
        }

        public void handleMessage(Message msg) {
            switch (msg.what) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    be((String) msg.obj);
                default:
                    bh.t("Don't know how to handle this message.");
            }
        }
    }

    public n(Status status) {
        this.vl = status;
        this.zs = null;
    }

    public n(TagManager tagManager, Looper looper, Container container, a aVar) {
        this.TY = tagManager;
        if (looper == null) {
            looper = Looper.getMainLooper();
        }
        this.zs = looper;
        this.TU = container;
        this.TX = aVar;
        this.vl = Status.zQ;
        tagManager.a(this);
    }

    private void iG() {
        if (this.TW != null) {
            this.TW.bd(this.TV.iD());
        }
    }

    public synchronized void a(Container container) {
        if (!this.zk) {
            if (container == null) {
                bh.t("Unexpected null container.");
            } else {
                this.TV = container;
                iG();
            }
        }
    }

    public synchronized void ba(String str) {
        if (!this.zk) {
            this.TU.ba(str);
        }
    }

    void bc(String str) {
        if (this.zk) {
            bh.t("setCtfeUrlPathAndQuery called on a released ContainerHolder.");
        } else {
            this.TX.bc(str);
        }
    }

    public synchronized Container getContainer() {
        Container container = null;
        synchronized (this) {
            if (this.zk) {
                bh.t("ContainerHolder is released.");
            } else {
                if (this.TV != null) {
                    this.TU = this.TV;
                    this.TV = null;
                }
                container = this.TU;
            }
        }
        return container;
    }

    String getContainerId() {
        if (!this.zk) {
            return this.TU.getContainerId();
        }
        bh.t("getContainerId called on a released ContainerHolder.");
        return "";
    }

    public Status getStatus() {
        return this.vl;
    }

    String iF() {
        if (!this.zk) {
            return this.TX.iF();
        }
        bh.t("setCtfeUrlPathAndQuery called on a released ContainerHolder.");
        return "";
    }

    public synchronized void refresh() {
        if (this.zk) {
            bh.t("Refreshing a released ContainerHolder.");
        } else {
            this.TX.iH();
        }
    }

    public synchronized void release() {
        if (this.zk) {
            bh.t("Releasing a released ContainerHolder.");
        } else {
            this.zk = true;
            this.TY.b(this);
            this.TU.release();
            this.TU = null;
            this.TV = null;
            this.TX = null;
            this.TW = null;
        }
    }

    public synchronized void setContainerAvailableListener(ContainerAvailableListener listener) {
        if (this.zk) {
            bh.t("ContainerHolder is released.");
        } else if (listener == null) {
            this.TW = null;
        } else {
            this.TW = new b(this, listener, this.zs);
            if (this.TV != null) {
                iG();
            }
        }
    }
}
