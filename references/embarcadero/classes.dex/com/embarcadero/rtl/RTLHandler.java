package com.embarcadero.rtl;

import android.os.Handler;
import android.os.Message;

public class RTLHandler extends Handler {
    public final RTLSuperHandler Super;
    private Listener mListener;

    public interface Listener {
        void handleMessage(Message message);
    }

    public class RTLSuperHandler {
        RTLHandler parent;

        public RTLSuperHandler(RTLHandler paramRTLHandler) {
            this.parent = paramRTLHandler;
        }

        public void handleMessage(Message paramMessage) {
            this.parent.superHandleMessage(paramMessage);
        }
    }

    public RTLHandler(Listener paramListener) {
        this.Super = new RTLSuperHandler(this);
        this.mListener = paramListener;
    }

    private void superHandleMessage(Message paramMessage) {
        super.handleMessage(paramMessage);
    }

    public void handleMessage(Message paramMessage) {
        this.mListener.handleMessage(paramMessage);
    }
}
