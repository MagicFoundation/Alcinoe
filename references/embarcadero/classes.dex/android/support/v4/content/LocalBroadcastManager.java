package android.support.v4.content;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.support.v4.media.TransportMediator;
import android.util.Log;
import com.google.android.gms.ads.AdSize;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMatch;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

public class LocalBroadcastManager {
    private static final boolean DEBUG = false;
    static final int MSG_EXEC_PENDING_BROADCASTS = 1;
    private static final String TAG = "LocalBroadcastManager";
    private static LocalBroadcastManager mInstance;
    private static final Object mLock;
    private final HashMap<String, ArrayList<ReceiverRecord>> mActions;
    private final Context mAppContext;
    private final Handler mHandler;
    private final ArrayList<BroadcastRecord> mPendingBroadcasts;
    private final HashMap<BroadcastReceiver, ArrayList<IntentFilter>> mReceivers;

    /* renamed from: android.support.v4.content.LocalBroadcastManager.1 */
    class AnonymousClass1 extends Handler {
        AnonymousClass1(Looper x0) {
            super(x0);
        }

        public void handleMessage(Message msg) {
            switch (msg.what) {
                case LocalBroadcastManager.MSG_EXEC_PENDING_BROADCASTS /*1*/:
                    LocalBroadcastManager.this.executePendingBroadcasts();
                default:
                    super.handleMessage(msg);
            }
        }
    }

    private static class BroadcastRecord {
        final Intent intent;
        final ArrayList<ReceiverRecord> receivers;

        BroadcastRecord(Intent _intent, ArrayList<ReceiverRecord> _receivers) {
            this.intent = _intent;
            this.receivers = _receivers;
        }
    }

    private static class ReceiverRecord {
        boolean broadcasting;
        final IntentFilter filter;
        final BroadcastReceiver receiver;

        ReceiverRecord(IntentFilter _filter, BroadcastReceiver _receiver) {
            this.filter = _filter;
            this.receiver = _receiver;
        }

        public String toString() {
            StringBuilder builder = new StringBuilder(TransportMediator.FLAG_KEY_MEDIA_NEXT);
            builder.append("Receiver{");
            builder.append(this.receiver);
            builder.append(" filter=");
            builder.append(this.filter);
            builder.append("}");
            return builder.toString();
        }
    }

    static {
        mLock = new Object();
    }

    public static LocalBroadcastManager getInstance(Context context) {
        LocalBroadcastManager localBroadcastManager;
        synchronized (mLock) {
            if (mInstance == null) {
                mInstance = new LocalBroadcastManager(context.getApplicationContext());
            }
            localBroadcastManager = mInstance;
        }
        return localBroadcastManager;
    }

    private LocalBroadcastManager(Context context) {
        this.mReceivers = new HashMap();
        this.mActions = new HashMap();
        this.mPendingBroadcasts = new ArrayList();
        this.mAppContext = context;
        this.mHandler = new AnonymousClass1(context.getMainLooper());
    }

    public void registerReceiver(BroadcastReceiver receiver, IntentFilter filter) {
        synchronized (this.mReceivers) {
            ReceiverRecord entry = new ReceiverRecord(filter, receiver);
            ArrayList<IntentFilter> filters = (ArrayList) this.mReceivers.get(receiver);
            if (filters == null) {
                filters = new ArrayList(MSG_EXEC_PENDING_BROADCASTS);
                this.mReceivers.put(receiver, filters);
            }
            filters.add(filter);
            for (int i = 0; i < filter.countActions(); i += MSG_EXEC_PENDING_BROADCASTS) {
                String action = filter.getAction(i);
                ArrayList<ReceiverRecord> entries = (ArrayList) this.mActions.get(action);
                if (entries == null) {
                    entries = new ArrayList(MSG_EXEC_PENDING_BROADCASTS);
                    this.mActions.put(action, entries);
                }
                entries.add(entry);
            }
        }
    }

    public void unregisterReceiver(BroadcastReceiver receiver) {
        synchronized (this.mReceivers) {
            ArrayList<IntentFilter> filters = (ArrayList) this.mReceivers.remove(receiver);
            if (filters == null) {
                return;
            }
            for (int i = 0; i < filters.size(); i += MSG_EXEC_PENDING_BROADCASTS) {
                IntentFilter filter = (IntentFilter) filters.get(i);
                for (int j = 0; j < filter.countActions(); j += MSG_EXEC_PENDING_BROADCASTS) {
                    String action = filter.getAction(j);
                    ArrayList<ReceiverRecord> receivers = (ArrayList) this.mActions.get(action);
                    if (receivers != null) {
                        int k = 0;
                        while (k < receivers.size()) {
                            if (((ReceiverRecord) receivers.get(k)).receiver == receiver) {
                                receivers.remove(k);
                                k--;
                            }
                            k += MSG_EXEC_PENDING_BROADCASTS;
                        }
                        if (receivers.size() <= 0) {
                            this.mActions.remove(action);
                        }
                    }
                }
            }
        }
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    public boolean sendBroadcast(Intent intent) {
        synchronized (this.mReceivers) {
            String action = intent.getAction();
            String type = intent.resolveTypeIfNeeded(this.mAppContext.getContentResolver());
            Uri data = intent.getData();
            String scheme = intent.getScheme();
            Set<String> categories = intent.getCategories();
            boolean debug = (intent.getFlags() & 8) != 0 ? true : DEBUG;
            if (debug) {
                String str = " of intent ";
                Log.v(TAG, "Resolving type " + type + " scheme " + scheme + r16 + intent);
            }
            ArrayList<ReceiverRecord> entries = (ArrayList) this.mActions.get(intent.getAction());
            if (entries != null) {
                int i;
                if (debug) {
                    Log.v(TAG, "Action list: " + entries);
                }
                ArrayList<ReceiverRecord> receivers = null;
                for (i = 0; i < entries.size(); i += MSG_EXEC_PENDING_BROADCASTS) {
                    ReceiverRecord receiver = (ReceiverRecord) entries.get(i);
                    if (debug) {
                        Log.v(TAG, "Matching against filter " + receiver.filter);
                    }
                    if (!receiver.broadcasting) {
                        int match = receiver.filter.match(action, type, scheme, data, categories, TAG);
                        if (match >= 0) {
                            if (debug) {
                                Log.v(TAG, "  Filter matched!  match=0x" + Integer.toHexString(match));
                            }
                            if (receivers == null) {
                                receivers = new ArrayList();
                            }
                            receivers.add(receiver);
                            receiver.broadcasting = true;
                        } else if (debug) {
                            String reason;
                            switch (match) {
                                case -4:
                                    reason = "category";
                                    break;
                                case -3:
                                    reason = "action";
                                    break;
                                case AdSize.AUTO_HEIGHT /*-2*/:
                                    reason = "data";
                                    break;
                                case TurnBasedMatch.MATCH_VARIANT_DEFAULT /*-1*/:
                                    reason = "type";
                                    break;
                                default:
                                    reason = "unknown reason";
                                    break;
                            }
                        } else {
                            continue;
                        }
                    } else if (debug) {
                        Log.v(TAG, "  Filter's target already added");
                    }
                }
                if (receivers != null) {
                    for (i = 0; i < receivers.size(); i += MSG_EXEC_PENDING_BROADCASTS) {
                        ((ReceiverRecord) receivers.get(i)).broadcasting = DEBUG;
                    }
                    this.mPendingBroadcasts.add(new BroadcastRecord(intent, receivers));
                    if (!this.mHandler.hasMessages(MSG_EXEC_PENDING_BROADCASTS)) {
                        this.mHandler.sendEmptyMessage(MSG_EXEC_PENDING_BROADCASTS);
                    }
                    return true;
                }
            }
            return DEBUG;
        }
    }

    public void sendBroadcastSync(Intent intent) {
        if (sendBroadcast(intent)) {
            executePendingBroadcasts();
        }
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    private void executePendingBroadcasts() {
        while (true) {
            synchronized (this.mReceivers) {
                int N = this.mPendingBroadcasts.size();
                if (N <= 0) {
                    return;
                }
                BroadcastRecord[] brs = new BroadcastRecord[N];
                this.mPendingBroadcasts.toArray(brs);
                this.mPendingBroadcasts.clear();
                for (int i = 0; i < brs.length; i += MSG_EXEC_PENDING_BROADCASTS) {
                    BroadcastRecord br = brs[i];
                    for (int j = 0; j < br.receivers.size(); j += MSG_EXEC_PENDING_BROADCASTS) {
                        ((ReceiverRecord) br.receivers.get(j)).receiver.onReceive(this.mAppContext, br.intent);
                    }
                }
            }
        }
        while (true) {
            break;
        }
    }
}
