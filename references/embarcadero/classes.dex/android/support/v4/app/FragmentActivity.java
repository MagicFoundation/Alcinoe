package android.support.v4.app;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.content.res.Resources.NotFoundException;
import android.content.res.TypedArray;
import android.os.Build.VERSION;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.os.Parcelable;
import android.support.v4.internal.view.SupportMenu;
import android.support.v4.media.TransportMediator;
import android.support.v4.util.SimpleArrayMap;
import android.support.v4.view.ViewCompat;
import android.util.AttributeSet;
import android.util.Log;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.games.request.GameRequest;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.io.FileDescriptor;
import java.io.PrintWriter;
import java.util.ArrayList;

public class FragmentActivity extends Activity {
    static final String FRAGMENTS_TAG = "android:support:fragments";
    private static final int HONEYCOMB = 11;
    static final int MSG_REALLY_STOPPED = 1;
    static final int MSG_RESUME_PENDING = 2;
    private static final String TAG = "FragmentActivity";
    SimpleArrayMap<String, LoaderManagerImpl> mAllLoaderManagers;
    boolean mCheckedForLoaderManager;
    final FragmentContainer mContainer;
    boolean mCreated;
    final FragmentManagerImpl mFragments;
    final Handler mHandler;
    LoaderManagerImpl mLoaderManager;
    boolean mLoadersStarted;
    boolean mOptionsMenuInvalidated;
    boolean mReallyStopped;
    boolean mResumed;
    boolean mRetaining;
    boolean mStopped;

    static class FragmentTag {
        public static final int[] Fragment;
        public static final int Fragment_id = 1;
        public static final int Fragment_name = 0;
        public static final int Fragment_tag = 2;

        FragmentTag() {
        }

        static {
            Fragment = new int[]{16842755, 16842960, 16842961};
        }
    }

    static final class NonConfigurationInstances {
        Object activity;
        SimpleArrayMap<String, Object> children;
        Object custom;
        ArrayList<Fragment> fragments;
        SimpleArrayMap<String, LoaderManagerImpl> loaders;

        NonConfigurationInstances() {
        }
    }

    public FragmentActivity() {
        this.mHandler = new Handler() {
            public void handleMessage(Message msg) {
                switch (msg.what) {
                    case FragmentActivity.MSG_REALLY_STOPPED /*1*/:
                        if (FragmentActivity.this.mStopped) {
                            FragmentActivity.this.doReallyStop(false);
                        }
                    case FragmentActivity.MSG_RESUME_PENDING /*2*/:
                        FragmentActivity.this.onResumeFragments();
                        FragmentActivity.this.mFragments.execPendingActions();
                    default:
                        super.handleMessage(msg);
                }
            }
        };
        this.mFragments = new FragmentManagerImpl();
        this.mContainer = new FragmentContainer() {
            public View findViewById(int id) {
                return FragmentActivity.this.findViewById(id);
            }
        };
    }

    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        this.mFragments.noteStateNotSaved();
        int index = requestCode >> 16;
        if (index != 0) {
            index--;
            if (this.mFragments.mActive == null || index < 0 || index >= this.mFragments.mActive.size()) {
                Log.w(TAG, "Activity result fragment index out of range: 0x" + Integer.toHexString(requestCode));
                return;
            }
            Fragment frag = (Fragment) this.mFragments.mActive.get(index);
            if (frag == null) {
                Log.w(TAG, "Activity result no fragment exists for index: 0x" + Integer.toHexString(requestCode));
                return;
            } else {
                frag.onActivityResult(GameRequest.TYPE_ALL & requestCode, resultCode, data);
                return;
            }
        }
        super.onActivityResult(requestCode, resultCode, data);
    }

    public void onBackPressed() {
        if (!this.mFragments.popBackStackImmediate()) {
            finish();
        }
    }

    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        this.mFragments.dispatchConfigurationChanged(newConfig);
    }

    protected void onCreate(Bundle savedInstanceState) {
        ArrayList arrayList = null;
        this.mFragments.attachActivity(this, this.mContainer, null);
        if (getLayoutInflater().getFactory() == null) {
            getLayoutInflater().setFactory(this);
        }
        super.onCreate(savedInstanceState);
        NonConfigurationInstances nc = (NonConfigurationInstances) getLastNonConfigurationInstance();
        if (nc != null) {
            this.mAllLoaderManagers = nc.loaders;
        }
        if (savedInstanceState != null) {
            Parcelable p = savedInstanceState.getParcelable(FRAGMENTS_TAG);
            FragmentManagerImpl fragmentManagerImpl = this.mFragments;
            if (nc != null) {
                arrayList = nc.fragments;
            }
            fragmentManagerImpl.restoreAllState(p, arrayList);
        }
        this.mFragments.dispatchCreate();
    }

    public boolean onCreatePanelMenu(int featureId, Menu menu) {
        if (featureId != 0) {
            return super.onCreatePanelMenu(featureId, menu);
        }
        boolean show = super.onCreatePanelMenu(featureId, menu) | this.mFragments.dispatchCreateOptionsMenu(menu, getMenuInflater());
        if (VERSION.SDK_INT >= HONEYCOMB) {
            return show;
        }
        return true;
    }

    public View onCreateView(String name, Context context, AttributeSet attrs) {
        Fragment fragment = null;
        int containerId = 0;
        if (!"fragment".equals(name)) {
            return super.onCreateView(name, context, attrs);
        }
        String fname = attrs.getAttributeValue(null, "class");
        TypedArray a = context.obtainStyledAttributes(attrs, FragmentTag.Fragment);
        if (fname == null) {
            fname = a.getString(0);
        }
        int id = a.getResourceId(MSG_REALLY_STOPPED, -1);
        String tag = a.getString(MSG_RESUME_PENDING);
        a.recycle();
        if (!Fragment.isSupportFragmentClass(this, fname)) {
            return super.onCreateView(name, context, attrs);
        }
        View parent = null;
        if (parent != null) {
            containerId = parent.getId();
        }
        if (containerId == -1 && id == -1 && tag == null) {
            throw new IllegalArgumentException(attrs.getPositionDescription() + ": Must specify unique android:id, android:tag, or have a parent with an id for " + fname);
        }
        if (id != -1) {
            fragment = this.mFragments.findFragmentById(id);
        }
        if (fragment == null && tag != null) {
            fragment = this.mFragments.findFragmentByTag(tag);
        }
        if (fragment == null && containerId != -1) {
            fragment = this.mFragments.findFragmentById(containerId);
        }
        if (FragmentManagerImpl.DEBUG) {
            Log.v(TAG, "onCreateView: id=0x" + Integer.toHexString(id) + " fname=" + fname + " existing=" + fragment);
        }
        if (fragment == null) {
            fragment = Fragment.instantiate(this, fname);
            fragment.mFromLayout = true;
            fragment.mFragmentId = id != 0 ? id : containerId;
            fragment.mContainerId = containerId;
            fragment.mTag = tag;
            fragment.mInLayout = true;
            fragment.mFragmentManager = this.mFragments;
            fragment.onInflate(this, attrs, fragment.mSavedFragmentState);
            this.mFragments.addFragment(fragment, true);
        } else if (fragment.mInLayout) {
            throw new IllegalArgumentException(attrs.getPositionDescription() + ": Duplicate id 0x" + Integer.toHexString(id) + ", tag " + tag + ", or parent id 0x" + Integer.toHexString(containerId) + " with another fragment for " + fname);
        } else {
            fragment.mInLayout = true;
            if (!fragment.mRetaining) {
                fragment.onInflate(this, attrs, fragment.mSavedFragmentState);
            }
            this.mFragments.moveToState(fragment);
        }
        if (fragment.mView == null) {
            throw new IllegalStateException("Fragment " + fname + " did not create a view.");
        }
        if (id != 0) {
            fragment.mView.setId(id);
        }
        if (fragment.mView.getTag() == null) {
            fragment.mView.setTag(tag);
        }
        return fragment.mView;
    }

    protected void onDestroy() {
        super.onDestroy();
        doReallyStop(false);
        this.mFragments.dispatchDestroy();
        if (this.mLoaderManager != null) {
            this.mLoaderManager.doDestroy();
        }
    }

    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (VERSION.SDK_INT >= 5 || keyCode != 4 || event.getRepeatCount() != 0) {
            return super.onKeyDown(keyCode, event);
        }
        onBackPressed();
        return true;
    }

    public void onLowMemory() {
        super.onLowMemory();
        this.mFragments.dispatchLowMemory();
    }

    public boolean onMenuItemSelected(int featureId, MenuItem item) {
        if (super.onMenuItemSelected(featureId, item)) {
            return true;
        }
        switch (featureId) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                return this.mFragments.dispatchOptionsItemSelected(item);
            case Participant.STATUS_UNRESPONSIVE /*6*/:
                return this.mFragments.dispatchContextItemSelected(item);
            default:
                return false;
        }
    }

    public void onPanelClosed(int featureId, Menu menu) {
        switch (featureId) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                this.mFragments.dispatchOptionsMenuClosed(menu);
                break;
        }
        super.onPanelClosed(featureId, menu);
    }

    protected void onPause() {
        super.onPause();
        this.mResumed = false;
        if (this.mHandler.hasMessages(MSG_RESUME_PENDING)) {
            this.mHandler.removeMessages(MSG_RESUME_PENDING);
            onResumeFragments();
        }
        this.mFragments.dispatchPause();
    }

    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        this.mFragments.noteStateNotSaved();
    }

    protected void onResume() {
        super.onResume();
        this.mHandler.sendEmptyMessage(MSG_RESUME_PENDING);
        this.mResumed = true;
        this.mFragments.execPendingActions();
    }

    protected void onPostResume() {
        super.onPostResume();
        this.mHandler.removeMessages(MSG_RESUME_PENDING);
        onResumeFragments();
        this.mFragments.execPendingActions();
    }

    protected void onResumeFragments() {
        this.mFragments.dispatchResume();
    }

    public boolean onPreparePanel(int featureId, View view, Menu menu) {
        if (featureId != 0 || menu == null) {
            return super.onPreparePanel(featureId, view, menu);
        }
        if (this.mOptionsMenuInvalidated) {
            this.mOptionsMenuInvalidated = false;
            menu.clear();
            onCreatePanelMenu(featureId, menu);
        }
        return onPrepareOptionsPanel(view, menu) | this.mFragments.dispatchPrepareOptionsMenu(menu);
    }

    protected boolean onPrepareOptionsPanel(View view, Menu menu) {
        return super.onPreparePanel(0, view, menu);
    }

    public final Object onRetainNonConfigurationInstance() {
        if (this.mStopped) {
            doReallyStop(true);
        }
        Object custom = onRetainCustomNonConfigurationInstance();
        ArrayList<Fragment> fragments = this.mFragments.retainNonConfig();
        boolean retainLoaders = false;
        if (this.mAllLoaderManagers != null) {
            int i;
            int N = this.mAllLoaderManagers.size();
            LoaderManagerImpl[] loaders = new LoaderManagerImpl[N];
            for (i = N - 1; i >= 0; i--) {
                loaders[i] = (LoaderManagerImpl) this.mAllLoaderManagers.valueAt(i);
            }
            for (i = 0; i < N; i += MSG_REALLY_STOPPED) {
                LoaderManagerImpl lm = loaders[i];
                if (lm.mRetaining) {
                    retainLoaders = true;
                } else {
                    lm.doDestroy();
                    this.mAllLoaderManagers.remove(lm.mWho);
                }
            }
        }
        if (fragments == null && !retainLoaders && custom == null) {
            return null;
        }
        Object nci = new NonConfigurationInstances();
        nci.activity = null;
        nci.custom = custom;
        nci.children = null;
        nci.fragments = fragments;
        nci.loaders = this.mAllLoaderManagers;
        return nci;
    }

    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        Parcelable p = this.mFragments.saveAllState();
        if (p != null) {
            outState.putParcelable(FRAGMENTS_TAG, p);
        }
    }

    protected void onStart() {
        super.onStart();
        this.mStopped = false;
        this.mReallyStopped = false;
        this.mHandler.removeMessages(MSG_REALLY_STOPPED);
        if (!this.mCreated) {
            this.mCreated = true;
            this.mFragments.dispatchActivityCreated();
        }
        this.mFragments.noteStateNotSaved();
        this.mFragments.execPendingActions();
        if (!this.mLoadersStarted) {
            this.mLoadersStarted = true;
            if (this.mLoaderManager != null) {
                this.mLoaderManager.doStart();
            } else if (!this.mCheckedForLoaderManager) {
                this.mLoaderManager = getLoaderManager("(root)", this.mLoadersStarted, false);
                if (!(this.mLoaderManager == null || this.mLoaderManager.mStarted)) {
                    this.mLoaderManager.doStart();
                }
            }
            this.mCheckedForLoaderManager = true;
        }
        this.mFragments.dispatchStart();
        if (this.mAllLoaderManagers != null) {
            int i;
            int N = this.mAllLoaderManagers.size();
            LoaderManagerImpl[] loaders = new LoaderManagerImpl[N];
            for (i = N - 1; i >= 0; i--) {
                loaders[i] = (LoaderManagerImpl) this.mAllLoaderManagers.valueAt(i);
            }
            for (i = 0; i < N; i += MSG_REALLY_STOPPED) {
                LoaderManagerImpl lm = loaders[i];
                lm.finishRetain();
                lm.doReportStart();
            }
        }
    }

    protected void onStop() {
        super.onStop();
        this.mStopped = true;
        this.mHandler.sendEmptyMessage(MSG_REALLY_STOPPED);
        this.mFragments.dispatchStop();
    }

    public Object onRetainCustomNonConfigurationInstance() {
        return null;
    }

    public Object getLastCustomNonConfigurationInstance() {
        NonConfigurationInstances nc = (NonConfigurationInstances) getLastNonConfigurationInstance();
        return nc != null ? nc.custom : null;
    }

    public void supportInvalidateOptionsMenu() {
        if (VERSION.SDK_INT >= HONEYCOMB) {
            ActivityCompatHoneycomb.invalidateOptionsMenu(this);
        } else {
            this.mOptionsMenuInvalidated = true;
        }
    }

    public void dump(String prefix, FileDescriptor fd, PrintWriter writer, String[] args) {
        String innerPrefix;
        if (VERSION.SDK_INT >= HONEYCOMB) {
            writer.print(prefix);
            writer.print("Local FragmentActivity ");
            writer.print(Integer.toHexString(System.identityHashCode(this)));
            writer.println(" State:");
            innerPrefix = prefix + "  ";
            writer.print(innerPrefix);
            writer.print("mCreated=");
            writer.print(this.mCreated);
            writer.print("mResumed=");
            writer.print(this.mResumed);
            writer.print(" mStopped=");
            writer.print(this.mStopped);
            writer.print(" mReallyStopped=");
            writer.println(this.mReallyStopped);
            writer.print(innerPrefix);
            writer.print("mLoadersStarted=");
            writer.println(this.mLoadersStarted);
        } else {
            writer.print(prefix);
            writer.print("Local FragmentActivity ");
            writer.print(Integer.toHexString(System.identityHashCode(this)));
            writer.println(" State:");
            innerPrefix = prefix + "  ";
            writer.print(innerPrefix);
            writer.print("mCreated=");
            writer.print(this.mCreated);
            writer.print("mResumed=");
            writer.print(this.mResumed);
            writer.print(" mStopped=");
            writer.print(this.mStopped);
            writer.print(" mReallyStopped=");
            writer.println(this.mReallyStopped);
            writer.print(innerPrefix);
            writer.print("mLoadersStarted=");
            writer.println(this.mLoadersStarted);
        }
        if (this.mLoaderManager != null) {
            writer.print(prefix);
            writer.print("Loader Manager ");
            writer.print(Integer.toHexString(System.identityHashCode(this.mLoaderManager)));
            writer.println(":");
            this.mLoaderManager.dump(prefix + "  ", fd, writer, args);
        }
        this.mFragments.dump(prefix, fd, writer, args);
        writer.print(prefix);
        writer.println("View Hierarchy:");
        dumpViewHierarchy(prefix + "  ", writer, getWindow().getDecorView());
    }

    private static String viewToString(View view) {
        char c;
        char c2 = 'F';
        char c3 = '.';
        StringBuilder out = new StringBuilder(TransportMediator.FLAG_KEY_MEDIA_NEXT);
        out.append(view.getClass().getName());
        out.append('{');
        out.append(Integer.toHexString(System.identityHashCode(view)));
        out.append(' ');
        switch (view.getVisibility()) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                out.append('V');
                break;
            case DetectedActivity.UNKNOWN /*4*/:
                out.append('I');
                break;
            case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                out.append('G');
                break;
            default:
                out.append('.');
                break;
        }
        if (view.isFocusable()) {
            c = 'F';
        } else {
            c = '.';
        }
        out.append(c);
        if (view.isEnabled()) {
            c = 'E';
        } else {
            c = '.';
        }
        out.append(c);
        out.append(view.willNotDraw() ? '.' : 'D');
        if (view.isHorizontalScrollBarEnabled()) {
            c = 'H';
        } else {
            c = '.';
        }
        out.append(c);
        if (view.isVerticalScrollBarEnabled()) {
            c = 'V';
        } else {
            c = '.';
        }
        out.append(c);
        if (view.isClickable()) {
            c = 'C';
        } else {
            c = '.';
        }
        out.append(c);
        if (view.isLongClickable()) {
            c = 'L';
        } else {
            c = '.';
        }
        out.append(c);
        out.append(' ');
        if (!view.isFocused()) {
            c2 = '.';
        }
        out.append(c2);
        if (view.isSelected()) {
            c = 'S';
        } else {
            c = '.';
        }
        out.append(c);
        if (view.isPressed()) {
            c3 = 'P';
        }
        out.append(c3);
        out.append(' ');
        out.append(view.getLeft());
        out.append(',');
        out.append(view.getTop());
        out.append('-');
        out.append(view.getRight());
        out.append(',');
        out.append(view.getBottom());
        int id = view.getId();
        if (id != -1) {
            out.append(" #");
            out.append(Integer.toHexString(id));
            Resources r = view.getResources();
            if (!(id == 0 || r == null)) {
                String pkgname;
                switch (ViewCompat.MEASURED_STATE_MASK & id) {
                    case ViewCompat.MEASURED_STATE_TOO_SMALL /*16777216*/:
                        pkgname = "android";
                        break;
                    case 2130706432:
                        pkgname = "app";
                        break;
                    default:
                        try {
                            pkgname = r.getResourcePackageName(id);
                            break;
                        } catch (NotFoundException e) {
                            break;
                        }
                }
                String typename = r.getResourceTypeName(id);
                String entryname = r.getResourceEntryName(id);
                out.append(" ");
                out.append(pkgname);
                out.append(":");
                out.append(typename);
                out.append("/");
                out.append(entryname);
            }
        }
        out.append("}");
        return out.toString();
    }

    private void dumpViewHierarchy(String prefix, PrintWriter writer, View view) {
        writer.print(prefix);
        if (view == null) {
            writer.println("null");
            return;
        }
        writer.println(viewToString(view));
        if (view instanceof ViewGroup) {
            ViewGroup grp = (ViewGroup) view;
            int N = grp.getChildCount();
            if (N > 0) {
                prefix = prefix + "  ";
                for (int i = 0; i < N; i += MSG_REALLY_STOPPED) {
                    dumpViewHierarchy(prefix, writer, grp.getChildAt(i));
                }
            }
        }
    }

    void doReallyStop(boolean retaining) {
        if (!this.mReallyStopped) {
            this.mReallyStopped = true;
            this.mRetaining = retaining;
            this.mHandler.removeMessages(MSG_REALLY_STOPPED);
            onReallyStop();
        }
    }

    void onReallyStop() {
        if (this.mLoadersStarted) {
            this.mLoadersStarted = false;
            if (this.mLoaderManager != null) {
                if (this.mRetaining) {
                    this.mLoaderManager.doRetain();
                } else {
                    this.mLoaderManager.doStop();
                }
            }
        }
        this.mFragments.dispatchReallyStop();
    }

    public void onAttachFragment(Fragment fragment) {
    }

    public FragmentManager getSupportFragmentManager() {
        return this.mFragments;
    }

    public void startActivityForResult(Intent intent, int requestCode) {
        if (requestCode == -1 || (SupportMenu.CATEGORY_MASK & requestCode) == 0) {
            super.startActivityForResult(intent, requestCode);
            return;
        }
        throw new IllegalArgumentException("Can only use lower 16 bits for requestCode");
    }

    public void startActivityFromFragment(Fragment fragment, Intent intent, int requestCode) {
        if (requestCode == -1) {
            super.startActivityForResult(intent, -1);
        } else if ((SupportMenu.CATEGORY_MASK & requestCode) != 0) {
            throw new IllegalArgumentException("Can only use lower 16 bits for requestCode");
        } else {
            super.startActivityForResult(intent, ((fragment.mIndex + MSG_REALLY_STOPPED) << 16) + (GameRequest.TYPE_ALL & requestCode));
        }
    }

    void invalidateSupportFragment(String who) {
        if (this.mAllLoaderManagers != null) {
            LoaderManagerImpl lm = (LoaderManagerImpl) this.mAllLoaderManagers.get(who);
            if (lm != null && !lm.mRetaining) {
                lm.doDestroy();
                this.mAllLoaderManagers.remove(who);
            }
        }
    }

    public LoaderManager getSupportLoaderManager() {
        if (this.mLoaderManager != null) {
            return this.mLoaderManager;
        }
        this.mCheckedForLoaderManager = true;
        this.mLoaderManager = getLoaderManager("(root)", this.mLoadersStarted, true);
        return this.mLoaderManager;
    }

    LoaderManagerImpl getLoaderManager(String who, boolean started, boolean create) {
        if (this.mAllLoaderManagers == null) {
            this.mAllLoaderManagers = new SimpleArrayMap();
        }
        LoaderManagerImpl lm = (LoaderManagerImpl) this.mAllLoaderManagers.get(who);
        if (lm != null) {
            lm.updateActivity(this);
            return lm;
        } else if (!create) {
            return lm;
        } else {
            lm = new LoaderManagerImpl(who, this, started);
            this.mAllLoaderManagers.put(who, lm);
            return lm;
        }
    }
}
