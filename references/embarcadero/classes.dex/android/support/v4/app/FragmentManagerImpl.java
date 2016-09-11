package android.support.v4.app;

import android.content.Context;
import android.content.res.Configuration;
import android.os.Build.VERSION;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.os.Parcelable;
import android.support.v4.app.Fragment.SavedState;
import android.support.v4.app.FragmentManager.BackStackEntry;
import android.support.v4.app.FragmentManager.OnBackStackChangedListener;
import android.support.v4.media.TransportMediator;
import android.support.v4.util.DebugUtils;
import android.support.v4.util.LogWriter;
import android.util.Log;
import android.util.SparseArray;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.animation.AccelerateInterpolator;
import android.view.animation.AlphaAnimation;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.AnimationSet;
import android.view.animation.AnimationUtils;
import android.view.animation.DecelerateInterpolator;
import android.view.animation.Interpolator;
import android.view.animation.ScaleAnimation;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.io.FileDescriptor;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/* compiled from: FragmentManager */
final class FragmentManagerImpl extends FragmentManager {
    static final Interpolator ACCELERATE_CUBIC;
    static final Interpolator ACCELERATE_QUINT;
    static final int ANIM_DUR = 220;
    public static final int ANIM_STYLE_CLOSE_ENTER = 3;
    public static final int ANIM_STYLE_CLOSE_EXIT = 4;
    public static final int ANIM_STYLE_FADE_ENTER = 5;
    public static final int ANIM_STYLE_FADE_EXIT = 6;
    public static final int ANIM_STYLE_OPEN_ENTER = 1;
    public static final int ANIM_STYLE_OPEN_EXIT = 2;
    static boolean DEBUG = false;
    static final Interpolator DECELERATE_CUBIC;
    static final Interpolator DECELERATE_QUINT;
    static final boolean HONEYCOMB;
    static final String TAG = "FragmentManager";
    static final String TARGET_REQUEST_CODE_STATE_TAG = "android:target_req_state";
    static final String TARGET_STATE_TAG = "android:target_state";
    static final String USER_VISIBLE_HINT_TAG = "android:user_visible_hint";
    static final String VIEW_STATE_TAG = "android:view_state";
    ArrayList<Fragment> mActive;
    FragmentActivity mActivity;
    ArrayList<Fragment> mAdded;
    ArrayList<Integer> mAvailBackStackIndices;
    ArrayList<Integer> mAvailIndices;
    ArrayList<BackStackRecord> mBackStack;
    ArrayList<OnBackStackChangedListener> mBackStackChangeListeners;
    ArrayList<BackStackRecord> mBackStackIndices;
    FragmentContainer mContainer;
    ArrayList<Fragment> mCreatedMenus;
    int mCurState;
    boolean mDestroyed;
    Runnable mExecCommit;
    boolean mExecutingActions;
    boolean mHavePendingDeferredStart;
    boolean mNeedMenuInvalidate;
    String mNoTransactionsBecause;
    Fragment mParent;
    ArrayList<Runnable> mPendingActions;
    SparseArray<Parcelable> mStateArray;
    Bundle mStateBundle;
    boolean mStateSaved;
    Runnable[] mTmpActions;

    /* compiled from: FragmentManager */
    /* renamed from: android.support.v4.app.FragmentManagerImpl.3 */
    class AnonymousClass3 implements Runnable {
        final /* synthetic */ int val$flags;
        final /* synthetic */ String val$name;

        AnonymousClass3(String str, int i) {
            this.val$name = str;
            this.val$flags = i;
        }

        public void run() {
            FragmentManagerImpl.this.popBackStackState(FragmentManagerImpl.this.mActivity.mHandler, this.val$name, -1, this.val$flags);
        }
    }

    /* compiled from: FragmentManager */
    /* renamed from: android.support.v4.app.FragmentManagerImpl.4 */
    class AnonymousClass4 implements Runnable {
        final /* synthetic */ int val$flags;
        final /* synthetic */ int val$id;

        AnonymousClass4(int i, int i2) {
            this.val$id = i;
            this.val$flags = i2;
        }

        public void run() {
            FragmentManagerImpl.this.popBackStackState(FragmentManagerImpl.this.mActivity.mHandler, null, this.val$id, this.val$flags);
        }
    }

    /* compiled from: FragmentManager */
    /* renamed from: android.support.v4.app.FragmentManagerImpl.5 */
    class AnonymousClass5 implements AnimationListener {
        final /* synthetic */ Fragment val$fragment;

        AnonymousClass5(Fragment fragment) {
            this.val$fragment = fragment;
        }

        public void onAnimationEnd(Animation animation) {
            if (this.val$fragment.mAnimatingAway != null) {
                this.val$fragment.mAnimatingAway = null;
                FragmentManagerImpl.this.moveToState(this.val$fragment, this.val$fragment.mStateAfterAnimating, 0, 0, FragmentManagerImpl.HONEYCOMB);
            }
        }

        public void onAnimationRepeat(Animation animation) {
        }

        public void onAnimationStart(Animation animation) {
        }
    }

    FragmentManagerImpl() {
        this.mCurState = 0;
        this.mStateBundle = null;
        this.mStateArray = null;
        this.mExecCommit = new Runnable() {
            public void run() {
                FragmentManagerImpl.this.execPendingActions();
            }
        };
    }

    static {
        boolean z = HONEYCOMB;
        DEBUG = HONEYCOMB;
        if (VERSION.SDK_INT >= 11) {
            z = true;
        }
        HONEYCOMB = z;
        DECELERATE_QUINT = new DecelerateInterpolator(2.5f);
        DECELERATE_CUBIC = new DecelerateInterpolator(1.5f);
        ACCELERATE_QUINT = new AccelerateInterpolator(2.5f);
        ACCELERATE_CUBIC = new AccelerateInterpolator(1.5f);
    }

    private void throwException(RuntimeException ex) {
        Log.e(TAG, ex.getMessage());
        Log.e(TAG, "Activity state:");
        PrintWriter pw = new PrintWriter(new LogWriter(TAG));
        if (this.mActivity != null) {
            try {
                this.mActivity.dump("  ", null, pw, new String[0]);
            } catch (Exception e) {
                Log.e(TAG, "Failed dumping state", e);
            }
        } else {
            try {
                dump("  ", null, pw, new String[0]);
            } catch (Exception e2) {
                Log.e(TAG, "Failed dumping state", e2);
            }
        }
        throw ex;
    }

    public FragmentTransaction beginTransaction() {
        return new BackStackRecord(this);
    }

    public boolean executePendingTransactions() {
        return execPendingActions();
    }

    public void popBackStack() {
        enqueueAction(new Runnable() {
            public void run() {
                FragmentManagerImpl.this.popBackStackState(FragmentManagerImpl.this.mActivity.mHandler, null, -1, 0);
            }
        }, HONEYCOMB);
    }

    public boolean popBackStackImmediate() {
        checkStateLoss();
        executePendingTransactions();
        return popBackStackState(this.mActivity.mHandler, null, -1, 0);
    }

    public void popBackStack(String name, int flags) {
        enqueueAction(new AnonymousClass3(name, flags), HONEYCOMB);
    }

    public boolean popBackStackImmediate(String name, int flags) {
        checkStateLoss();
        executePendingTransactions();
        return popBackStackState(this.mActivity.mHandler, name, -1, flags);
    }

    public void popBackStack(int id, int flags) {
        if (id < 0) {
            throw new IllegalArgumentException("Bad id: " + id);
        }
        enqueueAction(new AnonymousClass4(id, flags), HONEYCOMB);
    }

    public boolean popBackStackImmediate(int id, int flags) {
        checkStateLoss();
        executePendingTransactions();
        if (id >= 0) {
            return popBackStackState(this.mActivity.mHandler, null, id, flags);
        }
        throw new IllegalArgumentException("Bad id: " + id);
    }

    public int getBackStackEntryCount() {
        return this.mBackStack != null ? this.mBackStack.size() : 0;
    }

    public BackStackEntry getBackStackEntryAt(int index) {
        return (BackStackEntry) this.mBackStack.get(index);
    }

    public void addOnBackStackChangedListener(OnBackStackChangedListener listener) {
        if (this.mBackStackChangeListeners == null) {
            this.mBackStackChangeListeners = new ArrayList();
        }
        this.mBackStackChangeListeners.add(listener);
    }

    public void removeOnBackStackChangedListener(OnBackStackChangedListener listener) {
        if (this.mBackStackChangeListeners != null) {
            this.mBackStackChangeListeners.remove(listener);
        }
    }

    public void putFragment(Bundle bundle, String key, Fragment fragment) {
        if (fragment.mIndex < 0) {
            throwException(new IllegalStateException("Fragment " + fragment + " is not currently in the FragmentManager"));
        }
        bundle.putInt(key, fragment.mIndex);
    }

    public Fragment getFragment(Bundle bundle, String key) {
        int index = bundle.getInt(key, -1);
        if (index == -1) {
            return null;
        }
        if (index >= this.mActive.size()) {
            throwException(new IllegalStateException("Fragement no longer exists for key " + key + ": index " + index));
        }
        Fragment f = (Fragment) this.mActive.get(index);
        if (f != null) {
            return f;
        }
        throwException(new IllegalStateException("Fragement no longer exists for key " + key + ": index " + index));
        return f;
    }

    public List<Fragment> getFragments() {
        return this.mActive;
    }

    public SavedState saveFragmentInstanceState(Fragment fragment) {
        if (fragment.mIndex < 0) {
            throwException(new IllegalStateException("Fragment " + fragment + " is not currently in the FragmentManager"));
        }
        if (fragment.mState <= 0) {
            return null;
        }
        Bundle result = saveFragmentBasicState(fragment);
        if (result != null) {
            return new SavedState(result);
        }
        return null;
    }

    public boolean isDestroyed() {
        return this.mDestroyed;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder(TransportMediator.FLAG_KEY_MEDIA_NEXT);
        sb.append("FragmentManager{");
        sb.append(Integer.toHexString(System.identityHashCode(this)));
        sb.append(" in ");
        if (this.mParent != null) {
            DebugUtils.buildShortClassTag(this.mParent, sb);
        } else {
            DebugUtils.buildShortClassTag(this.mActivity, sb);
        }
        sb.append("}}");
        return sb.toString();
    }

    public void dump(String prefix, FileDescriptor fd, PrintWriter writer, String[] args) {
        int N;
        int i;
        Fragment f;
        String innerPrefix = prefix + "    ";
        if (this.mActive != null) {
            N = this.mActive.size();
            if (N > 0) {
                writer.print(prefix);
                writer.print("Active Fragments in ");
                writer.print(Integer.toHexString(System.identityHashCode(this)));
                writer.println(":");
                for (i = 0; i < N; i += ANIM_STYLE_OPEN_ENTER) {
                    f = (Fragment) this.mActive.get(i);
                    writer.print(prefix);
                    writer.print("  #");
                    writer.print(i);
                    writer.print(": ");
                    writer.println(f);
                    if (f != null) {
                        f.dump(innerPrefix, fd, writer, args);
                    }
                }
            }
        }
        if (this.mAdded != null) {
            N = this.mAdded.size();
            if (N > 0) {
                writer.print(prefix);
                writer.println("Added Fragments:");
                for (i = 0; i < N; i += ANIM_STYLE_OPEN_ENTER) {
                    f = (Fragment) this.mAdded.get(i);
                    writer.print(prefix);
                    writer.print("  #");
                    writer.print(i);
                    writer.print(": ");
                    writer.println(f.toString());
                }
            }
        }
        if (this.mCreatedMenus != null) {
            N = this.mCreatedMenus.size();
            if (N > 0) {
                writer.print(prefix);
                writer.println("Fragments Created Menus:");
                for (i = 0; i < N; i += ANIM_STYLE_OPEN_ENTER) {
                    f = (Fragment) this.mCreatedMenus.get(i);
                    writer.print(prefix);
                    writer.print("  #");
                    writer.print(i);
                    writer.print(": ");
                    writer.println(f.toString());
                }
            }
        }
        if (this.mBackStack != null) {
            N = this.mBackStack.size();
            if (N > 0) {
                writer.print(prefix);
                writer.println("Back Stack:");
                for (i = 0; i < N; i += ANIM_STYLE_OPEN_ENTER) {
                    BackStackRecord bs = (BackStackRecord) this.mBackStack.get(i);
                    writer.print(prefix);
                    writer.print("  #");
                    writer.print(i);
                    writer.print(": ");
                    writer.println(bs.toString());
                    bs.dump(innerPrefix, fd, writer, args);
                }
            }
        }
        synchronized (this) {
            if (this.mBackStackIndices != null) {
                N = this.mBackStackIndices.size();
                if (N > 0) {
                    writer.print(prefix);
                    writer.println("Back Stack Indices:");
                    for (i = 0; i < N; i += ANIM_STYLE_OPEN_ENTER) {
                        bs = (BackStackRecord) this.mBackStackIndices.get(i);
                        writer.print(prefix);
                        writer.print("  #");
                        writer.print(i);
                        writer.print(": ");
                        writer.println(bs);
                    }
                }
            }
            if (this.mAvailBackStackIndices != null && this.mAvailBackStackIndices.size() > 0) {
                writer.print(prefix);
                writer.print("mAvailBackStackIndices: ");
                writer.println(Arrays.toString(this.mAvailBackStackIndices.toArray()));
            }
        }
        if (this.mPendingActions != null) {
            N = this.mPendingActions.size();
            if (N > 0) {
                writer.print(prefix);
                writer.println("Pending Actions:");
                for (i = 0; i < N; i += ANIM_STYLE_OPEN_ENTER) {
                    Runnable r = (Runnable) this.mPendingActions.get(i);
                    writer.print(prefix);
                    writer.print("  #");
                    writer.print(i);
                    writer.print(": ");
                    writer.println(r);
                }
            }
        }
        writer.print(prefix);
        writer.println("FragmentManager misc state:");
        writer.print(prefix);
        writer.print("  mActivity=");
        writer.println(this.mActivity);
        writer.print(prefix);
        writer.print("  mContainer=");
        writer.println(this.mContainer);
        if (this.mParent != null) {
            writer.print(prefix);
            writer.print("  mParent=");
            writer.println(this.mParent);
        }
        writer.print(prefix);
        writer.print("  mCurState=");
        writer.print(this.mCurState);
        writer.print(" mStateSaved=");
        writer.print(this.mStateSaved);
        writer.print(" mDestroyed=");
        writer.println(this.mDestroyed);
        if (this.mNeedMenuInvalidate) {
            writer.print(prefix);
            writer.print("  mNeedMenuInvalidate=");
            writer.println(this.mNeedMenuInvalidate);
        }
        if (this.mNoTransactionsBecause != null) {
            writer.print(prefix);
            writer.print("  mNoTransactionsBecause=");
            writer.println(this.mNoTransactionsBecause);
        }
        if (this.mAvailIndices != null && this.mAvailIndices.size() > 0) {
            writer.print(prefix);
            writer.print("  mAvailIndices: ");
            writer.println(Arrays.toString(this.mAvailIndices.toArray()));
        }
    }

    static Animation makeOpenCloseAnimation(Context context, float startScale, float endScale, float startAlpha, float endAlpha) {
        AnimationSet set = new AnimationSet(HONEYCOMB);
        ScaleAnimation scale = new ScaleAnimation(startScale, endScale, startScale, endScale, ANIM_STYLE_OPEN_ENTER, 0.5f, ANIM_STYLE_OPEN_ENTER, 0.5f);
        scale.setInterpolator(DECELERATE_QUINT);
        scale.setDuration(220);
        set.addAnimation(scale);
        AlphaAnimation alpha = new AlphaAnimation(startAlpha, endAlpha);
        alpha.setInterpolator(DECELERATE_CUBIC);
        alpha.setDuration(220);
        set.addAnimation(alpha);
        return set;
    }

    static Animation makeFadeAnimation(Context context, float start, float end) {
        AlphaAnimation anim = new AlphaAnimation(start, end);
        anim.setInterpolator(DECELERATE_CUBIC);
        anim.setDuration(220);
        return anim;
    }

    Animation loadAnimation(Fragment fragment, int transit, boolean enter, int transitionStyle) {
        Animation animObj = fragment.onCreateAnimation(transit, enter, fragment.mNextAnim);
        if (animObj != null) {
            return animObj;
        }
        if (fragment.mNextAnim != 0) {
            Animation anim = AnimationUtils.loadAnimation(this.mActivity, fragment.mNextAnim);
            if (anim != null) {
                return anim;
            }
        }
        if (transit == 0) {
            return null;
        }
        int styleIndex = transitToStyleIndex(transit, enter);
        if (styleIndex < 0) {
            return null;
        }
        switch (styleIndex) {
            case ANIM_STYLE_OPEN_ENTER /*1*/:
                return makeOpenCloseAnimation(this.mActivity, 1.125f, 1.0f, 0.0f, 1.0f);
            case ANIM_STYLE_OPEN_EXIT /*2*/:
                return makeOpenCloseAnimation(this.mActivity, 1.0f, 0.975f, 1.0f, 0.0f);
            case ANIM_STYLE_CLOSE_ENTER /*3*/:
                return makeOpenCloseAnimation(this.mActivity, 0.975f, 1.0f, 0.0f, 1.0f);
            case ANIM_STYLE_CLOSE_EXIT /*4*/:
                return makeOpenCloseAnimation(this.mActivity, 1.0f, 1.075f, 1.0f, 0.0f);
            case ANIM_STYLE_FADE_ENTER /*5*/:
                return makeFadeAnimation(this.mActivity, 0.0f, 1.0f);
            case ANIM_STYLE_FADE_EXIT /*6*/:
                return makeFadeAnimation(this.mActivity, 1.0f, 0.0f);
            default:
                if (transitionStyle == 0 && this.mActivity.getWindow() != null) {
                    transitionStyle = this.mActivity.getWindow().getAttributes().windowAnimations;
                }
                if (transitionStyle == 0) {
                    return null;
                }
                return null;
        }
    }

    public void performPendingDeferredStart(Fragment f) {
        if (!f.mDeferStart) {
            return;
        }
        if (this.mExecutingActions) {
            this.mHavePendingDeferredStart = true;
            return;
        }
        f.mDeferStart = HONEYCOMB;
        moveToState(f, this.mCurState, 0, 0, HONEYCOMB);
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    void moveToState(Fragment f, int newState, int transit, int transitionStyle, boolean keepActive) {
        if ((!f.mAdded || f.mDetached) && newState > ANIM_STYLE_OPEN_ENTER) {
            newState = ANIM_STYLE_OPEN_ENTER;
        }
        if (f.mRemoving && newState > f.mState) {
            newState = f.mState;
        }
        if (f.mDeferStart && f.mState < ANIM_STYLE_CLOSE_EXIT && newState > ANIM_STYLE_CLOSE_ENTER) {
            newState = ANIM_STYLE_CLOSE_ENTER;
        }
        if (f.mState >= newState) {
            if (f.mState > newState) {
                switch (f.mState) {
                    case ANIM_STYLE_OPEN_ENTER /*1*/:
                        break;
                    case ANIM_STYLE_OPEN_EXIT /*2*/:
                        break;
                    case ANIM_STYLE_CLOSE_ENTER /*3*/:
                        break;
                    case ANIM_STYLE_CLOSE_EXIT /*4*/:
                        break;
                    case ANIM_STYLE_FADE_ENTER /*5*/:
                        if (newState < ANIM_STYLE_FADE_ENTER) {
                            if (DEBUG) {
                                Log.v(TAG, "movefrom RESUMED: " + f);
                            }
                            f.performPause();
                            f.mResumed = HONEYCOMB;
                            break;
                        }
                        break;
                    default:
                        break;
                }
            }
        } else if (!f.mFromLayout || f.mInLayout) {
            if (f.mAnimatingAway != null) {
                f.mAnimatingAway = null;
                moveToState(f, f.mStateAfterAnimating, 0, 0, true);
            }
            switch (f.mState) {
                case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                    if (DEBUG) {
                        Log.v(TAG, "moveto CREATED: " + f);
                    }
                    if (f.mSavedFragmentState != null) {
                        f.mSavedViewState = f.mSavedFragmentState.getSparseParcelableArray(VIEW_STATE_TAG);
                        f.mTarget = getFragment(f.mSavedFragmentState, TARGET_STATE_TAG);
                        if (f.mTarget != null) {
                            f.mTargetRequestCode = f.mSavedFragmentState.getInt(TARGET_REQUEST_CODE_STATE_TAG, 0);
                        }
                        f.mUserVisibleHint = f.mSavedFragmentState.getBoolean(USER_VISIBLE_HINT_TAG, true);
                        if (!f.mUserVisibleHint) {
                            f.mDeferStart = true;
                            if (newState > ANIM_STYLE_CLOSE_ENTER) {
                                newState = ANIM_STYLE_CLOSE_ENTER;
                            }
                        }
                    }
                    f.mActivity = this.mActivity;
                    f.mParentFragment = this.mParent;
                    f.mFragmentManager = this.mParent != null ? this.mParent.mChildFragmentManager : this.mActivity.mFragments;
                    f.mCalled = HONEYCOMB;
                    f.onAttach(this.mActivity);
                    if (f.mCalled) {
                        if (f.mParentFragment == null) {
                            this.mActivity.onAttachFragment(f);
                        }
                        if (!f.mRetaining) {
                            f.performCreate(f.mSavedFragmentState);
                        }
                        f.mRetaining = HONEYCOMB;
                        if (f.mFromLayout) {
                            f.mView = f.performCreateView(f.getLayoutInflater(f.mSavedFragmentState), null, f.mSavedFragmentState);
                            if (f.mView == null) {
                                f.mInnerView = null;
                                break;
                            }
                            f.mInnerView = f.mView;
                            f.mView = NoSaveStateFrameLayout.wrap(f.mView);
                            if (f.mHidden) {
                                f.mView.setVisibility(8);
                            }
                            f.onViewCreated(f.mView, f.mSavedFragmentState);
                            break;
                        }
                    }
                    throw new SuperNotCalledException("Fragment " + f + " did not call through to super.onAttach()");
                    break;
                case ANIM_STYLE_OPEN_ENTER /*1*/:
                    break;
                case ANIM_STYLE_OPEN_EXIT /*2*/:
                case ANIM_STYLE_CLOSE_ENTER /*3*/:
                    break;
                case ANIM_STYLE_CLOSE_EXIT /*4*/:
                    break;
            }
        } else {
            return;
        }
        f.mState = newState;
    }

    void moveToState(Fragment f) {
        moveToState(f, this.mCurState, 0, 0, HONEYCOMB);
    }

    void moveToState(int newState, boolean always) {
        moveToState(newState, 0, 0, always);
    }

    void moveToState(int newState, int transit, int transitStyle, boolean always) {
        if (this.mActivity == null && newState != 0) {
            throw new IllegalStateException("No activity");
        } else if (always || this.mCurState != newState) {
            this.mCurState = newState;
            if (this.mActive != null) {
                boolean loadersRunning = HONEYCOMB;
                for (int i = 0; i < this.mActive.size(); i += ANIM_STYLE_OPEN_ENTER) {
                    Fragment f = (Fragment) this.mActive.get(i);
                    if (f != null) {
                        moveToState(f, newState, transit, transitStyle, HONEYCOMB);
                        if (f.mLoaderManager != null) {
                            loadersRunning |= f.mLoaderManager.hasRunningLoaders();
                        }
                    }
                }
                if (!loadersRunning) {
                    startPendingDeferredFragments();
                }
                if (this.mNeedMenuInvalidate && this.mActivity != null && this.mCurState == ANIM_STYLE_FADE_ENTER) {
                    this.mActivity.supportInvalidateOptionsMenu();
                    this.mNeedMenuInvalidate = HONEYCOMB;
                }
            }
        }
    }

    void startPendingDeferredFragments() {
        if (this.mActive != null) {
            for (int i = 0; i < this.mActive.size(); i += ANIM_STYLE_OPEN_ENTER) {
                Fragment f = (Fragment) this.mActive.get(i);
                if (f != null) {
                    performPendingDeferredStart(f);
                }
            }
        }
    }

    void makeActive(Fragment f) {
        if (f.mIndex < 0) {
            if (this.mAvailIndices == null || this.mAvailIndices.size() <= 0) {
                if (this.mActive == null) {
                    this.mActive = new ArrayList();
                }
                f.setIndex(this.mActive.size(), this.mParent);
                this.mActive.add(f);
            } else {
                f.setIndex(((Integer) this.mAvailIndices.remove(this.mAvailIndices.size() - 1)).intValue(), this.mParent);
                this.mActive.set(f.mIndex, f);
            }
            if (DEBUG) {
                Log.v(TAG, "Allocated fragment index " + f);
            }
        }
    }

    void makeInactive(Fragment f) {
        if (f.mIndex >= 0) {
            if (DEBUG) {
                Log.v(TAG, "Freeing fragment index " + f);
            }
            this.mActive.set(f.mIndex, null);
            if (this.mAvailIndices == null) {
                this.mAvailIndices = new ArrayList();
            }
            this.mAvailIndices.add(Integer.valueOf(f.mIndex));
            this.mActivity.invalidateSupportFragment(f.mWho);
            f.initState();
        }
    }

    public void addFragment(Fragment fragment, boolean moveToStateNow) {
        if (this.mAdded == null) {
            this.mAdded = new ArrayList();
        }
        if (DEBUG) {
            Log.v(TAG, "add: " + fragment);
        }
        makeActive(fragment);
        if (!fragment.mDetached) {
            if (this.mAdded.contains(fragment)) {
                throw new IllegalStateException("Fragment already added: " + fragment);
            }
            this.mAdded.add(fragment);
            fragment.mAdded = true;
            fragment.mRemoving = HONEYCOMB;
            if (fragment.mHasMenu && fragment.mMenuVisible) {
                this.mNeedMenuInvalidate = true;
            }
            if (moveToStateNow) {
                moveToState(fragment);
            }
        }
    }

    public void removeFragment(Fragment fragment, int transition, int transitionStyle) {
        boolean inactive;
        if (DEBUG) {
            Log.v(TAG, "remove: " + fragment + " nesting=" + fragment.mBackStackNesting);
        }
        if (fragment.isInBackStack()) {
            inactive = HONEYCOMB;
        } else {
            inactive = true;
        }
        if (!fragment.mDetached || inactive) {
            int i;
            if (this.mAdded != null) {
                this.mAdded.remove(fragment);
            }
            if (fragment.mHasMenu && fragment.mMenuVisible) {
                this.mNeedMenuInvalidate = true;
            }
            fragment.mAdded = HONEYCOMB;
            fragment.mRemoving = true;
            if (inactive) {
                i = 0;
            } else {
                i = ANIM_STYLE_OPEN_ENTER;
            }
            moveToState(fragment, i, transition, transitionStyle, HONEYCOMB);
        }
    }

    public void hideFragment(Fragment fragment, int transition, int transitionStyle) {
        if (DEBUG) {
            Log.v(TAG, "hide: " + fragment);
        }
        if (!fragment.mHidden) {
            fragment.mHidden = true;
            if (fragment.mView != null) {
                Animation anim = loadAnimation(fragment, transition, HONEYCOMB, transitionStyle);
                if (anim != null) {
                    fragment.mView.startAnimation(anim);
                }
                fragment.mView.setVisibility(8);
            }
            if (fragment.mAdded && fragment.mHasMenu && fragment.mMenuVisible) {
                this.mNeedMenuInvalidate = true;
            }
            fragment.onHiddenChanged(true);
        }
    }

    public void showFragment(Fragment fragment, int transition, int transitionStyle) {
        if (DEBUG) {
            Log.v(TAG, "show: " + fragment);
        }
        if (fragment.mHidden) {
            fragment.mHidden = HONEYCOMB;
            if (fragment.mView != null) {
                Animation anim = loadAnimation(fragment, transition, true, transitionStyle);
                if (anim != null) {
                    fragment.mView.startAnimation(anim);
                }
                fragment.mView.setVisibility(0);
            }
            if (fragment.mAdded && fragment.mHasMenu && fragment.mMenuVisible) {
                this.mNeedMenuInvalidate = true;
            }
            fragment.onHiddenChanged(HONEYCOMB);
        }
    }

    public void detachFragment(Fragment fragment, int transition, int transitionStyle) {
        if (DEBUG) {
            Log.v(TAG, "detach: " + fragment);
        }
        if (!fragment.mDetached) {
            fragment.mDetached = true;
            if (fragment.mAdded) {
                if (this.mAdded != null) {
                    if (DEBUG) {
                        Log.v(TAG, "remove from detach: " + fragment);
                    }
                    this.mAdded.remove(fragment);
                }
                if (fragment.mHasMenu && fragment.mMenuVisible) {
                    this.mNeedMenuInvalidate = true;
                }
                fragment.mAdded = HONEYCOMB;
                moveToState(fragment, ANIM_STYLE_OPEN_ENTER, transition, transitionStyle, HONEYCOMB);
            }
        }
    }

    public void attachFragment(Fragment fragment, int transition, int transitionStyle) {
        if (DEBUG) {
            Log.v(TAG, "attach: " + fragment);
        }
        if (fragment.mDetached) {
            fragment.mDetached = HONEYCOMB;
            if (!fragment.mAdded) {
                if (this.mAdded == null) {
                    this.mAdded = new ArrayList();
                }
                if (this.mAdded.contains(fragment)) {
                    throw new IllegalStateException("Fragment already added: " + fragment);
                }
                if (DEBUG) {
                    Log.v(TAG, "add from attach: " + fragment);
                }
                this.mAdded.add(fragment);
                fragment.mAdded = true;
                if (fragment.mHasMenu && fragment.mMenuVisible) {
                    this.mNeedMenuInvalidate = true;
                }
                moveToState(fragment, this.mCurState, transition, transitionStyle, HONEYCOMB);
            }
        }
    }

    public Fragment findFragmentById(int id) {
        int i;
        Fragment f;
        if (this.mAdded != null) {
            for (i = this.mAdded.size() - 1; i >= 0; i--) {
                f = (Fragment) this.mAdded.get(i);
                if (f != null && f.mFragmentId == id) {
                    return f;
                }
            }
        }
        if (this.mActive != null) {
            for (i = this.mActive.size() - 1; i >= 0; i--) {
                f = (Fragment) this.mActive.get(i);
                if (f != null && f.mFragmentId == id) {
                    return f;
                }
            }
        }
        return null;
    }

    public Fragment findFragmentByTag(String tag) {
        int i;
        Fragment f;
        if (!(this.mAdded == null || tag == null)) {
            for (i = this.mAdded.size() - 1; i >= 0; i--) {
                f = (Fragment) this.mAdded.get(i);
                if (f != null && tag.equals(f.mTag)) {
                    return f;
                }
            }
        }
        if (!(this.mActive == null || tag == null)) {
            for (i = this.mActive.size() - 1; i >= 0; i--) {
                f = (Fragment) this.mActive.get(i);
                if (f != null && tag.equals(f.mTag)) {
                    return f;
                }
            }
        }
        return null;
    }

    public Fragment findFragmentByWho(String who) {
        if (!(this.mActive == null || who == null)) {
            for (int i = this.mActive.size() - 1; i >= 0; i--) {
                Fragment f = (Fragment) this.mActive.get(i);
                if (f != null) {
                    f = f.findFragmentByWho(who);
                    if (f != null) {
                        return f;
                    }
                }
            }
        }
        return null;
    }

    private void checkStateLoss() {
        if (this.mStateSaved) {
            throw new IllegalStateException("Can not perform this action after onSaveInstanceState");
        } else if (this.mNoTransactionsBecause != null) {
            throw new IllegalStateException("Can not perform this action inside of " + this.mNoTransactionsBecause);
        }
    }

    public void enqueueAction(Runnable action, boolean allowStateLoss) {
        if (!allowStateLoss) {
            checkStateLoss();
        }
        synchronized (this) {
            if (this.mDestroyed || this.mActivity == null) {
                throw new IllegalStateException("Activity has been destroyed");
            }
            if (this.mPendingActions == null) {
                this.mPendingActions = new ArrayList();
            }
            this.mPendingActions.add(action);
            if (this.mPendingActions.size() == ANIM_STYLE_OPEN_ENTER) {
                this.mActivity.mHandler.removeCallbacks(this.mExecCommit);
                this.mActivity.mHandler.post(this.mExecCommit);
            }
        }
    }

    public int allocBackStackIndex(BackStackRecord bse) {
        synchronized (this) {
            int index;
            if (this.mAvailBackStackIndices == null || this.mAvailBackStackIndices.size() <= 0) {
                if (this.mBackStackIndices == null) {
                    this.mBackStackIndices = new ArrayList();
                }
                index = this.mBackStackIndices.size();
                if (DEBUG) {
                    Log.v(TAG, "Setting back stack index " + index + " to " + bse);
                }
                this.mBackStackIndices.add(bse);
                return index;
            }
            index = ((Integer) this.mAvailBackStackIndices.remove(this.mAvailBackStackIndices.size() - 1)).intValue();
            if (DEBUG) {
                Log.v(TAG, "Adding back stack index " + index + " with " + bse);
            }
            this.mBackStackIndices.set(index, bse);
            return index;
        }
    }

    public void setBackStackIndex(int index, BackStackRecord bse) {
        synchronized (this) {
            if (this.mBackStackIndices == null) {
                this.mBackStackIndices = new ArrayList();
            }
            int N = this.mBackStackIndices.size();
            if (index < N) {
                if (DEBUG) {
                    Log.v(TAG, "Setting back stack index " + index + " to " + bse);
                }
                this.mBackStackIndices.set(index, bse);
            } else {
                while (N < index) {
                    this.mBackStackIndices.add(null);
                    if (this.mAvailBackStackIndices == null) {
                        this.mAvailBackStackIndices = new ArrayList();
                    }
                    if (DEBUG) {
                        Log.v(TAG, "Adding available back stack index " + N);
                    }
                    this.mAvailBackStackIndices.add(Integer.valueOf(N));
                    N += ANIM_STYLE_OPEN_ENTER;
                }
                if (DEBUG) {
                    Log.v(TAG, "Adding back stack index " + index + " with " + bse);
                }
                this.mBackStackIndices.add(bse);
            }
        }
    }

    public void freeBackStackIndex(int index) {
        synchronized (this) {
            this.mBackStackIndices.set(index, null);
            if (this.mAvailBackStackIndices == null) {
                this.mAvailBackStackIndices = new ArrayList();
            }
            if (DEBUG) {
                Log.v(TAG, "Freeing back stack index " + index);
            }
            this.mAvailBackStackIndices.add(Integer.valueOf(index));
        }
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    public boolean execPendingActions() {
        if (!this.mExecutingActions) {
            if (Looper.myLooper() == this.mActivity.mHandler.getLooper()) {
                boolean didSomething = HONEYCOMB;
                while (true) {
                    synchronized (this) {
                        if (this.mPendingActions == null || this.mPendingActions.size() == 0) {
                        } else {
                            int numActions = this.mPendingActions.size();
                            if (this.mTmpActions == null || this.mTmpActions.length < numActions) {
                                this.mTmpActions = new Runnable[numActions];
                            }
                            this.mPendingActions.toArray(this.mTmpActions);
                            this.mPendingActions.clear();
                            this.mActivity.mHandler.removeCallbacks(this.mExecCommit);
                            this.mExecutingActions = true;
                            for (int i = 0; i < numActions; i += ANIM_STYLE_OPEN_ENTER) {
                                this.mTmpActions[i].run();
                                this.mTmpActions[i] = null;
                            }
                            this.mExecutingActions = HONEYCOMB;
                            didSomething = true;
                        }
                    }
                }
                while (true) {
                    break;
                }
            }
            throw new IllegalStateException("Must be called from main thread of process");
        }
        throw new IllegalStateException("Recursive entry to executePendingTransactions");
    }

    void reportBackStackChanged() {
        if (this.mBackStackChangeListeners != null) {
            for (int i = 0; i < this.mBackStackChangeListeners.size(); i += ANIM_STYLE_OPEN_ENTER) {
                ((OnBackStackChangedListener) this.mBackStackChangeListeners.get(i)).onBackStackChanged();
            }
        }
    }

    void addBackStackState(BackStackRecord state) {
        if (this.mBackStack == null) {
            this.mBackStack = new ArrayList();
        }
        this.mBackStack.add(state);
        reportBackStackChanged();
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    boolean popBackStackState(Handler handler, String name, int id, int flags) {
        if (this.mBackStack == null) {
            return HONEYCOMB;
        }
        if (name == null && id < 0 && (flags & ANIM_STYLE_OPEN_ENTER) == 0) {
            int last = this.mBackStack.size() - 1;
            if (last < 0) {
                return HONEYCOMB;
            }
            ((BackStackRecord) this.mBackStack.remove(last)).popFromBackStack(true);
            reportBackStackChanged();
        } else {
            int index = -1;
            if (name != null || id >= 0) {
                BackStackRecord bss;
                index = this.mBackStack.size() - 1;
                while (index >= 0) {
                    bss = (BackStackRecord) this.mBackStack.get(index);
                    if ((name == null || !name.equals(bss.getName())) && (id < 0 || id != bss.mIndex)) {
                        index--;
                    }
                }
                if (index < 0) {
                    return HONEYCOMB;
                }
                if ((flags & ANIM_STYLE_OPEN_ENTER) != 0) {
                    index--;
                    while (index >= 0) {
                        bss = (BackStackRecord) this.mBackStack.get(index);
                        if ((name != null && name.equals(bss.getName())) || (id >= 0 && id == bss.mIndex)) {
                            index--;
                        }
                    }
                }
            }
            if (index == this.mBackStack.size() - 1) {
                return HONEYCOMB;
            }
            int i;
            ArrayList<BackStackRecord> states = new ArrayList();
            for (i = this.mBackStack.size() - 1; i > index; i--) {
                states.add(this.mBackStack.remove(i));
            }
            int LAST = states.size() - 1;
            for (i = 0; i <= LAST; i += ANIM_STYLE_OPEN_ENTER) {
                boolean z;
                if (DEBUG) {
                    Log.v(TAG, "Popping back stack state: " + states.get(i));
                }
                BackStackRecord backStackRecord = (BackStackRecord) states.get(i);
                if (i == LAST) {
                    z = true;
                } else {
                    z = HONEYCOMB;
                }
                backStackRecord.popFromBackStack(z);
            }
            reportBackStackChanged();
        }
        return true;
    }

    ArrayList<Fragment> retainNonConfig() {
        ArrayList<Fragment> fragments = null;
        if (this.mActive != null) {
            for (int i = 0; i < this.mActive.size(); i += ANIM_STYLE_OPEN_ENTER) {
                Fragment f = (Fragment) this.mActive.get(i);
                if (f != null && f.mRetainInstance) {
                    if (fragments == null) {
                        fragments = new ArrayList();
                    }
                    fragments.add(f);
                    f.mRetaining = true;
                    f.mTargetIndex = f.mTarget != null ? f.mTarget.mIndex : -1;
                    if (DEBUG) {
                        Log.v(TAG, "retainNonConfig: keeping retained " + f);
                    }
                }
            }
        }
        return fragments;
    }

    void saveFragmentViewState(Fragment f) {
        if (f.mInnerView != null) {
            if (this.mStateArray == null) {
                this.mStateArray = new SparseArray();
            } else {
                this.mStateArray.clear();
            }
            f.mInnerView.saveHierarchyState(this.mStateArray);
            if (this.mStateArray.size() > 0) {
                f.mSavedViewState = this.mStateArray;
                this.mStateArray = null;
            }
        }
    }

    Bundle saveFragmentBasicState(Fragment f) {
        Bundle result = null;
        if (this.mStateBundle == null) {
            this.mStateBundle = new Bundle();
        }
        f.performSaveInstanceState(this.mStateBundle);
        if (!this.mStateBundle.isEmpty()) {
            result = this.mStateBundle;
            this.mStateBundle = null;
        }
        if (f.mView != null) {
            saveFragmentViewState(f);
        }
        if (f.mSavedViewState != null) {
            if (result == null) {
                result = new Bundle();
            }
            result.putSparseParcelableArray(VIEW_STATE_TAG, f.mSavedViewState);
        }
        if (!f.mUserVisibleHint) {
            if (result == null) {
                result = new Bundle();
            }
            result.putBoolean(USER_VISIBLE_HINT_TAG, f.mUserVisibleHint);
        }
        return result;
    }

    Parcelable saveAllState() {
        execPendingActions();
        if (HONEYCOMB) {
            this.mStateSaved = true;
        }
        if (this.mActive == null || this.mActive.size() <= 0) {
            return null;
        }
        int i;
        int N = this.mActive.size();
        FragmentState[] active = new FragmentState[N];
        boolean haveFragments = HONEYCOMB;
        for (i = 0; i < N; i += ANIM_STYLE_OPEN_ENTER) {
            Fragment f = (Fragment) this.mActive.get(i);
            if (f != null) {
                if (f.mIndex < 0) {
                    throwException(new IllegalStateException("Failure saving state: active " + f + " has cleared index: " + f.mIndex));
                }
                haveFragments = true;
                FragmentState fs = new FragmentState(f);
                active[i] = fs;
                if (f.mState <= 0 || fs.mSavedFragmentState != null) {
                    fs.mSavedFragmentState = f.mSavedFragmentState;
                } else {
                    fs.mSavedFragmentState = saveFragmentBasicState(f);
                    if (f.mTarget != null) {
                        if (f.mTarget.mIndex < 0) {
                            throwException(new IllegalStateException("Failure saving state: " + f + " has target not in fragment manager: " + f.mTarget));
                        }
                        if (fs.mSavedFragmentState == null) {
                            fs.mSavedFragmentState = new Bundle();
                        }
                        putFragment(fs.mSavedFragmentState, TARGET_STATE_TAG, f.mTarget);
                        if (f.mTargetRequestCode != 0) {
                            fs.mSavedFragmentState.putInt(TARGET_REQUEST_CODE_STATE_TAG, f.mTargetRequestCode);
                        }
                    }
                }
                if (DEBUG) {
                    Log.v(TAG, "Saved state of " + f + ": " + fs.mSavedFragmentState);
                }
            }
        }
        if (haveFragments) {
            int[] added = null;
            BackStackState[] backStack = null;
            if (this.mAdded != null) {
                N = this.mAdded.size();
                if (N > 0) {
                    added = new int[N];
                    for (i = 0; i < N; i += ANIM_STYLE_OPEN_ENTER) {
                        added[i] = ((Fragment) this.mAdded.get(i)).mIndex;
                        if (added[i] < 0) {
                            throwException(new IllegalStateException("Failure saving state: active " + this.mAdded.get(i) + " has cleared index: " + added[i]));
                        }
                        if (DEBUG) {
                            Log.v(TAG, "saveAllState: adding fragment #" + i + ": " + this.mAdded.get(i));
                        }
                    }
                }
            }
            if (this.mBackStack != null) {
                N = this.mBackStack.size();
                if (N > 0) {
                    backStack = new BackStackState[N];
                    for (i = 0; i < N; i += ANIM_STYLE_OPEN_ENTER) {
                        backStack[i] = new BackStackState(this, (BackStackRecord) this.mBackStack.get(i));
                        if (DEBUG) {
                            Log.v(TAG, "saveAllState: adding back stack #" + i + ": " + this.mBackStack.get(i));
                        }
                    }
                }
            }
            Parcelable fms = new FragmentManagerState();
            fms.mActive = active;
            fms.mAdded = added;
            fms.mBackStack = backStack;
            return fms;
        } else if (!DEBUG) {
            return null;
        } else {
            Log.v(TAG, "saveAllState: no fragments!");
            return null;
        }
    }

    void restoreAllState(Parcelable state, ArrayList<Fragment> nonConfig) {
        if (state != null) {
            FragmentManagerState fms = (FragmentManagerState) state;
            if (fms.mActive != null) {
                int i;
                Fragment f;
                FragmentState fs;
                if (nonConfig != null) {
                    for (i = 0; i < nonConfig.size(); i += ANIM_STYLE_OPEN_ENTER) {
                        f = (Fragment) nonConfig.get(i);
                        if (DEBUG) {
                            Log.v(TAG, "restoreAllState: re-attaching retained " + f);
                        }
                        fs = fms.mActive[f.mIndex];
                        fs.mInstance = f;
                        f.mSavedViewState = null;
                        f.mBackStackNesting = 0;
                        f.mInLayout = HONEYCOMB;
                        f.mAdded = HONEYCOMB;
                        f.mTarget = null;
                        if (fs.mSavedFragmentState != null) {
                            fs.mSavedFragmentState.setClassLoader(this.mActivity.getClassLoader());
                            f.mSavedViewState = fs.mSavedFragmentState.getSparseParcelableArray(VIEW_STATE_TAG);
                        }
                    }
                }
                this.mActive = new ArrayList(fms.mActive.length);
                if (this.mAvailIndices != null) {
                    this.mAvailIndices.clear();
                }
                for (i = 0; i < fms.mActive.length; i += ANIM_STYLE_OPEN_ENTER) {
                    fs = fms.mActive[i];
                    if (fs != null) {
                        f = fs.instantiate(this.mActivity, this.mParent);
                        if (DEBUG) {
                            Log.v(TAG, "restoreAllState: active #" + i + ": " + f);
                        }
                        this.mActive.add(f);
                        fs.mInstance = null;
                    } else {
                        this.mActive.add(null);
                        if (this.mAvailIndices == null) {
                            this.mAvailIndices = new ArrayList();
                        }
                        if (DEBUG) {
                            Log.v(TAG, "restoreAllState: avail #" + i);
                        }
                        this.mAvailIndices.add(Integer.valueOf(i));
                    }
                }
                if (nonConfig != null) {
                    for (i = 0; i < nonConfig.size(); i += ANIM_STYLE_OPEN_ENTER) {
                        f = (Fragment) nonConfig.get(i);
                        if (f.mTargetIndex >= 0) {
                            if (f.mTargetIndex < this.mActive.size()) {
                                f.mTarget = (Fragment) this.mActive.get(f.mTargetIndex);
                            } else {
                                Log.w(TAG, "Re-attaching retained fragment " + f + " target no longer exists: " + f.mTargetIndex);
                                f.mTarget = null;
                            }
                        }
                    }
                }
                if (fms.mAdded != null) {
                    this.mAdded = new ArrayList(fms.mAdded.length);
                    for (i = 0; i < fms.mAdded.length; i += ANIM_STYLE_OPEN_ENTER) {
                        f = (Fragment) this.mActive.get(fms.mAdded[i]);
                        if (f == null) {
                            throwException(new IllegalStateException("No instantiated fragment for index #" + fms.mAdded[i]));
                        }
                        f.mAdded = true;
                        if (DEBUG) {
                            Log.v(TAG, "restoreAllState: added #" + i + ": " + f);
                        }
                        if (this.mAdded.contains(f)) {
                            throw new IllegalStateException("Already added!");
                        }
                        this.mAdded.add(f);
                    }
                } else {
                    this.mAdded = null;
                }
                if (fms.mBackStack != null) {
                    this.mBackStack = new ArrayList(fms.mBackStack.length);
                    for (i = 0; i < fms.mBackStack.length; i += ANIM_STYLE_OPEN_ENTER) {
                        BackStackRecord bse = fms.mBackStack[i].instantiate(this);
                        if (DEBUG) {
                            Log.v(TAG, "restoreAllState: back stack #" + i + " (index " + bse.mIndex + "): " + bse);
                            bse.dump("  ", new PrintWriter(new LogWriter(TAG)), HONEYCOMB);
                        }
                        this.mBackStack.add(bse);
                        if (bse.mIndex >= 0) {
                            setBackStackIndex(bse.mIndex, bse);
                        }
                    }
                    return;
                }
                this.mBackStack = null;
            }
        }
    }

    public void attachActivity(FragmentActivity activity, FragmentContainer container, Fragment parent) {
        if (this.mActivity != null) {
            throw new IllegalStateException("Already attached");
        }
        this.mActivity = activity;
        this.mContainer = container;
        this.mParent = parent;
    }

    public void noteStateNotSaved() {
        this.mStateSaved = HONEYCOMB;
    }

    public void dispatchCreate() {
        this.mStateSaved = HONEYCOMB;
        moveToState(ANIM_STYLE_OPEN_ENTER, HONEYCOMB);
    }

    public void dispatchActivityCreated() {
        this.mStateSaved = HONEYCOMB;
        moveToState(ANIM_STYLE_OPEN_EXIT, HONEYCOMB);
    }

    public void dispatchStart() {
        this.mStateSaved = HONEYCOMB;
        moveToState(ANIM_STYLE_CLOSE_EXIT, HONEYCOMB);
    }

    public void dispatchResume() {
        this.mStateSaved = HONEYCOMB;
        moveToState(ANIM_STYLE_FADE_ENTER, HONEYCOMB);
    }

    public void dispatchPause() {
        moveToState(ANIM_STYLE_CLOSE_EXIT, HONEYCOMB);
    }

    public void dispatchStop() {
        this.mStateSaved = true;
        moveToState(ANIM_STYLE_CLOSE_ENTER, HONEYCOMB);
    }

    public void dispatchReallyStop() {
        moveToState(ANIM_STYLE_OPEN_EXIT, HONEYCOMB);
    }

    public void dispatchDestroyView() {
        moveToState(ANIM_STYLE_OPEN_ENTER, HONEYCOMB);
    }

    public void dispatchDestroy() {
        this.mDestroyed = true;
        execPendingActions();
        moveToState(0, HONEYCOMB);
        this.mActivity = null;
        this.mContainer = null;
        this.mParent = null;
    }

    public void dispatchConfigurationChanged(Configuration newConfig) {
        if (this.mAdded != null) {
            for (int i = 0; i < this.mAdded.size(); i += ANIM_STYLE_OPEN_ENTER) {
                Fragment f = (Fragment) this.mAdded.get(i);
                if (f != null) {
                    f.performConfigurationChanged(newConfig);
                }
            }
        }
    }

    public void dispatchLowMemory() {
        if (this.mAdded != null) {
            for (int i = 0; i < this.mAdded.size(); i += ANIM_STYLE_OPEN_ENTER) {
                Fragment f = (Fragment) this.mAdded.get(i);
                if (f != null) {
                    f.performLowMemory();
                }
            }
        }
    }

    public boolean dispatchCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        int i;
        Fragment f;
        boolean show = HONEYCOMB;
        ArrayList<Fragment> newMenus = null;
        if (this.mAdded != null) {
            for (i = 0; i < this.mAdded.size(); i += ANIM_STYLE_OPEN_ENTER) {
                f = (Fragment) this.mAdded.get(i);
                if (f != null && f.performCreateOptionsMenu(menu, inflater)) {
                    show = true;
                    if (newMenus == null) {
                        newMenus = new ArrayList();
                    }
                    newMenus.add(f);
                }
            }
        }
        if (this.mCreatedMenus != null) {
            for (i = 0; i < this.mCreatedMenus.size(); i += ANIM_STYLE_OPEN_ENTER) {
                f = (Fragment) this.mCreatedMenus.get(i);
                if (newMenus == null || !newMenus.contains(f)) {
                    f.onDestroyOptionsMenu();
                }
            }
        }
        this.mCreatedMenus = newMenus;
        return show;
    }

    public boolean dispatchPrepareOptionsMenu(Menu menu) {
        boolean show = HONEYCOMB;
        if (this.mAdded != null) {
            for (int i = 0; i < this.mAdded.size(); i += ANIM_STYLE_OPEN_ENTER) {
                Fragment f = (Fragment) this.mAdded.get(i);
                if (f != null && f.performPrepareOptionsMenu(menu)) {
                    show = true;
                }
            }
        }
        return show;
    }

    public boolean dispatchOptionsItemSelected(MenuItem item) {
        if (this.mAdded != null) {
            for (int i = 0; i < this.mAdded.size(); i += ANIM_STYLE_OPEN_ENTER) {
                Fragment f = (Fragment) this.mAdded.get(i);
                if (f != null && f.performOptionsItemSelected(item)) {
                    return true;
                }
            }
        }
        return HONEYCOMB;
    }

    public boolean dispatchContextItemSelected(MenuItem item) {
        if (this.mAdded != null) {
            for (int i = 0; i < this.mAdded.size(); i += ANIM_STYLE_OPEN_ENTER) {
                Fragment f = (Fragment) this.mAdded.get(i);
                if (f != null && f.performContextItemSelected(item)) {
                    return true;
                }
            }
        }
        return HONEYCOMB;
    }

    public void dispatchOptionsMenuClosed(Menu menu) {
        if (this.mAdded != null) {
            for (int i = 0; i < this.mAdded.size(); i += ANIM_STYLE_OPEN_ENTER) {
                Fragment f = (Fragment) this.mAdded.get(i);
                if (f != null) {
                    f.performOptionsMenuClosed(menu);
                }
            }
        }
    }

    public static int reverseTransit(int transit) {
        switch (transit) {
            case FragmentTransaction.TRANSIT_FRAGMENT_OPEN /*4097*/:
                return FragmentTransaction.TRANSIT_FRAGMENT_CLOSE;
            case FragmentTransaction.TRANSIT_FRAGMENT_FADE /*4099*/:
                return FragmentTransaction.TRANSIT_FRAGMENT_FADE;
            case FragmentTransaction.TRANSIT_FRAGMENT_CLOSE /*8194*/:
                return FragmentTransaction.TRANSIT_FRAGMENT_OPEN;
            default:
                return 0;
        }
    }

    public static int transitToStyleIndex(int transit, boolean enter) {
        int animAttr = -1;
        switch (transit) {
            case FragmentTransaction.TRANSIT_FRAGMENT_OPEN /*4097*/:
                animAttr = enter ? ANIM_STYLE_OPEN_ENTER : ANIM_STYLE_OPEN_EXIT;
                break;
            case FragmentTransaction.TRANSIT_FRAGMENT_FADE /*4099*/:
                animAttr = enter ? ANIM_STYLE_FADE_ENTER : ANIM_STYLE_FADE_EXIT;
                break;
            case FragmentTransaction.TRANSIT_FRAGMENT_CLOSE /*8194*/:
                animAttr = enter ? ANIM_STYLE_CLOSE_ENTER : ANIM_STYLE_CLOSE_EXIT;
                break;
        }
        return animAttr;
    }
}
