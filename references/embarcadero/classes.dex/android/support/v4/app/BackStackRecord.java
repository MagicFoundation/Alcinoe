package android.support.v4.app;

import android.support.v4.app.FragmentManager.BackStackEntry;
import android.support.v4.media.TransportMediator;
import android.support.v4.util.LogWriter;
import android.util.Log;
import java.io.FileDescriptor;
import java.io.PrintWriter;
import java.util.ArrayList;

final class BackStackRecord extends FragmentTransaction implements BackStackEntry, Runnable {
    static final int OP_ADD = 1;
    static final int OP_ATTACH = 7;
    static final int OP_DETACH = 6;
    static final int OP_HIDE = 4;
    static final int OP_NULL = 0;
    static final int OP_REMOVE = 3;
    static final int OP_REPLACE = 2;
    static final int OP_SHOW = 5;
    static final String TAG = "FragmentManager";
    boolean mAddToBackStack;
    boolean mAllowAddToBackStack;
    int mBreadCrumbShortTitleRes;
    CharSequence mBreadCrumbShortTitleText;
    int mBreadCrumbTitleRes;
    CharSequence mBreadCrumbTitleText;
    boolean mCommitted;
    int mEnterAnim;
    int mExitAnim;
    Op mHead;
    int mIndex;
    final FragmentManagerImpl mManager;
    String mName;
    int mNumOp;
    int mPopEnterAnim;
    int mPopExitAnim;
    Op mTail;
    int mTransition;
    int mTransitionStyle;

    static final class Op {
        int cmd;
        int enterAnim;
        int exitAnim;
        Fragment fragment;
        Op next;
        int popEnterAnim;
        int popExitAnim;
        Op prev;
        ArrayList<Fragment> removed;

        Op() {
        }
    }

    public String toString() {
        StringBuilder sb = new StringBuilder(TransportMediator.FLAG_KEY_MEDIA_NEXT);
        sb.append("BackStackEntry{");
        sb.append(Integer.toHexString(System.identityHashCode(this)));
        if (this.mIndex >= 0) {
            sb.append(" #");
            sb.append(this.mIndex);
        }
        if (this.mName != null) {
            sb.append(" ");
            sb.append(this.mName);
        }
        sb.append("}");
        return sb.toString();
    }

    public void dump(String prefix, FileDescriptor fd, PrintWriter writer, String[] args) {
        dump(prefix, writer, true);
    }

    public void dump(String prefix, PrintWriter writer, boolean full) {
        if (full) {
            writer.print(prefix);
            writer.print("mName=");
            writer.print(this.mName);
            writer.print(" mIndex=");
            writer.print(this.mIndex);
            writer.print(" mCommitted=");
            writer.println(this.mCommitted);
            if (this.mTransition != 0) {
                writer.print(prefix);
                writer.print("mTransition=#");
                writer.print(Integer.toHexString(this.mTransition));
                writer.print(" mTransitionStyle=#");
                writer.println(Integer.toHexString(this.mTransitionStyle));
            }
            if (!(this.mEnterAnim == 0 && this.mExitAnim == 0)) {
                writer.print(prefix);
                writer.print("mEnterAnim=#");
                writer.print(Integer.toHexString(this.mEnterAnim));
                writer.print(" mExitAnim=#");
                writer.println(Integer.toHexString(this.mExitAnim));
            }
            if (!(this.mPopEnterAnim == 0 && this.mPopExitAnim == 0)) {
                writer.print(prefix);
                writer.print("mPopEnterAnim=#");
                writer.print(Integer.toHexString(this.mPopEnterAnim));
                writer.print(" mPopExitAnim=#");
                writer.println(Integer.toHexString(this.mPopExitAnim));
            }
            if (!(this.mBreadCrumbTitleRes == 0 && this.mBreadCrumbTitleText == null)) {
                writer.print(prefix);
                writer.print("mBreadCrumbTitleRes=#");
                writer.print(Integer.toHexString(this.mBreadCrumbTitleRes));
                writer.print(" mBreadCrumbTitleText=");
                writer.println(this.mBreadCrumbTitleText);
            }
            if (!(this.mBreadCrumbShortTitleRes == 0 && this.mBreadCrumbShortTitleText == null)) {
                writer.print(prefix);
                writer.print("mBreadCrumbShortTitleRes=#");
                writer.print(Integer.toHexString(this.mBreadCrumbShortTitleRes));
                writer.print(" mBreadCrumbShortTitleText=");
                writer.println(this.mBreadCrumbShortTitleText);
            }
        }
        if (this.mHead != null) {
            writer.print(prefix);
            writer.println("Operations:");
            String innerPrefix = prefix + "    ";
            Op op = this.mHead;
            int num = OP_NULL;
            while (op != null) {
                String cmdStr;
                switch (op.cmd) {
                    case OP_NULL /*0*/:
                        cmdStr = "NULL";
                        break;
                    case OP_ADD /*1*/:
                        cmdStr = "ADD";
                        break;
                    case OP_REPLACE /*2*/:
                        cmdStr = "REPLACE";
                        break;
                    case OP_REMOVE /*3*/:
                        cmdStr = "REMOVE";
                        break;
                    case OP_HIDE /*4*/:
                        cmdStr = "HIDE";
                        break;
                    case OP_SHOW /*5*/:
                        cmdStr = "SHOW";
                        break;
                    case OP_DETACH /*6*/:
                        cmdStr = "DETACH";
                        break;
                    case OP_ATTACH /*7*/:
                        cmdStr = "ATTACH";
                        break;
                    default:
                        cmdStr = "cmd=" + op.cmd;
                        break;
                }
                writer.print(prefix);
                writer.print("  Op #");
                writer.print(num);
                writer.print(": ");
                writer.print(cmdStr);
                writer.print(" ");
                writer.println(op.fragment);
                if (full) {
                    if (!(op.enterAnim == 0 && op.exitAnim == 0)) {
                        writer.print(prefix);
                        writer.print("enterAnim=#");
                        writer.print(Integer.toHexString(op.enterAnim));
                        writer.print(" exitAnim=#");
                        writer.println(Integer.toHexString(op.exitAnim));
                    }
                    if (!(op.popEnterAnim == 0 && op.popExitAnim == 0)) {
                        writer.print(prefix);
                        writer.print("popEnterAnim=#");
                        writer.print(Integer.toHexString(op.popEnterAnim));
                        writer.print(" popExitAnim=#");
                        writer.println(Integer.toHexString(op.popExitAnim));
                    }
                }
                if (op.removed != null && op.removed.size() > 0) {
                    for (int i = OP_NULL; i < op.removed.size(); i += OP_ADD) {
                        writer.print(innerPrefix);
                        if (op.removed.size() == OP_ADD) {
                            writer.print("Removed: ");
                        } else {
                            if (i == 0) {
                                writer.println("Removed:");
                            }
                            writer.print(innerPrefix);
                            writer.print("  #");
                            writer.print(i);
                            writer.print(": ");
                        }
                        writer.println(op.removed.get(i));
                    }
                }
                op = op.next;
                num += OP_ADD;
            }
        }
    }

    public BackStackRecord(FragmentManagerImpl manager) {
        this.mAllowAddToBackStack = true;
        this.mIndex = -1;
        this.mManager = manager;
    }

    public int getId() {
        return this.mIndex;
    }

    public int getBreadCrumbTitleRes() {
        return this.mBreadCrumbTitleRes;
    }

    public int getBreadCrumbShortTitleRes() {
        return this.mBreadCrumbShortTitleRes;
    }

    public CharSequence getBreadCrumbTitle() {
        if (this.mBreadCrumbTitleRes != 0) {
            return this.mManager.mActivity.getText(this.mBreadCrumbTitleRes);
        }
        return this.mBreadCrumbTitleText;
    }

    public CharSequence getBreadCrumbShortTitle() {
        if (this.mBreadCrumbShortTitleRes != 0) {
            return this.mManager.mActivity.getText(this.mBreadCrumbShortTitleRes);
        }
        return this.mBreadCrumbShortTitleText;
    }

    void addOp(Op op) {
        if (this.mHead == null) {
            this.mTail = op;
            this.mHead = op;
        } else {
            op.prev = this.mTail;
            this.mTail.next = op;
            this.mTail = op;
        }
        op.enterAnim = this.mEnterAnim;
        op.exitAnim = this.mExitAnim;
        op.popEnterAnim = this.mPopEnterAnim;
        op.popExitAnim = this.mPopExitAnim;
        this.mNumOp += OP_ADD;
    }

    public FragmentTransaction add(Fragment fragment, String tag) {
        doAddOp(OP_NULL, fragment, tag, OP_ADD);
        return this;
    }

    public FragmentTransaction add(int containerViewId, Fragment fragment) {
        doAddOp(containerViewId, fragment, null, OP_ADD);
        return this;
    }

    public FragmentTransaction add(int containerViewId, Fragment fragment, String tag) {
        doAddOp(containerViewId, fragment, tag, OP_ADD);
        return this;
    }

    private void doAddOp(int containerViewId, Fragment fragment, String tag, int opcmd) {
        fragment.mFragmentManager = this.mManager;
        if (tag != null) {
            if (fragment.mTag == null || tag.equals(fragment.mTag)) {
                fragment.mTag = tag;
            } else {
                throw new IllegalStateException("Can't change tag of fragment " + fragment + ": was " + fragment.mTag + " now " + tag);
            }
        }
        if (containerViewId != 0) {
            if (fragment.mFragmentId == 0 || fragment.mFragmentId == containerViewId) {
                fragment.mFragmentId = containerViewId;
                fragment.mContainerId = containerViewId;
            } else {
                throw new IllegalStateException("Can't change container ID of fragment " + fragment + ": was " + fragment.mFragmentId + " now " + containerViewId);
            }
        }
        Op op = new Op();
        op.cmd = opcmd;
        op.fragment = fragment;
        addOp(op);
    }

    public FragmentTransaction replace(int containerViewId, Fragment fragment) {
        return replace(containerViewId, fragment, null);
    }

    public FragmentTransaction replace(int containerViewId, Fragment fragment, String tag) {
        if (containerViewId == 0) {
            throw new IllegalArgumentException("Must use non-zero containerViewId");
        }
        doAddOp(containerViewId, fragment, tag, OP_REPLACE);
        return this;
    }

    public FragmentTransaction remove(Fragment fragment) {
        Op op = new Op();
        op.cmd = OP_REMOVE;
        op.fragment = fragment;
        addOp(op);
        return this;
    }

    public FragmentTransaction hide(Fragment fragment) {
        Op op = new Op();
        op.cmd = OP_HIDE;
        op.fragment = fragment;
        addOp(op);
        return this;
    }

    public FragmentTransaction show(Fragment fragment) {
        Op op = new Op();
        op.cmd = OP_SHOW;
        op.fragment = fragment;
        addOp(op);
        return this;
    }

    public FragmentTransaction detach(Fragment fragment) {
        Op op = new Op();
        op.cmd = OP_DETACH;
        op.fragment = fragment;
        addOp(op);
        return this;
    }

    public FragmentTransaction attach(Fragment fragment) {
        Op op = new Op();
        op.cmd = OP_ATTACH;
        op.fragment = fragment;
        addOp(op);
        return this;
    }

    public FragmentTransaction setCustomAnimations(int enter, int exit) {
        return setCustomAnimations(enter, exit, OP_NULL, OP_NULL);
    }

    public FragmentTransaction setCustomAnimations(int enter, int exit, int popEnter, int popExit) {
        this.mEnterAnim = enter;
        this.mExitAnim = exit;
        this.mPopEnterAnim = popEnter;
        this.mPopExitAnim = popExit;
        return this;
    }

    public FragmentTransaction setTransition(int transition) {
        this.mTransition = transition;
        return this;
    }

    public FragmentTransaction setTransitionStyle(int styleRes) {
        this.mTransitionStyle = styleRes;
        return this;
    }

    public FragmentTransaction addToBackStack(String name) {
        if (this.mAllowAddToBackStack) {
            this.mAddToBackStack = true;
            this.mName = name;
            return this;
        }
        throw new IllegalStateException("This FragmentTransaction is not allowed to be added to the back stack.");
    }

    public boolean isAddToBackStackAllowed() {
        return this.mAllowAddToBackStack;
    }

    public FragmentTransaction disallowAddToBackStack() {
        if (this.mAddToBackStack) {
            throw new IllegalStateException("This transaction is already being added to the back stack");
        }
        this.mAllowAddToBackStack = false;
        return this;
    }

    public FragmentTransaction setBreadCrumbTitle(int res) {
        this.mBreadCrumbTitleRes = res;
        this.mBreadCrumbTitleText = null;
        return this;
    }

    public FragmentTransaction setBreadCrumbTitle(CharSequence text) {
        this.mBreadCrumbTitleRes = OP_NULL;
        this.mBreadCrumbTitleText = text;
        return this;
    }

    public FragmentTransaction setBreadCrumbShortTitle(int res) {
        this.mBreadCrumbShortTitleRes = res;
        this.mBreadCrumbShortTitleText = null;
        return this;
    }

    public FragmentTransaction setBreadCrumbShortTitle(CharSequence text) {
        this.mBreadCrumbShortTitleRes = OP_NULL;
        this.mBreadCrumbShortTitleText = text;
        return this;
    }

    void bumpBackStackNesting(int amt) {
        if (this.mAddToBackStack) {
            if (FragmentManagerImpl.DEBUG) {
                Log.v(TAG, "Bump nesting in " + this + " by " + amt);
            }
            for (Op op = this.mHead; op != null; op = op.next) {
                if (op.fragment != null) {
                    Fragment fragment = op.fragment;
                    fragment.mBackStackNesting += amt;
                    if (FragmentManagerImpl.DEBUG) {
                        Log.v(TAG, "Bump nesting of " + op.fragment + " to " + op.fragment.mBackStackNesting);
                    }
                }
                if (op.removed != null) {
                    for (int i = op.removed.size() - 1; i >= 0; i--) {
                        Fragment r = (Fragment) op.removed.get(i);
                        r.mBackStackNesting += amt;
                        if (FragmentManagerImpl.DEBUG) {
                            Log.v(TAG, "Bump nesting of " + r + " to " + r.mBackStackNesting);
                        }
                    }
                }
            }
        }
    }

    public int commit() {
        return commitInternal(false);
    }

    public int commitAllowingStateLoss() {
        return commitInternal(true);
    }

    int commitInternal(boolean allowStateLoss) {
        if (this.mCommitted) {
            throw new IllegalStateException("commit already called");
        }
        if (FragmentManagerImpl.DEBUG) {
            Log.v(TAG, "Commit: " + this);
            dump("  ", null, new PrintWriter(new LogWriter(TAG)), null);
        }
        this.mCommitted = true;
        if (this.mAddToBackStack) {
            this.mIndex = this.mManager.allocBackStackIndex(this);
        } else {
            this.mIndex = -1;
        }
        this.mManager.enqueueAction(this, allowStateLoss);
        return this.mIndex;
    }

    public void run() {
        if (FragmentManagerImpl.DEBUG) {
            Log.v(TAG, "Run: " + this);
        }
        if (!this.mAddToBackStack || this.mIndex >= 0) {
            bumpBackStackNesting(OP_ADD);
            for (Op op = this.mHead; op != null; op = op.next) {
                Fragment f;
                switch (op.cmd) {
                    case OP_ADD /*1*/:
                        f = op.fragment;
                        f.mNextAnim = op.enterAnim;
                        this.mManager.addFragment(f, false);
                        break;
                    case OP_REPLACE /*2*/:
                        f = op.fragment;
                        if (this.mManager.mAdded != null) {
                            for (int i = OP_NULL; i < this.mManager.mAdded.size(); i += OP_ADD) {
                                Fragment old = (Fragment) this.mManager.mAdded.get(i);
                                if (FragmentManagerImpl.DEBUG) {
                                    Log.v(TAG, "OP_REPLACE: adding=" + f + " old=" + old);
                                }
                                if (f == null || old.mContainerId == f.mContainerId) {
                                    if (old == f) {
                                        f = null;
                                        op.fragment = null;
                                    } else {
                                        if (op.removed == null) {
                                            op.removed = new ArrayList();
                                        }
                                        op.removed.add(old);
                                        old.mNextAnim = op.exitAnim;
                                        if (this.mAddToBackStack) {
                                            old.mBackStackNesting += OP_ADD;
                                            if (FragmentManagerImpl.DEBUG) {
                                                Log.v(TAG, "Bump nesting of " + old + " to " + old.mBackStackNesting);
                                            }
                                        }
                                        this.mManager.removeFragment(old, this.mTransition, this.mTransitionStyle);
                                    }
                                }
                            }
                        }
                        if (f == null) {
                            break;
                        }
                        f.mNextAnim = op.enterAnim;
                        this.mManager.addFragment(f, false);
                        break;
                    case OP_REMOVE /*3*/:
                        f = op.fragment;
                        f.mNextAnim = op.exitAnim;
                        this.mManager.removeFragment(f, this.mTransition, this.mTransitionStyle);
                        break;
                    case OP_HIDE /*4*/:
                        f = op.fragment;
                        f.mNextAnim = op.exitAnim;
                        this.mManager.hideFragment(f, this.mTransition, this.mTransitionStyle);
                        break;
                    case OP_SHOW /*5*/:
                        f = op.fragment;
                        f.mNextAnim = op.enterAnim;
                        this.mManager.showFragment(f, this.mTransition, this.mTransitionStyle);
                        break;
                    case OP_DETACH /*6*/:
                        f = op.fragment;
                        f.mNextAnim = op.exitAnim;
                        this.mManager.detachFragment(f, this.mTransition, this.mTransitionStyle);
                        break;
                    case OP_ATTACH /*7*/:
                        f = op.fragment;
                        f.mNextAnim = op.enterAnim;
                        this.mManager.attachFragment(f, this.mTransition, this.mTransitionStyle);
                        break;
                    default:
                        throw new IllegalArgumentException("Unknown cmd: " + op.cmd);
                }
            }
            this.mManager.moveToState(this.mManager.mCurState, this.mTransition, this.mTransitionStyle, true);
            if (this.mAddToBackStack) {
                this.mManager.addBackStackState(this);
                return;
            }
            return;
        }
        throw new IllegalStateException("addToBackStack() called after commit()");
    }

    public void popFromBackStack(boolean doStateMove) {
        if (FragmentManagerImpl.DEBUG) {
            Log.v(TAG, "popFromBackStack: " + this);
            dump("  ", null, new PrintWriter(new LogWriter(TAG)), null);
        }
        bumpBackStackNesting(-1);
        for (Op op = this.mTail; op != null; op = op.prev) {
            Fragment f;
            switch (op.cmd) {
                case OP_ADD /*1*/:
                    f = op.fragment;
                    f.mNextAnim = op.popExitAnim;
                    this.mManager.removeFragment(f, FragmentManagerImpl.reverseTransit(this.mTransition), this.mTransitionStyle);
                    break;
                case OP_REPLACE /*2*/:
                    f = op.fragment;
                    if (f != null) {
                        f.mNextAnim = op.popExitAnim;
                        this.mManager.removeFragment(f, FragmentManagerImpl.reverseTransit(this.mTransition), this.mTransitionStyle);
                    }
                    if (op.removed == null) {
                        break;
                    }
                    for (int i = OP_NULL; i < op.removed.size(); i += OP_ADD) {
                        Fragment old = (Fragment) op.removed.get(i);
                        old.mNextAnim = op.popEnterAnim;
                        this.mManager.addFragment(old, false);
                    }
                    break;
                case OP_REMOVE /*3*/:
                    f = op.fragment;
                    f.mNextAnim = op.popEnterAnim;
                    this.mManager.addFragment(f, false);
                    break;
                case OP_HIDE /*4*/:
                    f = op.fragment;
                    f.mNextAnim = op.popEnterAnim;
                    this.mManager.showFragment(f, FragmentManagerImpl.reverseTransit(this.mTransition), this.mTransitionStyle);
                    break;
                case OP_SHOW /*5*/:
                    f = op.fragment;
                    f.mNextAnim = op.popExitAnim;
                    this.mManager.hideFragment(f, FragmentManagerImpl.reverseTransit(this.mTransition), this.mTransitionStyle);
                    break;
                case OP_DETACH /*6*/:
                    f = op.fragment;
                    f.mNextAnim = op.popEnterAnim;
                    this.mManager.attachFragment(f, FragmentManagerImpl.reverseTransit(this.mTransition), this.mTransitionStyle);
                    break;
                case OP_ATTACH /*7*/:
                    f = op.fragment;
                    f.mNextAnim = op.popEnterAnim;
                    this.mManager.detachFragment(f, FragmentManagerImpl.reverseTransit(this.mTransition), this.mTransitionStyle);
                    break;
                default:
                    throw new IllegalArgumentException("Unknown cmd: " + op.cmd);
            }
        }
        if (doStateMove) {
            this.mManager.moveToState(this.mManager.mCurState, FragmentManagerImpl.reverseTransit(this.mTransition), this.mTransitionStyle, true);
        }
        if (this.mIndex >= 0) {
            this.mManager.freeBackStackIndex(this.mIndex);
            this.mIndex = -1;
        }
    }

    public String getName() {
        return this.mName;
    }

    public int getTransition() {
        return this.mTransition;
    }

    public int getTransitionStyle() {
        return this.mTransitionStyle;
    }

    public boolean isEmpty() {
        return this.mNumOp == 0;
    }
}
