package com.google.android.gms.plus;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Parcelable;
import android.support.v4.view.accessibility.AccessibilityEventCompat;
import android.text.TextUtils;
import android.util.Log;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.Scopes;
import com.google.android.gms.internal.er;
import com.google.android.gms.internal.ir;
import com.google.android.gms.plus.model.people.Person;
import java.util.ArrayList;
import java.util.List;

public final class PlusShare {
    public static final String EXTRA_CALL_TO_ACTION = "com.google.android.apps.plus.CALL_TO_ACTION";
    public static final String EXTRA_CONTENT_DEEP_LINK_ID = "com.google.android.apps.plus.CONTENT_DEEP_LINK_ID";
    public static final String EXTRA_CONTENT_DEEP_LINK_METADATA = "com.google.android.apps.plus.CONTENT_DEEP_LINK_METADATA";
    public static final String EXTRA_CONTENT_URL = "com.google.android.apps.plus.CONTENT_URL";
    public static final String EXTRA_IS_INTERACTIVE_POST = "com.google.android.apps.plus.GOOGLE_INTERACTIVE_POST";
    public static final String EXTRA_SENDER_ID = "com.google.android.apps.plus.SENDER_ID";
    public static final String KEY_CALL_TO_ACTION_DEEP_LINK_ID = "deepLinkId";
    public static final String KEY_CALL_TO_ACTION_LABEL = "label";
    public static final String KEY_CALL_TO_ACTION_URL = "url";
    public static final String KEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION = "description";
    public static final String KEY_CONTENT_DEEP_LINK_METADATA_THUMBNAIL_URL = "thumbnailUrl";
    public static final String KEY_CONTENT_DEEP_LINK_METADATA_TITLE = "title";
    public static final String PARAM_CONTENT_DEEP_LINK_ID = "deep_link_id";

    public static class Builder {
        private boolean Rb;
        private ArrayList<Uri> Rc;
        private final Context mContext;
        private final Intent mIntent;

        public Builder(Activity launchingActivity) {
            this.mContext = launchingActivity;
            this.mIntent = new Intent().setAction("android.intent.action.SEND");
            this.mIntent.addFlags(AccessibilityEventCompat.TYPE_GESTURE_DETECTION_END);
            if (launchingActivity != null && launchingActivity.getComponentName() != null) {
                this.Rb = true;
            }
        }

        @Deprecated
        public Builder(Activity launchingActivity, PlusClient plusClient) {
            this(launchingActivity);
            er.a(plusClient != null, "PlusClient must not be null.");
            er.a(plusClient.isConnected(), "PlusClient must be connected to create an interactive post.");
            er.a(plusClient.hj().aR(Scopes.PLUS_LOGIN), "Must request PLUS_LOGIN scope in PlusClient to create an interactive post.");
            Person currentPerson = plusClient.getCurrentPerson();
            this.mIntent.putExtra(PlusShare.EXTRA_SENDER_ID, currentPerson != null ? currentPerson.getId() : "0");
        }

        public Builder(Context context) {
            this.mContext = context;
            this.mIntent = new Intent().setAction("android.intent.action.SEND");
        }

        public Builder addCallToAction(String label, Uri uri, String deepLinkId) {
            er.a(this.Rb, "Must include the launching activity with PlusShare.Builder constructor before setting call-to-action");
            boolean z = (uri == null || TextUtils.isEmpty(uri.toString())) ? false : true;
            er.b(z, (Object) "Must provide a call to action URL");
            Bundle bundle = new Bundle();
            if (!TextUtils.isEmpty(label)) {
                bundle.putString(PlusShare.KEY_CALL_TO_ACTION_LABEL, label);
            }
            bundle.putString(PlusShare.KEY_CALL_TO_ACTION_URL, uri.toString());
            if (!TextUtils.isEmpty(deepLinkId)) {
                er.a(PlusShare.aO(deepLinkId), "The specified deep-link ID was malformed.");
                bundle.putString(PlusShare.KEY_CALL_TO_ACTION_DEEP_LINK_ID, deepLinkId);
            }
            this.mIntent.putExtra(PlusShare.EXTRA_CALL_TO_ACTION, bundle);
            this.mIntent.putExtra(PlusShare.EXTRA_IS_INTERACTIVE_POST, true);
            this.mIntent.setType("text/plain");
            return this;
        }

        public Builder addStream(Uri streamUri) {
            Uri uri = (Uri) this.mIntent.getParcelableExtra("android.intent.extra.STREAM");
            if (uri == null) {
                return setStream(streamUri);
            }
            if (this.Rc == null) {
                this.Rc = new ArrayList();
            }
            this.Rc.add(uri);
            this.Rc.add(streamUri);
            return this;
        }

        public Intent getIntent() {
            boolean z = true;
            boolean z2 = this.Rc != null && this.Rc.size() > 1;
            boolean equals = "android.intent.action.SEND_MULTIPLE".equals(this.mIntent.getAction());
            boolean booleanExtra = this.mIntent.getBooleanExtra(PlusShare.EXTRA_IS_INTERACTIVE_POST, false);
            boolean z3 = (z2 && booleanExtra) ? false : true;
            er.a(z3, "Call-to-action buttons are only available for URLs.");
            z3 = !booleanExtra || this.mIntent.hasExtra(PlusShare.EXTRA_CONTENT_URL);
            er.a(z3, "The content URL is required for interactive posts.");
            if (!(!booleanExtra || this.mIntent.hasExtra(PlusShare.EXTRA_CONTENT_URL) || this.mIntent.hasExtra(PlusShare.EXTRA_CONTENT_DEEP_LINK_ID))) {
                z = false;
            }
            er.a(z, "Must set content URL or content deep-link ID to use a call-to-action button.");
            if (this.mIntent.hasExtra(PlusShare.EXTRA_CONTENT_DEEP_LINK_ID)) {
                er.a(PlusShare.aO(this.mIntent.getStringExtra(PlusShare.EXTRA_CONTENT_DEEP_LINK_ID)), "The specified deep-link ID was malformed.");
            }
            if (!z2 && equals) {
                this.mIntent.setAction("android.intent.action.SEND");
                if (this.Rc == null || this.Rc.isEmpty()) {
                    this.mIntent.removeExtra("android.intent.extra.STREAM");
                } else {
                    this.mIntent.putExtra("android.intent.extra.STREAM", (Parcelable) this.Rc.get(0));
                }
                this.Rc = null;
            }
            if (z2 && !equals) {
                this.mIntent.setAction("android.intent.action.SEND_MULTIPLE");
                if (this.Rc == null || this.Rc.isEmpty()) {
                    this.mIntent.removeExtra("android.intent.extra.STREAM");
                } else {
                    this.mIntent.putParcelableArrayListExtra("android.intent.extra.STREAM", this.Rc);
                }
            }
            if ("com.google.android.gms.plus.action.SHARE_INTERNAL_GOOGLE".equals(this.mIntent.getAction())) {
                this.mIntent.setPackage(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE);
                return this.mIntent;
            } else if (this.mIntent.hasExtra("android.intent.extra.STREAM")) {
                this.mIntent.setPackage("com.google.android.apps.plus");
                return this.mIntent;
            } else {
                this.mIntent.setAction("com.google.android.gms.plus.action.SHARE_GOOGLE");
                this.mIntent.setPackage(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE);
                return this.mIntent;
            }
        }

        public Builder setContentDeepLinkId(String deepLinkId) {
            return setContentDeepLinkId(deepLinkId, null, null, null);
        }

        public Builder setContentDeepLinkId(String deepLinkId, String title, String description, Uri thumbnailUri) {
            er.b(this.Rb, (Object) "Must include the launching activity with PlusShare.Builder constructor before setting deep links");
            er.b(!TextUtils.isEmpty(deepLinkId), (Object) "The deepLinkId parameter is required.");
            Bundle a = PlusShare.a(title, description, thumbnailUri);
            this.mIntent.putExtra(PlusShare.EXTRA_CONTENT_DEEP_LINK_ID, deepLinkId);
            this.mIntent.putExtra(PlusShare.EXTRA_CONTENT_DEEP_LINK_METADATA, a);
            this.mIntent.setType("text/plain");
            return this;
        }

        public Builder setContentUrl(Uri uri) {
            Object obj = null;
            if (uri != null) {
                obj = uri.toString();
            }
            if (TextUtils.isEmpty(obj)) {
                this.mIntent.removeExtra(PlusShare.EXTRA_CONTENT_URL);
            } else {
                this.mIntent.putExtra(PlusShare.EXTRA_CONTENT_URL, obj);
            }
            return this;
        }

        public Builder setRecipients(Person user, List<Person> recipientList) {
            this.mIntent.putExtra(PlusShare.EXTRA_SENDER_ID, user != null ? user.getId() : "0");
            return setRecipients(recipientList);
        }

        @Deprecated
        public Builder setRecipients(List<Person> recipientList) {
            int size = recipientList != null ? recipientList.size() : 0;
            if (size == 0) {
                this.mIntent.removeExtra("com.google.android.apps.plus.RECIPIENT_IDS");
                this.mIntent.removeExtra("com.google.android.apps.plus.RECIPIENT_DISPLAY_NAMES");
            } else {
                ArrayList arrayList = new ArrayList(size);
                ArrayList arrayList2 = new ArrayList(size);
                for (Person person : recipientList) {
                    arrayList.add(person.getId());
                    arrayList2.add(person.getDisplayName());
                }
                this.mIntent.putStringArrayListExtra("com.google.android.apps.plus.RECIPIENT_IDS", arrayList);
                this.mIntent.putStringArrayListExtra("com.google.android.apps.plus.RECIPIENT_DISPLAY_NAMES", arrayList2);
            }
            return this;
        }

        public Builder setStream(Uri streamUri) {
            this.Rc = null;
            this.mIntent.putExtra("android.intent.extra.STREAM", streamUri);
            return this;
        }

        public Builder setText(CharSequence text) {
            this.mIntent.putExtra("android.intent.extra.TEXT", text);
            return this;
        }

        public Builder setType(String mimeType) {
            this.mIntent.setType(mimeType);
            return this;
        }
    }

    @Deprecated
    protected PlusShare() {
        throw new AssertionError();
    }

    public static Bundle a(String str, String str2, Uri uri) {
        Bundle bundle = new Bundle();
        bundle.putString(KEY_CONTENT_DEEP_LINK_METADATA_TITLE, str);
        bundle.putString(KEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION, str2);
        if (uri != null) {
            bundle.putString(KEY_CONTENT_DEEP_LINK_METADATA_THUMBNAIL_URL, uri.toString());
        }
        return bundle;
    }

    protected static boolean aO(String str) {
        if (TextUtils.isEmpty(str)) {
            Log.e("GooglePlusPlatform", "The provided deep-link ID is empty.");
            return false;
        } else if (!str.contains(" ")) {
            return true;
        } else {
            Log.e("GooglePlusPlatform", "Spaces are not allowed in deep-link IDs.");
            return false;
        }
    }

    public static Person createPerson(String id, String displayName) {
        if (TextUtils.isEmpty(id)) {
            throw new IllegalArgumentException("MinimalPerson ID must not be empty.");
        } else if (!TextUtils.isEmpty(displayName)) {
            return new ir(displayName, id, null, 0, null);
        } else {
            throw new IllegalArgumentException("Display name must not be empty.");
        }
    }

    public static String getDeepLinkId(Intent intent) {
        return (intent == null || intent.getData() == null) ? null : intent.getData().getQueryParameter(PARAM_CONTENT_DEEP_LINK_ID);
    }
}
