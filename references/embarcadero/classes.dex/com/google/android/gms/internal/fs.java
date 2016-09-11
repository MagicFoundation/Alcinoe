package com.google.android.gms.internal;

import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.metadata.CollectionMetadataField;
import com.google.android.gms.drive.metadata.MetadataField;
import com.google.android.gms.drive.metadata.internal.a;
import com.google.android.gms.drive.metadata.internal.e;
import com.google.android.gms.drive.metadata.internal.g;
import com.google.android.gms.drive.metadata.internal.i;
import com.google.android.gms.drive.metadata.internal.j;
import com.google.android.gms.plus.PlusShare;

public class fs {
    public static final MetadataField<String> EA;
    public static final MetadataField<String> EB;
    public static final MetadataField<String> EC;
    public static final MetadataField<Boolean> ED;
    public static final MetadataField<DriveId> El;
    public static final MetadataField<Boolean> Em;
    public static final MetadataField<Boolean> En;
    public static final MetadataField<Boolean> Eo;
    public static final MetadataField<String> Ep;
    public static final CollectionMetadataField<String> Eq;
    public static final MetadataField<String> Er;
    public static final MetadataField<Boolean> Es;
    public static final MetadataField<String> Et;
    public static final MetadataField<String> Eu;
    public static final MetadataField<Long> Ev;
    public static final MetadataField<Boolean> Ew;
    public static final MetadataField<Boolean> Ex;
    public static final MetadataField<String> Ey;
    public static final MetadataField<Long> Ez;
    public static final MetadataField<Boolean> IS_PINNED;
    public static final MetadataField<String> MIME_TYPE;
    public static final CollectionMetadataField<DriveId> PARENTS;
    public static final MetadataField<Boolean> STARRED;
    public static final MetadataField<String> TITLE;
    public static final MetadataField<Boolean> TRASHED;

    /* renamed from: com.google.android.gms.internal.fs.1 */
    static class AnonymousClass1 extends a {
        AnonymousClass1(String str, int i) {
            super(str, i);
        }

        protected /* synthetic */ Object b(DataHolder dataHolder, int i, int i2) {
            return d(dataHolder, i, i2);
        }

        protected Boolean d(DataHolder dataHolder, int i, int i2) {
            return Boolean.valueOf(dataHolder.getInteger(getName(), i, i2) != 0);
        }
    }

    static {
        El = fu.EI;
        TITLE = new j(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_TITLE, 4100000);
        MIME_TYPE = new j("mimeType", 4100000);
        STARRED = new a("starred", 4100000);
        TRASHED = new AnonymousClass1("trashed", 4100000);
        Em = new a("isEditable", 4100000);
        IS_PINNED = new a("isPinned", 4100000);
        En = new a("isAppData", 4300000);
        Eo = new a("isShared", 4300000);
        PARENTS = new g("parents", 4100000);
        Ep = new j("alternateLink", 4300000);
        Eq = new i("ownerNames", 4300000);
        Er = new j(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION, 4300000);
        Es = new a("isCopyable", 4300000);
        Et = new j("embedLink", 4300000);
        Eu = new j("fileExtension", 4300000);
        Ev = new e("fileSize", 4300000);
        Ew = new a("isViewed", 4300000);
        Ex = new a("isRestricted", 4300000);
        Ey = new j("originalFilename", 4300000);
        Ez = new e("quotaBytesUsed", 4300000);
        EA = new j("webContentLink", 4300000);
        EB = new j("webViewLink", 4300000);
        EC = new j("indexableText", 4300000);
        ED = new a("hasThumbnail", 4300000);
    }
}
