package com.google.android.gms.drive.query;

import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.metadata.CollectionMetadataField;
import com.google.android.gms.drive.metadata.MetadataField;
import com.google.android.gms.drive.metadata.OrderedMetadataField;
import com.google.android.gms.internal.fs;
import com.google.android.gms.internal.ft;
import java.util.Date;

public class SearchableField {
    public static final OrderedMetadataField<Date> EH;
    public static final MetadataField<Boolean> IS_PINNED;
    public static final OrderedMetadataField<Date> LAST_VIEWED_BY_ME;
    public static final MetadataField<String> MIME_TYPE;
    public static final OrderedMetadataField<Date> MODIFIED_DATE;
    public static final CollectionMetadataField<DriveId> PARENTS;
    public static final MetadataField<Boolean> STARRED;
    public static final MetadataField<String> TITLE;
    public static final MetadataField<Boolean> TRASHED;

    static {
        TITLE = fs.TITLE;
        MIME_TYPE = fs.MIME_TYPE;
        TRASHED = fs.TRASHED;
        PARENTS = fs.PARENTS;
        EH = ft.EH;
        STARRED = fs.STARRED;
        MODIFIED_DATE = ft.EE;
        LAST_VIEWED_BY_ME = ft.LAST_VIEWED_BY_ME;
        IS_PINNED = fs.IS_PINNED;
    }
}
