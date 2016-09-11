package com.google.android.gms.drive;

import com.google.android.gms.common.data.Freezable;
import com.google.android.gms.drive.metadata.MetadataField;
import com.google.android.gms.internal.fs;
import com.google.android.gms.internal.ft;
import com.google.android.gms.internal.fv;
import java.util.Date;

public abstract class Metadata implements Freezable<Metadata> {
    public static final int CONTENT_AVAILABLE_LOCALLY = 1;
    public static final int CONTENT_NOT_AVAILABLE_LOCALLY = 0;

    protected abstract <T> T a(MetadataField<T> metadataField);

    public String getAlternateLink() {
        return (String) a(fs.Ep);
    }

    public int getContentAvailability() {
        Integer num = (Integer) a(fv.EJ);
        return num == null ? 0 : num.intValue();
    }

    public Date getCreatedDate() {
        return (Date) a(ft.EG);
    }

    public String getDescription() {
        return (String) a(fs.Er);
    }

    public DriveId getDriveId() {
        return (DriveId) a(fs.El);
    }

    public String getEmbedLink() {
        return (String) a(fs.Et);
    }

    public String getFileExtension() {
        return (String) a(fs.Eu);
    }

    public long getFileSize() {
        return ((Long) a(fs.Ev)).longValue();
    }

    public Date getLastViewedByMeDate() {
        return (Date) a(ft.LAST_VIEWED_BY_ME);
    }

    public String getMimeType() {
        return (String) a(fs.MIME_TYPE);
    }

    public Date getModifiedByMeDate() {
        return (Date) a(ft.EF);
    }

    public Date getModifiedDate() {
        return (Date) a(ft.EE);
    }

    public String getOriginalFilename() {
        return (String) a(fs.Ey);
    }

    public long getQuotaBytesUsed() {
        return ((Long) a(fs.Ez)).longValue();
    }

    public Date getSharedWithMeDate() {
        return (Date) a(ft.EH);
    }

    public String getTitle() {
        return (String) a(fs.TITLE);
    }

    public String getWebContentLink() {
        return (String) a(fs.EA);
    }

    public String getWebViewLink() {
        return (String) a(fs.EB);
    }

    public boolean isEditable() {
        Boolean bool = (Boolean) a(fs.Em);
        return bool == null ? false : bool.booleanValue();
    }

    public boolean isFolder() {
        return DriveFolder.MIME_TYPE.equals(getMimeType());
    }

    public boolean isInAppFolder() {
        Boolean bool = (Boolean) a(fs.En);
        return bool == null ? false : bool.booleanValue();
    }

    public boolean isPinnable() {
        Boolean bool = (Boolean) a(fv.EK);
        return bool == null ? false : bool.booleanValue();
    }

    public boolean isPinned() {
        Boolean bool = (Boolean) a(fs.IS_PINNED);
        return bool == null ? false : bool.booleanValue();
    }

    public boolean isRestricted() {
        Boolean bool = (Boolean) a(fs.Ex);
        return bool == null ? false : bool.booleanValue();
    }

    public boolean isShared() {
        Boolean bool = (Boolean) a(fs.Eo);
        return bool == null ? false : bool.booleanValue();
    }

    public boolean isStarred() {
        Boolean bool = (Boolean) a(fs.STARRED);
        return bool == null ? false : bool.booleanValue();
    }

    public boolean isTrashed() {
        Boolean bool = (Boolean) a(fs.TRASHED);
        return bool == null ? false : bool.booleanValue();
    }

    public boolean isViewed() {
        Boolean bool = (Boolean) a(fs.Ew);
        return bool == null ? false : bool.booleanValue();
    }
}
