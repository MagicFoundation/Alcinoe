package com.google.android.gms.drive;

import com.google.android.gms.drive.metadata.internal.MetadataBundle;
import com.google.android.gms.internal.fs;
import com.google.android.gms.internal.ft;
import java.util.Date;

public final class MetadataChangeSet {
    private final MetadataBundle Dj;

    public static class Builder {
        private final MetadataBundle Dj;

        public Builder() {
            this.Dj = MetadataBundle.fh();
        }

        public MetadataChangeSet build() {
            return new MetadataChangeSet(null);
        }

        public Builder setIndexableText(String text) {
            this.Dj.b(fs.EC, text);
            return this;
        }

        public Builder setLastViewedByMeDate(Date date) {
            this.Dj.b(ft.LAST_VIEWED_BY_ME, date);
            return this;
        }

        public Builder setMimeType(String mimeType) {
            this.Dj.b(fs.MIME_TYPE, mimeType);
            return this;
        }

        public Builder setPinned(boolean pinned) {
            this.Dj.b(fs.IS_PINNED, Boolean.valueOf(pinned));
            return this;
        }

        public Builder setStarred(boolean starred) {
            this.Dj.b(fs.STARRED, Boolean.valueOf(starred));
            return this;
        }

        public Builder setTitle(String title) {
            this.Dj.b(fs.TITLE, title);
            return this;
        }

        public Builder setViewed(boolean viewed) {
            this.Dj.b(fs.Ew, Boolean.valueOf(viewed));
            return this;
        }
    }

    private MetadataChangeSet(MetadataBundle bag) {
        this.Dj = MetadataBundle.a(bag);
    }

    public MetadataBundle eS() {
        return this.Dj;
    }

    public String getIndexableText() {
        return (String) this.Dj.a(fs.EC);
    }

    public Date getLastViewedByMeDate() {
        return (Date) this.Dj.a(ft.LAST_VIEWED_BY_ME);
    }

    public String getMimeType() {
        return (String) this.Dj.a(fs.MIME_TYPE);
    }

    public String getTitle() {
        return (String) this.Dj.a(fs.TITLE);
    }

    public Boolean isPinned() {
        return (Boolean) this.Dj.a(fs.IS_PINNED);
    }

    public Boolean isStarred() {
        return (Boolean) this.Dj.a(fs.STARRED);
    }

    public Boolean isViewed() {
        return (Boolean) this.Dj.a(fs.Ew);
    }
}
