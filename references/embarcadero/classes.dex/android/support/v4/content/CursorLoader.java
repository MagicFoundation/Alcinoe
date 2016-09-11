package android.support.v4.content;

import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.support.v4.content.Loader.ForceLoadContentObserver;
import java.io.FileDescriptor;
import java.io.PrintWriter;
import java.util.Arrays;

public class CursorLoader extends AsyncTaskLoader<Cursor> {
    Cursor mCursor;
    final ForceLoadContentObserver mObserver;
    String[] mProjection;
    String mSelection;
    String[] mSelectionArgs;
    String mSortOrder;
    Uri mUri;

    public Cursor loadInBackground() {
        Cursor cursor = getContext().getContentResolver().query(this.mUri, this.mProjection, this.mSelection, this.mSelectionArgs, this.mSortOrder);
        if (cursor != null) {
            cursor.getCount();
            cursor.registerContentObserver(this.mObserver);
        }
        return cursor;
    }

    public void deliverResult(Cursor cursor) {
        if (!isReset()) {
            Cursor oldCursor = this.mCursor;
            this.mCursor = cursor;
            if (isStarted()) {
                super.deliverResult(cursor);
            }
            if (oldCursor != null && oldCursor != cursor && !oldCursor.isClosed()) {
                oldCursor.close();
            }
        } else if (cursor != null) {
            cursor.close();
        }
    }

    public CursorLoader(Context context) {
        super(context);
        this.mObserver = new ForceLoadContentObserver();
    }

    public CursorLoader(Context context, Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        super(context);
        this.mObserver = new ForceLoadContentObserver();
        this.mUri = uri;
        this.mProjection = projection;
        this.mSelection = selection;
        this.mSelectionArgs = selectionArgs;
        this.mSortOrder = sortOrder;
    }

    protected void onStartLoading() {
        if (this.mCursor != null) {
            deliverResult(this.mCursor);
        }
        if (takeContentChanged() || this.mCursor == null) {
            forceLoad();
        }
    }

    protected void onStopLoading() {
        cancelLoad();
    }

    public void onCanceled(Cursor cursor) {
        if (cursor != null && !cursor.isClosed()) {
            cursor.close();
        }
    }

    protected void onReset() {
        super.onReset();
        onStopLoading();
        if (!(this.mCursor == null || this.mCursor.isClosed())) {
            this.mCursor.close();
        }
        this.mCursor = null;
    }

    public Uri getUri() {
        return this.mUri;
    }

    public void setUri(Uri uri) {
        this.mUri = uri;
    }

    public String[] getProjection() {
        return this.mProjection;
    }

    public void setProjection(String[] projection) {
        this.mProjection = projection;
    }

    public String getSelection() {
        return this.mSelection;
    }

    public void setSelection(String selection) {
        this.mSelection = selection;
    }

    public String[] getSelectionArgs() {
        return this.mSelectionArgs;
    }

    public void setSelectionArgs(String[] selectionArgs) {
        this.mSelectionArgs = selectionArgs;
    }

    public String getSortOrder() {
        return this.mSortOrder;
    }

    public void setSortOrder(String sortOrder) {
        this.mSortOrder = sortOrder;
    }

    public void dump(String prefix, FileDescriptor fd, PrintWriter writer, String[] args) {
        super.dump(prefix, fd, writer, args);
        writer.print(prefix);
        writer.print("mUri=");
        writer.println(this.mUri);
        writer.print(prefix);
        writer.print("mProjection=");
        writer.println(Arrays.toString(this.mProjection));
        writer.print(prefix);
        writer.print("mSelection=");
        writer.println(this.mSelection);
        writer.print(prefix);
        writer.print("mSelectionArgs=");
        writer.println(Arrays.toString(this.mSelectionArgs));
        writer.print(prefix);
        writer.print("mSortOrder=");
        writer.println(this.mSortOrder);
        writer.print(prefix);
        writer.print("mCursor=");
        writer.println(this.mCursor);
        writer.print(prefix);
        writer.print("mContentChanged=");
        writer.println(this.mContentChanged);
    }
}
