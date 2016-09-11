package com.embarcadero.firemonkey.medialibrary;

import android.app.Activity;
import android.content.Intent;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.Bitmap.CompressFormat;
import android.graphics.BitmapFactory;
import android.graphics.BitmapFactory.Options;
import android.graphics.Matrix;
import android.media.ExifInterface;
import android.net.Uri;
import android.util.Log;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

public class MediaImage {
    private static final String ADD_PHOTO_TO_LIBRARY = "android.intent.action.MEDIA_SCANNER_SCAN_FILE";
    private static final String JPEG_FILE_PREFIX = "IMG_";
    private static final String JPEG_FILE_SUFFIX = ".jpg";
    private static final String TAG = "FireMonkey.MediaLibrary.MediaImage";
    private Activity mActivity;
    private Bitmap mBitmap;
    private boolean mFileChanged;
    private File mImageFile;

    public enum ImageStorePlace {
        CAMERA_PHOTO,
        CACHE
    }

    public static MediaImage loadFromUri(Activity activity, Uri imageUri, Size requiredSize) {
        MediaImage mediaImage = new MediaImage(activity, ImageStorePlace.CACHE);
        String fileName = mediaImage.getFileNameFromURI(imageUri);
        if (isContentImage(fileName) || isHttpImage(fileName)) {
            mediaImage.loadFromWeb(imageUri, requiredSize);
        } else {
            mediaImage.loadFromFile(fileName, requiredSize);
        }
        mediaImage.saveToTempFile();
        return mediaImage;
    }

    public MediaImage(Activity activity) {
        this.mActivity = null;
        this.mImageFile = null;
        this.mBitmap = null;
        this.mFileChanged = false;
        this.mActivity = activity;
        this.mFileChanged = false;
    }

    public MediaImage(Activity activity, ImageStorePlace imageType) {
        this.mActivity = null;
        this.mImageFile = null;
        this.mBitmap = null;
        this.mFileChanged = false;
        this.mActivity = activity;
        this.mFileChanged = false;
        if (imageType == ImageStorePlace.CAMERA_PHOTO) {
            this.mImageFile = createPhotoFile();
        } else {
            this.mImageFile = createTempFile();
        }
    }

    public void remove() {
        if (isFileExists()) {
            this.mImageFile.delete();
        }
    }

    public String getFileName() {
        return this.mImageFile.getAbsolutePath();
    }

    public Uri getFileUri() {
        return Uri.fromFile(this.mImageFile);
    }

    public boolean isFileExists() {
        return this.mImageFile != null && this.mImageFile.exists();
    }

    public boolean isImageLoaded() {
        return this.mBitmap != null;
    }

    public int getRotationAngle() {
        int rotationInDegrees = 0;
        try {
            rotationInDegrees = exifToDegrees(new ExifInterface(getFileName()).getAttributeInt("Orientation", 1));
        } catch (IOException e) {
            Log.e(TAG, e.getMessage(), e);
            e.printStackTrace();
        }
        return rotationInDegrees;
    }

    public void normalizeOrientation() {
        int angle = getRotationAngle();
        if (angle != 0) {
            Matrix matrix = new Matrix();
            matrix.preRotate((float) angle);
            Bitmap rotatedBitmap = Bitmap.createBitmap(this.mBitmap, 0, 0, this.mBitmap.getWidth(), this.mBitmap.getHeight(), matrix, true);
            unloadBitmap();
            this.mBitmap = rotatedBitmap;
            this.mFileChanged = true;
        }
    }

    public void addPhotoToGallery() {
        if (this.mActivity != null) {
            Intent intent = new Intent(ADD_PHOTO_TO_LIBRARY);
            intent.setData(getFileUri());
            this.mActivity.sendBroadcast(intent);
        }
    }

    public void saveToFile() {
        if (this.mFileChanged) {
            try {
                FileOutputStream out = new FileOutputStream(getFileName());
                this.mBitmap.compress(CompressFormat.JPEG, 100, out);
                out.close();
                this.mFileChanged = false;
            } catch (Exception e) {
                Log.e(TAG, e.getMessage(), e);
                e.printStackTrace();
            }
        }
    }

    public void saveToTempFile() {
        this.mImageFile = createTempFile();
        this.mFileChanged = true;
        saveToFile();
    }

    public void loadFromFile(Size requiredSize) {
        loadFromFile(getFileName(), requiredSize);
    }

    public void loadFromFile(String fileName, Size requiredSize) {
        File fileTmp = new File(fileName);
        unloadBitmap();
        if (fileTmp.exists()) {
            this.mImageFile = fileTmp;
            Options options = new Options();
            options.inJustDecodeBounds = true;
            BitmapFactory.decodeFile(fileName, options);
            options.inSampleSize = calculateInSampleSize(options.outWidth, options.outHeight, requiredSize);
            options.inJustDecodeBounds = false;
            options.inPurgeable = true;
            this.mBitmap = BitmapFactory.decodeFile(fileName, options);
            if (options.inSampleSize > 1) {
                this.mFileChanged = true;
                return;
            }
            return;
        }
        this.mBitmap = null;
        this.mFileChanged = false;
    }

    private void loadFromWeb(Uri fileUri, Size requiredSize) {
        Exception e;
        try {
            InputStream is;
            String fileName = getFileNameFromURI(fileUri);
            if (isContentImage(fileName)) {
                is = this.mActivity.getContentResolver().openInputStream(Uri.parse(fileName));
            } else {
                is = new URL(fileName).openStream();
            }
            OutputStream os = new FileOutputStream(this.mImageFile);
            OutputStream outputStream;
            try {
                Utils.copyStream(is, os);
                os.close();
                loadFromFile(this.mImageFile.getAbsolutePath(), requiredSize);
                outputStream = os;
            } catch (Exception e2) {
                e = e2;
                outputStream = os;
                Log.e(TAG, e.getMessage(), e);
                e.printStackTrace();
            } catch (Throwable th) {
                os.close();
            }
        } catch (Exception e3) {
            e = e3;
            Log.e(TAG, e.getMessage(), e);
            e.printStackTrace();
        }
    }

    public void unloadBitmap() {
        if (this.mBitmap != null) {
            this.mBitmap.recycle();
        }
        this.mBitmap = null;
    }

    private File createTempFile() {
        try {
            File fileTemp = File.createTempFile(JPEG_FILE_PREFIX + new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.US).format(new Date()), JPEG_FILE_SUFFIX);
            fileTemp.deleteOnExit();
            return fileTemp;
        } catch (IOException e) {
            Log.e(TAG, e.getMessage(), e);
            e.printStackTrace();
            return null;
        }
    }

    private File createPhotoFile() {
        return new File(Utils.getPhotosDir(), JPEG_FILE_PREFIX + new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.US).format(new Date()) + JPEG_FILE_SUFFIX);
    }

    private int calculateInSampleSize(int width, int height, Size reqSize) {
        int inSampleSize = 1;
        if (height > reqSize.getHeight() || width > reqSize.getWidth()) {
            while (true) {
                if (height / inSampleSize <= reqSize.getHeight() && width / inSampleSize <= reqSize.getWidth()) {
                    break;
                }
                inSampleSize *= 2;
            }
        }
        return inSampleSize;
    }

    private String getFileNameFromURI(Uri fileUri) {
        if (fileUri != null && fileUri.toString().startsWith("content://com.android.gallery3d.provider")) {
            fileUri = Uri.parse(fileUri.toString().replace("com.android.gallery3d", "com.google.android.gallery3d"));
        }
        String filePath = null;
        Cursor cursor = this.mActivity.getContentResolver().query(fileUri, new String[]{"_data"}, null, null, null);
        if (cursor != null) {
            try {
                if (cursor.moveToFirst()) {
                    int column_index_data = cursor.getColumnIndexOrThrow("_data");
                    if (column_index_data != -1) {
                        filePath = cursor.getString(column_index_data);
                    }
                }
            } catch (Throwable th) {
                if (cursor != null) {
                    cursor.close();
                }
            }
        }
        if (cursor != null) {
            cursor.close();
        }
        if (filePath == null) {
            return fileUri.toString();
        }
        return filePath;
    }

    private static int exifToDegrees(int exifOrientation) {
        if (exifOrientation == 6) {
            return 90;
        }
        if (exifOrientation == 3) {
            return 180;
        }
        if (exifOrientation == 8) {
            return 270;
        }
        return 0;
    }

    private static boolean isContentImage(String str) {
        if (str != null) {
            return str.startsWith("content://");
        }
        return false;
    }

    private static boolean isHttpImage(String str) {
        if (str == null) {
            return false;
        }
        if (str.startsWith("http://") || str.startsWith("https://")) {
            return true;
        }
        return false;
    }

    public boolean cropImage(Uri selectedUri) {
        Intent intent = new Intent("com.android.camera.action.CROP");
        File tempFile = createPhotoFile();
        if (selectedUri == null) {
            selectedUri = getFileUri();
        }
        intent.setDataAndType(selectedUri, "image/*");
        intent.putExtra("return_data", false);
        intent.putExtra("output", Uri.fromFile(tempFile));
        if (intent.resolveActivity(this.mActivity.getPackageManager()) == null) {
            return false;
        }
        this.mImageFile = tempFile;
        this.mActivity.startActivityForResult(intent, 3);
        return true;
    }
}
