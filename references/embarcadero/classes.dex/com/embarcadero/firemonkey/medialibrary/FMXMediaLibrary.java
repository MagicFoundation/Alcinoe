package com.embarcadero.firemonkey.medialibrary;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore.Images.Media;
import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat;
import com.embarcadero.firemonkey.medialibrary.MediaImage.ImageStorePlace;

public class FMXMediaLibrary {
    public static final int ACTION_CROP_IMAGE = 3;
    public static final int ACTION_TAKE_IMAGE_FROM_CAMERA = 1;
    public static final int ACTION_TAKE_IMAGE_FROM_LIBRARY = 2;
    private static final String PHOTO_PATH_STORAGE_KEY = "photo_path";
    private Activity mActivity;
    private boolean mEditable;
    private MediaImage mImage;
    private FMXMediaLibraryListener mListener;
    private Size mMaxSize;
    private boolean mNeedSaveToAlbum;

    public FMXMediaLibrary(Activity activity) {
        this.mActivity = null;
        this.mImage = null;
        this.mMaxSize = new Size(AccessibilityNodeInfoCompat.ACTION_NEXT_HTML_ELEMENT, AccessibilityNodeInfoCompat.ACTION_NEXT_HTML_ELEMENT);
        this.mEditable = false;
        this.mNeedSaveToAlbum = false;
        this.mActivity = activity;
    }

    public static boolean isRequestForTakingImage(int requestCode) {
        return requestCode == ACTION_TAKE_IMAGE_FROM_CAMERA || requestCode == ACTION_TAKE_IMAGE_FROM_LIBRARY || requestCode == ACTION_CROP_IMAGE;
    }

    public void takeImageFromCamera(int maxWidth, int maxHeight, boolean editable, boolean needSaveToAlbum) {
        if (hasActivity()) {
            this.mMaxSize = new Size(maxWidth, maxHeight);
            this.mEditable = editable;
            this.mNeedSaveToAlbum = needSaveToAlbum;
            this.mImage = new MediaImage(this.mActivity, ImageStorePlace.CAMERA_PHOTO);
            Intent intent = new Intent("android.media.action.IMAGE_CAPTURE");
            intent.putExtra("output", this.mImage.getFileUri());
            this.mActivity.startActivityForResult(intent, ACTION_TAKE_IMAGE_FROM_CAMERA);
        }
    }

    public void takeImageFromLibrary(int maxWidth, int maxHeight, boolean editable) {
        if (hasActivity()) {
            this.mImage = null;
            this.mMaxSize = new Size(maxWidth, maxHeight);
            this.mEditable = editable;
            this.mNeedSaveToAlbum = false;
            Intent intent = new Intent("android.intent.action.PICK");
            intent.setData(Media.EXTERNAL_CONTENT_URI);
            this.mActivity.startActivityForResult(intent, ACTION_TAKE_IMAGE_FROM_LIBRARY);
        }
    }

    public void handleTakingPhotoRequest(Intent data, int requestCode) {
        switch (requestCode) {
            case ACTION_TAKE_IMAGE_FROM_CAMERA /*1*/:
                handleCameraPhoto();
            case ACTION_TAKE_IMAGE_FROM_LIBRARY /*2*/:
                handleLibraryPhoto(data);
            case ACTION_CROP_IMAGE /*3*/:
                handleCrop(data);
            default:
        }
    }

    private void postHandleCameraPhoto() {
        this.mImage.loadFromFile(this.mMaxSize);
        try {
            this.mImage.normalizeOrientation();
            this.mImage.saveToFile();
            if (this.mNeedSaveToAlbum) {
                this.mImage.addPhotoToGallery();
            }
            if (this.mListener != null) {
                this.mListener.OnMediaLibraryAccept();
            }
            this.mImage.unloadBitmap();
        } catch (Throwable th) {
            this.mImage.unloadBitmap();
        }
    }

    private void handleCameraPhoto() {
        if (!this.mImage.isFileExists()) {
            return;
        }
        if (!this.mEditable) {
            postHandleCameraPhoto();
        } else if (!this.mImage.cropImage(this.mImage.getFileUri())) {
            postHandleCameraPhoto();
        }
    }

    private void postHandleLibraryPhoto(Uri uri) {
        try {
            this.mImage = MediaImage.loadFromUri(this.mActivity, uri, this.mMaxSize);
            this.mImage.normalizeOrientation();
            this.mImage.saveToFile();
            if (this.mListener != null) {
                this.mListener.OnMediaLibraryAccept();
            }
            this.mImage.unloadBitmap();
        } catch (Throwable th) {
            this.mImage.unloadBitmap();
        }
    }

    private void handleLibraryPhoto(Intent data) {
        Uri uri = data.getData();
        if (uri == null) {
            return;
        }
        if (this.mEditable) {
            this.mImage = new MediaImage(this.mActivity);
            if (!this.mImage.cropImage(uri)) {
                postHandleLibraryPhoto(uri);
                return;
            }
            return;
        }
        postHandleLibraryPhoto(uri);
    }

    private void handleCrop(Intent data) {
        if (this.mImage.isFileExists()) {
            this.mImage.loadFromFile(this.mMaxSize);
            try {
                this.mImage.normalizeOrientation();
                this.mImage.saveToFile();
                if (this.mListener != null) {
                    this.mListener.OnMediaLibraryAccept();
                }
                this.mImage.unloadBitmap();
            } catch (Throwable th) {
                this.mImage.unloadBitmap();
            }
        }
    }

    public String getLastPhotoName() {
        if (this.mImage != null) {
            return this.mImage.getFileName();
        }
        return null;
    }

    private boolean hasActivity() {
        return this.mActivity != null;
    }

    public void onSaveInstanceState(Bundle outState) {
        if (this.mImage != null) {
            outState.putString(PHOTO_PATH_STORAGE_KEY, this.mImage.getFileName());
        }
    }

    public void onRestoreInstanceState(Bundle savedInstanceState) {
        String photoPath = savedInstanceState.getString(PHOTO_PATH_STORAGE_KEY);
        if (photoPath != null) {
            if (this.mImage == null) {
                this.mImage = new MediaImage(this.mActivity);
            }
            this.mImage.loadFromFile(photoPath, this.mMaxSize);
        }
    }

    public void setListener(FMXMediaLibraryListener listener) {
        this.mListener = listener;
    }
}
