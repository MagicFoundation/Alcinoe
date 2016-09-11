package com.embarcadero.firemonkey.medialibrary;

public class Size {
    private int mHeight;
    private int mWidth;

    public Size(int width, int height) {
        this.mWidth = 0;
        this.mHeight = 0;
        this.mWidth = width;
        this.mHeight = height;
    }

    public int getWidth() {
        return this.mWidth;
    }

    public int getHeight() {
        return this.mHeight;
    }

    public boolean isEmpty() {
        return this.mHeight <= 0 || this.mWidth <= 0;
    }
}
