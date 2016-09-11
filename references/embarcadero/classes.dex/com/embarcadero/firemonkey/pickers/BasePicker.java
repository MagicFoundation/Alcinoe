package com.embarcadero.firemonkey.pickers;

public abstract class BasePicker {
    protected int mTheme;

    public abstract void hide();

    public abstract boolean isShown();

    public abstract void show();

    public BasePicker() {
        this.mTheme = 0;
    }

    public void setTheme(int theme) {
        this.mTheme = theme;
    }
}
