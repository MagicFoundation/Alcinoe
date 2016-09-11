package com.embarcadero.firemonkey.camerapreview;

import android.content.Context;
import android.graphics.Canvas;
import android.hardware.Camera;
import android.hardware.Camera.Parameters;
import android.view.SurfaceHolder;
import android.view.SurfaceHolder.Callback;
import android.view.SurfaceView;
import java.io.IOException;

public class CamPreview extends SurfaceView implements Callback {
    public Camera mCamera;
    private SurfaceHolder mHolder;

    public CamPreview(Context context) {
        super(context);
        this.mHolder = getHolder();
        this.mHolder.addCallback(this);
        this.mHolder.setType(3);
    }

    public void surfaceCreated(SurfaceHolder holder) {
        this.mCamera = Camera.open(1);
        try {
            this.mCamera.setPreviewDisplay(holder);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void surfaceDestroyed(SurfaceHolder holder) {
        this.mCamera.stopPreview();
        this.mCamera = null;
    }

    public void surfaceChanged(SurfaceHolder holder, int format, int w, int h) {
        Parameters parameters = this.mCamera.getParameters();
        parameters.setPreviewSize(w, h);
        this.mCamera.setParameters(parameters);
        this.mCamera.startPreview();
    }

    public void draw(Canvas canvas) {
        super.draw(canvas);
    }
}
