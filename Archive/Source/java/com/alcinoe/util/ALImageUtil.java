package com.alcinoe.util;

import java.io.File;
import java.io.FileOutputStream;
import android.media.ExifInterface;
import android.graphics.Matrix;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Bitmap.CompressFormat;
import android.util.Log;

public class ALImageUtil {

  public static boolean rotateImageIfRequired(String imagePath) {

    try {

      ExifInterface exif = new ExifInterface(imagePath);
      int orientation = exif.getAttributeInt(ExifInterface.TAG_ORIENTATION, ExifInterface.ORIENTATION_NORMAL);
      Matrix matrix = new Matrix();
      switch (orientation) {

        case ExifInterface.ORIENTATION_NORMAL:
            return true;

        case ExifInterface.ORIENTATION_FLIP_HORIZONTAL:
            matrix.setScale(-1, 1);
            break;

        case ExifInterface.ORIENTATION_ROTATE_180:
            matrix.setRotate(180);
            break;

        case ExifInterface.ORIENTATION_FLIP_VERTICAL:
            matrix.setRotate(180);
            matrix.postScale(-1, 1);
            break;

        case ExifInterface.ORIENTATION_TRANSPOSE:
            matrix.setRotate(90);
            matrix.postScale(-1, 1);
            break;

       case ExifInterface.ORIENTATION_ROTATE_90:
           matrix.setRotate(90);
           break;

       case ExifInterface.ORIENTATION_TRANSVERSE:
           matrix.setRotate(-90);
           matrix.postScale(-1, 1);
           break;

       case ExifInterface.ORIENTATION_ROTATE_270:
           matrix.setRotate(-90);
           break;

       default:
           return true;

      }
            
      Bitmap bitmap = BitmapFactory.decodeFile(imagePath);
      Bitmap bmRotated = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), matrix, true);
      bitmap.recycle();
 
      File file = new File(imagePath);
      //Log.v("ALImageUtil", "File size before compress: " + file.length());  
      file.delete();

      //long startTime = System.currentTimeMillis();

      FileOutputStream fileoutputstream = new FileOutputStream(imagePath);
      bmRotated.compress(CompressFormat.JPEG, 95, fileoutputstream);
      fileoutputstream.flush();
      fileoutputstream.close();
      bmRotated.recycle();
      
      //long elapsedTime = System.currentTimeMillis() - startTime;
      //Log.v("ALImageUtil", "compress - timetaken: " + elapsedTime + "ms");  
      //file = new File(imagePath);
      //Log.v("ALImageUtil", "File size after compress: " + file.length());  
      
      return true;

    }
    catch (Throwable e){ 
      Log.e("ALImageUtil", "rotateImageIfRequired - Exception", e);  
      return false;
    }    

  }

}