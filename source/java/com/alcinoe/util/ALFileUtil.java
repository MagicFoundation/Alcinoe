package com.alcinoe.util;

import java.io.File;
import java.io.OutputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import android.content.Context;
import android.net.Uri;
import android.util.Log;

public class ALFileUtil {

  public static boolean createFileFromURI (Context context, Uri uri, String fileName) {
  
    try{

      InputStream inputStream = context.getContentResolver().openInputStream(uri);
      File f = new File(fileName);
      f.setWritable(true/*writable*/, false /*ownerOnly*/);
      OutputStream outputStream = new FileOutputStream(f);
      byte buffer[] = new byte[1024];
      int length = 0;  
    
      while((length=inputStream.read(buffer)) != -1) {
        outputStream.write(buffer,0,length);
      }
    
      outputStream.close();
      inputStream.close();
    
    }     
    catch (Throwable e){ 
      Log.e("ALFileUtil", "createFileFromURI - Exception", e);  
      return false;
    }
    
    return true;
    
  }
   
}