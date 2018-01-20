//
//  Android Java Interface for Pascal and Delphi
//
//   Developer
//              Simon,Choi / Choi,Won-sik
//                           simonsayz@naver.com
//                           http://blog.naver.com/simonsayz
//
//              LoadMan    / Jang,Yang-Ho
//                           wkddidgh@naver.com
//
//
//              jmpessoa   / Jose Marques Pessoa
//                           jmpessoa@hotmail.com   
//                           https://github.com/jmpessoa/lazandroidmodulewizard   
//
//   Ref.
//     1. Non-Visual Lib. by Simon,Choi & LoadMan
//           http://blog.naver.com/simonsayz
//
//     2. Visual Lib. by jmpessoa
//           https://github.com/jmpessoa/lazandroidmodulewizard    
//
//
//   Version History
//
//               2013.02.24 ver0.01 Started
//               2013.02.28 ver0.02 added Delphi Style
//               2013.03.01 ver0.03 added sysInfo
//               2013.03.05 ver0.04 added Java Loading Png
//               2013.03.08 ver0.05 Restructuring (Interlation #.02)
//               2013.07.13 ver0.06 added TForm
//               2013.07.22 ver0.07 added Back Event for Close
//               2013.07.26 ver0.08 Class,Method Cache (Single Thread,Class)
//               2013.07.30 ver0.09 added TEditText Keyboard,Focus
//               2013.08.02 ver0.10 added TextView - Enabled
//               2013.08.05 ver0.11 added Form Object
//               2013.08.11 ver0.12 added Canvas
//                                  added Direct Bitmap access
//               2013.08.14 ver0.13 Fixed Memory Leak
//               2013.08.18 ver0.14 added OpenGL ES1 2D (Stencil)
//               2013.08.21 ver0.15 Fixed jImageBtn Memory Leak
//                                  Fixed Socket Buffer
//               2013.08.23 ver0.16 Fixed Memory Leak for Form,Control
//                                  added Form Stack
//               2013.08.24 ver0.17 added Thread
//               2013.08.26 ver0.18 added OpenGL ES2 2D/3D
//                                  added Button Font Color/Height 
//               2013.08.31 ver0.19 added Unified OpenGL ES1,2 Canvas
//                                  added OpenGL ES1,2 Simulator for Windows  
//               2013.09.01 ver0.20 added GLThread on Canvas
//                                  Fixed OpenGL Crash
//                                  rename example Name
//               2014.11.17         added CameraView
//               2014.12.22 ver0.21 Restructuring (Interlation #.03)
//          
//
package com.mormot;

import android.app.Activity;
import android.app.ActivityManager.RunningAppProcessInfo;
import android.app.ActivityManager.RunningServiceInfo;
import android.app.ActivityManager;
import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.PackageManager;
import android.graphics.Bitmap.CompressFormat;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.ImageFormat;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.YuvImage;
import android.hardware.Camera.AutoFocusCallback;
import android.hardware.Camera.Parameters;
import android.hardware.Camera.PictureCallback;
import android.hardware.Camera.ShutterCallback;
import android.hardware.Camera;
import android.net.Uri;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.opengl.GLES10;
import android.opengl.GLES20;
import android.opengl.GLSurfaceView.Renderer;
import android.opengl.GLSurfaceView;
import android.opengl.GLU;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Build.VERSION_CODES;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.PowerManager;
import android.os.PowerManager.WakeLock;
import android.provider.MediaStore;
import android.telephony.TelephonyManager;
import android.text.Editable;
import android.text.InputFilter;
import android.text.InputType;
import android.text.TextWatcher;
import android.util.DisplayMetrics;
import android.util.Log;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.SurfaceHolder;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View.OnClickListener;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.view.animation.AccelerateInterpolator;
import android.view.animation.AlphaAnimation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.Animation;
import android.view.animation.AnimationSet;
import android.view.animation.AnimationUtils;
import android.view.animation.TranslateAnimation;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.HorizontalScrollView;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.RadioButton;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;
import android.widget.ViewFlipper;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import javax.microedition.khronos.egl.EGL10;
import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.egl.EGLContext;
import javax.microedition.khronos.opengles.GL10;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.util.ByteArrayBuffer;
import org.apache.http.util.EntityUtils;

import android.app.ActionBar;


// -------------------------------------------------------------------------
//   Constants
// -------------------------------------------------------------------------
class Const {
  public static final String LogHeader                  = "AndCtrls_Java";
  //
  public static final int Version_Base                  =     1; // 2008 Oct.
  public static final int Version_Base_1_1              =     2; // 2009 Feb. Android 1.1
  public static final int Version_CupCake               =     3; // 2009 May. Android 1.5
  public static final int Version_Donut                 =     4; // 2009 Sep. Android 1.6
  public static final int Version_Eclair                =     5; // 2009 Nov. Android 2.0
  public static final int Version_Eclair_0_1            =     6; // 2009 Dec. Android 2.0.1
  public static final int Version_Eclair_MR1            =     7; // 2010 Jan. Android 2.1
  public static final int Version_Froyo                 =     8; // 2010 Jun  Android 2.2 
  public static final int Version_GingerBread           =     9; // 2010 Nov. Android 2.3
  public static final int Version_GingerBread_MR1       =    10; // 2011 Feb. Android 2.3.3
  public static final int Version_HoneyComb             =    11; // 2011 Feb. Android 3.0
  public static final int Version_HoneyComb_MR1         =    12; // 2011 May. Android 3.1
  public static final int Version_HoneyComb_MR2         =    13; // 2011 Jun. Android 3.2
  public static final int Version_Ice_Cream_SandWich    =    14; // 2011 Oct. Android 4.0
  public static final int Version_Ice_Cream_SandWich_MR1=    15; // 2011 Dec. Android 4.0.3
  public static final int Version_Jelly_Bean            =    16; // 2012 Jun. Android 4.1
  public static final int Version_Jelly_Bean_MR1        =    17; // 2012 Nov. Android 4.2
  public static final int Version_Jelly_Bean_MR2        =    18; // 2013 Jul. Android 4.3
  public static final int Version_KitKat                =    19; // 2013 Oct. Android 4.4
  public static final int Version_KitKat_Watch          =    20; //           Android 4.4W
  public static final int Version_LolliPop              =    21; // 
  public static final int Version_Cur_Development       = 10000; // 
  //
  public static final int DIRECTORY_ALARMS              =     0; 
  public static final int DIRECTORY_DCIM                =     1;
  public static final int DIRECTORY_DOCUMENTS           =     2;
  public static final int DIRECTORY_DOWNLOADS           =     3;
  public static final int DIRECTORY_MOVIES              =     4;
  public static final int DIRECTORY_MUSIC               =     5;
  public static final int DIRECTORY_NOTIFICATIONS       =     6;
  public static final int DIRECTORY_PICTURES            =     7;
  public static final int DIRECTORY_PODCASTS            =     8;
  public static final int DIRECTORY_RINGTONES           =     9;
  public static final int DIRECTORY_App                 = 10001;
  public static final int DIRECTORY_Files               = 10002; 
  public static final int DIRECTORY_SDCard              = 10003;
  public static final int DIRECTORY_DataBase            = 10004;
  public static final int DIRECTORY_Shared_Prefs        = 10005;
  
  //
  public static final int ScreenStyle_Normal            =  0;
  public static final int ScreenStyle_Full              =  1;
  //
  public static final int Network_None                  =  0;
  public static final int Network_Wifi                  =  1;
  public static final int Network_Mobile                =  2;
  //
  public static final int Paint_Style_Fill              =  0;
  public static final int Paint_Style_Fill_And_Stroke   =  1;
  public static final int Paint_Style_Stroke            =  2;
  //
  public static final int CompressFormat_PNG            =  0;
  public static final int CompressFormat_JPEG           =  1;
  //
  public static final int Touch_Down                    =  0;
  public static final int Touch_Move                    =  1;
  public static final int Touch_Up                      =  2;
  //
  public static final int Click_Default                 =  0;
  public static final int Click_Yes                     = -1;
  public static final int Click_No                      = -2;
  //
  public static final int Eft_None                      = 0x00000000;
  public static final int Eft_iR2L                      = 0x00000001;
  public static final int Eft_oR2L                      = 0x00000002;
  public static final int Eft_iL2R                      = 0x00000004;
  public static final int Eft_oL2R                      = 0x00000008;
  public static final int Eft_FadeIn                    = 0x00000010;
  public static final int Eft_FadeOut                   = 0x00000020;
  //
  public static final int GLView_State_SurfaceCreated   =  0;
  public static final int GLView_State_SurfaceChanged   =  1;
  public static final int GLView_State_DrawFrame        =  2;
  public static final int GLView_State_SurfaceDestroyed =  3;
  public static final int GLView_State_SurfaceThread    =  4;
  //
  public static final int WebView_Act_Continue          =  0;
  public static final int WebView_Act_Break             =  1;

  public static final int WebView_State_Unknown         =  0;
  public static final int WebView_State_Before          =  1;
  public static final int WebView_State_Finish          =  2;
  public static final int WebView_State_Error           =  3;
  //
  public static final int AsyncTask_State_PreExecute    =  0;
  public static final int AsyncTask_State_Update        =  1;
  public static final int AsyncTask_State_PostExecute   =  2;
  public static final int AsyncTask_State_BackGround    =  3;
  //
  public static final int Edit_Style_SingleLine         =  0;
  public static final int Edit_Style_MultiLine          =  1;
  //
  public static final int Edit_Type_Number              =  0;
  public static final int Edit_Type_Text                =  1;
  public static final int Edit_Type_Phone               =  2;
  public static final int Edit_Type_PassNumber          =  3;
  public static final int Edit_Type_PassText            =  4;
  //
  public static final int Http_Act_Text                 =  0;
  public static final int Http_Act_Download             =  1;
  public static final int Http_Act_Upload               =  2;
  //
  public static final int Http_State_Xfer               =  0;
  public static final int Http_State_Done               =  1;
  public static final int Http_State_Error              =  2;
  //
  public static final String Http_Boundary              = "---------------------------7df9330d90b18";
}

// -------------------------------------------------------------------------
//  Timer
//          Event : pOnTimer
// http://daddycat.blogspot.kr/2011/05/android-thread-ui.html
// http://lsit81.tistory.com/entry/ActivityrunOnUiThread%EC%99%80-post%EC%9D%98-%EC%B0%A8%EC%9D%B4%EC%A0%90
// -------------------------------------------------------------------------
class jTimer {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  // Property
  private boolean         Enabled = false;   // default : false
  private int             Interval = 1000;   // 1000msec
  // Java Object
  private Runnable runnable;
  private Handler  handler;

  // Constructor
  public  jTimer(Controls ctrls, int pasobj) {
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    //
    // Init Class
    handler  = new Handler();
    runnable = new Runnable() {
     @Override
     public  final void run() {
         if (Enabled) {
              controls.pOnTimer(PasObj); // Pascal Event
              if (handler != null)
               handler.postDelayed(runnable,Interval);
         };
       }
    };
  }
  
  public  void SetInterval(int interval) {
    Interval = interval;
  }

  public  void SetEnabled(boolean enabled) {
    if (Enabled == enabled) return;
    Enabled = enabled;
    if (Enabled) { handler.postDelayed(runnable,Interval); };
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    Enabled  = false;
    handler  = null;
    runnable = null;
  }

};

// -------------------------------------------------------------------------
//  Form
//          Event : pOnClick
// -------------------------------------------------------------------------

class jForm {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event

  private RelativeLayout  layout   = null;
  private LayoutParams    layparam = null;
  private RelativeLayout  parent   = null;

  private OnClickListener onClickListener;   // event
  private Boolean         enabled  = true;   //


  // Constructor
  public  jForm(Controls ctrls, int pasobj) {
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    //
    layout   = new RelativeLayout(controls.activity);
    layparam = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                                ViewGroup.LayoutParams.MATCH_PARENT);
    layout.setLayoutParams(layparam);
    // Init Event
    onClickListener = new OnClickListener() {
      public  void onClick(View view) {
        if (enabled) {
          controls.pOnClick(PasObj,Const.Click_Default);
        }
      };
    };
    layout.setOnClickListener(onClickListener);
  };

  //
  public  RelativeLayout GetLayout() {
    return layout;
  }

  //
  public  void Show(int effect) {
    controls.appLayout.addView( layout );
    parent = controls.appLayout;
    //
    if (effect != Const.Eft_None) {
      layout.startAnimation(controls.Ani_Effect(effect,250));
    };
  }

  //
  public  void Close(int effect ) {
    switch ( effect ) {
     case Const.Eft_None  : { controls.appLayout.removeView(layout);
                 controls.pOnClose(PasObj);
                 break; }
     default : { Animation animation = controls.Ani_Effect(effect,250);
                 animation.setAnimationListener(new AnimationListener() {
                   @Override
                   public  void onAnimationEnd   (Animation animation) {
                     controls.appLayout.removeView(layout);
                     parent = null;
                     controls.pOnClose(PasObj);         };
                   @Override
                   public  void onAnimationRepeat(Animation animation) {}
                   @Override
                   public  void onAnimationStart(Animation animation)  {}
                 });
                 layout.startAnimation(animation);
               };
    };
  }

  //
  public  void SetVisible ( boolean visible ) {
    if (visible) { if (layout.getParent() == null)
                   { controls.appLayout.addView(layout); } }
    else         { if (layout.getParent() != null)
                   { controls.appLayout.removeView(layout); } };
  }

  public  void SetTitle ( boolean visible ) {
    //Activity host = (Activity) controls.activity;
    Log.d(Const.LogHeader,"Setting visibility of titlebar");
    if (visible)
    {
      controls.activity.getWindow().addFlags(WindowManager.LayoutParams.FLAG_FORCE_NOT_FULLSCREEN);
      controls.activity.getWindow().clearFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN);
    }
    else
    {
      controls.activity.getWindow().addFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN);
      controls.activity.getWindow().clearFlags(WindowManager.LayoutParams.FLAG_FORCE_NOT_FULLSCREEN);
      //controls.activity.requestWindowFeature(Window.FEATURE_NO_TITLE);
      //controls.activity.getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);
    }
  }

  //
  public  void SetEnabled ( boolean enabled ) {
    Log.d(Const.LogHeader,"Parent Form Enabled "+ Integer.toString(layout.getChildCount()));
    for (int i = 0; i < layout.getChildCount(); i++) {
      View child = layout.getChildAt(i);
      child.setEnabled(enabled);
    }
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { controls.appLayout.removeView(layout); }
    onClickListener = null;
    layout.setOnClickListener(null);
    layparam = null;
    layout   = null;
  }

};


// -------------------------------------------------------------------------
//  TextView
//          Event : pOnClick
// -------------------------------------------------------------------------

class jTextView extends TextView {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private LayoutParams    lparams;           // layout XYWH
  private OnClickListener onClickListener;   // event
  private Boolean         enabled  = true;   //

  // Constructor
  public  jTextView(android.content.Context context,
                    Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new LayoutParams(100,100);     // W,H
    lparams.setMargins        ( 50, 50,0,0); // L,T,
    // Init Event
    onClickListener = new OnClickListener() {
      public  void onClick(View view) {
        if (enabled) {
          controls.pOnClick(PasObj,Const.Click_Default);
        }
      };
    };
    setOnClickListener(onClickListener);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
  }

  // 2013.08.13 LORDMAN added property
  public  void setTextAlignment( int align ) {
    switch ( align ) {
     case 0 : { setGravity( Gravity.LEFT              ); }; break;
     case 1 : { setGravity( Gravity.RIGHT             ); }; break;
     case 2 : { setGravity( Gravity.TOP               ); }; break;
     case 3 : { setGravity( Gravity.BOTTOM            ); }; break;
     case 4 : { setGravity( Gravity.CENTER            ); }; break;
     case 5 : { setGravity( Gravity.CENTER_HORIZONTAL ); }; break;
     case 6 : { setGravity( Gravity.CENTER_VERTICAL   ); }; break;
    default : { setGravity( Gravity.LEFT              ); }; break;
   };
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  //
  public  void setParent2( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
    //
    Animation animation;
    animation = controls.Ani_iR2L(250); // In  (Left  to Right)
    startAnimation(animation);
    Log.d(Const.LogHeader,"animation");
  }

  //
  public  void setEnabled( boolean value ) {
    enabled = value;
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    setText("");
    lparams = null;
    setOnClickListener(null);
  }
}

// -------------------------------------------------------------------------
//  EditText
//          Event : pOnClick( , )
// -------------------------------------------------------------------------

class jEditText extends EditText {
  // Pascal Interface
  private int           PasObj   = 0;      // Pascal Obj
  private Controls      controls = null;   // Control Cass for Event
  //
  private ViewGroup     parent   = null;       // parent view
  private RelativeLayout.LayoutParams lparams; // layout XYWH
  private OnKeyListener onKeyListener;         //
  private TextWatcher   textwatcher;           // OnChange

  private boolean       changeFlag = false;    //
  // Constructor
  public  jEditText(android.content.Context context,
                    Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new RelativeLayout.LayoutParams(100,100);
    lparams.setMargins        ( 50, 50,0,0);
    // 1 Line
    this.setSingleLine();
    // 
    setHorizontalScrollBarEnabled(false);
    setHorizontallyScrolling(true);    
    
    //
    onKeyListener = new OnKeyListener() {
      public  boolean onKey(View v, int keyCode, KeyEvent event) {
        if ((keyCode           == KeyEvent.KEYCODE_ENTER) &&
            (event.getAction() == KeyEvent.ACTION_UP    )    ) {
          InputMethodManager imm = (InputMethodManager) controls.activity.getSystemService(Context.INPUT_METHOD_SERVICE);
          imm.hideSoftInputFromWindow(getWindowToken(), 0);
          //
          Log.d(Const.LogHeader,"OnEnter, Hide KeyBoard");
          // LoadMan
          controls.pOnEnter(PasObj);
          return true;
        }
        return false;
      }
    };
    setOnKeyListener(onKeyListener);
    
    // Event
    textwatcher = new TextWatcher() {
      @Override
      public  void beforeTextChanged(CharSequence s, int start, int count, int after) {
        if (changeFlag) {
          Log.d(Const.LogHeader,"BeforeTextChanged");
          controls.pOnChange(PasObj,0);
        }
      }

      @Override
      public  void onTextChanged(CharSequence s, int start, int before, int count) {
        if (changeFlag) {
          Log.d(Const.LogHeader,"OnTextChanged");
          controls.pOnChange(PasObj,1);
        }
      }

      @Override
      public  void afterTextChanged(Editable s) {
        if (changeFlag) {
          Log.d(Const.LogHeader,"AfterTextChanged");
          controls.pOnChange(PasObj,2);
        }
      }
    };
    addTextChangedListener(textwatcher);

  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
  }

  // 2013.08.13 LORDMAN  added Property
  public  void setTextAlignment( int align ) {
    switch ( align ) {
     case 0 : { setGravity( Gravity.LEFT              ); }; break;
     case 1 : { setGravity( Gravity.RIGHT             ); }; break;
     case 2 : { setGravity( Gravity.TOP               ); }; break;
     case 3 : { setGravity( Gravity.BOTTOM            ); }; break;
     case 4 : { setGravity( Gravity.CENTER            ); }; break;
     case 5 : { setGravity( Gravity.CENTER_HORIZONTAL ); }; break;
     case 6 : { setGravity( Gravity.CENTER_VERTICAL   ); }; break;
    default : { setGravity( Gravity.LEFT              ); }; break;
   };
  }

  // 2015.03.04 DonAlfredo added Multiline & patch
  public void setMultiLine( int editStyle ) {
    switch (editStyle) {
     case Const.Edit_Style_MultiLine : { setOnKeyListener(null);
                                         setSingleLine(false);
                                         setInputType(getInputType() | EditorInfo.TYPE_TEXT_FLAG_MULTI_LINE);
                                         setHorizontallyScrolling(false);
                                         setGravity( getGravity() | Gravity.TOP );
                                         break; }
     default                         : { setOnKeyListener(onKeyListener);
                                         setSingleLine(true);
                                         setInputType(getInputType() & (~EditorInfo.TYPE_TEXT_FLAG_MULTI_LINE));
                                         setHorizontallyScrolling(true);
                                         setGravity( getGravity() & (~Gravity.TOP ) );
                                         break; }                                  
    }
  }  


  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    // removeTextChangedListener(textwatcher);
    // textwatcher = null;
    setOnKeyListener(null);
    setText("");
    lparams = null;
  }

}

// -------------------------------------------------------------------------
//  Button
//          Event : pOnClick
// -------------------------------------------------------------------------

class jButton extends Button {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private LayoutParams    lparams;           // layout XYWH
  private OnClickListener onClickListener;   // event

  // Constructor
  public  jButton(android.content.Context context,
                  Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    controls   = ctrls;
    PasObj = pasobj;
    // Init Class
    lparams = new LayoutParams(100,100);     // W,H
    lparams.setMargins        ( 50, 50,0,0); // L,T,
    // Init Event
    onClickListener = new OnClickListener() {
      public  void onClick(View view) {
        controls.pOnClick(PasObj,Const.Click_Default);
      }
    };
    setOnClickListener(onClickListener);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    setOnKeyListener(null);
    setText("");
    lparams = null;
  }
}

// -------------------------------------------------------------------------
//  CheckBox
//          Event : pOnClick
// -------------------------------------------------------------------------

class jCheckBox extends CheckBox {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private LayoutParams    lparams;           // layout XYWH
  private OnClickListener onClickListener;   // event

  // Constructor
  public  jCheckBox(android.content.Context context,
                    Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new LayoutParams(100,100);     // W,H
    lparams.setMargins        ( 50, 50,0,0); // L,T,
    // Init Event
    onClickListener = new OnClickListener() {
      public  void onClick(View view) {
        controls.pOnClick(PasObj,Const.Click_Default);
      }
    };
    setOnClickListener(onClickListener);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    setText("");
    lparams = null;
  }
}

// -------------------------------------------------------------------------
//  RadioButton
//          Event : pOnClick
// -------------------------------------------------------------------------

class jRadioButton extends RadioButton {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private LayoutParams    lparams;           // layout XYWH
  private OnClickListener onClickListener;   // event

  // Constructor
  public  jRadioButton(android.content.Context context,
                       Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    controls   = ctrls;
    PasObj = pasobj;
    // Init Class
    lparams = new LayoutParams  (100,100);
    lparams.setMargins( 50, 50,0,0);
    // Init Event
    onClickListener = new OnClickListener() {
      public  void onClick(View view) {
        controls.pOnClick(PasObj,Const.Click_Default);
      }
    };
    setOnClickListener(onClickListener);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    setText("");
    lparams = null;
  }
}

// -------------------------------------------------------------------------
//  ProgressBar
//          Event -
//
//   Ref.
//      Style : http://developer.android.com/reference/android/R.attr.html
//                android.R.attr
//                ------------------------------------------------
//                progressBarStyle              0x01010077 Default
//                progressBarStyleHorizontal    0x01010078
//                progressBarStyleInverse       0x01010287
//                progressBarStyleLarge         0x0101007a
//                progressBarStyleLargeInverse  0x01010289
//                progressBarStyleSmall         0x01010079
//                progressBarStyleSmallTitle    0x0101020f
//                progressDrawable              0x0101013c
//
// -------------------------------------------------------------------------

class jProgressBar extends ProgressBar {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private LayoutParams    lparams;           // layout XYWH

  // Constructor
  public  jProgressBar(android.content.Context context,
                       Controls ctrls,int pasobj,int style ) {
    //super(context,null,progressBarStyleHorizontal);
    super(context,null,style);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new LayoutParams(100,100);     // W,H
    lparams.setMargins        ( 50, 50,0,0); // L,T,
    setMax(100);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    lparams = null;
  }
}

// -------------------------------------------------------------------------
//  ImageView
//    Event : pOnClick
// -------------------------------------------------------------------------

class jImageView extends ImageView {
  // Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Cass for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private LayoutParams    lparams;           // layout XYWH
  private OnClickListener onClickListener;   //
  public  Bitmap          bmp      = null;   //

  // Constructor
  public  jImageView(android.content.Context context,
                     Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new LayoutParams(100,100);
    lparams.setMargins        ( 50, 50,0,0);
    //
    //setAdjustViewBounds(false);
    setScaleType(ImageView.ScaleType.FIT_XY);
    // Init Event
    onClickListener = new OnClickListener() {
      public  void onClick(View view) {
        controls.pOnClick(PasObj,Const.Click_Default);
      }
    };
    setOnClickListener(onClickListener);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    if (bmp    != null) { bmp.recycle();           }
    bmp     = null;
    setImageBitmap(null);
    lparams = null;
    setImageResource(0); //android.R.color.transparent);
    onClickListener = null;
    setOnClickListener(null);
  }

}

// -------------------------------------------------------------------------
//  ListView
//    Event :
//
//
// -------------------------------------------------------------------------

// http://stackoverflow.com/questions/7361135/how-to-change-color-and-font-on-listview
class jArrayAdapter extends ArrayAdapter {
  //
  private Context       ctx;
  private int           id;
  private List <String> items ;
  //
  private int           textColor = 0xFF000000; // black
  private int           textSize  = 20;         //

  public  jArrayAdapter(Context context, int textViewResourceId , List<String> list ) {
    super(context, textViewResourceId, list);
    ctx   = context;
    id    = textViewResourceId;
    items = list ;
  }

  public  void setTextColor ( int textcolor ) {
    textColor = textcolor;
  }

  public  void setTextSize  ( int textsize  ) {
    textSize  = textsize;
  }

  @Override
  public  View getView(int position, View v, ViewGroup parent) {
    View mView = v ;

    if(mView == null){
      LayoutInflater vi = (LayoutInflater)ctx.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
      mView = vi.inflate(id, null);
    }
    //
    TextView tv = (TextView)mView;
    tv.setTextColor (textColor);
    tv.setTextSize  (TypedValue.COMPLEX_UNIT_PX,textSize );
    tv.setText      (items.get(position));   // position [0 ~ n-1]

    return mView;
    };

}

class jListView extends ListView {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private int             textColor = 0xFF000000; // black
  private int             textSize  = 20;         //
  //
  private ViewGroup       parent    = null;       // parent view
  private RelativeLayout.LayoutParams lparams;  // Control xywh
  //
  private ArrayList<String>    alist;
  private jArrayAdapter        aadapter;
  private OnItemClickListener  onItemClickListener;

  // Constructor
  public  jListView(android.content.Context context,
                    Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new RelativeLayout.LayoutParams (100,100);
    lparams.setMargins (50,50,0,0);
    setBackgroundColor (0x00000000);
    setCacheColorHint  (0);
    //
    alist    = new ArrayList<String>();
    aadapter = new jArrayAdapter(context,
                                 android.R.layout.simple_list_item_1,
                                 alist);
    aadapter.setTextColor(textColor); // Font Color
    aadapter.setTextSize (textSize ); // Font Size
    //
    setAdapter   (aadapter);
    setChoiceMode(ListView.CHOICE_MODE_SINGLE);
    // Init Event
    onItemClickListener = new OnItemClickListener() {
      @Override
      public  void onItemClick(AdapterView<?> parent, View v, int position, long id) {
        controls.pOnClick(PasObj, (int)id );
      }
    };
    setOnItemClickListener(onItemClickListener);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  //
  public  void setTextColor( int textcolor ) {
    aadapter.setTextColor ( textcolor );
  }

  //
  public  void setTextSize ( int textsize  ) {
    aadapter.setTextSize ( textsize );
  }

 // LORDMAN - 2013-08-07
  public void setItemPosition ( int position, int y ) {
    setSelectionFromTop(position, y);
  }

  //
  public  void add ( String item ) {
    alist.add( item );
    aadapter.notifyDataSetChanged();
  }

  //
  public  void clear() {
    alist.clear();
    aadapter.notifyDataSetChanged();
  }

  //
  public  void delete( int index ) {
    alist.remove( index );
    aadapter.notifyDataSetChanged();
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    alist.clear();
    alist    = null;
    setAdapter(null);
    aadapter = null;
    lparams  = null;
    setOnItemClickListener(null);
  }

}

// -------------------------------------------------------------------------
//  ScrollView
//          Event pOnClick
// -------------------------------------------------------------------------

class jScrollView extends ScrollView {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private RelativeLayout.LayoutParams lparams;           // layout XYWH
  private RelativeLayout  scrollview;        // Scroll View
  private LayoutParams    scrollxywh;        // Scroll Area

  // Constructor
  public  jScrollView(android.content.Context context,
                      Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new RelativeLayout.LayoutParams(100,100);     // W,H
    lparams.setMargins        (50,50,0,0); // L,T,
    //
    //this.setBackgroundColor (0xFF0000FF);
    // Scroll Size
    scrollview = new RelativeLayout(context);
    //scrollview.setBackgroundColor (0xFFFF0000);

    scrollxywh = new LayoutParams(100,100);
    scrollxywh.setMargins(0,0,0,0);
    scrollview.setLayoutParams(scrollxywh);
    this.addView(scrollview);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    Log.d(Const.LogHeader,"setXYWH-> jScrollView1");
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
    //
    scrollxywh.width = w;
    scrollview.setLayoutParams(scrollxywh);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  public  void setScrollSize(int size) {
    scrollxywh.height = size;
    scrollxywh.width  = lparams.width;
    scrollview.setLayoutParams(scrollxywh);
  }

  public  RelativeLayout getView() {
    return ( scrollview );
  }

  public  void setEnabled(boolean enabled) {
    //setEnabled(enabled);
    scrollview.setEnabled  (enabled);
    scrollview.setFocusable(enabled);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    scrollxywh = null;
    this.removeView(scrollview);
    scrollview = null;
    lparams = null;
  }

  @Override
  public  boolean onInterceptTouchEvent(MotionEvent event) {
     if (!isEnabled()) { return(false); }
     else return super.onInterceptTouchEvent(event);
  }

}

// -------------------------------------------------------------------------
//  LORDMAN 2013-09-03
//  Horizontal ScrollView
//          Event pOnClick
// -------------------------------------------------------------------------

class jHorizontalScrollView extends HorizontalScrollView {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private RelativeLayout.LayoutParams lparams;           // layout XYWH
  private RelativeLayout  scrollview;        // Scroll View
  private LayoutParams    scrollxywh;        // Scroll Area

  // Constructor
  public  jHorizontalScrollView(android.content.Context context,
                                Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new RelativeLayout.LayoutParams(100,100);     // W,H
    lparams.setMargins        (50,50,0,0); // L,T,
    //
    //this.setBackgroundColor (0xFF0000FF);
    // Scroll Size
    scrollview = new RelativeLayout(context);
    //scrollview.setBackgroundColor (0xFFFF0000);

    scrollxywh = new LayoutParams(100,100);
    scrollxywh.setMargins(0,0,0,0);
    scrollview.setLayoutParams(scrollxywh);
    this.addView(scrollview);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    Log.d(Const.LogHeader,"setXYWH-> jHorizontalScrollView");
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
    //
    scrollxywh.width = w;
    scrollview.setLayoutParams(scrollxywh);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  public  void setScrollSize(int size) {
    scrollxywh.height = lparams.height;
    scrollxywh.width  = size;
    scrollview.setLayoutParams(scrollxywh);
  }

  public  RelativeLayout getView() {
    return ( scrollview );
  }

  public  void setEnabled(boolean enabled) {
    //setEnabled(enabled);
    scrollview.setEnabled  (enabled);
    scrollview.setFocusable(enabled);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    scrollxywh = null;
    this.removeView(scrollview);
    scrollview = null;
    lparams = null;
  }

  @Override
  public  boolean onInterceptTouchEvent(MotionEvent event) {
     if (!isEnabled()) { return(false); }
     else return super.onInterceptTouchEvent(event);
  }
}

// -------------------------------------------------------------------------
//  ViewFlipper
//          Event :
//   Ref. http://learneasyandroid.blogspot.kr/2013/03/viewflipper-example.html
//        http://promobile.tistory.com/129
//        http://neoroid.tistory.com/86
// -------------------------------------------------------------------------

class jViewFlipper extends ViewFlipper {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private LayoutParams    lparams;           // layout XYWH
  private OnTouchListener onTouchListener;   // event
  //
  private float           Xdn;               // Down Position
  private float           Xup;               // Up Position
  private int             inxcur;            // 0
  private int             inxmax;            // 3
  //
  private Animation       iL2R;
  private Animation       oL2R;
  private Animation       iR2L;
  private Animation       oR2L;
  //
  private jImageBtn       imagebtn;
  //

  // Constructor
  public  jViewFlipper(android.content.Context context,
                       Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new LayoutParams(100,100);     // W,H
    lparams.setMargins        ( 50, 50,0,0); // L,T,
    // Animation
    iL2R = controls.Ani_iL2R(250); // In  (Left  to Right)
    oL2R = controls.Ani_oL2R(250); // Out (Left  to Right)
    iR2L = controls.Ani_iR2L(250); // In  (Right to Left )
    oR2L = controls.Ani_oR2L(250); // Out (Right to Left )
    // Sample Code
    TextView tView1 = new TextView(context);
                      tView1.setText     ("View #1");
                      tView1.setTextColor(0xFF0000FF);
                      tView1.setTextSize (128);
                      addView(tView1);
    TextView tView2 = new TextView(context);
                      tView2.setText     ("View #2");
                      tView2.setTextColor(0xFF0000FF);
                      tView2.setTextSize (128);
                      addView(tView2);
    TextView tView3 = new TextView(context);
                      tView3.setText     ("View #3");
                      tView3.setTextColor(0xFF0000FF);
                      tView3.setTextSize (128);
                      addView(tView3);
    //
    int obj = 112321321;
    imagebtn = new jImageBtn(context,ctrls,obj);
    imagebtn.setButton("/data/data/com.mormot.mORMotDemoOnAndroid/files/btn1_1.png",
                       "/data/data/com.mormot.mORMotDemoOnAndroid/files/btn1_2.png");
    //tView2.View.addView(imagebtn);
    addView(imagebtn);
    //
    inxcur = 0;
    inxmax = 4;
    //
    setBackgroundColor(0x80FFFFFF);
    // Init Event
    onTouchListener = new OnTouchListener() {
      public  boolean onTouch(final View view, final MotionEvent event) {
        Log.d(Const.LogHeader,"ViewFlipper OnTouch");
        switch(event.getAction())  {
          case MotionEvent.ACTION_DOWN :
               { Xdn = event.getX();
                 break; }
          case MotionEvent.ACTION_UP   :
               { Xup   = event.getX();
                 if(Xup < Xdn) { // Right Direction
                                 Log.d(Const.LogHeader,"ViewFlipper-Right Dir.");
                                 setInAnimation (iR2L);
                                 setOutAnimation(oR2L);
                                 showNext();
                               //  if (inxcur < (inxmax-1)){ showNext();
                               //                            inxcur++;  }
                               }
                 else if (Xup > Xdn) { // Left Direction
                                       Log.d(Const.LogHeader,"ViewFlipper-Left Dir.");
                                 setInAnimation (iL2R);
                                 setOutAnimation(oL2R);
                                 showPrevious();
                                 //      if (inxcur > 0) { showPrevious();
                                 //                        inxcur--;          }
                                     }
               }
        }
        controls.pOnClick(PasObj,Const.Click_Default);
        return true;
      }
    };
    setOnTouchListener(onTouchListener);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    iL2R    = null;
    oL2R    = null;
    iR2L    = null;
    oR2L    = null;
    lparams = null;
    setOnTouchListener(null);
  }
}

// -------------------------------------------------------------------------
//  WebView
// -------------------------------------------------------------------------

// http://developer.android.com/reference/android/webkit/WebViewClient.html
class jWebClient extends WebViewClient {
  // Java-Pascal Interface
  public  int             PasObj   = 0;      // Pascal Obj
  public  Controls        controls = null;   // Control Class for Event

  @Override
  public  boolean shouldOverrideUrlLoading(WebView view, String url) {
    int rtn = controls.pOnWebViewState(PasObj,Const.WebView_State_Before,url);
    if (rtn == Const.WebView_Act_Continue)
         { view.loadUrl(url);
           return true; }
    else { return true; }
  }

  @Override
  public  void onLoadResource(WebView view, String url) {
  }

  @Override
  public  void onPageFinished(WebView view, String url) {
    controls.pOnWebViewState(PasObj,Const.WebView_State_Finish,url);
  }

  @Override
  public  void onReceivedError(WebView view, int errorCode, String description, String failingUrl)  {
    super.onReceivedError(view, errorCode, description, failingUrl);
    controls.pOnWebViewState(PasObj,Const.WebView_State_Error, description);
  }

}

class jWebView extends WebView {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private RelativeLayout.LayoutParams lparams;
  private jWebClient      webclient;
  // Constructor
  public  jWebView(android.content.Context context,
                   Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    webclient          = new jWebClient();
    webclient.PasObj   = pasobj;
    webclient.controls = ctrls;
    //
    setWebViewClient(webclient); // Prevent to run External Browser
    //
    getSettings().setJavaScriptEnabled(true);
    //
    lparams = new RelativeLayout.LayoutParams  (300,300);
    lparams.setMargins( 50, 50,0,0);
    //
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    webclient = null;
    setWebViewClient(null);
    lparams = null;
  }
}

// -------------------------------------------------------------------------
//  Custom Canvas
//
//  Ref.
//       http://developer.android.com/reference/android/graphics/Canvas.html
//       http://developer.android.com/reference/android/graphics/Paint.html
// -------------------------------------------------------------------------

class jCanvas {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private Canvas          canvas = null;
  private Paint           paint  = null;


  // Constructor
  public  jCanvas(Controls ctrls,int pasobj ) {
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    paint   = new Paint();
  }

  public  void setCanvas(Canvas scanvas) {
    canvas = scanvas;
  }

  public  void setStrokeWidth(float width) {
    paint.setStrokeWidth(width);
  }

  public  void setStyle(int style) {
    switch (style) {
      case 0  : { paint.setStyle(Paint.Style.FILL           ); break; }
      case 1  : { paint.setStyle(Paint.Style.FILL_AND_STROKE); break; }
      case 2  : { paint.setStyle(Paint.Style.STROKE);          break; }
      default : { paint.setStyle(Paint.Style.FILL           ); break; }
    };
  }

  public  void setColor(int color) {
    paint.setColor(color);
  }

  public  void setTextSize(float textsize) {
    paint.setTextSize(textsize);
  }

  public  void drawLine(float x1, float y1, float x2, float y2) {
    canvas.drawLine(x1,y1,x2,y2,paint);
  }

  public  void drawText(String text, float x, float y ) {
    canvas.drawText(text,x,y,paint);
  }

  public  void drawBitmap(Bitmap bitmap, int b, int l, int r, int t) {
    Rect rect = new Rect(b,l,r,t);
    canvas.drawBitmap(bitmap,null,rect,paint);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    paint = null;
  }

}

// -------------------------------------------------------------------------
//  Custom View
//  ref. http://iserveandroid.blogspot.kr/2010/12/button-press-button-states-images.html
// -------------------------------------------------------------------------

class jView extends View {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private LayoutParams    lparams;
  private jCanvas         jcanvas  = null;   //

  // Constructor
  public  jView(android.content.Context context,
                Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new LayoutParams(300,300);
    lparams.setMargins        ( 50, 50,0,0);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    //
    setLayoutParams(lparams);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  //
  public  void setjCanvas( jCanvas canvas ) {
    jcanvas = canvas;
  }

  //
  @Override
  public  boolean onTouchEvent( MotionEvent event) {
    int act     = event.getAction() & MotionEvent.ACTION_MASK;
    /*
    switch(act) {
      case MotionEvent.ACTION_DOWN: {
            int count = event.getPointerCount();
            for ( int i = 0; i < count; i++ ) {
              int ptID = event.getPointerId(i);
              controls.pOnTouch (PasObj,ptID,Const.TouchDown,event.getX(i), event.getY(i) );
            }
            break; }
      case MotionEvent.ACTION_MOVE: {
            int count = event.getPointerCount();
            for ( int i = 0; i < count; i++ ) {
              int ptID = event.getPointerId(i);
              controls.pOnTouch (PasObj,ptID,Const.TouchMove,event.getX(i), event.getY(i) );
            }
            break; }
      case MotionEvent.ACTION_UP: {
            int count = event.getPointerCount();
            for ( int i = 0; i < count; i++ ) {
              int ptID = event.getPointerId(i);
              controls.pOnTouch (PasObj,ptID,Const.TouchUp  ,event.getX(i), event.getY(i) );
            }
            break; }
    } */
    switch(act) {
      case MotionEvent.ACTION_DOWN: {
            switch (event.getPointerCount()) {
              case 1 : { controls.pOnTouch (PasObj,Const.Touch_Down,1,
                                            event.getX(0),event.getY(0),0,0); break; }
              default: { controls.pOnTouch (PasObj,Const.Touch_Down,2,
                                            event.getX(0),event.getY(0),
                                            event.getX(1),event.getY(1));     break; }
            }
           break;}
      case MotionEvent.ACTION_MOVE: {
            switch (event.getPointerCount()) {
              case 1 : { controls.pOnTouch (PasObj,Const.Touch_Move,1,
                                            event.getX(0),event.getY(0),0,0); break; }
              default: { controls.pOnTouch (PasObj,Const.Touch_Move,2,
                                            event.getX(0),event.getY(0),
                                            event.getX(1),event.getY(1));     break; }
            }
           break;}
      case MotionEvent.ACTION_UP: {
            switch (event.getPointerCount()) {
              case 1 : { controls.pOnTouch (PasObj,Const.Touch_Up  ,1,
                                            event.getX(0),event.getY(0),0,0); break; }
              default: { controls.pOnTouch (PasObj,Const.Touch_Up  ,2,
                                            event.getX(0),event.getY(0),
                                            event.getX(1),event.getY(1));     break; }
            }
           break;}
      case MotionEvent.ACTION_POINTER_DOWN: {
            switch (event.getPointerCount()) {
              case 1 : { controls.pOnTouch (PasObj,Const.Touch_Down,1,
                                            event.getX(0),event.getY(0),0,0); break; }
              default: { controls.pOnTouch (PasObj,Const.Touch_Down,2,
                                            event.getX(0),event.getY(0),
                                            event.getX(1),event.getY(1));     break; }
            }
           break;}
      case MotionEvent.ACTION_POINTER_UP  : {
            switch (event.getPointerCount()) {
              case 1 : { controls.pOnTouch (PasObj,Const.Touch_Up  ,1,
                                            event.getX(0),event.getY(0),0,0); break; }
              default: { controls.pOnTouch (PasObj,Const.Touch_Up  ,2,
                                            event.getX(0),event.getY(0),
                                            event.getX(1),event.getY(1));     break; }
            }
           break;}
    } 
    return true;
  }

  //
  @Override
  public  void onDraw( Canvas canvas) {
    jcanvas.setCanvas(canvas);
    controls.pOnDraw(PasObj,canvas); // improvement required
  }

  public void saveView( String sFileName ) {
    Bitmap b = Bitmap.createBitmap( getWidth(), getHeight(), Bitmap.Config.ARGB_8888);
    Canvas c = new Canvas( b );
    draw( c );

    FileOutputStream fos = null;
    try {
      fos = new FileOutputStream( sFileName );
      if (fos != null) {
       b.compress(Bitmap.CompressFormat.PNG, 100, fos );
       fos.close(); }  }
    catch ( Exception e) {
      Log.e("SaveView", "Exception: "+ e.toString() ); }
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this);   }
    lparams = null;
  }
}

// -------------------------------------------------------------------------
//  GLView
//          Event : pOnTouch
// -------------------------------------------------------------------------
// http://developer.android.com/reference/android/opengl/GLSurfaceView.EGLContextFactory.html
// http://stackoverflow.com/questions/8932732/android-ndk-opengl-gldeletetextures-causes-error-call-to-opengl-es-api-with-no
// http://stackoverflow.com/questions/2034272/do-i-have-to-use-gldeletetextures-in-the-end-of-the-program
// http://stackoverflow.com/questions/8168014/opengl-screenshot-android
// http://cafe.naver.com/mcbugi/250562
// http://cafe.naver.com/cocos2dxusers/405
class jGLSurfaceView extends GLSurfaceView {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private LayoutParams    lparams;           // layout XYWH
  private jRenderer       renderer;
  private GL10            savGL;

  //
  class jRenderer implements GLSurfaceView.Renderer{
   public  void onSurfaceCreated(GL10 gl, EGLConfig config) {
                 controls.pOnGLViewState (PasObj,Const.GLView_State_SurfaceCreated,0,0); }
   public  void onSurfaceChanged(GL10 gl, int w, int h) {
                 controls.pOnGLViewState (PasObj,Const.GLView_State_SurfaceChanged,w,h); }
   public  void onDrawFrame     (GL10 gl) {
                 controls.pOnGLViewState (PasObj,Const.GLView_State_DrawFrame,0,0);      }
  }

  // Constructor
  public  jGLSurfaceView (android.content.Context context,
                          Controls ctrls,int pasobj, int version ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new LayoutParams(100,100);     // W,H
    lparams.setMargins        ( 50, 50,0,0); // L,T,
    // OpenGL ES Version
    if (version != 1) {
      setEGLContextClientVersion(2); };
    //
    renderer = new jRenderer();
    setEGLConfigChooser(8,8,8,8,16,8);       // RGBA,Depath,Stencil
    setRenderer  ( renderer );
    setRenderMode( GLSurfaceView.RENDERMODE_WHEN_DIRTY );
 
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  //
  @Override
  public  boolean onTouchEvent( MotionEvent event) {
    int act     = event.getAction() & MotionEvent.ACTION_MASK;
    switch(act) {
      case MotionEvent.ACTION_DOWN: {
            switch (event.getPointerCount()) {
              case 1 : { controls.pOnTouch (PasObj,Const.Touch_Down,1,
                                             event.getX(0),event.getY(0),0,0); break; }
              default: { controls.pOnTouch (PasObj,Const.Touch_Down,2,
                                             event.getX(0),event.getY(0),
                                             event.getX(1),event.getY(1));     break; }
            }
           break;}
      case MotionEvent.ACTION_MOVE: {
            switch (event.getPointerCount()) {
              case 1 : { controls.pOnTouch (PasObj,Const.Touch_Move,1,
                                             event.getX(0),event.getY(0),0,0); break; }
              default: { controls.pOnTouch (PasObj,Const.Touch_Move,2,
                                             event.getX(0),event.getY(0),
                                             event.getX(1),event.getY(1));     break; }
            }
           break;}
      case MotionEvent.ACTION_UP: {
            switch (event.getPointerCount()) {
              case 1 : { controls.pOnTouch (PasObj,Const.Touch_Up  ,1,
                                             event.getX(0),event.getY(0),0,0); break; }
              default: { controls.pOnTouch (PasObj,Const.Touch_Up  ,2,
                                             event.getX(0),event.getY(0),
                                             event.getX(1),event.getY(1));     break; }
            }
           break;}
      case MotionEvent.ACTION_POINTER_DOWN: {
            switch (event.getPointerCount()) {
              case 1 : { controls.pOnTouch (PasObj,Const.Touch_Down,1,
                                             event.getX(0),event.getY(0),0,0); break; }
              default: { controls.pOnTouch (PasObj,Const.Touch_Down,2,
                                             event.getX(0),event.getY(0),
                                             event.getX(1),event.getY(1));     break; }
            }
           break;}
      case MotionEvent.ACTION_POINTER_UP  : {
            Log.d(Const.LogHeader,"PUp");
            switch (event.getPointerCount()) {
              case 1 : { controls.pOnTouch (PasObj,Const.Touch_Up  ,1,
                                             event.getX(0),event.getY(0),0,0); break; }
              default: { controls.pOnTouch (PasObj,Const.Touch_Up  ,2,
                                             event.getX(0),event.getY(0),
                                             event.getX(1),event.getY(1));     break; }
            }
           break;}
    } 
    return true;
  }

  //
  @Override
  public void surfaceDestroyed(SurfaceHolder holder) {
    Log.d(Const.LogHeader,"surfaceDestroyed");
    queueEvent(new Runnable() {
      @Override
      public void run() {
        controls.pOnGLViewState (PasObj,Const.GLView_State_SurfaceDestroyed,0,0); }
    });    
    super.surfaceDestroyed(holder);
  }

  //
  public  void genRender() {
    queueEvent(new Runnable() {
      @Override
      public void run() {
        try{
            requestRender();
           }
        catch ( Exception e) {
            Log.e("deleteTexture", "Exception: "+ e.toString() ); }
      }
      });    
  } 

  //
  public  void deleteTexture( int id ) {
    final int idx = id;
    queueEvent(new Runnable() {
      @Override
      public void run() {
        try{
            int[] ids = new int[1];
            ids[0] = idx;
            EGL10 egl = (EGL10)EGLContext.getEGL(); 
            GL10   gl = (GL10)egl.eglGetCurrentContext().getGL();  
            //gl.glBindTexture(GL10.GL_TEXTURE_2D, idx);
            gl.glDeleteTextures(1,ids,0);
           }
        catch ( Exception e) {
            Log.e("deleteTexture", "Exception: "+ e.toString() ); }
      }
      });    
  } 
  
  //
  public  void glThread() {
    queueEvent(new Runnable() {
      @Override
      public void run() {
        controls.pOnGLViewState (PasObj,Const.GLView_State_SurfaceThread,0,0); }
    });    
  }
  
  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    renderer = null;
    lparams  = null;
  }

}

// -------------------------------------------------------------------------
//  CameraView
//      2014.11.17
//
// -------------------------------------------------------------------------
//
//   Preview Size / getSupportedPreviewSizes()
//   -------------------------------------------------------
//   Galaxy Note 3
//    1920,1080
//    1440,1080
//    1280, 720
//    1056, 864
//     960, 720
//     800, 400
//     720, 480
//     640, 480
//     352, 288   /Default
//     320, 240
//     176, 144
//
// -------------------------------------------------------------------------
//   Event Sequence
//     jCameraView.Create
//     SurfaceCreate
//     SurfaceChange
//
//
class jCameraView extends SurfaceView implements SurfaceHolder.Callback,
                                                 Camera.PreviewCallback {
  // Java-Pascal Interface
  private int               PasObj   = 0;      // Pascal Obj
  private Controls          controls = null;   // Control Class for Event
  //
  private ViewGroup         parent   = null;   // parent view
  private LayoutParams      lparams;           // layout XYWH
  //
  private Camera            camera;
  private SurfaceHolder     surfaceHolder;

  private AutoFocusCallback autoFocusCallback;
  private PictureCallback   pictureCallback;
  private PictureCallback   jpegCallback;
  private ShutterCallback   shutterCallback;

  private String            savName;
  private int               SkipFrame = 0;

  // Constructor
  public  jCameraView (android.content.Context context, Controls ctrls, int pasobj) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new LayoutParams(288,352);     // W,H
    lparams.setMargins        ( 50, 50,0,0); // L,T,
    //
    savName = String.format("/sdcard/%d.jpg", System.currentTimeMillis());
    //
    Log.d(Const.LogHeader, "jCameraView-Before surfaceHolder");
    surfaceHolder = this.getHolder();
    surfaceHolder.addCallback(this);
    surfaceHolder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
    Log.d(Const.LogHeader, "jCameraView-After surfaceHolder");

    //--------------------------------------------------------------------------
    // Call Back
    //--------------------------------------------------------------------------
    autoFocusCallback = new AutoFocusCallback() {
    @Override
    public void onAutoFocus(boolean arg0, Camera arg1) {
      //camera.takePicture(shutterCallback, rawCallback, jpegCallback);
      Toast.makeText(controls.activity, "autoFocus", 2000).show();
     }
    };

    jpegCallback = new PictureCallback() {
      public void onPictureTaken(byte[] data, Camera camera) {
        FileOutputStream outStream = null;
        try {
          outStream = new FileOutputStream(savName);
          outStream.write(data);
          outStream.close();
          Log.d("Log", "onPictureTaken - wrote bytes: " + data.length);
        } catch (FileNotFoundException e) {
          e.printStackTrace();
        } catch (IOException e) {
          e.printStackTrace();
        } finally {
        }
        Toast.makeText(controls.activity, "Picture Saved", 2000).show();
        //refreshCamera();
      }
    };

  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    lparams  = null;
  }

  //
  public void saveImage(String fileName) {
    savName = fileName;
    camera.takePicture(null, null, jpegCallback); //take the picture
  }

  //
  public void capture ( int msec ) {
    camera.autoFocus ( autoFocusCallback );
  }

  //
  public void captureStart() {
    camera.setPreviewCallback(this);
  }

  //
  public void captureStop() {
    camera.setPreviewCallback(null);
  }

  public boolean flashExists(Camera.Parameters params) {
    if (getContext().getPackageManager().hasSystemFeature(PackageManager.FEATURE_CAMERA_FLASH)) {
      if (params.getFlashMode() != null) {
       List<String> supportedFlashModes = params.getSupportedFlashModes();
       if (supportedFlashModes == null ||
           supportedFlashModes.isEmpty() ||
           supportedFlashModes.size() == 1 &&
           supportedFlashModes.get(0).equals(Camera.Parameters.FLASH_MODE_OFF))
        { return false; }
    return true; } }
    return false;
  }

  public void surfaceCreated(SurfaceHolder holder) {
    Log.d(Const.LogHeader,"Camera:SurfaceCreated");

    camera = Camera.open();
    camera.setDisplayOrientation(90);

    Camera.Parameters params = camera.getParameters();

    // Setting Params
    params.setFocusMode  (Camera.Parameters.FOCUS_MODE_CONTINUOUS_PICTURE);
    if (flashExists(params)) {  params.setFlashMode  (Camera.Parameters.FLASH_MODE_TORCH); }
    params.setPreviewSize(352, 288);
    camera.setParameters (params  );

    camera.setPreviewCallback(this);

    try   { camera.setPreviewDisplay (surfaceHolder); }
    catch ( Exception e) {  System.err.println(e); return; }
    //
    camera.startPreview();
    //
  }

  // Now that the size is known, set up the camera parameters and begin the preview.
  public void surfaceChanged(SurfaceHolder holder, int format, int w, int h) {
    Log.d(Const.LogHeader,"Camera:SurfaceChanged");
  }

  // stop preview and release camera
  public void surfaceDestroyed(SurfaceHolder holder) {
    camera.setPreviewCallback(null);
    camera.stopPreview();
    camera.release();
    camera = null;
  }

  @Override
  public void onPreviewFrame(byte[] data, Camera camera) {
    SkipFrame++;
    if (SkipFrame >  5) { SkipFrame = 0; }

    if (SkipFrame == 0)
     {
       Log.d(Const.LogHeader,"onPreviewFrame");
       Camera.Parameters parameters = camera.getParameters();
       Camera.Size size = parameters.getPreviewSize();

       // Java -> Pascal Event
       controls.pOnCameraFrame   (PasObj, size.width, size.height , data.length, data );
     }
    }
}

// -------------------------------------------------------------------------
//  Dialog
//          Event pOnClick
// -------------------------------------------------------------------------

class jDialogYN {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private String          dlgTitle;
  private String          dlgMsg;
  private String          dlgY;
  private String          dlgN;
  //
  private DialogInterface.OnClickListener onClickListener = null;
  private AlertDialog dialog = null;

  // Constructor
  public  jDialogYN(android.content.Context context,
                    Controls ctrls, int pasobj,
                    String title, String msg, String y, String n ) {
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    //
    dlgTitle = title;
    dlgMsg   = msg;
    dlgY     = y;
    dlgN     = n;

    // Init Event
    onClickListener = new DialogInterface.OnClickListener() {
      public  void onClick(DialogInterface dialog, int id) {
        if  ( id == Const.Click_Yes) { controls.pOnClick(PasObj,Const.Click_Yes);}
                                else { controls.pOnClick(PasObj,Const.Click_No );}
      };
    };
    // Init Class
    AlertDialog.Builder builder = new AlertDialog.Builder(controls.activity);
    builder.setMessage       (dlgMsg  )
           .setCancelable    (false)
           .setPositiveButton(dlgY,onClickListener)
           .setNegativeButton(dlgN,onClickListener);
    dialog = builder.create();
    //
    dialog.setTitle(dlgTitle);
    dialog.setIcon(R.drawable.icon);

  }

 public  void show() {
    Log.d(Const.LogHeader,"DlgYN_Show");
    dialog.show();
  }

 public  void Free() {
    onClickListener = null;
    dialog.setTitle("");
    dialog.setIcon(null);
    dialog = null;
  }
}

// -------------------------------------------------------------------------
//  ProgressDialog
// -------------------------------------------------------------------------

class jDialogProgress {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ProgressDialog  dialog;

  // Constructor
  public  jDialogProgress(android.content.Context context,
                          Controls ctrls, int pasobj, String title,String msg ) {
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init & Run
    dialog = ProgressDialog.show(context,title,msg,true);
  }

  public  void Free() {
    dialog.dismiss();
    dialog = null;
  }
}


// -------------------------------------------------------------------------
//  jImageBtn
// -------------------------------------------------------------------------

class jImageBtn extends View {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private ViewGroup       parent   = null;   // parent view
  private LayoutParams    lparams;
  //
  private Paint           mPaint   = null;
  private Bitmap          bmpUp    = null;
  private Bitmap          bmpDn    = null;
  private Rect            rect;
  private int             btnState = 0;      // Normal = 0 , Pressed = 1
  private Boolean         enabled  = true;   //

  // Constructor
  public  jImageBtn(android.content.Context context,
                    Controls ctrls,int pasobj ) {
    super(context);
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init Class
    lparams = new LayoutParams  (100,100);
    lparams.setMargins( 50, 50,0,0);
    // BackGroundColor
    //setBackgroundColor(0xFF0000FF);
    //
    mPaint = new Paint();
    rect   = new Rect(0,0,100,100);
  }

  //
  public  void setXYWH ( int x, int y, int w, int h ) {
    lparams.width  = w;
    lparams.height = h;
    lparams.setMargins(x,y,0,0);
    rect.right     = w;
    rect.bottom    = h;
    //
    setLayoutParams(lparams);
  }

  //
  public  void setParent( android.view.ViewGroup viewgroup ) {
    if (parent != null) { parent.removeView(this); }
    parent = viewgroup;
    viewgroup.addView(this,lparams);
  }

  public  void setButton( String fileup , String filedn ) {
    if (bmpUp  != null) { bmpUp.recycle();         }
    bmpUp = BitmapFactory.decodeFile(fileup);
    if (bmpDn  != null) { bmpDn.recycle();         }
    bmpDn = BitmapFactory.decodeFile(filedn);
    invalidate();
  }

  //
  @Override
  public  boolean onTouchEvent( MotionEvent event) {
    // LORDMAN 2013-08-16
    if (enabled == false) { return(false); }

    int actType = event.getAction() &MotionEvent.ACTION_MASK;
    switch(actType) {
      case MotionEvent.ACTION_DOWN: { btnState = 1; invalidate(); Log.d(Const.LogHeader,"Here"); break; }
      case MotionEvent.ACTION_MOVE: {                             break; }
      case MotionEvent.ACTION_UP  : { btnState = 0; invalidate();
                                      controls.pOnClick(PasObj,Const.Click_Default);
                                      break; }
    }
    return true;
  }

  //
  @Override
  public  void onDraw( Canvas canvas) {
    //
    if (btnState == 0) { if (bmpUp != null) { canvas.drawBitmap(bmpUp,null,rect,mPaint); }; }
    else               { if (bmpDn != null) { canvas.drawBitmap(bmpDn,null,rect,mPaint); }; };
  }

  public  void setEnabled(boolean value) {
    enabled = value;
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
    if (parent != null) { parent.removeView(this); }
    if (bmpUp  != null) { bmpUp.recycle();         }
    if (bmpDn  != null) { bmpDn.recycle();         }
    bmpUp   = null;
    bmpDn   = null;
    lparams = null;
    mPaint  = null;
    rect    = null;
  }
}


//------------------------------------------------------------------------------
// jAsyncTask
//
//------------------------------------------------------------------------------

//                                 Params , Progress , Result
class jAsyncTask extends AsyncTask<Void   , Integer  , Void>{
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event

  // Constructor
  public  jAsyncTask(Controls ctrls,int pasobj) {
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
  }

  // Step #1. Before Process
  @Override
  protected void onPreExecute() {
    super.onPreExecute();
    controls.pOnAsyncTaskState(PasObj,Const.AsyncTask_State_PreExecute,0    ); // Pascal Event
  }

  // Step #3. Progress
  @Override
  protected void onProgressUpdate(Integer... params) {
    super.onProgressUpdate(params);
    controls.pOnAsyncTaskState(PasObj,Const.AsyncTask_State_Update,params[0]); // Pascal Event
  }

  // Step #4. After Process
  @Override
  protected void onPostExecute(Void result) {
    super.onPostExecute(result);
    controls.pOnAsyncTaskState(PasObj,Const.AsyncTask_State_PostExecute,100 ); // Pascal Event
  }

  // Step #2. Task
  @Override
  protected Void doInBackground(Void... params) {
    controls.pOnAsyncTaskState(PasObj,Const.AsyncTask_State_BackGround,100  ); // Pascal Event
    return null;
  };

  public void setProgress(Integer progress ) {
    Log.d(Const.LogHeader,"setProgress " );
    publishProgress(progress);
  }

  // Free object except Self, Pascal Code Free the class.
  public  void Free() {
  }

}

// -------------------------------------------------------------------------
//  jHttp
//   Event : onHttp_
//
//   getText      ( url )
//
//   DownloadFile ( url, localfile )
//   UploadFile   ( url, localfile )
//
// -------------------------------------------------------------------------

class jHttp {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  private String          urlText;
  private String          localFileText;

  // Constructor
  public  jHttp(android.content.Context context, Controls ctrls, int pasobj ) {
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
  }

  public  void Free() {
  }

  //
  private static String readStream(InputStream is) {
   BufferedReader reader = new BufferedReader(new InputStreamReader(is));
   StringBuilder sb      = new StringBuilder();

   String line = null;
   try     { while ((line = reader.readLine()) != null) {
             sb.append(line + "\n"); }}
   catch   ( IOException e) { Log.d(Const.LogHeader, "IOException"); }
   finally { try   { is.close(); }
             catch (IOException e) { Log.d(Const.LogHeader,"IOException"); } }
   return sb.toString();
  }

  //---------------------------------------------------------------------------
  // Get Text
  //---------------------------------------------------------------------------

  private class DownloadText extends AsyncTask<String, Integer, String> {

    private Context context;
    private int state;
    private String rst = "";

    public DownloadText(Context context) {
        this.context = context;
    }

    @Override
    protected void onPostExecute(String result) {
      if (rst == "") { controls.pOnHttpState(PasObj,Const.Http_Act_Text,Const.Http_State_Error,100,rst); }
      else           { controls.pOnHttpState(PasObj,Const.Http_Act_Text,Const.Http_State_Done ,100,rst); }
    }

    @Override
    protected String doInBackground(String... sUrl) {
        HttpURLConnection conn = null;
        try { URL url = new URL(sUrl[0]);
              conn = (HttpURLConnection) url.openConnection();
              conn.setConnectTimeout(3000 );
              conn.setUseCaches     (false);
              conn.connect();
              // check response
              if (conn.getResponseCode() == HttpURLConnection.HTTP_OK) {
                rst = readStream(conn.getInputStream());
                Log.d(Const.LogHeader,rst);
              } }
        catch (Exception e) { Log.d(Const.LogHeader,"URLConnection Err"); }
        finally {
            if (conn != null) conn.disconnect();  }
        return null;
    }
  }

  // Sample : http://www.google.com
  public  void getText ( String url ) {
    urlText = url;
    //
    final DownloadText downloadText = new DownloadText(controls.activity);
    downloadText.execute(url);
  }

  //---------------------------------------------------------------------------
  // Download File
  // http://stackoverflow.com/questions/3028306/download-a-file-with-android-and-showing-the-progress-in-a-progressdialog
  //---------------------------------------------------------------------------

  private class DownloadTask extends AsyncTask<String, Integer, String> {

    private Context context;
    private PowerManager.WakeLock mWakeLock;
    private String rst = "";

    public DownloadTask(Context context) {
        this.context = context;
    }

    @Override
    protected void onPreExecute() {
      super.onPreExecute();
      // PowerManager pm = (PowerManager) context.getSystemService(Context.POWER_SERVICE);
      // mWakeLock = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK,getClass().getName());
      // mWakeLock.acquire();
    }

    @Override
    protected void onProgressUpdate(Integer... progress) {
      super.onProgressUpdate(progress);
      //
      controls.pOnHttpState(PasObj,Const.Http_Act_Download,Const.Http_State_Xfer,progress[0],localFileText);
    }

    @Override
    protected void onPostExecute(String result) {
      // mWakeLock.release();
      if (rst == "") { controls.pOnHttpState(PasObj,Const.Http_Act_Download,Const.Http_State_Error,100,rst); }
      else           { controls.pOnHttpState(PasObj,Const.Http_Act_Download,Const.Http_State_Done ,100,rst); }
    }

    @Override
    protected String doInBackground(String... sUrl) {
        InputStream input = null;
        OutputStream output = null;
        HttpURLConnection connection = null;
        rst = "";
        try { URL url = new URL(sUrl[0]);
              connection = (HttpURLConnection) url.openConnection();
              connection.connect();

              // check response
              if (connection.getResponseCode() != HttpURLConnection.HTTP_OK) {
                Log.d(Const.LogHeader,"Server Error");
                return("Server Error");
              }

              int fileLength = connection.getContentLength();

              // download the file
              input  = connection.getInputStream();
              output = new FileOutputStream(localFileText);

              byte data[] = new byte[4096];
              long total    = 0;
              long totalSav = 0;
              int count;
              while ((count = input.read(data)) != -1) {
                // allow canceling with back button
                if (isCancelled()) {
                    input.close();
                    return null;  }
                total += count;
                // Update the progress [UI Thread]
                if (fileLength > 0) {
                  if ((total-totalSav) >= (100/fileLength)) {
                    publishProgress((int) (total * 100 / fileLength));
                    totalSav = total;
                  }
                }
                output.write(data, 0, count); } }
        catch (Exception e) { return e.toString(); }
        finally {
            try   { if ((output != null) && (input != null )) { rst = localFileText; }
                    if (output != null) output.close();
                    if (input  != null) input.close ();  }
            catch (IOException ignored) {  }
            if (connection != null) connection.disconnect();  }
        return null;
    }
  }

  // Sample File http://app.pixo.kr/maxpaper/test.jpg
  public  void downloadFile ( String url, String localFile ) {
    urlText       = url;
    localFileText = localFile;
    //
    final DownloadTask downloadTask = new DownloadTask(controls.activity);
    downloadTask.execute(url);
  }

  //---------------------------------------------------------------------------
  // Upload File
  // http://www.cuelogic.com/blog/android-code-to-upload-download-large-files-to-server-2
  //
  // Test Server Code
  // --------------------------------------------------------------------------
  // <html>
  // <head>
  //	<title>Upload Test</title>
  // </head>
  // <body>
  // <%
  //    Dim Upload, UpForm
  //    Set Upload = Server.CreateObject("TABSUpload4.Upload")
  //    Upload.Start "E:\web\LocalUser\app\www\maxpaper"
  //    Set UpForm = Upload.Form("uploadFile")
  //	UpForm.Save "E:\web\LocalUser\app\www\maxpaper\file", False
  //	Response.Write "FileName:" & UpForm.FileName & "<br>"
  //    Set Upload = Nothing
  // %>
  // </body>
  // </html>
  //

  private class UploadTask extends AsyncTask<String, Integer, String> {

    private Context context;
    private PowerManager.WakeLock mWakeLock;
    private String rst = "";

    public UploadTask(Context context) {
        this.context = context;
    }

    @Override
    protected void onPreExecute() {
      super.onPreExecute();
      // PowerManager pm = (PowerManager) context.getSystemService(Context.POWER_SERVICE);
      // mWakeLock = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK,getClass().getName());
      // mWakeLock.acquire();
    }

    @Override
    protected void onProgressUpdate(Integer... progress) {
      super.onProgressUpdate(progress);
      //
      controls.pOnHttpState(PasObj,Const.Http_Act_Upload,Const.Http_State_Xfer,progress[0],localFileText);
    }

    @Override
    protected void onPostExecute(String result) {
      // mWakeLock.release();
      if (rst == "") { controls.pOnHttpState(PasObj,Const.Http_Act_Upload,Const.Http_State_Error,100,rst); }
      else           { controls.pOnHttpState(PasObj,Const.Http_Act_Upload,Const.Http_State_Done ,100,rst); }
    }

    @Override
    protected String doInBackground(String... sUrl) {
        FileInputStream   input  = null;
        DataOutputStream  output = null;
        HttpURLConnection conn   = null;
        rst = "";
        try { URL url = new URL(sUrl[0]);
              conn = (HttpURLConnection) url.openConnection();
              conn.setDoInput        (true  );
              conn.setDoOutput       (true  );
              conn.setUseCaches      (false );

              conn.setRequestMethod  ("POST");
              conn.setRequestProperty("Connection","Keep-Alive");
              conn.setRequestProperty("Content-Type","multipart/form-data; boundary="+Const.Http_Boundary);
              //
              output = new DataOutputStream(conn.getOutputStream());
              //
              output.writeBytes("--" + Const.Http_Boundary + "\r\n");
              output.writeBytes("Content-Disposition: form-data; name=\"uploadFile\"; "+
                                "filename=\"" + localFileText + "\"\r\n");
              output.writeBytes("Content-Type: text/plain" + "\r\n\r\n");
              //
              input  = new FileInputStream (new File(localFileText));

              int totalSize      = input.available();
              int total          = 0;
              int totalSav       = 0;
              int curUpload      = 0;
              int maxBufferSize  = 1 * 4096;
              int bytesRead;
              int bytesAvailable = input.available();
              int bufferSize     = Math.min(bytesAvailable,maxBufferSize);
              byte buffer[]      = new byte[4096];

              // Read File
              bytesRead = input.read(buffer,0,bufferSize);
              try   { while(bytesRead >0) {
                       try   { output.write(buffer,0,bufferSize);           }
                       catch (OutOfMemoryError e) { return ("outOfMemory"); }
                      bytesAvailable = input.available();

                      // current upload
                      curUpload = (totalSize-bytesAvailable);
                      if ( (curUpload-totalSav) >= (100/totalSize)) {
                       publishProgress((int) ( (curUpload * 100 / totalSize) ));
                       totalSav = total;
                      }

                      bufferSize     = Math.min(bytesAvailable, maxBufferSize);
                      bytesRead      = input.read(buffer,0,bufferSize); } }
              catch (Exception e) { return ("error"); }
              //
              output.writeBytes("\r\n");
              output.writeBytes("--" + Const.Http_Boundary + "--"+"\r\n");

              // Responses from the server (code and message)
	      int serverResponseCode = conn.getResponseCode();
	      String serverResponseMessage = conn.getResponseMessage();
	      Log.d(Const.LogHeader,"Server Response Code " + " " + serverResponseCode);
	      Log.d(Const.LogHeader,"Server Response Message "+ serverResponseMessage);

              input.close();
              output.flush();

              InputStream is = conn.getInputStream();
              int ch;
              StringBuffer b = new StringBuffer();
              while (( ch = is.read() ) != -1) {b.append( (char)ch ); }

              String response = b.toString();
              Log.d(Const.LogHeader,"Response"+response); }
        catch (Exception e) { return e.toString(); }
        finally {
            try   { if ((output != null) && (input != null )) { rst = localFileText; }
                    if (output != null) output.close();
                    if (input  != null) input.close ();  }
            catch (IOException ignored) {  }
            if (conn != null) conn.disconnect();  }
        return null;
    }
  }

  // Sample File /data/data/com.kredix/files/alex.jpg
  public  void uploadFile ( String url, String localFile ) {
    urlText       = url;
    localFileText = localFile;
    //
    final UploadTask uploadTask = new UploadTask(controls.activity);
    uploadTask.execute(url);
  }

}


//------------------------------------------------------------------------------
//
//  Graphic API
//
//
//------------------------------------------------------------------------------
// http://forum.lazarus.freepascal.org/index.php?topic=21568.0
// https://github.com/alrieckert/lazarus/blob/master/lcl/interfaces/customdrawn/android/bitmap.pas
class jBitmap  {
  // Java-Pascal Interface
  private int             PasObj   = 0;      // Pascal Obj
  private Controls        controls = null;   // Control Class for Event
  //
  public  Bitmap bmp    = null;

  // Constructor
  public  jBitmap(Controls ctrls, int pasobj ) {
    // Connect Pascal I/F
    PasObj   = pasobj;
    controls = ctrls;
    // Init
  }

  public  void loadFile(String filename) {
    if (bmp != null) { bmp.recycle(); }
    bmp = BitmapFactory.decodeFile(filename);
  }

  public  void createBitmap(int w, int h) {
    if (bmp != null) { bmp.recycle(); }
    bmp = Bitmap.createBitmap( w,h, Bitmap.Config.ARGB_8888 );

  }

  public  int[] getWH() {
    int[] wh = new int[2];
    wh[0] = 0; // width
    wh[1] = 0; // height
    if ( bmp != null ) {
      wh[0] = bmp.getWidth();
      wh[1] = bmp.getHeight();
    }
    return ( wh );
  }

  public  void Free() {
    bmp.recycle();
    bmp = null;
  }

}

//------------------------------------------------------------------------------
//
//  Javas/Pascal Interface Class
//
//
//
//
//
//
//------------------------------------------------------------------------------

//
public  class Controls {
 //
 public   Activity       activity;                                     // Activity
 public   RelativeLayout appLayout;                                    // Base Layout
 public   int            screenStyle       = Const.ScreenStyle_Normal; // Noral,Full
 public   int            screenOrientation = ActivityInfo.SCREEN_ORIENTATION_PORTRAIT;
 public   int            SDKVer            = android.os.Build.VERSION.SDK_INT;

 // Jave -> Pascal Function ( Pascal Side = Event )
 public  native int  pOnAppScreenStyle      ();
 public  native int  pOnAppScreenOrientation();
 //
 public  native void pOnAppCreate      (RelativeLayout layout );
 public  native void pOnAppNewIntent   ();
 public  native void pOnAppDestroy     ();
 public  native void pOnAppPause       ();
 public  native void pOnAppRestart     ();
 public  native void pOnAppResume      ();
 public  native void pOnAppActive      ();
 public  native void pOnAppStop        ();
 public  native void pOnAppBackPressed ();
 public  native int  pOnAppRotate      (int rotate);
 public  native void pOnAppConfigurationChanged();
 public  native void pOnAppActivityResult(int requestCode, int resultCode, Intent data);
 //
 public  native void pOnDraw           (int pasobj, Canvas canvas);
 public  native void pOnClick          (int pasobj, int value);
 public  native void pOnChange         (int pasobj, int EventType);
 public  native void pOnEnter          (int pasobj);
 public  native void pOnTimer          (int pasobj);
 public  native void pOnTouch          (int pasobj, int act, int cnt,float x1, float y1,float x2, float y2);
 //
 public  native void pOnClose          (int pasobj);

 // State Event
 public  native void pOnGLViewState    (int pasobj, int state, int w, int h);
 public  native int  pOnWebViewState   (int pasobj, int state, String url);
 public  native void pOnAsyncTaskState (int pasobj, int state, int progress);
 public  native void pOnHttpState      (int pasobj, int style, int state, int progress, String msg);
 //
 public  native void pOnCameraFrame    (int pasobj, int w, int h, int format, byte[] data);

 // Load Pascal Library
 static {
     Log.d(Const.LogHeader, "Load libmain.so");
     System.loadLibrary("main");
 }

 // -------------------------------------------------------------------------
 //  Activity Event
 // -------------------------------------------------------------------------
 public  int  jOnAppScreenStyle()                  { return(pOnAppScreenStyle());        }
 public  int  jOnAppScreenOrientation()            { return(pOnAppScreenOrientation());  }
 //
 public  void jOnAppCreate(RelativeLayout layout ) { Log.d(Const.LogHeader,"####### jOnAppCreate"); 
 	                                                   pOnAppCreate(layout);               }
 public  void jOnAppNewIntent()                    { pOnAppNewIntent();                  }
 public  void jOnAppDestroy()                      { pOnAppDestroy();                    }
 public  void jOnAppPause()                        { pOnAppPause();                      }
 public  void jOnAppRestart()                      { pOnAppRestart();                    }
 public  void jOnAppResume()                       { pOnAppResume();                     }
 public  void jOnAppActive()                       { pOnAppActive();                     }
 public  void jOnAppStop()                         { pOnAppStop();                       }
 public  void jOnAppBackPressed()                  { pOnAppBackPressed();                }
 public  int  jOnAppRotate(int rotate)             { return(pOnAppRotate(rotate));       }
 public  void jOnAppConfigurationChanged()         { pOnAppConfigurationChanged();       }
 public  void jOnAppActivityResult(int requestCode,
                                   int resultCode,
                                   Intent data)
                                                   { pOnAppActivityResult(requestCode,
                                                                          resultCode,
                                                                          data);         }

 // -------------------------------------------------------------------------
 //  System, Class
 // -------------------------------------------------------------------------

 //
 public  void systemGC() {
   System.gc();
 }

 //
 public  int  systemGetScreenStyle() {
   return ( screenStyle );
 }

 //
 public  void systemSetScreenStyle (int screenstyle) {
   screenStyle = screenstyle;
   switch ( screenStyle ) {
    case Const.ScreenStyle_Normal : { activity.getWindow().addFlags(WindowManager.LayoutParams.FLAG_FORCE_NOT_FULLSCREEN);
                                      activity.getWindow().clearFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN);
                                      break; }
    case Const.ScreenStyle_Full   : { activity.getWindow().addFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN);
                                      activity.getWindow().clearFlags(WindowManager.LayoutParams.FLAG_FORCE_NOT_FULLSCREEN);
                                      break; }
   }
 }

 //by jmpessoa
 public  int  systemGetScreenOrientation() {
   return (activity.getResources().getConfiguration().orientation);
 }

 //
 public  void systemSetScreenOrientation(int orientation) {
   activity.setRequestedOrientation(orientation);
 }

 //
 public  void classSetNull (Class object) {
   object = null;
 }

 public  boolean classChkNull (Class object) {
   return ( object == null );
 }

 public Context GetContext() {
   return this.activity;
 }

 // -------------------------------------------------------------------------
 //  App Related
 // -------------------------------------------------------------------------

 //
 public  void appFinish () {
   activity.finish();
 }

 //
 public  void appKillProcess() {
   this.activity.finish();
 }

 public  void appSetTitleBar(boolean visible) {
    final ActionBar actionBar = this.activity.getActionBar();
    if (visible)
    {
      actionBar.show();
    }
    else
    {
      actionBar.hide();
    }
 }


 // -------------------------------------------------------------------------
 //  Asset Related
 // -------------------------------------------------------------------------

 // src : codedata.txt
 // tgt : /data/data/com.kredix.control/data/codedata.txt
 public  boolean assetSaveToFile(String src, String tgt) {
   InputStream is = null;
   FileOutputStream fos = null;
   String path = '/' + tgt.substring(1,tgt.lastIndexOf("/"));
   File outDir = new File(path);
   outDir.mkdirs();
   try {
     is = activity.getAssets().open(src);
     int size = is.available();
     byte[] buffer = new byte[size];
     File outfile = new File(tgt);
     fos = new FileOutputStream(outfile);
     for (int c = is.read(buffer); c != -1; c = is.read(buffer)){
       fos.write(buffer, 0, c);
     }
     is.close();
     fos.close();
     return(true); }
   catch (IOException e) {
     e.printStackTrace();
     return(false);       }
 }

 // -------------------------------------------------------------------------
 //  Animation Related
 //
 //   android.view.animation.AlphaAnimation
 //   android.view.animation.RotateAnimation
 //   android.view.animation.ScaleAnimation
 //   android.view.animation.TranslateAnimation
 //
 //   android.view.animation.AnimationSet
 //
 //   Ref.
 //    http://sripatim.wordpress.com/2011/03/18/viewflipper-on-button-click-in-android/
 //    http://stackoverflow.com/questions/7432375/how-to-animate-an-adding-of-a-view-in-android
 //    http://stackoverflow.com/questions/10909865/setanimation-vs-startanimation-in-android
 //    http://stackoverflow.com/questions/10695022/how-remove-view-in-android
 //
 //    AnimationUtils.loadAnimation(controls.activity, R.anim.slide_right_in);
 // -------------------------------------------------------------------------

 public  Animation Ani_iR2L(int duration) {
   Animation iR2L = new TranslateAnimation(
     Animation.RELATIVE_TO_PARENT,  1.0f, Animation.RELATIVE_TO_PARENT,  0.0f,
     Animation.RELATIVE_TO_PARENT,  0.0f, Animation.RELATIVE_TO_PARENT,  0.0f);
     iR2L.setDuration(duration);
     iR2L.setInterpolator(new AccelerateInterpolator());
     iR2L.setStartOffset(0);
     return iR2L; }

 public  Animation Ani_oR2L(int duration) {
   Animation oR2L = new TranslateAnimation(
     Animation.RELATIVE_TO_PARENT,  0.0f, Animation.RELATIVE_TO_PARENT, -1.0f,
     Animation.RELATIVE_TO_PARENT,  0.0f, Animation.RELATIVE_TO_PARENT,  0.0f);
     oR2L.setDuration(duration);
     oR2L.setInterpolator(new AccelerateInterpolator());
     oR2L.setStartOffset(0);
     return oR2L;  }

 public  Animation Ani_iL2R(int duration) {
   Animation iL2R = new TranslateAnimation(
     Animation.RELATIVE_TO_PARENT, -1.0f, Animation.RELATIVE_TO_PARENT,  0.0f,
     Animation.RELATIVE_TO_PARENT,  0.0f, Animation.RELATIVE_TO_PARENT,  0.0f);
     iL2R.setDuration(duration);
     iL2R.setInterpolator(new AccelerateInterpolator());
     iL2R.setStartOffset(0);
     return iL2R;   }

 public  Animation Ani_oL2R(int duration) {
   Animation oL2R = new TranslateAnimation(
     Animation.RELATIVE_TO_PARENT,  0.0f, Animation.RELATIVE_TO_PARENT,  1.0f,
     Animation.RELATIVE_TO_PARENT,  0.0f, Animation.RELATIVE_TO_PARENT,  0.0f);
     oL2R.setDuration(duration);
     oL2R.setInterpolator(new AccelerateInterpolator());
     oL2R.setStartOffset(0);
     return oL2R;   }

 public  Animation Ani_FadeIn(int duration) {
   Animation Alpha = new AlphaAnimation(0f,1f);
     Alpha.setDuration(duration);
     Alpha.setStartOffset(0);
     Alpha.setFillAfter (false);
     Alpha.setFillBefore(false);
     return Alpha;   }

 public  Animation Ani_FadeOut(int duration) {
   Animation Alpha = new AlphaAnimation(1f,0f);
     Alpha.setDuration(duration);
     Alpha.setStartOffset(0);
     Alpha.setFillAfter (false);
     Alpha.setFillBefore(false);
     return Alpha;   }

 // https://coderwall.com/p/jpijag
 public  AnimationSet Ani_iR2LFadeIn(int duration) {
   AnimationSet aniset = new AnimationSet(true);
   aniset.addAnimation(Ani_iR2L  (duration));
   aniset.addAnimation(Ani_FadeIn(duration));
   aniset.setFillAfter(true);
   return aniset;
 }

 public  AnimationSet Ani_Effect(int effect, int duration) {
   //
   AnimationSet aset = new AnimationSet(true);
   if ((effect & Const.Eft_iR2L   ) == Const.Eft_iR2L   ) { aset.addAnimation(Ani_iR2L   (duration)); };
   if ((effect & Const.Eft_iL2R   ) == Const.Eft_iL2R   ) { aset.addAnimation(Ani_iL2R   (duration)); };
   if ((effect & Const.Eft_oR2L   ) == Const.Eft_oR2L   ) { aset.addAnimation(Ani_oR2L   (duration)); };
   if ((effect & Const.Eft_oL2R   ) == Const.Eft_oL2R   ) { aset.addAnimation(Ani_oL2R   (duration)); };
   if ((effect & Const.Eft_FadeIn ) == Const.Eft_FadeIn ) { aset.addAnimation(Ani_FadeIn (duration)); };
   if ((effect & Const.Eft_FadeOut) == Const.Eft_FadeOut) { aset.addAnimation(Ani_FadeOut(duration)); };
   aset.setFillAfter(true);
   return aset;
 }

 // -------------------------------------------------------------------------
 //  View Related
 // -------------------------------------------------------------------------

 //
 public  void view_SetVisible        (View view, int state) {
   view.setVisibility(state);
 }

 public  void view_SetBackGroundColor(View view, int color) {
   view.setBackgroundColor(color);
 }

 public  void view_Invalidate        (View view) {
   view.invalidate();
 }


 // -------------------------------------------------------------------------
 //  Form Related
 // -------------------------------------------------------------------------

 //
 public  java.lang.Object jForm_Create(int pasobj ) {
   return (java.lang.Object)( new jForm(this,pasobj));
 }

 public  void jForm_Free(java.lang.Object form) {
  jForm obj = (jForm)form;
  obj.Free();
  //obj = null;
 }

 public  RelativeLayout jForm_GetLayout(java.lang.Object form) {
   return ((jForm)form).GetLayout();
 }

 public  void jForm_Show (java.lang.Object form, int effect) {
   ((jForm)form).Show(effect);
 }

 public  void jForm_Close (java.lang.Object form, int effect) {
   ((jForm)form).Close(effect);
 }

 public  void jForm_SetVisible (java.lang.Object form, boolean visible) {
   ((jForm)form).SetVisible(visible);
 }

 public  void jForm_SetTitle (java.lang.Object form, boolean visible) {
   ((jForm)form).SetTitle(visible);
 }

 public  void jForm_SetEnabled (java.lang.Object form, boolean enabled) {
   ((jForm)form).SetEnabled(enabled);
 }

 // -------------------------------------------------------------------------
 //  System Info
 // -------------------------------------------------------------------------
 // Result : Width(16bit) : Height (16bit)
 public  int  getScreenWH(android.content.Context context) {
   DisplayMetrics metrics = new DisplayMetrics();

   int h = this.activity.getResources().getDisplayMetrics().heightPixels;
   int w = this.activity.getResources().getDisplayMetrics().widthPixels;
   return ( (w << 16)| h );
 }

 // LORDMAN - 2013-07-28
 public  int getStrLength(String Txt) {
   int len = Txt.length();
   return ( len );
 }

 // LORDMAN - 2013-07-30
 public  String getStrDateTime() {
   SimpleDateFormat formatter = new SimpleDateFormat ( "yyyy-MM-dd HH:mm:ss", Locale.getDefault() );
   return( formatter.format ( new Date () ) );
 }

 public long getTick() {
   return ( System.currentTimeMillis() );
 }

 // -------------------------------------------------------------------------
 //  Android path
 // -------------------------------------------------------------------------

 // jmpessoa 
 // 2015.03.03 merged by funcions simonsayz
 // 
 // ref. http://developer.android.com/reference/android/os/Environment.html#DIRECTORY_ALARMS
 public String getPath(int _directory, String pkgName) {
  
  File   filePath = null;
  String path     = "";
    
  switch(_directory) {                         
   case Const.DIRECTORY_ALARMS        : { filePath = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_ALARMS);
                                          path     = filePath.getPath(); 
                                          break;}
   case Const.DIRECTORY_DCIM          : { filePath = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DCIM  ); 
                                          path     = filePath.getPath(); 
                                          break;}
   case Const.DIRECTORY_DOCUMENTS     : { if (SDKVer >= Const.Version_KitKat) 
                                            { filePath = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOCUMENTS );
                                              path     = filePath.getPath(); }
                                          else 
                                            { path = activity.getFilesDir().getAbsolutePath(); 
                                              path = path.substring(0, path.lastIndexOf("/")) + "/documents";
                                              File file = new File(path);
                                              file.mkdirs();
                                              path     = file.getPath(); }
                                          break;  }
   case Const.DIRECTORY_DOWNLOADS     : { filePath = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS     );
                                          path     = filePath.getPath();
                                          break; }
   case Const.DIRECTORY_MOVIES        : { filePath = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_MOVIES        ); 
                                          path     = filePath.getPath();
                                          break; }
   case Const.DIRECTORY_MUSIC         : { filePath = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_MUSIC         );
                                          path     = filePath.getPath();
                                          break; }
   case Const.DIRECTORY_NOTIFICATIONS : { filePath = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_NOTIFICATIONS ); 
                                          path     = filePath.getPath();
                                          break; }
   case Const.DIRECTORY_PICTURES      : { filePath = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES      ); 
                                          path     = filePath.getPath();
                                          break; }
   case Const.DIRECTORY_PODCASTS      : { filePath = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PODCASTS      ); 
                                          path     = filePath.getPath();
                                          break; }
   case Const.DIRECTORY_RINGTONES     : { filePath = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_RINGTONES     ); 
                                          path     = filePath.getPath();
                                          break; }
   //
   case Const.DIRECTORY_App           : { try    { path = activity.getPackageManager().getApplicationInfo( pkgName, 0 ).sourceDir; }
   	                                      catch  ( NameNotFoundException e) {}           
                                          break; }
   case Const.DIRECTORY_Files         : { path = activity.getFilesDir().getAbsolutePath();                                            
                                          break; }
   case Const.DIRECTORY_SDCard        : { if (Environment.getExternalStorageState().equals(Environment.MEDIA_MOUNTED) == true)
                                            { filePath = Environment.getExternalStorageDirectory();
                                            	path     = filePath.getPath(); }                                     
                                          break; }
   case Const.DIRECTORY_DataBase      : { path = activity.getFilesDir().getPath();
                                          path = path.substring(0, path.lastIndexOf("/")) + "/databases";      
                                          File file = new File(path);
                                          file.mkdirs();
                                          path      = file.getPath();
                                          break; }
   case Const.DIRECTORY_Shared_Prefs  : { path      = activity.getFilesDir().getPath();
                                          path      = path.substring(0, path.lastIndexOf("/")) + "/shared_prefs";           
                                          File file = new File(path);
                                          file.mkdirs();
                                          path      = file.getPath();
                                          break; }                               
  }                                  
  return path;
 }
 

 // -------------------------------------------------------------------------
 //  Android Device
 // -------------------------------------------------------------------------

 public  String getDevModel() {
   return ( android.os.Build.MODEL );
 }

 //
 public  String getDevVersion () {
   return (Build.VERSION.RELEASE);
 }

 // Result: Device ID - LORDMAN
 // Remarks : Nexus7 (no moblie device) -> Crash : fixed code - Simon
 public  String getDevDeviceID() {
   TelephonyManager telephony = (TelephonyManager) activity.getSystemService(Context.TELEPHONY_SERVICE);
   return ( telephony.getDeviceId()    );
 }

 // Result: Phone Number - LORDMAN
 public  String getDevPhoneNumber() {
   TelephonyManager telephony = (TelephonyManager) activity.getSystemService(Context.TELEPHONY_SERVICE);
   return ( telephony.getLine1Number() );
 }

 // Result: None,Wifi,Mobile
 public  int getDevNetwork()  {
   int rtn = Const.Network_None;
   ConnectivityManager mgr = (ConnectivityManager)activity.getSystemService(Context.CONNECTIVITY_SERVICE);
   try  { if (mgr == null) return(rtn);
          NetworkInfo infoW = mgr.getNetworkInfo(ConnectivityManager.TYPE_WIFI);
          if ( (infoW.isAvailable() && infoW.isConnected() ) ) return( Const.Network_Wifi  );
          NetworkInfo infoM = mgr.getNetworkInfo(ConnectivityManager.TYPE_MOBILE);
          if ( (infoM.isAvailable() && infoM.isConnected() ) ) return( Const.Network_Mobile); }
   catch( Exception e)
        { return (rtn); }
   return(rtn);
 }

 // -------------------------------------------------------------------------
 //  Bitmap
 // -------------------------------------------------------------------------

 // Get Image Width,Height without Decoding
 public  int Image_getWH (String filename ) {
   BitmapFactory.Options options = new BitmapFactory.Options();
   options.inJustDecodeBounds = true;
   BitmapFactory.decodeFile(filename, options);
   Log.d(Const.LogHeader, "wh:" + Integer.toString(options.outWidth ) +
                            "x" + Integer.toString(options.outHeight) );
   return ( (options.outWidth << 16) | (options.outHeight) );
 }

 //
 public  Bitmap Image_resample(String infile,int size) {
   int iw,ih,im; // input image w,h, max
   int scale;    //
   int ow,oh;    // output image w,h

   // get input image w,h
   BitmapFactory.Options options = new BitmapFactory.Options();
   options.inJustDecodeBounds = true;
   BitmapFactory.decodeFile(infile, options);
   iw = options.outWidth;
   ih = options.outHeight;
   //
   im = Math.max(iw,ih);
   scale = 1;
   if                      (size <= (im/32))  { scale = 32; }
   if (((im/32) < size) && (size <= (im/16))) { scale = 16; }
   if (((im/16) < size) && (size <= (im/ 8))) { scale =  8; }
   if (((im/ 8) < size) && (size <= (im/ 4))) { scale =  4; }
   if (((im/ 4) < size) && (size <= (im/ 2))) { scale =  2; }
   //
   options.inJustDecodeBounds = false;
   options.inSampleSize       = scale;
   Bitmap src = BitmapFactory.decodeFile(infile, options);
   //
   if  (im == iw) { ow = size;
                    oh = Math.round((float)ow*((float)ih/(float)iw)); }
             else { oh = size;
                    ow = Math.round((float)oh*((float)iw/(float)ih)); }
   //
   return( Bitmap.createScaledBitmap(src, ow,oh, true) );
 }

 //
 public  void Image_save(Bitmap bmp, String filename) {
   try { FileOutputStream out = new FileOutputStream(filename);
         bmp.compress(Bitmap.CompressFormat.PNG, 100, out); }
   catch (Exception e)
       { e.printStackTrace(); }
 }

 // -------------------------------------------------------------------------
 //  TextView
 // -------------------------------------------------------------------------

 public  java.lang.Object jTextView_Create(int pasobj ) {
   return (java.lang.Object)( new jTextView(this.activity,this,pasobj));
 }

 public  void jTextView_Free(java.lang.Object textview) {
   jTextView obj = (jTextView)textview;
   obj.Free();
   //obj = null;
 }

 public  void jTextView_setXYWH (java.lang.Object textview,
                                int x, int y, int w, int h ) {
   ((jTextView)textview).setXYWH(x,y,w,h);
 }

 public  void jTextView_setParent(java.lang.Object textview,
                                 android.view.ViewGroup viewgroup) {
   ((jTextView)textview).setParent(viewgroup);
 }

 public  void jTextView_setParent2(java.lang.Object textview,
                                  android.view.ViewGroup viewgroup) {
   ((jTextView)textview).setParent2(viewgroup);
 }

 public  void jTextView_setEnabled(java.lang.Object textview, boolean enabled) {
   ((jTextView)textview).setEnabled(enabled);
 }

 public  void jTextView_setText(java.lang.Object textview, String str) {
   ((jTextView)textview).setText(str);
 }

 public  void jTextView_setTextColor(java.lang.Object textview, int color) {
   ((jTextView)textview).setTextColor(color);
 }

 public  void jTextView_setTextSize(java.lang.Object textview, int pxSize) {
   ((jTextView)textview).setTextSize(TypedValue.COMPLEX_UNIT_PX,pxSize);
 }

 public  String jTextView_getText(java.lang.Object textview) {
   return ((jTextView)textview).getText().toString();
 }

 // LORDMAN 2013-08-12
 public  void jTextView_setTextAlignment(java.lang.Object textview, int align) {
   ((jTextView)textview).setTextAlignment(align);
 }

 // 2015.03.04 DonAlfredo added Property
 public  void jEditText_setEditStyle(java.lang.Object edittext, int editStyle) {
   ((jEditText)edittext).setInputType(android.text.InputType.TYPE_CLASS_TEXT); ((jEditText)edittext).setMultiLine(editStyle); 
 }  
    
 // LORDMAN 2013-08-27
 public  void jEditText_setEnabled(java.lang.Object edittext, boolean enabled ) {
   jEditText obj = (jEditText)edittext;
   obj.setClickable            (enabled);
   obj.setEnabled              (enabled);
   obj.setFocusable            (enabled);
   obj.setFocusableInTouchMode (enabled);
 }

 // LORDMAN 2013-08-27
 public  void jEditText_setEditable(java.lang.Object edittext, boolean enabled ) {
   jEditText obj = (jEditText)edittext;
   obj.setClickable            (enabled);
   if (enabled) {
     obj.setEnabled              (enabled); } //<--- ReadOnly
   obj.setFocusable            (enabled);
   obj.setFocusableInTouchMode (enabled);
 }

 // -------------------------------------------------------------------------
 //  EditText
 // -------------------------------------------------------------------------

 public  java.lang.Object jEditText_Create(int pasobj ) {
   return (java.lang.Object)( new jEditText(this.activity,this,pasobj));
 }

 public  void jEditText_Free(java.lang.Object edittext) {
   jEditText obj = (jEditText)edittext;
   obj.Free();
   //obj = null;
 }

 public  void jEditText_setXYWH (java.lang.Object edittext,
                                int x, int y, int w, int h ) {
   ((jEditText)edittext).setXYWH(x,y,w,h);
 }

 public  void jEditText_setParent(java.lang.Object edittext,
                                 android.view.ViewGroup viewgroup) {
   ((jEditText)edittext).setParent(viewgroup);
 }

 public  void jEditText_setText(java.lang.Object edittext, String str) {
   ((jEditText)edittext).setText(str);
 }

 public  String jEditText_getText(java.lang.Object edittext) {
   return ((jEditText)edittext).getText().toString();
 }

 public  void jEditText_setTextColor(java.lang.Object edittext, int color) {
   ((jEditText)edittext).setTextColor(color);
 }

 public  void jEditText_setTextSize(java.lang.Object edittext, int pxSize) {
   ((jEditText)edittext).setTextSize(TypedValue.COMPLEX_UNIT_PX,pxSize);
 }

 public  void jEditText_setHint(java.lang.Object edittext, String hint) {
   ((jEditText)edittext).setHint(hint);
 }

 // LORDMAN - 2013-07-26
 public  void jEditText_SetFocus(java.lang.Object edittext) {
   ((jEditText)edittext).requestFocus();
 }

 // LORDMAN - 2013-07-26
 public  void jEditText_immShow(java.lang.Object edittext) {
   InputMethodManager imm = (InputMethodManager) activity.getSystemService(Context.INPUT_METHOD_SERVICE);
   imm.toggleSoftInput(0, InputMethodManager.SHOW_IMPLICIT);
 }

 // LORDMAN - 2013-07-26
 public  void jEditText_immHide(java.lang.Object edittext) {
   InputMethodManager imm = (InputMethodManager) activity.getSystemService(Context.INPUT_METHOD_SERVICE);
   imm.hideSoftInputFromWindow(((jEditText)edittext).getWindowToken(), 0);
 }

 // LORDMAN - 2013-07-26
 public  void jEditText_setEditType(java.lang.Object edittext, int editType) {
    switch (editType) {
    case Const.Edit_Type_Number     : { ((jEditText)edittext).setInputType(android.text.InputType.TYPE_CLASS_NUMBER); break; }
    case Const.Edit_Type_Text       : { ((jEditText)edittext).setInputType(android.text.InputType.TYPE_CLASS_TEXT  ); break; }
    case Const.Edit_Type_Phone      : { ((jEditText)edittext).setInputType(android.text.InputType.TYPE_CLASS_PHONE);  break; }
    case Const.Edit_Type_PassNumber : { ((jEditText)edittext).setInputType(android.text.InputType.TYPE_CLASS_NUMBER);
                                        ((jEditText)edittext).setTransformationMethod(android.text.method.PasswordTransformationMethod.getInstance()); break; }
    case Const.Edit_Type_PassText   : { ((jEditText)edittext).setInputType(android.text.InputType.TYPE_CLASS_TEXT);
                                        ((jEditText)edittext).setTransformationMethod(android.text.method.PasswordTransformationMethod.getInstance()); break; }
    default                         : { ((jEditText)edittext).setInputType(android.text.InputType.TYPE_CLASS_TEXT); };
    } 
 }   

 // LORDMAN - 2013-07-26
 public  void jEditText_maxLength(java.lang.Object edittext, int mLength) {
   InputFilter[] FilterArray = new InputFilter[1];
   FilterArray[0] = new InputFilter.LengthFilter(mLength);
   ((jEditText)edittext).setFilters(FilterArray);
 }

 // LORDMAN - 2013-07-26 , 2013-08-05
 public  int[] jEditText_GetCursorPos(java.lang.Object edittext) {

   int[] vals = new int[2];

   vals[0] = ((jEditText)edittext).getSelectionStart();
   vals[1] = ((jEditText)edittext).getSelectionEnd();

   return vals;
 }

 // LORDMAN - 2013-07-26, 2013-08-05
 public  void jEditText_SetCursorPos(java.lang.Object edittext, int startPos, int endPos) {
   if (endPos == 0) { endPos = startPos; };
   ((jEditText)edittext).setSelection(startPos,endPos);
 }

 // LORDMAN 2013-08-12
 public  void jEditText_setTextAlignment(java.lang.Object edittext, int align) {
 ((jEditText)edittext).setTextAlignment(align);
 }

 // -------------------------------------------------------------------------
 //  Button
 // -------------------------------------------------------------------------

 public  java.lang.Object jButton_Create(int pasobj ) {
   return (java.lang.Object)( new jButton(this.activity,this,pasobj));
 }

 public  void jButton_Free(java.lang.Object button) {
   jButton obj = (jButton)button;
   obj.Free();
   //obj = null;
 }

 public  void jButton_setXYWH  (java.lang.Object button,
                               int x, int y, int w, int h ) {
   ((jButton)button).setXYWH(x,y,w,h);
 }

 public  void jButton_setParent(java.lang.Object button,
                               android.view.ViewGroup viewgroup) {
   ((jButton)button).setParent(viewgroup);
 }

 public  void jButton_setText(java.lang.Object button, String str) {
   ((jButton)button).setText(str);
 }

 public  String jButton_getText(java.lang.Object button) {
   return ((jButton)button).getText().toString();
 }

 public  void jButton_setTextColor(java.lang.Object button, int color) {
   ((jButton)button).setTextColor(color);
 }

 public  void jButton_setTextSize(java.lang.Object button, int pxSize) {
   ((jButton)button).setTextSize(TypedValue.COMPLEX_UNIT_PX,pxSize);
 }

 // -------------------------------------------------------------------------
 //  CheckBox
 // -------------------------------------------------------------------------

 public  java.lang.Object jCheckBox_Create(int pasobj ) {
   return (java.lang.Object)( new jCheckBox(this.activity,this,pasobj));
 }

 public  void jCheckBox_Free(java.lang.Object checkbox) {
   jCheckBox obj = (jCheckBox)checkbox;
   obj.Free();
   //obj = null;
 }

 public  void jCheckBox_setXYWH (java.lang.Object checkbox,
                                int x, int y, int w, int h ) {
   ((jCheckBox)checkbox).setXYWH(x,y,w,h);
 }

 public  void jCheckBox_setParent(java.lang.Object checkbox,
                                 android.view.ViewGroup viewgroup) {
   ((jCheckBox)checkbox).setParent(viewgroup);
 }

 public  void jCheckBox_setText(java.lang.Object checkbox, String str) {
   ((jCheckBox)checkbox).setText(str);
 }

 public  void jCheckBox_setTextColor(java.lang.Object checkbox, int color) {
   ((jCheckBox)checkbox).setTextColor(color);
 }

 public  void jCheckBox_setTextSize(java.lang.Object checkbox, int pxSize) {
   ((jCheckBox)checkbox).setTextSize(TypedValue.COMPLEX_UNIT_PX,pxSize);
 }

 public  String jCheckBox_getText(java.lang.Object checkbox) {
   return ((jCheckBox)checkbox).getText().toString();
 }

 public  boolean jCheckBox_isChecked( java.lang.Object checkbox) {
   return ((jCheckBox)checkbox).isChecked();
 }

 public  void jCheckBox_setChecked( java.lang.Object checkbox, boolean value) {
   ((jCheckBox)checkbox).setChecked(value);
 }

 // -------------------------------------------------------------------------
 //  RadioButton
 // -------------------------------------------------------------------------

 public  java.lang.Object jRadioButton_Create(int pasobj ) {
   return (java.lang.Object)( new jRadioButton(this.activity,this,pasobj));
 }

 public  void jRadioButton_Free(java.lang.Object radiobutton) {
   jRadioButton obj = (jRadioButton)radiobutton;
   obj.Free();
   //obj = null;
 }

 public  void jRadioButton_setXYWH (java.lang.Object radiobutton,
                                int x, int y, int w, int h ) {
   ((jRadioButton)radiobutton).setXYWH(x,y,w,h);
 }

 public  void jRadioButton_setParent(java.lang.Object radiobutton,
                                 android.view.ViewGroup viewgroup) {
   ((jRadioButton)radiobutton).setParent(viewgroup);
 }

 public  void jRadioButton_setText(java.lang.Object radiobutton, String str) {
   ((jRadioButton)radiobutton).setText(str);
 }

 public  void jRadioButton_setTextColor(java.lang.Object radiobutton, int color) {
   ((jRadioButton)radiobutton).setTextColor(color);
 }

 public  void jRadioButton_setTextSize(java.lang.Object radiobutton, int pxSize) {
   ((jRadioButton)radiobutton).setTextSize(TypedValue.COMPLEX_UNIT_PX,pxSize);
 }

 public  String jRadioButton_getText(java.lang.Object radiobutton) {
   return ((jRadioButton)radiobutton).getText().toString();
 }

 public  boolean jRadioButton_isChecked( java.lang.Object radiobutton) {
   return ((jRadioButton)radiobutton).isChecked();
 }

 public  void jRadioButton_setChecked( java.lang.Object radiobutton, boolean value) {
   ((jRadioButton)radiobutton).setChecked(value);
 }

 // -------------------------------------------------------------------------
 //  ProgressBar
 // -------------------------------------------------------------------------

 public  java.lang.Object jProgressBar_Create(int pasobj, int style ) {
   return (java.lang.Object)( new jProgressBar(this.activity,this,pasobj,style));
 }

 public  void jProgressBar_Free(java.lang.Object progressbar) {
   jProgressBar obj = (jProgressBar)progressbar;
   obj.Free();
   //obj = null;
 }

 public  void jProgressBar_setXYWH (java.lang.Object progressbar,
                                int x, int y, int w, int h ) {
   ((jProgressBar)progressbar).setXYWH(x,y,w,h);
 }

 public  void jProgressBar_setParent(java.lang.Object progressbar,
                                 android.view.ViewGroup viewgroup) {
   ((jProgressBar)progressbar).setParent(viewgroup);
 }

 public  int jProgressBar_getProgress(java.lang.Object progressbar) {
   return ( ((jProgressBar)progressbar).getProgress() );
 }

 public  void jProgressBar_setProgress(java.lang.Object progressbar, int progress) {
   ((jProgressBar)progressbar).setProgress(progress);
 }

 // -------------------------------------------------------------------------
 //  ImageView
 // -------------------------------------------------------------------------

 public  java.lang.Object jImageView_Create( int pasobj ) {
   return (java.lang.Object)( new jImageView(this.activity,this,pasobj));
 }

 public  void jImageView_Free(java.lang.Object imageview) {
  jImageView obj = (jImageView)imageview;
  obj.Free();
  //obj = null;
 }

 public  void jImageView_setXYWH  (java.lang.Object imageview,
                                  int x, int y, int w, int h ) {
   ((jImageView)imageview).setXYWH(x,y,w,h);
 }

 public  void jImageView_setParent(java.lang.Object imageview,
                                  android.view.ViewGroup viewgroup) {
   ((jImageView)imageview).setParent(viewgroup);
 }

 public  void jImageView_setImage(java.lang.Object imageview, String str) {
   Bitmap bmp;
   bmp = ((jImageView)imageview).bmp;
   if (bmp != null)        { bmp.recycle(); }
   if (str.equals("null")) { ((jImageView)imageview).setImageBitmap(null);
                             return; };
   bmp = BitmapFactory.decodeFile( str );
   ((jImageView)imageview).setImageBitmap(bmp);
 }

 // -------------------------------------------------------------------------
 //  ListView
 // -------------------------------------------------------------------------

 public  java.lang.Object jListView_Create(int pasobj ) {
   return (java.lang.Object)( new jListView(this.activity,this,pasobj));
 }

 public  void jListView_Free(java.lang.Object listview) {
   jListView obj = (jListView)listview;
   obj.Free();
   //obj = null;
 }

 public  void jListView_setXYWH  (java.lang.Object listview,
                                   int x, int y, int w, int h ) {
   ((jListView)listview).setXYWH(x,y,w,h);
 }

 public  void jListView_setParent(java.lang.Object listview,
                                 android.view.ViewGroup viewgroup) {
   ((jListView)listview).setParent(viewgroup);
 }

 public  void jListView_setTextColor(java.lang.Object listview, int color) {
   ((jListView)listview).setTextColor(color);
 }

 public  void jListView_setTextSize(java.lang.Object listview, int pxSize) {
   ((jListView)listview).setTextSize(pxSize);
 }

 // LORDMAN - 2013-08-07
 public void jListView_setItemPosition (java.lang.Object listview, int position, int y)  {
   ((jListView)listview).setItemPosition(position, y);
 }

 // Item.add
 public  void jListView_add      (java.lang.Object listview, String item) {
   ((jListView)listview).add(item);
 }

 // Item.delete
 public  void jListView_delete   (java.lang.Object listview, int index)  {
   ((jListView)listview).delete(index);
 }

 // Item.clear
 public  void jListView_clear    (java.lang.Object listview) {
   ((jListView)listview).clear();
 }

 // -------------------------------------------------------------------------
 //  ScrollView
 // -------------------------------------------------------------------------

 public  java.lang.Object jScrollView_Create( int pasobj ) {
   return (java.lang.Object)( new jScrollView(this.activity,this,pasobj));
 }

 public  void jScrollView_Free(java.lang.Object scrollview) {
   jScrollView obj = (jScrollView)scrollview;
   obj.Free();
   //obj = null;
 }

 public  void jScrollView_setXYWH  (java.lang.Object scrollview,
                                   int x, int y, int w, int h ) {
   ((jScrollView)scrollview).setXYWH(x,y,w,h);
 }

 public  void jScrollView_setParent(java.lang.Object scrollview,
                                   android.view.ViewGroup viewgroup) {
   ((jScrollView)scrollview).setParent(viewgroup);
 }

 public  void jScrollView_setScrollSize(java.lang.Object scrollview, int size) {
   ((jScrollView)scrollview).setScrollSize(size);
 }

 public  android.view.ViewGroup jScrollView_getView(java.lang.Object scrollview) {
   return (android.view.ViewGroup)( ((jScrollView)scrollview).getView() );
 }

 // -------------------------------------------------------------------------
 //  ViewFlipper
 // -------------------------------------------------------------------------

 public  java.lang.Object jViewFlipper_Create( int pasobj ) {
   return (java.lang.Object)( new jViewFlipper(this.activity,this,pasobj));
 }

 public  void jViewFlipper_Free(java.lang.Object viewflipper) {
   jViewFlipper obj = (jViewFlipper)viewflipper;
   obj.Free();
   //obj = null;
 }

 public  void jViewFlipper_setXYWH  (java.lang.Object viewflipper,
                                   int x, int y, int w, int h ) {
   ((jViewFlipper)viewflipper).setXYWH(x,y,w,h);
 }

 public  void jViewFlipper_setParent(java.lang.Object viewflipper,
                                   android.view.ViewGroup viewgroup) {
   ((jViewFlipper)viewflipper).setParent(viewgroup);
 }

 // -------------------------------------------------------------------------
 //  WebView
 // -------------------------------------------------------------------------

 public  java.lang.Object jWebView_Create(int pasobj ) {
   return (java.lang.Object)( new jWebView(this.activity,this,pasobj));
 }

 public  void jWebView_Free(java.lang.Object webview) {
   jWebView obj = (jWebView)webview;
   obj.Free();
   //obj = null;
 }

 public  void jWebView_setXYWH  (java.lang.Object webview,
                                   int x, int y, int w, int h ) {
   ((jWebView)webview).setXYWH(x,y,w,h);
 }

 public  void jWebView_setParent(java.lang.Object webview,
                                   android.view.ViewGroup viewgroup) {
   ((jWebView)webview).setParent(viewgroup);
 }

 public  void jWebView_setJavaScript(java.lang.Object webview,boolean javascript) {
   ((jWebView)webview).getSettings().setJavaScriptEnabled(javascript);
 }

 public  void jWebView_loadURL(java.lang.Object webview, String str) {
   ((jWebView)webview).loadUrl("about:blank");
   ((jWebView)webview).loadUrl(str);
 }

 // -------------------------------------------------------------------------
 //  Canvas : canvas + paint
 // -------------------------------------------------------------------------

 public  java.lang.Object jCanvas_Create( int pasobj ) {
   return (java.lang.Object)( new jCanvas(this,pasobj));
 }

 public  void jCanvas_Free(java.lang.Object canvas) {
   jCanvas obj = (jCanvas)canvas;
   obj.Free();
   //obj = null;
 }

 public  void jCanvas_setStrokeWidth (java.lang.Object canvas,float width ) {
   ((jCanvas)canvas).setStrokeWidth(width);
 }

 public  void jCanvas_setStyle(java.lang.Object canvas, int style ) {
   ((jCanvas)canvas).setStyle(style);
 }

 public  void jCanvas_setColor(java.lang.Object canvas, int color) {
   ((jCanvas)canvas).setColor(color);
 }

 public  void jCanvas_setTextSize(java.lang.Object canvas, float textsize) {
   ((jCanvas)canvas).setTextSize(textsize);
 }

 public  void jCanvas_drawLine(java.lang.Object canvas, float x1, float y1, float x2, float y2) {
   ((jCanvas)canvas).drawLine(x1,y1,x2,y2);
 }

 public  void jCanvas_drawText(java.lang.Object canvas, String text, float x, float y) {
   ((jCanvas)canvas).drawText(text,x,y);
 }

 public  void jCanvas_drawBitmap(java.lang.Object canvas, Bitmap bmp, int b, int l, int r, int t) {
   ((jCanvas)canvas).drawBitmap(bmp,b,l,r,t);
 }

 // -------------------------------------------------------------------------
 //  Bitmap
 // -------------------------------------------------------------------------

 public  java.lang.Object jBitmap_Create( int pasobj ) {
   return (java.lang.Object)( new jBitmap(this,pasobj));
 }

 public  void jBitmap_Free(java.lang.Object bitmap) {
   jBitmap obj = (jBitmap)bitmap;
   obj.Free();
   //obj = null;
 }

 public  void jBitmap_loadFile(java.lang.Object bitmap, String filename) {
   ((jBitmap)bitmap).loadFile(filename);
 }

 public  void jBitmap_saveFile(java.lang.Object bitmap, String filename, int format) {
   try{
     File file = new File(filename);
     FileOutputStream fos = activity.openFileOutput(filename , Context.MODE_PRIVATE);
     //
     if (format == Const.CompressFormat_PNG)
          { ((jBitmap)bitmap).bmp.compress(CompressFormat.PNG, 100 , fos); }
     else { ((jBitmap)bitmap).bmp.compress(CompressFormat.JPEG, 95 , fos); };
     fos.flush();
     fos.close(); }
   catch(Exception e)
     { };
 }

 public  void jBitmap_createBitmap (java.lang.Object bitmap,int w, int h) {
   ((jBitmap)bitmap).createBitmap(w,h);
 }

 public  int[] jBitmap_getWH( java.lang.Object bitmap) {
   return ( ((jBitmap)bitmap).getWH() );
 }

 public  Bitmap jBitmap_getJavaBitmap( java.lang.Object bitmap) {
   return ( ((jBitmap)bitmap).bmp );
 }

 // -------------------------------------------------------------------------
 //  View
 // -------------------------------------------------------------------------

 public  java.lang.Object jView_Create( int pasobj ) {
   return (java.lang.Object)( new jView(this.activity,this,pasobj));
 }

 public  void jView_Free(java.lang.Object view) {
   jView obj = (jView)view;
   obj.Free();
   //obj = null;
 }

 public  void jView_setXYWH  (java.lang.Object view,int x, int y, int w, int h ) {
   ((jView)view).setXYWH(x,y,w,h);
 }

 public  void jView_setParent(java.lang.Object view, android.view.ViewGroup viewgroup) {
   ((jView)view).setParent(viewgroup);
 }

 public  void jView_setjCanvas(java.lang.Object view, java.lang.Object canvas) {
   ((jView)view).setjCanvas( (jCanvas)canvas );
 }

 public  void jView_saveView(java.lang.Object view, String filename) {
   ((jView)view).saveView(filename);
 }

 // -------------------------------------------------------------------------
 //  GLView
 // -------------------------------------------------------------------------

 public  java.lang.Object jGLSurfaceView_Create( int pasobj, int version ) {
   return (java.lang.Object)( new jGLSurfaceView(this.activity,this,pasobj,version));
 }

 public  void jGLSurfaceView_Free(java.lang.Object surfaceview) {
   jGLSurfaceView obj = (jGLSurfaceView)surfaceview;
   obj.Free();
   //obj = null;
 }

 public  void jGLSurfaceView_setXYWH  (java.lang.Object surfaceview,
                                      int x, int y, int w, int h ) {
   ((jGLSurfaceView)surfaceview).setXYWH(x,y,w,h);
 }

 public  void jGLSurfaceView_setParent(java.lang.Object surfaceview,
                                      android.view.ViewGroup viewgroup) {
   ((jGLSurfaceView)surfaceview).setParent(viewgroup);
 }


 public  void jGLSurfaceView_SetAutoRefresh(java.lang.Object glView, boolean active ) {
   if (active) { ((jGLSurfaceView)glView).setRenderMode( GLSurfaceView.RENDERMODE_CONTINUOUSLY ); }
         else  { ((jGLSurfaceView)glView).setRenderMode( GLSurfaceView.RENDERMODE_WHEN_DIRTY   ); }
 }

 public  void jGLSurfaceView_Refresh(java.lang.Object glView) {
   ((GLSurfaceView)glView).requestRender();
 }

 public  void jGLSurfaceView_deleteTexture(java.lang.Object surfaceview, int id) {
   ((jGLSurfaceView)surfaceview).deleteTexture(id);
 }

 public  void jGLSurfaceView_glThread(java.lang.Object surfaceview) {
   ((jGLSurfaceView)surfaceview).glThread();
 }

 // -------------------------------------------------------------------------
 //  CameraView : SurfaceView
 // -------------------------------------------------------------------------

 public  java.lang.Object jCameraView_Create(int pasobj ) {
   return (java.lang.Object)( new jCameraView(this.activity,this,pasobj));
 }

 public  void jCameraView_Free(java.lang.Object surfaceview) {
   jCameraView obj = (jCameraView)surfaceview;
   obj.Free();
   obj = null;
 }

 public  void jCameraView_setXYWH  (java.lang.Object surfaceview,
                                    int x, int y, int w, int h ) {
   ((jCameraView)surfaceview).setXYWH(x,y,w,h);
 }

 public  void jCameraView_setParent(java.lang.Object surfaceview,
                                    android.view.ViewGroup viewgroup) {
   ((jCameraView)surfaceview).setParent(viewgroup);
 }

 public  void jCameraView_saveImage(java.lang.Object surfaceview,
                                    String fileName) {
   ((jCameraView)surfaceview).saveImage(fileName);
 }

 public  void jCameraView_captureStart(java.lang.Object surfaceview) {
  ((jCameraView)surfaceview).captureStart();
 }

 public  void jCameraView_captureStop(java.lang.Object surfaceview) {
  ((jCameraView)surfaceview).captureStop();
 }

 // -------------------------------------------------------------------------
 //  Timer
 // -------------------------------------------------------------------------
 public  java.lang.Object jTimer_Create(int pasobj) {
   return (jTimer)(new jTimer(this,pasobj) );
 }

 public  void jTimer_Free(java.lang.Object timer) {
   jTimer obj = (jTimer)timer;
   obj.Free();
   //obj = null;
 }

 public  void jTimer_SetInterval(java.lang.Object timer, int interval ) {
   ((jTimer)timer).SetInterval(interval);
 }

 public  void jTimer_SetEnabled(java.lang.Object timer, boolean active ) {
   ((jTimer)timer).SetEnabled(active);
 }

 // -------------------------------------------------------------------------
 //  Dialog YN
 // -------------------------------------------------------------------------

 //jDialogYN DialogYNSav;
 Object DialogYNSav;

 public  java.lang.Object jDialogYN_Create(int pasobj,
                                           String title, String msg, String y, String n ) {
   return (jDialogYN)(new jDialogYN(activity,this,pasobj,title,msg,y,n) );
   //DialogYNSav = (jDialogYN)(new jDialogYN(activity,this,pasobj,title,msg,y,n) );
   //return DialogYNSav;
 }

 public  void jDialogYN_Free(java.lang.Object dialog) {
   jDialogYN obj = (jDialogYN)dialog;
   obj.Free();
   //obj = null;
 }

 public  void jDialogYN_Show(java.lang.Object dialog ) {
   Log.d(Const.LogHeader,"jDialogYN_Show");
   if ( dialog instanceof jDialogYN ) { Log.d(Const.LogHeader,"jDialogYN -> YES"); }
   else                               { Log.d(Const.LogHeader,"jDialogYN -> No" ); }
   ((jDialogYN)dialog).show();
 }

 // -------------------------------------------------------------------------
 //  Dialog Progress
 // -------------------------------------------------------------------------

 public  java.lang.Object jDialogProgress_Create(int pasobj, String title, String msg) {
   return (jDialogProgress)(new jDialogProgress(activity,this,pasobj,title,msg ) );
 }

 public  void jDialogProgress_Free(java.lang.Object dialog) {
   jDialogProgress obj = (jDialogProgress)dialog;
   obj.Free();
   //obj = null;
 }

 // -------------------------------------------------------------------------
 //  Toast
 // -------------------------------------------------------------------------

 //
 public  void jToast( String str ) {
   Toast.makeText(activity, str, Toast.LENGTH_SHORT).show();
 }

 // -------------------------------------------------------------------------
 //  jImageBtn
 // -------------------------------------------------------------------------

 public  java.lang.Object jImageBtn_Create( int pasobj ) {
   return (java.lang.Object)( new jImageBtn(this.activity,this,pasobj));
 }

 public  void jImageBtn_Free(java.lang.Object imagebtn) {
   jImageBtn obj = (jImageBtn)imagebtn;
   obj.Free();
   //obj = null;
 }

 public  void jImageBtn_setXYWH  (java.lang.Object imagebtn,int x, int y, int w, int h ) {
   ((jImageBtn)imagebtn).setXYWH(x,y,w,h);
 }

 public  void jImageBtn_setParent(java.lang.Object imagebtn, android.view.ViewGroup viewgroup) {
   ((jImageBtn)imagebtn).setParent(viewgroup);
 }

 public  void jImageBtn_setButton(java.lang.Object imagebtn, String bmpup, String bmpdn ) {
   ((jImageBtn)imagebtn).setButton(bmpup,bmpdn);
 }

 // LORDMAN 2013-08-16
 public  void jImageBtn_setEnabled(java.lang.Object imagebtn, boolean enabled ) {
   ((jImageBtn)imagebtn).setEnabled(enabled);
 }

 // -------------------------------------------------------------------------
 //  jAsyncTask
 // -------------------------------------------------------------------------

 public  java.lang.Object jAsyncTask_Create(int pasobj ) {
   return (java.lang.Object)( new jAsyncTask(this,pasobj));
 }

 public  void jAsyncTask_Free(java.lang.Object asynctask) {
   jAsyncTask obj = (jAsyncTask)asynctask;
   obj.Free();
 }

 public  void jAsyncTask_Execute(java.lang.Object asynctask) {
   ((jAsyncTask)asynctask).execute();
 }

 public  void jAsyncTask_setProgress(java.lang.Object asynctask, int progress) {
   ((jAsyncTask)asynctask).setProgress(progress);
 }

 // -------------------------------------------------------------------------
 //  jHttp
 // -------------------------------------------------------------------------

 public  java.lang.Object jHttp_Create(int pasobj ) {
   return (java.lang.Object)( new jHttp(this.activity,this,pasobj));
 }

 public  void jHttp_Free(java.lang.Object http) {
   jHttp obj = (jHttp)http;
   obj.Free();
 }

 public  void jHttp_getText(java.lang.Object http, String url) {
   ((jHttp)http).getText(url);
 }

 public  void jHttp_downloadFile(java.lang.Object http, String url,String localFile) {
   ((jHttp)http).downloadFile(url,localFile);
 }

 public  void jHttp_uploadFile(java.lang.Object http, String url,String localFile) {
   ((jHttp)http).uploadFile(url,localFile);
 }


 // -------------------------------------------------------------------------
 //  Bitmap
 // -------------------------------------------------------------------------

 public  int[] getBmpArray(String file) {
   Bitmap bmp = BitmapFactory.decodeFile(file);
   int   length = bmp.getWidth()*bmp.getHeight();
   int[] pixels = new int[length+2];
   bmp.getPixels(pixels, 0, bmp.getWidth(), 0, 0, bmp.getWidth(), bmp.getHeight());
   pixels[length+0] = bmp.getWidth ();
   pixels[length+1] = bmp.getHeight();
   return ( pixels );
 }


 // -------------------------------------------------------------------------
 //  Camera
 // -------------------------------------------------------------------------
 public void takePhoto(String filename) {
   Intent intent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);

   Uri mImageCaptureUri = Uri.fromFile(new File("", filename));

   intent.putExtra(android.provider.MediaStore.EXTRA_OUTPUT, mImageCaptureUri);
   intent.putExtra("return-data", true);
   activity.startActivityForResult(intent, 12345);
 }

 // -------------------------------------------------------------------------
 //  BenchMark [Java/Pascal]
 // -------------------------------------------------------------------------

 public float[] benchMark () {
   long start_time;
   long end_time;
   start_time = System.currentTimeMillis();
   //
   float value = 30;
   int i = 0;
   for ( i = 0; i < 100000000; i++) {
     value = value * 2/ 3 + 5 - 1;
   };
   end_time = System.currentTimeMillis();
   Log.e("BenchMark1","Java : " + (end_time - start_time) + ", result" + value);
   //
   float[] vals = new float[2];
   vals[0] = end_time - start_time;
   vals[1] = value;
   return ( vals );
 }

}

