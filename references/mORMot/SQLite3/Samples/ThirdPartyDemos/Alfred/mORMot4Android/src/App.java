//
//   Java Activity for Pascal
//
//   http://blog.naver.com/simonsayz
//   Author : Simon,Choi 
//  
//
package com.mormot;

//
import java.lang.Override;
import android.app.Activity;
import android.content.Intent;
import android.content.res.Configuration;
import android.content.pm.ActivityInfo; 
import android.widget.RelativeLayout;
import android.view.WindowManager;
import android.view.Window;
import android.view.View;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;

// http://stackoverflow.com/questions/16282294/show-title-bar-from-code
public class App extends Activity {
    private Controls       controls;

    @Override
    public void onCreate(Bundle savedInstanceState) {
     super.onCreate(savedInstanceState);                            
      //
      Log.d(Const.LogHeader,"onCreate");
      controls             = new Controls();    // process Load Lib, Lib main , Jni_OnLoad
      Log.d(Const.LogHeader,"Controls created");
      controls.activity    = this; 
      controls.appLayout   = new RelativeLayout(this);
      controls.appLayout.getRootView().setBackgroundColor (0x00000000);

      // Event : Java -> Pascal : Get ScreenStyle,ScreenOrient
      controls.screenStyle       = controls.jOnAppScreenStyle();       // Normal,Full
      controls.screenOrientation = controls.jOnAppScreenOrientation(); // Portrait,...

      // Set ScreenStyle,ScreenOrientation
      controls.systemSetScreenStyle      (controls.screenStyle      );
      controls.systemSetScreenOrientation(controls.screenOrientation);

      this.setContentView(controls.appLayout);
      this.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_PAN);
      // Event : Java -> Pascal
      controls.jOnAppCreate(controls.appLayout);
    }
    
    @Override
    protected void onNewIntent(Intent intent) { super.onNewIntent(intent);
    	                                        controls.jOnAppNewIntent();     }
    
    @Override
    protected void onDestroy()                { super.onDestroy(); 
    	                                        controls.jOnAppDestroy();       }
    
    @Override
    protected void onPause()                  { super.onPause(); 
    	                                        controls.jOnAppPause();         }
    
    @Override
    protected void onRestart()                { super.onRestart(); 
    	                                        controls.jOnAppRestart();       }

    @Override
    protected void onResume()                 { super.onResume();  
    	                                        controls.jOnAppResume();        }

    @Override
    protected void onStart()                  { super.onStart();   
    	                                        controls.jOnAppActive();        }

    @Override
    protected void onStop()                   { super.onStop(); 
    	                                        controls.jOnAppStop();          }

    @Override
    public    void onBackPressed()            { controls.jOnAppBackPressed();   }
    
    @Override
    public    void onConfigurationChanged(Configuration newConfig)
    {
    	super.onConfigurationChanged(newConfig);
    	controls.jOnAppRotate(newConfig.orientation);
    	controls.jOnAppConfigurationChanged();
    }	   	
 
    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
      controls.jOnAppActivityResult(requestCode,resultCode,data);                                       
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu)
    {
      menu.add(Menu.NONE, 0, Menu.NONE, "History").setIcon(R.drawable.ic_menu_recent_history)
          .setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
      menu.add(Menu.NONE, 1, Menu.NONE, "Quit").setIcon(R.drawable.ic_menu_search)
          .setShowAsAction(MenuItem.SHOW_AS_ACTION_WITH_TEXT);


      //return true;
      return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item)
    {
    	//check selected menu item
    	if(item.getItemId() == 1)
    	{
    		//close the Activity
    		this.finish();
    		return true;
    	}
    	return false;
    }

}
