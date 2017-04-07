package com.alcinoe.widget;

import android.widget.LinearLayout;
import android.util.AttributeSet;
import android.content.Context;
import android.view.WindowManager;
import android.view.WindowManager.LayoutParams;

public class ALControlHostLayout extends LinearLayout {

  public ALControlHostLayout(Context context) {
    super(context);
  }

  public ALControlHostLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
  }
  
  public ALControlHostLayout(Context context, AttributeSet attrs, int defStyleAttr) {
    super(context, attrs, defStyleAttr);
  }

  public ALControlHostLayout(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
    super(context, attrs, defStyleAttr, defStyleRes);
  }
  
  /**
  * by default android animate the move of the view added via the
  * WindowManager. this is not very good for our design because 
  * the view is part of our form layout and must not do any animation
  *
  * i didn't find any other way than accessing hidden privateFlags
  * other way is to remove the view and add it back at the correct
  * position
  *
  * this function must be called AFTER the view is added to the firemonkey
  * viewstack
  *
  */
  public boolean disableMoveAnimations() {

    try {
      
      // i take this behavior by looking the firemonkey ViewStack.java in the classes.dex
      // it's look like they store the WindowManager.LayoutParams in the tag of the view
      
      WindowManager.LayoutParams lp = (WindowManager.LayoutParams) getTag();
      if (lp != null) {
        int currentFlags = (Integer) lp.getClass().getField("privateFlags").get(lp); // public int privateFlags;
        lp.getClass().getField("privateFlags").set(lp, currentFlags|0x00000040); // public static final int PRIVATE_FLAG_NO_MOVE_ANIMATION = 0x00000040;
        setTag(lp);
        return true;
      }
  
    } catch (Exception e) {
       // hide the exception - Probably using other version of android/firemonkey
    }
    
    return false;

  }

}
