package com.alcinoe.datepicker;

import android.app.DatePickerDialog;
import android.widget.DatePicker;
import android.content.Context;
import android.content.DialogInterface;

public class ALDatePickerDialog implements DialogInterface.OnClickListener {

  private DatePickerDialog mDatePickerDialog;
  private ALDatePickerDialogListener mDatePickerDialogListener;

	public ALDatePickerDialog(Context context, 
	                          CharSequence button_positive_text,
	                          CharSequence button_negative_text,
	                          CharSequence button_neutral_text) {

		mDatePickerDialog = new DatePickerDialog(context, null/*listener*/, 2000/*year*/, 1/*month*/, 1/*dayOfMonth*/);
		mDatePickerDialogListener = null;

    if (button_positive_text != null) { mDatePickerDialog.setButton(android.content.DialogInterface.BUTTON_POSITIVE, button_positive_text, this/*listener*/); }
    if (button_negative_text != null) { mDatePickerDialog.setButton(android.content.DialogInterface.BUTTON_NEGATIVE, button_negative_text, this/*listener*/); }
    if (button_neutral_text != null) { mDatePickerDialog.setButton(android.content.DialogInterface.BUTTON_NEUTRAL, button_neutral_text, this/*listener*/); }
        
    mDatePickerDialog.setOnCancelListener(new DialogInterface.OnCancelListener() {
        @Override
        public void onCancel(DialogInterface dialogInterface) {
          if (mDatePickerDialogListener != null) { mDatePickerDialogListener.onBtnClick(0, 
                                                                                        mDatePickerDialog.getDatePicker().getYear(),
                                                                                        mDatePickerDialog.getDatePicker().getMonth(),
                                                                                        mDatePickerDialog.getDatePicker().getDayOfMonth()); }  
        }
    });
    
	}

  public void setListener(ALDatePickerDialogListener listener) {
    mDatePickerDialogListener = listener;
  }    

	public void show(int year, int month, int dayOfMonth) {
	  mDatePickerDialog.getDatePicker().updateDate(year, month, dayOfMonth); 
    mDatePickerDialog.show();
	}

  @Override
  public void onClick(DialogInterface dialog, int which) {
    if (mDatePickerDialogListener != null) { mDatePickerDialogListener.onBtnClick(which, 
                                                                                  mDatePickerDialog.getDatePicker().getYear(),
                                                                                  mDatePickerDialog.getDatePicker().getMonth(),
                                                                                  mDatePickerDialog.getDatePicker().getDayOfMonth()); }  
  }
  
}
