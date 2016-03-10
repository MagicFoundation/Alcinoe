unit ALFmxCommon;

interface

uses FMX.controls;

Procedure ALFmxMakeBufBitmaps(const aControl: TControl; const NoBufBitmaps4MSWindows: boolean = True);

implementation

uses fmx.controls.presentation,
     ALFmxObjects,
     ALFmxImgList,
     ALFmxStylesObjects;

{****************************************************************************************************}
Procedure ALFmxMakeBufBitmaps(const aControl: TControl; const NoBufBitmaps4MSWindows: boolean = True);
var aChild: TControl;
begin

  {$IF Defined(MSWINDOWS)}
  if NoBufBitmaps4MSWindows then exit;
  {$ENDIF}

  if aControl is TPresentedControl then TPresentedControl(aControl).ApplyStyleLookup; // this to generate child controls
                                                                                      // that can be TALText for exemple
                                                                                      // (for the Tlabel)
  acontrol.DisableDisappear := true; // this to keep the style when the control get out of the visible are
                                     // else the style will be freed to be reaplied a little later
  if (aControl is TALText) then begin
    TALText(aControl).doubleBuffered := True;
    TALText(aControl).MakeBufBitmap;
  end
  else if (aControl is TALRectangle) then begin
    TALRectangle(aControl).doubleBuffered := True;
    TALRectangle(aControl).MakeBufBitmap;
  end
  else if (aControl is TALGlyph) then begin
    TALGlyph(aControl).doubleBuffered := True;
    TALGlyph(aControl).MakeBufBitmap;
  end
  else if (aControl is TALLine) then begin
    TALLine(aControl).doubleBuffered := True;
    TALLine(aControl).MakeBufBitmap;
  end
  else if (aControl is TALStyleObject) then begin
    TALStyleObject(aControl).doubleBuffered := True;
    TALStyleObject(aControl).MakeBufBitmap;
  end
  else if (aControl is TALCheckStyleObject) then begin
    TALCheckStyleObject(aControl).doubleBuffered := True;
    TALCheckStyleObject(aControl).MakeBufBitmap;
  end
  else if (aControl is TALButtonStyleObject) then begin
    TALButtonStyleObject(aControl).doubleBuffered := True;
    TALButtonStyleObject(aControl).MakeBufBitmap;
  end
  else if (aControl is TALButtonStyleTextObject) then begin
    TALButtonStyleTextObject(aControl).doubleBuffered := True;
    TALButtonStyleTextObject(aControl).MakeBufBitmap;
  end;
  for aChild in aControl.Controls do
    ALFmxMakeBufBitmaps(aChild);

end;

end.
