unit ALFmxCommon;

interface

uses FMX.controls;

Procedure ALFmxMakeBufBitmaps(const aControl: TControl);

implementation

uses ALFmxText,
     ALFmxRectangle;

{******************************************************}
Procedure ALFmxMakeBufBitmaps(const aControl: TControl);
var aChild: TControl;
begin
  if (aControl is TALText) then TALText(aControl).MakeBufBitmap
  else if (aControl is TALRectangle) then TALRectangle(aControl).MakeBufBitmap;
  for aChild in aControl.Controls do
    ALFmxMakeBufBitmaps(aChild);
end;

end.
