program CodeBuilder;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  system.AnsiStrings,
  system.Classes,
  System.SysUtils,
  System.IOUtils,
  Alcinoe.Execute,
  Alcinoe.files,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.common;

{***********************************************}
Procedure ExecuteCmdLine(const ACmdLine: String);
begin
  Var LInputStream := TMemorystream.Create;
  Var LOutputStream := TStringStream.Create;
  try
    Var LCmdLineResult := ALWinExecW(
                            ACmdLine, // const aCommandLine: String;
                            LInputStream, // const aInputStream: Tstream;
                            LOutputStream); //const aOutputStream: TStream;
    if LCmdLineResult <> 0 then
      raise Exception.Createfmt('Failed to execute %s'#13#10'%s', [ACmdLine, LOutputStream.DataString]);
  finally
    ALFreeandNil(LInputStream);
    ALFreeandNil(LOutputStream);
  end;
end;

{***************************************}
Procedure BuildAlcinoeFMXDynamicControls;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function FindAndReplace(const Source, OldPattern, NewPattern: AnsiString): AnsiString; overload;
  begin
    if (ALPosIgnoreCaseA(OldPattern, Source) <= 0) then begin
      ALSaveStringToFile(Source, ALgetModulePathW + 'Error_FindAndReplace_Source.pas');
      ALSaveStringToFile(OldPattern, ALgetModulePathW + 'Error_FindAndReplace_OldPattern.pas');
      Raise Exception.Create('Could not find OldPattern: '+ String(OldPattern));
    end;
    Result := ALStringReplaceA(Source, OldPattern, NewPattern, [RFIgnoreCase, RfReplaceALL]);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function FindAndReplace(const Source, OldPattern, NewPattern: AnsiString; const BlockStartPattern, BlockEndPattern: AnsiString): AnsiString; overload;
  begin
    var P1 := ALPosIgnoreCaseA(BlockStartPattern, Source);
    If P1 <= 0 then begin
      ALSaveStringToFile(Source, ALgetModulePathW + 'Error_FindAndReplace_Source.pas');
      ALSaveStringToFile(BlockStartPattern, ALgetModulePathW + 'Error_FindAndReplace_BlockStartPattern.pas');
      Raise Exception.Create('Could not find BlockStartPattern: '+ String(BlockStartPattern));
    end;
    var P2 := ALPosIgnoreCaseA(BlockEndPattern, Source, P1);
    If P2 <= 0 then begin
      ALSaveStringToFile(Source, ALgetModulePathW + 'Error_FindAndReplace_Source.pas');
      ALSaveStringToFile(BlockEndPattern, ALgetModulePathW + 'Error_FindAndReplace_BlockEndPattern.pas');
      Raise Exception.Create('Could not find BlockEndPattern: '+ String(BlockEndPattern));
    end;
    var LStr := ALcopyStr(Source, P1, P2-P1);
    LStr := FindAndReplace(LStr, OldPattern, NewPattern);
    result := Source;
    delete(Result, P1, P2-P1);
    insert(LStr, Result, P1);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function AddDefaultBoundsClass(const ASource, AClassName, ABoundsName, ADefaultValue: AnsiString): AnsiString;
  begin
    Result := ASource;
    var LPattern: AnsiString := 'constructor '+AClassName+'.Create(const'; // constructor TALDynamicListBoxSwitch.TThumb.Create(const
    var P1 := AlposIgnoreCaseA(LPattern, Result);
    If P1 <= 0 then begin
      ALSaveStringToFile(ASource, ALgetModulePathW + 'Error_FindAndReplace_Source.pas');
      ALSaveStringToFile(LPattern, ALgetModulePathW + 'Error_FindAndReplace_Pattern.pas');
      Raise Exception.Create('Could not find Pattern: '+ String(LPattern));
    end;
    //--
    LPattern := #13#10+'end;'#13#10; // end;
    Var P2 := AlposIgnoreCaseA(LPattern, Result, P1);
    If P2 <= 0 then begin
      ALSaveStringToFile(ASource, ALgetModulePathW + 'Error_FindAndReplace_Source.pas');
      ALSaveStringToFile(LPattern, ALgetModulePathW + 'Error_FindAndReplace_Pattern.pas');
      Raise Exception.Create('Could not find Pattern: '+ String(LPattern));
    end;
    inc(P2,length(LPattern));
    //--
    Insert(
      #13#10+
      '{***********************************************}'#13#10+
      'function '+AClassName+'.Create'+ABoundsName+': TALBounds;'#13#10+
      'begin'#13#10+
      '  Result := T'+ABoundsName+'.Create;'#13#10+
      'end;'#13#10,
      Result,
      P2);
    //--
    if AClassName <> 'TALDynamicListBoxCustomTrack.TTrack' then begin
      LPattern := ABoundsName+'.DefaultValue := TRectF.Create('+ADefaultValue+');'; // Margins.DefaultValue := TRectF.Create(4,4,4,4);
      var P3 := AlposIgnoreCaseA(LPattern,Result,P1);
      If (P3 <= 0) or (P3 >= P2) then begin
        ALSaveStringToFile(ASource, ALgetModulePathW + 'Error_FindAndReplace_Source.pas');
        ALSaveStringToFile(LPattern, ALgetModulePathW + 'Error_FindAndReplace_Pattern.pas');
        Raise Exception.Create('Could not find Pattern: '+ String(LPattern));
      end;
      Insert('//',Result,P3);
      //--
      LPattern := ABoundsName+'.Rect := '+ABoundsName+'.DefaultValue;'; // Margins.Rect := Margins.DefaultValue;
      P3 := AlposIgnoreCaseA(LPattern,Result,P1);
      If (P3 <= 0) or (P3 >= P2) then begin
        ALSaveStringToFile(ASource, ALgetModulePathW + 'Error_FindAndReplace_Source.pas');
        ALSaveStringToFile(LPattern, ALgetModulePathW + 'Error_FindAndReplace_Pattern.pas');
        Raise Exception.Create('Could not find Pattern: '+ String(LPattern));
      end;
      Insert('//',Result,P3);
      //--
      LPattern := 'var L'+ABoundsName+'Change: TNotifyEvent := '+ABoundsName+'.OnChange;'; // var LMarginsChange: TNotifyEvent := Margins.OnChange;
      P3 := AlposIgnoreCaseA(LPattern,Result,P1);
      If (P3 > 0) and (P3 < P2) then Insert('//',Result,P3);
      //--
      LPattern := ABoundsName+'.OnChange := nil;'; // Margins.OnChange := nil;
      P3 := AlposIgnoreCaseA(LPattern,Result,P1);
      If (P3 > 0) and (P3 < P2) then Insert('//',Result,P3);
      //--
      LPattern := ABoundsName+'.OnChange := L'+ABoundsName+'Change;'; // Margins.OnChange := LMarginsChange;
      P3 := AlposIgnoreCaseA(LPattern,Result,P1);
      If (P3 > 0) and (P3 < P2) then Insert('//',Result,P3);
    end;
    //--
    While (P1 > 0) and (Result[P1] <> '{') do dec(P1);
    If P1 <= 0 then raise Exception.Create('Error 692F9B26-0A5A-4482-B423-F073E4F81003');
    Insert(
      '{*******************************************************}'#13#10+
      'function '+AClassName+'.T'+ABoundsName+'.GetDefaultValue: TRectF;'#13#10+
      'begin'#13#10+
      '  Result := TRectF.Create('+ADefaultValue+');'#13#10+
      'end;'#13#10+
      #13#10,
      Result,
      P1);
    //--
    P1 := 1;
    var LLst := TalStringListA.create;
    Try
      LLst.LineBreak := '.';
      LLst.Text := AClassName;
      if LLst.Count <= 0 then raise Exception.Create('Error 9DBDDEC9-6089-4832-BECE-705820A62559');
      For var I := 0 To LLst.Count - 1 do begin
        LPattern := LLst[i] + ' = class('; // TALDynamicListBoxSwitch = class(
        P1 := AlposIgnoreCaseA(LPattern, Result, P1);
        If P1 <= 0 then begin
          ALSaveStringToFile(ASource, ALgetModulePathW + 'Error_FindAndReplace_Source.pas');
          ALSaveStringToFile(LPattern, ALgetModulePathW + 'Error_FindAndReplace_Pattern.pas');
          Raise Exception.Create('Could not find Pattern: '+ String(LPattern));
        end;
      end;
      var LIndentStr: AnsiString := '';
      P2 := P1 - 1;
      While (P2 > 0) and (Result[P2] <> #10) do begin
        LIndentStr := LIndentStr + ' ';
        dec(P2);
      end;
      If P2 <= 0 then raise Exception.Create('Error 692F9B26-0A5A-4482-B423-F073E4F81003');
      While (P1 <= high(Result)) and (Result[P1] <> #10) do inc(P1);
      inc(P1);
      Insert(
        LIndentStr+'public'#13#10+
        LIndentStr+'  Type'#13#10+
        LIndentStr+'    T'+ABoundsName+' = class(TALBounds)'#13#10+
        LIndentStr+'    protected'#13#10+
        LIndentStr+'      function GetDefaultValue: TRectF; override;'#13#10+
        LIndentStr+'    end;'#13#10+
        LIndentStr+'protected'#13#10+
        LIndentStr+'  function Create'+ABoundsName+': TALBounds; override;'#13#10,
        Result,
        P1);
    Finally
      ALFreeAndNil(LLst);
    End;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure UpdateSourceCode(Var aSrc: AnsiString);
  begin
    aSrc := FindAndReplace(aSrc, 'published', 'public');
    //--
    aSrc := FindAndReplace(aSrc, ' = class(TALControl)', ' = class(TALDynamicListBoxExtendedControl)');
    aSrc := FindAndReplace(aSrc, 'TALControl', 'TALDynamicListBoxControl');
    aSrc := FindAndReplace(aSrc, 'TALShape', 'TALDynamicListBoxShape');
    aSrc := FindAndReplace(aSrc, 'TALImage', 'TALDynamicListBoxImage');
    aSrc := FindAndReplace(aSrc, 'TALBaseRectangle', 'TALDynamicListBoxBaseRectangle');
    aSrc := FindAndReplace(aSrc, 'TALRectangle', 'TALDynamicListBoxRectangle');
    aSrc := FindAndReplace(aSrc, 'TALCircle', 'TALDynamicListBoxCircle');
    aSrc := FindAndReplace(aSrc, 'TALLine', 'TALDynamicListBoxLine');
    aSrc := FindAndReplace(aSrc, 'TALBaseText', 'TALDynamicListBoxBaseText');
    aSrc := FindAndReplace(aSrc, 'TALText', 'TALDynamicListBoxText');
    aSrc := FindAndReplace(aSrc, 'TALBaseStateStyle', 'TALDynamicListBoxBaseStateStyle');
    //aSrc := FindAndReplace(aSrc, 'TALAniIndicator', 'TALDynamicListBoxAniIndicator');
    aSrc := FindAndReplace(aSrc, 'TALBaseCheckBox', 'TALDynamicListBoxBaseCheckBox');
    aSrc := FindAndReplace(aSrc, 'TALCheckBox', 'TALDynamicListBoxCheckBox');
    aSrc := FindAndReplace(aSrc, 'TALRadioButton', 'TALDynamicListBoxRadioButton');
    aSrc := FindAndReplace(aSrc, 'TALSwitch', 'TALDynamicListBoxSwitch');
    aSrc := FindAndReplace(aSrc, 'TALButton', 'TALDynamicListBoxButton');
    aSrc := FindAndReplace(aSrc, 'TALCustomTrack', 'TALDynamicListBoxCustomTrack');
    aSrc := FindAndReplace(aSrc, 'TALTrackBar', 'TALDynamicListBoxTrackBar');
    aSrc := FindAndReplace(aSrc, 'TALRangeTrackBar', 'TALDynamicListBoxRangeTrackBar');
    aSrc := FindAndReplace(aSrc, 'TALCustomScrollBar', 'TALDynamicListBoxCustomScrollBar');
    aSrc := FindAndReplace(aSrc, 'TALScrollBar', 'TALDynamicListBoxScrollBar');
    aSrc := FindAndReplace(aSrc, 'TALLayout', 'TALDynamicListBoxLayout');
    aSrc := FindAndReplace(aSrc, 'TALVideoPlayerSurface', 'TALDynamicListBoxVideoPlayerSurface');
    //---
    aSrc := FindAndReplace(aSrc, 'TALDynamicListBoxImageWrapMode', 'TALImageWrapMode');
    aSrc := FindAndReplace(aSrc, 'TALDynamicListBoxLineType', 'TALLineType');
    aSrc := FindAndReplace(aSrc, 'TALDynamicListBoxTextElement', 'TALTextElement');
    aSrc := FindAndReplace(aSrc, 'TALDynamicListBoxBaseTextSettings', 'TALBaseTextSettings');
    aSrc := FindAndReplace(aSrc, 'TALDynamicListBoxTextDecoration', 'TALTextDecoration');
    aSrc := FindAndReplace(aSrc, 'TALDynamicListBoxTextSettings', 'TALTextSettings');
    aSrc := FindAndReplace(aSrc, 'TALDynamicListBoxTextDirection', 'TALTextDirection');
    aSrc := FindAndReplace(aSrc, 'TALDynamicListBoxTextHorzAlign', 'TALTextHorzAlign');
    //---
    //aSrc := FindAndReplace(aSrc, ' property Action', ' //property Action');
    aSrc := FindAndReplace(aSrc, ' property Anchors', ' //property Anchors');
    aSrc := FindAndReplace(aSrc, ' property CanFocus', ' //property CanFocus');
    //aSrc := FindAndReplace(aSrc, ' property CanParentFocus', ' //property CanParentFocus');
    //aSrc := FindAndReplace(aSrc, ' property DisableFocusEffect', ' //property DisableFocusEffect');
    aSrc := FindAndReplace(aSrc, ' property ClipChildren', ' //property ClipChildren');
    //aSrc := FindAndReplace(aSrc, ' property ClipParent', ' //property ClipParent');
    aSrc := FindAndReplace(aSrc, ' property DragMode', ' //property DragMode');
    aSrc := FindAndReplace(aSrc, ' property EnableDragHighlight', ' //property EnableDragHighlight');
    //aSrc := FindAndReplace(aSrc, ' property Hint', ' //property Hint');
    //aSrc := FindAndReplace(aSrc, ' property ParentShowHint', ' //property ParentShowHint');
    //aSrc := FindAndReplace(aSrc, ' property ShowHint', ' //property ShowHint');
    aSrc := FindAndReplace(aSrc, ' property Locked', ' //property Locked');
    aSrc := FindAndReplace(aSrc, ' property PopupMenu', ' //property PopupMenu');
    aSrc := FindAndReplace(aSrc, ' property Position', ' //property Position');
    aSrc := FindAndReplace(aSrc, ' property RotationAngle', ' //property RotationAngle');
    //aSrc := FindAndReplace(aSrc, ' property RotationCenter', ' //property RotationCenter');
    //aSrc := FindAndReplace(aSrc, ' property Pivot', ' //property Pivot');
    //aSrc := FindAndReplace(aSrc, ' property Scale', ' //property Scale');
    aSrc := FindAndReplace(aSrc, ' property Size', ' //property Size');
    aSrc := FindAndReplace(aSrc, ' property TabOrder', ' //property TabOrder');
    aSrc := FindAndReplace(aSrc, ' property TabStop', ' //property TabStop');
    aSrc := FindAndReplace(aSrc, ' property OnCanFocus', ' //property OnCanFocus');
    aSrc := FindAndReplace(aSrc, ' property OnDragEnter', ' //property OnDragEnter');
    aSrc := FindAndReplace(aSrc, ' property OnDragLeave', ' //property OnDragLeave');
    aSrc := FindAndReplace(aSrc, ' property OnDragOver', ' //property OnDragOver');
    aSrc := FindAndReplace(aSrc, ' property OnDragDrop', ' //property OnDragDrop');
    aSrc := FindAndReplace(aSrc, ' property OnDragEnd', ' //property OnDragEnd');
    aSrc := FindAndReplace(aSrc, ' property OnEnter', ' //property OnEnter');
    aSrc := FindAndReplace(aSrc, ' property OnExit', ' //property OnExit');
    aSrc := FindAndReplace(aSrc, ' property OnMouseWheel', ' //property OnMouseWheel');
    //aSrc := FindAndReplace(aSrc, ' property OnDblClick', ' //property OnDblClick');
    aSrc := FindAndReplace(aSrc, ' property OnKeyDown', ' //property OnKeyDown');
    aSrc := FindAndReplace(aSrc, ' property OnKeyUp', ' //property OnKeyUp');
    aSrc := FindAndReplace(aSrc, ' property OnResize', ' //property OnResize');
    //--
    aSrc := FindAndReplace(aSrc, '_TControlAccessProtected(ParentControl)','OwnerControl');
    aSrc := FindAndReplace(aSrc, '_TControlAccessProtected(FParent).click;','FParent.click;');
    //--
    aSrc := FindAndReplace(aSrc, 'FALParentControl := nil;','//FALParentControl := nil;');
    aSrc := FindAndReplace(aSrc, 'FALParentControl: TALDynamicListBoxControl;', '//FALParentControl: TALDynamicListBoxControl;');
    aSrc := FindAndReplace(aSrc, 'property ALParentControl: TALDynamicListBoxControl read FALParentControl;', '//property ALParentControl: TALDynamicListBoxControl read FALParentControl;');
    aSrc := FindAndReplace(aSrc, 'ALParentControl', 'OwnerControl');
    aSrc := FindAndReplace(aSrc, 'ParentControl', 'OwnerControl');
    aSrc := FindAndReplace(aSrc, 'var P: TControl := Self;','var P: TALDynamicListBoxControl := Self;');
    //--
    aSrc := FindAndReplace(aSrc, 'Size.SetPlatformDefaultWithoutNotification(False);','//Size.SetPlatformDefaultWithoutNotification(False);');
    aSrc := FindAndReplace(aSrc, ' Size.Height ',' Height ');
    aSrc := FindAndReplace(aSrc, ' Size.Width ',' Width ');
    aSrc := FindAndReplace(aSrc, '(Size.Height,','(Height,');
    aSrc := FindAndReplace(aSrc, '(Size.Width,','(Width,');
    aSrc := FindAndReplace(aSrc, 'Size.Size := ALAlignDimensionToPixelRound(Size.Size, ALGetScreenScale, TEpsilon.Position);', 'SetSize(ALAlignDimensionToPixelRound(TSizeF.Create(Width, Height), ALGetScreenScale, TEpsilon.Position));');
    aSrc := FindAndReplace(aSrc, 'LMaxThumb.size.size := size.size;','LMaxThumb.SetSize(TSizeF.Create(Width, Height));');
    aSrc := FindAndReplace(aSrc, 'Size.Size.IsZero', 'BoundsRect.IsEmpty');
    aSrc := FindAndReplace(aSrc, 'function IsSizeStored: Boolean;', '//function IsSizeStored: Boolean;');
    aSrc := FindAndReplace(aSrc, '//function IsSizeStored: Boolean;','function IsSizeStored: Boolean;','TStopIndicatorBrush = class(TALPersistentObserver)','end;');
    aSrc := FindAndReplace(aSrc, '//property Size: Single read FSize write SetSize stored IsSizeStored nodefault;','property Size: Single read FSize write SetSize stored IsSizeStored nodefault;','TStopIndicatorBrush = class(TALPersistentObserver)','end;');
    //--
    aSrc := FindAndReplace(aSrc, 'Position.X', 'Left');
    aSrc := FindAndReplace(aSrc, 'Position.Y', 'Top');
    aSrc := FindAndReplace(aSrc, 'PressedLeft','PressedPosition.X');
    aSrc := FindAndReplace(aSrc, 'PressedTop','PressedPosition.Y');
    aSrc := FindAndReplace(aSrc, 'FPressedThumbPos := FThumb.Position.Point','FPressedThumbPos := TPointF.Create(Single(FThumb.left), Single(FThumb.top))');
    aSrc := FindAndReplace(aSrc, 'fCustomTrackMouseDownPos := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(PressedPosition));','fCustomTrackMouseDownPos := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(PressedPosition)).ReducePrecision;');
    aSrc := FindAndReplace(aSrc, 'var LCustomTrackMousePos := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(TpointF.Create(X,Y)));','var LCustomTrackMousePos := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(TpointF.Create(X,Y))).ReducePrecision;');
    aSrc := FindAndReplace(
              aSrc,
              '    Position.Point := TpointF.Create('#13#10+
              '                        AThumb.Left - ((Width - AThumb.Width) / 2),'#13#10+
              '                        AThumb.Top - Height - Margins.Bottom);',
              '    SetPosition('#13#10+
              '      TpointF.Create('#13#10+
              '        AThumb.Left - ((Width - AThumb.Width) / 2),'#13#10+
              '        AThumb.Top - Height - Margins.Bottom));');


    aSrc := FindAndReplace(
              aSrc,
              '    Position.Point := TpointF.Create('#13#10+
              '                        AThumb.Left + AThumb.Width + Margins.Left,'#13#10+
              '                        AThumb.Top - ((Height - AThumb.Height) / 2));',
              '    SetPosition('#13#10+
              '      TpointF.Create('#13#10+
              '        AThumb.Left + AThumb.Width + Margins.Left,'#13#10+
              '        AThumb.Top - ((Height - AThumb.Height) / 2)));');
    //--
    aSrc := FindAndReplace(aSrc, 'SetBounds(X, Y, AWidth, AHeight: Single)','SetBounds(X, Y, AWidth, AHeight: Double)');
    aSrc := FindAndReplace(aSrc, 'PointInObjectLocal(X, Y: Single): Boolean;','PointInObjectLocal(X, Y: Double): Boolean;');
    //--
    aSrc := FindAndReplace(aSrc, 'FControlAbsolutePosAtMouseDown: TpointF;','FControlAbsolutePosAtMouseDown: TALPointD;');
    aSrc := FindAndReplace(aSrc, 'FControlAbsolutePosAtMouseDown := TpointF.zero;','FControlAbsolutePosAtMouseDown := TALPointD.zero;');
    //--
    aSrc := FindAndReplace(aSrc, 'FForm: TCommonCustomForm;', '//FForm: TCommonCustomForm;');
    aSrc := FindAndReplace(aSrc, 'property Form: TCommonCustomForm read FForm;', '//property Form: TCommonCustomForm read FForm;');
    aSrc := FindAndReplace(aSrc, 'FForm := nil;','//FForm := nil;');
    aSrc := FindAndReplace(aSrc, 'else //FForm := nil;','else FForm := nil;');
    //--
    aSrc := FindAndReplace(aSrc, 'procedure DelayOnResize(Sender: TObject);','//procedure DelayOnResize(Sender: TObject);');
    aSrc := FindAndReplace(aSrc, 'procedure DelayOnResized(Sender: TObject);','//procedure DelayOnResized(Sender: TObject);');
    //--
    aSrc := FindAndReplace(aSrc, 'DoMouseEnter','MouseEnter');
    aSrc := FindAndReplace(aSrc, 'DoMouseLeave','MouseLeave');
    aSrc := FindAndReplace(aSrc, 'TabStop := ACustomTrack.TabStop;','//TabStop := ACustomTrack.TabStop;');
    aSrc := FindAndReplace(aSrc, 'inherited TabStop := False;','//inherited TabStop := False;');
    aSrc := FindAndReplace(aSrc, 'procedure _SetTabStop(const Value: Boolean);','//procedure _SetTabStop(const Value: Boolean); virtual;');
    aSrc := FindAndReplace(aSrc, 'FThumb.TabOrder := 0;','//FThumb.TabOrder := 0;');
    aSrc := FindAndReplace(aSrc, 'FMaxThumb.TabOrder := 1;','//FMaxThumb.TabOrder := 1;');
    aSrc := FindAndReplace(aSrc, 'procedure DoEnter; override;','//procedure DoEnter; override;');
    aSrc := FindAndReplace(aSrc, 'procedure DoExit; override;','//procedure DoExit; override;');
    aSrc := FindAndReplace(aSrc, 'CanFocus := True;', '//CanFocus := True;');
    aSrc := FindAndReplace(aSrc, 'CanFocus := False;', '//CanFocus := False;');
    aSrc := FindAndReplace(aSrc, 'CanParentFocus := True;', '//CanParentFocus := True;');
    aSrc := FindAndReplace(aSrc, 'CanFocus := ACustomTrack.CanFocus;','//CanFocus := ACustomTrack.CanFocus;');
    aSrc := FindAndReplace(aSrc, 'function _GetCanFocus: Boolean;','//function _GetCanFocus: Boolean; virtual;');
    aSrc := FindAndReplace(aSrc, 'procedure _SetCanFocus(const Value: Boolean);','//procedure _SetCanFocus(const Value: Boolean); virtual;');
    aSrc := FindAndReplace(aSrc, 'procedure IsFocusedChanged; override;','//procedure IsFocusedChanged; override;');
    aSrc := FindAndReplace(aSrc, 'else if Parent.IsFocused then Result := Checked.Focused','//else if Parent.IsFocused then Result := Checked.Focused');
    aSrc := FindAndReplace(aSrc, 'else if Parent.IsFocused then Result := UnChecked.Focused','//else if Parent.IsFocused then Result := UnChecked.Focused');
    aSrc := FindAndReplace(aSrc, 'else if Parent.IsFocused then Result := Focused','//else if Parent.IsFocused then Result := Focused');
    aSrc := FindAndReplace(aSrc, 'If AThumb.IsFocused or Athumb.IsMouseOver or AThumb.Pressed then begin','If {AThumb.IsFocused or} Athumb.IsMouseOver or AThumb.Pressed then begin');
    aSrc := FindAndReplace(aSrc, 'procedure IsFocusedChanged; virtual;','//procedure IsFocusedChanged; virtual;');
    aSrc := FindAndReplace(aSrc, 'FFocusOnMouseDown: Boolean;','//FFocusOnMouseDown: Boolean;');
    aSrc := FindAndReplace(aSrc, 'FFocusOnMouseUp: Boolean;','//FFocusOnMouseUp: Boolean;');
    aSrc := FindAndReplace(aSrc, 'FDisableDoubleClickHandling: Boolean;','//FDisableDoubleClickHandling: Boolean;');
    aSrc := FindAndReplace(aSrc, 'FDisableDoubleClickHandling := True;','//FDisableDoubleClickHandling := True;');
    aSrc := FindAndReplace(aSrc, 'if FDisableDoubleClickHandling then Shift := Shift - [ssDouble];','//if FDisableDoubleClickHandling then Shift := Shift - [ssDouble];');
    aSrc := FindAndReplace(aSrc, 'property DisableDoubleClickHandling: Boolean read FDisableDoubleClickHandling write FDisableDoubleClickHandling;','//property DisableDoubleClickHandling: Boolean read FDisableDoubleClickHandling write FDisableDoubleClickHandling;');
    aSrc := FindAndReplace(aSrc, 'FFormerMarginsChangedHandler: TNotifyEvent;','//FFormerMarginsChangedHandler: TNotifyEvent;', 'TALDynamicListBoxExtendedControl = class(TALDynamicListBoxControl)', 'end;');
    aSrc := FindAndReplace(aSrc, 'procedure MarginsChangedHandler(Sender: TObject);','//procedure MarginsChangedHandler(Sender: TObject);', 'TALDynamicListBoxExtendedControl = class(TALDynamicListBoxControl)', 'end;');
    aSrc := FindAndReplace(aSrc, 'Procedure MarginsChanged; virtual;','//Procedure MarginsChanged; virtual;', 'TALDynamicListBoxExtendedControl = class(TALDynamicListBoxControl)', 'end;');
    aSrc := FindAndReplace(aSrc, 'FFormerMarginsChangedHandler := Margins.OnChange;','//FFormerMarginsChangedHandler := Margins.OnChange;', 'constructor TALDynamicListBoxExtendedControl.Create(', 'end;');
    aSrc := FindAndReplace(aSrc, 'Margins.OnChange := MarginsChangedHandler;','//Margins.OnChange := MarginsChangedHandler;', 'constructor TALDynamicListBoxExtendedControl.Create(', 'end;');
    aSrc := FindAndReplace(aSrc, 'FAlign: TALAlignLayout;','//FAlign: TALAlignLayout;');
    aSrc := FindAndReplace(aSrc, 'FAlign := TALAlignLayout.None;','//FAlign := TALAlignLayout.None;');
    aSrc := FindAndReplace(aSrc, 'property FocusOnMouseDown: Boolean read FFocusOnMouseDown write FFocusOnMouseDown;','//property FocusOnMouseDown: Boolean read FFocusOnMouseDown write FFocusOnMouseDown;');
    aSrc := FindAndReplace(aSrc, 'FFocusOnMouseDown := False;','//FFocusOnMouseDown := False;');
    aSrc := FindAndReplace(aSrc, 'FFocusOnMouseUp := False;','//FFocusOnMouseUp := False;');
    aSrc := FindAndReplace(aSrc, 'property FocusOnMouseUp: Boolean read FFocusOnMouseUp write FFocusOnMouseUp;','//property FocusOnMouseUp: Boolean read FFocusOnMouseUp write FFocusOnMouseUp;');
    aSrc := FindAndReplace(aSrc, 'function GetAlign: TALAlignLayout; Reintroduce;','//function GetAlign: TALAlignLayout; Reintroduce;');
    aSrc := FindAndReplace(aSrc, 'procedure ParentChanged; override;','//procedure ParentChanged; override;');
    aSrc := FindAndReplace(aSrc, 'procedure MakeBufDrawable; virtual;','procedure MakeBufDrawable; override;', 'TALDynamicListBoxExtendedControl = class(TALDynamicListBoxControl)', 'end;');
    aSrc := FindAndReplace(aSrc, 'procedure ClearBufDrawable; virtual;','procedure ClearBufDrawable; override;', 'TALDynamicListBoxExtendedControl = class(TALDynamicListBoxControl)', 'end;');
    aSrc := FindAndReplace(aSrc, 'procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;','//procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;');
    aSrc := FindAndReplace(aSrc, 'procedure DoMatrixChanged(Sender: TObject); override;','//procedure DoMatrixChanged(Sender: TObject); override;');
    aSrc := FindAndReplace(aSrc, 'DoMatrixChanged(nil);','//DoMatrixChanged(nil);');
    aSrc := FindAndReplace(aSrc, 'function GetPivot: TPosition;','//function GetPivot: TPosition;');
    aSrc := FindAndReplace(aSrc, 'procedure SetPivot(const Value: TPosition);','//procedure SetPivot(const Value: TPosition);');
    aSrc := FindAndReplace(aSrc, 'property Pivot: TPosition read GetPivot write SetPivot;','//property Pivot: TPosition read GetPivot write SetPivot;');
    aSrc := FindAndReplace(aSrc, 'Pivot.Point := TPointF.Create','Pivot := TPointF.Create');
    aSrc := FindAndReplace(aSrc, 'AbsoluteRect, // Const AAbsoluteRect: TRectF;','AbsoluteRect.ReducePrecision, // Const AAbsoluteRect: TRectF;');
    aSrc := FindAndReplace(aSrc, 'TouchTargetExpansion.Rect','TouchTargetExpansion');
    aSrc := FindAndReplace(aSrc, 'FValueRange := TValueRange.create(self);','FValueRange := TValueRange.create(_ALDummyComponent);');
    aSrc := FindAndReplace(aSrc, 'function GetDoubleBuffered: boolean; virtual;','function GetDoubleBuffered: boolean; override;');
    aSrc := FindAndReplace(aSrc, 'procedure SetDoubleBuffered(const AValue: Boolean); virtual;','procedure SetDoubleBuffered(const AValue: Boolean); override;');
    aSrc := FindAndReplace(aSrc, 'procedure BeginTextUpdate; virtual;','procedure BeginTextUpdate; override;');
    aSrc := FindAndReplace(aSrc, 'procedure EndTextUpdate; virtual;','procedure EndTextUpdate; override;');
    aSrc := FindAndReplace(aSrc, 'function IsDisplayed: Boolean; virtual;','function IsDisplayed: Boolean; override;');
    aSrc := FindAndReplace(aSrc, 'function GetAbsoluteDisplayedRect: TRectF; virtual;','//function GetAbsoluteDisplayedRect: TRectF; virtual;');
    aSrc := FindAndReplace(aSrc, 'function IsReadyToDisplay: Boolean; virtual;','function IsReadyToDisplay: Boolean; override;');
    //--
    aSrc := FindAndReplace(aSrc, 'FFormerTouchTargetExpansionChangedHandler: TNotifyEvent;','//FFormerTouchTargetExpansionChangedHandler: TNotifyEvent;');
    aSrc := FindAndReplace(aSrc, 'FFormerTouchTargetExpansionChangedHandler := TouchTargetExpansion.OnChange;','//FFormerTouchTargetExpansionChangedHandler := TouchTargetExpansion.OnChange;');
    aSrc := FindAndReplace(aSrc, 'TouchTargetExpansion.OnChange := TouchTargetExpansionChanged;','//TouchTargetExpansion.OnChange := TouchTargetExpansionChanged;');
    aSrc := FindAndReplace(aSrc, 'procedure TouchTargetExpansionChanged(Sender: TObject); virtual;','procedure SetTouchTargetExpansion(const AValue: TRectF); override;');
    aSrc := FindAndReplace(
              aSrc,
              'procedure TALDynamicListBoxRangeTrackBar.TMinThumb.TouchTargetExpansionChanged(Sender: TObject);'#13#10+
              'begin'#13#10+
              '  if Assigned(FFormerTouchTargetExpansionChangedHandler) then'#13#10+
              '    FFormerTouchTargetExpansionChangedHandler(Sender);'#13#10+
              '  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;'#13#10+
              '  if LMaxThumb <> nil then'#13#10+
              '    LMaxThumb.TouchTargetExpansion.assign(TouchTargetExpansion);'#13#10+
              'end;',
              'procedure TALDynamicListBoxRangeTrackBar.TMinThumb.SetTouchTargetExpansion(const AValue: TRectF);'#13#10+
              'begin'#13#10+
              '  Inherited;'#13#10+
              '  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;'#13#10+
              '  if LMaxThumb <> nil then'#13#10+
              '    LMaxThumb.TouchTargetExpansion := TouchTargetExpansion;'#13#10+
              'end;');
    //--
    aSrc := FindAndReplace(aSrc, 'procedure SetAlign(const Value: TALAlignLayout); Reintroduce; virtual;','//procedure SetAlign(const Value: TALAlignLayout); Reintroduce; virtual;');
    aSrc := FindAndReplace(aSrc, 'property Align: TALAlignLayout read FAlign write SetAlign default TALAlignLayout.None;','//property Align: TALAlignLayout read FAlign write SetAlign default TALAlignLayout.None;');
    //--
    aSrc := FindAndReplace(aSrc, 'Locked := True;', '//Locked := True;');
    //--
    aSrc := FindAndReplace(aSrc, 'procedure SetNewScene(AScene: IScene); override;','//procedure SetNewScene(AScene: IScene); override;');
    aSrc := FindAndReplace(aSrc, '(scene <> nil) and // SetNewScene will call again AdjustSize', '//(scene <> nil) and // SetNewScene will call again AdjustSize');
    aSrc := FindAndReplace(
              aSrc,
              '  if SameText(TRadioButtonGroupMessage(M).GroupName, GroupName) and (Sender <> Self) and (Scene <> nil) and'+#13#10+
              '     (not (Sender is TControl) or ((Sender as TControl).Scene = Scene)) then begin',
              '  if SameText(TRadioButtonGroupMessage(M).GroupName, GroupName) and (Sender <> Self) and (OwnerListBox <> nil) and'+#13#10+
              '     (not (Sender is TALDynamicListBoxControl) or ((Sender as TALDynamicListBoxControl).OwnerListBox = OwnerListBox)) then begin');
    //--
    aSrc := FindAndReplace(aSrc, 'FTrack.Parent := self;','//FTrack.Parent := self;');
    aSrc := FindAndReplace(aSrc, 'FTrack.Stored := False;','//FTrack.Stored := False;');
    aSrc := FindAndReplace(aSrc, 'FTrack.SetSubComponent(True);','//FTrack.SetSubComponent(True);');
    aSrc := FindAndReplace(aSrc, 'FTrack.Name := ''Track'';','//FTrack.Name := ''Track'';');
    aSrc := FindAndReplace(aSrc, 'FThumb.Parent := FTrack;','//FThumb.Parent := FTrack;');
    aSrc := FindAndReplace(aSrc, 'FThumb.Stored := False;','//FThumb.Stored := False;');
    aSrc := FindAndReplace(aSrc, 'FThumb.SetSubComponent(True);','//FThumb.SetSubComponent(True);');
    aSrc := FindAndReplace(aSrc, 'FThumb.Name := ''Thumb'';','//FThumb.Name := ''Thumb'';');
    aSrc := FindAndReplace(aSrc, 'Result.Parent := self;','//Result.Parent := self;');
    aSrc := FindAndReplace(aSrc, 'Result.Stored := False;','//Result.Stored := False;');
    aSrc := FindAndReplace(aSrc, 'Result.SetSubComponent(True);','//Result.SetSubComponent(True);');
    //--
    aSrc := FindAndReplace(aSrc, 'SetAcceptsControls(False);', '//SetAcceptsControls(False);');
    aSrc := FindAndReplace(aSrc, 'for var I := 0 to ControlsCount - 1 do begin','for var I := Low(FControls) to High(FControls) do begin');
    aSrc := FindAndReplace(aSrc, 'function GetParentedVisible: Boolean; override;','//function GetParentedVisible: Boolean; override;');
    aSrc := FindAndReplace(aSrc, 'procedure DoRootChanged; override;','//procedure DoRootChanged; override;');
    aSrc := FindAndReplace(aSrc, 'procedure Loaded; override;', '//procedure Loaded; override;');
    aSrc := FindAndReplace(aSrc, 'Create(AOwner: TComponent)', 'Create(const AOwner: TObject)');
    aSrc := FindAndReplace(aSrc, 'procedure SetName(const Value: TComponentName); override;','//procedure SetName(const Value: TComponentName); override;');
    //--
    aSrc := FindAndReplace(aSrc, 'function HasUnconstrainedAutosizeX: Boolean; virtual;','function HasUnconstrainedAutosizeX: Boolean; override;');
    aSrc := FindAndReplace(aSrc, 'function HasUnconstrainedAutosizeY: Boolean; virtual;','function HasUnconstrainedAutosizeY: Boolean; override;');
    //--
    aSrc := FindAndReplace(aSrc, 'if not (csLoading in ComponentState) and Assigned(FOnChange) then','//if {not (csLoading in ComponentState) and} Assigned(FOnChange) then');
    aSrc := FindAndReplace(aSrc, 'If ([csLoading, csDestroying, csDesigning] * parent.ComponentState <> []) then Exit;','If parent.IsDestroying then Exit;');
    aSrc := FindAndReplace(aSrc, '(not (csLoading in ComponentState)) and','//(not (csLoading in ComponentState)) and');
    aSrc := FindAndReplace(aSrc, 'not (csDestroying in ComponentState)','not IsDestroying');
    aSrc := FindAndReplace(aSrc, 'if csLoading in ComponentState then exit;','//if csLoading in ComponentState then exit;');
    aSrc := FindAndReplace(aSrc, 'function IsOwnerLoading: Boolean;','//function IsOwnerLoading: Boolean;');
    aSrc := FindAndReplace(aSrc, 'if not IsOwnerLoading then','//if not IsOwnerLoading then');
    //--
    aSrc := FindAndReplace(
              aSrc,
              '  Procedure DoBeginTextUpdate(const AControl: TControl);'+#13#10+
              '  begin'+#13#10+
              '    for var I := 0 to AControl.ControlsCount - 1 do'+#13#10+
              '      if AControl.Controls[i] is TALDynamicListBoxControl then'+#13#10+
              '        TALDynamicListBoxControl(AControl.Controls[i]).BeginTextUpdate'+#13#10+
              '      else'+#13#10+
              '        DoBeginTextUpdate(AControl.Controls[i]);'+#13#10+
              '  end;',
              '  Procedure DoBeginTextUpdate(const AControl: TALDynamicListBoxControl);'+#13#10+
              '  begin'+#13#10+
              '    for var I := 0 to AControl.ControlsCount - 1 do'+#13#10+
              '      //if AControl.Controls[i] is TALDynamicListBoxControl then'+#13#10+
              '      //  TALDynamicListBoxControl(AControl.Controls[i]).BeginTextUpdate'+#13#10+
              '      //else'+#13#10+
              '      //  DoBeginTextUpdate(AControl.Controls[i]);'+#13#10+
              '      AControl.Controls[i].BeginTextUpdate;'+#13#10+
              '  end;');
    //--
    aSrc := FindAndReplace(
              aSrc,
              '  Procedure DoEndTextUpdate(const AControl: TControl);'+#13#10+
              '  begin'+#13#10+
              '    for var I := 0 to AControl.ControlsCount - 1 do'+#13#10+
              '      if AControl.Controls[i] is TALDynamicListBoxControl then'+#13#10+
              '        TALDynamicListBoxControl(AControl.Controls[i]).EndTextUpdate'+#13#10+
              '      else'+#13#10+
              '        DoEndTextUpdate(AControl.Controls[i]);'+#13#10+
              '  end;',
              '  Procedure DoEndTextUpdate(const AControl: TALDynamicListBoxControl);'+#13#10+
              '  begin'+#13#10+
              '    for var I := 0 to AControl.ControlsCount - 1 do'+#13#10+
              '      //if AControl.Controls[i] is TALDynamicListBoxControl then'+#13#10+
              '      //  TALDynamicListBoxControl(AControl.Controls[i]).EndTextUpdate'+#13#10+
              '      //else'+#13#10+
              '      //  DoEndTextUpdate(AControl.Controls[i]);'+#13#10+
              '      AControl.Controls[i].EndTextUpdate;'+#13#10+
              '  end;');
    //--
    aSrc := FindAndReplace(
              aSrc,
              '        var LALChildControl: TALDynamicListBoxControl;'+#13#10+
              '        var LALChildControlAlign: TALAlignLayout;'+#13#10+
              '        If (LChildControl is TALDynamicListBoxControl) then begin'+#13#10+
              '          LALChildControl := TALDynamicListBoxControl(LChildControl);'+#13#10+
              '          LALChildControlAlign := LALChildControl.Align'+#13#10+
              '        end'+#13#10+
              '        else begin'+#13#10+
              '          LALChildControl := nil;'+#13#10+
              '          LALChildControlAlign := TALAlignLayout(LChildControl.Align);'+#13#10+
              '        end;',
              '        //var LALChildControl: TALDynamicListBoxControl;'+#13#10+
              '        //var LALChildControlAlign: TALAlignLayout;'+#13#10+
              '        //If (LChildControl is TALDynamicListBoxControl) then begin'+#13#10+
              '        //  LALChildControl := TALDynamicListBoxControl(LChildControl);'+#13#10+
              '        //  LALChildControlAlign := LALChildControl.Align'+#13#10+
              '        //end'+#13#10+
              '        //else begin'+#13#10+
              '        //  LALChildControl := nil;'+#13#10+
              '        //  LALChildControlAlign := TALAlignLayout(LChildControl.Align);'+#13#10+
              '        //end;'+#13#10+
              '        var LALChildControl := LChildControl;'+#13#10+
              '        var LALChildControlAlign := LALChildControl.Align;');
    //--
    aSrc := FindAndReplace(
              aSrc,
              '  function CheckAllChildrenAreReadyToDisplay(const AControl: TControl): boolean;'+#13#10+
              '  begin'+#13#10+
              '    Result := True;'+#13#10+
              '    for var I := 0 to AControl.ControlsCount - 1 do begin'+#13#10+
              '      if AControl.Controls[i] is TALDynamicListBoxControl then Result := TALDynamicListBoxControl(AControl.Controls[i]).IsReadyToDisplay'+#13#10+
              '      else Result := CheckAllChildrenAreReadyToDisplay(AControl.Controls[i]);'+#13#10+
              '      if not Result then exit;'+#13#10+
              '    end;'+#13#10+
              '  end;',
              '  function CheckAllChildrenAreReadyToDisplay(const AControl: TALDynamicListBoxControl): boolean;'+#13#10+
              '  begin'+#13#10+
              '    Result := True;'+#13#10+
              '    for var I := 0 to AControl.ControlsCount - 1 do begin'+#13#10+
              '      //if AControl.Controls[i] is TALDynamicListBoxControl then Result := TALDynamicListBoxControl(AControl.Controls[i]).IsReadyToDisplay'+#13#10+
              '      //else Result := CheckAllChildrenAreReadyToDisplay(AControl.Controls[i]);'+#13#10+
              '      Result := AControl.Controls[i].IsReadyToDisplay;'+#13#10+
              '      if not Result then exit;'+#13#10+
              '    end;'+#13#10+
              '  end;');
    //--
    aSrc := FindAndReplace(
              aSrc,
              '    if (csDesigning in ComponentState) and FChecked then inherited SetChecked(Value) // allows check/uncheck in design-mode'+#13#10+
              '    else begin'+#13#10+
              '      if (not value) and fMandatory then exit;'+#13#10+
              '      inherited SetChecked(Value);'+#13#10+
              '      if Value then begin'+#13#10+
              '        var M := TRadioButtonGroupMessage.Create(GroupName);'+#13#10+
              '        TMessageManager.DefaultManager.SendMessage(Self, M, True);'+#13#10+
              '      end;'+#13#10+
              '    end;',
              '    //if (csDesigning in ComponentState) and FChecked then inherited SetChecked(Value) // allows check/uncheck in design-mode'+#13#10+
              '    //else begin'+#13#10+
              '      if (not value) and fMandatory then exit;'+#13#10+
              '      inherited SetChecked(Value);'+#13#10+
              '      if Value then begin'+#13#10+
              '        var M := TRadioButtonGroupMessage.Create(GroupName);'+#13#10+
              '        TMessageManager.DefaultManager.SendMessage(Self, M, True);'+#13#10+
              '      end;'+#13#10+
              '    //end;');
    aSrc := FindAndReplace(
              aSrc,
              '  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then'+#13#10+
              '  begin'+#13#10+
              '    var R := LocalRect;'+#13#10+
              '    system.types.InflateRect(R, -0.5, -0.5);'+#13#10+
              '    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);'+#13#10+
              '  end;',
              '  //if (csDesigning in ComponentState) and not Locked and not FInPaintTo then'+#13#10+
              '  //begin'+#13#10+
              '  //  var R := LocalRect;'+#13#10+
              '  //  system.types.InflateRect(R, -0.5, -0.5);'+#13#10+
              '  //  Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);'+#13#10+
              '  //end;');
    aSrc := FindAndReplace(
              aSrc,
              '  if (csDesigning in ComponentState) and not Locked then'+#13#10+
              '    DrawDesignBorder;',
              '  //if (csDesigning in ComponentState) and not Locked then'+#13#10+
              '  //  DrawDesignBorder;');
    aSrc := FindAndReplace(
              aSrc,
              '        if (csDesigning in ComponentState) and (LChildControl.ClassName = ''TGrabHandle.TGrabHandleRectangle'') then'+#13#10+
              '          continue;',
              '        //if (csDesigning in ComponentState) and (LChildControl.ClassName = ''TGrabHandle.TGrabHandleRectangle'') then'+#13#10+
              '        //  continue;');
    aSrc := FindAndReplace(
              aSrc,
              '    if not (csLoading in ComponentState) then begin'+#13#10+
              '      BeginUpdate;'+#13#10+
              '      Try'+#13#10+
              '        SetBounds(Left, Top, Height, Width);'+#13#10+
              '        Margins.Rect := SwapTopBottomWithLeftRight(Margins.Rect);'+#13#10+
              '        Padding.Rect := SwapTopBottomWithLeftRight(Padding.Rect);'+#13#10+
              '        if FActiveTrack <> nil then begin'+#13#10+
              '          FActiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FActiveTrack.Margins.Rect);'+#13#10+
              '          FActiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FActiveTrack.Padding.Rect);'+#13#10+
              '          FActiveTrack.Align := SwapAlign(FActiveTrack.Align);'+#13#10+
              '        end;'+#13#10+
              '        if FInactiveTrack <> nil then begin'+#13#10+
              '          FInactiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FInactiveTrack.Margins.Rect);'+#13#10+
              '          FInactiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FInactiveTrack.Padding.Rect);'+#13#10+
              '          FInactiveTrack.Align := SwapAlign(FInactiveTrack.Align);'+#13#10+
              '        end;'+#13#10+
              '        if FThumb <> nil then begin'+#13#10+
              '          FThumb.Margins.Rect := SwapTopBottomWithLeftRight(FThumb.Margins.Rect);'+#13#10+
              '          FThumb.padding.Rect := SwapTopBottomWithLeftRight(FThumb.padding.Rect);'+#13#10+
              '          FThumb.TouchTargetExpansion := SwapTopBottomWithLeftRight(FThumb.TouchTargetExpansion);'+#13#10+
              '          var LThumbWidth := FThumb.Width;'+#13#10+
              '          FThumb.Width := FThumb.Height;'+#13#10+
              '          FThumb.Height := LThumbWidth;'+#13#10+
              '          FThumb.Align := SwapAlign(FThumb.Align);'+#13#10+
              '        end;'+#13#10+
              '      Finally'+#13#10+
              '        EndUpdate;'+#13#10+
              '      End;'+#13#10+
              '    end'+#13#10+
              '    else begin'+#13#10+
              '      if FActiveTrack <> nil then'+#13#10+
              '        FActiveTrack.Align := SwapAlign(FActiveTrack.Align);'+#13#10+
              '      if FInactiveTrack <> nil then'+#13#10+
              '        FInactiveTrack.Align := SwapAlign(FInactiveTrack.Align);'+#13#10+
              '      if FThumb <> nil then'+#13#10+
              '        FThumb.Align := SwapAlign(FThumb.Align);'+#13#10+
              '    end;',
              '    //if not (csLoading in ComponentState) then begin'+#13#10+
              '      BeginUpdate;'+#13#10+
              '      Try'+#13#10+
              '        SetBounds(Left, Top, Height, Width);'+#13#10+
              '        Margins.Rect := SwapTopBottomWithLeftRight(Margins.Rect);'+#13#10+
              '        Padding.Rect := SwapTopBottomWithLeftRight(Padding.Rect);'+#13#10+
              '        if FActiveTrack <> nil then begin'+#13#10+
              '          FActiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FActiveTrack.Margins.Rect);'+#13#10+
              '          FActiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FActiveTrack.Padding.Rect);'+#13#10+
              '          FActiveTrack.Align := SwapAlign(FActiveTrack.Align);'+#13#10+
              '        end;'+#13#10+
              '        if FInactiveTrack <> nil then begin'+#13#10+
              '          FInactiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FInactiveTrack.Margins.Rect);'+#13#10+
              '          FInactiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FInactiveTrack.Padding.Rect);'+#13#10+
              '          FInactiveTrack.Align := SwapAlign(FInactiveTrack.Align);'+#13#10+
              '        end;'+#13#10+
              '        if FThumb <> nil then begin'+#13#10+
              '          FThumb.Margins.Rect := SwapTopBottomWithLeftRight(FThumb.Margins.Rect);'+#13#10+
              '          FThumb.padding.Rect := SwapTopBottomWithLeftRight(FThumb.padding.Rect);'+#13#10+
              '          FThumb.TouchTargetExpansion := SwapTopBottomWithLeftRight(FThumb.TouchTargetExpansion);'+#13#10+
              '          var LThumbWidth := FThumb.Width;'+#13#10+
              '          FThumb.Width := FThumb.Height;'+#13#10+
              '          FThumb.Height := LThumbWidth;'+#13#10+
              '          FThumb.Align := SwapAlign(FThumb.Align);'+#13#10+
              '        end;'+#13#10+
              '      Finally'+#13#10+
              '        EndUpdate;'+#13#10+
              '      End;'+#13#10+
              '    //end'+#13#10+
              '    //else begin'+#13#10+
              '    //  if FActiveTrack <> nil then'+#13#10+
              '    //    FActiveTrack.Align := SwapAlign(FActiveTrack.Align);'+#13#10+
              '    //  if FInactiveTrack <> nil then'+#13#10+
              '    //    FInactiveTrack.Align := SwapAlign(FInactiveTrack.Align);'+#13#10+
              '    //  if FThumb <> nil then'+#13#10+
              '    //    FThumb.Align := SwapAlign(FThumb.Align);'+#13#10+
              '    //end;');
     aSrc := FindAndReplace(
               aSrc,
               '    if not (csLoading in ComponentState) then begin'+#13#10+
               '      BeginUpdate;'+#13#10+
               '      Try'+#13#10+
               '        inherited;'+#13#10+
               '        if FMaxInactiveTrack <> nil then begin'+#13#10+
               '          FMaxInactiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FMaxInactiveTrack.Margins.Rect);'+#13#10+
               '          FMaxInactiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FMaxInactiveTrack.Padding.Rect);'+#13#10+
               '          FMaxInactiveTrack.Align := SwapAlign(FMaxInactiveTrack.Align);'+#13#10+
               '        end;'+#13#10+
               '        if FMaxThumb <> nil then begin'+#13#10+
               '          FMaxThumb.Margins.Rect := SwapTopBottomWithLeftRight(FMaxThumb.Margins.Rect);'+#13#10+
               '          FMaxThumb.padding.Rect := SwapTopBottomWithLeftRight(FMaxThumb.padding.Rect);'+#13#10+
               '          FMaxThumb.TouchTargetExpansion := SwapTopBottomWithLeftRight(FMaxThumb.TouchTargetExpansion);'+#13#10+
               '          var LMaxThumbWidth := FMaxThumb.Width;'+#13#10+
               '          FMaxThumb.Width := FMaxThumb.Height;'+#13#10+
               '          FMaxThumb.Height := LMaxThumbWidth;'+#13#10+
               '          FMaxThumb.Align := SwapAlign(FMaxThumb.Align);'+#13#10+
               '        end;'+#13#10+
               '      Finally'+#13#10+
               '        EndUpdate;'+#13#10+
               '      End;'+#13#10+
               '    end'+#13#10+
               '    else begin'+#13#10+
               '      inherited;'+#13#10+
               '      if FMaxInactiveTrack <> nil then'+#13#10+
               '        FMaxInactiveTrack.Align := SwapAlign(FMaxInactiveTrack.Align);'+#13#10+
               '      if FMaxThumb <> nil then'+#13#10+
               '        FMaxThumb.Align := SwapAlign(FMaxThumb.Align);'+#13#10+
               '    end;',
               '    //if not (csLoading in ComponentState) then begin'+#13#10+
               '      BeginUpdate;'+#13#10+
               '      Try'+#13#10+
               '        inherited;'+#13#10+
               '        if FMaxInactiveTrack <> nil then begin'+#13#10+
               '          FMaxInactiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FMaxInactiveTrack.Margins.Rect);'+#13#10+
               '          FMaxInactiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FMaxInactiveTrack.Padding.Rect);'+#13#10+
               '          FMaxInactiveTrack.Align := SwapAlign(FMaxInactiveTrack.Align);'+#13#10+
               '        end;'+#13#10+
               '        if FMaxThumb <> nil then begin'+#13#10+
               '          FMaxThumb.Margins.Rect := SwapTopBottomWithLeftRight(FMaxThumb.Margins.Rect);'+#13#10+
               '          FMaxThumb.padding.Rect := SwapTopBottomWithLeftRight(FMaxThumb.padding.Rect);'+#13#10+
               '          FMaxThumb.TouchTargetExpansion := SwapTopBottomWithLeftRight(FMaxThumb.TouchTargetExpansion);'+#13#10+
               '          var LMaxThumbWidth := FMaxThumb.Width;'+#13#10+
               '          FMaxThumb.Width := FMaxThumb.Height;'+#13#10+
               '          FMaxThumb.Height := LMaxThumbWidth;'+#13#10+
               '          FMaxThumb.Align := SwapAlign(FMaxThumb.Align);'+#13#10+
               '        end;'+#13#10+
               '      Finally'+#13#10+
               '        EndUpdate;'+#13#10+
               '      End;'+#13#10+
               '    //end'+#13#10+
               '    //else begin'+#13#10+
               '    //  inherited;'+#13#10+
               '    //  if FMaxInactiveTrack <> nil then'+#13#10+
               '    //    FMaxInactiveTrack.Align := SwapAlign(FMaxInactiveTrack.Align);'+#13#10+
               '    //  if FMaxThumb <> nil then'+#13#10+
               '    //    FMaxThumb.Align := SwapAlign(FMaxThumb.Align);'+#13#10+
               '    //end;');
    aSrc := FindAndReplace(
              aSrc,
              '    var LOldOnResize := OnResize;'+#13#10+
              '    var LOldOnResized := OnResized;'+#13#10+
              '    if CSLoading in componentState then begin'+#13#10+
              '      OnResize := DelayOnResize;'+#13#10+
              '      OnResized := DelayOnResized;'+#13#10+
              '    end;'+#13#10+
              '    try',
              '    //var LOldOnResize := OnResize;'+#13#10+
              '    //var LOldOnResized := OnResized;'+#13#10+
              '    //if CSLoading in componentState then begin'+#13#10+
              '    //  OnResize := DelayOnResize;'+#13#10+
              '    //  OnResized := DelayOnResized;'+#13#10+
              '    //end;'+#13#10+
              '    //try');
    aSrc := FindAndReplace(
              aSrc,
              '    finally'+#13#10+
              '      OnResize := LOldOnResize;'+#13#10+
              '      OnResized := LOldOnResized;'+#13#10+
              '    end;',
              '    //finally'+#13#10+
              '    //  OnResize := LOldOnResize;'+#13#10+
              '    //  OnResized := LOldOnResized;'+#13#10+
              '    //end;');
    aSrc := FindAndReplace(
              aSrc,
              '  var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);'+#13#10+
              '  if (FFocusOnMouseUp) and'+#13#10+
              '     (FMouseDownAtLowVelocity) and'+#13#10+
              '     (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) <= TALScrollEngine.DefaultTouchSlop) and'+#13#10+
              '     (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) <= TALScrollEngine.DefaultTouchSlop) and'+#13#10+
              '     (not (csDesigning in ComponentState)) and'+#13#10+
              '     (not FIsFocused) then'+#13#10+
              '    SetFocus;',
              '  //var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);'+#13#10+
              '  //if (FFocusOnMouseUp) and'+#13#10+
              '  //   (FMouseDownAtLowVelocity) and'+#13#10+
              '  //   (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) <= TALScrollEngine.DefaultTouchSlop) and'+#13#10+
              '  //   (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) <= TALScrollEngine.DefaultTouchSlop) and'+#13#10+
              '  //   (not (csDesigning in ComponentState)) and'+#13#10+
              '  //   (not FIsFocused) then'+#13#10+
              '  //  SetFocus;');

    aSrc := FindAndReplace(
              aSrc,
              '    if not (csLoading in ComponentState) then begin'+#13#10+
              '      if FInternalState <> vpsIdle then begin'+#13#10+
              '        var LVideoPlayerEngine: TALBaseVideoPlayer := TALAsyncVideoPlayer.create;'+#13#10+
              '        LVideoPlayerEngine.Looping := fVideoPlayerEngine.Looping;'+#13#10+
              '        LVideoPlayerEngine.PlaybackSpeed := fVideoPlayerEngine.PlaybackSpeed;'+#13#10+
              '        LVideoPlayerEngine.Volume := fVideoPlayerEngine.Volume;'+#13#10+
              '        LVideoPlayerEngine.OnError := fVideoPlayerEngine.OnError;'+#13#10+
              '        LVideoPlayerEngine.OnPrepared := fVideoPlayerEngine.OnPrepared;'+#13#10+
              '        LVideoPlayerEngine.OnCompletion := fVideoPlayerEngine.OnCompletion;'+#13#10+
              '        LVideoPlayerEngine.OnVideoSizeChanged := fVideoPlayerEngine.OnVideoSizeChanged;'+#13#10+
              '        LVideoPlayerEngine.OnFrameAvailable := DoOnFrameAvailable;'+#13#10+
              '        //--'+#13#10+
              '        ALFreeAndNil(fVideoPlayerEngine);'+#13#10+
              '        fVideoPlayerEngine := LVideoPlayerEngine;'+#13#10+
              '      end;'+#13#10+
              '      if FDataSource <> '''' then begin'+#13#10+
              '        FVideoPlayerEngine.Prepare(FDataSource);'+#13#10+
              '        if AutoStartMode = TAutoStartMode.WhenPrepared then'+#13#10+
              '          FVideoPlayerEngine.Start;'+#13#10+
              '      end;'+#13#10+
              '    end;',
              '    //if not (csLoading in ComponentState) then begin'+#13#10+
              '      if FInternalState <> vpsIdle then begin'+#13#10+
              '        var LVideoPlayerEngine: TALBaseVideoPlayer := TALAsyncVideoPlayer.create;'+#13#10+
              '        LVideoPlayerEngine.Looping := fVideoPlayerEngine.Looping;'+#13#10+
              '        LVideoPlayerEngine.PlaybackSpeed := fVideoPlayerEngine.PlaybackSpeed;'+#13#10+
              '        LVideoPlayerEngine.Volume := fVideoPlayerEngine.Volume;'+#13#10+
              '        LVideoPlayerEngine.OnError := fVideoPlayerEngine.OnError;'+#13#10+
              '        LVideoPlayerEngine.OnPrepared := fVideoPlayerEngine.OnPrepared;'+#13#10+
              '        LVideoPlayerEngine.OnCompletion := fVideoPlayerEngine.OnCompletion;'+#13#10+
              '        LVideoPlayerEngine.OnVideoSizeChanged := fVideoPlayerEngine.OnVideoSizeChanged;'+#13#10+
              '        LVideoPlayerEngine.OnFrameAvailable := DoOnFrameAvailable;'+#13#10+
              '        //--'+#13#10+
              '        ALFreeAndNil(fVideoPlayerEngine);'+#13#10+
              '        fVideoPlayerEngine := LVideoPlayerEngine;'+#13#10+
              '      end;'+#13#10+
              '      if FDataSource <> '''' then begin'+#13#10+
              '        FVideoPlayerEngine.Prepare(FDataSource);'+#13#10+
              '        if AutoStartMode = TAutoStartMode.WhenPrepared then'+#13#10+
              '          FVideoPlayerEngine.Start;'+#13#10+
              '      end;'+#13#10+
              '    //end;');
    //--
    aSrc := FindAndReplace(aSrc, '(LocalRect)','(LocalRect.ReducePrecision)');
    aSrc := FindAndReplace(aSrc, '(LocalRect,','(LocalRect.ReducePrecision,');
    aSrc := FindAndReplace(aSrc, ' LocalRect,',' LocalRect.ReducePrecision,');
    aSrc := FindAndReplace(aSrc, ' LocalRect;',' LocalRect.ReducePrecision;');
    aSrc := FindAndReplace(aSrc, '.LocalRect;','.LocalRect.ReducePrecision;');
    aSrc := FindAndReplace(aSrc, ' LocalRect'#13#10, 'LocalRect.ReducePrecision'#13#10);
    //--
    var LSrcLst := TALStringListA.Create;
    Try
      LSrcLst.Text := aSrc;
      var i := 0;
      While I <= LSrcLst.Count - 1 do begin
        var LSrcLine := LSrcLst[i];
        //--
        if (alposIgnoreCaseA('.Loaded;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.DoEnter;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.DoExit;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('TALDynamicListBoxExtendedControl.IsSizeStored: Boolean;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.DoRootChanged;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.SetNewScene(AScene: IScene);', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.IsOwnerLoading: Boolean;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('TALDynamicListBoxExtendedControl.SetAlign(const Value: TALAlignLayout);', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.GetParentedVisible: Boolean;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.GetAlign: TALAlignLayout;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.IsFocusedChanged;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('TALDynamicListBoxExtendedControl.MarginsChangedHandler(Sender: TObject);', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('TALDynamicListBoxExtendedControl.MarginsChanged;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.ParentChanged;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('._GetCanFocus: Boolean;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('._SetCanFocus(const Value: Boolean);', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('._SetTabStop(const Value: Boolean);', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.GetPivot: TPosition;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.SetPivot(const Value: TPosition);', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.DoMatrixChanged(Sender: TObject);', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.GetAbsoluteDisplayedRect: TRectF;', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.SetName(const Value: TComponentName);', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.DelayOnResize(Sender: TObject);', ALTrim(LSrcLine)) > 0) or
           (alposIgnoreCaseA('.DelayOnResized(Sender: TObject);', ALTrim(LSrcLine)) > 0) then begin
          While I <= LSrcLst.Count - 1 do begin
            LSrcLine := LSrcLst[i];
            LSrcLst[i] := '//' + LSrcLst[i];
            if ALSameTextA(LSrcLine, 'end;') then Break;
            inc(i);
          end;
        end
        //--
        else if ALSameTextA(ALTrim(LSrcLine), 'If (integer(FAlign) >= integer(TALAlignLayout.TopCenter)) and') then begin
          var P1 := ALPosA('If (integer(FAlign) >= integer(TALAlignLayout.TopCenter)) and', LSrcLine);
          var LEndStr: ansiString := '';
          for var J := 1 to P1 - 1 do LEndStr := LEndStr + ' ';
          LEndStr := LEndStr + 'end;';
          While I <= LSrcLst.Count - 1 do begin
            LSrcLine := LSrcLst[i];
            LSrcLst[i] := AlCopyStr(LSrcLine, 1, P1-1) + '//' + AlCopyStr(LSrcLine, P1, MAxint);
            if ALSameTextA(LSrcLine, LEndStr) then Break;
            inc(i);
          end;
        end
        //--
        else if ALSameTextA(ALTrim(LSrcLine), 'if (not FFocusOnMouseDown) or (FFocusOnMouseUp) or (not FMouseDownAtLowVelocity) then begin') then begin
          var P1 := ALPosA('if (not FFocusOnMouseDown) or (FFocusOnMouseUp) or (not FMouseDownAtLowVelocity) then begin', LSrcLine);
          While I <= LSrcLst.Count - 1 do begin
            LSrcLine := LSrcLst[i];
            LSrcLst[i] := AlCopyStr(LSrcLine, 1, P1-1) + '//' + AlCopyStr(LSrcLine, P1, MAxint);
            if ALSameTextA(ALTrim(LSrcLine), 'else') then Break;
            inc(i);
          end;
        end
        //--
        else if ALSameTextA(ALTrim(LSrcLine), '{$IF defined(ALBackwardCompatible)}') then begin
          var P1 := ALPosA('{$IF defined(ALBackwardCompatible)}', LSrcLine);
          While I <= LSrcLst.Count - 1 do begin
            LSrcLine := LSrcLst[i];
            LSrcLst[i] := AlCopyStr(LSrcLine, 1, P1-1) + '//' + AlCopyStr(LSrcLine, P1, MAxint);
            if ALSameTextA(ALTrim(LSrcLine), '{$ENDIF}') then Break;
            inc(i);
          end;
        end;
        inc(i);
      end;
      aSrc := ALTrim(LSrcLst.Text);
    Finally
      ALFreeAndNil(LSrcLst);
    End;
    //--
    aSrc := AddDefaultBoundsClass(aSrc, 'TALDynamicListBoxSwitch.TThumb', 'Margins', '4,4,4,4');
    aSrc := AddDefaultBoundsClass(aSrc, 'TALDynamicListBoxButton', 'Padding', '12{Left}, 6{Top}, 12{Right}, 6{Bottom}');
    aSrc := AddDefaultBoundsClass(aSrc, 'TALDynamicListBoxCustomTrack.TValueIndicator', 'Margins', '6{Left}, 4{Top}, 6{Right}, 4{Bottom}');
    aSrc := AddDefaultBoundsClass(aSrc, 'TALDynamicListBoxCustomTrack.TValueIndicator', 'Padding', '16{Left}, 12{Top}, 16{Right}, 12{Bottom}');
    //--
    aSrc := FindAndReplace(aSrc, 'var LHalfHeight := GetDefaultSize.Height / 2;','//var LHalfHeight := GetDefaultSize.Height / 2;');
    aSrc := FindAndReplace(aSrc, 'var LMarginsChange := Result.Margins.OnChange;','//var LMarginsChange := Result.Margins.OnChange;');
    aSrc := FindAndReplace(aSrc, 'Result.Margins.OnChange := nil;','//Result.Margins.OnChange := nil;');
    aSrc := FindAndReplace(aSrc, 'Result.Margins.DefaultValue := TrectF.Create(0,LHalfHeight-1,0,LHalfHeight-1);','//Result.Margins.DefaultValue := TrectF.Create(0,LHalfHeight-1,0,LHalfHeight-1);');
    aSrc := FindAndReplace(aSrc, 'Result.Margins.Rect := Result.Margins.DefaultValue;','//Result.Margins.Rect := Result.Margins.DefaultValue;');
    aSrc := FindAndReplace(aSrc, 'Result.Margins.OnChange := LMarginsChange;','//Result.Margins.OnChange := LMarginsChange;');
    aSrc := AddDefaultBoundsClass(aSrc, 'TALDynamicListBoxCustomTrack.TTrack', 'Margins', '0{Left}, 15{Top}, 0{Right}, 15{Bottom}');

  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExtractClassInterface(Const aSrc: ansiString; Const AClassName: AnsiString): AnsiString;
  begin

    //  TALShape = class(TALControl)
    //  strict private
    //    FFill: TALBrush;
    //    FStroke: TALStrokeBrush;
    //    fShadow: TALShadow;
    //    function GetFill: TALBrush;
    //    procedure SetFill(const Value: TALBrush);
    //    function GetStroke: TALStrokeBrush;
    //    procedure SetStroke(const Value: TALStrokeBrush);
    //    function GetShadow: TALShadow;
    //    procedure SetShadow(const Value: TALShadow);
    //  protected
    //    procedure FillChanged(Sender: TObject); virtual;
    //    procedure StrokeChanged(Sender: TObject); virtual;
    //    procedure ShadowChanged(Sender: TObject); virtual;
    //  public
    //    constructor Create(AOwner: TComponent); override;
    //    destructor Destroy; override;
    //    procedure AlignToPixel; override;
    //    property Fill: TALBrush read GetFill write SetFill;
    //    property Stroke: TALStrokeBrush read GetStroke write SetStroke;
    //    property Shadow: TALShadow read GetShadow write SetShadow;
    //  end;

    Var P1 := AlposIgnoreCaseA('  '+AClassName+' = class(', aSrc); // TALShape = class(TALControl)
    If P1 <= low(aSrc) then
      raise Exception.Create('Error BEDEF98F-7167-459E-B26C-1F30FC4B43DC');
    inc(P1,2); //  TALShape = class(TALControl)
               //  ^P1
    var LIndent: AnsiString := '';
    While (P1 > low(aSrc)) and (not (aSrc[P1-1] in [#13, #10])) do begin
      LIndent := aSrc[P1-1] + LIndent;
      dec(P1); //  TALShape = class(TALControl)
               //^P1
    end;
    If P1 <= low(aSrc) then raise Exception.Create('Error BEDEF98F-7167-459E-B26C-1F30FC4B43DC');
    var P2 := AlposIgnoreCaseA(#13#10+LIndent+'end;', aSrc, P1); // end;
    If P2 <= 0 then raise Exception.Create('Error 4514C0C2-F078-4239-A711-408DA0C10BEC');
    inc(P2, length(#13#10+LIndent+'end;'));
    Result := '{~~~~}' +#13#10+ AlCopyStr(aSrc, P1, P2 - P1);

  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExtractClassImplementation(Const aSrc: ansiString; Const AClassName: AnsiString): AnsiString;
  begin

    //{*********************************}
    //{$IF defined(ALBackwardCompatible)}
    //constructor TALShape.Create(AOwner: TComponent);
    //begin
    //  inherited;
    //  FFill := TALBrush.Create($FFE0E0E0);
    //  FFill.OnChanged := FillChanged;
    //  FStroke := TALStrokeBrush.Create($FF000000);
    //  FStroke.OnChanged := StrokeChanged;
    //  fShadow := TALShadow.Create;
    //  fShadow.OnChanged := ShadowChanged;
    //end;
    //{$ENDIF}

    Result := '';
    Var P1 := 0;
    While true do begin
      P1 := AlposIgnoreCaseA(' '+AClassName+'.', aSrc, P1+1); //constructor TALShape.Create(AOwner: TComponent);
      If P1 <= low(aSrc) then break;                          //           ^P1
      //--
      var P0 := P1;
      While (P0 > low(aSrc)) and (not (aSrc[P0] in [#13, #10])) do dec(P0);
      If P0 <= low(aSrc) then raise Exception.Create('Error 0B02D0D7-D561-4C76-B3BB-630BB644D36F');
      inc(P0); //constructor TALShape.Create(AOwner: TComponent);
               //^P0        ^P1
      var LMethodspecifier := ALTrim(ALcopyStr(aSrc, P0, P1-P0)); //constructor
      If (not ALSameTextA(LMethodspecifier, 'procedure')) and
         (not ALSameTextA(LMethodspecifier, 'function')) and
         (not ALSameTextA(LMethodspecifier, 'class procedure')) and
         (not ALSameTextA(LMethodspecifier, 'class function')) and
         (not ALSameTextA(LMethodspecifier, 'constructor')) and
         (not ALSameTextA(LMethodspecifier, 'destructor')) then
        continue;
      P1 := P0; //constructor TALShape.Create(AOwner: TComponent);
                //^P1
      While (P1 > low(aSrc)) and (AlPosA(#13#10#13#10, aSrc, P1) <> P1) do dec(P1);
      If P1 <= low(aSrc) then raise Exception.Create('Error F6CFAE13-4FB3-4BF3-BFF1-DE626FFB1615');
      //--
      var P2 := AlposIgnoreCaseA(#13#10'end;', aSrc, P1); //#13#10end;
      If P2 <= 0 then raise Exception.Create('Error 0524A10F-F395-48DF-ABA4-65D7731FD77D');
      inc(P2, length(#13#10'end;'));
      While (P2 < High(aSrc)) and (AlPosA(#13#10#13#10, aSrc, P2) <> P2) do inc(P2);
      If P2 >= High(aSrc) then raise Exception.Create('Error 08829838-0D8C-426B-922E-55903082066E');
      Result := Result + #13#10#13#10 + ALTrim(AlCopyStr(aSrc, P1, P2 - P1));
      P1 := P2;
    end;
    Result := ALTrim(Result);

  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure ExtractClass(Const aSrc: ansiString; Const AClassName: AnsiString; var AOutputInterface, OutputImplementation: AnsiString);
  begin
    AOutputInterface := ALTrim(AOutputInterface + #13#10#13#10 + ExtractClassInterface(aSrc, AClassName));
    OutputImplementation := ALTrim(OutputImplementation + #13#10#13#10 + ExtractClassImplementation(aSrc, AClassName));
  end;

begin

  var LOutputInterface: AnsiString := '';
  var LOutputImplementation: AnsiString := '';
  //-----
  var LAlcinoeFMXControlsPas := ALGetStringFromFile(ALgetModulePathW + '\..\..\Source\Alcinoe.FMX.Controls.pas');
  LAlcinoeFMXControlsPas := FindAndReplace(LAlcinoeFMXControlsPas, 'TALControl = class(TControl)', 'TALDynamicListBoxExtendedControl = class(TALDynamicListBoxControl)');
  LAlcinoeFMXControlsPas := FindAndReplace(LAlcinoeFMXControlsPas, ' TALControl.', ' TALDynamicListBoxExtendedControl.');
  ExtractClass(LAlcinoeFMXControlsPas, 'TALDynamicListBoxExtendedControl', LOutputInterface, LOutputImplementation);
  //-----
  var LAlcinoeFMXObjectsPas := ALGetStringFromFile(ALgetModulePathW + '\..\..\Source\Alcinoe.FMX.Objects.pas');
  ExtractClass(LAlcinoeFMXObjectsPas, 'TALShape', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXObjectsPas, 'TALImage', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXObjectsPas, 'TALBaseRectangle', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXObjectsPas, 'TALRectangle', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXObjectsPas, 'TALCircle', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXObjectsPas, 'TALLine', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXObjectsPas, 'TALBaseText', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXObjectsPas, 'TALText', LOutputInterface, LOutputImplementation);
  //-----
  var LAlcinoeFMXCommonPas := ALGetStringFromFile(ALgetModulePathW + '\..\..\Source\Alcinoe.FMX.Common.pas');
  ExtractClass(LAlcinoeFMXCommonPas, 'TALBaseStateStyle', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXCommonPas, 'TALBaseStateStyles', LOutputInterface, LOutputImplementation);
  //-----
  var LAlcinoeFMXStdCtrlsPas := ALGetStringFromFile(ALgetModulePathW + '\..\..\Source\Alcinoe.FMX.StdCtrls.pas');
  //ExtractClass(LAlcinoeFMXStdCtrlsPas, 'TALAniIndicator', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXStdCtrlsPas, 'TALBaseCheckBox', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXStdCtrlsPas, 'TALCheckBox', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXStdCtrlsPas, 'TALRadioButton', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXStdCtrlsPas, 'TALSwitch', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXStdCtrlsPas, 'TALButton', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXStdCtrlsPas, 'TALCustomTrack', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXStdCtrlsPas, 'TALTrackBar', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXStdCtrlsPas, 'TALRangeTrackBar', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXStdCtrlsPas, 'TALCustomScrollBar', LOutputInterface, LOutputImplementation);
  ExtractClass(LAlcinoeFMXStdCtrlsPas, 'TALScrollBar', LOutputInterface, LOutputImplementation);
  //-----
  var LAlcinoeFMXVideoPlayerPas := ALGetStringFromFile(ALgetModulePathW + '\..\..\Source\Alcinoe.FMX.VideoPlayer.pas');
  ExtractClass(LAlcinoeFMXVideoPlayerPas, 'TALVideoPlayerSurface', LOutputInterface, LOutputImplementation);
  //-----
  var LAlcinoeFMXLayoutsPas := ALGetStringFromFile(ALgetModulePathW + '\..\..\Source\Alcinoe.FMX.Layouts.pas');
  ExtractClass(LAlcinoeFMXLayoutsPas, 'TALLayout', LOutputInterface, LOutputImplementation);
  //-----
  if (alposA(#1,LOutputInterface) > 0) or
     (alposA(#1,LOutputImplementation) > 0) then
    raise Exception.Create('Error 719C9F9F-CB9F-41A6-926A-499E86B7E8C5');
  var LTmpSrcCode :=  LOutputInterface + #1 + LOutputImplementation;
  UpdateSourceCode(LTmpSrcCode);
  var P1:= alposA(#1,LTmpSrcCode);
  if (P1 <= 0) then raise Exception.Create('Error F72CBC76-F79E-4A90-8C17-1B9D55DA5CA8');
  LOutputInterface := ALCopyStr(LTmpSrcCode, 1, P1 - 1);
  LOutputImplementation := ALCopyStr(LTmpSrcCode, P1 + 1, maxint);
  //-----
  Var LtempDir := ALgetModulePathW + '\Temp\';
  If (TDirectory.Exists(LtempDir)) and (not AlEmptyDirectoryW(LtempDir, true)) then
    Raise Exception.create('Temporary directory "' + LtempDir + '" exists but could not be cleared');
  TDirectory.CreateDirectory(LtempDir);
  ALSaveStringToFile(LOutputInterface, LtempDir + 'Interface.pas');
  ALSaveStringToFile(LOutputImplementation, LtempDir + 'implementation.pas');
  ExecuteCmdLine('"'+ALgetModulePathW+'\..\UnitNormalizer\UnitNormalizer.exe" -Dir="'+LtempDir+'" -NoInteraction=true -CreateBackup=false');
  LOutputInterface := ALGetStringFromFile(LtempDir + 'Interface.pas');
  LOutputImplementation := ALGetStringFromFile(LtempDir + 'implementation.pas');
  If (not AlEmptyDirectoryW(LtempDir, true)) then
    Raise Exception.create('Failed to clean up temporary directory "' + LtempDir + '"');
  TDirectory.Delete(LtempDir);
  //-----
  var LAlcinoeFMXDynamicListBoxPas: AnsiString := ALGetStringFromFile(ALgetModulePathW + '\..\..\Source\Alcinoe.FMX.DynamicListbox.pas');
  Var LCodeBelowSignature: AnsiString := '//////////////////////////////////////////////'#13#10+
                                         '/// THE CODE BELOW WAS AUTO-GENERATED FROM ///'#13#10+
                                         '/// <ALCINOE>\Tools\CodeBuilder.           ///'#13#10+
                                         '//////////////////////////////////////////////'#13#10;
  Var LCodeAboveSignature: AnsiString := '//////////////////////////////////////////////'#13#10+
                                         '/// THE CODE ABOVE WAS AUTO-GENERATED FROM ///'#13#10+
                                         '/// <ALCINOE>\Tools\CodeBuilder.           ///'#13#10+
                                         '//////////////////////////////////////////////'#13#10;
  P1 := ALposA(LCodeBelowSignature, LAlcinoeFMXDynamicListBoxPas);
  if P1 <= 0 then Raise Exception.Create('Error 5506BEA8-A77C-4BA5-870C-AC21A263D8C7');
  inc(P1, Length(LCodeBelowSignature));
  Var P2 := ALposA(LCodeAboveSignature, LAlcinoeFMXDynamicListBoxPas, P1);
  if P2 <= 0 then Raise Exception.Create('Error 120310CE-AD81-4891-AF5C-A7D80E9D57F3');
  Delete(LAlcinoeFMXDynamicListBoxPas,P1,P2-P1);
  Insert(
    #13#10+
    '{$REGION ''AUTO-GENERATED''}'#13#10+
    #13#10'  '+ALTrim(LOutputInterface)+#13#10+
    #13#10+
    '{$ENDREGION}'#13#10+
    #13#10,
    LAlcinoeFMXDynamicListBoxPas,
    P1);
  //--
  P1 := ALposA(LCodeBelowSignature, LAlcinoeFMXDynamicListBoxPas,P2);
  if P1 <= 0 then Raise Exception.Create('Error 5506BEA8-A77C-4BA5-870C-AC21A263D8C7');
  inc(P1, Length(LCodeBelowSignature));
  P2 := ALposA(LCodeAboveSignature, LAlcinoeFMXDynamicListBoxPas, P1);
  if P2 <= 0 then Raise Exception.Create('Error 120310CE-AD81-4891-AF5C-A7D80E9D57F3');
  Delete(LAlcinoeFMXDynamicListBoxPas,P1,P2-P1);
  Insert(
    #13#10+
    '{$REGION ''AUTO-GENERATED''}'#13#10+
    #13#10+ALTrim(LOutputImplementation)+#13#10+
    #13#10+
    '{$ENDREGION}'#13#10+
    #13#10,
    LAlcinoeFMXDynamicListBoxPas, P1);
  //--
  ALSaveStringToFile(LAlcinoeFMXDynamicListBoxPas, ALgetModulePathW + '\..\..\Source\Alcinoe.FMX.DynamicListbox.pas');

end;

begin
  try
    var LParamLst := TALStringListW.Create;
    try

      // Init LParamLst
      for var I := 1 to ParamCount do
        LParamLst.Add(ParamStr(i));

      // Init LNoInteraction
      var LNoInteraction: Boolean := AlStrToBool(ALTrim(LParamLst.Values['-NoInteraction']));

      // Build Alcinoe.FMX.DynamicListbox.pas
      BuildAlcinoeFMXDynamicControls;
      Writeln('The code for Alcinoe.FMX.DynamicListbox.pas was auto-generated successfully');

      // Finished
      if not LNoInteraction then begin
        Writeln('');
        Writeln('Build successful');
        Writeln('Press <Enter> key to quit');
        Readln;
      end;

    finally
      ALFreeandNil(LParamLst);
    end;
  except
    on E: Exception do begin
      Writeln(E.ClassName, ': ', E.Message);
      Writeln('');
      Writeln('');
      Writeln('Build failed!');
      Writeln('Press <Enter> key to quit');
      Readln;
      halt(1);
    end;
  end;
end.
