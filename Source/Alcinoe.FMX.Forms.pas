unit Alcinoe.FMX.forms;

interface

{$I Alcinoe.inc}

uses
  system.Classes,
  System.UITypes,
  Alcinoe.FMX.common,
  Alcinoe.FMX.Objects;

type

  {***********}
  /// <summary>
  ///   TALFrame is a drop-in replacement for TFrame that allows using a
  ///   custom base control (TALControl) while retaining compatibility with
  ///   the design-time experience of TFrame.
  /// </summary>
  /// <remarks>
  ///   To use TALFrame, create your frame as usual using TFrame, then replace
  ///   TFrame with TALFrame in the unit's type declaration. This enables you
  ///   to maintain visual design support while leveraging the custom behavior
  ///   of TALControl.
  /// </remarks>
  TALFrame = class(TALRectangle)
  public
    type
      TFill = class(TALBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      TStroke = class(TALStrokeBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
  private
    procedure SkipProperty(Reader: TReader);
  protected
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{***************************************************}
function TALFrame.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{*****************************************************}
function TALFrame.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{**********************************************}
constructor TALFrame.Create(AOwner: TComponent);
begin
  inherited;
  if not InitInheritedComponent(Self, TALFrame) then
    raise EResNotFound.CreateFmt('Resource %s not found', [ClassName]);
end;

{*************************************}
function TALFrame.CreateFill: TALBrush;
begin
  result := TFill.Create;
end;

{*********************************************}
function TALFrame.CreateStroke: TALStrokeBrush;
begin
  result := TStroke.Create;
end;

{*************************************************}
procedure TALFrame.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('DesignerMasterStyle', SkipProperty{ReadData}, nil{WriteData}, false{hasdata});
  Filer.DefineProperty('ClientHeight', SkipProperty{ReadData}, nil{WriteData}, false{hasdata});
  Filer.DefineProperty('ClientWidth', SkipProperty{ReadData}, nil{WriteData}, false{hasdata});
  Filer.DefineProperty('FormFactor.Width', SkipProperty{ReadData}, nil{WriteData}, false{hasdata});
  Filer.DefineProperty('FormFactor.Height', SkipProperty{ReadData}, nil{WriteData}, false{hasdata});
  Filer.DefineProperty('FormFactor.Devices', SkipProperty{ReadData}, nil{WriteData}, false{hasdata});
end;

{***********************************************}
procedure TALFrame.SkipProperty(Reader: TReader);
begin
  Reader.SkipValue;
end;

end.
