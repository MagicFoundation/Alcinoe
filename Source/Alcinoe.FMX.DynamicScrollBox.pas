unit Alcinoe.FMX.DynamicScrollBox;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.Types,
  System.UITypes,
  System.Messaging,
  System.Generics.Collections,
  FMX.layouts,
  FMX.Types,
  FMX.Controls,
  Alcinoe.FMX.Controls,
  ALcinoe.FMX.Common,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.ScrollEngine;

type

// A view implements a scrollable area (either horizontal or vertical)
//TView = Class(Tobject)
  //property Data: TALJSONNodeW read GetData write Fdata;
  //property ScrollEngine: TALScrollEngine read fScrollEngine;
  //property Items[Index: Integer]: TMFAppDynamicScrollBoxItem read GetItem;
//End;


  {*************************************}
  TALDynamicScrollBox = class(TALControl)
  public
    type
      // ------------------------------------------------
      // An Item area represents a non-scrollable section
      TItem = Class(Tobject)
        //property Data: TALJSONNodeW read GetData write Fdata;
        //property ItemIndex: Integer read FItemIndex write SetItemIndex;
        //property EphemeralLayout: TMFAppDynamicScrollBoxItemEphemeralLayout read FEphemeralLayout;
      End;
      // -------
      // TButton
      TButton = class(TALButton)
      protected
        procedure SetParent(const Value: TFmxObject); override;
        procedure HandleSizeChanged; override;
      public
        constructor Create(AOwner: TComponent); override;
      end;
  private
  protected
  public
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  FMX.Platform,
  FMX.Consts,
  FMX.Effects,
  FMX.utils,
  FMX.Ani,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Objects,
  Alcinoe.Common;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALDynamicScrollBox]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALDynamicScrollBox, 'Size');
  UnlistPublishedProperty(TALDynamicScrollBox, 'StyleName');
  UnlistPublishedProperty(TALDynamicScrollBox, 'OnTap');
  {$ENDIF}
end;

{*****************************************************************}
constructor TALDynamicScrollBox.TButton.Create(AOwner: TComponent);
begin
  inherited create(nil);
end;

{***********************************************************************}
procedure TALDynamicScrollBox.TButton.SetParent(const Value: TFmxObject);
begin
  raise Exception.Create('Setting a parent for TALDynamicScrollBox.TButton is not allowed');
end;

{******************************************************}
procedure TALDynamicScrollBox.TButton.HandleSizeChanged;
begin
  // do nothing
end;


initialization
  RegisterFmxClasses([TALDynamicScrollBox]);

end.
