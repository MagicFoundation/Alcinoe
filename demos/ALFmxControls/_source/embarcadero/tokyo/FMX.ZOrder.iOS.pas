{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.ZOrder.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Generics.Collections, iOSapi.UIKit, iOSApi.CoreGraphics, FMX.Controls, FMX.Forms;

type

  /// <summary>Helper class used to manage platform controls</summary>
  TiOSZOrderManager = class
  private
    FLinks: TDictionary<TControl, UIView>;
    function FindControlForm(const AControl: TControl; out AForm: TCommonCustomForm): Boolean;
    function GetFormView(const AForm: TCommonCustomForm): UIView; inline;
  public
    /// <summary>Add or set new link of TControl and UIView to manager</summary>
    procedure AddOrSetLink(const Control: TControl; const AView: UIView);
    /// <summary>Remove link of TControl and UIView from manager</summary>
    procedure RemoveLink(const Control: TControl);

    /// <summary>Find first parented platform control</summary>
    function FindParentControl(const ForControl: TControl; out AControl: TControl): Boolean;
    /// <summary>Find first parented UIView and return true if it exists</summary>
    function FindParentView(const ForControl: TControl): Boolean; overload;
    /// <summary>Find first parented UIView and return true if it exists</summary>
    function FindParentView(const ForControl: TControl; out AView: UIView): Boolean; overload;
    /// <summary>Checks is control is a platform control</summary>
    function FindView(const AControl: TControl; out AView: UIView): Boolean;
    /// <summary>Find first previous platform view in same superview</summary>
    function FindPreviousSiblingView(const AControl: TControl; out AView: UIView): Boolean;

    /// <summary>Convert absolute coordinates to parent view's platform coordinates</summary>
    function AbsoluteToParentView(const AControl: TControl; const Bounds: TRectF): CGRect;
    /// <summary>Fix z-order for platform control</summary>
    procedure UpdateOrder(const AControl: TControl; const View: UIView = nil);
    /// <summary>Update platform view coordinates using control coordinates</summary>
    procedure UpdateBounds(const AControl: TControl; const View: UIView = nil);
    /// <summary>Call of FixOrder and FixBounds</summary>
    procedure UpdateOrderAndBounds(const AControl: TControl; const View: UIView = nil);
  end;

implementation

uses System.Math, System.Math.Vectors, Macapi.ObjectiveC, FMX.Platform.iOS, FMX.Helpers.iOS, Macapi.Helpers, FMX.Types,
  FMX.Consts;

function TiOSZOrderManager.AbsoluteToParentView(const AControl: TControl; const Bounds: TRectF): CGRect;
var
  View: UIView;
  Form: TCommonCustomForm;
begin
  if FindParentView(AControl, View) and FindControlForm(AControl, Form) then
    Result := View.convertRect(CGRectFromRect(Bounds), GetFormView(Form))
  else
    Result := CGRectFromRect(Bounds);
end;

procedure TiOSZOrderManager.AddOrSetLink(const Control: TControl; const AView: UIView);
var
  CurrentView: UIView;
begin
  if FLinks = nil then
    FLinks := TDictionary<TControl, UIView>.Create;
  if not FLinks.ContainsKey(Control) then
    FLinks.Add(Control, AView)
  else if FLinks.TryGetValue(Control, CurrentView) then
    if CurrentView <> AView then
      FLinks.AddOrSetValue(Control, AView);
end;

procedure TiOSZOrderManager.RemoveLink(const Control: TControl);
begin
  if FLinks <> nil then
    FLinks.Remove(Control);
end;

function TiOSZOrderManager.FindParentControl(const ForControl: TControl; out AControl: TControl): Boolean;
var
  ParentTmp: TControl;
  View: UIView;
begin
  AControl := nil;
  ParentTmp := ForControl.ParentControl;
  Result := False;
  while not Result and (ParentTmp <> nil) do
  begin
    if FindView(ParentTmp, View) then
      Result := True
    else
      ParentTmp := ParentTmp.ParentControl;
  end;
  if Result then
    AControl := ParentTmp;
end;

function TiOSZOrderManager.FindControlForm(const AControl: TControl; out AForm: TCommonCustomForm): Boolean;
begin
  Result := AControl.Root is TCommonCustomForm;
  if Result then
    AForm := TCommonCustomForm(AControl.Root)
  else
    AForm := nil;
end;

function TiOSZOrderManager.GetFormView(const AForm: TCommonCustomForm): UIView;
var
  FormHandle: TiOSWindowHandle;
begin
  FormHandle := WindowHandleToPlatform(AForm.Handle);
  Result := FormHandle.View;
end;

function TiOSZOrderManager.FindParentView(const ForControl: TControl): Boolean;
var
  View: UIView;
begin
  Result := FindParentView(ForControl, View);
end;

function TiOSZOrderManager.FindParentView(const ForControl: TControl;             {[Weak]} out AView: UIView): Boolean;
var
  Form: TCommonCustomForm;
  ParentControl: TControl;
begin
  if FindParentControl(ForControl, ParentControl) then
    FindView(ParentControl, AView);

  // if native container was not found, get native container/control from Form
  if (AView = nil) and FindControlForm(ForControl, Form) then
    AView := GetFormView(Form);

  Result := AView <> nil;
end;

function TiOSZOrderManager.FindPreviousSiblingView(const AControl: TControl; out AView: UIView): Boolean;
var
  I: Integer;
  Parent: TFmxObject;
begin
  AView := nil;
  Parent := AControl.Parent;
  if Parent <> nil then
    for I := Min(AControl.Index, Parent.ChildrenCount) - 1 downto 0 do
      if (Parent.Children[I] is TControl) and FindView(TControl(Parent.Children[I]), AView) then
        Break;
  Result := AView <> nil;
end;

function TiOSZOrderManager.FindView(const AControl: TControl; out AView: UIView): Boolean;
begin
  AView := nil;
  if FLinks <> nil then
    Result := FLinks.TryGetValue(AControl, AView)
  else
    Result := False;
end;

procedure TiOSZOrderManager.UpdateBounds(const AControl: TControl; const View: UIView = nil);
var
  Bounds: TRectF;
  LView: UIView;
begin
  if View = nil then
    FindView(AControl, LView)
  else
  begin
    LView := View;
    AddOrSetLink(AControl, View);
  end;

  if LView <> nil then
  begin
    Bounds := AControl.AbsoluteRect;
    if SameValue(Bounds.Width, 0, TEpsilon.Position) or SameValue(Bounds.Height, 0, TEpsilon.Position) then
      LView.setHidden(True)
    else
    begin
      LView.setFrame(AbsoluteToParentView(AControl, Bounds));
      LView.setHidden(not AControl.ParentedVisible);
    end;
  end;
end;

procedure TiOSZOrderManager.UpdateOrder(const AControl: TControl; const View: UIView = nil);
var
  LView, ParentView, PreviousView: UIView;
begin
  if View = nil then
    FindView(AControl, LView)
  else
  begin
    LView := View;
    AddOrSetLink(AControl, View);
  end;

  if (LView <> nil) and FindParentView(AControl, ParentView) then
  begin
    if LView.superview <> (ParentView as ILocalObject).GetObjectID then
      LView.removeFromSuperview;

    if FindPreviousSiblingView(AControl, PreviousView) then
      ParentView.insertSubview(LView, PreviousView)
    else
    begin
      if LView.superview = nil then
        ParentView.addSubview(LView);
      ParentView.sendSubviewToBack(LView);
    end;
  end;
end;

procedure TiOSZOrderManager.UpdateOrderAndBounds(const AControl: TControl; const View: UIView);
begin
  UpdateOrder(AControl, View);
  UpdateBounds(AControl, View);
end;

end.
