{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclDFMUtils.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains util routines for DFM reading and writing...                                            }
{                                                                                                  }
{ Known Issues:                                                                                    }
{   This is a preview - class and functionnames might be changed                                   }
{                                                                                                  }
{ Unit owner: Uwe Schuster                                                                         }
{ Last modified: December 13, 2003                                                                 }
{                                                                                                  }
{**************************************************************************************************}

unit JclDFMUtils;

{$I jcl.inc}

interface

uses
  Windows, Classes, SysUtils, Controls, ImgList, JclDFM, Graphics, Forms,
  ComCtrls;

procedure ReadImageListFromDFMComponent(AImageList: TCustomImageList;
  ADFMComponent: TJclDFMComponent);
procedure ExtractImageListToBitmapsFromDFMComponent(ABitmapList: TList;
  ADFMComponent: TJclDFMComponent);
procedure LoadLayout(AControl: TControl; ADFMComponent: TJclDFMComponent); overload;
procedure LoadLayout(AControl: TControl; AStream: TStream); overload;
procedure LoadLayout(AControl: TControl; AFileName: string); overload;
procedure ReadTreeViewItemsFromDFMComponent(ATreeView: TCustomTreeView;
  ADFMComponent: TJclDFMComponent);

implementation

type
  TJclDFMImagelist = class(TCustomImageList);
  TJclDFMTreeView = class(TCustomTreeView);
  TJclDFMTreeNodes = class(TTreeNodes);

procedure ReadImageListFromDFMComponent(AImageList: TCustomImageList;
  ADFMComponent: TJclDFMComponent);
var
  DFMFiler: TJclDFMFiler;
  FilerStream, BitmapStream: TMemoryStream;
  I: Integer;
  BitmapReadProc: TStreamProc;
begin
  if Assigned(AImageList) and Assigned(ADFMComponent) then
  begin
    BitmapStream := nil;
    for I := 0 to ADFMComponent.Properties.Count - 1 do
      if (ADFMComponent.Properties[I].Typ = vaBinary) and
        SameText(ADFMComponent.Properties[I].Name, 'Bitmap') then
      begin
        BitmapStream := ADFMComponent.Properties[I].AsStream;
        Break;
      end;
    if Assigned(BitmapStream) then
    begin
      FilerStream := TMemoryStream.Create;
      try
        DFMFiler := TJclDFMFiler.Create(FilerStream, 0);
        try
          TJclDFMImagelist(AImageList).DefineProperties(DFMFiler);
          if BitmapStream.Size > 0 then
          begin
            BitmapStream.Position := 0;
            BitmapReadProc := DFMFiler.GetBinaryReadProcByName('Bitmap');
            if Assigned(BitmapReadProc) then
              BitmapReadProc(BitmapStream);
          end;
        finally
          DFMFiler.Free;
        end;
      finally
        FilerStream.free;
      end;
    end;
  end;
end;

procedure ExtractImageListToBitmapsFromDFMComponent(ABitmapList: TList;
  ADFMComponent: TJclDFMComponent);
var
  ImageList: TImageList;
  I: Integer;
  Bitmap: TBitmap;
begin
  if Assigned(ABitmapList) and Assigned(ADFMComponent) then
  begin
    ImageList := TImageList.Create(nil);
    try
      ReadImageListFromDFMComponent(ImageList, ADFMComponent);
      for I := 0 to ImageList.Count - 1 do
      begin
        Bitmap := TBitmap.Create;
        Bitmap.Width := ImageList.Width;
        Bitmap.Height := ImageList.Height;
        ImageList.Draw(Bitmap.Canvas, 0, 0, I);
        ABitmapList.Add(Bitmap);
      end;
    finally
      ImageList.Free;
    end;
  end;
end;

procedure ControlsToList(AList: TList; AControl: TControl);
var
  I: Integer;
begin
  if Assigned(AList) and Assigned(AControl) then
  begin
    if AList.IndexOf(AControl) = -1 then
      AList.Add(AControl);
    if (AControl is TWinControl) {and
      (csAcceptsControls in AControl.ControlStyle)} then
      for I := 0 to TWinControl(AControl).ControlCount - 1 do
        ControlsToList(AList, TWinControl(AControl).Controls[I]);
  end;
end;

procedure LoadControlFromDFMComponent(ADFMComponent: TJclDFMComponent;
  AControl: TControl);
var
  ObjectStream: TMemoryStream;
begin
  ObjectStream := TMemoryStream.Create;
  try
    ADFMComponent.GetObjectBinary(ObjectStream, False);
    ObjectStream.Position := 0;
    ObjectStream.ReadComponent(AControl);
  finally
    ObjectStream.Free;
  end;
end;

procedure LoadControlsFromDFMComponent(ADFMComponent: TJclDFMComponent;
  AParentControl: TControl; AControlList: TList);
var
  I: Integer;
  CControl: TControl;
begin
  CControl := nil;
  for I := 0 to AControlList.Count - 1 do
    if TControl(AControlList[I]).Name = ADFMComponent.ComponentName then
    begin
      CControl := AControlList[I];
      Break;
    end;
  if Assigned(CControl) then
  begin
    if AParentControl is TWinControl then
      CControl.Parent := TWinControl(AParentControl);
    CControl.Visible := True; 
    LoadControlFromDFMComponent(ADFMComponent, CControl);
    for I := 0 to ADFMComponent.SubComponents.Count - 1 do
      LoadControlsFromDFMComponent(ADFMComponent.SubComponents[I], CControl,
        AControlList);
  end;
end;

procedure LoadLayout(AControl: TControl; ADFMComponent: TJclDFMComponent); overload;
var
  ControlList: TList;
  ControlComponent: TJclDFMComponent;
  I: Integer;
begin
  if Assigned(AControl) and Assigned(ADFMComponent) then
  begin
    ControlComponent := ADFMComponent.FindComponent(AControl.Name);
    if Assigned(ControlComponent) then
    begin
      ControlList := TList.Create;
      try
        ControlsToList(ControlList, AControl);
        for I := 0 to ControlList.Count - 1 do
          if ControlList[I] <> AControl then
            with TControl(ControlList[I]) do
            begin
              if AControl is TWinControl then
                Parent := TWinControl(AControl);
              Visible := False;
            end;
        LoadControlsFromDFMComponent(ControlComponent, nil, ControlList);
      finally
        ControlList.Free;
      end;
    end;
  end;
end;

procedure LoadLayout(AControl: TControl; AStream: TStream); overload;
var
  RComp: TJclDFMRootComponent;
begin
  if Assigned(AControl) and Assigned(AStream) then
  begin
    RComp := TJclDFMRootComponent.Create;
    try
      RComp.LoadFromStream(AStream);
      LoadLayout(AControl, RComp);
    finally
      RComp.Free;
    end;
  end;
end;

procedure LoadLayout(AControl: TControl; AFileName: string);
var
  RComp: TJclDFMRootComponent;
begin
  if Assigned(AControl) and FileExists(AFileName) then
  begin
    RComp := TJclDFMRootComponent.Create;
    try
      RComp.LoadFromFile(AFileName);
      LoadLayout(AControl, RComp);
    finally
      RComp.Free;
    end;
  end;
end;

procedure ReadTreeViewItemsFromDFMComponent(ATreeView: TCustomTreeView;
  ADFMComponent: TJclDFMComponent);
var
  DFMFiler: TJclDFMFiler;
  FilerStream, ItemsStream: TMemoryStream;
  I: Integer;
  ItemsReadProc: TStreamProc;
begin
  if Assigned(ATreeView) and Assigned(ADFMComponent) then
  begin
    TJclDFMTreeView(ATreeView).Items.Clear;
    ItemsStream := nil;
    for I := 0 to ADFMComponent.Properties.Count - 1 do
      if (ADFMComponent.Properties[I].Typ = vaBinary) and
        SameText(ADFMComponent.Properties[I].Name, 'Items.Data') then
      begin
        ItemsStream := ADFMComponent.Properties[I].AsStream;
        Break;
      end;
    if Assigned(ItemsStream) then
    begin
      FilerStream := TMemoryStream.Create;
      try
        DFMFiler := TJclDFMFiler.Create(FilerStream, 0);
        try
          TJclDFMTreeNodes(TJclDFMTreeView(ATreeView).Items).DefineProperties(DFMFiler);
          if ItemsStream.Size > 0 then
          begin
            ItemsStream.Position := 0;
            ItemsReadProc := DFMFiler.GetBinaryReadProcByName('Data');
            if Assigned(ItemsReadProc) then
              ItemsReadProc(ItemsStream);
          end;
        finally
          DFMFiler.Free;
        end;
      finally
        FilerStream.free;
      end;
    end;
  end;
end;

end.
