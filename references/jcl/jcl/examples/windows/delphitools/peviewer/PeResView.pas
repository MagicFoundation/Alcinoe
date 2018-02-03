{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) - Delphi Tools                                                   }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is PeResView.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit PeResView;

{$I JCL.INC}

interface

uses
  SHDocVw_TLB,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JclPeImage, PeResource, JclLogic, JclGraphUtils, ComCtrls, StdCtrls,
  ExtCtrls, Grids, ToolWin, ActnList, OleCtrls, Menus;

type
  TPeResViewChild = class(TForm)
    ResourceTreeView: TTreeView;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    DirTab: TTabSheet;
    HexDumpTab: TTabSheet;
    DirListView: TListView;
    HexDumpListView: TListView;
    StringsTab: TTabSheet;
    StringsListView: TListView;
    GraphDirTab: TTabSheet;
    GraphDrawGrid: TDrawGrid;
    TextTab: TTabSheet;
    TextRichEdit: TRichEdit;
    AviTab: TTabSheet;
    Animate1: TAnimate;
    AviToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ActionList1: TActionList;
    AviPlay1: TAction;
    AviStop1: TAction;
    HTMLTab: TTabSheet;
    GraphTab: TTabSheet;
    GraphImage: TImage;
    Bevel1: TBevel;
    GraphStatusBar: TStatusBar;
    DetailedStringMemo: TMemo;
    Splitter2: TSplitter;
    Bevel2: TBevel;
    AviStatusBar: TStatusBar;
    AviBkColor1: TAction;
    ColorDialog1: TColorDialog;
    ToolButton3: TToolButton;
    AviPopupMenu: TPopupMenu;
    Play1: TMenuItem;
    Stop1: TMenuItem;
    Color1: TMenuItem;
    DialogTab: TTabSheet;
    SaveDialog1: TSaveDialog;
    DialogTestBtn: TButton;
    Bevel3: TBevel;
    PopupMenu1: TPopupMenu;
    Copytoclipboard1: TMenuItem;
    Savetofile1: TMenuItem;
    N1: TMenuItem;
    Viewdetails1: TMenuItem;
    Viewashex1: TMenuItem;
    Selectall1: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ResourceTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure DirListViewData(Sender: TObject; Item: TListItem);
    procedure HexDumpListViewData(Sender: TObject; Item: TListItem);
    procedure StringsListViewData(Sender: TObject; Item: TListItem);
    procedure GraphDrawGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure AviPlay1Execute(Sender: TObject);
    procedure AviStop1Execute(Sender: TObject);
    procedure Animate1Stop(Sender: TObject);
    procedure StringsListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Animate1Open(Sender: TObject);
    procedure Animate1Close(Sender: TObject);
    procedure AviBkColor1Execute(Sender: TObject);
    procedure ResourceTreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure DialogTestBtnClick(Sender: TObject);
  private
    FCurrentDir: TPeResItem;
    FOriginalPageControlWndProc: TWndMethod;
    FResourceImage: TPeResImage;
    FSelectedItem: TPeResItem;
    FSelectedNode: TTreeNode;
    FShowAsHexView: Boolean;
    FStringsList: TStringList;
    FShowSpecialDirView: Boolean;
    FTempGraphic: TPicture;
    WebBrowser1: TWebBrowser;
    procedure CreateStringsList(Item: TPeResUnkStrings);
    procedure CreateGraphicList(Item: TPeResItem);
    function GetPeImage: TJclPeImage;
    procedure PageControlWndProc(var Message: TMessage);
    procedure UpdateSelected;
    procedure UpdateView;
    procedure SetShowAsHexView(const Value: Boolean);
    procedure SetShowSpecialDirView(const Value: Boolean);
  public
    constructor CreateEx(AOwner: TComponent; APeImage: TJclPeImage);
    function CanSaveResource: Boolean;
    procedure SaveResource;
    property PeImage: TJclPeImage read GetPeImage;
    property ShowAsHexView: Boolean read FShowAsHexView write SetShowAsHexView;
    property ShowSpecialDirView: Boolean read FShowSpecialDirView write SetShowSpecialDirView;
  end;

var
  PeResViewChild: TPeResViewChild;

implementation

{$R *.DFM}

uses
  CommCtrl, PeViewerMain, ToolsUtils, JclStrings, JclSysUtils;

resourcestring
  RsAviStatus = 'Width: %u, Height: %u, Frames: %u';
  RsGraphicStatus = 'Width: %u, Height: %u, Bits per pixel: %u';
  RsTitle = 'Resources - %s';

const
  MinGraphRowHeight = 18;
  MaxGraphRowHeight = 150;

{ TPeResViewChild }

constructor TPeResViewChild.CreateEx(AOwner: TComponent; APeImage: TJclPeImage);
begin
  inherited Create(AOwner);
  FShowSpecialDirView := True;
  FStringsList := TStringList.Create;
  FTempGraphic := TPicture.Create;
  FResourceImage := TPeResImage.Create;
  FResourceImage.PeImage := APeImage;
  Caption := Format(RsTitle, [ExtractFileName(FResourceImage.FileName)]);
  WebBrowser1 := TWebBrowser.Create(Self);
  TWinControl(WebBrowser1).Parent := HTMLTab;
  WebBrowser1.Align := alClient;
end;

procedure TPeResViewChild.PageControlWndProc(var Message: TMessage);
begin
// remove PageControl's border
  FOriginalPageControlWndProc(Message);
  with Message do
    if (Msg = TCM_ADJUSTRECT) and (Message.WParam = 0) then
      InflateRect(PRect(LParam)^, 4, 4);
end;

procedure TPeResViewChild.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  with PageControl1 do
  begin
    for I := 0 to PageCount - 1 do Pages[I].TabVisible := False;
    FOriginalPageControlWndProc := WindowProc;
    WindowProc := PageControlWndProc;
    ActivePage := DirTab;
    Realign;
  end;
  UpdateView;
end;

procedure TPeResViewChild.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTempGraphic);
  FreeAndNil(FStringsList);
  FreeAndNil(FResourceImage);
end;

procedure TPeResViewChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Fix_ListViewBeforeClose(Self);
  Action := caFree;
end;

procedure TPeResViewChild.UpdateView;
var
  I: Integer;
begin
  with ResourceTreeView do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for I := 0 to FResourceImage.Count - 1 do
        with Items.AddObject(nil, FResourceImage[I].ResName, FResourceImage[I]) do
        begin
          ImageIndex := icoFolderShut;
          SelectedIndex := icoFolderOpen;
          HasChildren := True;
        end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

function TPeResViewChild.GetPeImage: TJclPeImage;
begin
  Result := FResourceImage.PeImage;
end;

procedure TPeResViewChild.ResourceTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  DirListView.Items.Count := 0;
  HexDumpListView.Items.Count := 0;
  StringsListView.Items.Count := 0;
  GraphDrawGrid.RowCount := 2;
  FSelectedNode := Node;
  UpdateSelected;
end;

procedure TPeResViewChild.DirListViewData(Sender: TObject;
  Item: TListItem);
begin
  with Item, FCurrentDir[Item.Index] do
  begin
    Caption := ResName;
    SubItems.Add(Format('%x', [Offset]));
    SubItems.Add(Format('%x', [Size]));
  end;
end;

procedure TPeResViewChild.HexDumpListViewData(Sender: TObject;
  Item: TListItem);
var
  DumpData: PByte;
  Address, EndAddress: Integer;
  Hex, Ascii: string;
  I: Integer;
begin
  with Item do
  begin
    DumpData := PByte(DWORD(FSelectedItem.RawData) + DWORD(Index * 16));
    Address := FSelectedItem.Offset + Index * 16;
    EndAddress := FSelectedItem.Offset + FSelectedItem.Size - 1;
    SetLength(Hex, 3 * 16);
    SetLength(Ascii, 3 * 16);
    Hex := '';
    Ascii := '';
    for I := 0 to 15 do
    begin
      Hex := Hex + Format('%.2x ', [DumpData^]);
      if DumpData^ >= 32 then
        Ascii := Ascii + Chr(DumpData^)
      else
        Ascii := Ascii + '.';
      Inc(DumpData);
      if Address + I >= EndAddress then Break;
    end;
    Item.Caption := Format('%x', [Address]);
    Item.SubItems.Add(Hex);
    Item.SubItems.Add(Ascii);
  end;
end;

procedure TPeResViewChild.SetShowAsHexView(const Value: Boolean);
begin
  if FShowAsHexView <> Value then
  begin
    FShowAsHexView := Value;
    UpdateSelected;
  end;
end;

procedure TPeResViewChild.SetShowSpecialDirView(const Value: Boolean);
begin
  if FShowSpecialDirView <> Value then
  begin
    FShowSpecialDirView := Value;
    UpdateSelected;
  end;
end;

procedure TPeResViewChild.CreateStringsList(Item: TPeResUnkStrings);
var
  I: Integer;
begin
  FStringsList.Clear;
  DetailedStringMemo.Lines.Clear;
  if not Item.IsList then
    TPeResUnkStrings(Item).FillStrings(FStringsList)
  else
    for I := 0 to Item.ItemCount - 1 do
      TPeResUnkStrings(Item[I]).FillStrings(FStringsList);
  StringsListView.Items.Count := FStringsList.Count;
  StringsListView.Invalidate;
end;

procedure TPeResViewChild.StringsListViewData(Sender: TObject; Item: TListItem);
begin
  with Item do
  begin
    Caption := Format('%u', [DWORD(FStringsList.Objects[Index])]);
    SubItems.Add(StrRemoveChars(FStringsList[Index], CharIsReturn));
  end;
end;

procedure TPeResViewChild.CreateGraphicList(Item: TPeResItem);
var
  I, J, MaxRowHeight, TotalMaxRowHeight: Integer;

  procedure CalculateHeight(Item: TPeResItem);
  var
    H: Integer;
  begin
    case Item.Kind of
      rkCursor:
        H := GetSystemMetrics(SM_CYCURSOR);
      rkIcon:
        H := GetSystemMetrics(SM_CYICON);
      rkBitmap:
        H := TPeResUnkGraphic(Item).GraphicProperties.Height;
    else
      FTempGraphic.Assign(Item);
      H := FTempGraphic.Height;
    end;
    MaxRowHeight := Max(MaxRowHeight, H);
  end;

begin
  TotalMaxRowHeight := 0;
  with GraphDrawGrid do
  begin
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
    try
      RowCount := Item.ItemCount + 1;
      RowHeights[0] := MinGraphRowHeight;
      for I := 0 to Item.ItemCount - 1 do
      begin
        MaxRowHeight := 0;
        if Item[I].IsList then
          for J := 0 to Item[I].ItemCount - 1 do
            CalculateHeight(Item[I][J])
        else
          CalculateHeight(Item[I]);
        RowHeights[I + 1] := Min(Max(MinGraphRowHeight, MaxRowHeight + 4), MaxGraphRowHeight);
        TotalMaxRowHeight := Max(TotalMaxRowHeight, MaxRowHeight);
      end;
    finally
      SendMessage(Handle, WM_SETREDRAW, 1, 0);
      Invalidate;
    end;
  end;
end;

procedure TPeResViewChild.GraphDrawGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  Text: string;
  Item: TPeResItem;
  I, W: Integer;
  DrawRect: TRect;
begin
  with GraphDrawGrid do
  begin
    if ARow = 0 then
      with Canvas do
      begin
        case ACol of
          0: Text := 'Name';
          1: Text := 'Graphic';
        end;
        Brush.Color := clBtnFace;
        Font.Color := clBtnText;
        Dec(Rect.Bottom, 2);
        Dec(Rect.Right);
        FillRect(Rect);
        TextRect(Rect, Rect.Left + 6, Rect.Top + 2, Text);
        DrawEdge(Handle, Rect, EDGE_ETCHED, BF_BOTTOMRIGHT or BF_FLAT);
        Pen.Color := Color;
        Polyline([Point(Rect.Right, Rect.Top), Point(Rect.Right, Rect.Bottom),
          Point(Rect.Left, Rect.Bottom)]);
        Inc(Rect.Bottom);
        MoveTo(Rect.Left, Rect.Bottom);
        LineTo(Rect.Right, Rect.Bottom);
        Pen.Color := clBtnFace;
        Inc(Rect.Bottom);
        MoveTo(Rect.Left, Rect.Bottom);
        LineTo(Rect.Right, Rect.Bottom);
      end else
    begin
      if (gdSelected in State) and Focused then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
        Canvas.FillRect(Rect);
        DrawFocusRect(Canvas.Handle, Rect);
      end else
      begin
        Canvas.Brush.Color := Color;
        Canvas.Font.Color := Font.Color;
        Canvas.FillRect(Rect);
      end;
      InflateRect(Rect, -1, -1);
      Item := FCurrentDir[ARow - 1];
      case ACol of
        0:Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, Item.ResName);
        1:begin
            W := 0;
            if not Item.IsList then
            begin
              FTempGraphic.Assign(Item);
              with FTempGraphic do
                SetRect(DrawRect, Rect.Left, Rect.Top, Rect.Left + Width, Rect.Top + Height);
              if not RectIncludesRect(DrawRect, Rect) then
              begin
                DrawRect.Right := Min(DrawRect.Right, Rect.Right);
                DrawRect.Bottom := Min(DrawRect.Bottom, Rect.Bottom);
                Canvas.StretchDraw(DrawRect, FTempGraphic.Graphic);
              end
              else
                Canvas.Draw(Rect.Left + 2, Rect.Top + 2, FTempGraphic.Graphic);
            end else
            for I := 0 to Item.ItemCount - 1 do
            begin
              FTempGraphic.Assign(Item[I]);
              Canvas.Draw(Rect.Left + 2 + W, Rect.Top + 2, FTempGraphic.Graphic);
              Inc(W, FTempGraphic.Width + 5);
            end;
          end;
      end;
    end;
  end;
end;

procedure TPeResViewChild.AviPlay1Execute(Sender: TObject);
begin
  with Animate1 do
    Play(1, FrameCount, 1);
  AviStop1.Enabled := True;
  AviPlay1.Enabled := False;
end;

procedure TPeResViewChild.AviStop1Execute(Sender: TObject);
begin
  Animate1.Stop;
  AviStop1.Enabled := False;
end;

procedure TPeResViewChild.Animate1Stop(Sender: TObject);
begin
  AviPlay1.Enabled := True;
  AviStop1.Enabled := False;
end;

procedure TPeResViewChild.StringsListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then DetailedStringMemo.Text := Item.SubItems[0];
end;

procedure TPeResViewChild.Animate1Open(Sender: TObject);
begin
  with Animate1 do
    AviStatusBar.Panels[0].Text := Format(RsAviStatus, [FrameWidth, FrameHeight,
      FrameCount]);
end;

procedure TPeResViewChild.Animate1Close(Sender: TObject);
begin
  AviStatusBar.Panels[0].Text := '';
end;

procedure TPeResViewChild.AviBkColor1Execute(Sender: TObject);
begin
  with ColorDialog1 do
  begin
    CustomColors.Values['ColorA'] := Format('%.6x', [ColorToRGB(clBtnFace)]);
    Color := Animate1.Color;
    if Execute then Animate1.Color := Color;
  end;
end;

procedure TPeResViewChild.UpdateSelected;

  function SpecialDirectoryView: Boolean;
  begin
    Result := True;
    case FCurrentDir.Kind of
      rkBitmap, rkCursor, rkIcon:
        begin
          CreateGraphicList(FCurrentDir);
          PageControl1.ActivePage := GraphDirTab;
        end;
      rkString:
        begin
          CreateStringsList(TPeResString(FCurrentDir));
          PageControl1.ActivePage := StringsTab;
        end;
    else
      Result := False;
    end;
  end;

  procedure DefaultDirectoryView;
  begin
    DirListView.Items.Count := FCurrentDir.ItemCount;
    DirListView.Invalidate;
    PageControl1.ActivePage := DirTab;
  end;

  function SpecialDetailView: Boolean;
  begin
    Result := True;
    case FSelectedItem.Kind of
      rkAccelerator:
        begin
          TextRichEdit.Lines.Assign(TPeResAccelerator(FSelectedItem));
          PageControl1.ActivePage := TextTab;
        end;
      rkAvi:
        begin
          Animate1.Assign(FSelectedItem);
          PageControl1.ActivePage := AviTab;
        end;
      rkBitmap, rkIcon, rkCursor:
        begin
          GraphImage.Picture.Assign(FSelectedItem);
          if GraphImage.Picture.Graphic is TBitmap then
            GraphImage.Picture.Bitmap.Transparent := True;
          with TPeResUnkGraphic(FSelectedItem).GraphicProperties do
            GraphStatusBar.Panels[0].Text := Format(RsGraphicStatus, [Width, Height, BitsPerPixel]);
          PageControl1.ActivePage := GraphTab;
        end;
      rkString:
        begin
          CreateStringsList(TPeResString(FSelectedItem));
          PageControl1.ActivePage := StringsTab;
        end;
      rkHTML:
        begin
          WebBrowser1.Navigate(TPeResHTML(FSelectedItem).ResPath);
          PageControl1.ActivePage := HTMLTab;
        end;
      rkData:
        if TPeResRCData(FSelectedItem).DataKind <> dkUnknown then
        begin
          TextRichEdit.Lines.Assign(TPeResRCData(FSelectedItem));
          PageControl1.ActivePage := TextTab;
        end else
          Result := False;
{      rkDialog:
        begin
          DialogTestBtn.Enabled := TPeResDialog(FSelectedItem).CanShowDialog;
          PageControl1.ActivePage := DialogTab;
        end;} { TODO : Check for dialog templates }
      rkMessageTable:
        begin
          CreateStringsList(TPeResUnkStrings(FSelectedItem));
          PageControl1.ActivePage := StringsTab;
        end;
      rkVersion:
        begin
          TextRichEdit.Lines.Assign(TPeResVersion(FSelectedItem));
          PageControl1.ActivePage := TextTab;
        end;
    else
      Result := False;
    end;
  end;

  procedure DefaultDetailView;
  begin
    HexDumpListView.Items.Count := (FSelectedItem.Size - 1) div 16 + 1;
    HexDumpListView.Invalidate;
    PageControl1.ActivePage := HexDumpTab;
  end;

begin
  FSelectedItem := TPeResItem(FSelectedNode.Data);
  FCurrentDir := FSelectedItem;
  if FSelectedNode.Level = 0 then
  begin
//    FCurrentDir := FSelectedItem;
    if (not FShowSpecialDirView) or (not SpecialDirectoryView) then
      DefaultDirectoryView;
  end else
  begin
    if FSelectedItem.IsList then
    begin
//      FCurrentDir := FSelectedItem;
      DefaultDirectoryView;
    end else
    begin
      if FShowAsHexView or (not SpecialDetailView) then
        DefaultDetailView;
    end;
  end;
end;

function TPeResViewChild.CanSaveResource: Boolean;
begin
  Result := Assigned(FSelectedItem) and not FSelectedItem.IsList and
    ResourceTreeView.Focused;
end;

procedure TPeResViewChild.ResourceTreeViewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  N, L: Integer;
  ListNode, ItemNode: TTreeNode;
  Item, RootItem: TPeResItem;
begin
  if Node.GetFirstChild = nil then with ResourceTreeView do
  begin
    Items.BeginUpdate;
    try
      RootItem := TPeResItem(Node.Data);
      for N := 0 to RootItem.ItemCount - 1 do
      begin
        Item := RootItem[N];
        ListNode := Items.AddChildObject(Node, Item.ResName, Item);
        if Item.IsList then
        begin
          ListNode.ImageIndex := icoFolderShut;
          ListNode.SelectedIndex := icoFolderOpen;
          for L := 0 to Item.ItemCount - 1 do
          begin
            ItemNode := Items.AddChildObject(ListNode, Item[L].ResName, Item[L]);
            ItemNode.ImageIndex := icoResItem;
            ItemNode.SelectedIndex := icoResItem;
          end;
        end else
        begin
          ListNode.ImageIndex := icoResItem;
          ListNode.SelectedIndex := icoResItem;
        end;
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TPeResViewChild.SaveResource;
var
  FileStream: TFileStream;
begin
  with SaveDialog1, (FSelectedItem as TPeResUnknown) do
  begin
    Filter := Format('*.%s files|*.%s', [FileExt, FileExt]);
    FileName := ResName + '.' + FileExt;
    if Execute then
    begin
      FileStream := TFileStream.Create(FileName, fmCreate);
      try
        SaveToStream(FileStream);
      finally
        FileStream.Free;
      end;
    end;      
  end;
end;

procedure TPeResViewChild.DialogTestBtnClick(Sender: TObject);
var
  Res: Integer;
begin
  with ResourceTreeView do
    while True do
    begin
      with TPeResDialog(FSelectedItem) do
        if CanShowDialog then
          Res := ShowDialog(Application.Handle)
        else
          Res := 1;
      if (Res = 1) and (Selected.GetNextSibling <> nil) then
      begin
        Selected := Selected.GetNextSibling;
        Selected.MakeVisible;
        ResourceTreeView.Update;
      end else
        Break;
    end;
end;

end.
