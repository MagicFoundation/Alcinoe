{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALListBox
Version:      3.50

Description:  AlListBox with onPaint property
              - Flat design (no clt3D feature)
              - Border color property property
              - Read only property
              - Mouse is down (property to know if the mouse is down)
              - Mouse in control (property to know if the mouse is
                in the control area)
              - Custom property (string to add custom informations like
                "tag" property)
              - Onpaint event (powerfull event fired each time the
                control needed to be repaint and permit to change, before
                the repaint process, some property of the control like it's
                color. For exemple we can easily change the font or border
                color of the control each time the mouse enter or leave the
                control.

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :    1. The style property only support lbStandard and
                 lbOwnerDrawFixed and lbOwnerDrawVariable

History :     23/11/2004: Add the style, OnDrawItem, itemHeight functionality
              24/11/2004: Add the OnMeasureItem event and lbOwnerDrawVariable
              24/11/2004: correct the TalKeyListBox AddItemKey to permit to
                          access the the key in the onmeasureitem event
              30/10/2005: move WM_AFTERKEYPRESS = WM_user + 1 to
                          WM_AFTERKEYPRESS = WM_user + 1000
                          and WM_ItemIndexChanged = WM_user + 2 to
                          WM_ItemIndexChanged = WM_user + 1001

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALListBox;

interface

Uses Windows,
     Messages,
     SysUtils,
     Classes,
     Graphics,
     Controls,
     Forms,
     StdCtrls,
     ExtCtrls,
     AlScrollBar,
     ALCommon;

{--------------------------------------}
Const WM_AFTERKEYPRESS = WM_user + 1000;
      WM_ItemIndexChanged = WM_user + 1001;

Type

  {---------------------}
  TALCustomListBox=class;
  TALCustomNoScrollBarListBox = Class;

  {---------------------------------}
  TALListBoxStrings = class(TStrings)
  private
    ListBox: TALCustomNoScrollBarListBox;
  protected
    procedure Changed; virtual;
    procedure Put(Index: Integer; const S: string); override;
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;

 {-----------------------------------------------------------------}
 TALCustomNoScrollBarListBox = Class (TCustomMultiSelectListControl)
  private
    FMouseMoveTimer : Ttimer;
    FOnScroll: TScrollEvent;
    FCurPos : Integer;
    FAutoComplete: Boolean;
    FCount: Integer;
    FItems: TStrings;
    FFilter: String;
    FLastTime: Cardinal;
    FBorderStyle: TBorderStyle;
    FCanvas: TCanvas;
    FColumns: Integer;
    FItemHeight: Integer;
    FOldCount: Integer;
    FStyle: TListBoxStyle;
    FIntegralHeight: Boolean;
    FSorted: Boolean;
    FExtendedSelect: Boolean;
    FTabWidth: Integer;
    FSaveItems: TStringList;
    FSaveTopIndex: Integer;
    FSaveItemIndex: Integer;
    FOnDrawItem: TDrawItemEvent;
    FOnMeasureItem: TMeasureItemEvent;
    FOnData: TLBGetDataEvent;
    FOnDataFind: TLBFindDataEvent;
    FOnDataObject: TLBGetDataObjectEvent;
    FOnItemsChange: TNotifyEvent;
    FOnItemIndexChange: TNotifyEvent;
    FLastItemIndex : Integer;
    FItemIndexChangedAccumulator : Integer;
    FForceItemIndexChanged : Boolean;
    function GetItemHeight: Integer;
    function GetTopIndex: Integer;
    procedure LBGetText(var Message: TMessage); message LB_GETTEXT;
    procedure LBGetTextLen(var Message: TMessage); message LB_GETTEXTLEN;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColumnWidth;
    procedure SetColumns(Value: Integer);
    procedure SetCount(const Value: Integer);
    procedure SetExtendedSelect(Value: Boolean);
    procedure SetIntegralHeight(Value: Boolean);
    procedure SetItemHeight(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure SetSelected(Index: Integer; Value: Boolean);
    procedure SetSorted(Value: Boolean);
    procedure SetStyle(Value: TListBoxStyle);
    procedure SetTabWidth(Value: Integer);
    procedure SetTopIndex(Value: Integer);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    function GetScrollWidth: Integer;
    procedure SetScrollWidth(const Value: Integer);
    Procedure TriggerScrollEvent(Pos: Integer);
    Property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
    procedure WMAfterKeyPress(var message: TMessage); message WM_AFTERKEYPRESS;
    procedure DoMouseMoveTimer(Sender: TObject);
    procedure WMItemIndexChanged(var Message: TMessage); message WM_ItemIndexChanged;
  protected
    FMoving: Boolean;
    procedure ItemIndexChanged; virtual;
    procedure ItemsChanged; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function DoGetData(const Index: Integer): String;
    function DoGetDataObject(const Index: Integer): TObject;
    function DoFindData(const Data: String): Integer;
    procedure WndProc(var Message: TMessage); override;
    procedure DragCanceled; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
    function GetCount: Integer; override;
    function GetSelCount: Integer; override;
    procedure MeasureItem(Index: Integer; var Height: Integer); virtual;
    function InternalGetItemData(Index: Integer): Longint; dynamic;
    procedure InternalSetItemData(Index: Integer; AData: Longint); dynamic;
    function GetItemData(Index: Integer): LongInt; dynamic;
    function GetItemIndex: Integer; override;
    function GetSelected(Index: Integer): Boolean;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetItemData(Index: Integer; AData: LongInt); dynamic;
    procedure ResetContent; dynamic;
    procedure DeleteString(Index: Integer); dynamic;
    procedure SetMultiSelect(Value: Boolean); override;
    procedure SetItemIndex(const Value: Integer); override;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Columns: Integer read FColumns write SetColumns default 0;
    property ExtendedSelect: Boolean read FExtendedSelect write SetExtendedSelect default True;
    property IntegralHeight: Boolean read FIntegralHeight write SetIntegralHeight default False;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ParentColor default False;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property Style: TListBoxStyle read FStyle write SetStyle default lbStandard;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 0;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    property OnData: TLBGetDataEvent read FOnData write FOnData;
    property OnDataObject: TLBGetDataObjectEvent read FOnDataObject write FOnDataObject;
    property OnDataFind: TLBFindDataEvent read FOnDataFind write FOnDataFind;
    property OnItemsChange: TNotifyEvent read FOnItemsChange write FOnItemsChange;
    property OnItemIndexChange: TNotifyEvent read FOnItemIndexChange write FOnItemIndexChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(Item: String; AObject: TObject); override;
    procedure Clear; override;
    procedure ClearSelection; override;
    procedure CopySelection(Destination: TCustomListControl); override;
    procedure DeleteSelected; override;
    function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
    function ItemRect(Index: Integer): TRect;
    procedure SelectAll; override;
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default True;
    property Canvas: TCanvas read FCanvas;
    property Count: Integer read GetCount write SetCount;
    property Items: TStrings read FItems write SetItems;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property ScrollWidth: Integer read GetScrollWidth write SetScrollWidth default 0;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
  published
    property TabStop default True;
  end;

  {--------------------------------------------------------}
  TALNoScrollBarListBox = class(TALCustomNoScrollBarListBox)
  published
    property Style;
    property AutoComplete;
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollWidth;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    Property OnItemsChange;
    Property OnItemIndexChange;
  end;

  {----------------------------------------------}
  TALListBoxScrollBarProperty = class(TPersistent)
  Private
    FScrollBar : TalScrollBar;
    FOnChange: TNotifyEvent;
  protected
    procedure SetBarColor(const Value: TColor);
    procedure SetArrowColor(const Value: TColor);
    procedure Setcolor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    Function GetBarColor: Tcolor;
    Function GetArrowColor: Tcolor;
    Function Getcolor: Tcolor;
    Function GetBorderColor: Tcolor;
    Function GetmouseInControl : Boolean;
    Function GetMouseIsDown: Boolean;
    Function GetMouseDownAt: TALScrollbarArea;
    Function GetEnabled: Boolean;
  public
    procedure Changed;
    constructor Create(AOwner: TalScrollBar);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property MouseIsDown: Boolean read GetMouseIsDown;
    property MouseDownAt: TALScrollbarArea read GetMouseDownAt;
    property mouseInControl : Boolean read GetmouseInControl;
    property Enabled : Boolean read GetEnabled;
  published
    property BarColor : Tcolor read GetBarColor write SetBarColor default ClBlack;
    property ArrowColor : Tcolor read GetArrowColor write SetArrowColor Default ClBlack;
    property Color: Tcolor read Getcolor Write Setcolor Default clBtnFace;
    property BorderColor : Tcolor read GetBorderColor write SetBorderColor default clBlack;
  end;

  {-------------------------------------}
  TALCustomListBox = class (TCustomPanel)
   private
     FOnDrawItem: TDrawItemEvent;
     FOnMeasureItem: TMeasureItemEvent;
     FOnData: TLBGetDataEvent;
     FOnDataFind: TLBFindDataEvent;
     FOnDataObject: TLBGetDataObjectEvent;
     FReadOnly : Boolean;
     FintegralHeight: Boolean;
     FborderColor: Tcolor;
     FmouseIsDown: Boolean;
     FmouseInControl : Boolean;
     FListBox:TALNoScrollBarListBox;
     FVertScrollBar: TALScrollBar;
     FVertScrollBarProperty: TALListBoxScrollBarProperty;
     FOnPaint: TAlPaintEvent;
     FOnPaintScrollBar: TAlScrollBarPaintEvent;
     FcustomProperty : String;
     FOnItemIndexChange: TNotifyEvent;
     procedure ItemIndexChanged(Sender: Tobject);
     Procedure itemsChanged(Sender: Tobject);
     Procedure PaintListBox;
     procedure PaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
     Procedure ScrollBarMove(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
     Procedure ListBoxMove(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
     procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
     procedure SetItems(Value: TStrings);
     Function GetItems: Tstrings;
     Function GetAutocomplete: Boolean;
     Procedure SetAutoComplete(Value: Boolean);
     Function GetExtendedSelect: Boolean;
     Procedure SetExtendedSelect(Value: Boolean);
     Function GetIntegralHeight: Boolean;
     Procedure SetIntegralHeight(Value: Boolean);
     Function GetMultiSelect: Boolean;
     Procedure SetMultiSelect(Value: Boolean);
     Function GetSorted: Boolean;
     Procedure SetSorted(Value: Boolean);
     Function GetTabWidth: Integer;
     Procedure SetTabWidth(Value: Integer);
     Function Getcount: Integer;
     Procedure SetCount(Value: Integer);
     procedure SetSelected(Index: Integer; Value: Boolean);
     function GetSelected(Index: Integer): Boolean;
     Function GetTopIndex: Integer;
     Procedure SetTopIndex(Value: Integer);
     Function GetItemIndex: Integer;
     Procedure SetItemIndex(Value: Integer);
     Function GetItemHeight: Integer;
     Function CalculateIntegralHeight(FromHeight: Integer): Integer;
     Procedure InternaldoClick(Sender: TObject);
     Procedure InternaldoDblClick(Sender: TObject);
     Procedure InternaldoDragDrop(Sender, Source: TObject; X, Y: Integer);
     Procedure InternaldoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
     Procedure InternaldoEndDrag(Sender, Target: TObject; X, Y: Integer);
     Procedure InternaldoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
     Procedure InternaldoKeyPress(Sender: TObject; var Key: Char);
     Procedure InternaldoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
     Procedure InternaldoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     Procedure InternaldoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     Procedure InternaldoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
     Procedure InternaldoStartDrag(Sender: TObject; var DragObject: TDragObject);
     function GetStyle: TListBoxStyle;
     procedure SetStyle(const Value: TListBoxStyle);
     procedure SetItemHeight(const Value: Integer);
     function GetCanvas: TCanvas;
     procedure SetOnData(const Value: TLBGetDataEvent);
     procedure SetOnDataFind(const Value: TLBFindDataEvent);
     procedure SetOnDataObject(const Value: TLBGetDataObjectEvent);
     procedure SetOnDrawItem(const Value: TDrawItemEvent);
     procedure SetOnMeasureItem(const Value: TMeasureItemEvent);
     procedure InternalDoMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
     procedure InternalDoData(Control: TWinControl; Index: Integer; var Data: string);
     function InternalDoDataFind(Control: TWinControl; FindString: string): Integer;
     procedure InternalDoDataObject(Control: TWinControl; Index: Integer; var DataObject: TObject);
     procedure InternalDoDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
   Protected
     function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
     procedure Loaded; override;
     procedure SetDragMode(Value: TDragMode); override;
     procedure SetDragKind(Value: TDragKind);
     procedure SetDragCursor(Value: TCursor);
     function GetDragKind: TDragKind;
     function GetDragCursor: TCursor;
     property OnItemIndexChange: TNotifyEvent read FOnItemIndexChange write FOnItemIndexChange;
     procedure CreateWindowHandle(const Params: TCreateParams); override;
     procedure WndProc(var Message: TMessage); override;
     Property VertScrollBar: TALListBoxScrollBarProperty read FVertScrollBarProperty write FVertScrollBarProperty;
     Property OnPaint :TAlPaintEvent read FonPaint write FonPaint;
     Property OnPaintScrollBar :TAlScrollBarPaintEvent read FonPaintScrollBar write FonPaintScrollBar;
     property Items: TStrings read GetItems write SetItems;
     property AutoComplete : boolean read GetAutocomplete write SetAutoComplete Default True;
     property ExtendedSelect: Boolean Read GetExtendedSelect Write SetExtendedSelect Default True;
     property IntegralHeight: Boolean Read GetIntegralHeight Write SetIntegralHeight Default False;
     property MultiSelect: Boolean Read GetMultiSelect Write SetMultiSelect Default False;
     property Sorted: Boolean Read GetSorted Write SetSorted Default False;
     property TabWidth: Integer Read GetTabWidth Write SetTabWidth Default 0;
     Property BorderColor: Tcolor Read FBorderColor Write FborderColor Default ClBlack;
     Property CustomProperty: String read FcustomProperty write FcustomProperty;
     property ReadOnly: Boolean Read FReadOnly Write FReadOnly Default False;
     property DragCursor read GetDragCursor write SetDragCursor default crDrag;
     property DragKind read GetDragKind Write SetDragKind default dkdrag;
     property Style: TListBoxStyle read GetStyle write SetStyle default lbStandard;
     property ItemHeight: Integer read GetItemHeight write SetItemHeight;
     property OnDrawItem: TDrawItemEvent read FOnDrawItem write SetOnDrawItem;
     property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write SetOnMeasureItem;
     property OnData: TLBGetDataEvent read FOnData write SetOnData;
     property OnDataObject: TLBGetDataObjectEvent read FOnDataObject write SetOnDataObject;
     property OnDataFind: TLBFindDataEvent read FOnDataFind write SetOnDataFind;
   public
     constructor Create(AOwner: TComponent); override;
     Destructor Destroy; Override;
     procedure AddItem(Item: String; AObject: TObject); virtual;
     procedure Clear; virtual;
     procedure ClearSelection; virtual;
     procedure DeleteSelected; virtual;
     function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer; virtual;
     function ItemRect(Index: Integer): TRect; virtual;
     procedure SelectAll; virtual;
     Property MouseInControl: Boolean read FmouseInControl;
     property Count: Integer read GetCount write SetCount;
     property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
     property TopIndex: Integer read GetTopIndex write SetTopIndex;
     property ItemIndex: Integer read GetItemIndex write SetItemIndex;
     function Focused: Boolean; override;
     property Canvas: TCanvas read GetCanvas;
   published
 end;

 {-----------------------------------}
 TALListBox = class (TALCustomListBox)
   private
   Protected
   public
   published
      Property ReadOnly;
      Property CustomProperty;
      Property OnPaint;
      Property OnPaintScrollBar;
      Property VertScrollBar;
      Property BorderColor;
      //property BevelEdges;
      //property BevelInner;
      //property BevelKind default bkNone;
      //property BevelOuter;
      //property BorderStyle;
      //property Columns;
      //property Ctl3D;
      //property ParentCtl3D;
      //property ScrollWidth;
      //property OnContextPopup;
      //property OnEndDock;
      //property OnStartDock;
      //Property OnItemsChange;
      property Style;
      property ItemHeight;
      property OnDrawItem;
      property OnMeasureItem;
      //property OnData;
      //property OnDataFind;
      //property OnDataObject;
      property AutoComplete;
      property Align;
      property Anchors;
      property BiDiMode;
      property Color Default clWhite;
      property Constraints;
      property DragCursor;
      property DragKind;
      property DragMode;
      property Enabled;
      property ExtendedSelect;
      property Font;
      property ImeMode;
      property ImeName;
      property IntegralHeight;
      property Items;
      property MultiSelect;
      property ParentBiDiMode;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property Sorted;
      property TabOrder;
      property TabStop;
      property TabWidth;
      property Visible;
      property OnClick;
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnStartDrag;
      Property OnItemIndexChange;
 end;

 {-------------------------------}
 TALKeyListBox = class(TALListBox)
  private
    FitemsKey : Tstrings;
  protected
    procedure SetItemsKey(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Procedure AddItemKey(Value,key: String);
    procedure Clear; override;
    procedure DeleteSelected; override;
    function IndexOfKey(key: String):integer;
    property ItemsKey: TStrings read FitemsKey write SetItemsKey;
  published
  end;


procedure Register;

implementation

{$R ..\resource\ALListBox.dcr}

uses Consts,
     RTLConsts;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALListBox]);
  RegisterComponents('Alcinoe', [TALKeyListBox]);
end;


////////////////////////////////////////////////////////////////////////////
///////////////// Classe NoScrollBarLISTBOX ////////////////////////////////
////////////////////////////////////////////////////////////////////////////

{----------------------------------------------------------------}
const BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);

{**********************************}
procedure TALListBoxStrings.Changed;
begin
  ListBox.ItemsChanged;
end;

{*******************************************}
function TALListBoxStrings.GetCount: Integer;
begin
  Result := SendMessage(ListBox.Handle, LB_GETCOUNT, 0, 0);
end;

{*****************************************************}
function TALListBoxStrings.Get(Index: Integer): string;
var Len: Integer;
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then
    Result := ListBox.DoGetData(Index)
  else
  begin
    Len := SendMessage(ListBox.Handle, LB_GETTEXTLEN, Index, 0);
    if Len = LB_ERR then Error(SListIndexError, Index);
    SetLength(Result, Len);
    if Len <> 0 then
    begin
      Len := SendMessage(ListBox.Handle, LB_GETTEXT, Index, Longint(PChar(Result)));
      SetLength(Result, Len);  // LB_GETTEXTLEN isn't guaranteed to be accurate
    end;
  end;
end;

{************************************************************}
function TALListBoxStrings.GetObject(Index: Integer): TObject;
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then
    Result := ListBox.DoGetDataObject(Index)
  else
  begin
    Result := TObject(ListBox.GetItemData(Index));
    if Longint(Result) = LB_ERR then Error(SListIndexError, Index);
  end;
end;

{***************************************************************}
procedure TALListBoxStrings.Put(Index: Integer; const S: string);
var
  I: Integer;
  TempData: Longint;
begin
  I := ListBox.ItemIndex;
  TempData := ListBox.InternalGetItemData(Index);
  // Set the Item to 0 in case it is an object that gets freed during Delete
  ListBox.InternalSetItemData(Index, 0);
  Delete(Index);
  InsertObject(Index, S, nil);
  ListBox.InternalSetItemData(Index, TempData);
  ListBox.ItemIndex := I;
  Changed;
end;

{**********************************************************************}
procedure TALListBoxStrings.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index <> -1) and not (ListBox.Style in [lbVirtual, lbVirtualOwnerDraw]) then
    ListBox.SetItemData(Index, LongInt(AObject));
  Changed;
end;

{*******************************************************}
function TALListBoxStrings.Add(const S: string): Integer;
begin
  Result := -1;
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  Result := SendMessage(ListBox.Handle, LB_ADDSTRING, 0, Longint(PChar(S)));
  if Result < 0 then raise EOutOfResources.Create(SInsertLineError);
  Changed;
end;

{******************************************************************}
procedure TALListBoxStrings.Insert(Index: Integer; const S: string);
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  if SendMessage(ListBox.Handle, LB_INSERTSTRING, Index,
    Longint(PChar(S))) < 0 then
    raise EOutOfResources.Create(SInsertLineError);
  Changed;
end;

{*************************************************}
procedure TALListBoxStrings.Delete(Index: Integer);
begin
  ListBox.DeleteString(Index);
  Changed;
end;

{************************************************************}
procedure TALListBoxStrings.Exchange(Index1, Index2: Integer);
var
  TempData: Longint;
  TempString: string;
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempData := ListBox.InternalGetItemData(Index1);
    Strings[Index1] := Strings[Index2];
    ListBox.InternalSetItemData(Index1, ListBox.InternalGetItemData(Index2));
    Strings[Index2] := TempString;
    ListBox.InternalSetItemData(Index2, TempData);
    if ListBox.ItemIndex = Index1 then
      ListBox.ItemIndex := Index2
    else if ListBox.ItemIndex = Index2 then
      ListBox.ItemIndex := Index1;
    Changed;
  finally
    EndUpdate;
  end;
end;

{********************************}
procedure TALListBoxStrings.Clear;
begin
  ListBox.ResetContent;
  Changed;
end;

{************************************************************}
procedure TALListBoxStrings.SetUpdateState(Updating: Boolean);
begin
  SendMessage(ListBox.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then begin
    ListBox.Refresh;
    Changed;
  end;
end;

{***********************************************************}
function TALListBoxStrings.IndexOf(const S: string): Integer;
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then
    Result := ListBox.DoFindData(S)
  else
    Result := SendMessage(ListBox.Handle, LB_FINDSTRINGEXACT, -1, LongInt(PChar(S)));
end;

{************************************************************}
procedure TALListBoxStrings.Move(CurIndex, NewIndex: Integer);
var
  TempData: Longint;
  TempString: string;
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  BeginUpdate;
  ListBox.FMoving := True;
  try
    if CurIndex <> NewIndex then
    begin
      TempString := Get(CurIndex);
      TempData := ListBox.InternalGetItemData(CurIndex);
      ListBox.InternalSetItemData(CurIndex, 0);
      Delete(CurIndex);
      Insert(NewIndex, TempString);
      ListBox.InternalSetItemData(NewIndex, TempData);
    end;
  finally
    ListBox.FMoving := False;
    EndUpdate;
  end;
end;

{**********************************************************************}
procedure TALCustomNoScrollBarListBox.DoMouseMoveTimer(Sender: TObject);
begin
  if MultiSelect then TriggerScrollEvent(SendMessage(Handle, LB_GETCARETINDEX, 0, 0))
  else TriggerScrollEvent(SendMessage(Handle, LB_GETCURSEL, 0, 0));

  ItemIndexChanged;
end;

{***************************************************************************}
procedure TALCustomNoScrollBarListBox.WMAfterKeyPress(var message: TMessage);
Begin
  if MultiSelect then TriggerScrollEvent(SendMessage(Handle, LB_GETCARETINDEX, 0, 0))
  else TriggerScrollEvent(SendMessage(Handle, LB_GETCURSEL, 0, 0));

  ItemIndexChanged;
end;

{*******************************************************************************}
procedure TALCustomNoScrollBarListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  POSTMESSAGE(self.Handle,WM_AFTERKEYPRESS,0,0);
end;

{*********************************************************************}
Procedure TALCustomNoScrollBarListBox.TriggerScrollEvent(Pos: Integer);
Begin
 If Pos > items.Count - 1 then Pos := items.Count - 1;
 If Pos < 0 then pos := 0;

 IF (FcurPos <> Pos) then begin
   FcurPos := Pos;
   If assigned(FonScroll) then FOnScroll(Self, scposition, FcurPos);
 end;
End;

{*************************************************}
procedure TALCustomNoScrollBarListBox.ItemsChanged;
begin
  If assigned(FonItemsChange) then FonItemsChange(self);
  FForceItemIndexChanged := True;
  ItemIndexChanged;
end;

{*****************************************************}
procedure TALCustomNoScrollBarListBox.ItemIndexChanged;
begin
  FItemIndexChangedAccumulator := 1;
  POSTMESSAGE(self.Handle,WM_ItemIndexChanged,0,0);
end;

{******************************************************************************}
procedure TALCustomNoScrollBarListBox.WMItemIndexChanged(var Message: TMessage);
Var AItemIndex : Integer;
begin
 iF FItemIndexChangedAccumulator = 0 THEN EXIT;

 If assigned(FonItemIndexChange) then begin
   AItemIndex := itemIndex;
   If (AitemIndex <> FlastItemIndex) or FForceItemIndexChanged then Begin
     FForceItemIndexChanged := False;
     FlastItemIndex := AITemIndex;
     FonItemIndexChange(self);
   end;
 end;

 FItemIndexChangedAccumulator := 0;
end;

{*****************************************************************}
constructor TALCustomNoScrollBarListBox.Create(AOwner: TComponent);
const
  ListBoxStyle = [csSetCaption, csDoubleClicks];
begin
  inherited Create(AOwner);
  if NewStyleControls then
    ControlStyle := ListBoxStyle else
    ControlStyle := ListBoxStyle + [csFramed];
  Width := 121;
  Height := 97;
  TabStop := True;
  ParentColor := False;
  FAutoComplete := True;
  FItems := TALListBoxStrings.Create;
  TALListBoxStrings(FItems).ListBox := Self;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FItemHeight := 16;
  FBorderStyle := bsSingle;
  FExtendedSelect := True;
  FOldCount := -1;
  FcurPos := 0;

  FMouseMoveTimer := TTimer.Create(Self);
  with FMouseMoveTimer do begin
    Enabled := False;
    Interval := 50;
    OnTimer := DoMouseMoveTimer;
  end;

  FLastItemIndex := -1;
  FItemIndexChangedAccumulator := 0;
  FForceItemIndexChanged := False;
end;

{*********************************************}
destructor TALCustomNoScrollBarListBox.Destroy;
begin
  FMouseMoveTimer.Enabled := True;
  FMouseMoveTimer.Free;
  inherited Destroy;
  FCanvas.Free;
  FItems.Free;
  FSaveItems.Free;
end;

{****************************************************************************}
procedure TALCustomNoScrollBarListBox.AddItem(Item: String; AObject: TObject);
var
  S: String;
begin
  SetString(S, PChar(Item), StrLen(PChar(Item)));
  Items.AddObject(S, AObject);
end;

{************************************************************************}
function TALCustomNoScrollBarListBox.GetItemData(Index: Integer): LongInt;
begin
  Result := SendMessage(Handle, LB_GETITEMDATA, Index, 0);
end;

{********************************************************************************}
procedure TALCustomNoScrollBarListBox.SetItemData(Index: Integer; AData: LongInt);
begin
  SendMessage(Handle, LB_SETITEMDATA, Index, AData);
end;

{********************************************************************************}
function TALCustomNoScrollBarListBox.InternalGetItemData(Index: Integer): LongInt;
begin
  Result := GetItemData(Index);
end;

{****************************************************************************************}
procedure TALCustomNoScrollBarListBox.InternalSetItemData(Index: Integer; AData: LongInt);
begin
  SetItemData(Index, AData);
end;

{*******************************************************************}
procedure TALCustomNoScrollBarListBox.DeleteString( Index: Integer );
begin
  SendMessage(Handle, LB_DELETESTRING, Index, 0);
end;

{*************************************************}
procedure TALCustomNoScrollBarListBox.ResetContent;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  SendMessage(Handle, LB_RESETCONTENT, 0, 0);
end;

{******************************************}
procedure TALCustomNoScrollBarListBox.Clear;
begin
  FItems.Clear;
end;

{***************************************************}
procedure TALCustomNoScrollBarListBox.ClearSelection;
var I: Integer;
begin
  if MultiSelect then begin
    for I := 0 to Items.Count - 1 do Selected[I] := False
  end
  else ItemIndex := -1;
end;

{***********************************************************************************}
procedure TALCustomNoScrollBarListBox.CopySelection(Destination: TCustomListControl);
var
  I: Integer;
begin
  if MultiSelect then
  begin
    for I := 0 to Items.Count - 1 do
      if Selected[I] then Destination.AddItem(PChar(Items[I]), Items.Objects[I]);
  end
  else if ItemIndex <> -1 then
      Destination.AddItem(PChar(Items[ItemIndex]), Items.Objects[ItemIndex]);
end;

{***************************************************}
procedure TALCustomNoScrollBarListBox.DeleteSelected;
var
  I: Integer;
begin
  if MultiSelect then begin
    for I := Items.Count - 1 downto 0 do
      if Selected[I] then Items.Delete(I);
  end
  else if ItemIndex <> -1 then
      Items.Delete(ItemIndex);
end;

{***************************************************}
procedure TALCustomNoScrollBarListBox.SetColumnWidth;
var
  ColWidth: Integer;
begin
  if (FColumns > 0) and (Width > 0) then
  begin
    ColWidth := Trunc(ClientWidth / FColumns);
    if ColWidth < 1 then ColWidth := 1;
    SendMessage(Handle, LB_SETCOLUMNWIDTH, ColWidth, 0);
  end;
end;

{***************************************************************}
procedure TALCustomNoScrollBarListBox.SetColumns(Value: Integer);
begin
  if FColumns <> Value then
    if (FColumns = 0) or (Value = 0) then
    begin
      FColumns := Value;
      RecreateWnd;
    end else
    begin
      FColumns := Value;
      if HandleAllocated then SetColumnWidth;
    end;
end;

{*********************************************************}
function TALCustomNoScrollBarListBox.GetItemIndex: Integer;
begin
  if MultiSelect then
    Result := SendMessage(Handle, LB_GETCARETINDEX, 0, 0)
  else
    Result := SendMessage(Handle, LB_GETCURSEL, 0, 0);
end;

{*****************************************************}
function TALCustomNoScrollBarListBox.GetCount: Integer;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then
    Result := FCount
  else
    Result := Items.Count;
end;

{********************************************************}
function TALCustomNoScrollBarListBox.GetSelCount: Integer;
begin
  Result := SendMessage(Handle, LB_GETSELCOUNT, 0, 0);
end;

{***********************************************************************}
procedure TALCustomNoScrollBarListBox.SetItemIndex(const Value: Integer);
begin
  if GetItemIndex <> Value then begin
    if MultiSelect then SendMessage(Handle, LB_SETCARETINDEX, Value, 0)
    else SendMessage(Handle, LB_SETCURSEL, Value, 0);

    TriggerScrollEvent(Value);
    ItemIndexChanged;
  end;
end;

{**********************************************************************}
procedure TALCustomNoScrollBarListBox.SetExtendedSelect(Value: Boolean);
begin
  if Value <> FExtendedSelect then
  begin
    FExtendedSelect := Value;
    RecreateWnd;
  end;
end;

{**********************************************************************}
procedure TALCustomNoScrollBarListBox.SetIntegralHeight(Value: Boolean);
begin
  if Value <> FIntegralHeight then
  begin
    FIntegralHeight := Value;
    RecreateWnd;
    RequestAlign;
  end;
end;

{**********************************************************}
function TALCustomNoScrollBarListBox.GetItemHeight: Integer;
var
  R: TRect;
begin
  Result := FItemHeight;
  if HandleAllocated and (FStyle = lbStandard) then
  begin
    Perform(LB_GETITEMRECT, 0, Longint(@R));
    Result := R.Bottom - R.Top;
  end;
end;

{******************************************************************}
procedure TALCustomNoScrollBarListBox.SetItemHeight(Value: Integer);
begin
  if (FItemHeight <> Value) and (Value > 0) then
  begin
    FItemHeight := Value;
    RecreateWnd;
  end;
end;

{****************************************************************}
procedure TALCustomNoScrollBarListBox.SetTabWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FTabWidth <> Value then
  begin
    FTabWidth := Value;
    RecreateWnd;
  end;
end;

{*******************************************************************}
procedure TALCustomNoScrollBarListBox.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

{************************************************************************}
function TALCustomNoScrollBarListBox.GetSelected(Index: Integer): Boolean;
var
  R: Longint;
begin
  R := SendMessage(Handle, LB_GETSEL, Index, 0);
  if R = LB_ERR then
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
  Result := LongBool(R);
end;

{********************************************************************************}
procedure TALCustomNoScrollBarListBox.SetSelected(Index: Integer; Value: Boolean);
begin
  if FMultiSelect then begin
    if SendMessage(Handle, LB_SETSEL, Longint(Value), Index) = LB_ERR then
      raise EListError.CreateResFmt(@SListIndexError, [Index]);
  end
  else
    if Value then begin
      if SendMessage(Handle, LB_SETCURSEL, Index, 0) = LB_ERR then
        raise EListError.CreateResFmt(@SListIndexError, [Index])
    end
    else SendMessage(Handle, LB_SETCURSEL, -1, 0);


  ItemIndexChanged;
end;

{**************************************************************}
procedure TALCustomNoScrollBarListBox.SetSorted(Value: Boolean);
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  if FSorted <> Value then
  begin
    FSorted := Value;
    RecreateWnd;
  end;
end;

{*******************************************************************}
procedure TALCustomNoScrollBarListBox.SetStyle(Value: TListBoxStyle);
begin
  if FStyle <> Value then
  begin
    if Value in [lbVirtual, lbVirtualOwnerDraw] then
    begin
      Items.Clear;
      Sorted := False;
    end;
    FStyle := Value;
    RecreateWnd;
  end;
end;

{********************************************************}
function TALCustomNoScrollBarListBox.GetTopIndex: Integer;
begin
  Result := SendMessage(Handle, LB_GETTOPINDEX, 0, 0);
end;

{*********************************************************************}
procedure TALCustomNoScrollBarListBox.LBGetText(var Message: TMessage);
var
  S: string;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then
  begin
    if Assigned(FOnData) and (Message.WParam > -1) and (Message.WParam < Count) then
    begin
      S := '';
      OnData(Self, Message.wParam, S);
      StrCopy(PChar(Message.lParam), PChar(S));
      Message.Result := Length(S);
    end
    else
      Message.Result := LB_ERR;
  end
  else
    inherited;
end;

{************************************************************************}
procedure TALCustomNoScrollBarListBox.LBGetTextLen(var Message: TMessage);
var
  S: string;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then
  begin
    if Assigned(FOnData) and (Message.WParam > -1) and (Message.WParam < Count) then
    begin
      S := '';
      OnData(Self, Message.wParam, S);
      Message.Result := Length(S);
    end
    else
      Message.Result := LB_ERR;
  end
  else
    inherited;
end;

{************************************************************************}
procedure TALCustomNoScrollBarListBox.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

{****************************************************************}
procedure TALCustomNoScrollBarListBox.SetTopIndex(Value: Integer);
begin
  if GetTopIndex <> Value then
    SendMessage(Handle, LB_SETTOPINDEX, Value, 0);
end;

{**************************************************************}
procedure TALCustomNoScrollBarListBox.SetItems(Value: TStrings);
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then
    case Style of
      lbVirtual: Style := lbStandard;
      lbVirtualOwnerDraw: Style := lbOwnerDrawFixed;
    end;
  Items.Assign(Value);
end;

{**************************************************************************************}
function TALCustomNoScrollBarListBox.ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
var
  Count: Integer;
  ItemRect: TRect;
begin
  if PtInRect(ClientRect, Pos) then
  begin
    Result := TopIndex;
    Count := Items.Count;
    while Result < Count do
    begin
      Perform(LB_GETITEMRECT, Result, Longint(@ItemRect));
      if PtInRect(ItemRect, Pos) then Exit;
      Inc(Result);
    end;
    if not Existing then Exit;
  end;
  Result := -1;
end;

{*******************************************************************}
function TALCustomNoScrollBarListBox.ItemRect(Index: Integer): TRect;
var
  Count: Integer;
begin
  Count := Items.Count;
  if (Index = 0) or (Index < Count) then
    Perform(LB_GETITEMRECT, Index, Longint(@Result))
  else if Index = Count then
  begin
    Perform(LB_GETITEMRECT, Index - 1, Longint(@Result));
    OffsetRect(Result, 0, Result.Bottom - Result.Top);
  end else FillChar(Result, SizeOf(Result), 0);
end;

{****************************************************************************}
procedure TALCustomNoScrollBarListBox.CreateParams(var Params: TCreateParams);
type
  PSelects = ^TSelects;
  TSelects = array[Boolean] of DWORD;
const
  Styles: array[TListBoxStyle] of DWORD =
    (0, LBS_OWNERDRAWFIXED, LBS_OWNERDRAWVARIABLE, LBS_OWNERDRAWFIXED,
     LBS_OWNERDRAWFIXED);
  Sorteds: array[Boolean] of DWORD = (0, LBS_SORT);
  MultiSelects: array[Boolean] of DWORD = (0, LBS_MULTIPLESEL);
  ExtendSelects: array[Boolean] of DWORD = (0, LBS_EXTENDEDSEL);
  IntegralHeights: array[Boolean] of DWORD = (LBS_NOINTEGRALHEIGHT, 0);
  MultiColumns: array[Boolean] of DWORD = (0, LBS_MULTICOLUMN);
  TabStops: array[Boolean] of DWORD = (0, LBS_USETABSTOPS);
  CSHREDRAW: array[Boolean] of DWORD = (CS_HREDRAW, 0);
  Data: array[Boolean] of DWORD = (LBS_HASSTRINGS, LBS_NODATA);
var
  Selects: PSelects;
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'LISTBOX');
  with Params do
  begin
    Selects := @MultiSelects;
    if FExtendedSelect then Selects := @ExtendSelects;
    Style := Style or (Data[Self.Style in [lbVirtual, lbVirtualOwnerDraw]] or
      LBS_NOTIFY) or Styles[FStyle] or Sorteds[FSorted] or
      Selects^[FMultiSelect] or IntegralHeights[FIntegralHeight] or
      MultiColumns[FColumns <> 0] or BorderStyles[FBorderStyle] or
      TabStops[FTabWidth <> 0];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CSHREDRAW[UseRightToLeftAlignment] or CS_VREDRAW);
  end;
end;

{**********************************************}
procedure TALCustomNoScrollBarListBox.CreateWnd;
var
  W, H: Integer;
begin
  W := Width;
  H := Height;
  inherited CreateWnd;
  SetWindowPos(Handle, 0, Left, Top, W, H, SWP_NOZORDER or SWP_NOACTIVATE);
  if FTabWidth <> 0 then
    SendMessage(Handle, LB_SETTABSTOPS, 1, Longint(@FTabWidth));
  SetColumnWidth;
  if (FOldCount <> -1) or Assigned(FSaveItems) then
  begin
    if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
      Count := FOldCount;
    if FSaveItems <> nil then
    begin
      FItems.Assign(FSaveItems);
      FreeAndNil(FSaveItems);
    end;
    SetTopIndex(FSaveTopIndex);
    SetItemIndex(FSaveItemIndex);
    FOldCount := -1;
  end;
end;

{***********************************************}
procedure TALCustomNoScrollBarListBox.DestroyWnd;
begin
  if (FItems.Count > 0) then
  begin
    if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
      FOldCount := FItems.Count
    else
    begin
      FSaveItems := TStringList.Create;
      FSaveItems.Assign(FItems);
    end;
    FSaveTopIndex := GetTopIndex;
    FSaveItemIndex := GetItemIndex;
  end;
  inherited DestroyWnd;
end;

{*******************************************************************}
procedure TALCustomNoScrollBarListBox.WndProc(var Message: TMessage);
begin
  {for auto drag mode, let listbox handle itself, instead of TControl}
  if not (csDesigning in ComponentState) and ((Message.Msg = WM_LBUTTONDOWN) or
    (Message.Msg = WM_LBUTTONDBLCLK)) and not Dragging then
  begin
    if DragMode = dmAutomatic then
    begin
      if IsControlMouseMsg(TWMMouse(Message)) then
        Exit;
      ControlState := ControlState + [csLButtonDown];
      Dispatch(Message);  {overrides TControl's BeginDrag}
      Exit;
    end;
  end;
  inherited WndProc(Message);
end;

{*******************************************************************************}
procedure TALCustomNoScrollBarListBox.WMLButtonDown(var Message: TWMLButtonDown);
var
  ItemNo : Integer;
  ShiftState: TShiftState;
begin
  ItemNo := ItemAtPos(SmallPointToPoint(Message.Pos), True);
  FMouseMoveTimer.Enabled := True;

  ShiftState := KeysToShiftState(Message.Keys);
  if (DragMode = dmAutomatic) and FMultiSelect then
  begin
    if not (ssShift in ShiftState) or (ssCtrl in ShiftState) then
    begin
      if (ItemNo >= 0) and (Selected[ItemNo]) then
      begin
        BeginDrag (False);
        Exit;
      end;
    end;
  end;
  inherited;
  if (DragMode = dmAutomatic) and not (FMultiSelect and
    ((ssCtrl in ShiftState) or (ssShift in ShiftState))) then
    BeginDrag(False);
end;

{***************************************************************************}
procedure TALCustomNoScrollBarListBox.WMLButtonUp(var Message: TWMLButtonUp);
begin
  Inherited;
  FMouseMoveTimer.Enabled := False;
end;

{***********************************************************************************}
procedure TALCustomNoScrollBarListBox.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Inherited;
  FMouseMoveTimer.Enabled := True;
end;

{***********************************************************************}
procedure TALCustomNoScrollBarListBox.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    LBN_SELCHANGE:
      begin
        inherited Changed;
        Click;
      end;
    LBN_DBLCLK: DblClick;
  end;
end;

{*******************************************************************}
procedure TALCustomNoScrollBarListBox.WMPaint(var Message: TWMPaint);

  procedure PaintListBox;
  var
    DrawItemMsg: TWMDrawItem;
    MeasureItemMsg: TWMMeasureItem;
    DrawItemStruct: TDrawItemStruct;
    MeasureItemStruct: TMeasureItemStruct;
    R: TRect;
    Y, I, H, W: Integer;
  begin
    { Initialize drawing records }
    DrawItemMsg.Msg := CN_DRAWITEM;
    DrawItemMsg.DrawItemStruct := @DrawItemStruct;
    DrawItemMsg.Ctl := Handle;
    DrawItemStruct.CtlType := ODT_LISTBOX;
    DrawItemStruct.itemAction := ODA_DRAWENTIRE;
    DrawItemStruct.itemState := 0;
    DrawItemStruct.hDC := Message.DC;
    DrawItemStruct.CtlID := Handle;
    DrawItemStruct.hwndItem := Handle;

    { Intialize measure records }
    MeasureItemMsg.Msg := CN_MEASUREITEM;
    MeasureItemMsg.IDCtl := Handle;
    MeasureItemMsg.MeasureItemStruct := @MeasureItemStruct;
    MeasureItemStruct.CtlType := ODT_LISTBOX;
    MeasureItemStruct.CtlID := Handle;

    { Draw the listbox }
    Y := 0;
    I := TopIndex;
    GetClipBox(Message.DC, R);
    H := Height;
    W := Width;
    while Y < H do
    begin
      MeasureItemStruct.itemID := I;
      if I < Items.Count then
        MeasureItemStruct.itemData := Longint(Pointer(Items.Objects[I]));
      MeasureItemStruct.itemWidth := W;
      MeasureItemStruct.itemHeight := FItemHeight;
      DrawItemStruct.itemData := MeasureItemStruct.itemData;
      DrawItemStruct.itemID := I;
      Dispatch(MeasureItemMsg);
      DrawItemStruct.rcItem := Rect(0, Y, MeasureItemStruct.itemWidth,
        Y + Integer(MeasureItemStruct.itemHeight));
      Dispatch(DrawItemMsg);
      Inc(Y, MeasureItemStruct.itemHeight);
      Inc(I);
      if I >= Items.Count then break;
    end;
  end;

begin
  if Message.DC <> 0 then
    { Listboxes don't allow paint "sub-classing" like the other windows controls
      so we have to do it ourselves. }
    PaintListBox
  else inherited;
end;

{*****************************************************************}
procedure TALCustomNoScrollBarListBox.WMSize(var Message: TWMSize);
begin
  inherited;
  SetColumnWidth;
end;

{*************************************************}
procedure TALCustomNoScrollBarListBox.DragCanceled;
var
  M: TWMMouse;
  MousePos: TPoint;
begin
  with M do
  begin
    Msg := WM_LBUTTONDOWN;
    GetCursorPos(MousePos);
    Pos := PointToSmallPoint(ScreenToClient(MousePos));
    Keys := 0;
    Result := 0;
  end;
  DefaultHandler(M);
  M.Msg := WM_LBUTTONUP;
  DefaultHandler(M);
end;

{*************************************************************************}
procedure TALCustomNoScrollBarListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Flags: Longint;
  Data: String;
begin
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, Index, Rect, State) else
  begin
    FCanvas.FillRect(Rect);
    if Index < Count then
    begin
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      if not UseRightToLeftAlignment then
        Inc(Rect.Left, 2)
      else
        Dec(Rect.Right, 2);
      Data := '';
      if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
        Data := DoGetData(Index)
      else
        Data := Items[Index];
      DrawText(FCanvas.Handle, PChar(Data), Length(Data), Rect, Flags);
    end;
  end;
end;

{*************************************************************************************}
procedure TALCustomNoScrollBarListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if Assigned(FOnMeasureItem) then FOnMeasureItem(Self, Index, Height)
end;

{*************************************************************************}
procedure TALCustomNoScrollBarListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      FCanvas.Brush.Color := clHighlight;
      FCanvas.Font.Color := clHighlightText
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State) else
      FCanvas.FillRect(rcItem);
    if odFocused in State then DrawFocusRect(hDC, rcItem);
    FCanvas.Handle := 0;
  end;
end;

{*******************************************************************************}
procedure TALCustomNoScrollBarListBox.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemHeight := FItemHeight;
    if FStyle = lbOwnerDrawVariable then
      MeasureItem(itemID, Integer(itemHeight));
  end;
end;

{**************************************************************************}
procedure TALCustomNoScrollBarListBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

{**********************************************}
procedure TALCustomNoScrollBarListBox.SelectAll;
var
  I: Integer;
begin
  if FMultiSelect then
    for I := 0 to Items.Count - 1 do
      Selected[I] := True;
end;

{************************************************************}
procedure TALCustomNoScrollBarListBox.KeyPress(var Key: Char);

  procedure FindString;
  var
    Idx: Integer;
  begin
    if Style in [lbVirtual, lbVirtualOwnerDraw] then
      Idx := DoFindData(FFilter)
    else
      Idx := SendMessage(Handle, LB_FINDSTRING, -1, LongInt(PChar(FFilter)));
    if Idx <> LB_ERR then
    begin
      if MultiSelect then
      begin
        ClearSelection;
        SendMessage(Handle, LB_SELITEMRANGE, 1, MakeLParam(Idx, Idx))
      end;
      ItemIndex := Idx;
      Click;
    end;
    if not Ord(Key) in [VK_RETURN, VK_BACK, VK_ESCAPE] then
      Key := #0;  // Clear so that the listbox's default search mechanism is disabled
  end;

var
  Msg: TMsg;
begin
  inherited KeyPress(Key);
  POSTMESSAGE(self.Handle,WM_AFTERKEYPRESS,0,0);
  if not FAutoComplete then exit;
  if GetTickCount - FLastTime >= 500 then
    FFilter := '';
  FLastTime := GetTickCount;

  if Ord(Key) <> VK_BACK then
  begin
    if Key in LeadBytes then
    begin
      if PeekMessage(Msg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE) then
      begin
        FFilter := FFilter + Key + Chr(Msg.wParam);
        Key := #0;
      end;
    end
    else
      FFilter := FFilter + Key;
  end
  else
  begin
    while ByteType(FFilter, Length(FFilter)) = mbTrailByte do
      Delete(FFilter, Length(FFilter), 1);
    Delete(FFilter, Length(FFilter), 1);
  end;

  if Length(FFilter) > 0 then
    FindString
  else
  begin
    ItemIndex := 0;
    Click;
  end;
end;

{*******************************************************************}
procedure TALCustomNoScrollBarListBox.SetCount(const Value: Integer);
var
  Error: Integer;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then
  begin
    // Limited to 32767 on Win95/98 as per Win32 SDK
    Error := SendMessage(Handle, LB_SETCOUNT, Value, 0);
    if (Error <> LB_ERR) and (Error <> LB_ERRSPACE) then
      FCount := Value
    else
      raise Exception.CreateFmt(SErrorSettingCount, [Name]);
  end
  else
    raise Exception.CreateFmt(SListBoxMustBeVirtual, [Name]);
end;

{***************************************************************************}
function TALCustomNoScrollBarListBox.DoGetData(const Index: Integer): String;
begin
  if Assigned(FOnData) then FOnData(Self, Index, Result);
end;

{**********************************************************************************}
function TALCustomNoScrollBarListBox.DoGetDataObject(const Index: Integer): TObject;
begin
  if Assigned(FOnDataObject) then FOnDataObject(Self, Index, Result);
end;

{***************************************************************************}
function TALCustomNoScrollBarListBox.DoFindData(const Data: String): Integer;
begin
  if Assigned(FOnDataFind) then
    Result := FOnDataFind(Self, Data)
  else
    Result := -1;
end;

{***********************************************************}
function TALCustomNoScrollBarListBox.GetScrollWidth: Integer;
begin
  Result := SendMessage(Handle, LB_GETHORIZONTALEXTENT, 0, 0);
end;

{*************************************************************************}
procedure TALCustomNoScrollBarListBox.SetScrollWidth(const Value: Integer);
begin
  if Value <> ScrollWidth then
    SendMessage(Handle, LB_SETHORIZONTALEXTENT, Value, 0);
end;


////////////////////////////////////////////////////////////////////////////
///////////////// Classe ALLISTBOX /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////

{*******************************************************************}
constructor TALListBoxScrollBarProperty.Create(AOwner: TalScrollBar);
begin
  inherited Create;
  FScrollBar := AOwner;
end;

{********************************************}
procedure TALListBoxScrollBarProperty.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{*********************************************************************}
procedure TALListBoxScrollBarProperty.SetBarColor(const Value: TColor);
begin
  FScrollBar.BarColor := Value;
  Changed;
end;

{***********************************************************************}
procedure TALListBoxScrollBarProperty.SetArrowColor(const Value: TColor);
begin
    FScrollBar.ArrowColor := Value;
    Changed;
end;

{******************************************************************}
procedure TALListBoxScrollBarProperty.Setcolor(const Value: TColor);
begin
    FScrollBar.Color := Value;
    Changed;
end;

{************************************************************************}
procedure TALListBoxScrollBarProperty.SetBorderColor(const Value: TColor);
begin
    FScrollBar.BorderColor := Value;
    Changed;
end;

{*******************************************************}
Function TALListBoxScrollBarProperty.GetBarColor: Tcolor;
begin
  Result := FScrollBar.BarColor;
end;

{*********************************************************}
Function TALListBoxScrollBarProperty.GetArrowColor: Tcolor;
begin
  Result := FScrollBar.arrowColor;
end;

{****************************************************}
Function TALListBoxScrollBarProperty.Getcolor: Tcolor;
begin
  Result := FScrollBar.Color;
end;

{**********************************************************}
Function TALListBoxScrollBarProperty.GetBorderColor: Tcolor;
begin
  Result := FScrollBar.BorderColor;
end;

{***************************************************************}
Function TALListBoxScrollBarProperty.GetmouseInControl : Boolean;
begin
  Result := FScrollBar.MouseInControl;
end;

{***********************************************************}
Function TALListBoxScrollBarProperty.GetMouseIsDown: Boolean;
begin
  Result := FScrollBar.MouseIsDown;
end;

{********************************************************************}
Function TALListBoxScrollBarProperty.GetMouseDownAt: TALScrollbarArea;
begin
  Result := FScrollBar.MouseDownAt;
end;

{*******************************************************}
Function TALListBoxScrollBarProperty.GetEnabled: Boolean;
begin
  Result := FScrollBar.Enabled;
end;

{**********************************************************}
Procedure TALCustomListBox.InternaldoClick(Sender: TObject);
Begin
 If assigned(onClick) then Onclick(self);
end;

{*************************************************************}
Procedure TALCustomListBox.InternaldoDblClick(Sender: TObject);
Begin
 If assigned(onDblClick) then OnDblClick(self);
end;

{************************************************************************************}
Procedure TALCustomListBox.InternaldoDragDrop(Sender, Source: TObject; X, Y: Integer);
Begin
 If assigned(OnDragDrop) then begin
   If sender=FlistBox then sender := Self;
   If source=FlistBox then source := Self;
   OnDragDrop(Sender, Source, X, Y);
 end;
end;

{****************************************************************************************************************************}
Procedure TALCustomListBox.InternaldoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
Begin
 If assigned(OnDragOver) then begin
   If sender=FlistBox then sender := Self;
   If source=FlistBox then source := Self;
   OnDragOver(Sender, Source, X, Y, State, Accept)
 end
 else Accept := False;
end;

{***********************************************************************************}
Procedure TALCustomListBox.InternaldoEndDrag(Sender, Target: TObject; X, Y: Integer);
Begin
 If assigned(OnEndDrag) then begin
   If sender=FlistBox then sender := Self;
   If Target=FlistBox then Target := Self;
   OnEndDrag(Sender, Target, X, Y);
 end;
end;

{***********************************************************************************************}
Procedure TALCustomListBox.InternaldoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
 If assigned(OnKeyDown) then OnKeyDown(self, Key, Shift);
end;

{****************************************************************************}
Procedure TALCustomListBox.InternaldoKeyPress(Sender: TObject; var Key: Char);
Begin
 If assigned(OnKeyPress) then OnKeyPress(self, Key);
end;

{*********************************************************************************************}
Procedure TALCustomListBox.InternaldoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
 If assigned(OnKeyUp) then OnKeyUp(self, Key, Shift);
end;

{***********************************************************************************************************************}
Procedure TALCustomListBox.InternaldoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
 If assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X, Y);
end;

{*********************************************************************************************************************}
Procedure TALCustomListBox.InternaldoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
 If assigned(OnMouseUp) then OnMouseUp(Self, Button, Shift, X, Y);
end;

{*************************************************************************************************}
Procedure TALCustomListBox.InternaldoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Begin
 If assigned(OnMouseMove) then OnMouseMove(Self, Shift, X, Y);
end;

{*******************************************************************************************}
Procedure TALCustomListBox.InternaldoStartDrag(Sender: TObject; var DragObject: TDragObject);
Begin
 If assigned(OnStartDrag) then OnStartDrag(Self, DragObject);
end;

{***********************************************}
Function TALCustomListBox.GetItemHeight: Integer;
Begin
  IF not FlistBox.HandleAllocated then FlistBox.HandleNeeded;
  Result := FListBox.ItemHeight;
end;

{******************************************************************************}
Function TALCustomListBox.CalculateIntegralHeight(FromHeight: Integer): Integer;
Begin
  IF not FlistBox.HandleAllocated then FlistBox.HandleNeeded;
  If style in [lbOwnerDrawFixed, lbStandard] then Result := (((FromHeight - 2) div GetItemHeight) * GetItemHeight) + 2
  else result := Fromheight;
end;

{******************************************************************}
procedure TALCustomListBox.CMMouseWheel(var Message: TCMMouseWheel);
begin
  If not FVertScrollBar.Enabled then Inherited
  else If (TCMMouseWheel(message).wheelDelta) < 0 then FvertScrollBar.Position := FvertScrollBar.Position + 3
  else FvertScrollBar.Position := FvertScrollBar.Position - 3
end;

{*******************************************************}
Procedure TALCustomListBox.itemsChanged(Sender: Tobject);

  {-------------------------------}
  Function LastItemHeight: Integer;
  Var aindex, aItemHeight, aTotalItemHeight: Integer;
  Begin
    Result := 0;
    aTotalItemHeight := 0;
    For aindex := FlistBox.items.Count - 1 downto 0 do begin
      aItemHeight := SendMessage(FlistBox.Handle,LB_GETITEMHEIGHT,aIndex,0);
      If aTotalItemHeight + aItemHeight <= FlistBox.height then begin
        aTotalItemHeight := aTotalItemHeight + aItemHeight;
        inc(result);
      end
      else Break;
    end;
  end;

  {---------------------------------}
  Function EnabledScrollBar: Boolean;
  Var aindex, aItemHeight, aTotalItemHeight: Integer;
  Begin
    Result := False;
    aTotalItemHeight := 0;
    For aindex := 0 to FlistBox.items.Count - 1 do begin
      aItemHeight := SendMessage(FlistBox.Handle,LB_GETITEMHEIGHT,aIndex,0);
      If aTotalItemHeight + aItemHeight <= FlistBox.height then aTotalItemHeight := aTotalItemHeight + aItemHeight
      else begin
        Result := True;
        Break;
      end;
    end;
  end;

Begin
  If not EnabledScrollBar then FVertScrollBar.enabled := False
  else begin
    FVertScrollBar.Max := FlistBox.items.Count - LastItemHeight;
    FVertScrollBar.enabled := True;
  end;
end;

{***********************************************************}
Procedure TALCustomListBox.itemIndexChanged(Sender: Tobject);
Begin
  If assigned(FonItemIndexChange) then FonItemIndexChange(self);
end;

{*********************************************************************************************************}
Procedure TALCustomListBox.ScrollBarMove(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
Begin
  If ScrollCode = ScPosition then SendMessage(FlistBox.handle, WM_VScroll,MakeLParam(SB_THUMBPOSITION, Scrollpos), 0)
end;

{*******************************************************************************************************}
Procedure TALCustomListBox.ListBoxMove(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
Var aTopIndex: Integer;
Begin
  If ScrollCode = ScPosition then begin
    aTopIndex := SendMessage(FlistBox.Handle,LB_GETTOPINDEX,0,0);
    FVertScrollBar.position := aTopIndex;
  end;
end;

{******************************************************}
constructor TALCustomListBox.Create(AOwner: TComponent);
Begin
  inherited;
  FReadOnly := False;
  Color := ClWhite;
  Width := 121;
  Height := 97;
  FcustomProperty := '';
  FOnDrawItem:= nil;
  FOnMeasureItem:= nil;
  FOnData:= nil;
  FOnDataFind:= nil;
  FOnDataObject:= nil;


  FIntegralHeight := False;
  FVertScrollBar:= TALScrollBar.create(Self);
  FVertScrollBar.parent := Self;
  FListBox:= TALNoScrollBarListBox.create(Self);
  FListBox.parent := self;

    ParentCtl3d := False;
    Ctl3d:=False;
    BevelInner := bvNone;
    BevelKind := BkNone;
    BevelOuter := BVNone;
    BorderStyle := BsNone;
    BorderColor := ClBlack;
    FmouseIsDown:= False;
    FmouseInControl := False;

 FVertScrollBarProperty:= TALListBoxScrollBarProperty.create(FVertScrollBar);
 FlistBox.onItemschange := ItemsChanged;
 FlistBox.onItemIndexchange := ItemIndexChanged;
 FlistBox.OnClick := InternaldoClick;
 FlistBox.OnDblClick := InternaldoDblClick;
 FlistBox.OnDragDrop := InternaldoDragDrop;
 FlistBox.OnDragOver := InternaldoDragOver;
 FlistBox.OnEndDrag := InternaldoEndDrag;
 FlistBox.OnKeyDown := InternaldoKeyDown;
 FlistBox.OnKeyPress := InternaldoKeyPress;
 FlistBox.OnKeyUp := InternaldoKeyUp;
 FlistBox.onMouseDown := InternaldoMouseDown;
 FlistBox.onMouseUp := InternaldoMouseUp;
 FlistBox.onMouseMove := InternaldoMouseMove;
 FlistBox.OnStartDrag := InternaldoStartDrag;
 FVertScrollBar.OnScroll := ScrollBarMove;
 FListBox.OnScroll := ListBoxMove;
 FListBox.DragMode := DragMode;
 FListBox.DragKind := DragKind;
 FListBox.DragMode := DragMode;
end;

{**********************************}
Destructor TALCustomListBox.Destroy;
Begin
  FVertScrollBar.free;
  FVertScrollBarProperty.free;
  FlistBox.free;
  inherited;
end;

{********************************************************************************************************}
procedure TALCustomListBox.PaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
var C: TControlCanvas;
Begin
  If assigned(FonPaintScrollBar) then FOnPaintScrollBar(Self, continue, Area);
  If not continue then exit;

  If not FVertScrollBar.Enabled then Begin
    C:= TControlCanvas.Create;
    Try
      with C do begin
        control := FVertScrollBar;
        Pen.Color := BorderColor;
        MoveTo(0,0);
        LineTo(FVertscrollBar.ClientWidth-1,0);
        LineTo(FVertscrollBar.ClientWidth-1,FVertscrollBar.Clientheight-1);
        LineTo(-1,FVertscrollBar.Clientheight-1);
      end;
    Finally
      C.free;
    end;
  end;

End;

{**************************************}
Procedure TALCustomListBox.PaintListBox;
var Continue : Boolean;
    C: TControlCanvas;
begin
  continue := true;
  If assigned(FonPaint) then FOnPaint(Self, continue);
  if not continue then exit;

  C:= TControlCanvas.Create;
  Try
    c.control := Self;
    c.Brush.Color := BorderColor;
    c.FrameRect(ClientRect);
  Finally
    C.free;
  end;

  FVertScrollBar.paint;
end;

{********************************************************}
procedure TALCustomListBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    {-------------------}
    WM_LButtonDown: begin
                      inherited;
                      If not (csDesigning in ComponentState) then begin
                        FMouseIsDown:= true;
                        PaintListBox;
                      end;
                    end;
    {-----------------}
    WM_LButtonUp: begin
                    inherited;
                    If FMouseIsDown then begin
                      FMouseIsDown:= False;
                      PaintListBox;
                    end;
                   end;
    {---------------------}
    WM_LButtonDblClk: begin
                        inherited;
                        FMouseIsDown:= true;
                        PaintListBox;
                      end;
    {------------------}
    CM_MouseEnter: Begin
                     inherited;
                     If not mouseInControl then begin
                       FmouseInControl := true;
                       PaintListBox;
                     end;
                   End;
    {------------------}
    CM_MouseLeave: Begin
                    Inherited;
                    If ptinrect(ClientRect,screentoclient(Mouse.CursorPos)) then exit;
                    FmouseInControl := False;
                    PaintListBox;
                   End;
    {-------------}
    WM_Paint: Begin
                inherited;
                PaintListBox;
              end;
    {------------}
    WM_Size: begin
             inherited;
             Resize;
            end;
    {-------------}
    CM_Enter: begin
                inherited;
                PaintListBox;
              end;
    {------------}
    CM_Exit: begin
               inherited;
               If FMouseIsDown then FMouseIsDown := False;
               PaintListBox;
             end;
    {-------------------}
    CM_FontChanged: begin
                     inherited;
                     If (not ((csLoading) in ComponentState)) and FIntegralHeight then Height := CalculateIntegralHeight(height);
                    end;
     {-------------}
     else inherited;
  end;
end;

{*************************************************************************}
procedure TALCustomListBox.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);

  With FVertScrollBar do begin
    OnPaint := PaintScrollBar;
    Kind := sbVertical;
    Width := 16;
    Top := 0;
    Left := parent.Width-width;
    Height := Parent.Height;
    anchors := [AkTop,AkRight,AkBottom]
  end;

  With FListBox do begin
    Left := 1;
    Top := 1;
    Width := Parent.Width - FVertScrollBar.Width - 1;
    Height := Parent.Height - 2;
    anchors := [akleft,AkTop,AkRight, AkBottom];
    Ctl3d:=False;
    BevelInner := bvNone;
    BevelKind := BkNone;
    BevelOuter := BVNone;
    BorderStyle := BsNone;
    ParentBiDiMode := True;
    ParentColor := True;
    ParentFont := True;
    ParentShowHint := True;
  end;

  If FIntegralHeight then Height := CalculateIntegralHeight(height);

  ItemsChanged(Self);
end;

{********************************}
procedure TALCustomListBox.Loaded;
begin
  Inherited Loaded;
  FVertScrollBar.ButtonWidth := 18;
end;

{*****************************************************************************}
function TALCustomListBox.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  If FIntegralHeight then NewHeight := CalculateIntegralHeight(NewHeight);
  Result := inherited CanResize(NewWidth, NewHeight);
end;

{*******************************************************}
procedure TALCustomListBox.SetDragMode(Value: TDragMode);
Begin
  Inherited;
  FListBox.DragMode := Value;
end;

{*******************************************************}
procedure TALCustomListBox.SetDragKind(Value: TDragKind);
Begin
  inherited DragKind := Value;
  FlistBox.DragKind := Value;
end;

{*******************************************************}
procedure TALCustomListBox.SetDragCursor(Value: TCursor);
Begin
  inherited DragCursor := Value;
  FlistBox.DragCursor := Value;
end;

{***********************************************}
function TALCustomListBox.GetDragKind: TDragKind;
Begin
  Result := inherited dragKind;
End;

{***********************************************}
function TALCustomListBox.GetDragCursor: TCursor;
Begin
  Result := inherited DragCursor;
End;

{***************************************************}
procedure TALCustomListBox.SetItems(Value: TStrings);
Begin
  FlistBox.Items.Assign(Value);
End;

{*******************************************}
Function TALCustomListBox.GetItems: Tstrings;
Begin
  Result := FlistBox.items;
End;

{*************************************************}
Function TALCustomListBox.GetAutocomplete: Boolean;
Begin
  Result := FlistBox.Autocomplete;
End;

{*********************************************************}
Procedure TALCustomListBox.SetAutoComplete(Value: Boolean);
Begin
  FlistBox.Autocomplete := value;
End;

{***************************************************}
Function TALCustomListBox.GetExtendedSelect: Boolean;
Begin
  Result := FlistBox.ExtendedSelect;
End;

{***********************************************************}
Procedure TALCustomListBox.SetExtendedSelect(Value: Boolean);
Begin
  FlistBox.ExtendedSelect := value;
End;

{***************************************************}
Function TALCustomListBox.GetIntegralHeight: Boolean;
Begin
  Result := FIntegralHeight;
End;

{***********************************************************}
Procedure TALCustomListBox.SetIntegralHeight(Value: Boolean);
Begin
  If FIntegralHeight <> Value then Begin
    FIntegraLheight := Value;
    If FIntegralHeight and (not ((csLoading) in ComponentState)) Then Height := CalculateIntegralHeight(height);
  end;
End;

{************************************************}
Function TALCustomListBox.GetMultiSelect: Boolean;
Begin
  Result := FlistBox.MultiSelect;
End;

{********************************************************}
Procedure TALCustomListBox.SetMultiSelect(Value: Boolean);
Begin
  FlistBox.MultiSelect := value;
End;

{*******************************************}
Function TALCustomListBox.GetSorted: Boolean;
Begin
  Result := FlistBox.Sorted;
End;

{***************************************************}
Procedure TALCustomListBox.SetSorted(Value: Boolean);
Begin
  FlistBox.Sorted := value;
End;

{*********************************************}
Function TALCustomListBox.GetTabWidth: Integer;
Begin
  Result := FlistBox.TabWidth;
End;

{*****************************************************}
Procedure TALCustomListBox.SetTabWidth(Value: Integer);
Begin
  FlistBox.TabWidth := value;
End;

{******************************************}
Function TALCustomListBox.Getcount: Integer;
Begin
  Result := FlistBox.count;
End;

{**************************************************}
Procedure TALCustomListBox.SetCount(Value: Integer);
Begin
  FlistBox.count := value;
End;

{*************************************************************}
function TALCustomListBox.GetSelected(Index: Integer): Boolean;
Begin
  Result := FlistBox.Selected[index];
End;

{*********************************************************************}
procedure TALCustomListBox.SetSelected(Index: Integer; Value: Boolean);
Begin
  FlistBox.Selected[index] := value;
End;

{*********************************************}
Function TALCustomListBox.GetTopIndex: Integer;
Begin
  Result := FlistBox.TopIndex;
End;

{*****************************************************}
Procedure TALCustomListBox.SetTopIndex(Value: Integer);
Begin
  FlistBox.TopIndex := value;
End;

{**********************************************}
Function TALCustomListBox.GetItemIndex: Integer;
Begin
  Result := FlistBox.ItemIndex;
End;

{******************************************************}
Procedure TALCustomListBox.SetItemIndex(Value: Integer);
Begin
  FlistBox.ItemIndex := value;
End;

{*****************************************************************}
procedure TALCustomListBox.AddItem(Item: String; AObject: TObject);
Begin
  FlistBox.AddItem(Item, AObject);
end;

{*******************************}
procedure TALCustomListBox.Clear;
Begin
  FlistBox.clear;
end;

{****************************************}
procedure TALCustomListBox.ClearSelection;
begin
  FlistBox.clearSelection;
end;

{****************************************}
procedure TALCustomListBox.DeleteSelected;
Begin
  FlistBox.deleteSelected;
end;

{***************************************************************************}
function TALCustomListBox.ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
Begin
  Result := FlistBox.ItemAtPos(Pos,Existing)
End;

{********************************************************}
function TALCustomListBox.ItemRect(Index: Integer): TRect;
Begin
  Result := FlistBox.ItemRect(Index);
End;

{***********************************}
procedure TALCustomListBox.SelectAll;
Begin
  FlistBox.SelectAll;
End;

{*****************************************}
Function TALCustomListBox.Focused: Boolean;
Begin
  Result := FVertscrollBar.Focused or FlistBox.Focused;
End;

{************************************************}
function TALCustomListBox.GetStyle: TListBoxStyle;
begin
  Result := FlistBox.Style;
end;

{**************************************************************}
procedure TALCustomListBox.SetStyle(const Value: TListBoxStyle);
begin
  If FlistBox.Style <> value then begin
    If Value in [lbOwnerDrawFixed, lbOwnerDrawVariable, lbStandard] then FlistBox.Style := Value;
    If FintegralHeight then Height := CalculateIntegralHeight(height);
  end;  
end;

{*************************************************************}
procedure TALCustomListBox.SetItemHeight(const Value: Integer);
begin
  FlistBox.ItemHeight := Value;
  If FIntegralHeight then Height := CalculateIntegralHeight(height);
end;

{*******************************************}
function TALCustomListBox.GetCanvas: TCanvas;
begin
  Result := FlistBox.Canvas;
end;

{*****************************************************************}
procedure TALCustomListBox.SetOnData(const Value: TLBGetDataEvent);
begin
  FOnData := Value;
  If assigned(Value) then FlistBox.OnData := InternalDoData
  else FlistBox.OnData := nil;

end;

{**********************************************************************}
procedure TALCustomListBox.SetOnDataFind(const Value: TLBFindDataEvent);
begin
  FOnDataFind := Value;
  If assigned(Value) then FlistBox.OnDataFind := InternalDoDataFind
  else FlistBox.OnDataFind := nil;
end;

{*****************************************************************************}
procedure TALCustomListBox.SetOnDataObject(const Value: TLBGetDataObjectEvent);
begin
  FOnDataObject := Value;
  If assigned(Value) then FlistBox.OnDataObject := InternalDoDataObject
  else FlistBox.OnDataObject := nil;
end;

{********************************************************************}
procedure TALCustomListBox.SetOnDrawItem(const Value: TDrawItemEvent);
begin
  FOnDrawItem := Value;
  If assigned(Value) then FlistBox.OnDrawItem := InternalDoDrawItem
  else FlistBox.OnDrawItem := nil;
end;

{**************************************************************************}
procedure TALCustomListBox.SetOnMeasureItem(const Value: TMeasureItemEvent);
begin
  FOnMeasureItem := Value;
  If assigned(Value) then FlistBox.OnMeasureItem := InternalDoMeasureItem
  else FlistBox.OnMeasureItem := nil;
end;

{**********************************************************************************************************}
procedure TALCustomListBox.InternalDoMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
Begin
  If assigned(FOnMeasureItem) then Begin
    If Control=FlistBox then Control := Self;
    FOnMeasureItem(Control,Index,Height);
  end;
end;

{***********************************************************************************************************************}
procedure TALCustomListBox.InternalDoDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
Begin
  If assigned(FOnDrawItem) then begin
    If Control=FlistBox then Control := Self;
    FOnDrawItem(Control,Index,Rect,State);
  end;
end;

{*************************************************************************************************************}
procedure TALCustomListBox.InternalDoDataObject(Control: TWinControl; Index: Integer; var DataObject: TObject);
Begin
  If assigned(FOnDataObject) then begin
    If Control=FlistBox then Control := Self;
    FOnDataObject(Control,Index,DataObject);
  end;
end;

{**********************************************************************************************}
Function TALCustomListBox.InternalDoDataFind(Control: TWinControl; FindString: string): Integer;
Begin
  If assigned(FOnDataFind) then Begin
    If Control=FlistBox then Control := Self;
    result := FOnDataFind(Control,FindString)
  end
  else result := -1;
end;

{************************************************************************************************}
procedure TALCustomListBox.InternalDoData(Control: TWinControl; Index: Integer; var Data: string);
Begin
  If assigned(FOnData) then begin
    If Control=FlistBox then Control := Self;
    FOnData(Control,Index, Data);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
///////////// TAlKeyCustomListBox  /////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{*****************************************************}
function TALKeyListBox.IndexOfKey(key: String):integer;
begin
   Result := FitemsKey.indexOf(key);
end;

{***************************************************}
constructor TALKeyListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemsKey := TStringList.Create;
end;

{*******************************}
destructor TALKeyListBox.Destroy;
begin
  FItemsKey.Free;
  inherited Destroy;
end;

{****************************************************}
{make this way to permit to have the index in itemKey}
{for the onmeasureitem Event                         }
Procedure TALKeyListBox.AddItemKey(Value,key: String);
Var P,H: Integer;
    OldOnMeasureItem: TMeasureItemEvent;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  OldOnMeasureItem := FlistBox.OnMeasureItem;
  FlistBox.OnMeasureItem := nil;
  P := SendMessage(FListBox.Handle, LB_ADDSTRING, 0, Longint(PChar(Value)));
  if P < 0 then raise EOutOfResources.Create(SInsertLineError);
  ItemsKey.Insert(P,Key);
  if assigned(OldOnMeasureItem) then begin
    h := SendMessage(FListBox.Handle, LB_GETITEMHEIGHT, P, 0);
    OldOnMeasureItem(self,P,h);
    SendMessage(FListBox.Handle, LB_SETITEMHEIGHT, P, MAKELPARAM(h,0));
  end;
  FlistBox.ItemsChanged;
  FlistBox.OnMeasureItem := OldOnMeasureItem;
end;

{*********************************************************}
procedure TALKeyListBox.SetItemsKey(const Value: TStrings);
begin
  FItemsKey.Assign(Value)
end;

{*************************************}
procedure TALKeyListBox.DeleteSelected;
var I: Integer;
begin
  if MultiSelect then begin
    for I := Items.Count - 1 downto 0 do
      if Selected[I] then begin
        ItemsKey.Delete(I);
        Items.Delete(I);
      end;
  end
  else if ItemIndex <> -1 then begin
    ItemsKey.Delete(ItemIndex);
    Items.Delete(ItemIndex);
  end;
end;

{****************************}
Procedure TAlKeyListBox.Clear;
begin
  Inherited;
  ItemsKey.clear;
end;

end.

