unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, AlScrollBar, ALMemo, ALButton,
  ALEdit, ALComboBox, OleCtrls, SHDocVw, ComObj;

type
  TForm1 = class(TForm)
    ALButton1: TALButton;
    ALButton2: TALButton;
    Memo_SQL: TALMemo;
    Label1: TLabel;
    Label4: TLabel;
    edt_DataBaseName: TALEdit;
    Label2: TLabel;
    Memo_DatabaseParams: TALMemo;
    ComboBox_apiVer: TALComboBox;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    Memo_SelectResult: TALMemo;
    Label6: TLabel;
    Panel1: TPanel;
    Label8: TLabel;
    Label12: TLabel;
    Panel2: TPanel;
    PanelWebBrowser: TPanel;
    procedure ALButton1Paint(Sender: TObject; var continue: Boolean);
    procedure Memo_SQLPaint(Sender: TObject; var continue: Boolean);
    procedure Memo_SQLPaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
    procedure ALEdit1Paint(Sender: TObject; var continue: Boolean);
    procedure ComboBox_apiVerPaint(Sender: TObject; var continue: Boolean);
    procedure edt_DataBaseNameButtonClick(Sender: TObject);
    procedure ALButton1Click(Sender: TObject);
    procedure ALButton2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
  public
    Function SelectData(Req: string; const TPB: string = ''): String;
    procedure UpdateData(Req: string; const TPB: string = '');
  end;

var Form1: TForm1;

implementation

uses alFcnSkin,
     AlFbxBase,
     AlFbxLib,
     alStringList,
     ALFcnHTML,
     alFcnString;

{$R *.dfm}

{**********************************************************************}
Function TForm1.SelectData(Req: string; const TPB: string = ''): String;
Const buffSize: integer = 16384;
Var ResultCurrentPos: Integer;
    ResultCurrentLength: integer;
    CurFieldTag : String;
    aDBHandle: IscDbHandle;
    aTraHandle: IscTrHandle;
    aLibrary: TALFBXLibrary;
    aSQLDIALECT: Word;
    aFBAPiVersion: TALFBXVersion_API;

    {-----------------------------------}
    Procedure MoveStr2Result(Src:String);
    Var l : integer;
    Begin
      L := Length(Src);
      If L+ResultCurrentPos-1>ResultCurrentLength Then begin
        ResultCurrentLength := L+ResultCurrentPos-1 + BuffSize;
        SetLength(Result,ResultCurrentLength);
      end;
      ALMove(Src[1],Result[ResultCurrentPos],L);
      ResultCurrentPos := ResultCurrentPos + L;
    end;

    {--------------------------}
    Procedure InternalExecQuery;
    Var i: integer;
        aStmtHandle: IscStmtHandle;
        aSqlda: TALFbxSQLResult;
    Begin
      aSqlda := TALFBXSQLResult.Create(ALFBXStrToCharacterSet(Memo_DatabaseParams.Lines.Values['lc_ctype']));
      Try
        aStmtHandle := nil;
        alibrary.DSQLAllocateStatement(aDBHandle, aStmtHandle);
        try
          alibrary.DSQLPrepare(aDBHandle, aTraHandle, aStmtHandle, Req, aSQLDIALECT, aSqlda);
          aLibrary.DSQLExecute(aTraHandle, aStmtHandle, aSQLDIALECT, nil);
          while alibrary.DSQLFetch(aDBHandle, aTraHandle, aStmtHandle, aSQLDIALECT, asqlda) do begin
            MoveStr2Result('<rec>');
            For i := 0 to asqlda.FieldCount - 1 do begin
              CurFieldTag := ALlowercase(asqlda.AliasName[i]);
              MoveStr2Result('<'+CurFieldTag+'>' + ALXMLTextElementEncode(asqlda.AsString[i]) + '</'+CurFieldTag+'>');
            end;
            MoveStr2Result('</rec>');
          end;
        finally
          alibrary.DSQLFreeStatement(aStmtHandle, DSQL_drop);
        end;
      finally
        aSqlda.free;
      end;
    end;
begin
  {init}
  aTraHandle := nil;
  aDBHandle := nil;
  aSQLDIALECT := 3;

  case ComboBox_apiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;    
    else aFBAPiVersion := FB102;
  end;

  aLibrary := TALFBXLibrary.Create(aFBAPiVersion);
  Try

    aLibrary.Load;
    aLibrary.AttachDatabase(edt_DataBaseName.text,
                            aDBHandle,
                            AlStringReplace(trim(Memo_DatabaseParams.lines.text),
                                            #13#10,
                                            ';',
                                            [rfReplaceAll]));
    try

      try

        {start the transaction}
        If TPB = '' then alibrary.TransactionStart(aTraHandle,
                                                   aDBHandle,
                                                   isc_tpb_version3 +
                                                   isc_tpb_read +
                                                   isc_tpb_concurrency +
                                                   isc_tpb_nowait)
        else alibrary.TransactionStart(aTraHandle,
                                       aDBHandle,
                                       TPB);

        {init}
        ResultCurrentLength := BuffSize;
        SetLength(Result,ResultCurrentLength);
        ResultCurrentPos := 1;
        MoveStr2Result('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'+#13#10+
                       '<root>');

        {exec the query}
        InternalExecQuery;

        {close the sql}
        MoveStr2Result('</root>');
        SetLength(Result,ResultCurrentPos-1);
        alibrary.TransactionCommit(aTraHandle);

      except
        On E: Exception do begin
          if aTraHandle <> nil then alibrary.TransactionRollback(aTraHandle);
          raise;
        end;
      end;

    Finally
      if aDBHandle <> nil then aLibrary.DetachDatabase(aDBHandle);
    end;

  finally
    aLibrary.free;
  end;
end;

{***************************************************************}
procedure TForm1.UpdateData(Req: string; const TPB: string = '');
Var aTraHandle: IscTrHandle;
    aDBHandle: IscDbHandle;
    aLibrary: TALFBXLibrary;
    aSQLDIALECT: Word;
    aFBAPiVersion: TALFBXVersion_API;

begin
  aTraHandle:= nil;
  aDBHandle := nil;
  aSQLDIALECT := 3;

  case ComboBox_apiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    else aFBAPiVersion := FB102;
  end;

  aLibrary := TALFBXLibrary.Create(aFBAPiVersion);
  Try

    aLibrary.Load;
    aLibrary.AttachDatabase(edt_DataBaseName.text,
                            aDBHandle,
                            AlStringReplace(trim(Memo_DatabaseParams.lines.text),
                                            #13#10,
                                            ';',
                                            [rfReplaceAll]));

    Try

      If TPB='' then alibrary.TransactionStart(aTraHandle,
                                               aDBHandle,
                                               isc_tpb_version3 +
                                               isc_tpb_write +
                                               isc_tpb_concurrency +
                                               isc_tpb_nowait)
      else alibrary.TransactionStart(aTraHandle,
                                     aDBHandle,
                                     TPB);

      Try

        alibrary.DSQLExecuteImmediate(aDBHandle, aTraHandle, Req, aSQLDIALECT, nil);
        alibrary.TransactionCommit(aTraHandle);

      except
        On E: Exception do begin
          if aTraHandle <> nil then alibrary.TransactionRollback(aTraHandle);
          raise;
        end;
      end;

    Finally
      if aDBHandle <> nil then aLibrary.DetachDatabase(aDBHandle);
    end;

  finally
    aLibrary.free;
  end;
end;

{**********************************************************************}
procedure TForm1.ALButton1Paint(Sender: TObject; var continue: Boolean);
begin
  PaintAlButtonBlueSkin(Sender, Continue);
end;

{********************************************************************}
procedure TForm1.Memo_SQLPaint(Sender: TObject; var continue: Boolean);
begin
  paintAlMemoBlueSkin(sender, Continue);
end;

{*****************************************************************************************************}
procedure TForm1.Memo_SQLPaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
begin
  paintAlMemoScrollBarBlueSkin(sender, Continue, area);
end;

{********************************************************************}
procedure TForm1.ALEdit1Paint(Sender: TObject; var continue: Boolean);
begin
  PaintAlEditBlueSkin(Sender, Continue);
end;

{****************************************************************************}
procedure TForm1.ComboBox_apiVerPaint(Sender: TObject; var continue: Boolean);
begin
  PaintAlComboBoxBlueSkin(Sender, Continue);
end;

{************************************************************}
procedure TForm1.edt_DataBaseNameButtonClick(Sender: TObject);
begin
  If OpenDialog1.Execute then edt_DataBaseName.Text := OpenDialog1.FileName;
end;

{***********************************************}
procedure TForm1.ALButton1Click(Sender: TObject);
begin
  Memo_SelectResult.Lines.Text := SelectData(Memo_SQL.Lines.Text);
end;

procedure TForm1.ALButton2Click(Sender: TObject);
begin
  UpdateData(Memo_SQL.Lines.Text);
  Memo_SelectResult.Lines.Text := 'DONE'
end;



{-------------------}
var ie: IWebBrowser2;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
var Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  ie := CreateOleObject('InternetExplorer.Application') as IWebBrowser2;
  SetWindowLong(ie.hwnd, GWL_STYLE, GetWindowLong(ie.hwnd, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME );
  SetWindowPos(ie.hwnd, HWND_TOP, Left, Top, Width, Height, SWP_FRAMECHANGED);
  windows.setparent(ie.hwnd, PanelWebBrowser.handle);
  ie.Left := maxint; // don't understand why it's look impossible to setup the position
  ie.Top  := maxint; // don't understand why it's look impossible to setup the position
  ie.Width := 100;
  ie.Height := 300;
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  Url := 'http://www.arkadia.com/html/alcinoe_like.html';
  ie.Navigate2(Url,Flags,TargetFrameName,PostData,Headers);
  ie.Visible := true;
end;

{********************************************************************}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    ie.quit;
  except
  end;
  sleep(500);
end;

{$IFDEF DEBUG}
initialization
  ReportMemoryleaksOnSHutdown := True;
{$ENDIF}

end.
