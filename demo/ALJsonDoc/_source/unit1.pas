unit Unit1;

interface

uses Windows,
     Messages,
     SysUtils,
     Classes,
     Graphics,
     Controls,
     Forms,
     StdCtrls,
     ALString,
     ALJsonDoc,
     ExtCtrls,
     OleCtrls,
     SHDocVw,
     ComObj,
     ALStringList;

type

  TForm1 = class(TForm)
    ButtonLoadXmlWithALXmlDocument: TButton;
    MemoLoadJsonDocument: TMemo;
    ButtonCreateDynamicallyJsonDocument: TButton;
    MemoCreateDynamicallyJsonDocument: TMemo;
    ButtonSaveToBson: TButton;
    Panel1: TPanel;
    Label8: TLabel;
    Label12: TLabel;
    Panel2: TPanel;
    PanelWebBrowser: TPanel;
    MemoLoadJsonDocumentSAXMODEResult: TMemo;
    MemoBSON1: TMemo;
    Label1: TLabel;
    MemoBSON2: TMemo;
    Button1: TButton;
    procedure ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
    procedure ButtonCreateDynamicallyJsonDocumentClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveToBsonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{********************************************************************}
procedure TForm1.ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
Var aALJsonDocument: TALJsonDocument;
begin

  //clear MemoLoadJsonDocumentSAXMODEResult
  MemoLoadJsonDocumentSAXMODEResult.Lines.Clear;

  //exemple 1 load the JSON doc in memory
  aALJsonDocument := TALJsonDocument.Create;
  try
    aALJsonDocument.LoadFromJSON(AnsiString(MemoLoadJsonDocument.Lines.Text));
    aALJsonDocument.Options := [doNodeAutoIndent];
    MemoLoadJsonDocument.Lines.Text := String(aALJsonDocument.JSON);
  finally
    aALJsonDocument.Free;
  end;

  //exemple 2 load the JSON doc in SAX MODE
  aALJsonDocument := TALJsonDocument.Create;
  try
    aALJsonDocument.onParseText := procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const str: AnsiString; const NodeSubType: TALJSONNodeSubType)
                                   begin
                                     MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + String(str));
                                   end;
    aALJsonDocument.LoadFromJSON(AnsiString(MemoLoadJsonDocument.Lines.Text), true{saxMode});
  finally
    aALJsonDocument.Free;
  end;

end;

{*******************************************************************************}
procedure TForm1.ButtonCreateDynamicallyJsonDocumentClick(Sender: TObject);
Var aALJsonDocument: TALJsonDocument;
begin

  aALJsonDocument:= TALJsonDocument.Create(true);
  Try

    aALJsonDocument.addchild('_id').float := 1.32;
    with aALJsonDocument.addchild('name', ntObject) do begin
      addchild('first').text := 'John';
      addchild('last').text := 'Backus';
    end;
    aALJsonDocument.addchild('birth').datetime := Now;
    with aALJsonDocument.addchild('contribs', ntArray) do begin
      addchild.text := 'Fortran';
      addchild.text := 'ALGOL';
      addchild.text := 'Backus-Naur Form';
      addchild.text := 'FP';
    end;
    with aALJsonDocument.addchild('awards', ntArray) do begin
      with addchild(ntObject) do begin
        addchild('award').text := 'National Medal of Science';
        addchild('year').int32 := 1975;
        addchild('by').text := 'National Science Foundation';
      end;
      with addchild(ntObject) do begin
        addchild('award').text := 'Turing Award';
        addchild('year').int32 := 1977;
        addchild('by').text := 'ACM';
      end;
    end;
    aALJsonDocument.addchild('spouse');
    aALJsonDocument.addchild('address', ntObject);
    aALJsonDocument.addchild('phones', ntArray);

    aALJsonDocument.Options := [doNodeAutoIndent];
    MemoCreateDynamicallyJsonDocument.Lines.Text := String(aALJsonDocument.JSON);

  finally
    aALJsonDocument.Free;
  end;

end;

{******************************************************}
procedure TForm1.ButtonSaveToBsonClick(Sender: TObject);
Var aALJsonDocument: TALJsonDocument;
    aBsonStr: AnsiString;
    i: integer;
begin

  //clear MemoLoadJsonDocumentSAXMODEResult
  MemoBSON1.Lines.Clear;
  MemoBSON2.Lines.Clear;

  aALJsonDocument:= TALJsonDocument.Create(true);
  Try

    aALJsonDocument.addchild('_id').float := 1.32;
    with aALJsonDocument.addchild('name', ntObject) do begin
      addchild('first').text := 'John';
      addchild('last').text := 'Backus';
    end;
    aALJsonDocument.addchild('birth').datetime := Now;
    with aALJsonDocument.addchild('contribs', ntArray) do begin
      addchild.text := 'Fortran';
      addchild.text := 'ALGOL';
      addchild.text := 'Backus-Naur Form';
      addchild.text := 'FP';
    end;
    with aALJsonDocument.addchild('awards', ntArray) do begin
      with addchild(ntObject) do begin
        addchild('award').text := 'National Medal of Science';
        addchild('year').int32 := 1975;
        addchild('by').text := 'National Science Foundation';
      end;
      with addchild(ntObject) do begin
        addchild('award').text := 'Turing Award';
        addchild('year').int32 := 1977;
        addchild('by').text := 'ACM';
      end;
    end;
    aALJsonDocument.addchild('spouse').NodeSubType := nstText;
    aALJsonDocument.addchild('address', ntObject);
    aALJsonDocument.addchild('phones', ntArray);

    aALJsonDocument.Options := [doNodeAutoIndent];
    MemoBSON1.Lines.Text := String(aALJsonDocument.JSON);
    aBsonStr := aALJsonDocument.BSON;
    for I := 1 to length(aBsonStr) do
      MemoBSON2.Lines.add(Inttostr(ord(aBsonStr[i])));

  finally
    aALJsonDocument.Free;
  end;

end;

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);
var aBsonStr: AnsiString;
    aALJsonDocument: TALJsonDocument;
    i: integer;
begin
  aBsonStr := '';
  for I := 0 to MemoBSON2.Lines.Count - 1 do
    aBsonStr := aBsonStr + AnsiChar(StrToInt(MemoBSON2.Lines[i]));
  MemoBSON1.Lines.Clear;

  //exemple 1 load the JSON doc in memory
  aALJsonDocument := TALJsonDocument.Create;
  try
    aALJsonDocument.LoadFromBSON(aBsonStr);
    aALJsonDocument.Options := [doNodeAutoIndent];
    MemoBSON1.Lines.Text := String(aALJsonDocument.JSON);
  finally
    aALJsonDocument.Free;
  end;

  //exemple 2 load the JSON doc in SAX MODE
  MemoLoadJsonDocumentSAXMODEResult.Lines.Clear;
  aALJsonDocument := TALJsonDocument.Create;
  try
    aALJsonDocument.onParseText := procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const str: AnsiString; const NodeSubType: TALJSONNodeSubType)
                                   begin
                                     MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + String(str));
                                   end;
    aALJsonDocument.LoadFromBSON(aBsonStr, true{saxMode});
  finally
    aALJsonDocument.Free;
  end;

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
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  ie.Width := 100;
  ie.Height := 300;
  Url := 'http://static.arkadia.com/html/alcinoe_like.html';
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

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
