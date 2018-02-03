unit ReadMailDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, JclMapi;

type
  TForm1 = class(TForm)
    HeadersListView: TListView;
    PreviewRichEdit: TRichEdit;
    Splitter1: TSplitter;
    AttachmentsListBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HeadersListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure HeadersListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    Email: TJclEmail;
  public
    procedure PrevievMessage(const SeedMessageID: string);
    procedure ReadHeaders;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  Mapi, // MAPI_UNREAD constant
  JclSysUtils;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Email := TJclEmail.Create;
  ReadHeaders;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Email);
end;

procedure TForm1.PrevievMessage(const SeedMessageID: string);
var
  HeaderLinesCount: Integer;
begin
  with PreviewRichEdit do
  begin
    Lines.BeginUpdate;
    try
      Lines.Clear;
      // Set SeedMessageID before reading
      Email.SeedMessageID := AnsiString(SeedMessageID);
      Email.Read;
      HeaderLinesCount := Email.MessageReport(Lines);
      // Message header part highlighting
      SelStart := 0;
      SelLength := SendMessage(Handle, EM_LINEINDEX, HeaderLinesCount, 0);
      SelAttributes.Style := [fsBold];
      SelLength := 0;
      SelStart := 0;
    finally
      Lines.EndUpdate;
    end;
  end;
  AttachmentsListBox.Items.Assign(Email.Attachments);
  AttachmentsListBox.Visible := AttachmentsListBox.Items.Count > 0;
end;

procedure TForm1.ReadHeaders;
var
  NextMessage: Boolean;
begin
  // You have to be logged on before reading messages. LogOff is automatically
  // called in TJclEmail destructor.
  Email.LogOn;

  // SimpleMAPI is limited to read messages from InBox root folder only
  HeadersListView.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    HeadersListView.Items.Clear;
    NextMessage := Email.FindFirstMessage;
    while NextMessage do
    begin
      Email.Read([roHeaderOnly]);
      with HeadersListView.Items.Add do
      begin
        Caption := string(Email.Recipients.Originator.Name);
        SubItems.Add(string(Email.Subject));
        SubItems.Add(DateTimeToStr(Email.ReadMsg.DateReceived));
        SubItems.Add(string(Email.SeedMessageID));
        Data := Pointer(Email.ReadMsg.Flags); // store Flags for custom draw
      end;
      NextMessage := Email.FindNextMessage;
    end;
  finally
    HeadersListView.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.HeadersListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    PrevievMessage(Item.SubItems[2]);
end;

procedure TForm1.HeadersListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if DWORD(Item.Data) and MAPI_UNREAD <> 0 then
    Sender.Canvas.Font.Style := [fsBold];
end;

end.
