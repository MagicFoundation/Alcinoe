unit Unit1;

interface

uses
  Vcl.StdCtrls,
  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  VCL.Forms,
  Alcinoe.MongoDB.Client,
  Alcinoe.JSONDoc,
  Vcl.ExtCtrls,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ToolsPanel: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    FindDatabaseNameEdit: TEdit;
    FindCollectionNameEdit: TEdit;
    FindFilterMemo: TMemo;
    FindOptsMemo: TMemo;
    FindButton: TButton;
    InfoPanel: TPanel;
    Label7: TLabel;
    FindResultMemo: TMemo;
    Panel1: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    ChangeStreamDatabaseNameEdit: TEdit;
    ChangeStreamCollectionNameEdit: TEdit;
    ChangeStreamPipelineMemo: TMemo;
    ChangeStreamOptsMemo: TMemo;
    ChangeStreamStartButton: TButton;
    ChangeStreamResultMemo: TMemo;
    Panel2: TPanel;
    Label12: TLabel;
    TabSheet3: TTabSheet;
    Panel3: TPanel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    FindAndModifyDatabaseNameEdit: TEdit;
    FindAndModifyCollectionNameEdit: TEdit;
    FindAndModifyQueryMemo: TMemo;
    FindAndModifyButton: TButton;
    Panel4: TPanel;
    Label18: TLabel;
    FindAndModifyResultMemo: TMemo;
    Label17: TLabel;
    FindAndModifySortMemo: TMemo;
    Label19: TLabel;
    FindAndModifyUpdateMemo: TMemo;
    FindAndModifyFieldsMemo: TMemo;
    Label20: TLabel;
    FindAndModifyRemoveCheckBox: TCheckBox;
    FindAndModifyUpsertCheckBox: TCheckBox;
    FindAndModifyNewCheckBox: TCheckBox;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    Panel7: TPanel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    InsertDatabaseNameEdit: TEdit;
    InsertCollectionNameEdit: TEdit;
    InsertDocumentsMemo: TMemo;
    InsertOptsMemo: TMemo;
    InsertOneButton: TButton;
    Panel8: TPanel;
    Label33: TLabel;
    InsertResultMemo: TMemo;
    InsertManyButton: TButton;
    Panel5: TPanel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    ReplaceDatabaseNameEdit: TEdit;
    ReplaceCollectionNameEdit: TEdit;
    ReplaceSelectorMemo: TMemo;
    ReplaceOneButton: TButton;
    ReplaceReplacementMemo: TMemo;
    ReplaceOptsMemo: TMemo;
    Panel9: TPanel;
    Label34: TLabel;
    ReplaceResultMemo: TMemo;
    Panel6: TPanel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    UpdateDatabaseNameEdit: TEdit;
    UpdateCollectionNameEdit: TEdit;
    UpdateSelectorMemo: TMemo;
    UpdateOneButton: TButton;
    UpdateUpdateMemo: TMemo;
    UpdateOptsMemo: TMemo;
    UpdateManyButton: TButton;
    Panel10: TPanel;
    Label46: TLabel;
    UpdateResultMemo: TMemo;
    Panel11: TPanel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    DeleteDatabaseNameEdit: TEdit;
    DeleteCollectionNameEdit: TEdit;
    DeleteSelectorMemo: TMemo;
    DeleteOptsMemo: TMemo;
    DeleteOneButton: TButton;
    Panel12: TPanel;
    Label45: TLabel;
    DeleteResultMemo: TMemo;
    DeleteManyButton: TButton;
    Panel13: TPanel;
    Label47: TLabel;
    UriEdit: TEdit;
    StartTransactionButton: TButton;
    CommitTransactionButton: TButton;
    AbortTransactionButton: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FindButtonClick(Sender: TObject);
    procedure ChangeStreamStartButtonClick(Sender: TObject);
    procedure UriEditChange(Sender: TObject);
    procedure FindAndModifyButtonClick(Sender: TObject);
    procedure UpdateOneButtonClick(Sender: TObject);
    procedure UpdateManyButtonClick(Sender: TObject);
    procedure InsertOneButtonClick(Sender: TObject);
    procedure InsertManyButtonClick(Sender: TObject);
    procedure ReplaceOneButtonClick(Sender: TObject);
    procedure DeleteOneButtonClick(Sender: TObject);
    procedure DeleteManyButtonClick(Sender: TObject);
    procedure StartTransactionButtonClick(Sender: TObject);
    procedure CommitTransactionButtonClick(Sender: TObject);
    procedure AbortTransactionButtonClick(Sender: TObject);
  private
    FMongoDBClient: TALMongoDBClient;
    FMongoDBChangeStreamListener: TALMongoDBChangeStreamListener;
    procedure OnChangeStreamChange(Sender: TObject; const AChangeDoc: TALJsonNodeA);
    procedure OnChangeStreamError(Sender: TObject; const AError: Exception);
  end;

var
  Form1: TForm1;

implementation

Uses
  System.UITypes,
  VCL.Dialogs,
  Alcinoe.FileUtils,
  Alcinoe.StringUtils,
  Alcinoe.Common,
  Alcinoe.MongoDB.Wrapper;

{$R *.DFM}

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
    {$IF defined(Win64)}
  ALCreateALMongoDBLibrary(ALGetModulePathW + '..\..\..\..\Libraries\dll\MongoDB\Win64');
  {$ELSE}
  ALCreateALMongoDBLibrary(ALGetModulePathW + '..\..\..\..\Libraries\dll\MongoDB\Win32');
  {$ENDIF}
  FMongoDBClient := nil;
  FMongoDBChangeStreamListener := nil;
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  ALFreeAndNil(FMongoDBClient);
  ALFreeAndNil(FMongoDBChangeStreamListener);
  ALFreeALMongoDBLibrary;
end;

{*************************************************************************************}
procedure TForm1.OnChangeStreamChange(Sender: TObject; const AChangeDoc: TALJsonNodeA);
begin
  var LJsonStr: AnsiString;
  AChangeDoc.SaveToJSONString(LJsonStr, [TALJSONSaveOption.soNodeAutoIndent]);
  if ChangeStreamResultMemo.Text <> '' then
    LJsonStr := #13#10 +
                #13#10 +
                '--------------------' +
                #13#10 +
                #13#10 +
                LJsonStr;
  TThread.Queue(nil,
    Procedure
    begin
      ChangeStreamResultMemo.Text := ALTrim(ChangeStreamResultMemo.Text + String(LJsonStr));
    end);
end;

{*****************************************************************************}
procedure TForm1.OnChangeStreamError(Sender: TObject; const AError: Exception);
begin
  var LErrorMessage := AError.Message;
  if ChangeStreamResultMemo.Text <> '' then
    LErrorMessage := #13#10 +
                     #13#10 +
                     '--------------------' +
                     #13#10 +
                     #13#10 +
                     LErrorMessage;
  TThread.Queue(nil,
    Procedure
    begin
      ChangeStreamResultMemo.Text := ALTrim(ChangeStreamResultMemo.Text + LErrorMessage);
    end);
end;

{**********************************************}
procedure TForm1.UriEditChange(Sender: TObject);
begin
  ALFreeAndNil(FMongoDBClient);
end;

{*************************************************************}
procedure TForm1.ChangeStreamStartButtonClick(Sender: TObject);
begin
  if ChangeStreamStartButton.Tag = 0 then begin
    FMongoDBChangeStreamListener := TALMongoDBChangeStreamListener.Create;
    FMongoDBChangeStreamListener.Uri := AnsiString(UriEdit.Text);
    FMongoDBChangeStreamListener.DatabaseName := AnsiString(ChangeStreamDataBaseNameEdit.Text);
    FMongoDBChangeStreamListener.CollectionName := AnsiString(ChangeStreamCollectionNameEdit.Text);
    FMongoDBChangeStreamListener.Pipeline := AnsiString(ChangeStreamPipelineMemo.Text);
    FMongoDBChangeStreamListener.Opts := AnsiString(ChangeStreamOptsMemo.Text);
    FMongoDBChangeStreamListener.OnChange := OnChangeStreamChange;
    FMongoDBChangeStreamListener.OnError := OnChangeStreamError;
    FMongoDBChangeStreamListener.Start;
    ChangeStreamStartButton.Tag := 1;
    ChangeStreamStartButton.Caption := 'Stop';
  end
  else begin
    ALFreeAndNil(FMongoDBChangeStreamListener);
    ChangeStreamStartButton.Tag := 0;
    ChangeStreamStartButton.Caption := 'Start';
  end;
end;

{************************************************}
procedure TForm1.FindButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then
    FMongoDBClient := TALMongoDBClient.Create(AnsiString(UriEdit.Text));
  var LJsonNode := FMongoDBClient.Find(
                     AnsiString(FindDataBaseNameEdit.Text), // const ADatabaseName: AnsiString;
                     AnsiString(FindCollectionNameEdit.Text), // const ACollectionName: AnsiString;
                     AnsiString(FindfilterMemo.Text), // const Afilter: AnsiString;
                     AnsiString(FindOptsMemo.Text)); // const AOpts: AnsiString = '';
  Try
    var LJsonStr: AnsiString;
    LJsonNode.SaveToJSONString(LJsonStr, [TALJSONSaveOption.soNodeAutoIndent]);
    FindResultMemo.Text := String(LJsonStr);
  Finally
    ALFreeAndNil(LJsonNode);
  End;
end;

{*********************************************************}
procedure TForm1.FindAndModifyButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then
    FMongoDBClient := TALMongoDBClient.Create(AnsiString(UriEdit.Text));
  var LJsonNode := FMongoDBClient.FindAndModify(
                     AnsiString(FindAndModifyDataBaseNameEdit.Text), // const ADatabaseName: AnsiString;
                     AnsiString(FindAndModifyCollectionNameEdit.Text), // const ACollectionName: AnsiString;
                     AnsiString(FindAndModifyQueryMemo.Text), // const AQuery: AnsiString;
                     AnsiString(FindAndModifySortMemo.Text), // const ASort: AnsiString;
                     AnsiString(FindAndModifyUpdateMemo.Text), // const AUpdate: AnsiString;
                     AnsiString(FindAndModifyFieldsMemo.Text), // const AFields: AnsiString;
                     FindAndModifyRemoveCheckBox.Checked, // const ARemove: ByteBool;
                     FindAndModifyUpsertCheckBox.Checked, // const AUpsert: ByteBool;
                     FindAndModifyNewCheckBox.Checked); // const ANew: ByteBool;
  Try
    var LJsonStr: AnsiString;
    LJsonNode.SaveToJSONString(LJsonStr, [TALJSONSaveOption.soNodeAutoIndent]);
    FindAndModifyResultMemo.Text := String(LJsonStr);
  Finally
    ALFreeAndNil(LJsonNode);
  End;
end;

{*****************************************************}
procedure TForm1.UpdateOneButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then
    FMongoDBClient := TALMongoDBClient.Create(AnsiString(UriEdit.Text));
  var LJsonNode := FMongoDBClient.UpdateOne(
                     AnsiString(UpdateDataBaseNameEdit.Text), // const ADatabaseName: AnsiString;
                     AnsiString(UpdateCollectionNameEdit.Text), // const ACollectionName: AnsiString;
                     AnsiString(UpdateSelectorMemo.Text), // const ASelector: AnsiString;
                     AnsiString(UpdateUpdateMemo.Text), // const AUpdate: AnsiString;
                     AnsiString(UpdateOptsMemo.Text)); // const AOpts: AnsiString;
  Try
    var LJsonStr: AnsiString;
    LJsonNode.SaveToJSONString(LJsonStr, [TALJSONSaveOption.soNodeAutoIndent]);
    UpdateResultMemo.Text := String(LJsonStr);
  Finally
    ALFreeAndNil(LJsonNode);
  End;
end;

{******************************************************}
procedure TForm1.UpdateManyButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then
    FMongoDBClient := TALMongoDBClient.Create(AnsiString(UriEdit.Text));
  var LJsonNode := FMongoDBClient.UpdateMany(
                     AnsiString(UpdateDataBaseNameEdit.Text), // const ADatabaseName: AnsiString;
                     AnsiString(UpdateCollectionNameEdit.Text), // const ACollectionName: AnsiString;
                     AnsiString(UpdateSelectorMemo.Text), // const ASelector: AnsiString;
                     AnsiString(UpdateUpdateMemo.Text), // const AUpdate: AnsiString;
                     AnsiString(UpdateOptsMemo.Text)); // const AOpts: AnsiString;
  Try
    var LJsonStr: AnsiString;
    LJsonNode.SaveToJSONString(LJsonStr, [TALJSONSaveOption.soNodeAutoIndent]);
    UpdateResultMemo.Text := String(LJsonStr);
  Finally
    ALFreeAndNil(LJsonNode);
  End;
end;

{*****************************************************}
procedure TForm1.InsertOneButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then
    FMongoDBClient := TALMongoDBClient.Create(AnsiString(UriEdit.Text));
  var LJsonNode := FMongoDBClient.InsertOne(
                     AnsiString(InsertDataBaseNameEdit.Text), // const ADatabaseName: AnsiString;
                     AnsiString(InsertCollectionNameEdit.Text), // const ACollectionName: AnsiString;
                     AnsiString(InsertDocumentsMemo.Text), // const ADocument: AnsiString;
                     AnsiString(InsertOptsMemo.Text)); // const AOpts: AnsiString;
  Try
    var LJsonStr: AnsiString;
    LJsonNode.SaveToJSONString(LJsonStr, [TALJSONSaveOption.soNodeAutoIndent]);
    InsertResultMemo.Text := String(LJsonStr);
  Finally
    ALFreeAndNil(LJsonNode);
  End;
end;

{******************************************************}
procedure TForm1.InsertManyButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then
    FMongoDBClient := TALMongoDBClient.Create(AnsiString(UriEdit.Text));
  var LDocuments: TArray<AnsiString>;
  Setlength(LDocuments, 2);
  LDocuments[0] := AnsiString(InsertDocumentsMemo.Text);
  LDocuments[1] := AnsiString(InsertDocumentsMemo.Text);
  var LJsonNode := FMongoDBClient.InsertMany(
                     AnsiString(InsertDataBaseNameEdit.Text), // const ADatabaseName: AnsiString;
                     AnsiString(InsertCollectionNameEdit.Text), // const ACollectionName: AnsiString;
                     LDocuments, // const ADocuments: TArray<AnsiString>;
                     AnsiString(InsertOptsMemo.Text)); // const AOpts: AnsiString;
  Try
    var LJsonStr: AnsiString;
    LJsonNode.SaveToJSONString(LJsonStr, [TALJSONSaveOption.soNodeAutoIndent]);
    InsertResultMemo.Text := String(LJsonStr);
  Finally
    ALFreeAndNil(LJsonNode);
  End;
end;

{******************************************************}
procedure TForm1.ReplaceOneButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then
    FMongoDBClient := TALMongoDBClient.Create(AnsiString(UriEdit.Text));
  var LJsonNode := FMongoDBClient.ReplaceOne(
                     AnsiString(ReplaceDataBaseNameEdit.Text), // const ADatabaseName: AnsiString;
                     AnsiString(ReplaceCollectionNameEdit.Text), // const ACollectionName: AnsiString;
                     AnsiString(ReplaceSelectorMemo.Text),// const ASelector: AnsiString;
                     AnsiString(ReplaceReplacementMemo.Text),// const AReplacement: AnsiString;
                     AnsiString(ReplaceOptsMemo.Text)); // const AOpts: AnsiString;
  Try
    var LJsonStr: AnsiString;
    LJsonNode.SaveToJSONString(LJsonStr, [TALJSONSaveOption.soNodeAutoIndent]);
    ReplaceResultMemo.Text := String(LJsonStr);
  Finally
    ALFreeAndNil(LJsonNode);
  End;
end;

{*****************************************************}
procedure TForm1.DeleteOneButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then
    FMongoDBClient := TALMongoDBClient.Create(AnsiString(UriEdit.Text));
  var LJsonNode := FMongoDBClient.DeleteOne(
                     AnsiString(DeleteDataBaseNameEdit.Text), // const ADatabaseName: AnsiString;
                     AnsiString(DeleteCollectionNameEdit.Text), // const ACollectionName: AnsiString;
                     AnsiString(DeleteSelectorMemo.Text), // const ASelector: AnsiString;
                     AnsiString(DeleteOptsMemo.Text)); // const AOpts: AnsiString;
  Try
    var LJsonStr: AnsiString;
    LJsonNode.SaveToJSONString(LJsonStr, [TALJSONSaveOption.soNodeAutoIndent]);
    DeleteResultMemo.Text := String(LJsonStr);
  Finally
    ALFreeAndNil(LJsonNode);
  End;
end;

{******************************************************}
procedure TForm1.DeleteManyButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then
    FMongoDBClient := TALMongoDBClient.Create(AnsiString(UriEdit.Text));
  var LJsonNode := FMongoDBClient.DeleteMany(
                     AnsiString(DeleteDataBaseNameEdit.Text), // const ADatabaseName: AnsiString;
                     AnsiString(DeleteCollectionNameEdit.Text), // const ACollectionName: AnsiString;
                     AnsiString(DeleteSelectorMemo.Text), // const ASelector: AnsiString;
                     AnsiString(DeleteOptsMemo.Text)); // const AOpts: AnsiString;
  Try
    var LJsonStr: AnsiString;
    LJsonNode.SaveToJSONString(LJsonStr, [TALJSONSaveOption.soNodeAutoIndent]);
    DeleteResultMemo.Text := String(LJsonStr);
  Finally
    ALFreeAndNil(LJsonNode);
  End;
end;

{************************************************************}
procedure TForm1.StartTransactionButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then
    FMongoDBClient := TALMongoDBClient.Create(AnsiString(UriEdit.Text));
  FMongoDBClient.StartTransaction;
end;

{*************************************************************}
procedure TForm1.CommitTransactionButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then raise Exception.Create('No active transaction');
  var LJsonNode := FMongoDBClient.CommitTransaction;
  try
    var LJsonStr: AnsiString;
    LJsonNode.SaveToJSONString(LJsonStr, [TALJSONSaveOption.soNodeAutoIndent]);
    MessageDlg(String(LJsonStr), mtConfirmation, [mbOK], 0);
  finally
    ALFreeAndNil(LJsonNode);
  end;
end;

{************************************************************}
procedure TForm1.AbortTransactionButtonClick(Sender: TObject);
begin
  if FMongoDBClient = nil then exit;
  FMongoDBClient.AbortTransaction;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.