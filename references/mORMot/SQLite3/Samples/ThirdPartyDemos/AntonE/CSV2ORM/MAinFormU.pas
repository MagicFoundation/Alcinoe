unit MAinFormU;

interface

{*****************************************}
{***  Please see ReadMe file           ***}
{***  No copyright/license             ***}
{***  No guarantees                    ***)
{***  Free to use/modify/distribute    ***)
(***  Created by Anton Ekermans        ***)
(***  antone@true.co.za                ***)
(***  Original :                       ***)
(***  ftp://ftp.true.co.za/CSV2ORM.zip ***)
{*****************************************}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.DBCtrls, Data.DB,
  Vcl.Grids, Vcl.DBGrids, JvExDBGrids, JvDBGrid, JvDBUltimGrid,
  Datasnap.DBClient, Vcl.StdCtrls, Vcl.Buttons, Vcl.Menus, Vcl.ComCtrls,
  JvExExtCtrls, JvExtComponent, JvCaptionPanel, JvExControls, JvDBLookup;

type
  TMainForm = class(TForm)
    CDS: TClientDataSet;
    CDSName: TStringField;
    CDSTipe: TStringField;
    CDSControl: TStringField;
    DS: TDataSource;
    FileOpenDialog1: TFileOpenDialog;
    FileSaveDialog1: TFileSaveDialog;
    PopupMenu1: TPopupMenu;
    ImportthisasNameType1: TMenuItem;
    CDSOrde: TIntegerField;
    CDSSize: TIntegerField;
    CDSField: TStringField;
    CDSControlName: TStringField;
    CDSFieldAs: TStringField;
    PageMain: TPageControl;
    TSProject: TTabSheet;
    TSTemplates: TTabSheet;
    Panel2: TPanel;
    JvDBUltimGrid1: TJvDBUltimGrid;
    DBMemo1: TDBMemo;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    DBNavigator1: TDBNavigator;
    Templates: TClientDataSet;
    DSTemplates: TDataSource;
    TemplatesName: TStringField;
    TemplatesFiles: TDataSetField;
    Files: TClientDataSet;
    FilesFileName: TStringField;
    FilesSource: TBlobField;
    DSFiles: TDataSource;
    Panel3: TPanel;
    BtnTemplSave: TBitBtn;
    PanelLeft: TPanel;
    PanelFiles: TJvCaptionPanel;
    PanelTemplates: TJvCaptionPanel;
    Splitter2: TSplitter;
    JvDBUltimGrid2: TJvDBUltimGrid;
    JvDBUltimGrid3: TJvDBUltimGrid;
    MemoSource: TDBMemo;
    BtnGenerate: TBitBtn;
    ComboTempl: TJvDBLookupCombo;
    CDSDBControl: TStringField;
    procedure BitBtn1Click(Sender: TObject);
    procedure ImportthisasNameType1Click(Sender: TObject);
    procedure BtnTemplSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnGenerateClick(Sender: TObject);
  private
    { Private declarations }
    procedure AnalyseData;
  public
    { Public declarations }
    ObjName:String;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses idGlobal;

procedure TMainForm.BitBtn1Click(Sender: TObject);
begin
 if FileOpenDialog1.Execute
    then {CDS.LoadFromFile(FileSaveDialog1.FileName);}
 ImportthisasNameType1Click(nil);
 BtnGenerate.Enabled:=True;
end;

procedure TMainForm.BtnGenerateClick(Sender: TObject);
var SS,
    SD,
    ST         : TStringList;
    FileName   : String;
    I,K,T      : Integer;
    S1,S2,S3   : String;
    MyTop      : Integer;
    MyTabOrder : Integer;
begin
 if CDS.RecordCount=0
    then begin
          ShowMessage('Import a CSV file first with MetaData.');
          exit;
         end;
 if ComboTempl.Text=''
    then begin
          ShowMEssage('Select a template first.');
          exit;
         end;

 SS:=TStringList.Create;
 SD:=TStringList.Create;
 ST:=TStringList.Create;
 Files.First;
 while not Files.EOF do
  begin
   FileName:=FilesFileName.AsString;
   FileName:=StringReplace(FileName,'MyObj',ObjName,[rfReplaceAll]);
   SS.Clear;
   SD.Clear;
   SS.Text:=MemoSource.Lines.Text;
   I:=0;
   while I<SS.Count do
    begin
     S1:=SS[I];
     if Pos('[Fields]',S1)>0
        then begin
              ST.Clear;
              ST.Add(S1);
              K:=I;
              Inc(I);
              while Pos('[/Fields]',S1)<=0 do
               begin
                try
                 S1:=SS[I];
                except
                 ShowMessage('Unresolved [Fields] loop on line '+IntToStr(K)+' in '+FilesFileName.AsString);
                 break;
                end;
                ST.Add(S1);
                Inc(I);
               end;

              CDS.First;
              while not CDS.EOF do
               begin
                for t := 0 to Pred(ST.Count) do
                 begin
                  S1:=ST[T];
                  if (Pos('[Size]',S1)>0)and (CDSSize.AsInteger=0)
                     then continue;
                  if (Pos('[!Size]',S1)>0)and (CDSSize.AsInteger>0)
                     then continue;
                  S2:=StringReplace(S1,'[Fields]','',[rfReplaceAll]);
                  S2:=StringReplace(S2,'[/Fields]','',[rfReplaceAll]);
                  S2:=StringReplace(S2,'[Size]','',[rfReplaceAll]);
                  S2:=StringReplace(S2,'[!Size]','',[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyObj',ObjName,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyType',CDSTipe.AsString,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyName',CDSName.AsString,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyFieldAs',CDSFieldAs.AsString,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyField',CDSField.AsString,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MySize',CDSSize.AsString,[rfReplaceAll]);
                  SD.Add(S2);
                 end;
                CDS.Next;
               end;
              Continue;
             end;
     if Pos('[Controls]',S1)>0
        then begin
              ST.Clear;
              ST.Add(S1);
              K:=I;
              Inc(I);
              while Pos('[/Controls]',S1)<=0 do
               begin
                try
                 S1:=SS[I];
                except
                 ShowMessage('Unresolved [Controls] loop on line '+IntToStr(K)+' in '+FilesFileName.AsString);
                 break;
                end;
                ST.Add(S1);
                Inc(I);
               end;

              MyTop:=21;
              MyTabOrder:=0;
              CDS.First;
              while not CDS.EOF do
               begin
                if CDSControl.AsString=''
                   then begin
                         CDS.Next;
                         continue;
                        end;
                for t := 0 to Pred(ST.Count) do
                 begin
                  S1:=ST[T];
                  if (Pos('[Size]',S1)>0)and (CDSSize.AsInteger=0)
                     then continue;
                  if (Pos('[!Size]',S1)>0)and (CDSSize.AsInteger>0)
                     then continue;
                  S2:=StringReplace(S1,'[Controls]','',[rfReplaceAll]);
                  S2:=StringReplace(S2,'[/Controls]','',[rfReplaceAll]);
                  S2:=StringReplace(S2,'[Size]','',[rfReplaceAll]);
                  S2:=StringReplace(S2,'[!Size]','',[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyObj',ObjName,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyType',CDSTipe.AsString,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyName',CDSName.AsString,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyFieldAs',CDSFieldAs.AsString,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyField',CDSField.AsString,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyControlName',CDSControlName.AsString,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyControl',CDSControl.AsString,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyDBControl',CDSDBControl.AsString,[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyTop',IntToStr(MyTop),[rfReplaceAll]);
                  S2:=StringReplace(S2,'MyTabOrder',IntToStr(MyTabORder),[rfReplaceAll]);
                  S2:=StringReplace(S2,'MySize',CDSSize.AsString,[rfReplaceAll]);
                  SD.Add(S2);
                 end;
                Inc(MyTop,21);
                Inc(MyTabOrder);
                CDS.Next;
               end;
              Continue;
             end;

     S1:=StringReplace(S1,'MyObj',ObjName,[rfReplaceAll]);
     S1:=StringReplace(S1,'MyType',CDSTipe.AsString,[rfReplaceAll]);
     SD.Add(S1);
     Inc(I);
    end;
   SD.SaveToFile(FileName);
   Files.Next;
  end;
end;

procedure TMainForm.AnalyseData;
var S1 : String;
begin
 CDS.First;
 while not CDS.EOF do
  begin
   (*CDS fields*)
   CDS.Edit;
   S1:='';
   CDSField.Clear;
   if CompareText(CDSTipe.AsString,'Integer')=0
      then begin
            CDSField.AsString:='TIntegerField';
            CDSFieldAs.AsString:='AsInteger';
           end;
   if CompareText(CDSTipe.AsString,'Int64')=0
      then begin
            CDSField.AsString:='TLargeIntField';
            CDSFieldAs.AsString:='AsLargeInt';
           end;
   if CompareText(CDSTipe.AsString,'RawUTF8')=0
      then begin
            CDSField.AsString:='TStringField';
            CDSFieldAs.AsString:='AsString';
           end;
   if CompareText(CDSTipe.AsString,'Boolean')=0
      then begin
            CDSField.AsString:='TBooleanField';
            CDSFieldAs.AsString:='AsBoolean';
           end;
   if CompareText(CDSTipe.AsString,'TDateTime')=0
      then begin
            CDSField.AsString:='TDateTimeField';
            CDSFieldAs.AsString:='AsDateTime';
           end;
   if CompareText(CDSTipe.AsString,'Double')=0
      then begin
            CDSField.AsString:='TFloatField';
            CDSFieldAs.AsString:='AsFloat';
           end;
   if CompareText(CDSTipe.AsString,'Currency')=0
      then begin
            CDSField.AsString:='TCurrencyField';
            CDSFieldAs.AsString:='AsCurrency';
           end;
   if CDSField.AsString=''
      then raise Exception.Create('Error Message 141 - Unknown type : '+CDSTipe.AsString);

   (*PAS UI controls*)
   CDSControlName.Clear;
   if (CDSControl.AsString='')or
      (CDSControl.AsString='-none-')
      then begin
            CDSControl.Clear;
            CDS.Post;
            CDS.Next;
            continue;
           end;
   S1:='';
   if CompareText(CDSControl.AsString,'Edit')=0
      then begin
            CDSControl  .AsString:='TEdit';
            CDSDBControl.AsString:='TDBEdit';
            CDSControlName.AsString:='Edit'+CDSName.AsString;
           end;
   if CompareText(CDSControl.AsString,'Combo')=0
      then begin
            CDSControl.AsString:='TComboBox';
            CDSDBControl.AsString:='TDBComboBox';
            CDSControlName.AsString:='Combo'+CDSName.AsString;
           end;
   if CompareText(CDSControl.AsString,'ComboBox')=0
      then begin
            CDSControl.AsString:='TComboBox';
            CDSDBControl.AsString:='TDBComboBox';
            CDSControlName.AsString:='Combo'+CDSName.AsString;
           end;
   if CompareText(CDSControl.AsString,'Radio')=0
      then begin
            CDSControl.AsString:='TRadioGroup';
            CDSDBControl.AsString:='TDBRadioGroup';
            CDSControlName.AsString:='Radio'+CDSName.AsString;
           end;
   if CompareText(CDSControl.AsString,'RadioGroup')=0
      then begin
            CDSControl.AsString:='TRadioGroup';
            CDSDBControl.AsString:='TDBRadioGroup';
            CDSControlName.AsString:='Radio'+CDSName.AsString;
           end;
   if CompareText(CDSControl.AsString,'RadioButton')=0
      then begin
            CDSControl.AsString:='TDBRadioGroup';
            CDSDBControl.AsString:='TRadioGroup';
            CDSControlName.AsString:='Radio'+CDSName.AsString;
           end;
   if CompareText(CDSControl.AsString,'Check')=0
      then begin
            CDSControl.AsString:='TCheckBox';
            CDSDBControl.AsString:='TDBCheckBox';
            CDSControlName.AsString:='Check'+CDSName.AsString;
           end;
   if CompareText(CDSControl.AsString,'CheckBox')=0
      then begin
            CDSControl.AsString:='TCheckBox';
            CDSDBControl.AsString:='TDBCheckBox';
            CDSControlName.AsString:='Check'+CDSName.AsString;
           end;
   if CompareText(CDSControl.AsString,'DateEdit')=0
      then begin
            CDSControl.AsString:='TJvDateEdit';
            CDSDBControl.AsString:='TJvDBDateEdit';
            CDSControlName.AsString:='Edit'+CDSName.AsString;
           end;
   if CDSControlName.AsString=''
      then raise Exception.Create('Error Message 170 - Unknown control : '+CDSControl.AsString);
   CDS.Post;
   CDS.Next;
  end;
end;

procedure TMainForm.BtnTemplSaveClick(Sender: TObject);
begin
 Templates.SaveToFile;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var I : Integer;
begin
 try
  Templates.LoadFromFile;
 except
  I:=0;
 end;
end;

procedure TMainForm.ImportthisasNameType1Click(Sender: TObject);
var SL,SL2     : TStringList;
    S1,
    S2,
    S3,
    S4     : String;
    I      : Integer;
begin
 CDS.EmptyDataSet;
 ObjName:=ExtractFileName(FileOpenDialog1.FileName);
 ObjName:=Fetch(ObjName,'.',True);

 SL:=TStringList.Create;
 SL2:=TStringList.Create;
 SL.LoadFromFile(FileOpenDialog1.FileName);
 I:=0;
 while SL.Count>0 do
  begin
   SL2.CommaText:=Trim(SL[0]);
   S1:=Trim(SL2[0]);
   S2:=Trim(SL2[1]);
   S3:=Trim(SL2[2]);
   if SL2.Count>3
      then S4:=Trim(SL2[3])
      else S4:='0';
   SL.Delete(0);
   if S1=''
      then break;
   CDS.Insert;
   CDSOrde   .AsInteger:=I;
   CDSName   .AsString :=S1;
   CDSTipe   .AsString :=S2;
   CDSControl.AsString :=S3;
   CDSSize   .AsString :=S4;
   CDS.Post;
   Inc(I);
  end;
 SL2.Free;
 SL .Free;
 AnalyseData;
end;

end.
