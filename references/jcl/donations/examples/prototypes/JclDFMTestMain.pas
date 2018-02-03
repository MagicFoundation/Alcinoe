unit JclDFMTestMain;

{$I jcl.inc}

interface

uses
  SysUtils, Classes, JclDFM, JclDFMUtils,
{$IFDEF VCL}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls;
{$ENDIF VCL}
{$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QComCtrls, QExtCtrls;
{$ENDIF VisualCLX}  


type
  TfmJclDFMTest = class(TForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PageControl1: TPageControl;
    tsTV: TTabSheet;
    tsComponents: TTabSheet;
    tsSkipAndReplaceList: TTabSheet;
    Memo1: TMemo;
    tvDFMTree: TTreeView;
    memSkipProperties: TMemo;
    tsImage: TTabSheet;
    Image: TImage;
    tsLayout: TTabSheet;
    tsTVItems: TTabSheet;
    tvItems: TTreeView;
    tsTranslate: TTabSheet;
    Panel5: TPanel;
    Label1: TLabel;
    btnDFM2Tree: TButton;
    Panel6: TPanel;
    Label2: TLabel;
    btnDFMGetComps: TButton;
    Panel7: TPanel;
    Label3: TLabel;
    Panel8: TPanel;
    Label4: TLabel;
    btnCleanDFM: TButton;
    btnExtractImageLists: TButton;
    Panel9: TPanel;
    Label5: TLabel;
    btnLoadLayout: TButton;
    Panel10: TPanel;
    Label6: TLabel;
    btnLoadTreeViewItems: TButton;
    memPropertyReplaceList: TMemo;
    Panel1: TPanel;
    Panel11: TPanel;
    pnlLayout: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel12: TPanel;
    Label7: TLabel;
    btnTranslate: TButton;
    procedure btnDFM2TreeClick(Sender: TObject);
    procedure btnDFMGetCompsClick(Sender: TObject);
    procedure btnCleanDFMClick(Sender: TObject);
    procedure btnExtractImageListsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tsImageResize(Sender: TObject);
    procedure btnLoadLayoutClick(Sender: TObject);
    procedure btnLoadTreeViewItemsClick(Sender: TObject);
    procedure btnTranslateClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FAppliedNewLayout: Boolean;
  public
    { Public-Deklarationen }
  end;

var
  fmJclDFMTest: TfmJclDFMTest;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

procedure AddDFMObjectToTree(ATreeView: TTreeView; ANode: TTreeNode; ADFMObj: TJclDFMComponent);
var
  I: Integer;
  an, an2: TTreeNode;
  S: string;
  PropertyValueString: string;
begin
  an := ATreeView.Items.AddChild(ANode, ADFMObj.ComponentName + ':' +
    ADFMObj.ComponentClassName);
  if ADFMObj.Properties.Count > 0 then
  begin
    an2 := ATreeView.Items.AddChild(an, 'Properties');
    for I := 0 to ADFMObj.Properties.Count - 1 do
    begin
      PropertyValueString := ADFMObj.Properties[I].AsString;
      if Length(PropertyValueString) > 100 then
        PropertyValueString := Copy(PropertyValueString, 1, 100) + '...';
      S := Format('=%s', [PropertyValueString]);
      ATreeView.Items.AddChild(an2, ADFMObj.Properties[I].Name + ' ' +
        ValueTypeToString(ADFMObj.Properties[I].Typ) + S);
    end;
  end;
  if ADFMObj.SubComponents.Count > 0 then
  begin
    an2 := ATreeView.Items.AddChild(an, 'SubComponents');
    for I := 0 to ADFMObj.SubComponents.Count - 1 do
      AddDFMObjectToTree(ATreeView, an2, ADFMObj.SubComponents[I]);
  end;
end;

procedure DrawImageListToCanvasFromDFMComponent(ADFMComponent: TJclDFMComponent;
  ACanvas: TCanvas; Ax, Ay: Integer);
var
  BitmapList: TList;
  I: Integer;
  Bitmap: TBitmap;
  cx: Integer;
begin
  BitmapList := TList.Create;
  try
    ExtractImageListToBitmapsFromDFMComponent(BitmapList, ADFMComponent);
    if BitmapList.Count > 0 then
    begin
      cx := 0;
      for I := 0 to BitmapList.Count - 1 do
      begin
        Bitmap := BitmapList[I];
        if Assigned(Bitmap) then
        begin
          ACanvas.Draw(Ax + cx, Ay, Bitmap);
          Inc(cx, Bitmap.Width);
          Bitmap.Free;
        end;
      end;
    end;
  finally
    BitmapList.Free;
  end;
end;

procedure TfmJclDFMTest.btnDFM2TreeClick(Sender: TObject);
var
  RComp: TJclDFMRootComponent;
begin
  if OpenDialog.Execute then
  begin
    RComp := TJclDFMRootComponent.Create;
    try
      RComp.LoadFromFile(OpenDialog.FileName);
      AddDFMObjectToTree(tvDFMTree, nil, RComp);
    finally
      RComp.Free;
    end;
    PageControl1.ActivePage := tsTV;
  end;
end;

procedure TfmJclDFMTest.btnDFMGetCompsClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    DFMGetAllComponentTypes(OpenDialog.FileName, Memo1.Lines);
    PageControl1.ActivePage := tsComponents;
  end;
end;

procedure TfmJclDFMTest.btnCleanDFMClick(Sender: TObject);
var
  RComp: TJclDFMRootComponent;
begin
  if OpenDialog.Execute then
  begin
    RComp := TJclDFMRootComponent.Create;
    try
      RComp.LoadFromFile(OpenDialog.FileName);
      DFMRemoveUnwantedComponentsAndProps(RComp, nil, memSkipProperties.Lines);
      DFMReplacePropertyValues(RComp, memPropertyReplaceList.Lines); 
      SaveDialog.FileName := ChangeFileExt(OpenDialog.FileName, '.cleaned_dfm');
      if SaveDialog.Execute then
        RComp.SaveToFile(SaveDialog.FileName);
    finally
      RComp.Free;
    end;
  end;
end;

procedure TfmJclDFMTest.btnExtractImageListsClick(Sender: TObject);
var
  RComp: TJclDFMRootComponent;
  ComponentList: TList;
  I, J, imgcy, tw, imgh: Integer;
  S: string;
begin
  if OpenDialog.Execute then
  begin
    Image.Canvas.FillRect(Rect(0,0,Image.Width, Image.Height));
    RComp := TJclDFMRootComponent.Create;
    try
      RComp.LoadFromFile(OpenDialog.FileName);
      ComponentList := TList.Create;
      try
        if RComp.FindComponentsByClass('TImageList', ComponentList) > 0 then
        begin
          imgcy := 2;
          for I := 0 to ComponentList.Count - 1 do
            with TJclDFMComponent(ComponentList[I]) do
            begin
              S := Format('%s.%s', [RComp.ComponentClassName, ComponentName]);
              with Image.Canvas do
              begin
                tw := TextWidth(S);
                TextOut(2, imgcy, S);
              end;
              DrawImageListToCanvasFromDFMComponent(ComponentList[I],
                Image.Canvas, tw + 7, imgcy);
              imgh := 16;
              for J := 0 to Properties.Count - 1 do
                if SameText(Properties[J].Name, 'Height') then
                begin
                  imgh := Properties[J].AsInteger;
                  Break;
                end;
              Inc(imgcy, imgh + 4);
            end;
        end;
      finally
        ComponentList.Free;
      end;
    finally
      RComp.Free;
    end;
    PageControl1.ActivePage := tsImage;
  end;
end;

procedure TfmJclDFMTest.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := tsTV;
  FAppliedNewLayout := False;
end;

procedure TfmJclDFMTest.tsImageResize(Sender: TObject);
begin
  Image.Picture.Bitmap.Width := Image.Width;
  Image.Picture.Bitmap.Height := Image.Height;
end;

procedure TfmJclDFMTest.btnLoadLayoutClick(Sender: TObject);
var
  LayoutFile: string;
begin
  PageControl1.ActivePage := tsLayout;
  if FAppliedNewLayout then
    ShowMessage('The new layout is still applied !')
  else
  begin
    LayoutFile := ExtractFilePath(Application.ExeName) + 'LoadLayout.partial_dfm';
    if FileExists(LayoutFile) then
    begin
      ShowMessage('Old Layout');
      LoadLayout(pnlLayout, LayoutFile);
      ShowMessage('New Layout');
      FAppliedNewLayout := True;
    end
    else
      ShowMessage(Format('Could not find %s', [LayoutFile]));
  end;
end;

procedure TfmJclDFMTest.btnLoadTreeViewItemsClick(Sender: TObject);
var
  RComp: TJclDFMRootComponent;
  ComponentList: TList;
begin
  if OpenDialog.Execute then
  begin
    RComp := TJclDFMRootComponent.Create;
    try
      RComp.LoadFromFile(OpenDialog.FileName);
      ComponentList := TList.Create;
      try
        if RComp.FindComponentsByClass('TTreeView', ComponentList) > 0 then
          ReadTreeViewItemsFromDFMComponent(tvItems, ComponentList[0]);
      finally
        ComponentList.Free;
      end;
    finally
      RComp.Free;
    end;
    PageControl1.ActivePage := tsTVItems;
  end;
end;

function ReverseString(AString: string): string;
var
  I: Integer;
begin
  Result := '';
  if AString <> '' then
    for I := Length(AString) downto 1 do
      Result := Result + AString[I];
end;

procedure ReverseAllCaptionsAndHints(ADFMComponent: TJclDFMComponent);

  procedure ReplaceInProperties(AProperties: TJclDFMProperties);
  var
    I, J: Integer;
    DFMCollection: TJclDFMCollectionProperty;
  begin
    for I := AProperties.Count - 1 downto 0 do
    begin
      if (AProperties[I].Name = 'Caption') or (AProperties[I].Name = 'Hint') then
      begin
        if not (AProperties[I].Typ in [vaWString {$IFDEF DELPHI6_UP}, vaUTF8String {$ENDIF}]) then
          AProperties[I].AsString := ReverseString(AProperties[I].AsString)
        else
          AProperties[I].AsWideString := ReverseString(AProperties[I].AsWideString);
      end
      else if AProperties[I].Typ = vaCollection then
      begin
        DFMCollection := AProperties[I].AsCollectionProperty;
        for J := 0 to DFMCollection.Count - 1 do
          ReplaceInProperties(DFMCollection[J].Properties);
      end;
    end;
  end;

var
  I: Integer;
begin
  with ADFMComponent do
  begin
    ReplaceInProperties(Properties);
    for I := SubComponents.Count - 1 downto 0 do
      ReverseAllCaptionsAndHints(SubComponents[I]);
  end;
end;

procedure TfmJclDFMTest.btnTranslateClick(Sender: TObject);
var
  RComp: TJclDFMRootComponent;
begin
  if OpenDialog.Execute then
  begin
    RComp := TJclDFMRootComponent.Create;
    try
      RComp.LoadFromFile(OpenDialog.FileName);
      ReverseAllCaptionsAndHints(RComp);
      SaveDialog.FileName := ChangeFileExt(OpenDialog.FileName, '.reversed_dfm');
      if SaveDialog.Execute then
        RComp.SaveToFile(SaveDialog.FileName);
    finally
      RComp.Free;
    end;
  end;
end;

end.
