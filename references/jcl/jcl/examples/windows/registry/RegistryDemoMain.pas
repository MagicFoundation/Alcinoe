unit RegistryDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  JclRegistry, JclStrings;

type
  TForm1 = class(TForm)
    tvKeys: TTreeView;
    Splitter1: TSplitter;
    lvValues: TListView;
    procedure tvKeysExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure tvKeysChange(Sender: TObject; Node: TTreeNode);
  private
    function BuildPath(const Node: TTreeNode): string;
    procedure InitTree;
    function ExtractRootKey(const FullPath: string): string;
    function ExtractKey(const FullPath: string): string;
    function AddChildNode(const Node: TTreeNode; const Text: string): TTreeNode;
    procedure GetKeyInfos(const Node: TTreeNode; var RootKey: HKEY;
      var Key: string);
  public
  end;

var
  Form1: TForm1;

implementation

uses
  JclSysUtils;

{$R *.DFM}

procedure TForm1.InitTree;
begin
  tvKeys.Items.Clear;
  with tvKeys.Items.AddChild(nil, 'HKEY_CLASSES_ROOT') do
    HasChildren := true;

  with tvKeys.Items.AddChild(nil, 'HKEY_CURRENT_USER') do
    HasChildren := true;

  with tvKeys.Items.AddChild(nil, 'HKEY_LOCAL_MACHINE') do
    HasChildren := true;

  with tvKeys.Items.AddChild(nil, 'HKEY_USERS') do
    HasChildren := true;
    
end;

function TForm1.BuildPath(const Node: TTreeNode): string;
begin
  if Node <> nil then
    Result := BuildPath(Node.Parent) + Node.Text + '\'
  else
    Result := '';
end;

function TForm1.ExtractRootKey(const FullPath: string): string;
var
  strTmp: string;
begin
  strTmp := FullPath;
  Result := StrToken(strTmp, '\');
end;

function TForm1.ExtractKey(const FullPath: string): string;
var
  strTmp: string;
begin
  strTmp := FullPath;
  StrToken(strTmp, '\');
  Result := strTmp;
end;

procedure TForm1.GetKeyInfos(const Node: TTreeNode; var RootKey: HKEY; var Key: string);
var
  strTmp,
  strRootKey: string;
begin
  strTmp := BuildPath(Node);
  strRootKey := ExtractRootKey(strTmp);

  if strRootKey = 'HKEY_CLASSES_ROOT' then
    RootKey := HKEY_CLASSES_ROOT;
  if strRootKey = 'HKEY_CURRENT_USER' then
    RootKey := HKEY_CURRENT_USER;
  if strRootKey = 'HKEY_LOCAL_MACHINE' then
    RootKey := HKEY_LOCAL_MACHINE;
  if strRootKey = 'HKEY_USERS' then
    RootKey := HKEY_USERS;

  Key:= ExtractKey(strTmp);
end;

procedure TForm1.tvKeysExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var
  strTmp,
  Key: string;
  RootKey: HKEY;
  stlSubKeys: TStrings;
  i: Integer;
  NewNode: TTreeNode;
begin
  GetKeyInfos(Node, RootKey, Key);

  stlSubKeys := TStringList.Create;
  RegGetKeyNames(RootKey, Key, stlSubKeys);

  for i := 0 to stlSubKeys.Count - 1 do begin
    strTmp := stlSubKeys[i];
    NewNode := AddChildNode(Node, strTmp);
    if NewNode <> nil then
      NewNode.HasChildren := RegHasSubKeys(RootKey, Key + strTmp);
  end;

  stlSubKeys.Free;

end;

function TForm1.AddChildNode(const Node: TTreeNode; const Text: string): TTreeNode;
var
  i: integer;
  DoesExist: boolean;
begin
  DoesExist := false;
  Result := nil;
  
  for i := 0 to Node.Count - 1 do
    if Node.Item[i].Text = Text then begin
      DoesExist := true;
      break;
    end;

  if not DoesExist then
    Result := tvKeys.Items.AddChild(Node, Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitTree;
end;

procedure TForm1.tvKeysChange(Sender: TObject; Node: TTreeNode);
var
  strTmp,
  Key: string;
  RootKey: HKEY;
  stlValueNames: TStrings;
  i: integer;
begin
  lvValues.Items.Clear;
  GetKeyInfos(Node, RootKey, Key);

  stlValueNames := TStringList.Create;
  if RegGetValueNames(RootKey, Key, stlValueNames) then begin
    for i := 0 to stlValueNames.Count - 1 do begin
      strTmp := stlValueNames[i];
      with lvValues.Items.Add do begin
        Caption := strTmp;
        SubItems.Add(RegReadString(RootKey, Key, strTmp));
      end;
    end;
  end;

end;

end.
