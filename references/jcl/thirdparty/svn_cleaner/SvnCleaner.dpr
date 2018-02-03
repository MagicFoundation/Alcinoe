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
{ The Original Code is SvnCleaner.dpr.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Subversion repository cleaner.                                                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

program SvnCleaner;

{$APPTYPE CONSOLE}

{$I jcl.inc}
{$I crossplatform.inc}

uses
  SysUtils,
  Classes,
  JclFileUtils,
  JclSimpleXml,
  JclSysUtils,
  JclStrings,
  JclStreams;

type
  TSvnProperty = record
    Name: string;
    Value: string;
  end;

  TSvnProperties = array of TSvnProperty;

  TSvnSetting = record
    Path: string;
    Masks: array of string;
    Recurse: Boolean;
    DirOnly: Boolean;
    Properties: TSvnProperties;
  end;

//=== { TSvnSettings } =======================================================

type
  TSvnSettings = class
  private
    FSettings: array of TSvnSetting;
    FRoot: string;
  public
    procedure LoadFromXml(XmlNode: TJclSimpleXMLElem);
    function GetProperties(const Path: string): TSvnProperties;
    property Root: string read FRoot;
  end;

function TSvnSettings.GetProperties(const Path: string): TSvnProperties;
var
  IndexSetting, IndexMask, IndexProperty, IndexCheck: Integer;
  RelPath: string;
  AddProperties: Boolean;
begin
  SetLength(Result, 0);
  for IndexSetting := Low(FSettings) to High(FSettings) do
  begin
    AddProperties := False;
    RelPath := PathGetRelativePath(FSettings[IndexSetting].Path, Path);
    if Pos('..', RelPath) = 0 then
    begin
      if Length(FSettings[IndexSetting].Masks) = 0 then
        AddProperties := (RelPath = '.') or FSettings[IndexSetting].Recurse
      else
      if RelPath <> '.' then
        if (Pos(DirDelimiter, RelPath) = 0) or FSettings[IndexSetting].Recurse then
          for IndexMask := Low(FSettings[IndexSetting].Masks) to High(FSettings[IndexSetting].Masks) do
            if StrMatches(FSettings[IndexSetting].Masks[IndexMask], RelPath) then
            begin
              AddProperties := True;
              Break;
            end;

      if (FileGetAttr(Path) and faDirectory) = 0 then
      begin
        AddProperties := AddProperties and not FSettings[IndexSetting].DirOnly;
      end;
        
      if AddProperties then
      begin
        for IndexProperty := Low(FSettings[IndexSetting].Properties) to High(FSettings[IndexSetting].Properties) do
        begin
          for IndexCheck := Low(Result) to High(Result) do
            if FSettings[IndexSetting].Properties[IndexProperty].Name = Result[IndexCheck].Name then
              raise Exception.CreateFmt('duplicate property "%s" for item "%s"', [Result[IndexCheck].Name, Path]);
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := FSettings[IndexSetting].Properties[IndexProperty];
        end;
      end;
    end;
  end;
end;

function FixEndOfLines(const Value: string): string;
var
  Lines: TStrings;
begin
  Lines := TStringList.Create;
  try
    StrToStrings(StrReplaceChar(Value, NativeLineFeed, NativeCarriageReturn), NativeCarriageReturn, Lines, False);
    Result := StringsToStr(Lines, NativeLineBreak, False);
  finally
    Lines.Free;
  end;
end;

procedure TSvnSettings.LoadFromXml(XmlNode: TJclSimpleXMLElem);
  function LoadProperty(Elem: TJclSimpleXmlElem): TSvnProperty;
  var
    NameProp: TJclSimpleXMLProp;
    ValueElem: TJclSimpleXMLElem;
    Index: Integer;
  begin
    NameProp := Elem.Properties.ItemNamed['name'];
    if not Assigned(NameProp) then
      raise Exception.Create('no name property');
    Result.Name := NameProp.Value;
    if Elem.Value <> '' then
      Result.Value := FixEndOfLines(Elem.Value)
    else
    begin
      Result.Value := '';
      for Index := 0 to Elem.Items.Count - 1 do
      begin
        ValueElem := Elem.Items.Item[Index];
        if ValueElem.Name = 'value' then
        begin
          if Result.Value = '' then
            Result.Value := ValueElem.Value
          else
            Result.Value := Result.Value + NativeLineBreak + ValueElem.Value;
        end
        else
        if ValueElem.Name <> '' then
          raise Exception.CreateFmt('unknown item "%s"', [ValueElem.Name]);
      end;
      Result.Value := FixEndOfLines(Result.Value);
    end;
  end;

  procedure LoadSetting(Elem: TJclSimpleXMLElem; const Path: string);
  var
    PathProp, MaskProp, RecurseProp, DirOnlyProp: TJclSimpleXMLProp;
    SubElem: TJclSimpleXMLElem;
    Index: Integer;
    MyPath: string;
    MySetting: TSvnSetting;
    Masks: TStrings;
  begin
    PathProp := Elem.Properties.ItemNamed['path'];
    if not Assigned(PathProp) then
      raise Exception.Create('no path property');
    MaskProp := Elem.Properties.ItemNamed['mask'];
    if not Assigned(MaskProp) then
      raise Exception.Create('no mask prop');
    RecurseProp := Elem.Properties.ItemNamed['recurse'];
    if not Assigned(RecurseProp) then
      raise Exception.Create('no recurse prop');
    DirOnlyProp := Elem.Properties.ItemNamed['dironly'];
    if not Assigned(DirOnlyProp) then
      raise Exception.Create('no dironly prop');
    {$IFDEF MSWINDOWS}
    MyPath := StringReplace(PathProp.Value, '/', DirDelimiter, [rfReplaceAll]);
    {$ELSE ~MSWINDOWS}
    MyPath := PathProp.Value;
    {$ENDIF ~MSWINDOWS}
    MyPath := PathAddSeparator(Path) + MyPath;
    MySetting.Path := PathCanonicalize(PathGetRelativePath(GetCurrentDir, MyPath));
    Masks := TStringList.Create;
    try
      StrToStrings(MaskProp.Value, NativeSpace, Masks);
      SetLength(MySetting.Masks, Masks.Count);
      for Index := 0 to Masks.Count - 1 do
        {$IFDEF MSWINDOWS}
        MySetting.Masks[Index] := StringReplace(Masks.Strings[Index], '/', DirDelimiter, [rfReplaceAll]);
        {$ELSE ~MSWINDOWS}
        MySetting.Masks[Index] := Masks.Strings[Index];
        {$ENDIF ~MSWINDOWS}
    finally
      Masks.Free;
    end;
    MySetting.Recurse := RecurseProp.Value = 'yes';
    MySetting.DirOnly := DirOnlyProp.Value = 'yes';
    for Index := 0 to Elem.Items.Count - 1 do
    begin
      SubElem := Elem.Items.Item[Index];
      if SubElem.Name = 'setting' then
        LoadSetting(SubElem, MyPath)
      else
      if SubElem.Name = 'property' then
      begin
        SetLength(MySetting.Properties, Length(MySetting.Properties) + 1);
        MySetting.Properties[High(MySetting.Properties)] := LoadProperty(SubElem);
      end
      else
      if SubElem.Name <> '' then
        raise Exception.CreateFmt('unknown item "%s"', [SubElem.Name]);
    end;
    if Length(MySetting.Properties) > 0 then
    begin
      SetLength(FSettings, Length(FSettings) + 1);
      FSettings[High(FSettings)] := MySetting;
    end;
  end;

  procedure LoadTarget(Elem: TJclSimpleXmlElem);
  var
    PathProp: TJclSimpleXMLProp;
    MySetting: TSvnSetting;
    SubElem: TJclSimpleXMLElem;
    Index: Integer;
  begin
    PathProp := Elem.Properties.ItemNamed['path'];
    if not Assigned(PathProp) then
      raise Exception.Create('no path property');
    MySetting.Path := PathProp.Value;
    SetLength(MySetting.Masks, 0);
    MySetting.Recurse := False;
    MySetting.DirOnly := False;
    SetLength(MySetting.Properties, 0);
    for Index := 0 to Elem.Items.Count - 1 do
    begin
      SubElem := Elem.Items.Item[Index];
      if SubElem.Name = 'property' then
      begin
        SetLength(MySetting.Properties, Length(MySetting.Properties) + 1);
        MySetting.Properties[High(MySetting.Properties)] := LoadProperty(SubElem);
      end
      else
      if SubElem.Name <> '' then
        raise Exception.CreateFmt('unknown item "%s"', [SubElem.Name]);
    end;
    if Length(MySetting.Properties) > 0 then
    begin
      SetLength(FSettings, Length(FSettings) + 1);
      FSettings[High(FSettings)] := MySetting;
    end;
  end;
var
  Elem: TJclSimpleXMLElem;
  Index: Integer;
  RootProp: TJclSimpleXMLProp;
begin
  if XmlNode.Name = 'svncleaner' then
  begin
    // svn cleaner setting file
    RootProp := XmlNode.Properties.ItemNamed['root'];
    if not Assigned(RootProp) then
      raise Exception.Create('no root property');
    {$IFDEF MSWINDOWS}
    FRoot := StringReplace(RootProp.Value, '/', DirDelimiter, [rfReplaceAll]);
    {$ELSE ~MSWINDOWS}
    FRoot := RootProp.Value;
    {$ENDIF ~MSWINDOWS}
    FRoot := PathCanonicalize(PathGetRelativePath(GetCurrentDir, FRoot));
    for Index := 0 to XmlNode.Items.Count - 1 do
    begin
      Elem := XmlNode.Items.Item[Index];
      if Elem.Name = 'setting' then
        LoadSetting(Elem, '.')
      else
      if Elem.Name <> '' then
        raise Exception.CreateFmt('Unknown elem name "%s"', [Elem.Name]);
    end;
  end
  else
  if XmlNode.Name = 'properties' then
  begin
    // "svn proplist" result file
    for Index := 0 to XmlNode.Items.Count - 1 do
    begin
      Elem := XmlNode.Items.Item[Index];
      if Elem.Name = 'target' then
        LoadTarget(Elem)
      else
      if Elem.Name <> '' then
        raise Exception.CreateFmt('Unknown elem name "%s"', [Elem.Name]);
    end;
  end;
end;

//=== { TSvnCleaner } ========================================================

type
  TSvnCleaner = class
  private
    FSettings: TSvnSettings;
    FSvnProperties: TSvnSettings;
    FSvnItems: TStrings;
    FSvnExternals: TStrings;
    FSvnExe: string;
    function ExecuteSvn(const Argument: string): string;
    procedure CleanItem(const ItemName: string);
  public
    constructor Create(const XmlFileName: string);
    destructor Destroy; override;
    procedure Execute;
  end;

constructor TSvnCleaner.Create(const XmlFileName: string);
  procedure ParseSvnItems(RootElem: TJclSimpleXMLElem; Dest, Externals: TStrings);
  var
    TargetIndex, EntryIndex: Integer;
    TargetElem, EntryElem, WcStatusElem: TJclSimpleXMLElem;
    PathProp, ItemProp: TJclSimpleXMLProp;
    EntryPath: string;
  begin
    if RootElem.Name <> 'status' then
      raise Exception.CreateFmt('Unknown elem name "%s"', [RootElem.Name]);

    for TargetIndex := 0 to RootElem.Items.Count - 1 do
    begin
      TargetElem := RootElem.Items.Item[TargetIndex];
      if TargetElem.Name <> 'target' then
        raise Exception.CreateFmt('Unknown elem name "%s"', [TargetElem.Name]);

      for EntryIndex := 0 to TargetElem.Items.Count - 1 do
      begin
        EntryElem := TargetElem.Items.Item[EntryIndex];
        if EntryElem.Name <> 'entry' then
          raise Exception.CreateFmt('Unknown elem name "%s"', [EntryElem.Name]);
        PathProp := EntryElem.Properties.ItemNamed['path'];
        if not Assigned(PathProp) then
          raise Exception.Create('no path prop');
        EntryPath := PathProp.Value;

        if (EntryElem.Items.Count <> 1) then
          raise Exception.Create('invalid entry elem');

        WcStatusElem := EntryElem.Items.Item[0];
        if WcStatusElem.Name <> 'wc-status' then
          raise Exception.CreateFmt('Unknown elem name "%s"', [WcStatusElem.Name]);

        ItemProp := WcStatusElem.Properties.ItemNamed['item'];
        if not Assigned(ItemProp) then
          raise Exception.Create('no item prop');
        if ItemProp.Value = 'external' then
          Externals.Add(EntryPath)
        else
        if ItemProp.Value <> 'unversioned' then
          Dest.Add(EntryPath);
      end;
    end;
  end;
var
  Xml: TJclSimpleXML;
  StorageStream: TStream;
  AStringStream: TJclStringStream;
  SvnResult: string;
begin
  inherited Create;
  FSvnExe := GetEnvironmentVariable('SVN');
  if FSvnExe = '' then
    repeat
      WriteLn('Enter path to svn.exe');
      ReadLn(FSvnExe);
    until FileExists(FSvnExe);
  FSettings := TSvnSettings.Create;
  FSvnProperties := TSvnSettings.Create;
  FSvnItems := TStringList.Create;
  FSvnExternals := TStringList.Create;

  Xml := TJclSimpleXML.Create;
  try
    // load svn cleaner options
    Xml.LoadFromFile(XmlFileName);
    Xml.Options := Xml.Options - [sxoAutoCreate];

    FSettings.LoadFromXml(Xml.Root);

    StorageStream := TMemoryStream.Create;
    try
      AStringStream := TJclAutoStream.Create(StorageStream, False);
      try
        // retrieve the SVN properties
        WriteLn('getting SVN properties...');
        SvnResult := ExecuteSvn('proplist -v --xml -R ' + FSettings.Root);
        AStringStream.WriteString(SvnResult, 1, Length(SvnResult));
        AStringStream.Seek(0, soBeginning);
        Xml.LoadFromStringStream(AStringStream);
        FSvnProperties.LoadFromXml(Xml.Root);
      finally
        AStringStream.Free;
      end;

      StorageStream.Size := 0;

      AStringStream := TJclAutoStream.Create(StorageStream, False);
      try
        // retrieve the list of SVN items
        WriteLn('getting SVN items...');
        SvnResult := ExecuteSvn('status -v --xml ' + FSettings.Root);
        AStringStream.WriteString(SvnResult, 1, Length(SvnResult));
        AStringStream.Seek(0, soBeginning);
        Xml.LoadFromStringStream(AStringStream);
        ParseSvnItems(Xml.Root, FSvnItems, FSvnExternals);
      finally
        AStringStream.Free;
      end;
    finally
      StorageStream.Free;
    end;
  finally
    Xml.Free;
  end;
end;

destructor TSvnCleaner.Destroy;
begin
  FSvnExternals.Free;
  FSvnItems.Free;
  FSvnProperties.Free;
  FSettings.Free;
  inherited Destroy;
end;

procedure TSvnCleaner.CleanItem(const ItemName: string);
var
  ExternalItem, Choice, PropFileName: string;
  Index, IndexCheck: Integer;
  Properties, NewProperties: TSvnProperties;
  Found: Boolean;
begin
  WriteLn('processing item "', ItemName, '"');

  // do not process external items
  for Index := 0 to FSvnExternals.Count - 1 do
  begin
    ExternalItem := FSvnExternals.Strings[Index];
    if Copy(ItemName, 1, Length(ExternalItem)) = ExternalItem then
      Exit;
  end;

  Properties := FSvnProperties.GetProperties(ItemName);
  NewProperties := FSettings.GetProperties(ItemName);

  for Index := Low(Properties) to High(Properties) do
  begin
    Found := False;
    for IndexCheck := Low(NewProperties) to High(NewProperties) do
      if Properties[Index].Name = NewProperties[IndexCheck].Name then
      begin
        if (Properties[Index].Value <> NewProperties[IndexCheck].Value)
          and ((Properties[Index].Value + NativeLineBreak) <> NewProperties[IndexCheck].Value)
          and (Properties[Index].Value <> (NewProperties[IndexCheck].Value + NativeLineBreak)) then
        begin
          WriteLn('property "', Properties[Index].Name, '" for item "', ItemName, '" will be changed');
          WriteLn('old value: ', Properties[Index].Value);
          WriteLn('new value: ', NewProperties[IndexCheck].Value);
          repeat
            Write('process? (y)es, (n)o, (a)bord: ');
            ReadLn(Choice);
          until (Choice = 'y') or (Choice = 'Y') or (Choice = 'n') or (Choice = 'N') or (Choice = 'a') or (Choice = 'A');
          case Choice[1] of
            'y', 'Y':
              begin
                PropFileName := ChangeFileExt(ParamStr(0), '.prop');
                {$IFDEF SUPPORTS_UNICODE}
                StringToFile(PropFileName, RawByteString(NewProperties[IndexCheck].Value));
                {$ELSE ~SUPPORTS_UNICODE}
                StringToFile(PropFileName, NewProperties[IndexCheck].Value);
                {$ENDIF ~SUPPORTS_UNICODE}
                ExecuteSvn(Format('propset "%s" -F "%s" "%s"', [NewProperties[IndexCheck].Name, PropFileName, ItemName]));
              end;
            'n', 'N': ;
            'a', 'A':
              Abort;
          end;
        end;
        Found := True;
        Break;
      end;
    if not Found then
    begin
      WriteLn('property "', Properties[Index].Name, '" for item "', ItemName, '" will be deleted');
      WriteLn('old value: ', Properties[Index].Value);
      repeat
        Write('process? (y)es, (n)o, (a)bord: ');
        ReadLn(Choice);
      until (Choice = 'y') or (Choice = 'Y') or (Choice = 'n') or (Choice = 'N') or (Choice = 'a') or (Choice = 'A');
      case Choice[1] of
        'y', 'Y':
          ExecuteSvn(Format('propdel "%s" "%s"', [Properties[Index].Name, ItemName]));
        'n', 'N': ;
        'a', 'A':
          Abort;
      end;
    end;
  end;

  for Index := Low(NewProperties) to High(NewProperties) do
  begin
    Found := False;
    for IndexCheck := Low(Properties) to High(Properties) do
      if NewProperties[Index].Name = Properties[IndexCheck].Name then
      begin
        Found := True;
        Break;
      end;
    if not Found then
    begin
      WriteLn('property "', NewProperties[Index].Name, '" for item "', ItemName, '" will be added');
      WriteLn('new value: ', NewProperties[Index].Value);
      repeat
        Write('process? (y)es, (n)o, (a)bord: ');
        ReadLn(Choice);
      until (Choice = 'y') or (Choice = 'Y') or (Choice = 'n') or (Choice = 'N') or (Choice = 'a') or (Choice = 'A');
      case Choice[1] of
        'y', 'Y':
          begin
            PropFileName := ChangeFileExt(ParamStr(0), '.prop');
            {$IFDEF SUPPORTS_UNICODE}
            StringToFile(PropFileName, RawByteString(NewProperties[Index].Value));
            {$ELSE ~SUPPORTS_UNICODE}
            StringToFile(PropFileName, NewProperties[Index].Value);
            {$ENDIF ~SUPPORTS_UNICODE}
            ExecuteSvn(Format('propset "%s" -F "%s" "%s"', [NewProperties[Index].Name, PropFileName, ItemName]));
          end;
        'n', 'N': ;
        'a', 'A':
          Abort;
      end;
    end;
  end;
end;

procedure TSvnCleaner.Execute;
var
  Index: Integer;
begin
  for Index := 0 to FSvnItems.Count - 1 do
    CleanItem(FSvnItems.Strings[Index]);
end;

function TSvnCleaner.ExecuteSvn(const Argument: string): string;
begin
  Result := '';
  JclSysUtils.Execute(Format('"%s" %s', [FSvnExe, Argument]), Result);
end;

var
  ACleaner: TSvnCleaner;
begin
  try
    ACleaner := TSvnCleaner.Create(ChangeFileExt(ParamStr(0), '.xml'));
    try
      ACleaner.Execute;
    finally
      ACleaner.Free;
    end;
  except
    on E:Exception do
      WriteLn(E.Classname, ': ', E.Message);
  end;
end.
