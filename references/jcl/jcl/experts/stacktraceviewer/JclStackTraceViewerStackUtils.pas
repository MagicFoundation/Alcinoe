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
{ The Original Code is JclStackTraceViewerStackUtils.pas.                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) 2009 Uwe Schuster. All rights reserved.       }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStackTraceViewerStackUtils;

{$I jcl.inc}

interface

uses
  Windows, SysUtils, Classes, Contnrs, ActiveX,
  ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclStrings, JclFileUtils,
  JclDebugSerialization, JclStackTraceViewerClasses, JclStackTraceViewerStackCodeUtils,
  JclStackTraceViewerOptions, JclStackTraceViewerAPIImpl, JclStackTraceViewerAPI;

type
  TLocationInfoProcessorProgressStatus = (lippsUnknown, lippsStart, lippsFinished);

  TJclLocationInfoProgressEvent = procedure(AStatus: TLocationInfoProcessorProgressStatus;
    APos, AMax: Integer; const AText: string) of object;

  TJclLocationInfoProcessor = class(TInterfacedObject, IInterface, IJclStackTraceViewerStackProcessorServices)
  private
    FModuleList: IJclModuleInfoList;
    FOnProgress: TJclLocationInfoProgressEvent;
    FOptions: TExceptionViewerOption;
    FRootDir: string;
    procedure DoProgress(AStatus: TLocationInfoProcessorProgressStatus; APos, AMax: Integer; const AText: string);
  public
    property OnProgress: TJclLocationInfoProgressEvent read FOnProgress write FOnProgress;
    property Options: TExceptionViewerOption read FOptions write FOptions;
    property RootDir: string read FRootDir write FRootDir;

    { IInterface }
    // function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IJclStackTraceViewerStackProcessorServices }
    function GetModuleInfoList: IJclModuleInfoList;
    procedure PrepareLocationInfoList(AStack: IJclPreparedLocationInfoList; AForce: Boolean = False);
    procedure SetModuleInfoList(AValue: IJclModuleInfoList);
    property ModuleList: IJclModuleInfoList read GetModuleInfoList write SetModuleInfoList;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\stacktraceviewer';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclOtaResources;

type
  TFileSearchItem = class(TObject)
  private
    FName: string;
    FResults: TStringList;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName;
    property Results: TStringList read FResults;
  end;

  TFileSearcher = class(TObject)
  private
    FFiles: TObjectList;
    FSearchPaths: TStringList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TFileSearchItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName: string): TFileSearchItem;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function IndexOf(const AName: string): Integer;
    procedure Search;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TFileSearchItem read GetItems; default;
    property SearchPaths: TStringList read FSearchPaths;
  end;

{ TFileSearcher }

constructor TFileSearcher.Create;
begin
  inherited Create;
  FFiles := TObjectList.Create;
  FSearchPaths := TStringList.Create;
end;

destructor TFileSearcher.Destroy;
begin
  FSearchPaths.Free;
  FFiles.Free;
  inherited Destroy;
end;

function TFileSearcher.Add(const AName: string): TFileSearchItem;
begin
  FFiles.Add(TFileSearchItem.Create(AName));
  Result := TFileSearchItem(FFiles.Last);
end;

procedure TFileSearcher.Clear;
begin
  FFiles.Clear;
end;

procedure TFileSearcher.Delete(AIndex: Integer);
begin
  FFiles.Delete(AIndex);
end;

function TFileSearcher.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

function TFileSearcher.GetItems(AIndex: Integer): TFileSearchItem;
begin
  Result := TFileSearchItem(FFiles[AIndex]);
end;

function TFileSearcher.IndexOf(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Name = AName then
    begin
      Result := I;
      Break;
    end;
end;

procedure TFileSearcher.Search;
var
  I, Idx: Integer;
  FastIndexList: TStringList;
  FS: TFileSearchItem;
  Path: string;
  sr: TSearchRec;
  re: Integer;
begin
  if (Count > 0) then
  begin
    for I := 0 to Count - 1 do
      Items[I].Results.Clear;
    if FSearchPaths.Count > 0 then
    begin
      FastIndexList := TStringList.Create;
      try
        FastIndexList.Sorted := True;
        for I := 0 to Count - 1 do
          FastIndexList.AddObject(Items[I].Name, Items[I]);
        for I := 0 to FSearchPaths.Count - 1 do
        begin
          Path := PathAddSeparator(FSearchPaths[I]);
          re := FindFirst(Path + '*.*', faAnyFile - faDirectory, sr);
          while re = 0 do
          begin
            Idx := FastIndexList.IndexOf(sr.Name);
            if Idx <> -1 then
            begin
              FS := TFileSearchItem(FastIndexList.Objects[Idx]);
              FS.Results.Add(Path + sr.Name);
            end;
            re := FindNext(sr);
          end;
          FindClose(sr);
        end;
      finally
        FastIndexList.Free;
      end;
    end;
  end;
end;

{ TFileSearchItem }

constructor TFileSearchItem.Create(const AName: string);
begin
  inherited Create;
  FResults := TStringList.Create;
  FName := AName;
end;

destructor TFileSearchItem.Destroy;
begin
  FResults.Free;
  inherited Destroy;
end;

type
  TFindMapping = class(TObject)
  private
    FItems: TInterfaceList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): IJclPreparedLocationInfo;
  public
    FoundFile: Boolean;
    FileName: string;
    ProjectName: string;
    constructor Create;
    destructor Destroy; override;
    procedure Add(AStackViewItem: IJclPreparedLocationInfo);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: IJclPreparedLocationInfo read GetItems; default;
  end;

constructor TFindMapping.Create;
begin
  inherited Create;
  FItems := TInterfaceList.Create;
end;

destructor TFindMapping.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TFindMapping.Add(AStackViewItem: IJclPreparedLocationInfo);
begin
  FItems.Add(AStackViewItem);
end;

function TFindMapping.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TFindMapping.GetItems(AIndex: Integer): IJclPreparedLocationInfo;
begin
  Result := IJclPreparedLocationInfo(FItems[AIndex]);
end;

procedure TJclLocationInfoProcessor.DoProgress(AStatus: TLocationInfoProcessorProgressStatus;
  APos, AMax: Integer; const AText: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(AStatus, APos, AMax, AText);
end;

function TJclLocationInfoProcessor.GetModuleInfoList: IJclModuleInfoList;
begin
  Result := FModuleList;
end;

procedure TJclLocationInfoProcessor.PrepareLocationInfoList(AStack: IJclPreparedLocationInfoList; AForce: Boolean);
var
  I, J, K, Idx: Integer;
  StackViewItem: IJclPreparedLocationInfo;
  FindFileList: TStringList;
  FindMapping: TFindMapping;
  FileName, ProjectName: string;
  RevisionMS: TMemoryStream;
  RevisionStream, Stream: IStream;
  MS: TMemoryStream;
  SA: TStreamAdapter;

  S, ModuleListModulePath, FindMappingModulePath: string;
  EV: IOTAEnvironmentOptions;
  FileSearcher: TFileSearcher;
  BrowsingPaths: TStringList;

  Found: Boolean;
  RevisionLineNumbers, CurrentLineNumbers: TList;
begin
  if AForce or not AStack.Prepared then
  begin
    DoProgress(lippsStart, 0, 100, '');
    if AStack.Count > 0 then
    begin
      FindFileList := TStringList.Create;
      try
        FindFileList.Sorted := True;
        //check if the files can be found in a project in the current project group
        DoProgress(lippsUnknown, 0, AStack.Count, LoadResString(@RsSTVFindFilesInProjectGroup));
        for I := 0 to AStack.Count - 1 do
          if AStack.Items[I].QueryInterface(IJclPreparedLocationInfo, StackViewItem) = S_OK then
          begin
            StackViewItem.Revision := StackViewItem.UnitVersionRevision;
            Idx := FindFileList.IndexOf(StackViewItem.SourceName);
            if Idx <> -1 then
            begin
              FindMapping := TFindMapping(FindFileList.Objects[Idx]);
              FindMapping.Add(StackViewItem);
              StackViewItem.FoundFile := FindMapping.FoundFile;
              StackViewItem.FileName := FindMapping.FileName;
              StackViewItem.ProjectName := FindMapping.ProjectName;
            end
            else
            begin
              if AStack.Items[I].SourceName <> '' then
              begin
                DoProgress(lippsUnknown, I + 1, AStack.Count, Format(LoadResString(@RsSTVFindFileInProjectGroup), [AStack.Items[I].SourceName]));
                FileName := FindModuleAndProject(AStack.Items[I].SourceName, ProjectName);
              end
              else
              begin
                FileName := '';
                ProjectName := '';
              end;
              FindMapping := TFindMapping.Create;
              FindMapping.Add(StackViewItem);
              FindFileList.AddObject(AStack.Items[I].SourceName, FindMapping);
              FindMapping.FoundFile := FileName <> '';
              FindMapping.FileName := FileName;
              FindMapping.ProjectName := ProjectName;

              StackViewItem.FoundFile := FileName <> '';
              StackViewItem.FileName := FileName;
              StackViewItem.ProjectName := ProjectName;
            end;
            DoProgress(lippsUnknown, I + 1, AStack.Count, LoadResString(@RsSTVFindFilesInProjectGroup));
          end;

        //use the build number from the version number as revision number if the revision number is empty
        if Assigned(FOptions) and FOptions.ModuleVersionAsRevision and Assigned(FModuleList) then
        begin
          for I := 0 to FindFileList.Count - 1 do
          begin
            FindMapping := TFindMapping(FindFileList.Objects[I]);
            if (FindMapping.Count > 0) and (FindMapping[0].Revision = '') and (FindMapping[0].ModuleName <> '') then
            begin
              Idx := -1;
              {Why full filenames?

               It is possible to load
               <Path 1>\TestDLL.DLL
               <Path 2>\TestDLL.DLL}
              for J := 0 to FModuleList.Count - 1 do
              begin
                ModuleListModulePath := ExtractFilePath(FModuleList[J].ModuleName);
                FindMappingModulePath := ExtractFilePath(FindMapping[0].ModuleName);
                if (CompareText(ExtractFileName(FModuleList[J].ModuleName), ExtractFileName(FindMapping[0].ModuleName)) = 0) and
                  ((ModuleListModulePath = '') or (FindMappingModulePath = '') or
                    (CompareText(ModuleListModulePath, FindMappingModulePath) = 0)) then
                begin
                  Idx := J;
                  Break;
                end;
              end;
              if Idx <> -1 then
              begin
                S := FModuleList[Idx].BinFileVersion;
                K := Pos('.', S);
                if K > 0 then
                  Delete(S, 1, K);
                K := Pos('.', S);
                if K > 0 then
                  Delete(S, 1, K);
                K := Pos('.', S);
                if K > 0 then
                begin
                  Delete(S, 1, K);
                  for J := 0 to FindMapping.Count - 1 do
                    FindMapping[J].Revision := S;
                end;
              end;
            end;
          end;
        end;

        //check if the other files can be found in BrowsingPath
        if FRootDir <> '' then
        begin
          Found := False;
          for I := 0 to FindFileList.Count - 1 do
          begin
            FindMapping := TFindMapping(FindFileList.Objects[I]);
            if (FindFileList[I] <> '') and (not FindMapping.FoundFile) then
            begin
              Found := True;
              Break;
            end;
          end;
          if Found then
          begin
            FileSearcher := TFileSearcher.Create;
            try
              BrowsingPaths := TStringList.Create;
              try
                EV := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
                StrTokenToStrings(EV.Values['BrowsingPath'], ';', BrowsingPaths);
                for I := 0 to BrowsingPaths.Count - 1 do
                begin
                  S := BrowsingPaths[I];
                  if Pos('$(BDS)', S) > 0 then
                    S := StringReplace(S, '$(BDS)', FRootDir, []);
                  FileSearcher.SearchPaths.Add(S);
                end;
              finally
                BrowsingPaths.Free;
              end;
              if FileSearcher.SearchPaths.Count > 0 then
              begin
                for I := 0 to FindFileList.Count - 1 do
                begin
                  FindMapping := TFindMapping(FindFileList.Objects[I]);
                  if (FindFileList[I] <> '') and (not FindMapping.FoundFile) and (FileSearcher.IndexOf(FindFileList[I]) = -1) then
                    FileSearcher.Add(FindFileList[I]);
                end;
                if FileSearcher.Count > 0 then
                begin
                  DoProgress(lippsUnknown, 0, 100, LoadResString(@RsSTVFindFilesInBrowsingPath));
                  FileSearcher.Search;
                  DoProgress(lippsUnknown, 75, 100, LoadResString(@RsSTVFindFilesInBrowsingPath));
                  for I := 0 to FindFileList.Count - 1 do
                  begin
                    FindMapping := TFindMapping(FindFileList.Objects[I]);
                    if not FindMapping.FoundFile then
                    begin
                      Idx := FileSearcher.IndexOf(FindFileList[I]);
                      if (Idx <> -1) and (FileSearcher[Idx].Results.Count > 0) then
                      begin
                        FindMapping.FoundFile := True;
                        FindMapping.FileName := FileSearcher[Idx].Results[0];
                        FindMapping.ProjectName := '';
                        for J := 0 to FindMapping.Count - 1 do
                        begin
                          FindMapping[J].FoundFile := FindMapping.FoundFile;
                          FindMapping[J].FileName := FindMapping.FileName;
                          FindMapping[J].ProjectName := FindMapping.ProjectName;
                        end;
                      end;
                    end;
                    DoProgress(lippsUnknown, FindFileList.Count * 3 + I + 1, FindFileList.Count * 4, LoadResString(@RsSTVFindFilesInBrowsingPath));
                  end;
                end;
              end;
            finally
              FileSearcher.Free;
            end;
          end;
        end;
        DoProgress(lippsUnknown, 0, FindFileList.Count, '');
        for I := 0 to FindFileList.Count - 1 do
        begin
          FindMapping := TFindMapping(FindFileList.Objects[I]);
          if (FindMapping.FoundFile) and (FindMapping.Count > 0) and (FindMapping[0].Revision <> '') then
          begin
            Found := False;
            for J := 0 to FindMapping.Count - 1 do
              if FindMapping[J].LineNumber > 0 then
              begin
                Found := True;
                Break;
              end;
            if Found then
            begin
              Stream := GetFileEditorContent(FindMapping.FileName);
              if not Assigned(Stream) then
              begin
                if FileExists(FindMapping.FileName) then
                begin
                  SA := TStreamAdapter.Create(TMemoryStream.Create, soOwned);
                  Stream := SA;
                  MS := TMemoryStream(SA.Stream);
                  MS.LoadFromFile(FindMapping.FileName);
                end;
              end;
              if Assigned(Stream) then
              begin
                RevisionLineNumbers := TList.Create;
                CurrentLineNumbers := TList.Create;
                try
                  for J := 0 to FindMapping.Count - 1 do
                    if FindMapping[J].LineNumber > 0 then
                      RevisionLineNumbers.Add(Pointer(FindMapping[J].LineNumber));
                  RevisionMS := TMemoryStream.Create;
                  try
                    RevisionStream := TStreamAdapter.Create(RevisionMS);
                    if GetRevisionContent(FindMapping.FileName, FindMapping[0].Revision, RevisionStream) then
                    begin
                      if TranslateLineNumbers(RevisionStream, Stream, RevisionLineNumbers, CurrentLineNumbers) > 0 then
                      begin
                        if RevisionLineNumbers.Count = CurrentLineNumbers.Count then
                        begin
                          for J := 0 to FindMapping.Count - 1 do
                            if FindMapping[J].LineNumber > 0 then
                            begin
                              FindMapping[J].TranslatedLineNumber := -1;
                              for K := 0 to RevisionLineNumbers.Count - 1 do
                                if Integer(RevisionLineNumbers[K]) = FindMapping[J].LineNumber then
                                begin
                                  FindMapping[J].TranslatedLineNumber := Integer(CurrentLineNumbers[K]);
                                  Break;
                                end;
                            end;
                        end;
                      end;
                    end;
                  finally
                    RevisionMS.Free;
                  end;
                finally
                  RevisionLineNumbers.Free;
                  CurrentLineNumbers.Free;
                end;
              end;
            end;
          end;
          DoProgress(lippsUnknown, I + 1, FindFileList.Count, '');
        end;
      finally
        for I := 0 to FindFileList.Count - 1 do
          FindFileList.Objects[I].Free;
        FindFileList.Free;
      end;
    end;
    DoProgress(lippsFinished, 0, 0, '');
    AStack.Prepared := True;
  end;
end;

procedure TJclLocationInfoProcessor.SetModuleInfoList(AValue: IJclModuleInfoList);
begin
  FModuleList := AValue;
end;

function TJclLocationInfoProcessor._AddRef: Integer;
begin
  Result := -1;
end;

function TJclLocationInfoProcessor._Release: Integer;
begin
  Result := -1;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
