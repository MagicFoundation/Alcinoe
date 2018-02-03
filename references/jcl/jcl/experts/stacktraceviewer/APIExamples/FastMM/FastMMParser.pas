unit FastMMParser;

{$I jcl.inc}

interface

uses
  SysUtils, Classes, Contnrs,
  JclBase,
  {$IFNDEF NOVIEW}
  JclStackTraceViewerClasses,
  {$ENDIF ~NOVIEW}
  JclDebug;

type
  {$IFDEF NOVIEW}
  TFastMMLocationInfoList = class(TJclCustomLocationInfoList)
  private
    function GetItems(AIndex: Integer): TJclLocationInfoEx;
  public
    constructor Create; override;
    function Add(Addr: Pointer): TJclLocationInfoEx;
    property Items[AIndex: Integer]: TJclLocationInfoEx read GetItems; default;
  end;
  {$ELSE ~NOVIEW}
  TFastMMLocationInfoList = TJclStackTraceViewerLocationInfoList;
  {$ENDIF ~NOVIEW}

  TFastMMMemoryArray = array [0..255] of Byte;

  TFastMMReport = class;

  TFastMMLeak = class(TObject)
  private
    FAddress: TJclAddr;
    FAllocationNumber: Integer;
    FBlockClass: string;
    FDateStr: string;
    FMemory: TFastMMMemoryArray;
    FFoundMemory: Boolean;
    FLeakSize: TJclAddr;
    FParent: TFastMMReport;
    FThreadID: Integer;
    FStack: TFastMMLocationInfoList;
  public
    constructor Create(AParent: TFastMMReport);
    destructor Destroy; override;
    property Address: TJclAddr read FAddress write FAddress;
    property AllocationNumber: Integer read FAllocationNumber write FAllocationNumber;
    property BlockClass: string read FBlockClass write FBlockClass;
    property DateStr: string read FDateStr write FDateStr;
    property Memory: TFastMMMemoryArray read FMemory write FMemory;
    property FoundMemory: Boolean read FFoundMemory write FFoundMemory;
    property LeakSize: TJclAddr read FLeakSize write FLeakSize;
    property Parent: TFastMMReport read FParent;
    property Stack: TFastMMLocationInfoList read FStack;
    property ThreadID: Integer read FThreadID write FThreadID;
  end;

  TFastMMLeakGroup = class(TObject)
  private
    FItems: TList;
    FLeakSize: TJclAddr;
    FLeakSizeUpdate: Boolean;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TFastMMLeak;
    function GetLeakSize: TJclAddr;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ALeak: TFastMMLeak);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TFastMMLeak read GetItems; default;
    property LeakSize: TJclAddr read GetLeakSize;
  end;

  TFastMMVMOnFreedObject = class(TObject)
  private
    FAddress: TJclAddr;
    FAllocationNumber: Integer;
    FObjectClass: string;
    FMemory: TFastMMMemoryArray;
    FFoundMemory: Boolean;
    FStack1: TFastMMLocationInfoList;
    FStack1Thread: Integer;
    FStack2: TFastMMLocationInfoList;
    FStack2Thread: Integer;
    FStack3: TFastMMLocationInfoList;
    FStack3Thread: Integer;
    FVirtualMethod: string;
    FVirtualMethodAddress: TJclAddr;
  public
    constructor Create;
    destructor Destroy; override;
    property Address: TJclAddr read FAddress write FAddress;
    property AllocationNumber: Integer read FAllocationNumber write FAllocationNumber;
    property ObjectClass: string read FObjectClass write FObjectClass;
    property Memory: TFastMMMemoryArray read FMemory write FMemory;
    property FoundMemory: Boolean read FFoundMemory write FFoundMemory;
    property Stack1Thread: Integer read FStack1Thread write FStack1Thread;
    property Stack1: TFastMMLocationInfoList read FStack1;
    property Stack2Thread: Integer read FStack2Thread write FStack2Thread;
    property Stack2: TFastMMLocationInfoList read FStack2;
    property Stack3Thread: Integer read FStack3Thread write FStack3Thread;
    property Stack3: TFastMMLocationInfoList read FStack3;
    property VirtualMethod: string read FVirtualMethod write FVirtualMethod;
    property VirtualMethodAddress: TJclAddr read FVirtualMethodAddress write FVirtualMethodAddress;
  end;

  TFastMMReport = class(TObject)
  private
    FLeakGroups: TObjectList;
    FLeaks: TObjectList;
    FLeakSummary: TStringList;
    FReportCompilerVersion: Double;
    FVMOnFreedObjects: TObjectList;
    function GetLeakCount: Integer;
    function GetLeaks(AIndex: Integer): TFastMMLeak;
    function GetLeakGroupCount: Integer;
    function GetLeakGroupItems(AIndex: Integer): TFastMMLeakGroup;
    function SameStack(AStack1, AStack2: TFastMMLocationInfoList): Boolean;
    function GetVMOnFreedObjectCount: Integer;
    function GetVMOnFreedObjectItems(AIndex: Integer): TFastMMVMOnFreedObject;
  public
    constructor Create;
    destructor Destroy; override;
    function AddLeak: TFastMMLeak;
    function AddLeakGroup: TFastMMLeakGroup;
    function AddVMOnFreedObject: TFastMMVMOnFreedObject;
    procedure BuildGroups;
    property LeakCount: Integer read GetLeakCount;
    property LeakGroupCount: Integer read GetLeakGroupCount;
    property LeakGroupItems[AIndex: Integer]: TFastMMLeakGroup read GetLeakGroupItems;
    property LeakItems[AIndex: Integer]: TFastMMLeak read GetLeaks;
    property LeakSummary: TStringList read FLeakSummary;
    property ReportCompilerVersion: Double read FReportCompilerVersion write FReportCompilerVersion;
    property VMOnFreedObjectCount: Integer read GetVMOnFreedObjectCount;
    property VMOnFreedObjectItems[AIndex: Integer]: TFastMMVMOnFreedObject read GetVMOnFreedObjectItems;
  end;

  TFastMMFileParser = class(TObject)
  private
    procedure FixStack(ALocationInfoList: TFastMMLocationInfoList);
  public
    function ParseFile(const AFileName: string; AReportList: TObjectList): Integer;
  end;

implementation

{$IFDEF NOVIEW}
function TFastMMLocationInfoList.Add(Addr: Pointer): TJclLocationInfoEx;
begin
  Result := InternalAdd(Addr);
end;

constructor TFastMMLocationInfoList.Create;
begin
  inherited Create;
  FOptions := [];
end;

function TFastMMLocationInfoList.GetItems(AIndex: Integer): TJclLocationInfoEx;
begin
  Result := TJclLocationInfoEx(FItems[AIndex]);
end;
{$ENDIF NOVIEW}

{ TFastMMLeak }

constructor TFastMMLeak.Create(AParent: TFastMMReport);
begin
  inherited Create;
  FAddress := 0;
  FAllocationNumber := 0;
  FBlockClass := '';
  FFoundMemory := False;
  FLeakSize := 0;
  FParent := AParent;
  FThreadID := 0;
  FStack := TFastMMLocationInfoList.Create;
end;

destructor TFastMMLeak.Destroy;
begin
  FStack.Free;
  inherited Destroy;
end;

{ TFastMMLeakGroup }

constructor TFastMMLeakGroup.Create;
begin
  inherited Create;
  FItems := TList.Create;
  FLeakSizeUpdate := True;
end;

destructor TFastMMLeakGroup.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TFastMMLeakGroup.Add(ALeak: TFastMMLeak);
begin
  FItems.Add(ALeak);
end;

function TFastMMLeakGroup.GetCount: Integer;
begin
  Result := FItems.Count
end;

function TFastMMLeakGroup.GetItems(AIndex: Integer): TFastMMLeak;
begin
  Result := TFastMMLeak(FItems[AIndex]);
end;

function TFastMMLeakGroup.GetLeakSize: TJclAddr;
var
  I: Integer;
begin
  if FLeakSizeUpdate then
  begin
    FLeakSizeUpdate := False;
    FLeakSize := 0;
    for I := 0 to Count - 1 do
      Inc(FLeakSize, Items[I].LeakSize);
  end;
  Result := FLeakSize;
end;

{ TFastMMVMOnFreedObject }

constructor TFastMMVMOnFreedObject.Create;
begin
  inherited Create;
  FAddress := 0;
  FAllocationNumber := 0;
  FFoundMemory := False;
  FStack1 := TFastMMLocationInfoList.Create;
  FStack1Thread := 0;
  FStack2 := TFastMMLocationInfoList.Create;
  FStack2Thread := 0;
  FStack3 := TFastMMLocationInfoList.Create;
  FStack3Thread := 0;
end;

destructor TFastMMVMOnFreedObject.Destroy;
begin
  FStack3.Free;
  FStack2.Free;
  FStack1.Free;
  inherited Destroy;
end;

{ TFastMMReport }

constructor TFastMMReport.Create;
begin
  inherited Create;
  FLeakGroups := TObjectList.Create;
  FLeaks := TObjectList.Create;
  FLeakSummary := TStringList.Create;
  {$IFDEF CONDITIONALEXPRESSIONS}
  FReportCompilerVersion := CompilerVersion;
  {$ELSE ~CONDITIONALEXPRESSIONS}
  FReportCompilerVersion := 5.01;
  {$ENDIF ~CONDITIONALEXPRESSIONS}
  FVMOnFreedObjects := TObjectList.Create;
end;

destructor TFastMMReport.Destroy;
begin
  FVMOnFreedObjects.Free;
  FLeakSummary.Free;
  FLeaks.Free;
  FLeakGroups.Free;
  inherited Destroy;
end;

function TFastMMReport.AddLeak: TFastMMLeak;
begin
  FLeaks.Add(TFastMMLeak.Create(Self));
  Result := TFastMMLeak(FLeaks.Last);
end;

function TFastMMReport.AddLeakGroup: TFastMMLeakGroup;
begin
  FLeakGroups.Add(TFastMMLeakGroup.Create);
  Result := TFastMMLeakGroup(FLeakGroups.Last);
end;

function TFastMMReport.AddVMOnFreedObject: TFastMMVMOnFreedObject;
begin
  FVMOnFreedObjects.Add(TFastMMVMOnFreedObject.Create);
  Result := TFastMMVMOnFreedObject(FVMOnFreedObjects.Last);
end;

procedure TFastMMReport.BuildGroups;
var
  I: Integer;
  LeftLeaks: TList;
  LeakGroup: TFastMMLeakGroup;
  FirstLeak: TFastMMLeak;
begin
  FLeakGroups.Clear;
  if LeakCount > 0 then
  begin
    LeftLeaks := TList.Create;
    try
      for I := 0 to LeakCount - 1 do
        LeftLeaks.Add(LeakItems[I]);
      while LeftLeaks.Count > 0 do
      begin
        LeakGroup := AddLeakGroup;
        FirstLeak := TFastMMLeak(LeftLeaks[0]);
        LeakGroup.Add(FirstLeak);
        LeftLeaks.Delete(0);
        for I := LeftLeaks.Count - 1 downto 0 do
          if SameStack(FirstLeak.Stack, TFastMMLeak(LeftLeaks[I]).Stack) then
          begin
            LeakGroup.Add(TFastMMLeak(LeftLeaks[I]));
            LeftLeaks.Delete(I);
          end;
      end;
    finally
      LeftLeaks.Free;
    end;
  end;
end;

function TFastMMReport.GetLeakCount: Integer;
begin
  Result := FLeaks.Count;
end;

function TFastMMReport.GetLeakGroupCount: Integer;
begin
  Result := FLeakGroups.Count
end;

function TFastMMReport.GetLeakGroupItems(AIndex: Integer): TFastMMLeakGroup;
begin
  Result := TFastMMLeakGroup(FLeakGroups[AIndex]);
end;

function TFastMMReport.GetLeaks(AIndex: Integer): TFastMMLeak;
begin
  Result := TFastMMLeak(FLeaks[AIndex]);
end;

function TFastMMReport.GetVMOnFreedObjectCount: Integer;
begin
  Result := FVMOnFreedObjects.Count;
end;

function TFastMMReport.GetVMOnFreedObjectItems(AIndex: Integer): TFastMMVMOnFreedObject;
begin
  Result := TFastMMVMOnFreedObject(FVMOnFreedObjects[AIndex]);
end;

function TFastMMReport.SameStack(AStack1, AStack2: TFastMMLocationInfoList): Boolean;
var
  I: Integer;
begin
  Result := Assigned(AStack1) and Assigned(AStack2) and (AStack1.Count = AStack2.Count);
  if Result then
    for I := 0 to AStack1.Count - 1 do
      if AStack1[I].Address <> AStack2[I].Address then
      begin
        Result := False;
        Break;
      end;
end;

function GetLocationInfoFromFastMMLine(AStr: string; var ALocationInfo: TJclLocationInfoEx): Boolean;
var
  I: Integer;
  BlockOpen, LastIsNumber: Boolean;
  C: Char;
  S: string;
  Blocks: TStringList;
begin
  Result := False;
  BlockOpen := False;
  Blocks := TStringList.Create;
  try
    S := '';
    for I := 1 to Length(AStr) do
    begin
      C := AStr[I];
      if C = '[' then
      begin
        if BlockOpen then
        begin
          Blocks.Clear;
          Break;
        end
        else
        begin
          BlockOpen := True;
          S := '';
        end;
      end
      else
      if C = ']' then
      begin
        if BlockOpen then
        begin
          BlockOpen := False;
          Blocks.Add(S);
        end
        else
        begin
          Blocks.Clear;
          Break;
        end;
      end
      else
        S := S + C;
    end;

    if Blocks.Count > 0 then
    begin
      LastIsNumber := False;
      S := Blocks[Blocks.Count - 1];
      if S <> '' then
      begin
        LastIsNumber := True;
        for I := 1 to Length(S) do
          {$IFDEF COMPILER12_UP}
          if not CharInSet(S[I], ['0'..'9']) then
          {$ELSE !COMPILER12_UP}
          if not (S[I] in ['0'..'9']) then
          {$ENDIF !COMPILER12_UP}
          begin
            LastIsNumber := False;
            Break;
          end;
      end;
      if LastIsNumber then
      begin
        if Blocks.Count = 4 then
        begin
          ALocationInfo.SourceName := Blocks[0];
          ALocationInfo.SourceUnitName := Blocks[1];
          ALocationInfo.ProcedureName := Blocks[2];
          ALocationInfo.LineNumber := StrToInt(Blocks[3]);
          Result := True;
        end
        else
        if Blocks.Count = 3 then
        begin
          ALocationInfo.SourceUnitName := Blocks[0];
          ALocationInfo.ProcedureName := Blocks[1];
          ALocationInfo.LineNumber := StrToInt(Blocks[2]);
          Result := True;
        end;
      end
      else
      if Blocks.Count = 1 then
      begin
        ALocationInfo.ProcedureName := Blocks[0];
        Result := True;
      end
      else
      if Blocks.Count = 2 then
      begin
        ALocationInfo.SourceUnitName := Blocks[0];
        ALocationInfo.ProcedureName := Blocks[1];
        Result := True;
      end;
      if Result then
      begin
        S := '';
        for I := 1 to Length(AStr) do
        begin
          C := AStr[I];
          {$IFDEF COMPILER12_UP}
          if CharInSet(C, ['0'..'9', 'A'..'F']) then
          {$ELSE !COMPILER12_UP}
          if C in ['0'..'9', 'A'..'F'] then
          {$ENDIF !COMPILER12_UP}
            S := S + C
          else
          if C = ' ' then
          begin
            if S <> '' then
              ALocationInfo.Address := Pointer(StrToInt64('$' + S));
            Break;
          end
          else
            Break;
        end;

      end;
    end;
  finally
    Blocks.Free;
  end;
end;

{ TFastMMFileParser }

procedure TFastMMFileParser.FixStack(ALocationInfoList: TFastMMLocationInfoList);
var
  I: Integer;
  FixProcedureName: Boolean;
  S: string;
  LocationInfoEx: TJclLocationInfoEx;
begin
  if ALocationInfoList.Count > 0 then
  begin
    FixProcedureName := True;
    for I := 0 to ALocationInfoList.Count - 1 do
    begin
      LocationInfoEx := ALocationInfoList[I];
      if (LocationInfoEx.SourceUnitName <> '') and
        (Pos(LocationInfoEx.SourceUnitName + '.', LocationInfoEx.ProcedureName) <> 1) then
      begin
        FixProcedureName := False;
        Break;
      end;
    end;
    if FixProcedureName then
      for I := 0 to ALocationInfoList.Count - 1 do
      begin
        LocationInfoEx := ALocationInfoList[I];
        if LocationInfoEx.SourceUnitName <> '' then
        begin
          S := LocationInfoEx.ProcedureName;
          Delete(S, 1, Length(LocationInfoEx.SourceUnitName) + 1);
          LocationInfoEx.ProcedureName := S;
        end;
      end;
  end;
end;

{ TODO : Parse compiler version when they exist in the report }
function TFastMMFileParser.ParseFile(const AFileName: string; AReportList: TObjectList): Integer;
type { TODO : There is at least one other report type (FastMM4Messages.InterfaceErrorHeader) }
  TReportType = (rtUnknown, rtMemoryLeak, rtVMOnFreedObject);
const
  //Leak constants
  cDateTime = '--------------------------------2';
  cLeakSize = 'A memory block has been leaked. The size is: ';
  cThread = 'This block was allocated by thread 0x';
  cStack = 'the stack trace (return addresses) at the time was:';
  cBlockClass = 'The block is currently used for an object of class: ';
  cAllocNo = 'The allocation number is: ';
  cMemory = 'Current memory dump of 256 bytes starting at pointer address ';
  cReportEnd = 'This application has leaked memory.';
  cReportSummaryPart = ' bytes: ';
  //Virtual method call on freed object
  cVMFOStart = 'FastMM has detected an attempt to call a virtual method on a freed object. An access violation will now be raised in order to abort the current operation.';
  cVMFOClass = 'Freed object class: ';
  cVMFOVirtualMethod = 'Virtual method: ';
  cVMFOVirtualMethodAddress = 'Virtual method address: ';
  cVMFOAllocNo = 'The allocation number was: ';
  cVMFOStack1Thread = 'The object was allocated by thread 0x';
  cVMFOStack1Stack = 'and the stack trace (return addresses) at the time was:';
  cVMFOStack2Thread = 'The object was subsequently freed by thread 0x';
  cVMFOStack2Stack = 'and the stack trace (return addresses) at the time was:';
  cVMFOStack3Thread = 'The current thread ID is 0x';
  cVMFOStack3Stack = 'and the stack trace (return addresses) leading to this error is:';
  cVMFOMemory = 'Current memory dump of 256 bytes starting at pointer address ';
var
  TSL: TStringList;
  I, J, K, P: Integer;
  Report: TFastMMReport;
  Leak: TFastMMLeak;
  VMOnFreedObject: TFastMMVMOnFreedObject;
  S, S2: string;
  LI: TJclLocationInfoEx;
  LocationInfoEx: TJclLocationInfoEx;
  MemoryArray: TFastMMMemoryArray;
  CreateNewReport: Boolean;
  ReportType, LastReportType: TReportType;
begin
  Result := -1;
  if FileExists(AFileName) and Assigned(AReportList) then
  begin
    TSL := TStringList.Create;
    try
      TSL.LoadFromFile(AFileName);
      TSL.Text := AdjustLineBreaks(TSL.Text);
      I := 0;
      Leak := nil;
      VMOnFreedObject := nil;
      Report := nil;
      CreateNewReport := True;
      ReportType := rtUnknown;
      LastReportType := rtUnknown;
      while I < TSL.Count do
      begin
        S := TSL[I];
        if Pos(cLeakSize, S) = 1 then
        begin
          ReportType := rtMemoryLeak;
          if CreateNewReport or (LastReportType <> ReportType) then
          begin
            AReportList.Add(TFastMMReport.Create);
            Report := TFastMMReport(AReportList.Last);
            CreateNewReport := False;
          end;
          LastReportType := ReportType;
          Leak := Report.AddLeak;
          Delete(S, 1, Length(cLeakSize));
          Leak.LeakSize := StrToInt64Def(S, 0);
          if (I > 1) then
          begin
            S := TSL[I - 1];
            if Pos(cDateTime, S) = 1 then
            begin
              Delete(S, 1, Length(cDateTime) - 1);
              P := Pos('-', S);
              if P > 1 then
                Leak.DateStr := Copy(S, 1, P - 1);
            end;
          end;
        end
        else
        if Pos(cVMFOStart, S) = 1 then
        begin
          ReportType := rtVMOnFreedObject;
          if CreateNewReport or (LastReportType <> ReportType) then
          begin
            AReportList.Add(TFastMMReport.Create);
            Report := TFastMMReport(AReportList.Last);
            CreateNewReport := False;
          end;
          LastReportType := ReportType;
          VMOnFreedObject := Report.AddVMOnFreedObject;
        end
        else
        if (ReportType = rtMemoryLeak) and Assigned(Leak) then
        begin
          if Pos(cThread, S) = 1 then
          begin
            Delete(S, 1, Length(cThread));
            P := Pos(',', S);
            if P > 1 then
              Leak.ThreadID := StrToIntDef('$' + Copy(S, 1, P - 1), 0);
          end;
          if Pos(cStack, S) > 0 then
          begin
            Inc(I);

            LI := TJclLocationInfoEx.Create(nil, nil);
            try
              while (Trim(TSL[I]) = '') or GetLocationInfoFromFastMMLine(TSL[I], LI) do
              begin
                if Trim(TSL[I]) <> '' then
                begin
                  LocationInfoEx := Leak.Stack.Add(nil);
                  LocationInfoEx.Assign(LI);
                  LI.Clear;
                end;
                Inc(I);
              end;
            finally
              LI.Free;
            end;

            Dec(I);
          end;
          if Pos(cBlockClass, S) = 1 then
          begin
            Delete(S, 1, Length(cBlockClass));
            Leak.BlockClass := S;
          end;
          if Pos(cAllocNo, S) = 1 then
          begin
            Delete(S, 1, Length(cAllocNo));
            Leak.AllocationNumber := StrToIntDef(S, 0);
          end;
          if Pos(cMemory, S) = 1 then
          begin
            Delete(S, 1, Length(cMemory));
            P := Pos(':', S);
            if P > 1 then
            begin
              Leak.Address := StrToInt64Def('$' + Copy(S, 1, P - 1), 0);
              Inc(I);
              for J := 0 to 7 do
              begin
                S := Trim(TSL[I]);
                if Length(S) = 95 then
                begin
                  for K := 0 to 31 do
                  begin
                    S2 := Copy(S, K * 3 + 1, 2);
                    MemoryArray[J * 32 + K] := StrToIntDef('$' + S2, 0);
                  end;
                end
                else
                  Break;
                Inc(I);
                if J = 7 then
                begin
                  Leak.FoundMemory := True;
                  Leak.Memory := MemoryArray;
                end;
              end;
              Dec(I);
            end;
          end;
          if Pos(cReportEnd, S) > 0 then
          begin
            Inc(I);
            while (I < TSL.Count) and ((TSL[I]) = '') do
              Inc(I);
            while (I < TSL.Count) and (Pos(cReportSummaryPart, TSL[I]) > 0) do
            begin
              Report.LeakSummary.Add(TSL[I]);
              Inc(I);
            end;
            CreateNewReport := True;
          end;
        end
        else
        if (ReportType = rtVMOnFreedObject) and Assigned(VMOnFreedObject) then
        begin
          if Pos(cVMFOClass, S) = 1 then
          begin
            Delete(S, 1, Length(cVMFOClass));
            VMOnFreedObject.ObjectClass := S;
          end
          else
          if Pos(cVMFOVirtualMethod, S) = 1 then
          begin
            Delete(S, 1, Length(cVMFOVirtualMethod));
            VMOnFreedObject.VirtualMethod := S;
          end
          else
          if Pos(cVMFOVirtualMethodAddress, S) = 1 then
          begin
            Delete(S, 1, Length(cVMFOVirtualMethodAddress));
            VMOnFreedObject.VirtualMethodAddress := StrToInt64Def('$' + S, 0);
          end
          else
          if Pos(cVMFOAllocNo, S) = 1 then
          begin
            Delete(S, 1, Length(cVMFOAllocNo));
            VMOnFreedObject.AllocationNumber := StrToIntDef(S, 0);
          end
          else
          if Pos(cVMFOStack1Thread, S) = 1 then
          begin
            Delete(S, 1, Length(cVMFOStack1Thread));
            P := Pos(',', S);
            if P > 1 then
              VMOnFreedObject.Stack1Thread := StrToIntDef('$' + Copy(S, 1, P - 1), 0);
            if Pos(cVMFOStack1Stack, S) > 0 then
            begin
              Inc(I);

              LI := TJclLocationInfoEx.Create(nil, nil);
              try
                while (Trim(TSL[I]) = '') or GetLocationInfoFromFastMMLine(TSL[I], LI) do
                begin
                  if Trim(TSL[I]) <> '' then
                  begin
                    LocationInfoEx := VMOnFreedObject.Stack1.Add(nil);
                    LocationInfoEx.Assign(LI);
                    LI.Clear;
                  end;
                  Inc(I);
                end;
              finally
                LI.Free;
              end;

              Dec(I);
            end;
          end
          else
          if Pos(cVMFOStack2Thread, S) = 1 then
          begin
            Delete(S, 1, Length(cVMFOStack2Thread));
            P := Pos(',', S);
            if P > 1 then
              VMOnFreedObject.Stack2Thread := StrToIntDef('$' + Copy(S, 1, P - 1), 0);
            if Pos(cVMFOStack2Stack, S) > 0 then
            begin
              Inc(I);

              LI := TJclLocationInfoEx.Create(nil, nil);
              try
                while (Trim(TSL[I]) = '') or GetLocationInfoFromFastMMLine(TSL[I], LI) do
                begin
                  if Trim(TSL[I]) <> '' then
                  begin
                    LocationInfoEx := VMOnFreedObject.Stack2.Add(nil);
                    LocationInfoEx.Assign(LI);
                    LI.Clear;
                  end;
                  Inc(I);
                end;
              finally
                LI.Free;
              end;

              Dec(I);
            end;
          end
          else
          if Pos(cVMFOStack3Thread, S) = 1 then
          begin
            Delete(S, 1, Length(cVMFOStack3Thread));
            P := Pos(',', S);
            if P > 1 then
              VMOnFreedObject.Stack3Thread := StrToIntDef('$' + Copy(S, 1, P - 1), 0);
            if Pos(cVMFOStack3Stack, S) > 0 then
            begin
              Inc(I);

              LI := TJclLocationInfoEx.Create(nil, nil);
              try
                while (Trim(TSL[I]) = '') or GetLocationInfoFromFastMMLine(TSL[I], LI) do
                begin
                  if Trim(TSL[I]) <> '' then
                  begin
                    LocationInfoEx := VMOnFreedObject.Stack3.Add(nil);
                    LocationInfoEx.Assign(LI);
                    LI.Clear;
                  end;
                  Inc(I);
                end;
              finally
                LI.Free;
              end;

              Dec(I);
            end;
          end
          else
          if Pos(cVMFOMemory, S) = 1 then
          begin
            Delete(S, 1, Length(cVMFOMemory));
            P := Pos(':', S);
            if P > 1 then
            begin
              VMOnFreedObject.Address := StrToInt64Def('$' + Copy(S, 1, P - 1), 0);
              Inc(I);
              for J := 0 to 7 do
              begin
                while Trim(TSL[I]) = '' do
                  Inc(I);
                S := Trim(TSL[I]);
                if Length(S) = 95 then
                begin
                  for K := 0 to 31 do
                  begin
                    S2 := Copy(S, K * 3 + 1, 2);
                    MemoryArray[J * 32 + K] := StrToIntDef('$' + S2, 0);
                  end;
                end
                else
                  Break;
                Inc(I);
                if J = 7 then
                begin
                  VMOnFreedObject.FoundMemory := True;
                  VMOnFreedObject.Memory := MemoryArray;
                end;
              end;
              Dec(I);
            end;
          end;

        end;
        Inc(I);
      end;
    finally
      TSL.Free;
    end;
    for I := 0 to AReportList.Count - 1 do
    begin
      Report := TFastMMReport(AReportList[I]);
      Report.BuildGroups;
      for J := 0 to Report.LeakCount - 1 do
        FixStack(Report.LeakItems[J].Stack);
      for J := 0 to Report.VMOnFreedObjectCount - 1 do
      begin
        FixStack(Report.VMOnFreedObjectItems[J].Stack1);
        FixStack(Report.VMOnFreedObjectItems[J].Stack2);
        FixStack(Report.VMOnFreedObjectItems[J].Stack3);
      end;
    end;

    Result := AReportList.Count;
  end;
end;

end.
