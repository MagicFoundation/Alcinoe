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
{ The Original Code is JclContainerTemplates.pas.                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{         <outchy att users dott sourceforge dott net>                                             }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclPreProcessorContainerTemplates;

interface

{$I jcl.inc}

uses
  {$IFDEF HAS_UNITSCOPE}
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase,
  JclIDEUtils,
  JclPreProcessorTemplates,
  JclPreProcessorContainerTypes,
  JclPreProcessorContainer1DTemplates,
  JclPreProcessorContainer2DTemplates;

{$TYPEINFO ON}

type
  TJclContainerParams = class(TJclTemplateParams)
  private
    FTypeInfo: TJclContainerTypeInfo;
    FAllTypeIndex: Integer;
    FTrueTypeIndex: Integer;
    FMapInfo: TJclContainerMapInfo;
    FAllMapIndex: Integer;
    FTrueMapIndex: Integer;
    function GetAllTypeCount: Integer;
    function GetHelpAllTypeCount: Integer;
    function GetTrueTypeCount: Integer;
    function GetHelpTrueTypeCount: Integer;
    procedure SetAllTypeIndex(const Value: Integer);
    procedure SetTrueTypeIndex(const Value: Integer);
    function GetAllMapCount: Integer;
    function GetHelpAllMapCount: Integer;
    function GetTrueMapCount: Integer;
    function GetHelpTrueMapCount: Integer;
    procedure SetAllMapIndex(const Value: Integer);
    procedure SetMapTypeIndex(const Value: Integer);
  protected
    function ProcessConditional(const MacroText: string; ContainerTypeInfo: TJclContainerTypeInfo): string;
    procedure ProcessDefines(const Prefix, Defines, Undefs: string);
  public
    constructor Create;
    destructor Destroy; override;
    function ExpandMacro(const AName: string; const ParamValues: TDynStringArray): string; override;
  published
    property AllTypeIndex: Integer read FAllTypeIndex write SetAllTypeIndex;
    property AllTypeCount: Integer read GetAllTypeCount;
    property HelpAllTypeCount: Integer read GetHelpAllTypeCount;
    property TrueTypeIndex: Integer read FTrueTypeIndex write SetTrueTypeIndex;
    property TrueTypeCount: Integer read GetTrueTypeCount;
    property HelpTrueTypeCount: Integer read GetHelpTrueTypeCount;
    property AllMapIndex: Integer read FAllMapIndex write SetAllMapIndex;
    property AllMapCount: Integer read GetAllMapCount;
    property HelpAllMapCount: Integer read GetHelpAllMapCount;
    property TrueMapIndex: Integer read FTrueMapIndex write SetMapTypeIndex;
    property TrueMapCount: Integer read GetTrueMapCount;
    property HelpTrueMapCount: Integer read GetHelpTrueMapCount;
  end;

{$IFNDEF TYPEINFO_ON}
  {$TYPEINFO OFF}
{$ENDIF ~TYPEINFO_ON}

procedure RegisterContainerParams(const PrototypeName: string;
  InterfaceParamsClass: TJclInterfaceParamsClass); overload;
procedure RegisterContainerParams(const PrototypeName: string;
  ImplementationParamsClass: TJclImplementationParamsClass;
  InterfaceParamsClass: TJclInterfaceParamsClass); overload;
procedure FindContainerParams(const PrototypeName: string;
  out InterfaceParamsClass: TJclInterfaceParamsClass;
  out ImplementationParamsClass: TJclImplementationParamsClass);

procedure CheckJclContainers;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.TypInfo,
  System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  TypInfo,
  SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclRTTI,
  JclSysUtils,
  JclStrings,
  JclContainerIntf,
  JclPreProcessorContainerKnownTypes,
  JclPreProcessorContainerKnownMaps;

type
  TInterfaceParamsRec = record
    ParamsName: string;
    ParamsCount: Integer;
    ParamsClass: TJclInterfaceParamsClass;
  end;

  TImplementationParamsRec = record
    ParamsName: string;
    ParamsCount: Integer;
    ParamsClass: TJclImplementationParamsClass;
    InterfaceParamsClass: TJclInterfaceParamsClass;
  end;

var
  GlobalInterfaceParams: array of TInterfaceParamsRec;
  GlobalImplementationParams: array of TImplementationParamsRec;
  GlobalTypeAttributeHandlers: array [TAllTypeAttributeID] of TJclInterfaceParamsClass;
  GlobalTypeAttributeDependencies: array [TAllTypeAttributeID] of TAllTypeAttributeIDs;

procedure ClearRegisteredContainers;
var
  AttributeID: TAllTypeAttributeID;
begin
  SetLength(GlobalInterfaceParams, 0);
  SetLength(GlobalImplementationParams, 0);
  // avoid spurious errors while recompiling installed package in the IDE
  // for some reasons the GlobalTypeAttributeHandlers and GlobalAttirbuteDependencies
  // are not clean when the package is reloaded
  for AttributeID := Low(AttributeID) to High(AttributeID) do
  begin
    GlobalTypeAttributeHandlers[AttributeID] := nil;
    GlobalTypeAttributeDependencies[AttributeID] := [];
  end;
end;

procedure RegisterContainerParams(const PrototypeName: string;
  InterfaceParamsClass: TJclInterfaceParamsClass);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, Index: Integer;
  Dependencies: TAllTypeAttributeIDs;
begin
  // avoid duplicate registrations
  for Index := Low(GlobalInterfaceParams) to High(GlobalInterfaceParams) do
    if (GlobalInterfaceParams[Index].ParamsName = PrototypeName) then
      Exit;

  PropCount := GetStringPropList(InterfaceParamsClass.ClassInfo, PropList);
  if PropCount > 0 then
  begin
    try
      Dependencies := [];
      for Index := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[Index];
        if (PropInfo^.Index > 0) and (PropInfo^.StoredProc = nil) then
          Include(Dependencies, TAllTypeAttributeID(PropInfo^.Index));
      end;
      for Index := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[Index];
        if (PropInfo^.Index > 0) and (PropInfo^.StoredProc <> nil) then
        begin
          if Assigned(GlobalTypeAttributeHandlers[TAllTypeAttributeID(PropInfo^.Index)]) then
            raise EJclContainerException.CreateFmt('Duplicate handler for attribute %s: %s and %s',
                                                   [GetEnumName(TypeInfo(TAllTypeAttributeID), PropInfo^.Index),
                                                    GlobalTypeAttributeHandlers[TAllTypeAttributeID(PropInfo^.Index)].ClassName,
                                                    InterfaceParamsClass.ClassName]);
          GlobalTypeAttributeHandlers[TAllTypeAttributeID(PropInfo^.Index)] := InterfaceParamsClass;
          GlobalTypeAttributeDependencies[TAllTypeAttributeID(PropInfo^.Index)] := Dependencies;
        end;
      end;
      if PrototypeName <> '' then
      begin
        Index := Length(GlobalInterfaceParams);
        SetLength(GlobalInterfaceParams, Index + 1);
        GlobalInterfaceParams[Index].ParamsName := PrototypeName;
        GlobalInterfaceParams[Index].ParamsCount := PropCount;
        GlobalInterfaceParams[Index].ParamsClass := InterfaceParamsClass;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure RegisterContainerParams(const PrototypeName: string;
  ImplementationParamsClass: TJclImplementationParamsClass;
  InterfaceParamsClass: TJclInterfaceParamsClass);
var
  PropList: PPropList;
  PropCount, Index: Integer;
  PropInfo: PPropInfo;
begin
  // avoid duplicate registrations
  for Index := Low(GlobalImplementationParams) to High(GlobalImplementationParams) do
    if (GlobalImplementationParams[Index].ParamsName = PrototypeName) then
      Exit;

  PropCount := GetStringPropList(ImplementationParamsClass.ClassInfo, PropList);
  if PropCount > 0 then
  begin
    try
      for Index := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[Index];
        if (PropInfo^.Index > 0) and (PropInfo^.StoredProc <> nil) then
          raise EJclContainerException.CreateFmt('Invalid interface handler for attribute %s in %s',
                                                 [GetEnumName(TypeInfo(TAllTypeAttributeID), PropInfo^.Index),
                                                  ImplementationParamsClass.ClassName]);
      end;
      if PrototypeName <> '' then
      begin
        Index := Length(GlobalImplementationParams);
        SetLength(GlobalImplementationParams, Index + 1);
        GlobalImplementationParams[Index].ParamsName := PrototypeName;
        GlobalImplementationParams[Index].ParamsCount := PropCount;
        GlobalImplementationParams[Index].ParamsClass := ImplementationParamsClass;
        GlobalImplementationParams[Index].InterfaceParamsClass := InterfaceParamsClass;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure FindContainerParams(const PrototypeName: string;
  out InterfaceParamsClass: TJclInterfaceParamsClass;
  out ImplementationParamsClass: TJclImplementationParamsClass);
var
  ParamsCount, Index: Integer;
  MacroName: string;
begin
  InterfaceParamsClass := nil;
  ImplementationParamsClass := nil;
  Index := Pos('`', PrototypeName);
  if Index > 0 then
  begin
    MacroName := Copy(PrototypeName, 1, Index - 1);
    ParamsCount := StrToInt(Copy(PrototypeName, Index + 1, Length(PrototypeName) - Index));
  end
  else
  begin
    MacroName := PrototypeName;
    ParamsCount := -1;
  end;
  InterfaceParamsClass := nil;
  ImplementationParamsClass := nil;
  for Index := Low(GlobalInterfaceParams) to High(GlobalInterfaceParams) do
    if AnsiSameText(MacroName, GlobalInterfaceParams[Index].ParamsName) and
       ((ParamsCount = -1) or (ParamsCount = GlobalInterfaceParams[Index].ParamsCount)) then
  begin
    InterfaceParamsClass := GlobalInterfaceParams[Index].ParamsClass;
    ImplementationParamsClass := nil;
    Exit;
  end;
  for Index := Low(GlobalImplementationParams) to High(GlobalImplementationParams) do
    if AnsiSameText(MacroName, GlobalImplementationParams[Index].ParamsName) and
       ((ParamsCount = -1) or (ParamsCount = GlobalImplementationParams[Index].ParamsCount)) then
  begin
    InterfaceParamsClass := GlobalImplementationParams[Index].InterfaceParamsClass;
    ImplementationParamsClass := GlobalImplementationParams[Index].ParamsClass;
    Exit;
  end;
end;

procedure CheckJclContainers;
var
  Index: TAllTypeAttributeID;
begin
  for Index := Low(TAllTypeAttributeID) to High(TAllTypeAttributeID) do
    if (Index <> taTypeName) and (Index <> maMapAncestorClassName) and
       // exclude key and value attribute ID that are aliases to standard type ID
       ((Index < Low(TKeyAttributeID)) or (Index > High(TKeyAttributeID))) and
       ((Index < Low(TValueAttributeID)) or (Index > High(TValueAttributeID))) and
       (GlobalTypeAttributeHandlers[Index] = nil) then
      raise EJclContainerException.CreateFmt('No handler found for attribute %s',
                                             [GetEnumName(TypeInfo(TAllTypeAttributeID), Integer(Index))]);
end;

//function FindContainerParams(TypeInformationID: TTypeAttributeID): TJclContainerParamsClass; overload;
//begin

//end;

//function FindContainerParams(const PrototypeName: string): TJclContainerParamsClass; overload;
//begin

//end;

//=== { TJclContainerParams } ================================================

constructor TJclContainerParams.Create;
begin
  inherited Create;
  FTypeInfo := TJclContainerTypeInfo.Create;
  FMapInfo := TJclContainerMapInfo.Create;
end;

destructor TJclContainerParams.Destroy;
begin
  FMapInfo.Free;
  FTypeInfo.Free;
  inherited Destroy;
end;

function TJclContainerParams.ExpandMacro(const AName: string;
  const ParamValues: TDynStringArray): string;
var
  InterfaceParamsClass: TJclInterfaceParamsClass;
  ImplementationParamsClass: TJclImplementationParamsClass;
  InterfaceParams: TJclInterfaceParams;
  ImplementationParams: TJclImplementationParams;
  AMacroName, AMacroText: string;
  AMacro: IJclStrList;
  AMacroParams: TDynWideStringArray;
  Params: array of TVarRec;
  Index: Integer;
  PropInfo: PPropInfo;
  AliasAttributeID: TAllTypeAttributeID;
  AliasAttributeIDs: TAllTypeAttributeIDs;
  AliasTypeIndex, AliasMapIndex: Integer;
  AliasTypeAttributes: PKnownTypeAttributes;
  AliasMapAttributes: PKnownMapAttributes;
begin
  SetLength(AMacroParams, 0);
  FindContainerParams(AName, InterfaceParamsClass, ImplementationParamsClass);
  if InterfaceParamsClass <> nil then
  begin
    InterfaceParams := InterfaceParamsClass.Create;
    if InterfaceParams is TJclContainerInterfaceParams then
      TJclContainerInterfaceParams(InterfaceParams).TypeInfo := FTypeInfo;
    if InterfaceParams is TJclMapInterfaceParams then
      TJclMapInterfaceParams(InterfaceParams).MapInfo := FMapInfo;
    if ImplementationParamsClass <> nil then
      ImplementationParams := ImplementationParamsClass.Create(InterfaceParams)
    else
      ImplementationParams := nil;
    try
      AMacroName := Format('%s`%d', [AName, Length(ParamValues)]);
      AMacro := FindMacro(AMacroName);
      // the macro text is the last item, previous items are the macro parameter names
      AMacroText := AMacro.Strings[AMacro.Size - 1];
      AMacroParams := AssociateParameters(AMacro.SubList(0, AMacro.Size - 1), ParamValues);

      if ImplementationParams <> nil then
        AliasAttributeIDs := []
      else
        AliasAttributeIDs := InterfaceParams.AliasAttributeIDs;

      Result := '';

      if AliasAttributeIDs <> [] then
      begin
        // this is an alias declaration
        // replace the macro with some predefined text
        if (InterfaceParams is TJclContainerInterfaceParams) and FTypeInfo.KnownType then
        begin
          for AliasTypeIndex := Low(KnownAllTypes) to High(KnownAllTypes) do
          begin
            AliasTypeAttributes := KnownAllTypes[AliasTypeIndex];
            if AliasTypeAttributes^[taAlias] = FTypeInfo.TypeName then
            begin
              Result := Format('%s{$IFDEF %s}%s', [Result, AliasTypeAttributes^[taAliasCondition], NativeLineBreak]);
              for AliasAttributeID := Low(AliasAttributeID) to High(AliasAttributeID) do
                if AliasAttributeID in AliasAttributeIDs then
              begin
                  Result := Format('%s%s = %s;%s',
                     [Result, FTypeInfo.TypeAttributes[AliasAttributeID], AliasTypeAttributes^[AliasAttributeID], NativeLineBreak]);
              end;
              Result := Format('%s{$ENDIF %s}%s', [Result, AliasTypeAttributes^[taAliasCondition], NativeLineBreak]);
            end;
          end;
        end
        else
        if (InterfaceParams is TJclMapInterfaceParams) and FMapInfo.KnownMap then
        begin
          for AliasMapIndex := Low(KnownAllMaps) to High(KnownAllMaps) do
          begin
            AliasMapAttributes := KnownAllMaps[AliasMapIndex];
            // key alias
            if (AliasMapAttributes^.KeyAttributes[taAlias] = FMapInfo.KeyTypeInfo.TypeName) and
               (AliasMapAttributes^.ValueAttributes[taTypeName] = FMapInfo.ValueTypeInfo.TypeName) then
            begin
              Result := Format('%s{$IFDEF %s}%s', [Result, AliasMapAttributes^.KeyAttributes[taAliasCondition], NativeLineBreak]);
              for AliasAttributeID := Low(AliasAttributeID) to High(AliasAttributeID) do
                if AliasAttributeID in AliasAttributeIDs then
              begin
                Result := Format('%s%s = %s;%s',
                   [Result, FMapInfo.MapAttributes[AliasAttributeID], AliasMapAttributes^.MapAttributes[AliasAttributeID], NativeLineBreak]);
              end;
              Result := Format('%s{$ENDIF %s}%s', [Result, AliasMapAttributes^.KeyAttributes[taAliasCondition], NativeLineBreak]);
            end
            else
            // value alias
            if (AliasMapAttributes^.ValueAttributes[taAlias] = FMapInfo.ValueTypeInfo.TypeName) and
               (AliasMapAttributes^.KeyAttributes[taTypeName] = FMapInfo.KeyTypeInfo.TypeName) then
            begin
              Result := Format('%s{$IFDEF %s}%s', [Result, AliasMapAttributes^.ValueAttributes[taAliasCondition], NativeLineBreak]);
              for AliasAttributeID := Low(AliasAttributeID) to High(AliasAttributeID) do
                if AliasAttributeID in AliasAttributeIDs then
              begin
                Result := Format('%s%s = %s;%s',
                   [Result, FMapInfo.MapAttributes[AliasAttributeID], AliasMapAttributes^.MapAttributes[AliasAttributeID], NativeLineBreak]);
              end;
              Result := Format('%s{$ENDIF %s}%s', [Result, AliasMapAttributes^.ValueAttributes[taAliasCondition], NativeLineBreak]);
            end
            else
            // both aliases
            if (AliasMapAttributes^.KeyAttributes[taAlias] = FMapInfo.KeyTypeInfo.TypeName) and
               (AliasMapAttributes^.ValueAttributes[taAlias] = FMapInfo.ValueTypeInfo.TypeName) then
            begin
              if AliasMapAttributes^.KeyAttributes[taAliasCondition] <> AliasMapAttributes^.ValueAttributes[taAliasCondition] then
                Result := Format('%s{$IFDEF %s}%s{$IFDEF %s}%s',
                  [Result,
                   AliasMapAttributes^.KeyAttributes[taAliasCondition], NativeLineBreak,
                   AliasMapAttributes^.ValueAttributes[taAliasCondition], NativeLineBreak])
              else
                Result := Format('%s{$IFDEF %s}%s', [Result, AliasMapAttributes^.KeyAttributes[taAliasCondition], NativeLineBreak]);
              for AliasAttributeID := Low(AliasAttributeID) to High(AliasAttributeID) do
                if AliasAttributeID in AliasAttributeIDs then
              begin
                Result := Format('%s%s = %s;%s',
                   [Result, FMapInfo.MapAttributes[AliasAttributeID], AliasMapAttributes^.MapAttributes[AliasAttributeID], NativeLineBreak]);
              end;
              if AliasMapAttributes^.KeyAttributes[taAliasCondition] <> AliasMapAttributes^.ValueAttributes[taAliasCondition] then
                Result := Format('%s{$ENDIF %s}%s{$ENDIF %s}%s',
                  [Result,
                   AliasMapAttributes^.KeyAttributes[taAliasCondition], NativeLineBreak,
                   AliasMapAttributes^.ValueAttributes[taAliasCondition], NativeLineBreak])
              else
                Result := Format('%s{$ENDIF %s}%s', [Result, AliasMapAttributes^.KeyAttributes[taAliasCondition], NativeLineBreak]);
            end;
          end;
        end;
      end;

      if Result = '' then
      begin
        // expand the macro
        SetLength(Params, Length(ParamValues));
        for Index := Low(ParamValues) to High(ParamValues) do
        begin
          if AMacroParams[Index] = '' then
          begin
            // default to params
            if Assigned(ImplementationParams) then
            begin
              PropInfo := GetPropInfo(ImplementationParams, AMacro.Strings[Index]);
              if Assigned(PropInfo) then
                {$IFDEF COMPILER8_UP}
                AMacroParams[Index] := GetPropValue(ImplementationParams, PropInfo);
                {$ELSE ~COMPILER8_UP}
                AMacroParams[Index] := GetPropValue(ImplementationParams, AMacro.Strings[Index]);
                {$ENDIF ~COMPILER8_UP}
            end
            else
            begin
              PropInfo := GetPropInfo(InterfaceParams, AMacro.Strings[Index]);
              if Assigned(PropInfo) then
                {$IFDEF COMPILER8_UP}
                AMacroParams[Index] := GetPropValue(InterfaceParams, PropInfo)
                {$ELSE ~COMPILER8_UP}
                AMacroParams[Index] := GetPropValue(InterfaceParams, AMacro.Strings[Index])
                {$ENDIF ~COMPILER8_UP}
              else
                Beep;
            end;
          end;
          Params[Index].VType := vtPWideChar;
          Params[Index].VPWideChar := PWideChar(AMacroParams[Index]);
        end;
        Result := Format(AMacroText, Params);
      end;
      if Assigned(ImplementationParams) then
        Result := ImplementationParams.GetMacroHeader + Result + ImplementationParams.GetMacroFooter
      else
        Result := InterfaceParams.GetMacroHeader + Result + InterfaceParams.GetMacroFooter;

      // process conditional defines
      if (InterfaceParams is TJclMapInterfaceParams) and FMapInfo.KnownMap then
      begin
        ProcessDefines('KEY',
                       FMapInfo.KeyTypeInfo.TypeAttributes[taDefines],
                       FMapInfo.KeyTypeInfo.TypeAttributes[taUndefs]);
        ProcessDefines('VALUE',
                       FMapInfo.ValueTypeInfo.TypeAttributes[taDefines],
                       FMapInfo.ValueTypeInfo.TypeAttributes[taUndefs]);
        Result := ProcessConditional(Result, FMapInfo.KeyTypeInfo);
        if FMapInfo.KeyTypeInfo.TypeAttributes[taCondition] <> FMapInfo.ValueTypeInfo.TypeAttributes[taCondition] then
          Result := ProcessConditional(Result, FMapInfo.ValueTypeInfo);
      end
      else
      if (InterfaceParams is TJclContainerInterfaceParams) and FTypeInfo.KnownType then
      begin
        ProcessDefines('',
                       FTypeInfo.TypeAttributes[taDefines],
                       FTypeInfo.TypeAttributes[taUndefs]);
        Result := ProcessConditional(Result, FTypeInfo);
      end;
    finally
      ImplementationParams.Free;
      InterfaceParams.Free;
    end;
  end
  else
    Result := inherited ExpandMacro(AName, ParamValues);
end;

function TJclContainerParams.GetAllMapCount: Integer;
begin
  Result := Length(KnownAllMaps);
end;

function TJclContainerParams.GetHelpAllMapCount: Integer;
begin
  Result := Length(KnownAllMaps) - 1;
end;

function TJclContainerParams.GetAllTypeCount: Integer;
begin
  Result := Length(KnownAllTypes);
end;

function TJclContainerParams.GetHelpAllTypeCount: Integer;
begin
  Result := Length(KnownAllTypes) - 1;
end;

function TJclContainerParams.GetTrueMapCount: Integer;
begin
  Result := Length(KnownTrueMaps);
end;

function TJclContainerParams.GetHelpTrueMapCount: Integer;
begin
  Result := Length(KnownTrueMaps) - 1;
end;

function TJclContainerParams.GetTrueTypeCount: Integer;
begin
  Result := Length(KnownTrueTypes);
end;

function TJclContainerParams.GetHelpTrueTypeCount: Integer;
begin
  Result := Length(KnownTrueTypes) - 1;
end;

function TJclContainerParams.ProcessConditional(const MacroText: string; ContainerTypeInfo: TJclContainerTypeInfo): string;
var
  Condition: string;
begin
  Result := MacroText;
  Condition := ContainerTypeInfo.TypeAttributes[taCondition];
  if Condition <> '' then
    Result := Format('{$IFDEF %s}%s%s%s{$ENDIF %s}%s',
      [Condition, NativeLineBreak,
       Result, NativeLineBreak,
       Condition, NativeLineBreak]);
end;

procedure TJclContainerParams.ProcessDefines(const Prefix, Defines, Undefs: string);
var
  DefineList: TStrings;
  I: Integer;
begin
  DefineList := TStringList.Create;
  try
    StrToStrings(Defines, ';', DefineList, False);
    for I := 0 to DefineList.Count - 1 do
      Define(Prefix + DefineList.Strings[I]);
    StrToStrings(Undefs, ';', DefineList, False);
    for I := 0 to DefineList.Count - 1 do
      Undef(Prefix + DefineList.Strings[I]);
  finally
    DefineList.Free;
  end;
end;

procedure TJclContainerParams.SetAllMapIndex(const Value: Integer);
begin
  FAllMapIndex := Value;
  if (Value >= Low(KnownAllMaps)) and (Value <= High(KnownAllMaps)) then
  begin
    FMapInfo.KeyTypeInfo.TypeName := KnownAllMaps[Value]^.KeyAttributes[taTypeName];
    FMapInfo.ValueTypeInfo.TypeName := KnownAllMaps[Value]^.ValueAttributes[taTypeName];
    FMapInfo.KnownMap := True;
  end
  else
  begin
    FMapInfo.KeyTypeInfo.TypeName := '';
    FMapInfo.ValueTypeInfo.TypeName := '';
    FMapInfo.KnownMap := False;
  end;
end;

procedure TJclContainerParams.SetAllTypeIndex(const Value: Integer);
begin
  FAllTypeIndex := Value;
  if (Value >= Low(KnownAllTypes)) and (Value <= High(KnownAllTypes)) then
  begin
    FTypeInfo.TypeName := KnownAllTypes[Value]^[taTypeName];
    FTypeInfo.KnownType := True;
  end
  else
  begin
    FTypeInfo.TypeName := '';
    FTypeInfo.KnownType := False;
  end;
end;

procedure TJclContainerParams.SetMapTypeIndex(const Value: Integer);
begin
  FTrueMapIndex := Value;
  if (Value >= Low(KnownTrueMaps)) and (Value <= High(KnownTrueMaps)) then
  begin
    FMapInfo.KeyTypeInfo.TypeName := KnownTrueMaps[Value]^.KeyAttributes[taTypeName];
    FMapInfo.ValueTypeInfo.TypeName := KnownTrueMaps[Value]^.ValueAttributes[taTypeName];
    FMapInfo.KnownMap := True;
  end
  else
  begin
    FMapInfo.KeyTypeInfo.TypeName := '';
    FMapInfo.ValueTypeInfo.TypeName := '';
    FMapInfo.KnownMap := False;
  end;
end;

procedure TJclContainerParams.SetTrueTypeIndex(const Value: Integer);
begin
  FTrueTypeIndex := Value;
  if (Value >= Low(KnownTrueTypes)) and (Value <= High(KnownTrueTypes)) then
  begin
    FTypeInfo.TypeName := KnownTrueTypes[Value]^[taTypeName];
    FTypeInfo.KnownType := True;
  end
  else
  begin
    FTypeInfo.TypeName := '';
    FTypeInfo.KnownType := False;
  end;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  ClearRegisteredContainers;

end.
