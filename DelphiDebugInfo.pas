unit DelphiDebugInfo;

interface

uses
  WinApi.Windows, System.SysUtils, System.Classes, DebugInfo, Debuger,
  DebugerTypes, JclTD32Ex;

type
  TDelphiVersion = (dvAuto = 0, dvD1 = 8, dvD2 = 9, dvD3 = 10, dvD4 = 12,
    dvD5 = 13, dvD6 = 14, dvD7 = 15, dvD8 = 16, dvD2005 = 17, dvD2006_7 = 18,
    dvD2009 = 20, dvD2010 = 21, dvDXE = 22, dvDXE2 = 23, dvDXE3 = 24,
    dvDXE4 = 25, dvDXE5 = 26, dvDXE6 = 27, dvDXE7 = 28, dvDXE8 = 29);

  TDelphiDebugInfo = class(TDebugInfo)
  private
    FDelphiVersion: TDelphiVersion;
    FSystemUnits: TStringList;
    FAddressInfoList: TAddressInfoList;
    FIsHookSet: LongBool;

    function ImageBase: Cardinal;
    function ImageNames(const Index: TNameId): AnsiString;
    function LoadVar(UnitInfo: TUnitInfo; VarSymbol: TJclTD32NamedSymbol; Func: TFuncInfo): TVarInfo;
    procedure LoadFunc(UnitInfo: TUnitInfo; FuncSymbol: TJclTD32ProcSymbolInfo);
    procedure LoadSymbols(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
    function GetUnitFileName(const UnitName: String): String;
    procedure LoadConst(OwnerInfo: TSegmentCodeInfo; ConstSymbol: TJclTD32ConstantSymbolInfo);
    procedure LoadSourceLines(UnitInfo: TUnitInfo; UnitSourceModuleInfo: TUnitSourceModuleInfo; Source: TJclTD32SourceModuleInfo);
    procedure LoadSegments(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
    procedure LoadSourceModules(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
    function LoadType(UnitInfo: TUnitInfo; const TypeIndex: Integer; out DstType: TTypeInfo): Integer;
    procedure LoadUsedUnits(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
    function RegisterIndex(const Index: Byte): Integer;

    procedure InitSegments;

    function ParseUnit(Module: TJclTD32ModuleInfo): TUnitInfo;

    procedure ResolveUnits;

    function FindUnitByAddr(const Addr: Pointer): TUnitInfo;
    function FindFuncByAddr(const UnitInfo: TUnitInfo; const Addr: Pointer): TFuncInfo;
    function FindLineByAddr(const FuncInfo: TFuncInfo; const Addr: Pointer; const GetPrevLine: LongBool = False): TLineInfo;

    function CustomVariantAsString(const Value: Variant): String;
    procedure SetDelphiVersion(const Value: TDelphiVersion);
    procedure InitCodeTracking(const SetBP: LongBool);
    procedure FillSystemUnits;

  Protected
    FImage: TJclPeBorTD32Image;

    function GetDBGFileName(const FileName: String): String;

    function DoReadDebugInfo(Const FileName: String; ALoadDebugInfo: LongBool): LongBool; Override;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    function GetNameById(const Idx: TNameId): AnsiString; override;

    function ParseUnitName(UnitInfo: TUnitInfo; const WithExt: LongBool = True): String; override;
    function ParseFuncName(FuncInfo: TFuncInfo): String; override;
    function ParseTypeName(TypeInfo: TTypeInfo): String; override;
    function ParseConstName(ConstInfo: TConstInfo): String; override;
    function ParseVarName(VarInfo: TVarInfo): String; override;
    function ParseStructMemberName(StructMember: TStructMember): String; override;

    procedure ClearDebugInfo; Override;

    function HasDebugInfo(Const FileName: String): LongBool; Override;

    function GetAddrInfo(Var Addr: Pointer; Const FileName: String; Line: Cardinal): TFindResult; Override;

    function GetLineInfo(const Addr: Pointer; Var UnitInfo: TUnitInfo; Var FuncInfo: TFuncInfo; Var LineInfo: TLineInfo; GetPrevLine: LongBool): TFindResult; Override;

    function MakeFuncDbgFullName(Const ClassName, MethodName: AnsiString): AnsiString; Override;
    function MakeFuncShortName(Const MethodName: AnsiString): AnsiString; Override;
    function MakeFuncNativeName(Const MethodName: AnsiString): AnsiString; Override;

    function Evaluate(BriefMode: LongBool; Const Expression: String; Const TimeOut: Cardinal = INFINITE): String; Override;
    function EvaluateVariable(VarInfo: TVarInfo): Variant; override;

    function VarValueAsString(const Value: Variant): String; override;

    function GetSystemUnit: TUnitInfo;
    function GetMemoryManager: TVarInfo; virtual;
    function GetVMTClassName: TConstInfo;
    function SetDebugHook(const Value: Byte): LongBool;

    procedure SetMemoryManagerBreakpoints; Override;
    procedure ResetMemoryManagerBreakpoints; Override;

    procedure InitDebugHook; Override;

    function CheckAddr(Const Addr: Pointer): LongBool; Override;

    function GetClassName(Const ObjectPtr: Pointer): String; Override;
    function GetExceptionName(ExceptionRecord: PExceptionRecord): String; Override;
    function GetExceptionMessage(ExceptionRecord: PExceptionRecord; Const ThreadId: TThreadId): String; Override;
    function GetExceptionAddress(ExceptionRecord: PExceptionRecord): Pointer; Override;
    function GetExceptionFrame(ExceptionRecord: PExceptionRecord): Pointer; Override;
    function IsDelphiException(ExceptionRecord: PExceptionRecord): LongBool;
    function IsDelphiTraceException(ExceptionRecord: PExceptionRecord): LongBool;
    function CheckDebugException(ExceptionRecord: PExceptionRecord; Var IsTraceException: LongBool): LongBool; Override;
    function CheckSystemFile(Const FileName: String): LongBool; Override;

    property DelphiVersion: TDelphiVersion read FDelphiVersion write SetDelphiVersion;
  end;

Function HasDelphiDebugInfo(Const AFileName: String): LongBool;

Implementation

Uses
  JclDebug, JclPeImage, JclWin32,
  Math, Variants, classUtils, DebugHook, System.StrUtils, System.Contnrs, Vcl.Forms;

Const
  cContinuable = 0;
  cNonContinuable = 1;
  cDelphiException = DWORD($0EEDFADE);
  cDelphiReRaise = DWORD($0EEDFADF);
  cDelphiExcept = DWORD($0EEDFAE0);
  cDelphiFinally = DWORD($0EEDFAE1);
  cDelphiTerminate = DWORD($0EEDFAE2);
  cDelphiUnhandled = DWORD($0EEDFAE3);
  cNonDelphiException = DWORD($0EEDFAE4);
  cDelphiExitFinally = DWORD($0EEDFAE5);

Function HasDelphiDebugInfo(Const AFileName: String): LongBool;
Var
  PEImage: TJclPeBorTD32Image;
begin
  Result := FileExists(AFileName);
  If Result Then
  begin
    PEImage := TJclPeBorTD32Image.Create(True);
    Try
      PEImage.FileName := AFileName;
      Result := PEImage.IsTD32DebugPresent;
    Finally
      PEImage.Free;
    end;
  end;
end;

{ TDelphiDebugInfo }

Constructor TDelphiDebugInfo.Create;
begin
  Inherited Create;

  FImage := Nil;
  FDelphiVersion := dvAuto;
  FSystemUnits := TStringList.Create;
  FAddressInfoList := TAddressInfoList.Create(16 * 1024);
  FIsHookSet := False;

  FillSystemUnits;
end;

function TDelphiDebugInfo.CustomVariantAsString(const Value: Variant): String;
//var
//  CustomVariantData: ICustomVariantData;
//  ToStringData: TToStringData;
begin
  Result := '#VALUE#';

  //TODO:

//  If Supports(IUnknown(TVarData(Value).VUnknown), ICustomVariantData, CustomVariantData) Then
//  begin
//    ToStringData.DebugInfo := Self;
//    ToStringData.Mode := tsmBrief;
//    ToStringData.RecursionLevel := 0;
//
//    Result := CustomVariantData.AsString(ToStringData);
//  end
//  Else
//    Result := 'Unsupported data type';
end;

Destructor TDelphiDebugInfo.Destroy;
begin
  ClearDebugInfo;

  FreeAndNil(FImage);
  FreeAndNil(FSystemUnits);
  FreeAndNil(FAddressInfoList);

  Inherited Destroy;
end;

function TDelphiDebugInfo.ParseConstName(ConstInfo: TConstInfo): String;
var
  SL: TStringArray;
begin
  Result := inherited;

  if FUseShortNames then
  begin
    SplitStr(Result, '@', SL);
    Result := SL[ High(SL)];
  end;
end;

function TDelphiDebugInfo.ParseFuncName(FuncInfo: TFuncInfo): String;
var
  SL: TStringArray;
  Idx: Integer;
  S: String;
  P: Integer;
begin
  Result := inherited;

  if FUseShortNames then
  begin
    SplitStr(Result, '@', SL);

    Result := '';
    for Idx := 0 to High(SL) do
    begin
      S := SL[Idx];
      if S <> '' then
      begin
        if Result <> '' then
          Result := Result + '.';

        P := Pos('$qq', S);
        if P > 0 then
          SetLength(S, P - 1);

        Result := Result + S;

        if P > 0 then
          Break;
      end;
    end;
  end;
end;

function TDelphiDebugInfo.ParseStructMemberName(StructMember: TStructMember): String;
begin
  Result := inherited;
end;

function TDelphiDebugInfo.ParseTypeName(TypeInfo: TTypeInfo): String;
var
  SL: TStringArray;
  Idx: Integer;
  S: String;
  P: Integer;
begin
  Result := inherited;

  if FUseShortNames then
  begin
    SplitStr(Result, '@', SL);

    Result := '';
    for Idx := 0 to High(SL) do
    begin
      S := SL[Idx];
      if S <> '' then
      begin
        if Result <> '' then
          Result := Result + '.';

        repeat
          P := Pos('$', S);
          if P > 0 then
            Delete(S, P, 3);
        until P <= 0;

        Result := Result + S;
      end;
    end;
  end;
end;

Function TDelphiDebugInfo.ParseUnit(Module: TJclTD32ModuleInfo): TUnitInfo;
begin
  Result := TUnitInfo.Create;
  Result.SymbolInfo := Module;

  Result.NameId := Module.NameIndex;

  Units.AddObject(Result.ShortName, Result);
  UnitsByAddr.Add(Result);

  LoadSegments(Result, Module);
  LoadUsedUnits(Result, Module);
  LoadSymbols(Result, Module);
  LoadSourceModules(Result, Module);
end;

function TDelphiDebugInfo.ParseUnitName(UnitInfo: TUnitInfo; const WithExt: LongBool = True): String;
begin
  Result := inherited;
end;

function TDelphiDebugInfo.ParseVarName(VarInfo: TVarInfo): String;
var
  SL: TStringArray;
  Idx: Integer;
begin
  Result := inherited;

  if FUseShortNames then
  begin
    SplitStr(Result, '@', SL);

    Result := '';
    for Idx := 0 to High(SL) do
      if SL[Idx] <> '' then
      begin
        if Result <> '' then
          Result := Result + '.';

        Result := Result + SL[Idx];
      end;
  end;
end;
{ ............................................................................... }

Function TDelphiDebugInfo.GetUnitFileName(Const UnitName: String): String;
Var
  S: String;
  Ext: String;
  ST: TUnitType;
begin
  S := AnsiLowerCase(ExtractFileName(UnitName));

  Ext := ExtractFileExt(S);
  If (Ext <> '.pas') and (Ext <> '.inc') and (Ext <> '.dpr') Then
    S := S + '.pas';

  for ST := Low(TUnitType) to High(TUnitType) do
    if Dirs[ST].TryGetValue(S, Result) then
      Exit;

  Result := S;
end;

function TDelphiDebugInfo.GetVMTClassName: TConstInfo;
const
  _vmtClassName = 'vmtClassName';
Var
  USystem: TUnitInfo;
begin
  Result := Nil;

  USystem := GetSystemUnit;
  if Assigned(USystem) then
    Result := USystem.FindConstByName(_vmtClassName, True);
end;

function TDelphiDebugInfo.GetNameById(const Idx: TNameId): AnsiString;
begin
  Result := ImageNames(Idx);
end;

function TDelphiDebugInfo.GetSystemUnit: TUnitInfo;
const
  _SystemUnit: String = 'system.pas';
var
  I: Integer;
begin
  for I := 0 to Units.Count - 1 do
  begin
    Result := TUnitInfo(Units.Objects[I]);
    if SameText(_SystemUnit, Result.ShortName) then
      Exit;
  end;

  Result := Nil;
end;

procedure TDelphiDebugInfo.LoadSegments(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
var
  I: Integer;
  SegmentInfo: TSegmentInfo;
  S: TUnitSegmentInfo;
begin
  UnitInfo.Segments.Capacity := Module.SegmentCount;
  For I := 0 To Module.SegmentCount - 1 Do
  begin
    SegmentInfo := Module.Segment[I];

    S := TUnitSegmentInfo.Create;
    S.Address := Pointer(SegmentInfo.Offset + FImage.ImageSectionHeaders[SegmentInfo.Segment - 1].VirtualAddress + ImageBase);

    S.Size := SegmentInfo.Size;

    //   $0000  Data segment
    //   $0001  Code segment
    case SegmentInfo.Flags of
      $0000:
        S.SegmentClassInfo := GetSegmentByType(ustData);
      $0001:
        S.SegmentClassInfo := GetSegmentByType(ustCode);
    else
      RaiseDebugCoreException();
    end;

    if Assigned(S.SegmentClassInfo) then
    begin
      if (S.SegmentClassInfo.SegType = ustCode) and
        ((UnitInfo.Address = Nil) or (Cardinal(UnitInfo.Address) > Cardinal(S.Address)))
      then
        UnitInfo.Address := Pointer(S.Address);

      case S.SegmentClassInfo.SegType of
        ustData:
          Inc(UnitInfo.Size, S.Size);
        ustCode:
          Inc(UnitInfo.Size, S.Size);
      end;
    end
    else
      RaiseDebugCoreException('');

    UnitInfo.Segments.Add(S);
  end;
end;

procedure TDelphiDebugInfo.LoadSourceLines(UnitInfo: TUnitInfo; UnitSourceModuleInfo: TUnitSourceModuleInfo; Source: TJclTD32SourceModuleInfo);
Var
  I: Integer;
  LineInfo: TJclTD32LineInfo;
  L: TLineInfo;
  F: TFuncInfo;
begin
  UnitSourceModuleInfo.Lines.Capacity := Source.LineCount;
  For I := 0 To Source.LineCount - 1 Do
  begin
    LineInfo := Source.Line[I];

    L := TLineInfo.Create;
    L.LineNo := LineInfo.LineNo; // - 1; ???
    L.Address := Pointer(LineInfo.Offset + FImage.ImageSectionHeaders[LineInfo.Segment - 1].VirtualAddress + ImageBase);
    L.SrcSegment := UnitSourceModuleInfo;

    UnitSourceModuleInfo.Lines.Add(L);

    UnitInfo.Lines.Add(L);

    F := FindFuncByAddr(UnitInfo, L.Address);
    If F <> Nil Then
      F.Lines.Add(L);
  end;
end;

procedure TDelphiDebugInfo.LoadSourceModules(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
Var
  I: Integer;
  SourceModuleInfo: TJclTD32SourceModuleInfo;
  SM: TUnitSourceModuleInfo;
begin
  For I := 0 To Module.SourceModuleCount - 1 Do
  begin
    SourceModuleInfo := Module.SourceModules[I];

    SM := TUnitSourceModuleInfo.Create;
    SM.UnitInfo := UnitInfo;
    SM.NameId := SourceModuleInfo.NameIndex;
    SM.SymbolInfo := SourceModuleInfo;

    //LoadLines(UnitInfo, SourceModuleInfo);
    LoadSourceLines(UnitInfo, SM, SourceModuleInfo);

    UnitInfo.SourceSegments.Add(SM);
  end;
end;

procedure TDelphiDebugInfo.LoadUsedUnits(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
var
  I: Integer;
  Idx: Integer;
  Name: String;
  UName: String;
begin
  UnitInfo.UsedUnits.Capacity := Module.UsedModuleNameIndexCount;
  For I := 0 To Module.UsedModuleNameIndexCount - 1 Do
  begin
    Idx := Module.UsedModuleNameIndices[I];
    Name := String(ImageNames(Idx));
    UName := GetUnitFileName(Name);
    UnitInfo.UsedUnits.Add(UName);
  end;
end;

(*
procedure TDelphiDebugInfo.LoadLines(UnitInfo: TUnitInfo; Source: TJclTD32SourceModuleInfo);
var
  I: Integer;
  LineInfo: TJclTD32LineInfo;
  L: TLineInfo;
  F: TFuncInfo;
begin
  UnitInfo.Lines.Capacity := UnitInfo.Lines.Capacity + Source.LineCount;
  For I := 0 To Source.LineCount - 1 Do
  begin
    LineInfo := Source.Line[I];

    L := TLineInfo.Create;
    L.LineNo := LineInfo.LineNo; // - 1; ???
    L.Address := Pointer(LineInfo.Offset + FImage.ImageSectionHeaders[LineInfo.Segment - 1].VirtualAddress + ImageBase);
    UnitInfo.Lines.Add(L);

    F := FindFuncByAddr(UnitInfo, L.Address);
    If F <> Nil Then
      F.Lines.Add(L);
  end;
end;
*)

const
  _DefJclSymbolTypeKindToTypeKind: array[Low(TJclSymbolTypeKind) .. High(TJclSymbolTypeKind)] of TTypeKind = (
    tkBoolean, tkWordBool, tkLongBool, tkShortInt,
    tkSmallInt, tkInteger, tkInt64, tkByte, tkWord, tkCardinal, tkUInt64,
    tkSingle, tkReal48, tkReal, tkExtended, tkCurrency, tkComplex, tkPString,
    tkLString, tkWString, tkChar, tkPointer, tkSubRange, tkArray, tkEnum,
    tkStructure, tkClass, tkSet, tkVariant, tkProperty, tkFieldList, tkClosure,
    tkClassRef, tkWideChar, tkProcedure, tkArgList, tkMFunction, tkVoid);


Function TDelphiDebugInfo.LoadType(UnitInfo: TUnitInfo; const TypeIndex: Integer; out DstType: TTypeInfo): Integer;
Var
  SrcType: TJclSymbolTypeInfo;

  procedure _LoadPointerType;
  begin
    If SrcType.ElementType <> 0 Then
    begin
      LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
      If DstType.BaseType.Kind = tkClass Then
        DstType.Kind := tkObject
      Else If (DstType.BaseType.Kind = tkArray) And (DstType.BaseType.DataSize = -1) Then
        DstType.Kind := tkDynamicArray;
    end;
  end;

  procedure _LoadClassType;
  var
    I, J: Integer;
    SrcList: TJclSymbolTypeInfo;
    SrcMember: TJclTD32MemberSymbolInfo;
    SrcMemberType: TJclSymbolTypeInfo;
    DstMember: TStructMember;
    DstTypeMember: TStructMember;
  begin
    SrcList := FImage.TD32Scanner.SymbolTypes[SrcType.Elements];
    If SrcList.ElementType <> 0 Then
      LoadType(UnitInfo, SrcList.ElementType, DstType.BaseType);

    DstType.Members := TNameList.Create;
    DstType.Members.Capacity := SrcList.Members.Count;
    For I := 0 To SrcList.Members.Count - 1 Do
    begin
      //DstMember := Nil;
      SrcMember := TJclTD32MemberSymbolInfo(SrcList.Members[I]);

      SrcMemberType := FImage.TD32Scanner.SymbolTypes[SrcMember.TypeIndex];

      if SrcMemberType = Nil then
      begin
        // TODO: Что-то здесь непонятное в XE4 появилось
        Continue;
      end;

      DstMember := TStructMember.Create;
      DstMember.NameId := SrcMember.NameIndex;
      DstMember.SymbolInfo := SrcMember;

      Case SrcMember.Flags And 3 Of
        0, 3:
          DstMember.Scope := msPublic;
        1:
          DstMember.Scope := msPrivate;
        2:
          DstMember.Scope := msProtected;
      end;

      If SrcMemberType.Kind = stkClassRef Then
        LoadType(UnitInfo, SrcMemberType.ElementType, DstMember.DataType);

      If SrcMemberType.Kind <> stkProperty Then
      begin
        If SrcMemberType.Kind <> stkClassRef Then
          LoadType(UnitInfo, SrcMember.TypeIndex, DstMember.DataType);
        DstMember.Offset := SrcMember.Offset;
        DstMember.DataSize := DstMember.DataType.DataSize;
      End
      Else
      begin
        LoadType(UnitInfo, SrcMemberType.ElementType, DstMember.DataType);

        DstMember.IsDefault := (SrcMemberType.Flags And 1) = 1;

        If (SrcMemberType.Flags And 2) = 2 Then
          DstMember.MethodNameId := SrcMemberType.MinValue
        Else
        begin
          DstMember.Offset := SrcMemberType.MinValue;
          DstMember.DataSize := DstMember.DataType.DataSize;
        end;

        For J := 0 To DstType.Members.Count - 1 Do
        begin
          DstTypeMember := TStructMember(DstType.Members[J]);
          If DstTypeMember.Offset = SrcMemberType.MinValue Then
          begin
            // TODO: Возможно, надо здесь надо указывать на всю структуру DstTypeMember
            DstMember.AliasNameId := DstTypeMember.NameId;
            Break;
          end;
        end;
      end;

      If DstMember <> Nil Then
        DstType.Members.Add(DstMember);
    end;
  end;

  procedure _LoadStructureType;
  var
    I: Integer;
    SrcList: TJclSymbolTypeInfo;
    DstMember: TStructMember;
    SrcMember: TJclTD32MemberSymbolInfo;
  begin
    SrcList := FImage.TD32Scanner.SymbolTypes[SrcType.Elements];

    DstType.Members := TNameList.Create;
    DstType.Members.Capacity := SrcList.Members.Count;
    For I := 0 To SrcList.Members.Count - 1 Do
    begin
      SrcMember := TJclTD32MemberSymbolInfo(SrcList.Members[I]);

      DstMember := TStructMember.Create;
      DstMember.NameId := SrcMember.NameIndex;
      DstMember.SymbolInfo := SrcMember;
      DstMember.Scope := msPublic;

      LoadType(UnitInfo, SrcMember.TypeIndex, DstMember.DataType);

      DstMember.Offset := SrcMember.Offset;
      DstMember.DataSize := DstMember.DataType.DataSize;

      DstType.Members.Add(DstMember);
    end;
  end;

  procedure _LoadEnumType;
  var
    I: Integer;
    SrcList: TJclSymbolTypeInfo;
    SrcEnum: TJclEnumerateSymbolInfo;
    EnumMember: TEnumInfo;
  begin
    DstType.Elements := TNameList.Create;

    DstType.DataSize := FImage.TD32Scanner.SymbolTypes[SrcType.ElementType].DataSize;
    DstType.MinValue := High(DstType.MinValue);
    DstType.MaxValue := Low(DstType.MaxValue);

    SrcList := FImage.TD32Scanner.SymbolTypes[SrcType.Elements];
    DstType.Elements.Capacity := SrcList.Members.Count;
    For I := 0 To SrcList.Members.Count - 1 Do
    begin
      SrcEnum := TJclEnumerateSymbolInfo(SrcList.Members[I]);

      EnumMember := TEnumInfo.Create;
      EnumMember.NameId := SrcEnum.NameIndex;
      EnumMember.SymbolInfo := SrcEnum;

      EnumMember.TypeInfo := DstType;
      EnumMember.OrderValue := SrcEnum.Value;

      DstType.Elements.Add(EnumMember);

      If SrcEnum.Value < DstType.MinValue Then
        DstType.MinValue := SrcEnum.Value;
      If SrcEnum.Value > DstType.MaxValue Then
        DstType.MaxValue := SrcEnum.Value;
    end;
  end;

  procedure _LoadSubRangeType;
  var
    SrcList: TJclSymbolTypeInfo;
  begin
    DstType.MinValue := SrcType.MinValue;
    DstType.MaxValue := SrcType.MaxValue;

    SrcList := FImage.TD32Scanner.SymbolTypes[SrcType.IndexType];
    Case SrcList.Kind Of
      stkBoolean, stkWordBool, stkLongBool:
        Case DstType.DataSize Of
          1:
            DstType.Kind := tkBoolean;
          2:
            DstType.Kind := tkWordBool;
          4:
            DstType.Kind := tkLongBool;
        end;
      stkChar, stkWideChar:
        Case DstType.DataSize Of
          1:
            DstType.Kind := tkChar;
          2:
            DstType.Kind := tkWideChar;
        end;
    Else
      Case DstType.DataSize Of
        1:
          Case SrcList.Kind Of
            stkShortInt, stkSmallInt, stkInteger:
              DstType.Kind := tkShortInt;
          Else
            DstType.Kind := tkByte;
          end;
        2:
          Case SrcList.Kind Of
            stkShortInt, stkSmallInt, stkInteger:
              DstType.Kind := tkSmallInt;
          Else
            DstType.Kind := tkWord;
          end;
        4:
          Case SrcList.Kind Of
            stkShortInt, stkSmallInt, stkInteger:
              DstType.Kind := tkInteger;
          Else
            DstType.Kind := tkCardinal;
          end;
      end;
    end;
  end;

  procedure _LoadArrayType;
  var
    SrcList: TJclSymbolTypeInfo;
  begin
    LoadType(UnitInfo, SrcType.IndexType, DstType.IndexType);
    LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
    DstType.DataSize := SrcType.DataSize;

    SrcList := FImage.TD32Scanner.SymbolTypes[SrcType.IndexType];
    Case SrcList.Kind Of
      stkSubRange:
        begin
          DstType.MinValue := SrcList.MinValue;
          DstType.MaxValue := SrcList.MaxValue;
        End
      Else
      begin
        DstType.MinValue := DstType.IndexType.MinValue;
        DstType.MaxValue := DstType.IndexType.MaxValue;
      end;
    end;
  end;

begin
  SrcType := FImage.TD32Scanner.SymbolTypes[TypeIndex];

  If (SrcType <> Nil) and (SrcType.UnitInfo = UnitInfo) Then
  begin
    Result := SrcType.UnitInfoIndex;
    DstType := TTypeInfo(UnitInfo.Types[Result]);
    Exit;
  end;

  DstType := TTypeInfo.Create;
  DstType.Kind := tkVoid;
  DstType.DataSize := SizeOf(Pointer);
  DstType.UnitInfo := UnitInfo;
  DstType.NameId := -1;
  DstType.SymbolInfo := SrcType;

  DstType.TypeInfoIdx := UnitInfo.Types.Add(DstType);
  Result := DstType.TypeInfoIdx;

  if SrcType = Nil then
    Exit;

  SrcType.UnitInfo := UnitInfo;
  SrcType.UnitInfoIndex := DstType.TypeInfoIdx;

  DstType.NameId := SrcType.NameIndex;
  DstType.Kind := _DefJclSymbolTypeKindToTypeKind[SrcType.Kind];
  DstType.DataSize := SrcType.DataSize;

  Case SrcType.Kind Of
    stkBoolean:
      begin
        DstType.MinValue := 0;
        DstType.MaxValue := 1;
      end;
    stkWordBool:
      begin
        DstType.MinValue := 0;
        DstType.MaxValue := 1;
      end;
    stkLongBool:
      begin
        DstType.MinValue := 0;
        DstType.MaxValue := 1;
      end;
    stkShortInt:
      begin
        DstType.MinValue := Low(ShortInt);
        DstType.MaxValue := High(ShortInt);
      end;
    stkSmallInt:
      begin
        DstType.MinValue := Low(SmallInt);
        DstType.MaxValue := High(SmallInt);
      end;
    stkInteger:
      begin
        DstType.MinValue := Low(Integer);
        DstType.MaxValue := High(Integer);
      end;
    stkInt64: ;
    stkByte:
      begin
        DstType.MinValue := Low(Byte);
        DstType.MaxValue := High(Byte);
      end;
    stkWord:
      begin
        DstType.MinValue := Low(Word);
        DstType.MaxValue := High(Word);
      end;
    stkCardinal:
      begin
        DstType.MinValue := Low(Cardinal);
        Cardinal(DstType.MaxValue) := High(Cardinal);
      end;
    stkUInt64: ;
    stkSingle: ;
    stkReal48: ;
    stkReal: ;
    stkExtended: ;
    stkCurrency: ;
    stkComplex: ;
    stkPString:
      begin
        DstType.DataSize := SizeOf(ShortString);
        LoadType(UnitInfo, SrcType.IndexType, DstType.BaseType);
      end;
    stkLString:
      begin
        DstType.DataSize := SizeOf(AnsiString);
        LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
      end;
    stkWString:
      begin
        DstType.DataSize := SizeOf(WideString);
        LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
      end;
    stkChar:
      begin
        DstType.MinValue := 0;
        DstType.MaxValue := Ord(High(AnsiChar));
      end;
    stkPointer:
      begin
        _LoadPointerType;
      end;
    stkSubRange:
      begin
        _LoadSubRangeType;
      end;
    stkArray:
      begin
        _LoadArrayType;
      end;
    stkEnum:
      begin
        _LoadEnumType;
      end;
    stkStructure:
      begin
        _LoadStructureType;
      end;
    stkClass:
      begin
        _LoadClassType;
      end;
    stkSet:
      begin
        LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
        DstType.DataSize := SrcType.DataSize;
      end;
    stkVariant: ; // ???
    stkProperty:
      begin
        // TODO:
      end;
    stkFieldList: ; // ???
    stkClosure:
      begin
        DstType.Kind := tkPointer;
      end;
    stkClassRef:
      begin
        // TODO:
      end;
    stkWideChar:
      begin
        DstType.MinValue := Low(Word);
        DstType.MaxValue := High(Word);
      end;
    stkProcedure:
      begin
        // TODO: Params
      end;
    stkArgList: ;
    stkMFunction: ;
    stkVoid: ;
    else
      begin
        SrcType.Kind := SrcType.Kind;
        RaiseDebugCoreException();
      end;
  end;
end;

procedure TDelphiDebugInfo.LoadConst(OwnerInfo: TSegmentCodeInfo; ConstSymbol: TJclTD32ConstantSymbolInfo);
var
  ConstInfo: TConstInfo;
  TypeInfo: TJclSymbolTypeInfo;
  ConstName: String;

  procedure LoadExtended;
  //var
  //  ExtValue: Extended;
  begin
    //ExtValue := PExtended(ConstSymbol.Value)^;
    //TODO: ConstInfo.Value := IUnknown(TExtendedConstantValue.Create(ExtValue));
  end;

  procedure LoadSet;
  var
    SetValue: TBytes;
  begin
    SetLength(SetValue, 32);
    Move(ConstSymbol.Value^, SetValue[TypeInfo.MinValue], ConstSymbol.Size);
    LoadType(ConstInfo.UnitInfo, ConstSymbol.TypeIndex, ConstInfo.TypeInfo);
    //TODO: ConstInfo.Value := IUnknown(TSetVariantValue.Create(ConstInfo.TypeInfo, SetValue));
  end;

  procedure LoadSubRange;
  begin
    LoadType(ConstInfo.UnitInfo, ConstSymbol.TypeIndex, ConstInfo.TypeInfo);
    //TODO: ConstInfo.Value := GetValueNonRef(Nil, ConstInfo.TypeInfo, TUIntPtr(ConstSymbol.Value^), False);
  end;

begin
  ConstInfo := Nil;
  TypeInfo := FImage.TD32Scanner.SymbolTypes[ConstSymbol.TypeIndex];
  If TypeInfo <> Nil Then
    try
      ConstInfo := TConstInfo.Create;
      ConstInfo.Owner := OwnerInfo;
      ConstInfo.NameId := ConstSymbol.NameIndex;
      ConstInfo.SymbolInfo := ConstSymbol;
      LoadType(ConstInfo.UnitInfo, ConstSymbol.TypeIndex, ConstInfo.TypeInfo);

      Case TypeInfo.Kind Of
        stkBoolean:
          ConstInfo.Value := PBoolean(ConstSymbol.Value)^;
        stkWordBool:
          ConstInfo.Value := PWordBool(ConstSymbol.Value)^;
        stkLongBool:
          ConstInfo.Value := PBool(ConstSymbol.Value)^;
        stkShortInt:
          ConstInfo.Value := PShortInt(ConstSymbol.Value)^;
        stkSmallInt:
          ConstInfo.Value := PSmallInt(ConstSymbol.Value)^;
        stkInteger:
          ConstInfo.Value := PInteger(ConstSymbol.Value)^;
        stkInt64:
          ConstInfo.Value := PInt64(ConstSymbol.Value)^;
        stkByte:
          ConstInfo.Value := PByte(ConstSymbol.Value)^;
        stkWord:
          ConstInfo.Value := PWord(ConstSymbol.Value)^;
        stkCardinal:
          ConstInfo.Value := PCardinal(ConstSymbol.Value)^;
        stkUInt64:
          ConstInfo.Value := PUInt64(ConstSymbol.Value)^;
        stkSingle:
          ConstInfo.Value := PSingle(ConstSymbol.Value)^;
        stkReal48:
          ConstInfo.Value := PReal48(ConstSymbol.Value)^;
        stkReal:
          ConstInfo.Value := PReal(ConstSymbol.Value)^;
        stkExtended:
          LoadExtended;
        stkCurrency:
          ConstInfo.Value := PCurrency(ConstSymbol.Value)^;
        stkPointer:
          //TODO: ConstInfo.Value := IUnknown(TPointerConstantValue.Create(PPointer(ConstSymbol.Value)^));
          ConstInfo.Value := '#PTR_CONST#';
        stkLString, stkWString:
          ConstInfo.Value := '#STR_CONST#';
        stkSet:
          LoadSet;
        stkSubRange:
          LoadSubRange;
      Else
        FreeAndNil(ConstInfo);
      end;
    except
      on E: Exception do
      begin
        ConstName := String(ImageNames(ConstSymbol.NameIndex));
        RaiseDebugCoreException(Format('%s.%s', [ConstInfo.UnitInfo.Name, ConstName]));
      end;
    end;

  If ConstInfo <> Nil Then
  begin
    If ConstInfo.Owner is TFuncInfo Then
      TFuncInfo(ConstInfo.Owner).Consts.Add(ConstInfo)
    Else
      ConstInfo.UnitInfo.Consts.Add(ConstInfo);
  end;
end;

Function TDelphiDebugInfo.RegisterIndex(const Index: Byte): Integer;
begin
  Case Index Of
    1, 5, 9, 17:
      Result := 0;
    2, 6, 10, 18:
      Result := 1;
    3, 7, 11, 19:
      Result := 2;
    4, 8, 12, 20:
      Result := 3;
    13, 21:
      Result := 4;
    14, 22:
      Result := 5;
    15, 23:
      Result := 6;
    16, 24:
      Result := 7;
    31, 33:
      Result := 8;
  Else
    Result := -1;
  end;

  Case Index Of
    1, 2, 3, 4:
      Result := Result Or (1 Shl 4);
    5, 6, 7, 8:
      Result := Result Or (2 Shl 4);
    9, 10, 11, 12, 13, 14, 15, 16, 31:
      Result := Result Or (3 Shl 4);
  end;
end;

Function TDelphiDebugInfo.LoadVar(UnitInfo: TUnitInfo; VarSymbol: TJclTD32NamedSymbol; Func: TFuncInfo): TVarInfo;

  procedure LoadRegister(VarInfo: TVarInfo);
  Var
    I: Integer;
    RegInfo: TRegInfo;
    RegRange: PRegisterRange;
  begin
    VarInfo.VarKind := vkRegister;
    VarInfo.Offset := RegisterIndex(TJclTD32RegisterSymbolInfo(VarSymbol).Registers);
    VarInfo.RegisterRanges := TList.Create;

    For I := 0 To TJclTD32RegisterSymbolInfo(VarSymbol).RangeCount - 1 Do
    begin
      RegRange := TJclTD32RegisterSymbolInfo(VarSymbol).Range[I];

      RegInfo := TRegInfo.Create;
      RegInfo.StartOffset := Cardinal(Func.Address) + RegRange^.Start;
      RegInfo.EndOffset := RegInfo.StartOffset + RegRange^.Len;
      RegInfo.RegisterIndex := RegisterIndex(RegRange^.Registers);

      VarInfo.RegisterRanges.Add(RegInfo);
    end;
  end;

begin
  Result := TVarInfo.Create;
  Result.SymbolInfo := VarSymbol;

  Case VarSymbol.SymbolType Of
    SYMBOL_TYPE_REGISTER:
      begin
        LoadRegister(Result);
      end;
    SYMBOL_TYPE_BPREL32:
      begin
        Result.VarKind := vkStack;
        Result.Offset := TJclTD32BPRel32SymbolInfo(VarSymbol).Offset;
      end;
    SYMBOL_TYPE_LDATA32, SYMBOL_TYPE_GDATA32:
      begin
        Result.VarKind := vkGlobal;
        Result.Offset := TJclTD32DataSymbolInfo(VarSymbol).Offset + FImage.ImageSectionHeaders[TJclTD32DataSymbolInfo(VarSymbol).Segment - 1]
          .VirtualAddress + ImageBase;
      end;
    SYMBOL_TYPE_SLINK32:
      begin
        Result.VarKind := vkLink;
        Result.Offset := TJclTD32LinkSymbolInfo(VarSymbol).Offset;
      end;
  end;

  If VarSymbol.SymbolType = SYMBOL_TYPE_SLINK32 Then
  begin
    Result.NameId := -1;
  end
  Else
  begin
    Result.NameId := VarSymbol.NameIndex;
    LoadType(UnitInfo, VarSymbol.TypeIndex, Result.DataType);
  end;

  If Func <> Nil Then
  begin
    Result.Owner := Func;
    Func.Vars.Add(Result)
  end
  Else
  begin
    Result.Owner := UnitInfo;
    UnitInfo.Vars.Add(Result);
  end;
end;

procedure TDelphiDebugInfo.LoadSymbols(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
var
  I: Integer;
  SymbolInfo: TJclTD32SymbolInfo;
begin
  UnitInfo.Types.Capacity := 128;
  UnitInfo.Funcs.Capacity := 128;
  UnitInfo.FuncsByAddr.Capacity := 128;
  UnitInfo.Vars.Capacity := 32;
  UnitInfo.Consts.Capacity := 32;

  For I := 0 To Module.SymbolCount - 1 Do
  begin
    SymbolInfo := Module.Symbols[I];
    Case SymbolInfo.SymbolType Of
      SYMBOL_TYPE_PCONSTANT:
        LoadConst(UnitInfo, TJclTD32ConstantSymbolInfo(SymbolInfo));
      SYMBOL_TYPE_BPREL32, SYMBOL_TYPE_LDATA32, SYMBOL_TYPE_GDATA32, SYMBOL_TYPE_SLINK32:
        LoadVar(UnitInfo, TJclTD32NamedSymbol(SymbolInfo), Nil);
      SYMBOL_TYPE_LPROC32, SYMBOL_TYPE_GPROC32:
        LoadFunc(UnitInfo, TJclTD32ProcSymbolInfo(SymbolInfo));
    end;
  end;
end;

procedure TDelphiDebugInfo.LoadFunc(UnitInfo: TUnitInfo; FuncSymbol: TJclTD32ProcSymbolInfo);
var
  I: Integer;
  ProcInfo: TJclSymbolTypeInfo;
  FuncInfo: TFuncInfo;
  SymbolInfo: TJclTD32SymbolInfo;
  VarInfo: TVarInfo;
begin
  FuncInfo := TFuncInfo.Create;
  FuncInfo.NameId := FuncSymbol.NameIndex;
  FuncInfo.SymbolInfo := FuncSymbol;
  FuncInfo.Address := Pointer(FuncSymbol.Offset + FImage.ImageSectionHeaders[FuncSymbol.Segment - 1].VirtualAddress + ImageBase);
  FuncInfo.Size := FuncSymbol.Size;
  FuncInfo.UnitInfo := UnitInfo;
  FuncInfo.ID := FuncSymbol;
  FuncInfo.ParentID := FuncSymbol.Parent;

  ProcInfo := FImage.TD32Scanner.SymbolTypes[FuncSymbol.TypeIndex];
  LoadType(UnitInfo, ProcInfo.IndexType, FuncInfo.ResultType);

  FuncInfo.Vars.Capacity := 8;

  For I := 0 To FuncSymbol.SymbolCount - 1 Do
  begin
    SymbolInfo := FuncSymbol.Symbols[I];
    Case SymbolInfo.SymbolType Of
      SYMBOL_TYPE_PCONSTANT:
        LoadConst(FuncInfo, TJclTD32ConstantSymbolInfo(SymbolInfo));
      SYMBOL_TYPE_SLINK32:
        LoadVar(UnitInfo, TJclTD32NamedSymbol(SymbolInfo), FuncInfo);
      SYMBOL_TYPE_REGISTER, SYMBOL_TYPE_BPREL32, SYMBOL_TYPE_LDATA32, SYMBOL_TYPE_GDATA32:
        begin
          VarInfo := LoadVar(UnitInfo, TJclTD32NamedSymbol(SymbolInfo), FuncInfo);
          FuncInfo.Params.Add(VarInfo);
        end;
    end;
  end;

  UnitInfo.Funcs.Add(FuncInfo);
  UnitInfo.FuncsByAddr.Add(FuncInfo);
end;

procedure TDelphiDebugInfo.ResetMemoryManagerBreakpoints;
begin
  // gvDebuger.MemoryBPCheckMode := False;

  if MemoryManagerInfo.VarInfo = nil then Exit;

  if MemoryManagerInfo.GetMem <> nil then
    gvDebuger.DbgCodeProfiler.RemoveTrackBreakpoint(MemoryManagerInfo.GetMem.Address, tbMemInfo);
  if MemoryManagerInfo.FreeMem <> nil then
    gvDebuger.DbgCodeProfiler.RemoveTrackBreakpoint(MemoryManagerInfo.FreeMem.Address, tbMemInfo);
  if MemoryManagerInfo.ReallocMem <> nil then
    gvDebuger.DbgCodeProfiler.RemoveTrackBreakpoint(MemoryManagerInfo.ReallocMem.Address, tbMemInfo);
  if MemoryManagerInfo.AllocMem <> nil then
    gvDebuger.DbgCodeProfiler.RemoveTrackBreakpoint(MemoryManagerInfo.AllocMem.Address, tbMemInfo);

  gvDebuger.Log('Reset slow memory manager hook - ok');
end;

procedure TDelphiDebugInfo.ResolveUnits;
var
  I, J, U: Integer;
  Member: TStructMember;
  UInfo: TUnitInfo;
  TInfo: TTypeInfo;
  FuncJ, FuncU: TFuncInfo;
begin
  if Units.Count = 0 then
    Exit;

  Units.Sorted := True;

  For I := 0 To Units.Count - 1 Do
  begin
    UInfo := TUnitInfo(Units.Objects[I]);

    // DoProgress(Format('Check unit "%s"', [UInfo.Name]), 90 + Round((I + 1) * Delta));

    For J := 0 To UInfo.UsedUnits.Count - 1 Do
    begin
      U := Units.IndexOf(UInfo.UsedUnits[J]);
      If U <> -1 Then
        UInfo.UsedUnits.Objects[J] := Units.Objects[U];
    end;

    For J := 0 To UInfo.Types.Count - 1 Do
    begin
      TInfo := TTypeInfo(UInfo.Types[J]);
      If TInfo.Members <> Nil Then
        For U := 0 To TInfo.Members.Count - 1 Do
        begin
          Member := TStructMember(TInfo.Members[U]);
          If (Member.MethodNameId <> 0) then
            Member.Method := UInfo.FindFuncByNameId(Member.MethodNameId);
        end;
    end;

    UInfo.FuncsByAddr.Capacity := UInfo.Funcs.Count;
    For J := 0 To UInfo.Funcs.Count - 1 Do
    begin
      FuncJ := TFuncInfo(UInfo.Funcs[J]);

      For U := 0 To UInfo.Funcs.Count - 1 Do
      begin
        FuncU := TFuncInfo(UInfo.Funcs[U]);
        If FuncJ.ID = FuncU.ParentID Then
          FuncU.Parent := FuncJ;
      end;
    end;
  end;
end;

function TDelphiDebugInfo.SetDebugHook(const Value: Byte): LongBool;
Const
  _DebugHook: AnsiString = 'DebugHook';

  // Value:
  // 1 to notify debugger of non-Delphi exceptions
  // >1 to notify debugger of exception unwinding
Var
  USystem: TUnitInfo;
  DebugHook: TVarInfo;
begin
  Result := False;

  USystem := GetSystemUnit;
  if Assigned(USystem) then
  begin
    DebugHook := USystem.FindVarByName(_DebugHook, True);
    If Assigned(DebugHook) Then
      gvDebuger.WriteData(Pointer(DebugHook.Offset), @Value, SizeOf(Byte));
  end;
end;

procedure TDelphiDebugInfo.SetDelphiVersion(const Value: TDelphiVersion);
begin
  FDelphiVersion := Value;
end;


procedure TDelphiDebugInfo.SetMemoryManagerBreakpoints;
var
  Members: TNameList;
  Member: TStructMember;
  Addr: Pointer;

  function _GetFuncPtr: Pointer;
  var
    Offset: Pointer;
  begin
    if Member = nil then
      RaiseDebugCoreException();

    Result := nil;
    Offset := Pointer(MemoryManagerInfo.VarInfo.Offset + Member.Offset);
    if not gvDebuger.ReadData(Offset, @Result, SizeOf(Pointer)) then
      RaiseDebugCoreException();
  end;

  function _SetTrackBreakpoint(Addr: Pointer): TFuncInfo;
  var
    UnitInfo: TUnitInfo;
    LineInfo: TLineInfo;
  begin
    Result := Nil;

    if GetLineInfo(Addr, UnitInfo, Result, LineInfo, False) <> slNotFound then
      gvDebuger.DbgCodeProfiler.SetTrackBreakpoint(Addr, Result, tbMemInfo)
    else
      RaiseDebugCoreException();
  end;

begin
  if MemoryManagerInfo.VarInfo = nil then Exit;

  if MemoryManagerInfo.VarInfo.DataType = nil then Exit;

  Members := MemoryManagerInfo.VarInfo.DataType.Members;
  if Assigned(Members) and (Members.Count > 0) then
  begin
    gvDebuger.MemoryBPCheckMode := True;

    Member := TStructMember(Members.FindByName('GetMem'));
    Addr := _GetFuncPtr;
    MemoryManagerInfo.GetMem := _SetTrackBreakpoint(Addr);

    Member := TStructMember(Members.FindByName('FreeMem'));
    Addr := _GetFuncPtr;
    MemoryManagerInfo.FreeMem := _SetTrackBreakpoint(Addr);

    Member := TStructMember(Members.FindByName('ReallocMem'));
    Addr := _GetFuncPtr;
    MemoryManagerInfo.ReallocMem := _SetTrackBreakpoint(Addr);

    Member := TStructMember(Members.FindByName('AllocMem'));
    Addr := _GetFuncPtr;
    MemoryManagerInfo.AllocMem := _SetTrackBreakpoint(Addr);

    gvDebuger.Log('Set slow memory manager hook - ok');
  end;
end;

function TDelphiDebugInfo.VarValueAsString(const Value: Variant): String;
begin
  if VarType(Value) = varUnknown then
    Result := CustomVariantAsString(Value)
  else
    Result := VarToStrDef(Value, '');
end;

Function TDelphiDebugInfo.FindUnitByAddr(const Addr: Pointer): TUnitInfo;
var
  I, J: Integer;
  Segment: TUnitSegmentInfo;
begin
  // TODO:
  //Result := TUnitInfo(UnitsByAddr.FindByAddress(Addr));

  for I := 0 to Units.Count - 1 do
  begin
    Result := TUnitInfo(Units.Objects[I]);
    for J := 0 to Result.Segments.Count - 1 do
    begin
      Segment := Result.Segments[J];

      if Assigned(Segment.SegmentClassInfo) and (Segment.SegmentClassInfo.SegType = ustCode) and
        (Cardinal(Addr) >= Cardinal(Segment.Address)) and (Cardinal(Addr) <= (Cardinal(Segment.Address) + Segment.Size))
      then
        Exit;
    end;
  end;

  Result := Nil;
end;

procedure TDelphiDebugInfo.FillSystemUnits;
begin
  FSystemUnits.Clear;
  FSystemUnits.Sorted := False;
  FSystemUnits.CaseSensitive := False;

  FSystemUnits.Add('System');
  FSystemUnits.Add('Classes');
  FSystemUnits.Add('Windows');
  FSystemUnits.Add('SysUtils');
  FSystemUnits.Add('Variants');
  FSystemUnits.Add('StrUtils');
  FSystemUnits.Add('WideStrUtils');
  FSystemUnits.Add('XMLDoc');
  FSystemUnits.Add('XMLIntf');
  FSystemUnits.Add('Graphics');
  FSystemUnits.Add('Forms');
  FSystemUnits.Add('Controls');
  FSystemUnits.Add('StdCtrls');
  FSystemUnits.Add('ExtCtrls');
  FSystemUnits.Add('ComCtrls');
  FSystemUnits.Add('Buttons');
  FSystemUnits.Add('ActnList');
  FSystemUnits.Add('Mask');
  FSystemUnits.Add('Dialogs');

  FSystemUnits.Add('WinApi');
  FSystemUnits.Add('Vcl');
  FSystemUnits.Add('Soap');
  FSystemUnits.Add('Xml');
  FSystemUnits.Add('Web');
  FSystemUnits.Add('Data');

  (*
  FSystemUnits.Add('acPNG');
  FSystemUnits.Add('sCommonData');
  FSystemUnits.Add('acZLibEx');
  FSystemUnits.Add('sVclUtils');
  FSystemUnits.Add('sLabel');
  FSystemUnits.Add('sSkinProvider');
  FSystemUnits.Add('sSkinManager');
  FSystemUnits.Add('sGraphUtils');
  FSystemUnits.Add('acntUtils');
  *)

  FSystemUnits.Sorted := True;
end;

Function TDelphiDebugInfo.FindFuncByAddr(const UnitInfo: TUnitInfo; const Addr: Pointer): TFuncInfo;
Var
  I: Integer;
begin
  // TODO:
  //Result := TFuncInfo(UnitInfo.FuncsByAddr.FindByAddress(Addr));

  For I := 0 To UnitInfo.Funcs.Count - 1 Do
  begin
    Result := TFuncInfo(UnitInfo.Funcs[I]);

    if (Cardinal(Result.Address) <= Cardinal(Addr)) and (Cardinal(Addr) < Cardinal(Result.Address) + Result.Size) then
      Exit;
  end;
  Result := Nil;
end;

Function TDelphiDebugInfo.FindLineByAddr(const FuncInfo: TFuncInfo; const Addr: Pointer; const GetPrevLine: LongBool = False): TLineInfo;
Var
  LineIdx: Integer;
  //SearchLine: TLineInfo;
begin
  (*
  Result := Nil;

  SearchLine := TLineInfo.Create;
  try
    SearchLine.Address := Addr;
    FuncInfo.Lines.BinarySearch(SearchLine, LineIdx);
  finally
    FreeAndNil(SearchLine);
  end;

  if (LineIdx >= 0) and (LineIdx < FuncInfo.Lines.Count) then
  begin
    if (LineIdx > 0) and GetPrevLine then
      Dec(LineIdx);

    Result := FuncInfo.Lines[LineIdx];
  end;
  *)

  LineIdx := FuncInfo.Lines.Count - 1;

  While (LineIdx >= 0) Do
  begin
    Result := TLineInfo(FuncInfo.Lines[LineIdx]);
    If Cardinal(Addr) >= Cardinal(Result.Address) Then
    begin
      If GetPrevLine And (LineIdx > 0) Then
        Result := TLineInfo(FuncInfo.Lines[LineIdx - 1]);

      Exit;
    end;
    Dec(LineIdx);
  end;

  Result := Nil;
end;

Function TDelphiDebugInfo.DoReadDebugInfo(Const FileName: String; ALoadDebugInfo: LongBool): LongBool;
Var
  I: Integer;
  Module: TJclTD32ModuleInfo;
  Delta: Double;
begin
  Result := FileExists(FileName);
  If Result Then
  begin
    DoProgress('Prepare', 4);
    if Assigned(FImage) then
      FreeAndNil(FImage);

    DoProgress('Init image', 5);
    FImage := TJclPeBorTD32Image.Create(True);
    DoProgress('Load image', 5);

    FImage.FileName := GetDBGFileName(FileName);

    DoProgress('Load debug info', 10);
    Result := FImage.IsTD32DebugPresent;
    If Result And ALoadDebugInfo and (FImage.TD32Scanner.ModuleCount > 0) Then
    begin
      case FImage.TD32DebugDataType of
        ddtInImage:
          FDebugInfoType := 'Internal';
        ddtTDS:
          FDebugInfoType := 'External(TDS)';
      end;

      InitSegments;

      Delta := 70 / FImage.TD32Scanner.ModuleCount;
      For I := 0 To FImage.TD32Scanner.ModuleCount - 1 Do
      begin
        Module := FImage.TD32Scanner.Modules[I];
        ParseUnit(Module);

        DoProgress('Load debug info', 10 + Round((I + 1) * Delta));
      end;

      DoProgress('Check debug info', 80);
      ResolveUnits;

      // Выгружать нельзя, так как используется для DebugInfo
      //UnMapAndLoad(FImage.LoadedImage);
    end;
    DoProgress('Debug info loaded', 99);
  end;
end;

function TDelphiDebugInfo.CheckAddr(Const Addr: Pointer): LongBool;
begin
  Result := FindUnitByAddr(Addr) <> Nil;
end;

function TDelphiDebugInfo.CheckDebugException(ExceptionRecord: PExceptionRecord; Var IsTraceException: LongBool): LongBool;
begin
  Result := Inherited CheckDebugException(ExceptionRecord, IsTraceException);

  If Not Result And IsDelphiException(ExceptionRecord) Then
  begin
    IsTraceException := IsDelphiTraceException(ExceptionRecord);
    Result := ExceptionRecord^.ExceptionFlags = cContinuable;
  end;
end;

function TDelphiDebugInfo.CheckSystemFile(Const FileName: String): LongBool;
var
  FN: String;
  SL: TStringArray;
begin
  Result := False;
  FN := ExtractFileName(FileName);
  SplitStr(FN, '.', SL);
  if Length(SL) > 0 then
    Result := (FSystemUnits.IndexOf(SL[0]) >= 0);
end;

procedure TDelphiDebugInfo.ClearDebugInfo;
var
  DbgFileName: String;
begin
  FIsHookSet := False;

  if Assigned(FImage) then
  begin
    DbgFileName := '';

    if DebugInfoLoaded then
      DbgFileName := FImage.FileName;

    FreeAndNil(FImage);

    if DebugInfoLoaded and (DbgFileName <> '') then
      DeleteFile(PWideChar(DbgFileName));
  end;

  FDelphiVersion := dvAuto;

  if Assigned(FSystemUnits) then
    FSystemUnits.Clear;

  if Assigned(FAddressInfoList) then
    FAddressInfoList.Clear;

  Inherited ClearDebugInfo;
end;

Function TDelphiDebugInfo.HasDebugInfo(Const FileName: String): LongBool;
begin
  Result := HasDelphiDebugInfo(FileName);
end;

Function TDelphiDebugInfo.GetAddrInfo(Var Addr: Pointer; Const FileName: String; Line: Cardinal): TFindResult;
Var
  Index: Integer;
  UnitInfo: TUnitInfo;
  ExactMatch: LongBool;
begin
  Result := slNotFound;

  Index := Units.IndexOf(LowerCase(FileName));
  If Index <> -1 Then
  begin
    UnitInfo := TUnitInfo(Units.Objects[Index]);

    // TODO: переделать на неточный поиск (ближний по адресу)
    Index := UnitInfo.Lines.IndexOf(Pointer(Line));
    ExactMatch := (Index >= 0);
    If (Index >= 0) And (Index < UnitInfo.Lines.Count) Then
    begin
      Addr := TLineInfo(UnitInfo.Lines[Index]).Address;
      If ExactMatch Then
        Result := slFoundExact
      Else
        Result := slFoundNotExact;
    end;
  end;
end;

function TDelphiDebugInfo.GetClassName(Const ObjectPtr: Pointer): String;
Const
  _ValidChars = ['_', 'a' .. 'z', 'A' .. 'Z', '0' .. '9'];
Var
  ObjTypePtr: Pointer;
  classNamePtr: Pointer;
  ClassName: ShortString;
  I: Integer;
begin
  Result := '';
  ObjTypePtr := Nil;
  if gvDebuger.ReadData(ObjectPtr, @ObjTypePtr, SizeOf(Pointer)) then
  begin
    classNamePtr := Nil;
    if gvDebuger.ReadData(IncPointer(ObjTypePtr, RTLInfo.vmtClassName), @ClassNamePtr, SizeOf(Pointer)) then
    begin
      ClassName := gvDebuger.ReadStringP(IncPointer(ClassNamePtr, SizeOf(Byte)));
      for I := 1 to Length(ClassName) do
        if not(ClassName[I] in _ValidChars) then
          Exit;

      Result := String(ClassName);
    end;
  end;
end;

function TDelphiDebugInfo.GetDBGFileName(const FileName: String): String;
var
  I: Integer;
begin
  // Создаем копию исполняемого файла для загрузки дебажной информации
  // TODO: Мониторилка за изменением исполняемого файла и обновление дебажной инфы
  I := 0;
  repeat
    Result := ChangeFileExt(FileName, '.~dbg');
    if FileExists(Result) then
    begin
      if DeleteFile(PWideChar(Result)) then
        Break
      else
        Result := ChangeFileExt(FileName, '.~dbg' + IntToStr(I));

      Inc(I);

      if I >= 10 then
      begin
        Result := FileName;
        Break;
      end;
    end
    else
      Break;
  until False;

  if Result <> FileName then
    if not CopyFile(PWideChar(FileName), PWideChar(Result), True) then
      Result := FileName;
end;

Function TDelphiDebugInfo.GetExceptionAddress(ExceptionRecord: PExceptionRecord): Pointer;
begin
  If IsDelphiException(ExceptionRecord) And (ExceptionRecord^.NumberParameters > 0) Then
    Result := Pointer(ExceptionRecord^.ExceptionInformation[0])
  Else
    Result := Inherited GetExceptionAddress(ExceptionRecord);
end;

Function TDelphiDebugInfo.GetExceptionFrame(ExceptionRecord: PExceptionRecord): Pointer;
begin
  If ExceptionRecord^.ExceptionCode = cDelphiException Then
    Result := Pointer(ExceptionRecord^.ExceptionInformation[5])
  Else
    Result := Inherited GetExceptionFrame(ExceptionRecord);
end;

Function TDelphiDebugInfo.GetExceptionMessage(ExceptionRecord: PExceptionRecord; const ThreadId: TThreadId): String;
Var
  ExceptTypeAddr: Pointer;
  ExceptMsgPtr: Pointer;
begin
  Result := '';
  If ExceptionRecord^.ExceptionCode = cDelphiException Then
  begin
    ExceptTypeAddr := Pointer(ExceptionRecord^.ExceptionInformation[1]);

    ExceptMsgPtr := nil;
    if gvDebuger.ReadData(@Exception(ExceptTypeAddr).Message, @ExceptMsgPtr, SizeOf(Pointer)) then
      Result := gvDebuger.ReadStringW(ExceptMsgPtr);
  End
  Else
    Result := Inherited GetExceptionMessage(ExceptionRecord, ThreadId);
end;

Function TDelphiDebugInfo.GetExceptionName(ExceptionRecord: PExceptionRecord): String;
Var
  ExceptTypeAddr: Pointer;
begin
  If ExceptionRecord^.ExceptionCode = cDelphiException Then
  begin
    ExceptTypeAddr := Pointer(ExceptionRecord^.ExceptionInformation[1]);
    Result := GetClassName(ExceptTypeAddr);
  End
  Else
    Result := Inherited GetExceptionName(ExceptionRecord);
end;

Function TDelphiDebugInfo.GetLineInfo(const Addr: Pointer; Var UnitInfo: TUnitInfo; Var FuncInfo: TFuncInfo; Var LineInfo: TLineInfo;
  GetPrevLine: LongBool): TFindResult;
var
  AddressInfo: PAddressInfo;
begin
  Result := slNotFound;

  UnitInfo := Nil;
  FuncInfo := Nil;
  LineInfo := Nil;

  FAddressInfoList.Lock.beginRead;
  try
    if FAddressInfoList.TryGetValue(Addr, AddressInfo) then
    begin
      UnitInfo := AddressInfo.UnitInfo;
      FuncInfo := AddressInfo.FuncInfo;
      LineInfo := AddressInfo.LineInfo;
      Result := AddressInfo.FindResult;
    end
    else
    begin
      UnitInfo := FindUnitByAddr(Addr);
      If UnitInfo <> Nil Then
      begin
        FuncInfo := FindFuncByAddr(UnitInfo, Addr);
        If FuncInfo <> Nil Then
        begin
          LineInfo := FindLineByAddr(FuncInfo, Addr, GetPrevLine);
          If LineInfo = Nil Then
            Result := slFoundWithoutLine
          Else
          begin
            If LineInfo.Address = Addr Then
              Result := slFoundExact
            Else
              Result := slFoundNotExact;
          end;
        end;
      end;

      AddressInfo := AllocMem(SizeOf(RAddressInfo));
      AddressInfo.Addr := Addr;
      AddressInfo.UnitInfo := UnitInfo;
      AddressInfo.FuncInfo := FuncInfo;
      AddressInfo.LineInfo := LineInfo;
      AddressInfo.FindResult := Result;

      FAddressInfoList.Lock.beginWrite;
      try
        FAddressInfoList.AddOrSetValue(Addr, AddressInfo);
      finally
        FAddressInfoList.Lock.EndWrite;
      end;
    end;
  finally
    FAddressInfoList.Lock.EndRead;
  end;
end;

function TDelphiDebugInfo.GetMemoryManager: TVarInfo;
const
  _TMemoryManager: AnsiString = 'TMemoryManager';
Var
  USystem: TUnitInfo;
  MMType: TTypeInfo;
  J: Integer;
begin
  Result := Nil;

  USystem := GetSystemUnit;
  if Assigned(USystem) then
  begin
    MMType := USystem.FindTypeByName(_TMemoryManager, True);
    if Assigned(MMType) then
    begin
      for J := 0 to USystem.Vars.Count - 1 do
      begin
        Result := TVarInfo(USystem.Vars[J]);
        if Result.DataType = MMType then
          Exit;
      end;
      Result := nil;
    end;
  end;
end;

Function TDelphiDebugInfo.MakeFuncDbgFullName(Const ClassName, MethodName: AnsiString): AnsiString;
begin
  Result := '@' + ClassName + '@' + MethodName;
end;

Function TDelphiDebugInfo.MakeFuncShortName(Const MethodName: AnsiString): AnsiString;
Var
  I: Integer;
begin
  Result := MethodName;
  I := Pos(AnsiString('@'), Result);
  If I = 1 Then
  begin
    Delete(Result, 1, 1);
    I := Pos(AnsiString('@'), Result);
    If I > -1 Then
      Delete(Result, 1, I)
    Else
      Insert('@', Result, 1);
  end;
end;

Function TDelphiDebugInfo.MakeFuncNativeName(Const MethodName: AnsiString): AnsiString;
begin
  Result := MethodName;
  If Result <> '' Then
  begin
    If Result[1] = '@' Then
      Delete(Result, 1, 1);
    Result := AnsiString(StringReplace(String(Result), '@', '.', [rfReplaceAll]));
  end;
end;

Function TDelphiDebugInfo.Evaluate(BriefMode: LongBool; Const Expression: String; Const TimeOut: Cardinal = INFINITE): String;
// Var
// Parser   : TExprParser;
// UnitInfo : TUnitInfo;
// FuncInfo : TFuncInfo;
// LineInfo : TLineInfo;
begin
  // Result := '';
  // If Expression <> '' Then
  // Try
  // GetLineInfo(Debuger.GetRegisters.EIP, UnitInfo, FuncInfo, LineInfo, False);
  // Parser := TExprParser.Create(DebuggeeControl, Self, UnitInfo, FuncInfo, BriefMode, Expression);
  // Try
  // Result := Parser.CalculateAsString;
  // Finally
  // Parser.Free;
  // end;
  // Except
  // On E : EDebugException Do
  // Raise;
  // On E : Exception Do
  // Raise EEvaluateException.Create(E.Message);
  // end;
end;

function TDelphiDebugInfo.EvaluateVariable(VarInfo: TVarInfo): Variant;
//var
//  EBP: Pointer;
//  Value: Variant;
begin
  //EBP := Pointer(gvDebuger.GetRegisters(gvDebuger.CurThreadId).Ebp);
  //TODO: Value := EvaluateProcs.EvaluateVariable(gvDebuger, VarInfo, EBP, True);

  //TODO: Result := EvaluateProcs.CalculateValue(Value, CalculateData);
  Result := Unassigned;
end;

function TDelphiDebugInfo.ImageBase: Cardinal;
begin
  Result := FImage.OptionalHeader32.ImageBase;
end;

function TDelphiDebugInfo.ImageNames(const Index: TNameId): AnsiString;
begin
  if (Index >= 0) and (FImage <> Nil) and (FImage.TD32Scanner <> Nil) and (Index < FImage.TD32Scanner.NameCount) then
    Result := FImage.TD32Scanner.Names[Index]
  else
    Result := '';
end;

procedure TDelphiDebugInfo.InitCodeTracking(const SetBP: LongBool);
var
  FuncCount: Integer;
  I, J: Integer;
  UnitInfo: TUnitInfo;
  FuncInfo: TFuncInfo;
  Segment: TUnitSegmentInfo;
  OldProtect: Cardinal;
begin
  FuncCount := 128;

  if SetBP then
    for I := 0 to Units.Count - 1 do
      Inc(FuncCount, TUnitInfo(Units.Objects[I]).Funcs.Count);

  gvDebuger.DbgCodeProfiler.ClearDbgTracking;
  gvDebuger.DbgCodeProfiler.InitDbgTracking(FuncCount);

  if SetBP then
  begin
    for I := 0 to Units.Count - 1 do
    begin
      UnitInfo := TUnitInfo(Units.Objects[I]);

      for J := 0 to UnitInfo.Segments.Count - 1 do
      begin
        Segment := UnitInfo.Segments[J];
        if Assigned(Segment.SegmentClassInfo) and (Segment.SegmentClassInfo.SegType = ustCode) then
            Assert(
              VirtualProtectEx(
                gvDebuger.ProcessData.AttachedProcessHandle, Pointer(Segment.Address), Segment.Size, PAGE_EXECUTE_READWRITE, OldProtect
              )
            );
      end;

      if not gvDebuger.TrackSystemUnits and (UnitInfo.UnitType = utSystem) then
        Continue;

      for J := 0 to UnitInfo.Funcs.Count - 1 do
      begin
        FuncInfo := TFuncInfo(UnitInfo.Funcs[J]);
        gvDebuger.DbgCodeProfiler.SetTrackBreakpoint(FuncInfo.Address, FuncInfo);
      end;
    end;
  end;
end;

procedure TDelphiDebugInfo.InitDebugHook;
begin
  if not FIsHookSet then
  begin
    FIsHookSet := True;

    gvDebuger.ProcessData.SetPEImage(FImage);

    InitCodeTracking(gvDebuger.CodeTracking and not gvDebuger.SamplingMethod);

    MemoryManagerInfo.VarInfo := GetMemoryManager;
    RTLInfo.vmtClassNameInfo := GetVMTClassName;

    // Установка перехвата вызовов GetMem и FreeMem
    // SetMemoryManagerBreakpoints;

    // Инициализация дебажного потока в процессе
    // !!! Поток запустится не сразу, а через некоторое время
    LoadDbgHookDll(
      gvDebuger.ProcessData.AttachedProcessHandle,
      Format('%s\DbgHook32.dll', [ExtractFileDir(Application.ExeName)]),
      Pointer(FImage.OptionalHeader32.ImageBase),
      MemoryManagerInfo.VarInfo,
      RTLInfo.vmtClassName,
      gvDebuger.DbgMemoryProfiler.MemoryCallStack,
      gvDebuger.DbgSysncObjsProfiler.SyncObjsTracking
    );
  end;
end;

procedure TDelphiDebugInfo.InitSegments;
var
  Idx: Integer;
  Segment: TSegmentClassInfo;
  ImageSectionHeader: TImageSectionHeader;
begin
  Segments.Clear;

  for Idx := 0 to FImage.ImageSectionCount - 1 do
  begin
    Segment := TSegmentClassInfo.Create;

    Segment.SegType := TSegmentClassInfo.StrToSegmentType(FImage.ImageSectionNames[Idx]);

    ImageSectionHeader := FImage.ImageSectionHeaders[Idx];
    Segment.Address := Pointer(ImageSectionHeader.VirtualAddress + ImageBase);
    Segment.Size := ImageSectionHeader.SizeOfRawData;
    Segment.ID := Idx + 1;

    Segments.AddObject(FImage.ImageSectionNames[Idx], Segment);
  end;

  (*
  Segment := TSegmentClassInfo.Create;
  Segment.ID := $0000;
  Segment.SegType := ustData;

  Segments.AddObject('DATA', Segment);

  Segment := TSegmentClassInfo.Create;
  Segment.ID := $0001;
  Segment.SegType := ustCode;

  Segments.AddObject('CODE', Segment);
  *)
end;

function TDelphiDebugInfo.IsDelphiException(ExceptionRecord: PExceptionRecord): LongBool;
begin
  case ExceptionRecord^.ExceptionCode Of
    cDelphiUnhandled, cDelphiTerminate, cDelphiException, cDelphiReRaise, cDelphiExcept, cDelphiFinally, cNonDelphiException, cDelphiExitFinally:
      Result := True;
  else
    Result := False;
  end;
end;

function TDelphiDebugInfo.IsDelphiTraceException(ExceptionRecord: PExceptionRecord): LongBool;
begin
  case ExceptionRecord^.ExceptionCode Of
    // cDelphiUnhandled,
    // cDelphiTerminate,
    cDelphiException, cDelphiReRaise, cDelphiExcept, cDelphiFinally,
    // cNonDelphiException,
    cDelphiExitFinally:
      Result := True;
  else
    Result := False;
  end;
end;

initialization

// _HookThreads;

finalization

// _UnhookThreads;

end.
