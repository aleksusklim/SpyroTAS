library padSpyroTAShelper; // SpyroTAS is licensed under WTFPL

uses
  IniFiles,
  SysUtils,
  Windows;

const
  Version = ' [v1.1]';

var
  PADinit_, PADshutdown_, PADopen_, PADclose_, PADquery_, PADstartPoll_,
    PADpoll_, PADreadPort1_, PADreadPort2_, PADconfigure_, PADabout_, PADtest_,
    PADspyroTas_, PSEgetLibVersion_: Pointer;

type
  PADspyroTas = function(): Integer; stdcall;

type
  ArrayOfPChar = array of PAnsiChar;

function GetRawExports(Instance: Cardinal): ArrayOfPChar;
var
  DosHeader: PImageDosHeader;
  HeaderPtr: PAnsiChar;
  NtHeaders: PImageNtHeaders;
  OptionalHeader: PImageOptionalHeader;
  DataDirectory: PImageDataDirectory;
  ExportDirectory: ^IMAGE_EXPORT_DIRECTORY;
  AddressOfNames: PInteger;
  Index: Integer;
  FunctionName: PChar;
  OldProtect: Cardinal;
begin
  SetLength(Result, 0);
  DosHeader := {%H-}Pointer(Instance);
  HeaderPtr := PAnsiChar(DosHeader);
  if DosHeader.e_magic <> IMAGE_DOS_SIGNATURE then
    Exit;
  NtHeaders := Pointer(HeaderPtr + DosHeader._lfanew);
  if NtHeaders.Signature <> $4550 then
    Exit;
  OptionalHeader := @ntHeaders.OptionalHeader;
  DataDirectory := @OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
  ExportDirectory := Pointer(HeaderPtr + DataDirectory.VirtualAddress);
  AddressOfNames := PInteger(Pointer(HeaderPtr + Integer(ExportDirectory.AddressOfNames)));
  SetLength(Result, ExportDirectory.NumberOfFunctions);
  for Index := 0 to ExportDirectory.NumberOfFunctions - 1 do
  begin
    FunctionName := HeaderPtr + AddressOfNames^;
    Inc(AddressOfNames);
    VirtualProtect(FunctionName, StrLen(FunctionName), PAGE_READWRITE, OldProtect{%H-});
    Result[Index] := FunctionName;
  end;
end;

procedure RemoveExport(const Funcs: ArrayOfPChar; Name: PChar);
var
  Index: Integer;
begin
  for Index := 0 to Length(Funcs) - 1 do
    if (Name = nil) or (StrComp(Funcs[Index], Name) = 0) then
      Inc(Funcs[Index]^);
end;

function ReadDllFromIni(): string;
var
  Ini: TIniFile;
const
  Path = '.\SpyroTAS\SpyroTAS.ini';
  Section = 'spyrotas';
  Key = 'dll_path';
begin
  Result := '';
  Ini := nil;
  try
    Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + Path);
    Result := Ini.ReadString(Section, Key, '');
  except
  end;
  Ini.Free();
end;

var
  {$IFDEF FPC}
  LibraryName: PChar = 'SpyroTAS helper (Laz)' + Version;
  {$ELSE}
  LibraryName: PChar = 'SpyroTAS helper' + Version;
  {$ENDIF}
  LibraryVersion: Integer = 0;
  LibraryType: Integer = 8; // PAD type

procedure ConnectRouted();
var
  Name: string;
  Dll: THandle;
  Mode: Integer;
  Funcs: ArrayOfPChar;
begin
  Funcs := GetRawExports(HInstance);
  Name := ReadDllFromIni();
  try
    if Name = '' then
      Abort;
    Dll := LoadLibrary(PChar(Name));
    if Dll = 0 then
      Abort;
    if GetProcAddress(Dll, PChar('Spyro_TAS')) = nil then
    begin
      FreeLibrary(Dll);
      Abort;
    end;
    PADinit_ := GetProcAddress(Dll, PChar('PADinit'));
    PADshutdown_ := GetProcAddress(Dll, PChar('PADshutdown'));
    PADopen_ := GetProcAddress(Dll, PChar('PADopen'));
    PADclose_ := GetProcAddress(Dll, PChar('PADclose'));
    PADquery_ := GetProcAddress(Dll, PChar('PADquery'));
    PADstartPoll_ := GetProcAddress(Dll, PChar('PADstartPoll'));
    PADpoll_ := GetProcAddress(Dll, PChar('PADpoll'));
    PADreadPort1_ := GetProcAddress(Dll, PChar('PADreadPort1'));
    PADreadPort2_ := GetProcAddress(Dll, PChar('PADreadPort2'));
    PADconfigure_ := GetProcAddress(Dll, PChar('PADconfigure'));
    PADabout_ := GetProcAddress(Dll, PChar('PADabout'));
    PADtest_ := GetProcAddress(Dll, PChar('PADtest'));
    PADspyroTas_ := GetProcAddress(Dll, PChar('PADspyroTas'));
    PSEgetLibVersion_ := GetProcAddress(Dll, PChar('PSEgetLibVersion'));
    LibraryVersion := PADspyroTas(PSEgetLibVersion_);
  except
    RemoveExport(Funcs, nil);
    LibraryType := 0;
    Exit;
  end;
  Mode := PADspyroTas(PADspyroTas_);
  if (Mode and 1) = 0 then
  begin
    RemoveExport(Funcs, PChar('PADstartPoll'));
    RemoveExport(Funcs, PChar('PADpoll'));
  end;
  if (Mode and 2) = 0 then
  begin
    RemoveExport(Funcs, PChar('PADreadPort1'));
    RemoveExport(Funcs, PChar('PADreadPort2'));
  end;
end;

function PSEgetLibName(): PChar; stdcall;
begin
  Result := LibraryName;
end;

function PSEgetLibVersion(): Integer; stdcall;
begin
  Result := LibraryVersion;
end;

function PSEgetLibType(): Integer; stdcall;
begin
  Result := LibraryType;
end;

function PADinit: Integer;
asm
        JMP     PADinit_
end;

function PADshutdown: Integer;
asm
        JMP     PADshutdown_
end;

function PADopen: Integer;
asm
        JMP     PADopen_
end;

function PADclose: Integer;
asm
        JMP     PADclose_
end;

function PADquery: Integer;
asm
        JMP     PADquery_
end;

function PADstartPoll: Integer;
asm
        JMP     PADstartPoll_
end;

function PADpoll: Integer;
asm
        JMP     PADpoll_
end;

function PADreadPort1: Integer;
asm
        JMP     PADreadPort1_
end;

function PADreadPort2: Integer;
asm
        JMP     PADreadPort2_
end;

function PADconfigure: Integer;
asm
        JMP     PADconfigure_
end;

function PADabout: Integer;
asm
        JMP     PADabout_
end;

function PADtest: Integer;
asm
        JMP     PADtest_
end;

procedure Spyro_TAS();
begin
  ;
end;

exports
  PADinit,
  PADshutdown,
  PADopen,
  PADclose,
  PADquery,
  PADstartPoll,
  PADpoll,
  PADreadPort1,
  PADreadPort2,
  PADconfigure,
  PADabout,
  PADtest,
  PSEgetLibName,
  PSEgetLibVersion,
  PSEgetLibType,
  Spyro_TAS;

begin
  ConnectRouted();
end.

// EOF


