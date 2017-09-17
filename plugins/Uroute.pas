unit Uroute; // SpyroTAS is licensed under WTFPL

// all DLL-related stuff

interface

uses
  Utas, Windows, SysUtils, Classes, Forms, Umain, UFplug, Ugpu;

var // routed function pointers:
  GPUclearDynarec_, GPUdisplayText_, GPUinit_, GPUshutdown_, GPUopen_, GPUclose_,
    GPUdmaChain_, GPUupdateLace_, GPUsetMode_, GPUgetMode_, GPUreadData_,
    GPUreadDataMem_, GPUreadStatus_, GPUwriteData_, GPUwriteDataMem_,
    GPUwriteStatus_, GPUmakeSnapshot_, GPUfreeze_, GPUgetScreenPic_,
    GPUshowScreenPic_, GPUconfigure_, GPUtest_, GPUabout_, PSEgetLibName_,
    PSEgetLibVersion_, Spyro_TAS_: Pointer;

var // TODO
  PADinit_, PADshutdown_, PADopen_, PADclose_, PADquery_, PADstartPoll_,
    PADpoll_, PADreadPort1_, PADreadPort2_, PADconfigure_, PADabout_, PADtest_: Pointer;

var
  PluginCorrectVersion: Integer; // contains plugin version in binary format for emulator
  RamStartRequest: Pointer = nil; // populated when emulator gives RAM address, then copied in PointerToRamStart
  ApplicationInitialized: Boolean = False; // to not initialize twice (Lazarus)

procedure DoDllLoad(GpuName: string; PadName: string); // also called from Uplugin

// typecastings for calling routeds:

type
  VOID = procedure; stdcall; // void f()

type
  PSTR = function: PChar; stdcall; // char* f()

type
  PINT = function: Integer; stdcall; // int f()

type
  PDMA = function(a: Pointer; b: Integer): Integer; stdcall; // int f(int,int)

type
  POPN = function(a: Integer): Integer; stdcall; // int f(int)

// exports whithout exact prototypes:

function GPUclearDynarec: Integer; register;

function GPUdisplayText: Integer; register;

function GPUclose: Integer; register;

function GPUgetMode: Integer; register;

function GPUreadData: Integer; register;

function GPUreadStatus: Integer; register;

function GPUmakeSnapshot: Integer; register;

function GPUgetScreenPic: Integer; register;

function GPUshowScreenPic: Integer; register;

// exports that will be parsed:

function GPUopen(Wind: HWND): Integer; stdcall;

function GPUfreeze(Param1: Integer; Param2: Integer): Integer; stdcall;

function GPUsetMode(Param1: Integer): Integer; stdcall;

function GPUreadDataMem(Param1: Integer; Param2: Integer): Integer; stdcall;

function GPUwriteData(Param1: Integer): Integer; stdcall;

function GPUwriteDataMem(Param1: Integer; Param2: Integer): Integer; stdcall;

function GPUwriteStatus(Param1: Integer): Integer; stdcall;

procedure GPUupdateLace(); stdcall;

procedure GPUdmaChain(RamStart: Pointer; Param: Integer); stdcall;

function GPUtest(): Integer; stdcall;

procedure GPUconfigure(); stdcall;

procedure GPUabout(); stdcall;

procedure GPUinit(); stdcall;

procedure GPUshutdown(); stdcall;

function PSEgetLibName(): PChar; stdcall;

function PSEgetLibVersion(): Integer; stdcall;

function PSEgetLibType(): Integer; stdcall;

// custom exports:

function rundll32(a, b, c, d: Integer): Integer; stdcall;

function Spyro_TAS(): Integer; stdcall;

// TODO

function PADinit: Integer; register;

function PADshutdown: Integer; register;

function PADopen: Integer; register;

function PADclose: Integer; register;

function PADquery: Integer; register;

//function PADstartPoll: Integer; register;

//function PADpoll: Integer; register;

function PADreadPort1(data: Integer): Integer; stdcall;

function PADreadPort2(data: Integer): Integer; stdcall;

procedure PADconfigure(); stdcall;

procedure PADabout(); stdcall;

function PADtest(): Integer; stdcall;

function PADstartPoll(pad: Integer): Integer; stdcall;

function PADpoll(value: Integer): Integer; stdcall;

function PADspyroTas(): Integer;

procedure CWPush();

procedure CWPop();

implementation

uses
  Uforms, Uglob, Umisc, UFgui;

var
  LoopedBack: Boolean = False; // detect recursion loops
  LoopedAsm: Integer = 0; // counter of calls until first Lace();
  RealGpuHandle: Integer = 0; // keep current loaded real plugin
  RealPadHandle: Integer = 0;
  Emul80807CW: Word;
  CWStack: Integer = 0;
  
// internal, just to raise message with caller name:

function TestForLoop(FunctionName: string): Boolean;
begin
  if LoopedBack then // managed by caller
    Report(SpyroTASName + ' ' + FunctionName + '()',
      'LOOP DETECTED!! Please, resolve plugin recursion...');
  Result := LoopedBack;
  InitPlugin();
end;

// called from routed ASM, preventing recursion
procedure AsmLoopInc();
const
  MaxAllowedCalls: Integer = 64; // how many other calls might be before first Lace()
begin
  if LoopedAsm = -1 then // if disabled
    Exit;
  Inc(LoopedAsm);
  if LoopedAsm > MaxAllowedCalls then // if obviously looped
  begin
    Report(SpyroTASName + ' LOOPED:',
      'Loop detected!? You must configure SpyroTAS from the emulator menu to use a real GPU plugin. Or the emulator didn''t call Lace(). Exiting...');
    ExitProcess(0); // considered fatal
  end;
  InitPlugin();
end;

// route jumps, we don't have to provide exact ptototypes:

function GPUclearDynarec: Integer;
asm
        CALL    AsmLoopInc
        JMP     GPUclearDynarec_
end;

function GPUdisplayText: Integer;
asm
        CALL    AsmLoopInc
        JMP     GPUdisplayText_
end;

function GPUclose: Integer;
asm
        CALL    AsmLoopInc
        JMP     GPUclose_
end;
{
function GPUsetMode: Integer; register;
asm
        CALL    AsmLoopInc
        JMP     GPUsetMode_
end;
}

function GPUgetMode: Integer;
asm
        CALL    AsmLoopInc
        JMP     GPUgetMode_
end;

function GPUreadData: Integer;
asm
        CALL    AsmLoopInc
        JMP     GPUreadData_
end;
{
function GPUreadDataMem: Integer; register;
asm
        CALL    AsmLoopInc
        JMP     GPUreadDataMem_
end;
}

function GPUreadStatus: Integer;
asm
        CALL    AsmLoopInc
        JMP     GPUreadStatus_
end;
{
function GPUwriteData: Integer; register;
asm
        CALL    AsmLoopInc
        JMP     GPUwriteData_
end;

function GPUwriteDataMem: Integer; register;
asm
        CALL    AsmLoopInc
        JMP     GPUwriteDataMem_
end;

function GPUwriteStatus: Integer; register;
asm
        CALL    AsmLoopInc
        JMP     GPUwriteStatus_
end;
}

function GPUfreeze(Param1: Integer; Param2: Integer): Integer;
begin
  DumpGpuCommand(GpuCommand_freeze, Param1, Param2);
  Result := PDMA(GPUfreeze_)(Pointer(Param1), Param2);
end;

function GPUsetMode(Param1: Integer): Integer;
begin
  DumpGpuCommand(GpuCommand_setMode, Param1);
  Result := POPN(GPUsetMode_)(Param1);
end;

function GPUreadDataMem(Param1: Integer; Param2: Integer): Integer;
begin
  Result := PDMA(GPUreadDataMem_)(Pointer(Param1), Param2);
end;

function GPUwriteData(Param1: Integer): Integer;
begin
  DumpGpuCommand(GpuCommand_writeData, Param1);
  Result := POPN(GPUwriteData_)(Param1);
end;

function GPUwriteDataMem(Param1: Integer; Param2: Integer): Integer;
begin
  DumpGpuCommand(GpuCommand_writeDataMem, Param1, Param2);
  Result := PDMA(GPUwriteDataMem_)(Pointer(Param1), Param2);
end;

function GPUwriteStatus(Param1: Integer): Integer;
begin
  DumpGpuCommand(GpuCommand_writeStatus, Param1);
  Result := POPN(GPUwriteStatus_)(Param1);
end;

function GPUmakeSnapshot: Integer;
asm
        CALL    AsmLoopInc
        JMP     GPUmakeSnapshot_
end;
{
function GPUfreeze: Integer; register;
asm
        CALL    AsmLoopInc
        JMP     GPUfreeze_
end;
}

function GPUgetScreenPic: Integer;
asm
        CALL    AsmLoopInc
        JMP     GPUgetScreenPic_
end;

function GPUshowScreenPic: Integer;
asm
        CALL    AsmLoopInc
        JMP     GPUshowScreenPic_
end;

// placeholders:

function zero(): Integer; stdcall;  // for 0 args
asm
        XOR     EAX, EAX
end;

function one(a: Integer): Integer; stdcall;  // for 1 arg
asm
        XOR     EAX, EAX
end;

function two(a, b: Integer): Integer; stdcall;  // for 2 args
asm
        XOR     EAX, EAX
end;

// try to load new real DLL:
procedure DoDllLoad(GpuName: string; PadName: string);
begin
  if RealGpuHandle <> 0 then // free previous
    FreeLibrary(RealGpuHandle);
  if (Length(GpuName) > 3) and not IsAbsolutePath(GpuName) then
    GpuName := EmulatorRoot + GpuName;

  if GpuName <> '' then
    RealGpuHandle := LoadLibrary(PChar(GpuName)) // standard way
  else
    RealGpuHandle := 0; // so all following Proc() will fail
  if RealGpuHandle = -1 then
    RealGpuHandle := 0; // for consistency
  Spyro_TAS_ := GetProcAddress(RealGpuHandle, PChar('Spyro_TAS')); // is it me?
  if (RealGpuHandle <> 0) and (Spyro_TAS_ <> nil) and (PINT(Spyro_TAS_) =
    SelfDllTime) then // exactly
  begin
    FreeLibrary(RealGpuHandle); // free and set wrong
    RealGpuHandle := 0;
  end;
  // get all routed:
  GPUclearDynarec_ := GetProcAddress(RealGpuHandle, PChar('GPUclearDynarec'));
  GPUdisplayText_ := GetProcAddress(RealGpuHandle, PChar('GPUdisplayText'));
  GPUinit_ := GetProcAddress(RealGpuHandle, PChar('GPUinit'));
  GPUshutdown_ := GetProcAddress(RealGpuHandle, PChar('GPUshutdown'));
  GPUopen_ := GetProcAddress(RealGpuHandle, PChar('GPUopen'));
  GPUclose_ := GetProcAddress(RealGpuHandle, PChar('GPUclose'));
  GPUdmaChain_ := GetProcAddress(RealGpuHandle, PChar('GPUdmaChain'));
  GPUupdateLace_ := GetProcAddress(RealGpuHandle, PChar('GPUupdateLace'));
  GPUsetMode_ := GetProcAddress(RealGpuHandle, PChar('GPUsetMode'));
  GPUgetMode_ := GetProcAddress(RealGpuHandle, PChar('GPUgetMode'));
  GPUreadData_ := GetProcAddress(RealGpuHandle, PChar('GPUreadData'));
  GPUreadDataMem_ := GetProcAddress(RealGpuHandle, PChar('GPUreadDataMem'));
  GPUreadStatus_ := GetProcAddress(RealGpuHandle, PChar('GPUreadStatus'));
  GPUwriteData_ := GetProcAddress(RealGpuHandle, PChar('GPUwriteData'));
  GPUwriteDataMem_ := GetProcAddress(RealGpuHandle, PChar('GPUwriteDataMem'));
  GPUwriteStatus_ := GetProcAddress(RealGpuHandle, PChar('GPUwriteStatus'));
  GPUmakeSnapshot_ := GetProcAddress(RealGpuHandle, PChar('GPUmakeSnapshot'));
  GPUfreeze_ := GetProcAddress(RealGpuHandle, PChar('GPUfreeze'));
  GPUgetScreenPic_ := GetProcAddress(RealGpuHandle, PChar('GPUgetScreenPic'));
  GPUshowScreenPic_ := GetProcAddress(RealGpuHandle, PChar('GPUshowScreenPic'));
  GPUconfigure_ := GetProcAddress(RealGpuHandle, PChar('GPUconfigure'));
  GPUtest_ := GetProcAddress(RealGpuHandle, PChar('GPUtest'));
  GPUabout_ := GetProcAddress(RealGpuHandle, PChar('GPUabout'));
  PSEgetLibName_ := GetProcAddress(RealGpuHandle, PChar('PSEgetLibName'));
//PSEgetLibType_:=GetProcAddress(RealGpuHandle,PChar('PSEgetLibType')); // not routed
  PSEgetLibVersion_ := GetProcAddress(RealGpuHandle, PChar('PSEgetLibVersion'));
// some may not exist, use placeholders: (is it really so simple?..)
  if GPUclearDynarec_ = nil then
    GPUclearDynarec_ := @one;
  if GPUdisplayText_ = nil then
    GPUdisplayText_ := @one;
  if GPUinit_ = nil then
    GPUinit_ := @zero;
  if GPUshutdown_ = nil then
    GPUshutdown_ := @zero;
  if GPUclose_ = nil then
    GPUclose_ := @zero;
//if GPUdmaChain_ = nil then GPUdmaChain_ := @two; // shows wrong configuration
//if GPUopen_ = nil then GPUopen_ := @one;
//if GPUupdateLace_=nil then GPUupdateLace_:=@zero;
  if GPUsetMode_ = nil then
    GPUsetMode_ := @one;
  if GPUgetMode_ = nil then
    GPUgetMode_ := @zero;
  if GPUreadData_ = nil then
    GPUreadData_ := @zero;
  if GPUreadDataMem_ = nil then
    GPUreadDataMem_ := @two;
  if GPUreadStatus_ = nil then
    GPUreadStatus_ := @zero;
  if GPUwriteData_ = nil then
    GPUwriteData_ := @one;
  if GPUwriteDataMem_ = nil then
    GPUwriteDataMem_ := @two;
  if GPUwriteStatus_ = nil then
    GPUwriteStatus_ := @one;
  if GPUmakeSnapshot_ = nil then
    GPUmakeSnapshot_ := @zero;
  if GPUfreeze_ = nil then
    GPUfreeze_ := @two;
  if GPUgetScreenPic_ = nil then
    GPUgetScreenPic_ := @one;
  if GPUshowScreenPic_ = nil then
    GPUshowScreenPic_ := @one;
  if GPUconfigure_ = nil then
    GPUconfigure_ := @zero;
//if GPUtest_=nil then GPUtest_:=@zero; // don't change from nil if missing
  if GPUabout_ = nil then
    GPUabout_ := @zero;
  if PSEgetLibName_ = nil then
    PSEgetLibName_ := @zero;
//if PSEgetLibType_=nil then PSEgetLibType_:=@zero; // custom
  if PSEgetLibVersion_ = nil then
    PSEgetLibVersion_ := @zero;

  if RealPadHandle <> 0 then // TODO
    FreeLibrary(RealPadHandle);
  if (Length(PadName) > 3) and not IsAbsolutePath(PadName) then
    PadName := EmulatorRoot + PadName;

  if PadName <> '' then
    RealPadHandle := LoadLibrary(PChar(PadName))
  else
    RealPadHandle := 0;
  if RealPadHandle = -1 then
    RealPadHandle := 0;

  PADinit_ := GetProcAddress(RealPadHandle, PChar('PADinit'));
  PADshutdown_ := GetProcAddress(RealPadHandle, PChar('PADshutdown'));
  PADopen_ := GetProcAddress(RealPadHandle, PChar('PADopen'));
  PADclose_ := GetProcAddress(RealPadHandle, PChar('PADclose'));
  PADquery_ := GetProcAddress(RealPadHandle, PChar('PADquery'));
  PADstartPoll_ := GetProcAddress(RealPadHandle, PChar('PADstartPoll'));
  PADpoll_ := GetProcAddress(RealPadHandle, PChar('PADpoll'));
  PADreadPort1_ := GetProcAddress(RealPadHandle, PChar('PADreadPort1'));
  PADreadPort2_ := GetProcAddress(RealPadHandle, PChar('PADreadPort2'));
  PADconfigure_ := GetProcAddress(RealPadHandle, PChar('PADconfigure'));
  PADabout_ := GetProcAddress(RealPadHandle, PChar('PADabout'));
  PADtest_ := GetProcAddress(RealPadHandle, PChar('PADtest'));

end;

// this is called by emulator not less than every frame:
procedure GPUupdateLace();
begin
  DumpGpuCommand(GpuCommand_updateLace, StoredKeys, HistoryPosition);
  AsmLoopInc(); // last time
  IThreadMain := GetCurrentThreadId();
  if InterlockedDecrement(IsOneMain) >= 0 then
  begin
    ResetEvent(EventMainDone);
    CWPush();
    try
      MAIN(); // main logic goes here!
    except
      on e: Exception do
        Report(SpyroTASName + ' MAIN() exception:', e.Message);
    end;
    CWPop();
    SetEvent(EventMainDone);
  end;
  InterlockedIncrement(IsOneMain);
  if Assigned(GPUupdateLace_) then // should be
  try
    if (not UseSkip) or (UseLimit and not NoFrameLimitHold) then // this for hard limiting
      VOID(GPUupdateLace_); // call routed lace
  except
    on e: Exception do
      Report(SpyroTASName + ' routed GPUupdateLace() exception:', e.Message);
  end;
  LoopedAsm := -1 // disable ASM loop checking
end;

// TODO
function GPUopen(Wind: HWND): Integer;
begin
  Result := 0;
  if Assigned(GPUopen_) then // protector
  begin
    AsmLoopInc();
    if IsWindow(Wind) then
      EmulatorWindow := Wind;
    Result := POPN(GPUopen_)(Wind); // route
  end;
end;

// called for general drawing; TODO
procedure GPUdmaChain(RamStart: Pointer; Param: Integer);
begin
  DumpGpuCommand(GpuCommand_dmaChain, Integer(RamStart), Param);
  RamStartRequest := RamStart;
  if (PointerToRamStart <> nil) and (RamStartRequest <> nil) then
    PointerToRamStart := RamStartRequest;
  if Assigned(GPUdmaChain_) then // protector
  begin
    AsmLoopInc();
    if (not UseSkip) or (UseLimit and not NoFrameLimitHold) then // same as above
      PDMA(GPUdmaChain_)(RamStart, Param); // route
  end;
  HaveNewScreen := True;
end;

// called by emulator at plugins scanning:
function PSEgetLibName(): PChar;
begin
  InitPlugin();
  Result := PChar(SpyroTASName); // tell our name to emulator
end;

//
function PSEgetLibVersion(): Integer;
begin
  InitPlugin();
  Result := PluginCorrectVersion; // already crafted
end;

// type or this plugin; see all possible constants somewhere else))
function PSEgetLibType(): Integer;
begin
  InitPlugin();
  Result := 2; // GPU type
end;

// called by "Test" button of emulator configuration; 0 = good, -1 = wrong:
function GPUtest(): Integer;
begin
  if TestForLoop('GPUtest') then
  begin
    Result := -1; // plugin recursion is bad
    Exit;
  end;
  LoopedBack := True; // assume a loop
  Result := -1; // assume all bad
  try
    // call if correct, use return that value
    if (RealGpuHandle <> 0) and (RealGpuHandle <> -1) and (GPUtest_ <> nil) then
      Result := PINT(GPUtest_);
  except
    on e: Exception do
      Report(SpyroTASName + ' GPUtest() exception:', e.Message);
  end;
  LoopedBack := False; // we're back, hope with zero Result
end;

// called by "Configure" button of emulator configuration:
procedure GPUconfigure();
begin
  if LoopedBack then
    Exit; // skip when looped
  LoopedBack := True;
  InitPlugin();
  CWPush();
  try
    if not ApplicationInitialized then // actually for Lazarus
      Application.Initialize;
    ApplicationInitialized := True;
    Fplug := TFplug.CreateUsual(); // instantiate the window
    try
      Fplug.ShowModal(); // block current thread by our config window
    except
      on e: Exception do
        Report(SpyroTASName + ' GPUconfigure() exception:', e.Message);
    end;
    FreeAndNil(Fplug); // release window
  except
  end;
  CWPop();
  LoopedBack := False;
end;

// called by "About" button of emulator configuration:
procedure GPUabout();
begin
  InitPlugin();
  // simple text popup
  Report(SpyroTASName, 'Spyro Tool-Assisted Speedrun plugin' +
    SpyroTASVestionString + ', by Kly_Men_COmpany!'#13#13 +
    'Licensed under WTFPL, no rights reserved.'#13#13 +
    'It may be possible to extract the source code (if present), try unpack SpyroTAS plugin DLL file with 7zip...');
end;

// could be called several times:
procedure GPUinit();
begin
  if TestForLoop('GPUinit') then
    Exit;
  LoopedBack := True;
  VOID(GPUinit_); // routed with loop-aware
  LoopedBack := False;
end;

// same as above:
procedure GPUshutdown();
begin
  if TestForLoop('GPUshutdown') then
    Exit;
  LoopedBack := True;
  VOID(GPUshutdown_); // routed with loop-aware
  LoopedBack := False;
end;

// returns timestamp of this DLL initial loading:
function Spyro_TAS(): Integer;
begin
  Result := SelfDllTime; // for internal self check
end;

// entry point for RunDLL32.exe:
function rundll32(a, b, c, d: Integer): Integer; // don't care about the real pototype (for now)
begin
  Ignores(a, b, c, d);
  InitPlugin(True);
  GPUconfigure(); // firstly show real plugin selection dialog
  while True do
  begin // simulate Lace() calls until forever...
    GPUupdateLace();
    Sleep(100);
  end;
end;

// TODO

function PADinit: Integer;
asm
        CALL    AsmLoopInc
        JMP     PADinit_
end;

function PADshutdown: Integer;
asm
        CALL    AsmLoopInc
        JMP     PADshutdown_
end;

function PADopen: Integer;
asm
        CALL    AsmLoopInc
        JMP     PADopen_
end;

function PADclose: Integer;
asm
        CALL    AsmLoopInc
        JMP     PADclose_
end;

function PADquery: Integer;
asm
        CALL    AsmLoopInc
        JMP     PADquery_
end;

procedure PADconfigure();
begin
  Report(SpyroTASName, 'You should configure this in video plugin settings!')
end;

procedure PADabout();
begin
  Report(SpyroTASName, 'See actual information in video plugin settings!');
end;

function PADtest(): Integer;
begin
  if (PADstartPoll_ <> nil) or (PADreadPort1_ <> nil) then
    Result := 0
  else
    Result := -1;
end;

function PADstartPoll(pad: Integer): Integer;
begin
  Result := POPN(PADstartPoll_)(pad);
  Result := PadPollHandler(pad, Result, True);
end;

function PADpoll(value: Integer): Integer;
begin
  Result := POPN(PADpoll_)(value);
  Result := PadPollHandler(value, Result, False);
end;

function PADreadPort1(data: Integer): Integer;
begin
  Result := POPN(PADreadPort1_)(data);
  PadReadHandler(PChar(data), False);
end;

function PADreadPort2(data: Integer): Integer;
begin
  Result := POPN(PADreadPort2_)(data);
  PadReadHandler(PChar(data), True);
end;

function PADspyroTas(): Integer;
begin
  InitPlugin();
  Result := 0;
  if PADquery_ = nil then
    Exit;
  if (PADstartPoll_ <> nil) and (PADpoll_ <> nil) then
    Result := Result or 1;
  if (PADreadPort1_ <> nil) and (PADreadPort2_ <> nil) then
    Result := Result or 2;
end;

procedure CWPush();
begin
  if CWStack = 0 then
  begin
    Emul80807CW := Get8087CW();
    Set8087CW(MY80807CW);
  end;
  Inc(CWStack);
  if CWStack = 16 then
    Report(SpyroTASName, 'CW stack Inc() bug?');
end;

procedure CWPop();
begin
  if CWStack = 0 then
    Report(SpyroTASName, 'CW stack Dec() bug?');
  if CWStack = 1 then
    Set8087CW(Emul80807CW);
  Dec(CWStack);
end;

end.

// EOF


