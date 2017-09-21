library SpyroTAS; // SpyroTAS is licensed under WTFPL

// define SpyroTAS_no_Libs to remove a dependence to GPL'ed and other non-my code;
//   currently it's affecting only AVI and PNG output.

uses
  {$IFDEF FPC}
  Interfaces, // need for Lazarus LCL
  {$ENDIF}
  Windows,   
  {(*}
  // units
  {%H-}Uavi in 'Uavi.pas',
  {%H-}Uforms in 'Uforms.pas',
  {%H-}Uglob in 'Uglob.pas',
  {%H-}Ugpu in 'Ugpu.pas',
  {%H-}Uini in 'Uini.pas',
  {%H-}Ukey in 'Ukey.pas',
  {%H-}Ulang in 'Ulang.pas',
  {%H-}Umain in 'Umain.pas',
  {%H-}Umisc in 'Umisc.pas',
  {%H-}Uroute in 'Uroute.pas',
  {%H-}Utas in 'Utas.pas',
  // forms
  {%H-}UFbind in 'UFbind.pas' {Fbind},
  {%H-}UFedit in 'UFedit.pas' {Fedit},
  {%H-}UFext in 'UFext.pas' {Fext},
  {%H-}UFgui in 'UFgui.pas' {Fgui},
  {%H-}UFhide in 'UFhide.pas' {Fhide},
  {%H-}UFover in 'UFover.pas' {Fover},
  {%H-}UFplug in 'UFplug.pas' {Fplug},
  {%H-}UFshot in 'UFshot.pas' {Fshot},
  {%H-}UFview in 'UFview.pas' {Fview};
  {*)}

// exported DLL functions:

exports
  rundll32, // allows (limited) standalone execution
  // routed:
  GPUclearDynarec,
  GPUdisplayText,
  GPUinit,
  GPUshutdown,
  GPUopen,
  GPUclose,
  GPUdmaChain,
  GPUupdateLace,
  GPUsetMode,
  GPUgetMode,
  GPUreadData,
  GPUreadDataMem,
  GPUreadStatus,
  GPUwriteData,
  GPUwriteDataMem,
  GPUwriteStatus,
  GPUmakeSnapshot,
  GPUfreeze,
  GPUgetScreenPic,
  GPUshowScreenPic,
  GPUconfigure,
  GPUtest,
  GPUabout,
  // interface
  PSEgetLibName,
  PSEgetLibType,
  PSEgetLibVersion,
  // TODO
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
  PADspyroTas,
  // to detect self module
  Spyro_TAS;


{$IFDEF FPC}
procedure Lazarus_Detach_Hook(dllparam: PtrInt);
begin
  Ignores(dllparam);
  // lazarus will call from here
  ThisDllInit(DLL_PROCESS_DETACH);
end;
{$R padSpyroTAShelper_Laz.rc}
{$ELSE}
{$R padSpyroTAShelper.res}
{$ENDIF}

{$R *.res}



begin
  {$IFDEF FPC}
  // define detach routine for lazarus
  Dll_Process_Detach_Hook := @Lazarus_Detach_Hook;
  {$ELSE}
  // this is the same for delphi
  DllProc := @ThisDllInit;
  {$ENDIF}
  // called at initialization
  ThisDllInit(DLL_PROCESS_ATTACH);
end.

// EOF


