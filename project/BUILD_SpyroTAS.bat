
@REM  Set here full path to Lazarus lazbuild.exe, can be anywhere:
@set PATH_LAZBUILD="G:\LAZ\lazbuild.exe"

@REM  7zip (7z, 7za.exe) needed to combine source code, could be in system PATH or not:
@set PATH_7Z="7z.exe"

@REM  UPX is optional, could be in system PATH or not:
@set PATH_UPX="upx.exe"

@REM  Borland Delphi, must be in system PATH:
@set PATH_DCC="dcc32.exe"

@REM  Borland Resource Compiler (brc32 / brcc32), also in system PATH:
@set PATH_BRC="brc32.exe"

@title SpyroTAS build script:

@echo.
@echo  You can build SpyroTAS with Delphi (7) or Lazarus(1.6.2)/FPC (3.0.0)
@echo  You must have or Delphi, or Lazarus. Or both...
@echo  Also this script tries to use 7-zip and UPX.
@echo  You must manually set path to Lazarus in this batch file!
@echo  Delphi is required to be in system PATH (dcc32.exe)
@echo.
@echo PATH_LAZBUILD=%PATH_LAZBUILD%
@echo PATH_7Z=%PATH_7Z%
@echo PATH_UPX=%PATH_UPX%
@echo PATH_DCC=%PATH_DCC%
@echo.
@echo System PATH=%PATH%
@echo.
@echo  ========= ATTENTION! =========
@echo  If you ran this script first time, please close this window,
@echo  open .bat in Notepad and edit - to make sure that all paths to:
@echo.
@echo  lazbuild.exe (Lazarus IDE console compiler)
@echo  dcc32.exe    (Delphi 7 command-line compiler)
@echo  7z.exe       (7-zip archiver)
@echo  upx.exe      (UPX compressor)
@echo.
@echo - are correct. If they are, you may proceed, press ENTER:
@echo.

@pause
@cd /d %~dp0

@echo.
@echo ========= Deleting old binaries: =========
@echo.

del *.7z *.dll *.exe *.dcu *.~* pngzlib\*.dcu pngzlib\*.~* *.ddp *.lrs *.jar *.o *.a *.obj *.tds
if exist .\lib\ rmdir /s /q .\lib\
mkdir .\lib\

@echo.
@echo ========= 7-Zipping source code: =========
@echo.

%PATH_7Z% a SpyroTAS.7z *.lpr *.lpi *.dpr *.cfg *.dof *.lps *.pas *.dfm *.rc *.bat *.txt *.c *.ini pngzlib\*.pas pngzlib\*.inc included\*

@echo.
@echo ========= Compiling with Lazarus: =========
@echo.

%PATH_LAZBUILD% padSpyroTAShelper.lpi
if exist padSpyroTAShelper.dll rename padSpyroTAShelper.dll padSpyroTAShelper_Laz.dll
%PATH_UPX% -9 padSpyroTAShelper_Laz.dll
%PATH_LAZBUILD% SpyroTAS.lpi
if exist SpyroTAS.dll rename SpyroTAS.dll SpyroTAS_Laz.dll

@echo.
@echo ========= Compiling with Delphi: =========
@echo.

%PATH_DCC% padSpyroTAShelper.dpr
%PATH_BRC% -r padSpyroTAShelper.rc
%PATH_BRC% -r SpyroTASlangs.rc
%PATH_DCC% LCL_DFM_clean.dpr
@if exist LCL_DFM_clean.exe @for %%i in (*.dfm) do @LCL_DFM_clean.exe %%i
%PATH_BRC% -r SpyroTAS.rc
%PATH_DCC% -U.\pngzlib\ -O.\lib\ SpyroTAS.dpr

@echo.
@echo ========= Compressing with UPX: =========
@echo.

%PATH_UPX% -9 SpyroTAS.dll SpyroTAS_Laz.dll

@echo.
@echo ========= Combining with source: =========
@echo.

if exist SpyroTAS.7z if exist SpyroTAS.dll copy /b SpyroTAS.dll + /b SpyroTAS.7z gpuSpyroTAS.dll /b
if exist SpyroTAS.7z if exist SpyroTAS_Laz.dll copy /b SpyroTAS_Laz.dll + /b SpyroTAS.7z gpuSpyroTAS_Laz.dll /b
if exist SpyroTAS.dll del SpyroTAS.dll
if exist SpyroTAS_Laz.dll del SpyroTAS_Laz.dll

@echo.
@echo ========= Done. =========
@echo.
@echo. Continue if you want to clean project from object files, press ENTER:
@echo.

@pause

@echo.
@echo Deleting binary stuff (except executables):
@echo.

del *.dcu pngzlib\*.dcu *.ddp *.o *.a *.obj *.tds
if exist .\lib\ rmdir /s /q .\lib\
mkdir .\lib\

@echo.
@echo OK!
@echo.

@ping -n 2 127.0.0.1 1>nul 2>nul
@goto :eof

