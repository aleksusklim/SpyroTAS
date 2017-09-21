program LCL_DFM_clean;

// use this tool to make DFM's from Lazarus to be correct for Delphi
// Usage:
// LCL_DFM_clean.exe form1.dfm [form2.dfm ...]

// LCL_DFM_clean.bat:
//
// @cd /d %~dp0 & @for %%i in (*.dfm) do @LCL_DFM_clean.exe %%i

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  Index: Integer;
  Name, Temp, Line: string;
  Source, Target: Text;
  Touched: Boolean;

begin
  if ParamCount = 0 then
  begin
    Writeln('This tool is to make DFM`s from Lazarus to be correct for Delphi');
    Writeln('Usage:');
    Writeln('LCL_DFM_clean.exe form1.dfm [form2.dfm ...]');
    Writeln('');
    Writeln('Also this batch file shoud work: "LCL_DFM_clean.bat"');
    Writeln('');
    Writeln('@cd /d %~dp0 & @for %%i in (*.dfm) do @LCL_DFM_clean.exe %%i');
    Writeln('');
  end
  else
    for Index := 1 to ParamCount do
    begin
      Touched := False;
      Name := ParamStr(Index);
      Temp := Name + '.tmp';
      Assign(Target, Temp);
      Rewrite(Target);
      FileMode := 0;
      Assign(Source, Name);
      Reset(Source);
      while not Eof(Source) do
      begin
        Readln(Source, Line);
        if Copy(Trim(Line), 1, 3) <> 'LCL' then
          Writeln(Target, Line)
        else
          Touched := True;
      end;
      Close(Source);
      Close(Target);
      if Touched then
      begin
        DeleteFile(Name);
        RenameFile(Temp, Name);
      end
      else
        DeleteFile(Temp);
    end;
end.

