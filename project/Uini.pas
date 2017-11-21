unit Uini; // SpyroTAS is licensed under WTFPL

// interface to ini file usage

interface

type // define a key-value pair
  TStringPair = record
    Key, Value: string;
  end;

type // managed array of key-values
  TPairsArray = array of TStringPair;

// overloads to create such pairs (key is always a string):

function MakePair(const Key: string; const Value: string): TStringPair; overload;

function MakePair(const Key: string; const Value: Integer): TStringPair; overload;

function MakePair(const Key: string; const Value: Boolean): TStringPair; overload;

// overloads to get the value as any type from a key-value pair:

procedure GetPair(const Pair: TStringPair; out Value: string); overload;

procedure GetPair(const Pair: TStringPair; out Value: Integer); overload;

procedure GetPair(const Pair: TStringPair; out Value: Boolean); overload;

// two functions for actual work:

procedure IniSectionUpdate(const Filename: string; const Section: string; Data:
  TPairsArray; DoSave: Boolean);

function IniValueUpdate(const Filename: string; const Section: string; const Key:
  string; const NewValue: string = #0): string;

// encapsulation of above;
// uses SpyroTAS default ini file.
// should be called like this:
// var w,x,y,z:stirng;s:string;i:integer;b:boolean;t:TTasIni;
// begin t:=TTasIni.Create();
// t.Put(x,s);t.Put(y,i);t.Put(s,b);
// t.ReadFrom(w); // or // t.WriteTo(w);t.Free();end;
// t.Get(s);t.Get(i);t.Get(b);
// t.Free();end;

type
  TTasIni = class(TObject)
  public
    procedure Put(const Key: string; const Value: string); overload;
    procedure Put(const Key: string; const Value: Integer); overload;
    procedure Put(const Key: string; const Value: Boolean); overload;
    procedure Clear();
    procedure WriteTo(const Section: string);
    procedure ReadFrom(const Section: string);
    procedure Get(out Value: string); overload;
    procedure Get(out Value: Integer); overload;
    procedure Get(out Value: Boolean); overload;
  private
    procedure Grow(Read: Boolean);
    procedure Update(const Section: string; Write: Boolean);
  private
    Data: TPairsArray; // key-values
    Size: Integer; // memory size
    Index: Integer; // current position
    Ready: Boolean; // when values are read
  end;

implementation

uses
  SysUtils, IniFiles, Utas;

// simple string pair:
function MakePair(const Key: string; const Value: string): TStringPair; overload;
begin
  Result.Key := Key;
  Result.Value := Value;
end;

// integer value:
function MakePair(const Key: string; const Value: Integer): TStringPair; overload;
begin
  Result.Key := Key;
  Result.Value := IntToStr(Value);
end;

// boolean value stored as 0 or 1:
function MakePair(const Key: string; const Value: Boolean): TStringPair; overload;
begin
  Result.Key := Key;
  if Value then
    Result.Value := '1'
  else
    Result.Value := '0';
end;

// return a string, trim spaces:
procedure GetPair(const Pair: TStringPair; out Value: string); overload;
begin
  Value := Trim(Pair.Value);
end;

// return value as integer, 0 if wrong:
procedure GetPair(const Pair: TStringPair; out Value: Integer); overload;
begin
  Value := StrToIntDef(Trim(Pair.Value), 0);
end;

// return as boolean, true if valid and non-zero:
procedure GetPair(const Pair: TStringPair; out Value: Boolean); overload;
begin
  Value := (StrToIntDef(Trim(Pair.Value), 0) <> 0);
end;

// takes constant filename, section name,
// and an array or key-value pairs;
// can be used for reading or writing:
procedure IniSectionUpdate(const Filename: string; const Section: string; Data:
  TPairsArray; DoSave: Boolean);
var
  Ini: TIniFile; // standard way
  Index: Integer;
begin
  Ini := TIniFile.Create(Filename); // simple
  try
    for Index := 0 to Length(Data) - 1 do // all pairs
      if DoSave then // when saving put this
        Ini.WriteString(Section, Data[Index].Key, Data[Index].Value)
      else // when reading use previous as default
        Data[Index].Value := Ini.ReadString(Section, Data[Index].Key, Data[Index].Value);
  finally
    Ini.Free(); // close when all done
  end;
end;

// set or get one entry from ini file:
function IniValueUpdate(const Filename: string; const Section: string; const Key:
  string; const NewValue: string = #0): string;
var
  Data: TPairsArray; // will call section update routine
begin
  SetLength(Data, 1); // prepare one pair
  Data[0].Key := Key;
  Data[0].Value := NewValue;
  IniSectionUpdate(Filename, Section, Data, NewValue <> #0);
  Result := Data[0].Value; // return always
end;

// to reuse:
procedure TTasIni.Clear();
begin
  Ready := False;
  Index := 0;
  Size := 0;
  SetLength(Data, Size); // empty
end;

// internal, to enlarge memory or to check overflow:
procedure TTasIni.Grow(Read: Boolean);
begin
  if Read <> Ready then // don't allow mixing modes
    raise Exception.Create('TTasIni wrong usage');
  if Index = Size then // beyond the last
  begin
    if Read then // should never occur when getting values
      raise Exception.Create('TTasIni wrong index (' + IntToStr(Index) + '/' +
        IntToStr(Size) + ')');
    Size := 4 + Size * 2; // new memory size
    SetLength(Data, Size); // realloc
  end;
  Inc(Index); // will be one more than current
end;

// main task, operate on ini file:
procedure TTasIni.Update(const Section: string; Write: Boolean);
begin
  Grow(False); // just to check current mode
  Size := Index - 1; // actual size to call
  SetLength(Data, Size); // set it
  IniSectionUpdate(PathToIni, Section, Data, Write); // call!
  Index := 0; // prepare for reading
  Ready := True; // switch mode
end;

// push string:
procedure TTasIni.Put(const Key: string; const Value: string);
begin
  Grow(False);
  Data[Index - 1] := MakePair(Key, Value);
end;

// push int:
procedure TTasIni.Put(const Key: string; const Value: Integer);
begin
  Grow(False);
  Data[Index - 1] := MakePair(Key, Value);
end;

// push bool:
procedure TTasIni.Put(const Key: string; const Value: Boolean);
begin
  Grow(False);
  Data[Index - 1] := MakePair(Key, Value);
end;

// saving:
procedure TTasIni.WriteTo(const Section: string);
begin
  Update(Section, True);
end;

// loading:
procedure TTasIni.ReadFrom(const Section: string);
begin
  Update(Section, False);
end;

// pop string:
procedure TTasIni.Get(out Value: string);
begin
  Grow(True);
  GetPair(Data[Index - 1], Value);
end;

// pop int
procedure TTasIni.Get(out Value: Integer);
begin
  Grow(True);
  GetPair(Data[Index - 1], Value);
end;

// pop bool
procedure TTasIni.Get(out Value: Boolean);
begin
  Grow(True);
  GetPair(Data[Index - 1], Value);
end;

end.

// EOF




