unit UFhide; // SpyroTAS is licensed under WTFPL
                     
// used as screen corner click zone and for black painting

interface

uses
  Utas, Windows, Classes, Graphics, Controls, Forms, Uforms, StdCtrls;

type
  TFhide = class(TFSpyroTAS)
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
  public
    VisibleState: Boolean;
  end;

var
  Fhide: TFhide = nil;

implementation

{$R *.dfm}

uses Umisc;

// right click handler
procedure TFhide.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  Ignores(Button,Shift,X,Y);
  ShowHint := False;
  if Button = mbRight then // only right mouse button
  begin
    VisibleState := not VisibleState;
    if VisibleState then
      AllFormsAction(afaShow)
    else
      AllFormsAction(afaHide);
  end
  else
    SetCapture(Handle); // try capture mouse
end;

end.

// EOF


