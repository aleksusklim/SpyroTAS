unit UFover; // SpyroTAS is licensed under WTFPL

// controller overlay form

interface

uses
  Utas, Windows, Classes, Graphics, Controls, Forms, ExtCtrls, Uforms, SysUtils;

type
  TColorButtons = (ColorArrows, ColorTriangle, ColorSquare, ColorCircle,
    ColorCross, ColorOther);

type
  TFover = class(TFSpyroTAS)
    shp_Cross: TShape;
    shp_Triangle: TShape;
    shp_Square: TShape;
    shp_Circle: TShape;
    shp_Up: TShape;
    shp_Right: TShape;
    shp_Down: TShape;
    shp_Left: TShape;
    shp_L2: TShape;
    shp_L1: TShape;
    shp_R1: TShape;
    shp_R2: TShape;
    shp_L3: TShape;
    shp_R3: TShape;
    shp_Start: TShape;
    shp_Select: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure shp_anyMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  public
    procedure InitCounter(Second: Boolean);
  private
    procedure AdjustAlpha();
    procedure AdjustText(Resize: Boolean);
    procedure AdjustButtons();
    procedure ChangeButton(Shape: TShape; IsPressed: Integer; ColorType: TColorButtons);
  public
    IsSecondController: Boolean;
    NoBorders: Boolean;
  private
    WasDrawn: Boolean;
    ThisIsCounter: Boolean;
    RightAlign: Boolean;
    CurrentString, OldString: string;
    CurrentButtons, OldButtons: Integer;
  end;

var
  Fover1: TFover = nil; // for first
  Fover2: TFover = nil; // and second controllers
  FoverF: TFover = nil; // frame counter overlay
  FoverR: TFover = nil; // re-record monitor

var
  PadWindowsAlpha: Integer;

procedure UpdateOverlays();

procedure RequestOverlayRedraw(Keys: Integer; Counter, Rerecord: Integer);

function AreOverlaysVisible(): Boolean;

procedure AdjustAllAlpha(Alpha: Integer);

implementation

{$R *.dfm}

uses
  Ukey, Umisc, Uglob, Umain, Math;

const
  INITIAL_WIDTH = 128; // default overlay size
  INITIAL_HEIGHT = 64;
  TRANSPARENT_COLOR = TColor(1); // any color that not used in painting buttons

var
  TextWidth, TextHeight: Integer;

function AreOverlaysVisible(): Boolean;
begin
  Result := Fover1.Visible or Fover2.Visible or FoverF.Visible or FoverR.Visible;
end;

procedure RequestOverlayRedraw(Keys: Integer; Counter, Rerecord: Integer);
begin
  if not IsGuiInvoked or GuiClosing then
    Exit;
  if Fover1 <> nil then
    Fover1.CurrentButtons := Keys and $ffff;
  if Fover2 <> nil then
    Fover2.CurrentButtons := Keys shr 16;
  if FoverF <> nil then
  begin
    if (Counter < 0) and (Counter >= SprintValue) then
      FoverF.CurrentString := '--'
    else
      FoverF.CurrentString := IntToStr(Counter);
  end;
  if FoverR <> nil then
    FoverR.CurrentString := IntToStr(Rerecord);
end;

// paint a button on pad overlay:
procedure TFover.ChangeButton(Shape: TShape; IsPressed: Integer; ColorType:
  TColorButtons);
const
  Max = 255;
  Min = 64; // craft color mask:
  Light: array[0..4] of Byte = (0, Max, Max, Max, Max); // for simplicity
  Dark: array[0..4] of Byte = (0, Min, Min, Min, Min);
  Black = 0;
  Red = 1;
  Green = 2;
  Blue = 4;
  Yellow = Red + Green; // bits
  Cyan = Green + Blue;
  Magenta = Blue + Red;
var
  Mask: Byte;
begin
  case ColorType of
    ColorArrows: // up-left-right-down
      Mask := Yellow;
    ColorOther: // L123, R123, start-select
      Mask := Blue;
    ColorTriangle:
      Mask := Green;
    ColorSquare:
      Mask := Magenta;
    ColorCircle:
      Mask := Red;
    ColorCross:
      Mask := Cyan;
  else
    Mask := Black;
  end;
  if IsPressed <> 0 then  // actually boolean
  begin
    if Shape.Pen.Color = clWhite then // already pressed
      Exit;
    Shape.Brush.Color := RGB(Light[Mask and Red], Light[Mask and Green], Light[Mask
      and Blue]);
    Shape.Pen.Color := clWhite;
  end
  else
  begin
    if Shape.Pen.Color = clBlack then // already released
      Exit;
    Shape.Brush.Color := RGB(Dark[Mask and Red], Dark[Mask and Green], Dark[Mask
      and Blue]);
    Shape.Pen.Color := clBlack;
  end;
end;

procedure AdjustAllAlpha(Alpha: Integer);
begin
  PadWindowsAlpha := Alpha;
  if Fover1 <> nil then
    Fover1.AdjustAlpha();
  if Fover2 <> nil then
    Fover2.AdjustAlpha();
  if FoverF <> nil then
    FoverF.AdjustAlpha();
  if FoverR <> nil then
    FoverR.AdjustAlpha();
end;

// set transparency:
procedure TFover.AdjustAlpha();
begin
  if GuiClosing then
    Exit;
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
  SetLayeredWindowAttributes(Handle, Color, PadWindowsAlpha, LWA_COLORKEY or LWA_ALPHA);
  Popup();
  WasDrawn := False;
  Repaint();
end;

// calculate buttons layout:
procedure TFover.AdjustButtons();
var
  rw, rh: Real;
  w, h: Integer;
begin
  if GuiClosing then
    Exit;
  // <please_dont_ask>
//  LockWindowUpdate(Handle);
  rw := ClientWidth / 6;
  rh := ClientHeight / 3;
  w := Floor(rw);
  h := Floor(rh);
  shp_L2.SetBounds(Floor(0 * rw), Floor(0 * rh), w, h);
  shp_Up.SetBounds(Floor(1 * rw), Floor(0 * rh), w, h);
  shp_L1.SetBounds(Floor(2 * rw), Floor(0 * rh), w, h);
  shp_R1.SetBounds(Floor(3 * rw), Floor(0 * rh), w, h);
  shp_Triangle.SetBounds(Floor(4 * rw), Floor(0 * rh), w, h);
  shp_R2.SetBounds(Floor(5 * rw), Floor(0 * rh), w, h);
  shp_Left.SetBounds(Floor(0 * rw), Floor(1 * rh), w, h);
  shp_Right.SetBounds(Floor(2 * rw), Floor(1 * rh), w, h);
  shp_Square.SetBounds(Floor(3 * rw), Floor(1 * rh), w, h);
  shp_Circle.SetBounds(Floor(5 * rw), Floor(1 * rh), w, h);
  shp_L3.SetBounds(Floor(0 * rw), Floor(2 * rh), w, h);
  shp_Down.SetBounds(Floor(1 * rw), Floor(2 * rh), w, h);
  shp_Select.SetBounds(Floor(2 * rw), Floor(2 * rh), w, h);
  shp_Start.SetBounds(Floor(3 * rw), Floor(2 * rh), w, h);
  shp_Cross.SetBounds(Floor(4 * rw), Floor(2 * rh), w, h);
  shp_R3.SetBounds(Floor(5 * rw), Floor(2 * rh), w, h);
  h := Floor(rh / 4);
  w := Floor(rw / 4);
  shp_Up.Top := shp_Up.Top + h;
  shp_Triangle.Top := shp_Triangle.Top + h;
  shp_Down.Top := shp_Down.Top - h;
  shp_Cross.Top := shp_Cross.Top - h;
  shp_Left.Left := shp_Left.Left + w;
  shp_Square.Left := shp_Square.Left + w;
  shp_Right.Left := shp_Right.Left - w;
  shp_Circle.Left := shp_Circle.Left - w;
  shp_L1.Width := shp_L1.Width - w;
  shp_Start.Width := shp_Start.Width - w;
  shp_R2.Width := shp_R2.Width - w;
  shp_L3.Width := shp_L3.Width - w;
  shp_L2.Left := shp_L2.Left + w;
  shp_L2.Width := shp_L2.Width - w;
  shp_Select.Left := shp_Select.Left + w;
  shp_Select.Width := shp_Select.Width - w;
  shp_R1.Left := shp_R1.Left + w;
  shp_R1.Width := shp_R1.Width - w;
  shp_R3.Left := shp_R3.Left + w;
  shp_R3.Width := shp_R3.Width - w;
  shp_L2.Height := shp_L2.Height - h;
  shp_R2.Height := shp_R2.Height - h;
  shp_Select.Height := shp_Select.Height - h;
  shp_Start.Height := shp_Start.Height - h;
  shp_L3.Top := shp_L3.Top + h;
  shp_L3.Height := shp_L3.Height - h;
  shp_R3.Top := shp_R3.Top + h;
  shp_R3.Height := shp_R3.Height - h;
  shp_L1.Top := shp_L1.Top + h;
  shp_L1.Height := shp_L1.Height - h;
  shp_R1.Top := shp_R1.Top + h;
  shp_R1.Height := shp_R1.Height - h;
  WasDrawn := False;
  Repaint();
//  LockWindowUpdate(0);
  // </please_dont_ask>
end;

// initial size:
procedure TFover.FormCreate(Sender: TObject);
begin
  StopWipingHappen := True;
  ThisIsOverlay := True;
  SetSizeClient(Left, Top, INITIAL_WIDTH, INITIAL_HEIGHT, True);
  Color := TRANSPARENT_COLOR;
  FormStyle := fsStayOnTop;
  AdjustButtons();
end;

// set the title according to controller number:
procedure TFover.FormShow(Sender: TObject);
begin
  if ThisIsCounter then
  begin
    if SprintMode then
    begin
      if IsSecondController then
        Caption := 'Current FPS / - Average FPS (*1000)'
      else
        Caption := 'Current frame / - FPS deviation'
    end
    else
    begin
      if IsSecondController then
        Caption := 'Re-records:'
      else
        Caption := 'Frame counter:';
    end;
  end
  else
  begin
    if IsSecondController then
      Caption := 'Port 2:'
    else
      Caption := 'Port 1:';
  end;
  OldString := '';
  OldButtons := -1;
  UpdateOverlays();
end;

// resized:
procedure TFover.FormResize(Sender: TObject);
begin
  if ThisIsCounter then
    AdjustText(False)
  else
    AdjustButtons();
end;

// clicks:
procedure TFover.shp_anyMouseDown(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  Rect: TRect;
  TmpCol: TColor;
begin
  if GuiClosing then
    Exit;
  Ignores(Shift, X, Y);
  if Button = mbRight then // right click
  begin
    if ThisIsCounter then
    begin
      if NoBorders then
      begin
        TmpCol := Canvas.Brush.Color;
        Canvas.Brush.Color := Canvas.Pen.Color;
        Canvas.Pen.Color := TmpCol;
        if TmpCol = 0 then
        begin
          Canvas.Pen.Style := psSolid;
          Canvas.Pen.Width := Canvas.Pen.Width + 1;
          if Canvas.Pen.Width = 3 then
          begin
            Canvas.Pen.Width := 0;
            Canvas.Pen.Style := psClear;
          end;
        end;
      end
      else
        RightAlign := not RightAlign;
      AdjustAlpha();
      Repaint();
      Exit;
    end;
    if Fover1.Visible <> Fover2.Visible then // only one is visible
    begin
      Height := Round(Width * INITIAL_HEIGHT / INITIAL_WIDTH); // restore aspect ratio
      AdjustAlpha();
      Exit;
    end;
    if IsSecondController then // make as big as first
      Rect := Fover1.GetSizeClient()
    else
      Rect := Fover2.GetSizeClient(); // or as second
    SetSizeClient(Left, Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, True);
    AdjustAlpha();
    Exit;
  end;
  // toggle border, preserve real client size
  Rect := GetSizeClient();
  if NoBorders then
    BorderStyle := bsSizeable
  else // (here and there are fixes for Lazarus)
    BorderStyle := bsNone;
  SetSizeClient(Rect, False);
  NoBorders := not NoBorders;
  Visible := True;
  ForceSize();
  if ThisIsCounter then
    AdjustText(True)
  else
    AdjustButtons();
  AdjustAlpha();
end;

procedure TFover.AdjustText(Resize: Boolean);
var
  Rect: TRect;
begin
  Rect := GetSizeClient();
  Rect.Right := (Rect.Right - Rect.Left);
  Rect.Bottom := (Rect.Bottom - Rect.Top);
  if (Rect.Right < 2) or (Rect.Bottom < 2) then
    Exit;
  if (Rect.Right / Rect.Bottom) > (TextWidth / TextHeight) then
    Rect.Right := Round(Rect.Bottom * TextWidth / TextHeight)
  else
    Rect.Bottom := Round(Rect.Right * TextHeight / TextWidth);
  Font.Height := -Rect.Bottom;
  if Resize then
    SetSizeClient(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom + 2, False);
end;

procedure TFover.InitCounter(Second: Boolean);
const
  Sample = '002147483647';
begin
  IsSecondController := Second;
  RightAlign := Second;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clWhite;
  Canvas.Pen.Width := 1;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBlack;
  Canvas.Font.Size := 48;
  TextHeight := Canvas.TextHeight(Sample);
  TextWidth := Canvas.TextWidth(Sample);
  ThisIsCounter := True;
  shp_Cross.Visible := False;
  shp_Triangle.Visible := False;
  shp_Square.Visible := False;
  shp_Circle.Visible := False;
  shp_Up.Visible := False;
  shp_Left.Visible := False;
  shp_Down.Visible := False;
  shp_Right.Visible := False;
  shp_L1.Visible := False;
  shp_L2.Visible := False;
  shp_R1.Visible := False;
  shp_R2.Visible := False;
  shp_L3.Visible := False;
  shp_R3.Visible := False;
  shp_Start.Visible := False;
  shp_Select.Visible := False;
end;

procedure DrawTextOutline(const Canvas: TCanvas; const X, Y: Integer; const Text: string);
var
  OldBkMode: Integer;
begin
  OldBkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
  BeginPath(Canvas.Handle);
  Canvas.TextOut(X, Y, Text);
  EndPath(Canvas.Handle);
  StrokeAndFillPath(Canvas.Handle);
  SetBkMode(Canvas.Handle, OldBkMode);
end;

procedure TFover.FormPaint(Sender: TObject);
var
  X: Integer;
begin
  if not ThisIsCounter then
    Exit;
  if RightAlign then
    X := ClientWidth - Canvas.TextWidth(CurrentString) - 1
  else
    X := 0;
  DrawTextOutline(Canvas, X, 0, CurrentString);
end;

procedure UpdateOverlays();

  procedure ForPad(Pad: TFover);
  begin
    if (Pad <> nil) and (Pad.Visible) and (Pad.CurrentButtons <> Pad.OldButtons) then
      with Pad do
      begin
        OldButtons := CurrentButtons;
        ChangeButton(shp_Cross, pad_Cross and OldButtons, ColorCross);
        ChangeButton(shp_Triangle, pad_Triangle and OldButtons, ColorTriangle);
        ChangeButton(shp_Square, pad_Square and OldButtons, ColorSquare);
        ChangeButton(shp_Circle, pad_Circle and OldButtons, ColorCircle);
        ChangeButton(shp_Up, pad_Up and OldButtons, ColorArrows);
        ChangeButton(shp_Left, pad_Left and OldButtons, ColorArrows);
        ChangeButton(shp_Down, pad_Down and OldButtons, ColorArrows);
        ChangeButton(shp_Right, pad_Right and OldButtons, ColorArrows);
        ChangeButton(shp_L1, pad_L1 and OldButtons, ColorOther);
        ChangeButton(shp_L2, pad_L2 and OldButtons, ColorOther);
        ChangeButton(shp_R1, pad_R1 and OldButtons, ColorOther);
        ChangeButton(shp_R2, pad_R2 and OldButtons, ColorOther);
        ChangeButton(shp_L3, pad_L3 and OldButtons, ColorOther);
        ChangeButton(shp_R3, pad_R3 and OldButtons, ColorOther);
        ChangeButton(shp_Start, pad_Start and OldButtons, ColorOther);
        ChangeButton(shp_Select, pad_Select and OldButtons, ColorOther);
        Repaint();
      end;
  end;

  procedure ForCouner(Couner: TFover);
  begin
    if (Couner <> nil) and (Couner.Visible) and (Couner.CurrentString <> Couner.OldString)
      then
      with {%H-}Couner do
      begin
        OldString := CurrentString;
        Repaint();
      end;
  end;

begin
  if GuiClosing then
    Exit;
  ForPad(Fover1);
  ForPad(Fover2);
  ForCouner(FoverF);
  ForCouner(FoverR);
end;

end.

// EOF

// Fover


