unit Contour;
{Demonstrates contour drawing features of MathImage, as well as the use
 of the TSurface/TLevelSurface object. The routines marked by *********** use
 MathImage methods.
 The filled level lines routine is very memory intensive. I found
 Robert Lee's memory manager replacement (available from Code Central)
 to
 a) speed up things a lot,
 b) allow me to use more grid points.}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.Clipbrd,
  //
  MathImage,
  OverlayImage;

const
  gxmin = -3; gxmax = 6.5; gymin = -4; gymax = 4; {graph domain}
  xMesh = 100; yMesh = 100; {graph mesh}
  c = 4;
  colorarray: array[0..13] of TColor = ($00CB9F74, $00D8AD49, $00E6C986,
    $00F2E3C1, $00DAF0C4, $00A6E089, $0086D560, $0065CFB5, $008DC5FC, $0075D5FD,
    $0078E1ED, $00ACEDF4, $00D0F2F7, $00F2FBFD);
  levelsarray: array[0..13] of MathFloat = (-1, -0.7, -0.4, -0.2, 0, 0.2, 0.4,
    0.6, 0.8, 1.1, 1.4, 1.6, 2.2, 4);

type
  TContourform = class(TForm)
    Panel1: TPanel;
    ContourlinesButton: TButton;
    ColorDialog1: TColorDialog;
    Panel2: TPanel;
    FilledContoursButton: TButton;
    GraphImage: TMathImage;
    Label1: TLabel;
    procedure ContourlinesButtonClick(Sender: TObject);
    procedure GraphImageResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FilledContoursButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CurrentType: Integer;
    GraphSurface: TLevelSurface;
    LevelsMade: Boolean;
    procedure Graph(x, y: MathFloat; var z: MathFloat);
    procedure MakeGraphSurface;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    values: array[0..13] of TLabel;
    Colors: array[0..13] of TLabel;
  end;

var
  ContourForm: TContourform;

implementation


uses
  MDemo1;


{$R *.DFM}

procedure TContourform.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WndParent := Demoform.Handle;
    Parent := Demoform;
    Style := WS_CHILD or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
    Align := alClient;
  end;
end;


{*************************************}

procedure TContourform.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  makegraphsurface;
  ControlStyle := ControlStyle + [csOpaque];
  currenttype := 1;
  for i := 0 to 13 do
  begin
    values[i] := TLabel.Create(self);
    with values[i] do
    begin
      Top := Label1.Top + Label1.Height + 5 + i * Label1.Height;
      Left := Label1.Left;
      Parent := Panel1;
      Caption := FloatToStrf(levelsarray[i], ffgeneral, 4, 4);
    end;
    Colors[i] := TLabel.Create(self);
    with Colors[i] do
    begin
      autosize := False;
      Caption := '';
      Left := Label1.Left + Label1.Width;
      Top := values[i].Top;
      Width := values[i].Height;
      Height := Width;
      Color := colorarray[i];
      Parent := Panel1;
    end;
  end;
end;

procedure TContourform.Graph(x, y: MathFloat; var z: MathFloat);
{graph formula}
begin

  //this first one is a real test, it looks crummy unless you
  //give it a mesh of about 200x200, in which case the filled level
  //curve drawing runs out of mem under Win2K on my machine...
  //****Cure: Use Robert Lee's memory manager replacement,
  //available from CodeCentral, it's awesome.****************
  //There isn't anything I can do about level
  //curves near intersection points looking bad,
  //other than increasing the mesh.

 // z := 2 * (cos(1.5 * (x - y)) + sin(1.6 * (x + 0.6 * y))) + 0.8;

  if (x <> c) or (y <> 0) then
    z := sin(sqrt(sqr(x) + sqr(y))) + 1 / sqrt(sqr(x - c) + sqr(y))
  else
    z := 1.0E10
end;

{**************************}

procedure TContourform.ContourlinesButtonClick(Sender: TObject);
var      SavePen: TPen;      i: Integer;
begin
  Screen.Cursor := CrHourGlass;
  currenttype := 1;
  with GraphImage do
  begin
    SetWorld(gxmin, gymin, gxmax, gymax);
    Clear;
    DrawAxes('x  ', 'y', False, clSilver, clSilver, False);
    SavePen := TPen.Create;
    SavePen.assign(Pen);
    for i := 0 to High(levelsarray) do
    begin
      Pen.Color := colorarray[i];
      DrawLevelCurves(graphsurface, levelsarray[i]);
    end;
    Pen.assign(SavePen);
    SavePen.Free;
  end;
  Screen.Cursor := crDefault;
end;

{******************************}

procedure TContourform.FilledContoursButtonClick(Sender: TObject);
begin
  Screen.Cursor := CrHourGlass;
  currenttype := 2;
  with GraphImage do
  begin
    if not LevelsMade then
    begin          //The following is what takes long
      graphsurface.SetLevels(levelsarray, colorarray);
      LevelsMade := True;
    end;
    SetWorld(gxmin, gymin, gxmax, gymax);
    Clear;
    DrawAxes('x  ', 'y', False, clSilver, clSilver, False);
    DrawFilledLevelCurves(graphsurface);
  end;
  Screen.Cursor := crDefault;
end;
{*****************************************}

procedure TContourform.MakeGraphSurface;
var
  i, j: Integer; x, y, z: MathFloat;
begin
  graphsurface := TLevelSurface.Create(xMesh, yMesh);
  for i := 0 to xMesh do
  begin
    x := gxmin + i * (gxmax - gxmin) / xMesh;
    for j := 0 to yMesh do
    begin
      y := gymin + j * (gymax - gymin) / yMesh;
      Graph(x, y, z);
      graphsurface.Make(i, j, x, y, z);
    end;
  end;
  LevelsMade := False;
end;


{****************************}

procedure TContourform.GraphImageResize(Sender: TObject);
begin
  if currenttype = 1 then
    ContourlinesButtonClick(self)
  else FilledContoursButtonClick(self);
end;


procedure TContourform.FormDestroy(Sender: TObject);
begin
  graphsurface.Free;
end;

procedure TContourform.FormShow(Sender: TObject);
begin
///  SaveasMetafile1.enabled := False;
  ContourlinesButtonClick(self); //Already done in GraphimageResize
end;


initialization



end.

