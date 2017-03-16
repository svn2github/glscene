unit nDolphin;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ShellAPI,
  GL, GLU, nGL, n3Dpolys;

type
  TDColorForm = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    Label2: TLabel;
    Label36: TLabel;
    Bevel2: TBevel;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label12: TLabel;
    Label11: TLabel;
    Label9: TLabel;
    Label5: TLabel;
    Label10: TLabel;
    Label6: TLabel;
    Label19: TLabel;
    Label17: TLabel;
    Label20: TLabel;
    Label18: TLabel;
    Label23: TLabel;
    Label21: TLabel;
    Label24: TLabel;
    Label22: TLabel;
    Label1: TLabel;
    rXButton: TSpeedButton;
    rYButton: TSpeedButton;
    rZButton: TSpeedButton;
    XButton: TSpeedButton;
    YButton: TSpeedButton;
    ZButton: TSpeedButton;
    RotateButton: TButton;
    TurnButton: TButton;
    HelpBtn: TSpeedButton;
    ExitDolphin: TSpeedButton;
    DolphinBtn: TSpeedButton;
    DolphinFileSave: TSpeedButton;
    DolphinFileOpen: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    MinDistanceEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RotateButtonClick(Sender: TObject);
    procedure TurnButtonClick(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure rXButtonClick(Sender: TObject);
    procedure rYButtonClick(Sender: TObject);
    procedure rZButtonClick(Sender: TObject);
    procedure ZButtonClick(Sender: TObject);
    procedure Panel1MouseUp(Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label28Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure XButtonClick(Sender: TObject);
    procedure YButtonClick(Sender: TObject);
    procedure ExitDolphinClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure DolphinBtnClick(Sender: TObject);
    procedure DolphinFileOpenClick(Sender: TObject);
    procedure DolphinFileSaveClick(Sender: TObject);
    procedure MinDistanceEditChange(Sender: TObject);
procedure EnableAllButtons(Truth:Boolean);
  private
     
  public
     
    Scene: TsceneGL;
    mouse: T3dMouse;
    Dolphin: Tentity;
    ColorActual: longint;
    {this has the actual color I am using for painting}
  end;

var
  DColorForm: TDColorForm;
  rx, ry, rz: word;
  ColorFormInitialized:Boolean;
  Temptress: string;
implementation

{$R *.DFM}

procedure TDColorForm.ExitDolphinClick(Sender: TObject);
begin
  close;
end;
procedure TDColorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
{  DColorForm.Scene.free;}
end;

procedure TDColorForm.FormCreate(Sender: TObject);
var
  luz: Tlight;
 Tempted: string;
begin
  Tempted := ParamStr(0);
  Temptress := ExtractFilePath(Tempted);

If FileExists(Temptress+'dolphin.dxf')then
begin
ColorFormInitialized:=True;
  Scene := TsceneGL.create;
  {creation of the dolphin as an scene´s object}
  Dolphin := Tentity.create;
  Dolphin.SetColor(96, 96, 168);
  Dolphin.LoadDXF(Temptress+'dolphin.dxf');
  Dolphin.CalcNormals;
  Dolphin.Center;
  with Dolphin do
  begin
    Move(0, 0, -10);
    id := 1;
    {now the dolphin is identified with "name" 1 }
  end;
  Scene.Entities.add(Dolphin);
  luz := Tlight.create(1);
  Scene.Lights.add(luz);
  Scene.InitRC(panel1.handle);
  Scene.UpdateArea(panel1.width, panel1.height);
    {Now create the 3dMouse object}
  mouse := T3dMouse.create(Dolphin);
  {set movement and rotation speeds}
  mouse.scale(0.1, -0.1, 0.1, 1, 1, 1);
  ColorActual := $00708598{0}; {pure black}
  label29.color := colorActual;
end else
begin
showmessage('not f');
ColorFormInitialized:=False;
{Disable all buttons except open}
EnableAllButtons(False);
end;
end;

procedure TDColorForm.EnableAllButtons(Truth:Boolean);
begin
{}
rXButton.Enabled:=Truth;
rYButton.Enabled:=Truth;
rZButton.Enabled:=Truth;
XButton.Enabled:=Truth;
YButton.Enabled:=Truth;
ZButton.Enabled:=Truth;
RotateButton.Enabled:=Truth;
TurnButton.Enabled:=Truth;
DolphinFileSave.Enabled:=Truth;
end;

procedure TDColorForm.DolphinFileOpenClick(Sender: TObject);
var
  luz: Tlight;
var
  CheckStr: string;
begin {}
  OpenDialog1.InitialDir := Temptress;
  OpenDialog1.Filter := 'dxf and 3DO files|*.dxf;*.3do';
  If OpenDialog1.filename ='' then
  begin
  If SaveDialog1.filename <> '' then
  begin
  OpenDialog1.filename:=SaveDialog1.filename;
  CheckStr := Uppercase(ExtractFileExt(OpenDialog1.filename));
  if ((CheckStr <> '.3DO')or(CheckStr <> '.DXF') ) then
  OpenDialog1.filename:=ChangeFileExt(OpenDialog1.filename,'.dxf');
  end;end;

  if OpenDialog1.execute then begin
  Temptress := ExtractFilePath(OpenDialog1.filename);
  {Check the file extension }
  CheckStr := Uppercase(ExtractFileExt(OpenDialog1.filename));
If ColorFormInitialized then
begin
if (CheckStr = '.3DO') then
    Dolphin.Load(OpenDialog1.filename{'dolphin.3DO'})else {3DO: 3D object}
if (CheckStr = '.DXF') then
   begin
    Dolphin.LoadDXF(OpenDialog1.filename);
    Dolphin.CalcNormals;
    Dolphin.Center;
   end;
  scene.redraw;
end else
begin
{Enable all buttons}
EnableAllButtons(True);
ColorFormInitialized:=True;
  Scene := TsceneGL.create;
  {creation of the dolphin as an scene´s object}
  Dolphin := Tentity.create;
  Dolphin.SetColor(96, 96, 168);
if (CheckStr = '.3DO') then
    Dolphin.Load(OpenDialog1.filename{'dolphin.3DO'})else {3DO: 3D object}
if (CheckStr = '.DXF') then
  Dolphin.LoadDXF(OpenDialog1.filename);
{    Dolphin.Load('dolphin.3DO');}
{  Dolphin.LoadDXF('dolphin.dxf');}
  Dolphin.CalcNormals;
  Dolphin.Center;
  with Dolphin do
  begin
    Move(0, 0, -10);
    id := 1;
    {now the dolphin is identified with "name" 1 }
  end;
  Scene.Entities.add(Dolphin);
  luz := Tlight.create(1);
  Scene.Lights.add(luz);
  Scene.InitRC(panel1.handle);
  Scene.UpdateArea(panel1.width, panel1.height);
    {Now create the 3dMouse object}
  mouse := T3dMouse.create(Dolphin);
  {set movement and rotation speeds}
  mouse.scale(0.1, -0.1, 0.1, 1, 1, 1);
  ColorActual := $00708598{0}; {pure black}
  label29.color := colorActual;
end;
end;
End;

procedure TDColorForm.Label28Click(Sender: TObject);
begin
  ColorActual := Tlabel(sender).color; {select a new color}
  label29.color := colorActual;
end;

procedure TDColorForm.DolphinFileSaveClick(Sender: TObject);
var
  CheckStr: string;
begin {}
  SaveDialog1.InitialDir := Temptress;
  SaveDialog1.Filter := '3DO files|*.3do';
  If OpenDialog1.filename <> '' then
  begin
  SaveDialog1.filename:=OpenDialog1.filename;
  CheckStr := Uppercase(ExtractFileExt(SaveDialog1.filename));
  if (CheckStr <> '.3DO') then
  SaveDialog1.filename:=ChangeFileExt(SaveDialog1.filename,'.3do');
  end;
  if SaveDialog1.execute then begin
  Temptress := ExtractFilePath(SaveDialog1.filename);
  {Check the file extension }
  CheckStr := Uppercase(ExtractFileExt(SaveDialog1.filename));
  if (CheckStr = '.3DO') then
  Dolphin.Save(SaveDialog1.filename{'dolphin.3DO'}); {3DO: 3D object}
end;
End;


procedure TDColorForm.FormResize(Sender: TObject);
begin
{  panel2.top:=height-134;}
  Panel3.left := width - 186; {12+ 177 +9}
  panel1.height := height - 40;
  panel1.width := width - 198;
  If ColorFormInitialized then
  Scene.UpdateArea(panel1.width, panel1.height);
end;


procedure TDColorForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(15000);
end;

procedure TDColorForm.DolphinBtnClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'www.geocities.com/SiliconValley/Way/2132',
    '', '',
    SW_SHOW);
{Ricardo Sarmiento.  1997
ERIKA RINCON.  1998
mollyware@geocities.com

visualization of an
Amazon pink dolphin
(scientific name: Inia geoffrensis geoffrensis  }
end;


procedure TDColorForm.RotateButtonClick(Sender: TObject);
begin
  if mouse.mode = 1 then
  begin
    mouse.mode := 3;
    RotateButton.caption := 'Rotate';
  end
  else
  begin
    mouse.mode := 1;
    RotateButton.caption := 'Move';
  end
end;

procedure TDColorForm.TurnButtonClick(Sender: TObject);
begin
  rx := random(360);
  ry := random(360);
  rz := random(360);
  Tentity(Scene.Entities.Items[0]).Rotate(rx, ry, rz);
  {turn aleatorily the dolphin}
  Scene.Redraw;
  {and redraw}
end;

procedure TDColorForm.rXButtonClick(Sender: TObject);
begin              {block rotation in X}
  mouse.Block(4, TspeedButton(sender).down); {rx}
end;
procedure TDColorForm.rYButtonClick(Sender: TObject);
begin
  mouse.Block(5, TspeedButton(sender).down); {ry}
end;
procedure TDColorForm.rZButtonClick(Sender: TObject);
begin
  mouse.Block(6, TspeedButton(sender).down); {rz}
end;
procedure TDColorForm.ZButtonClick(Sender: TObject);
begin
  mouse.Block(3, TspeedButton(sender).down); {z}
end;
procedure TDColorForm.XButtonClick(Sender: TObject);
begin
  mouse.Block(1, TspeedButton(sender).down); {X}
end;
procedure TDColorForm.YButtonClick(Sender: TObject);
begin
  mouse.Block(2, TspeedButton(sender).down); {Y}
end;

procedure TDColorForm.Panel1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  If ColorFormInitialized then
  begin
  mouse.Move(x, y, shift); {drag the dolphin}
  Scene.Redraw; {and redraw}
  end;
end;

procedure TDColorForm.Panel1MouseUp(Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  point: Tvertex;
  poly, vert, i: longint;
  iR, iG, iB: byte;
begin
  If ColorFormInitialized then
  begin
{get selected vertices, they are stored in VertexHits}
  mouse.FindVertex(x, panel1.height - y, Scene, point);
  {for all vertices found with FindVertex}
  for i := 1 to numFound do
  begin
    poly := VertexHits[i] shr 16;
    vert := VertexHits[i] mod 65536;
    if (poly < Dolphin.Faces.count) and
    {make sure it is allright}
    (vert < Tface(Dolphin.Faces.Items[poly]).Vertices.count) then
    begin {why the mod ?, see delphi help about Tcolor}
      ir := (ColorActual) mod 256; {red}
      ig := (ColorActual shr 8) mod 256; {green}
      ib := (ColorActual shr 16) mod 256; {blue}
      Tvertex(Tface(
        Dolphin.Faces.Items[poly])
        .Vertices.items[vert]).point.setcolor(ir, ig, ib);
    end;
  end;
  {OpenGL said that the mouse was in xxx,  }
  label17.caption := intTostr(xxx);
{OpenGL said that the mouse was in yyy   }
  label18.caption := intTostr(yyy);
  {mouse x according to windows}
  label5.caption := intTostr(x);
  {mouse y according to windows}
  label6.caption := intTostr(y);
  {windows and OpenGL differ a little about the mouse position}
  label21.caption := intTostr(x - xxx);
  label22.caption := intTostr(y - yyy);
  if numFound = 0 then {no vertex found ? then show nothing}
  begin
    label17.caption := '  ';
    label18.caption := '  ';
    label21.caption := '  ';
    label22.caption := '  ';
  end;
  {number of vertices clicked}
  label11.caption := intTostr(numfound);
end;
End;


procedure TDColorForm.MinDistanceEditChange(Sender: TObject);
begin
if MinDistanceEdit.modified then
MinimalDistance := strtofloat(MinDistanceEdit.text);
end;

end.
