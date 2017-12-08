unit u_Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.ExtCtrls,
   
  GLScene,
  GLObjects,
  GLCoordinates, GLWin32Viewer, GLCrossPlatform,
  GLNodes,
  GLBaseClasses, GLHUDObjects,
  GLBitmapFont, GLWindowsFont, GLRenderContextInfo,
  GLVectorGeometry, GLVectorTypes,
  GLCadencer, GLColor,
  GLFileTGA,
  GLGraph;


type
  TForm1 = class(TForm)
    Memo1: TMemo;
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    cam: TGLCamera;
    dc_world: TGLDummyCube;
    dogl: TGLDirectOpenGL;
    sprt: TGLHUDSprite;
    txt1: TGLHUDText;
    wbmpFont: TGLWindowsBitmapFont;
    cad: TGLCadencer;
    Lines1: TGLLines;
    Lines2: TGLLines;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    grid1: TGLXYZGrid;
    grid2: TGLXYZGrid;
    txt2: TGLHUDText;
    pts: TGLPoints;
    cam_XY: TGLCamera;
    cam_XZ: TGLCamera;
    cam_YZ: TGLCamera;
    procedure FormCreate(Sender: TObject);
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vpMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cadProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);

  public
    procedure loadData(a_FileName:string);
  end;


type
  t_point = record
    x,z: single;
    id: integer;
    sub: array[0..9] of single;
  end;


var
  Form1: TForm1;

  // список вершин графа
  points: array of t_point;
  p_cnt: integer;
  m_turn: boolean = false;
  m_pos: TPoint;


implementation

{$R *.dfm}

//
// loadData
//
procedure TForm1.loadData(a_FileName:string);
const
    c: array[1..4] of cardinal = ($222222, $ff2222, $0000ff, $00ff00);

var
    s,t,u: TStringList;
    pi,li,pc,i,j,k: integer;
    p1,p2,p3: TPoint;
    v1,v2,v3: TVector3f;
    f,len: single;
    fs: TFormatSettings;

  function getpid(s:string):TPoint;
  var f:single;
  begin
    f := strtofloat(s, fs);
    result.x := round(f) - 1;                   // номер вершины
    result.y := round((f - round(f)) * 100);    // значение
  end;

begin
  if not fileexists(a_FileName) then exit;

  s := TStringList.Create;
  s.LoadFromFile(a_FileName);

  pi := s.IndexOf('[points]');
  li := s.IndexOf('[links]');

  if (pi < 0) or (li < 0) then begin
    s.Free;
    exit;
  end;

  t := TStringList.Create;
  u := TStringList.Create;
  fs.DecimalSeparator := '.';


  // парсим вершины
  pc := 0;
  for i := pi + 1 to li - 1 do
    if length(s[i]) > 7 then inc(pc);

  setlength(points, pc);
  p_cnt := 0;

  for i := 0 to pc - 1 do begin
    // готовим массив
    for j := 0 to high(points[i].sub) do
      if j = 0 then points[i].sub[j] := 0
        else points[i].sub[j] := 9999;

    t.Text := stringReplace(s[i + pi + 1], ' ', #13#10, [rfReplaceAll]);

    // заполняем массив вершин
    for j := 0 to t.Count - 1 do begin

      u.Text := stringReplace(t[j], ',', #13#10, [rfReplaceAll]);

      if u.Count = 3 then begin
        points[i].id := getpid(u[0]).x;
        points[i].x := strtofloat(u[1], fs);
        points[i].z := strtofloat(u[2], fs);
        inc(p_cnt);
        end
      else begin
        points[i].sub[getpid(u[0]).y] := strtofloat(u[1], fs);
        inc(p_cnt);
        end;
      end;
    end;


  // парсим связи сразу в линии
  Lines1.Nodes.Clear;
  Lines2.Nodes.Clear;
  pts.Positions.Clear;
  pts.Colors.Clear;

  f := 0.11;

  for i := li + 1 to min(s.Count - 1, li + 5) do
  begin
    t.Text := stringReplace(s[i], ' ', #13#10, [rfReplaceAll]);
    for j := 0 to t.Count - 1 do
      if length(t[j]) = 9 then begin

        u.Text := stringReplace(t[j], ':', #13#10, [rfReplaceAll]);

        p1 := getpid(u[0]);
        p2 := getpid(u[1]);

        if (points[p1.x].sub[p1.y] = 9999) or
           (points[p2.x].sub[p2.y] = 9999) then continue;

        if points[p2.x].sub[p2.y] > points[p1.x].sub[p1.y] then
        begin
          p3 := p2;
          p2 := p1;
          p1 := p3;
        end;

        with points[p1.x] do
          setvector(v1, x, sub[p1.y], z);

        // вершины сверху и снизу
        if (points[p2.x].sub[p2.y] < 0) and
           (points[p1.x].sub[p1.y] >= 0) then
        begin
          with points[p2.x] do
            setvector(v3, x, sub[p2.y], z);

          // пересечение с нулевой плоскостью
          v2 := VectorLerp(v1, v3, - v1.Y / (v3.Y - v1.Y));

          pts.Positions.Add(v2);
          pts.Colors.Add(ConvertWinColor(c[i - li - 1]));

          len := vectorDistance(v2, v3);

          lines2.Nodes.AddNode(v2);
          TGLLinesNode(lines2.Nodes.Last).Color.AsWinColor := c[i - li - 1];

          lines2.Nodes.AddNode(vectorLerp(v2, v3, 1 - f / len));
          TGLLinesNode(lines2.Nodes.Last).Color.AsWinColor := c[i - li - 1];

          len := vectorDistance(v1, v2);

          Lines1.Nodes.AddNode(vectorLerp(v1, v2, f / len));
          TGLLinesNode(lines1.Nodes.Last).Color.AsWinColor := c[i - li - 1];

          Lines1.Nodes.AddNode(v2);
          TGLLinesNode(lines1.Nodes.Last).Color.AsWinColor := c[i - li - 1];
        end
        else
        begin
          with points[p2.x] do
            setvector(v2, x, sub[p2.y], z);

          // обе вершины снизу
          if (points[p2.x].sub[p2.y] < 0) and
             (points[p1.x].sub[p1.y] < 0) then begin

            len := vectorDistance(v1, v2);

            lines2.Nodes.AddNode(vectorLerp(v1, v2, f / len));
            TGLLinesNode(lines2.Nodes.Last).Color.AsWinColor := c[i - li - 1];

            lines2.Nodes.AddNode(vectorLerp(v1, v2, 1 - f / len));
            TGLLinesNode(lines2.Nodes.Last).Color.AsWinColor := c[i - li - 1];

            end
          // обе вершины сверху
          else begin
            len := vectorDistance(v1, v2);
            Lines1.Nodes.AddNode(vectorLerp(v1, v2, f / len));

            TGLLinesNode(lines1.Nodes.Last).Color.AsWinColor := c[i - li - 1];

            Lines1.Nodes.AddNode(vectorLerp(v1, v2, 1 - f / len));
            TGLLinesNode(lines1.Nodes.Last).Color.AsWinColor := c[i - li - 1];
            end;
          end;
        end;
    end;
  s.Free;
  t.Free;
  u.Free;
end;


//
// FormCreate
//
procedure TForm1.FormCreate;
begin
  loadData('data');
end;

//
// vpMouseDown
//
procedure TForm1.vpMouseDown;
begin
  if shift = [ssleft] then begin
    m_turn := true;
    m_pos := mouse.CursorPos;
  end;
end;


//
// vpMouseUp
//
procedure TForm1.vpMouseUp;
begin
  m_turn := false;
end;


//
// cadProgress
//
procedure TForm1.cadProgress;
begin
  if m_turn and (vp.Camera = cam) then begin
    with mouse.CursorPos do
      cam.MoveAroundTarget(m_pos.y - y, m_pos.x - x);

    m_pos := mouse.CursorPos;
  end;
  pts.Visible := vp.Camera = cam;

  lines1.Visible := pts.Visible;
  lines2.Visible := pts.Visible;

  grid1.Visible := pts.Visible;
  grid2.Visible := pts.Visible;
end;


//
// doglRender
//
procedure TForm1.doglRender;
var
    i,j,k: integer;
    v: TVector;
    a1: array of record
      id,sub: integer;
      sx,sy,depth: single;
      end;

 procedure shellSort;
 var
     i,j,k,n: integer;
     f: single;
 begin
   k := high(a1) div 2;
   while k > 0 do
   begin
     for i := 0 to high(a1) - k do begin
       j := i;
       while (j >= 0) and (a1[j].depth < a1[j + k].depth) do
       begin
         n := a1[j].id; a1[j].id := a1[j + k].id; a1[j + k].id := n;
         n := a1[j].sub; a1[j].sub := a1[j + k].sub; a1[j + k].sub := n;
         f := a1[j].sx; a1[j].sx := a1[j + k].sx; a1[j + k].sx := f;
         f := a1[j].sy; a1[j].sy := a1[j + k].sy; a1[j + k].sy := f;
         f := a1[j].depth; a1[j].depth := a1[j + k].depth; a1[j + k].depth := f;
         if j > k then Dec(j, k)
         else j := 0;
         end;
       end;
       k := k div 2
     end;
   end;

begin
  SetLength(a1, p_cnt);
  k := 0;
  for i := 0 to High(points) do
    for j := 0 to High(points[i].sub) do
    begin
      if points[i].sub[j] = 9999 then
        continue;

      a1[k].id := points[i].id + 1;
      a1[k].sub := j;

      // позиция в пространстве и квадрат расстояния до камеры
      with points[i] do
        setVector(v, x, sub[j], z);
      a1[k].depth := vectorNorm(vectorSubtract(v, cam.AbsolutePosition));

      // позиция вершины в экранных координатах
      v := vp.Buffer.WorldToScreen(v);
      a1[k].sx := v.X;
      a1[k].sy := vp.Height - v.Y;
      inc(k);
    end;

  // сортируем по расстоянию до камеры
  shellSort;
  for i := 0 to p_cnt - 1 do
    with a1[i] do begin
      sprt.Position.SetPoint(sx, sy, 0);
      sprt.Render(rci);
      txt1.Position.SetPoint(sx - 2, sy, 0);
      txt1.Text := inttostr(id);
      txt1.Render(rci);

      if sub > 0 then begin
        txt2.Position.SetPoint(sx + wbmpFont.TextWidth(txt1.Text) / 2 + 1,
          sy - wbmpFont.CharHeight / 2 + 7, 0);
        txt2.Text := inttostr(sub);
        txt2.Render(rci);
        end;
      end;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  vp.Camera := cam_XY;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  vp.Camera := cam_XZ
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  vp.Camera := cam_YZ;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  vp.camera := cam;
end;

end.



