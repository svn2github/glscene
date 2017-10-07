unit main;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Imaging.Jpeg,

  GLScene,
  GLCadencer,
  GLWin32Viewer,
  GLCrossPlatform,
  GLBaseClasses,
  GLMaterial,
  GLVectorFileObjects,
  GLCoordinates,
  GLVectorTypes,
  GLVectorGeometry,
  GLProxyObjects,
  GLKeyboard,
  GLObjects,
  GLFile3DSSceneObjects,
  GLGeomObjects,
  GLFile3DS,
  GLTexture,
  GLGraphics,
  GLRenderContextInfo,
  GLHUDObjects;

type
  Tmain_form = class(TForm)
    GLScene1: TGLScene;
    camera: TGLCamera;
    camera_cube: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    main_light: TGLLightSource;
    world: TGLDummyCube;
    GLSceneViewer1: TGLSceneViewer;
    objs_cube: TGLDummyCube;
    DT_on_gl: TGLDirectOpenGL;
    GLHUDSprite1: TGLHUDSprite;
    marker: TGLCube;
    map_cube: TGLDummyCube;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure DT_on_glRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
    procedure AddPit(pos: TAffineVector);
  end;

const
  MinusYVector: TVector = (X: 0; Y: - 1; Z: 0; W: 0);

var
  main_form: Tmain_form;

  map, down: TGLFile3DSFreeForm;
  pits, pit_covers: TGLFile3DSFreeForm;
  mx, my: Integer;

implementation

{$R *.DFM}

procedure Tmain_form.AddPit;
var
  i: Integer;
  v, v2: TVector;
  va: TAffineVector;
  f: TGLFile3DSFreeForm;
  mo1, mo2: TMeshObject;
begin
  v := map.AbsoluteToLocal(VectorMake(pos));
  f := TGLFile3DSFreeForm.CreateAsChild(world);
  f.MaterialLibrary := TGLMaterialLibrary.Create(nil);

  f.LoadFromFile('pit.3ds');
  // Подгоняем край ямы (в 3DSMAX у него Z=0) под рельеф карты
  // (хотя по-хорошему его Z нужно находить здесь как наибольшее).
  with f.MeshObjects[0] do
  begin
    Vertices.Translate(AffineVectorMake(v));
    for i := 0 to Vertices.Count - 1 do
    begin
      va := Vertices[i];
      if Abs(va.Z - v.Z) < 0.1 then
        if map.OctreeRayCastIntersect
          (VectorAdd(map.LocalToAbsolute(VectorMake(va)), VectorMake(0, 100, 0)
          ), MinusYVector, @v2) then
          Vertices[i] := AffineVectorMake(va.X, va.Y, map.AbsoluteToLocal(v2)
            .Z + 0.35);
    end;
  end;
  mo1 := TMeshObject.CreateOwned(pits.MeshObjects);
  mo1.Assign(f.MeshObjects[0]);
  pits.StructureChanged;
  pits.BuildOctree(1);

  f.LoadFromFile('cover.3ds');
  // Подгоняем покрытие под рельеф карты. Чем больше
  // неровностей и чем они резче, тем мельче должны быть
  // ячейки в cover.3ds. Иначе сквозь него могут торчать
  // вершины карты (равно как и без небольшого зазора в ~0.3).
  with f.MeshObjects[0] do
  begin
    Vertices.Scale(0.999);
    Vertices.Translate(AffineVectorMake(v));
    for i := 0 to Vertices.Count - 1 do
    begin
      va := Vertices[i];
      if map.OctreeRayCastIntersect(VectorAdd(map.LocalToAbsolute(VectorMake(va)
        ), VectorMake(0, -10, 0)), YHmgVector, @v2) then
        Vertices[i] := AffineVectorMake(va.X, va.Y,
          map.AbsoluteToLocal(v2).Z + 0.3)
      else if map.OctreeRayCastIntersect
        (VectorAdd(map.LocalToAbsolute(VectorMake(va)), VectorMake(0, 100, 0)),
        MinusYVector, @v2) then
        Vertices[i] := AffineVectorMake(va.X, va.Y,
          map.AbsoluteToLocal(v2).Z + 0.3);
    end;
  end;
  mo2 := TMeshObject.CreateOwned(pit_covers.MeshObjects);
  mo2.Assign(f.MeshObjects[0]);
  pit_covers.StructureChanged;

  f.Free;
end;

procedure Tmain_form.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  SetCurrentDir('models');

  // Без этого не будет требуемого эффекта
  GLSceneViewer1.Buffer.DepthTest := false;
  map_cube.ObjectsSorting := osNone;

  map := TGLFile3DSFreeForm.CreateAsChild(map_cube);

  pits := TGLFile3DSFreeForm.CreateAsChild(map_cube);
  pits.MoveFirst;

  pit_covers := TGLFile3DSFreeForm.CreateAsChild(map_cube);
  pit_covers.Material.BlendingMode := bmTransparency;
  pit_covers.Material.FrontProperties.Diffuse.Alpha := 0.01;
  pit_covers.MoveFirst;
  pit_covers.MoveDown;

  down := TGLFile3DSFreeForm.CreateAsChild(map_cube);
  down.MoveFirst;

  {
    Итоговая иерархия объектов:

    DT_on_gl
    objs_cube
    - marker
    map_cube
    - down
    - pits
    - pit_covers
    - map

    Смысл заключается в том, что благодаря всем этим
    манипуляциям через pit_covers просвечивают стенки ямы
    и down (т.к. карта еще не отрисована). Но при этом
    отрисовывается он поверх карты!
    Назначение задника в том, чтобы через щели между
    картой, ямой и покрытием не просвечивал фон вьювера.
    Объекты в иерархии должны быть до карты - тогда они
    будут рисоваться и поверх карты, и поверх ям.

    Конечно, этот способ имеет некоторые погрешности,
    но зато он очень прост и быстро работает.
  }

  for i := 0 to map_cube.Count - 1 do
    with TGLFile3DSFreeForm(map_cube[i]) do
    begin
      Direction.SetVector(0, 1, 0);
      Up.SetVector(0, 0, 1);
      MaterialLibrary := TGLMaterialLibrary.Create(nil);
    end;

  map.LoadFromFile('terrain.3ds');
  map.BuildOctree;
  down.LoadFromFile('down.3ds');
  pits.LoadFromFile('pit.3ds');
  pits.MeshObjects.Clear;
  pits.BuildOctree(1);
  pit_covers.MaterialLibrary.Free;

  // Казалось бы, зачем загружать модель и сразу удалять ее?
  // Но если этого не сделать, то размещенные ямы будут
  // без текстуры!

  ShowCursor(false);
end;

procedure Tmain_form.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  k: single;
begin
  k := deltaTime * 60;
  with camera_cube do
  begin
    if IsKeyDown(ord('W')) then
      Position.AddScaledVector(k, Direction.AsVector);
    if IsKeyDown(ord('S')) then
      Position.AddScaledVector(-k, Direction.AsVector);
    if IsKeyDown(ord('A')) then
      Position.AddScaledVector(-k, camera.AbsoluteLeft);
    if IsKeyDown(ord('D')) then
      Position.AddScaledVector(k, camera.AbsoluteLeft);
    if IsKeyDown(ord('Z')) then
      Position.AddScaledVector(k, Up.AsVector);
    if IsKeyDown(ord('X')) then
      Position.AddScaledVector(-k, Up.AsVector);
  end;
  GLSceneViewer1.Invalidate;
end;

procedure Tmain_form.DT_on_glRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  glEnable(GL_DEPTH_TEST);
end;

procedure Tmain_form.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton(mbRight) then
    AddPit(GLSceneViewer1.Buffer.PixelRayToWorld(X, Y));
  mx := X;
  my := Y;
end;

procedure Tmain_form.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  v, p, n: TVector;
begin
  if ssLeft in Shift then
  begin
    camera_cube.Turn((X - mx) / 2);
    camera.Pitch((my - Y) / 2);
    mx := X;
    my := Y;
  end;

  if ssShift in Shift then
  begin
    v := GLSceneViewer1.Buffer.ScreenToVector(X - 30,
      GLSceneViewer1.Height - Y - 30);
    if not pits.OctreeRayCastIntersect(camera_cube.Position.AsVector, v, @p, @n)
    then
      map.OctreeRayCastIntersect(camera_cube.Position.AsVector, v, @p, @n);
    marker.Position.SetPoint(p);
    marker.Up.SetVector(n);
  end;
  GLHUDSprite1.Position.SetPoint(X, Y, 0);
end;

procedure Tmain_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled := false;
  FreeAndNil(GLSceneViewer1);
end;

end.
