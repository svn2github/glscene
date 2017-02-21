unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.jpeg,
   
  GLScene,
  GLVectorFileObjects,
  GLObjects,
  GLCadencer,
  GLWin32Viewer,
  GLFileSMD,
  GLFileOBJ,
  GLKeyboard,
  GLVectorGeometry,
  GLCrossPlatform,
  GLCoordinates,
  GLBaseClasses,
  GLMaterial,
  GLGeomObjects,
  GLVerletTypes,
  GLVerletClothify,
  GLVerletSkeletonColliders,
  GLVectorLists;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    Actor1: TGLActor;
    Timer1: TTimer;
    GLLightSource1: TGLLightSource;
    GLPlane1: TGLPlane;
    sword: TGLFreeForm;
    Panel1: TPanel;
    colliders_cube: TGLDummyCube;
    sword_cube: TGLDummyCube;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;
  mx, my: Integer;

  // colliders for body parts
  coll: array of TGLSceneObject;
  coll_count: Integer;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i, n: Integer;
begin
  // Загружаем актера
  Actor1.LoadFromFile('trinityRAGE.smd');
  Actor1.AddDataFromFile('walk.smd');
  Actor1.Animations[1].MakeSkeletalTranslationStatic;
  Actor1.SwitchToAnimation('walk');

  // Загружаем меч
  sword.LoadFromFile('sword.obj');
  sword.BuildOctree;

  // Загружаем *описания* коллайдеров. Коллайдеры --
  // это просто шары и капсулы, автоматически выравниваемые
  // с костями актера. Их можно сделать с помощью
  // прилагаемого конструктора
  Actor1.Skeleton.Colliders.LoadFromFile('trinityRAGE.glsc');
  Actor1.Skeleton.Colliders.AlignColliders;

  // Чтобы от описаний был толк, нужно создать сами
  // коллайдеры -- объекты GLScene, с которыми мы и
  // будем определять пересечения

  coll_count := Actor1.Skeleton.Colliders.Count;
  SetLength(coll, coll_count);
  for i := 0 to coll_count - 1 do
  begin
    // Находим кость, соответствующую i-тому коллайдеру
    for n := 0 to Actor1.Skeleton.BoneCount - 1 do
      if Actor1.Skeleton.Colliders[i].Bone.BoneID = n then
        Break;

    if Actor1.Skeleton.Colliders[i].ClassName = 'TSCVerletSphere' then
    begin
      coll[i] := TGLSphere.CreateAsChild(colliders_cube);
      TGLSphere(coll[i]).Radius :=
        TSCVerletSphere(Actor1.Skeleton.Colliders[i]).Radius;
    end
    else if Actor1.Skeleton.Colliders[i].ClassName = 'TSCVerletCapsule' then
    begin
      // Вместо капсул создадим кубы -- поскольку
      // функции определения столкновений с капсулами
      // нет. Результат же будет практически тем же.
      coll[i] := TGLCube.CreateAsChild(colliders_cube);
      TGLCube(coll[i]).CubeWidth := 2 * TSCVerletCapsule
        (Actor1.Skeleton.Colliders[i]).Radius;
      TGLCube(coll[i]).CubeHeight :=
        TSCVerletCapsule(Actor1.Skeleton.Colliders[i]).Length;
      TGLCube(coll[i]).CubeDepth := TGLCube(coll[i]).CubeWidth;
    end;

    // Запоминаем кость этого коллайдера
    coll[i].Tag := n;
    coll[i].Material.FrontProperties.Diffuse.SetColor(1, 0, 0);
    coll[i].Material.FrontProperties.Emission.SetColor(0.3, 0, 0);
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
label 1;
var
  i, a1, a2, a3: Integer;
  p1, p2, p3: TAffineVector;
  m: TMatrix;
  collided: Integer;
  s: string;
begin
  // Синхронизируем объекты-коллайдеры с их описаниями.
  // Сначала умножаем локальную матрицу коллайдера
  // в СО его кости на матрицу самой кости. Получаем
  // матрицу коллайдера в СО актера. Поскольку у нас
  // коллайдеры дочерни к актеру, на этом и остановимся.
  // (СО -- система отсчета)
  for i := 0 to coll_count - 1 do
  begin
    with Actor1.Skeleton do
      m := MatrixMultiply(Colliders[i].LocalMatrix, BoneByID(coll[i].Tag)
        .GlobalMatrix);
    coll[i].Matrix := m;
  end;

  if IsKeyDown('W') then
    sword_cube.Move(10 * deltaTime);
  if IsKeyDown('S') then
    sword_cube.Move(-10 * deltaTime);
  if IsKeyDown('A') then
    sword_cube.Slide(-10 * deltaTime);
  if IsKeyDown('D') then
    sword_cube.Slide(10 * deltaTime);
  if IsKeyDown('Z') then
    sword_cube.Lift(10 * deltaTime);
  if IsKeyDown('X') then
    sword_cube.Lift(-10 * deltaTime);

  if IsKeyDown('I') then
    sword.Pitch(-60 * deltaTime);
  if IsKeyDown('K') then
    sword.Pitch(60 * deltaTime);
  if IsKeyDown('J') then
    sword.Roll(60 * deltaTime);
  if IsKeyDown('L') then
    sword.Roll(-60 * deltaTime);

  // Перебираем коллайдеры и находим первый, с которым пересекается меч
  collided := -1;
  for i := 0 to coll_count - 1 do
    if coll[i] is TGLSphere then
      with TGLSphere(coll[i]) do
        if sword.OctreeSphereSweepIntersect(AbsolutePosition, AbsoluteDirection,
          0.3, Radius) then
        begin
          collided := i;
          Break;
        end
        else
      else if coll[i] is TGLCube then
      begin
        m := coll[i].AbsoluteMatrix;

        for a1 := 0 to sword.MeshObjects.Count - 1 do
          for a2 := 0 to sword.MeshObjects[a1].FaceGroups.Count - 1 do
            with (sword.MeshObjects[a1].FaceGroups[a2] as TFGVertexIndexList) do
              for a3 := 0 to TriangleCount - 1 do
              begin
                // Чтобы не выйти за границы списка треугольников
                if sword.MeshObjects[a1].Vertices.Count < a3 * 3 + 2 then
                  Continue;

                // Получаем абсолютные координаты вершин треугольника
                with sword do
                begin
                  p1 := LocalToAbsolute
                    (MeshObjects[a1].Vertices[VertexIndices[a3 * 3 + 0]]);
                  p2 := LocalToAbsolute
                    (MeshObjects[a1].Vertices[VertexIndices[a3 * 3 + 1]]);
                  p3 := LocalToAbsolute
                    (MeshObjects[a1].Vertices[VertexIndices[a3 * 3 + 2]]);
                end;
                {
                  Теперь применяем следующий трюк. Есть функция
                  IntersectTriangleBox, она принимает координаты
                  вершин треугольника и координаты двух противо-
                  положных вершин куба (т.е. его AABB -- Axis
                  Aligned Bounding Box). Все бы хорошо, но если
                  куб повернут, то он не будет совпадать со своей
                  AABB. Поэтому мы сбрасываем все трансформации
                  куба (в т.ч. и вращение), благодаря чему куб
                  уже будет совпадать со своим AABB. После этого
                  мы должны так же преобразовать треугольник.
                }
                with coll[i] do
                begin
                  AbsoluteMatrix := m;
                  p1 := AbsoluteToLocal(p1);
                  p2 := AbsoluteToLocal(p2);
                  p3 := AbsoluteToLocal(p3);
                  AbsoluteMatrix := IdentityHmgMatrix;

                  if IntersectTriangleBox(p1, p2, p3,
                    AxisAlignedBoundingBoxAbsolute(false, false).min,
                    AxisAlignedBoundingBoxAbsolute(false, false).max) then
                  begin
                    AbsoluteMatrix := m;
                    collided := i;
                    // Нужен выход из нескольких циклов
                    goto 1;
                  end;
                end;
                coll[i].AbsoluteMatrix := m;
              end;
      end;
1:
  if collided <> -1 then
  begin
    s := Actor1.Skeleton.BoneByID(coll[collided].Tag).Name;
    Panel2.Caption := 'Меч касается ' + s;
    // По имени кости узнаем часть тела
    if Pos(' Head ', s) <> 0 then
      Panel2.Caption := Panel2.Caption + '.    Попадание в голову'
    else if Pos('Spine', s) <> 0 then
      Panel2.Caption := Panel2.Caption + '.    Попадание в грудь'
    else if Pos('Pelvis', s) <> 0 then
      Panel2.Caption := Panel2.Caption + '.    Попадание в живот'
    else if Pos(' R ', s) <> 0 then
      if (Pos('arm', s) <> 0) or (Pos('Arm', s) <> 0) then
        Panel2.Caption := Panel2.Caption + '.    Попадание в правую руку'
      else if Pos('Leg', s) <> 0 then
        Panel2.Caption := Panel2.Caption + '.    Попадание в правую ногу'
      else
    else if Pos(' L ', s) <> 0 then
      if (Pos('arm', s) <> 0) or (Pos('Arm', s) <> 0) then
        Panel2.Caption := Panel2.Caption + '.    Попадание в левую руку'
      else if Pos('Leg', s) <> 0 then
        Panel2.Caption := Panel2.Caption + '.    Попадание в левую ногу';
  end
  else
    Panel2.Caption := 'Меч не касается модели';
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  GLCadencer1.Enabled := false;
  GLSceneViewer1.Free;

  for i := 0 to coll_count - 1 do
    coll[i].Free;
  SetLength(coll, 0);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('Определение столкновений с актером -- ' + '%2.1f FPS',
    [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

end.
