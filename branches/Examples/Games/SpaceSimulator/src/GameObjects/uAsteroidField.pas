{
  Забить на продвинутую версию, остановиться на простой генерации по эллипсам

  Предоставляет возможность создавать поля и пояса астероидов в определнном месте
  с определенной формой и некоторой плотностью в пространстве

  Большинство параметров редактируется из файла

  Форма задается с помощью кривой следования пояса и радиусов сечения пояса
  (для задания эллипса)
}

unit uAsteroidField;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.IniFiles,
  System.Classes,

  GLScene,
  GLObjects,
  GLVectorGeometry,
  GLVectorTypes,
  GLVectorLists,
  GLVectorFileObjects,
  GLFile3DS,
  GLFilePNG,
  GLMaterial,
  GLTexture,
  GLRenderContextInfo,

  uLog,
  uDebugInfo,
  uSimplePhysics,

  dfKeyboardExt;

const
  C_ASTEROIDS_PATH = 'data\asteroids\';
  C_ASTEROIDS_DISTANCE_TO_ROTATE = 600;
  C_DIST2 = C_ASTEROIDS_DISTANCE_TO_ROTATE * C_ASTEROIDS_DISTANCE_TO_ROTATE;
  C_SECTION_MAIN = 'Main';
  C_SECTION_NODES = 'Node';
  C_SECTION_ASTER = 'Asteroids';

  // C_GENDATA_SUFFIX = '_gen';
  C_GENDATA_EXT = '.gen';

type

  TdfAsteroidNode = record
    Dummy: TGLDummyCube;
    Radius: TAffineVector; // Радиусы по осям
    Count: Integer; // Число астероидов внутри
    AsterPositions: TAffineVectorList;
    vScale, vSpeed: TVector2f;
  end;

  TdfAsteroidField = class(TGLDummyCube)
  private
    FMatLib: TGLMaterialLibrary;
    FMaster: TGLFreeForm;
    FNodes: array of TdfAsteroidNode;
    // FIndAsteroidCount: Integer;
    procedure SetPhysicsForAsteroids;
    function GetRandomInSphere(aRad: TAffineVector): TAffineVector;
    procedure SetMasterAsteroid(aMesh: TGLFreeForm;
      aMeshPath, aTexturePath, aLibMaterial: String);
    procedure GenerateAsteroids();
    procedure LoadGenDataFromFile(aFile: String);
    procedure SaveGenDataToFile(aFile: String);
  public
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;
    destructor Destroy; override;
    procedure LoadFromFile(aFile: String; SaveGenerated: Boolean = True);
    procedure SetMatLib(aMatLib: TGLMaterialLibrary);
    procedure UpdateField(deltaTime: Double);
  end;

  // ====================================================================
implementation

// ====================================================================

uses
  uGameObjects;

{ TdfAsteroidField }

constructor TdfAsteroidField.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  inherited;
  SetLength(FNodes, 0);
  VisibilityCulling := vcObjectBased;
end;

destructor TdfAsteroidField.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FNodes) - 1 do
    FNodes[i].AsterPositions.Free;
  SetLength(FNodes, 0);
  inherited;
end;

procedure TdfAsteroidField.GenerateAsteroids;
var
  i, j: Integer;
  aAsterPos: TAffineVector;
begin
  Randomize;
  FMaster := TGLFreeForm.CreateAsChild(Self);
  SetMasterAsteroid(FMaster, 'a1.3ds', 'a1_fm1.png', 'asteroid1');
  for i := 0 to Length(FNodes) - 1 do
    with FNodes[i] do
    begin
      for j := 0 to FNodes[i].Count - 1 do
      begin
        aAsterPos := GetRandomInSphere(Radius);
        AsterPositions.Add(aAsterPos);
        with TGLProxyObject.CreateAsChild(Dummy) do
        begin
          MasterObject := FMaster;
          ProxyOptions := [pooObjects];
          Position.AsAffineVector := aAsterPos;
          RandomPointOnSphere(aAsterPos);
          Up.AsAffineVector := aAsterPos;
          Scale.Scale(vScale.X + (vScale.Y - vScale.X) * Random(100) / 100);
          TagFloat := vSpeed.X + (vSpeed.Y - vSpeed.X) * Random(100) / 100;
        end;
      end;
    end;
  SetPhysicsForAsteroids();
end;

function TdfAsteroidField.GetRandomInSphere(aRad: TAffineVector): TAffineVector;
var
  i: Integer;
begin
  for i := 0 to 2 do
    Result.V[i] := aRad.V[i] - Random(Round(aRad.V[i] * 200)) / 100;
end;

procedure TdfAsteroidField.LoadFromFile(aFile: String;
  SaveGenerated: Boolean = True);
var
  Ini: TIniFile;
  genFile: String;
  NodeCount: Integer;
  i: Integer;
  nodeVec: TAffineVector;

  // iAsters: Integer;
begin
  if FileExists(C_ASTEROIDS_PATH + aFile) then
  begin
    Ini := TIniFile.Create(C_ASTEROIDS_PATH + aFile);

    genFile := C_ASTEROIDS_PATH + aFile + C_GENDATA_EXT;
    // iAsters := 0;

    // Читаем основные параметры
    NodeCount := Ini.ReadInteger(C_SECTION_MAIN, 'NodeCount', 1);

    // Читаем ноды
    SetLength(FNodes, NodeCount);
    for i := 0 to NodeCount - 1 do
      with FNodes[i] do
      begin
        Dummy := TGLDummyCube.CreateAsChild(Self);
        // Dummy.VisibleAtRunTime := True;
        Dummy.CubeSize := 30;
        // Читаем позицию нода
        nodeVec.X := Ini.ReadFloat(C_SECTION_NODES + IntToStr(i), 'PosX', 0.0);
        nodeVec.Y := Ini.ReadFloat(C_SECTION_NODES + IntToStr(i), 'PosY', 0.0);
        nodeVec.Z := Ini.ReadFloat(C_SECTION_NODES + IntToStr(i), 'PosZ', 0.0);
        Dummy.Position.AsAffineVector := nodeVec;

        // Читаем радиусы зоны нода
        nodeVec.X := Ini.ReadFloat(C_SECTION_NODES + IntToStr(i), 'RadX', 0.0);
        nodeVec.Y := Ini.ReadFloat(C_SECTION_NODES + IntToStr(i), 'RadY', 0.0);
        nodeVec.Z := Ini.ReadFloat(C_SECTION_NODES + IntToStr(i), 'RadZ', 0.0);
        Radius := nodeVec;

        // Читаем число астероидов в ноде
        Count := Ini.ReadInteger(C_SECTION_NODES + IntToStr(i),
          'AsterCount', 0);
        AsterPositions := TAffineVectorList.Create;
        AsterPositions.Capacity := FNodes[i].Count;
        // iAsters := iAsters + FNodes[i].Count;

        // Читаем диапазон масштаба астероидов в ноде
        vScale.X := Ini.ReadFloat(C_SECTION_NODES + IntToStr(i),
          'ScaleMin', 1.0);
        vScale.Y := Ini.ReadFloat(C_SECTION_NODES + IntToStr(i),
          'ScaleMax', 1.0);

        // Читаем диапазон скоростей
        vSpeed.X := Ini.ReadFloat(C_SECTION_NODES + IntToStr(i),
          'SpeedMin', 1.0);
        vSpeed.Y := Ini.ReadFloat(C_SECTION_NODES + IntToStr(i),
          'SpeedMax', 1.0);
      end;

    // FIndAsteroidCount := dfDebugInfo.AddNewString('Число астероидов');
    // dfDebugInfo.UpdateParam(FIndAsteroidCount, iAsters);
    // dfDebugInfo.ShowString(FIndAsteroidCount);

    if Ini.ReadBool(C_SECTION_MAIN, 'ShouldGenerate', True) then
    begin
      // Генерируем астероиды
      GenerateAsteroids();
      if SaveGenerated then
      begin
        // Пишем в файл, что при следующей загрузке генерировать не надо
        // - все уже есть в сопутствующем gen-файле
        Ini.WriteBool(C_SECTION_MAIN, 'ShouldGenerate', False);
        // Записываем их в файл
        SaveGenDataToFile(genFile);
      end;
    end
    else
    begin
      if FileExists(genFile) then
        // Загружаем их данные из файла
        LoadGenDataFromFile(genFile)
      else
      begin
        logWriteError('TdfAsteroidField: Gendata file ' + genFile +
          ' not found!');
        GenerateAsteroids();
      end;
    end;
    logWriteMessage('TdfAsteroidField: Loading data from ' + aFile +
      ' completed!');
    Ini.Free;
  end
  else
    logWriteError('TdfAsteroidField: File ' + C_ASTEROIDS_PATH + aFile +
      ' not found!');
end;

procedure TdfAsteroidField.LoadGenDataFromFile(aFile: String);
var
  fs: TFileStream;
  i, j: Integer;
  vec: TAffineVector;
  tagf: Single;
begin
  fs := TFileStream.Create(aFile, fmOpenRead);
  fs.Seek(0, 0);
  vec := NullVector;
  tagf := 0;
  FMaster := TGLFreeForm.CreateAsChild(Self);
  SetMasterAsteroid(FMaster, 'a1.3ds', 'a1_fm1.png', 'asteroid1');

  for i := 0 to Length(FNodes) - 1 do
  begin
    fs.Read(FNodes[i].Count, SizeOf(FNodes[i].Count));
    for j := 0 to FNodes[i].Count - 1 do
      with TGLProxyObject.CreateAsChild(FNodes[i].Dummy) do
      begin
        MasterObject := FMaster;
        ProxyOptions := [pooObjects];
        fs.ReadBuffer(vec, SizeOf(TAffineVector));
        Position.AsAffineVector := vec;
        fs.ReadBuffer(vec, SizeOf(TAffineVector));
        Up.AsAffineVector := vec;
        fs.ReadBuffer(vec, SizeOf(TAffineVector));
        Scale.AsAffineVector := vec;
        fs.ReadBuffer(tagf, SizeOf(TagFloat));
        TagFloat := tagf;
      end;
  end;

  SetPhysicsForAsteroids();

  fs.Free;
end;

procedure TdfAsteroidField.SaveGenDataToFile(aFile: String);
var
  fs: TFileStream;
  i, j: Integer;
  vec: TAffineVector;
begin
  if FileExists(aFile) then
    fs := TFileStream.Create(aFile, fmOpenWrite)
  else
    fs := TFileStream.Create(aFile, fmCreate);

  fs.Seek(0, 0);

  for i := 0 to Length(FNodes) - 1 do
  begin
    fs.Write(FNodes[i].Count, SizeOf(FNodes[i].Count));
    for j := 0 to FNodes[i].Count - 1 do
      with FNodes[i].Dummy[j] do
      begin
        vec := Position.AsAffineVector;
        fs.WriteBuffer(vec, SizeOf(Single) * 3);
        vec := Up.AsAffineVector;
        fs.WriteBuffer(vec, SizeOf(TAffineVector));
        vec := Scale.AsAffineVector;
        fs.WriteBuffer(vec, SizeOf(TAffineVector));
        fs.WriteBuffer(TagFloat, SizeOf(TagFloat));
      end;
  end;
  fs.Free;
end;

procedure TdfAsteroidField.SetMasterAsteroid(aMesh: TGLFreeForm;
  aMeshPath, aTexturePath, aLibMaterial: String);
begin
  // Добавляем материал, если его нет, иначе пропускаем
  if not Assigned(FMatLib.LibMaterialByName(aLibMaterial)) then
    with FMatLib.Materials.Add do
    begin
      Name := aLibMaterial;
      with Material do
      begin
        Texture.Image.LoadFromFile(C_ASTEROIDS_PATH + aTexturePath);
        Texture.TextureMode := tmModulate;
        Texture.Enabled := True;
        FrontProperties.Diffuse.SetColor(1, 1, 1);
        BlendingMode := bmOpaque;
      end;
    end;

  with aMesh do
  begin
    Material.MaterialLibrary := FMatLib;
    Material.LibMaterialName := aLibMaterial;
    LoadFromFile(C_ASTEROIDS_PATH + aMeshPath);
    UseMeshMaterials := False;

    Visible := False;
  end;
end;

procedure TdfAsteroidField.SetMatLib(aMatLib: TGLMaterialLibrary);
begin
  FMatLib := aMatLib;
end;

procedure TdfAsteroidField.SetPhysicsForAsteroids;
var
  i, j, Group: Integer;
begin
  Group := GetFreeUserGroup();
  for i := 0 to Length(FNodes) - 1 do
    with FNodes[i] do
      for j := 0 to Dummy.Count - 1 do
      begin
        with dfPhysics.AddPhysToObject(Dummy[j], psSphere) do
        begin
          IsStatic := True;
          UserType := C_PHYS_ASTEROID;
          UserGroup := Group;
        end;
      end;
end;

procedure TdfAsteroidField.UpdateField(deltaTime: Double);
var
  i, j: Integer;
begin
  for i := 0 to Length(FNodes) - 1 do
    for j := 0 to FNodes[i].Count - 1 do
      with FNodes[i].Dummy[j] do
        if VectorDistance2(dfGameObjects.Player.AbsolutePosition,
          AbsolutePosition) < C_DIST2 then
          RotateAbsolute(Up.AsAffineVector, deltaTime * TagFloat);
end;

end.
