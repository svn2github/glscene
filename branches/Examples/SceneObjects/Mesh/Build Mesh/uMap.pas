unit uMap;

interface

uses
  Windows,
  SysUtils,
  Dialogs,
  Classes,
  GLMaterial,
  GLScene,
  GLColor,
  GLVectorFileObjects,
  GLVectorGeometry,

  uClasses,
  uMesh,
  uMaterials,
  uUtils;

type
  TMap = class (TMesh)
  public
    Title    : AnsiString;
    PathFile : AnsiString;

    //
    MeshList     : TMeshList;
    ObjList      : TList;
    GLScene      : TGLScene;
    c : Integer;

    procedure Load (FileName: AnsiString);
    procedure Build;
    procedure Clear;
    function  AddMesh(Name,FileName: AnsiString): TMesh;
    procedure Init;
  end;

implementation

//* TMap *//

procedure TMap.Init;
begin
  ObjList  := TList.Create;
  MeshList := TMeshList.Create;
end;

procedure TMap.Load(FileName: AnsiString);
var
  F       : TextFile;
  N       : Single;
  S1,S2   : AnsiString;
  TmpList : TStringList;
  S3,Dir  : AnsiString;
  Mesh    : TMesh;
  I       : Integer;
begin
  Clear;

  c := 0;
  Title := ChangeFileExt(ExtractFileName(FileName),'');
  Dir   := ExtractFileDir(FileName)+'\'+Title+'_dat\';

  MeshList.LoadList(Dir+'Meshes.info');

  AssignFile(F,FileName);
  Reset(F);

  ReadLn(F,N); //Camera.Position.X := N;
  ReadLn(F,N); //Camera.Position.Y := N;
  ReadLn(F,N); //Camera.Position.Z := N;

  ReadLn(F,N); //Target.Position.X := N;
  ReadLn(F,N); //Target.Position.Y := N;
  ReadLn(F,N); //Target.Position.Z := N;

  while not EOF(F) do
  begin
    ReadLn(F,S1);
    ReadLn(F,S2);
    ReadLn(F,S3);

    if S1 = 'Mesh:' then Mesh:= AddMesh(S2,S3);

    ReadLn(F,Mesh.Action);

    ReadLn(F,N); Mesh.Position.X:= N;
    ReadLn(F,N); Mesh.Position.Y:= N;
    ReadLn(F,N); Mesh.Position.Z:= N;

    ReadLn(F,N); Mesh.Scale.X:= N;
    ReadLn(F,N); Mesh.Scale.Y:= N;
    ReadLn(F,N); Mesh.Scale.Z:= N;

    ReadLn(F,N); Mesh.PitchAngle:= N;
    ReadLn(F,N); Mesh.TurnAngle := N;
    ReadLn(F,N); Mesh.RollAngle := N;

    ReadLn(F);
    ReadLn(F);
    ReadLn(F);
    ReadLn(F,S1);

    Mesh.MaterialData.Texture.Name:= S1;
    Mesh.MaterialData.Texture.FileName:= Dir+S1;

    ReadLn(F,S1);

    if S1 = 'TRUE' then Mesh.MaterialData.Texture.Enable:= True
    else Mesh.MaterialData.Texture.Enable:= False;

    ReadLn(F,N); Mesh.MaterialData.Texture.Scale.X := N;
    ReadLn(F,N); Mesh.MaterialData.Texture.Scale.Y := N;
    ReadLn(F,N); Mesh.MaterialData.Texture.Scale.Z := N;

    ReadLn(F,N); Mesh.MaterialData.Texture.Offset.X:= N;
    ReadLn(F,N); Mesh.MaterialData.Texture.Offset.Y:= N;
    ReadLn(F,N); Mesh.MaterialData.Texture.Offset.Z:= N;

    ReadLn(F,I);

    case I of
      0 : Mesh.MaterialData.Effect := effNone;
      1 : Mesh.MaterialData.Effect := effReflect;
      2 : Mesh.MaterialData.Effect := effReflect2;
      3 : Mesh.MaterialData.Effect := effAlphaChanelPNG;
    end;

    ReadLn(F,I); Mesh.MaterialData.Color.AsWinColor:= I;
    ReadLn(F,N); Mesh.MaterialData.Color.Alpha:= N;

    ApplyMaterialData(MaterialLibrary,Mesh);
  end;

  CloseFile(F);
end;

procedure TMap.Clear;
var
  i: Integer;
  Mesh: TMesh;
begin
  for i:= 0 to ObjList.Count - 1 do
  begin
    Mesh:= ObjList[i];
    FreeAndNil(Mesh);
  end;

  MeshList.ClearList;
  ObjList.Clear;
end;

function TMap.AddMesh(Name, FileName: AnsiString): TMesh;
var
  i,n      : Integer;
  Mesh     : TMesh;
  MeshItem : TMeshItem;
begin
  Result:= nil;
  MeshItem:= MeshList.GetItemByName(FileName);

  Mesh:= TMesh(GLScene.Objects.AddNewChild(TMesh));

  Mesh.FName    := MeshItem.Name;
  Mesh.FileName := MeshItem.FileName;
  Mesh.TypeMesh := MeshItem.TypeMesh;

  Mesh.Scale.SetVector(10,10,10);

  //if not MeshItem.Animation then
    //for i:= 0 to (Mesh as TMesh).Geom.MeshObjects.Count - 1 do
    //  (Mesh as TMesh).Geom.MeshObjects[i].UseVBO:= False;

  //if not MeshItem.Animation then
  (Mesh as TMesh).LoadFromFile(Mesh.FileName);
  //else (Mesh as TAnimatedMesh).Geom.LoadFromFile(Mesh.FileName);

  Mesh.Name:= Name;

  MaterialLibrary.Materials.Add.Name:= 'Material'+IntToStr(MaterialLibrary.Materials.Count);
  n:= MaterialLibrary.Materials.Count - 1;

  Mesh.MaterialData.Name  := MaterialLibrary.Materials[n].Name;
  Mesh.MaterialData.Color := TGLColor.Create(nil);

  //if not MeshItem.Animation then
  //begin
  Mesh.Material.MaterialLibrary := MaterialLibrary;
  Mesh.Material.LibMaterialName := Mesh.MaterialData.Name;
  Mesh.MeshObjects.UseVBO:= False;
  Mesh.BuildOctree(3);
  //Mesh.ObjectStyle:= [osDirectDraw];
  //end else
  //begin
  //  (Mesh as TAnimatedMesh).Geom.Material.MaterialLibrary := MaterialLibrary;
  //  (Mesh as TAnimatedMesh).Geom.Material.LibMaterialName := Mesh.MaterialData.Name;
  //end;

  ResetMaterialData (Mesh.MaterialData);
  ApplyMaterialData (MaterialLibrary,Mesh);

  ObjList.Add(Mesh);

  Result:= Mesh;
end;

procedure TMap.Build;
var
  i   : Integer;
  Obj : TMesh;
  mR  : TMatrix;
begin
  MeshObjects.UseVBO:= False;
  
  for i := 0 to ObjList.Count-1 do
  begin
    Obj := ObjList[i];
    CopyMeshObj (Obj,Self,Obj.Matrix);
    FreeAndNil  (Obj)
  end;

  BuildOctree(3);
end;

end. 
