unit uMesh;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Dialogs,
  GLObjects,
  GLVectorFileObjects,
  GLScene,
  GLVectorGeometry,
  uClasses;

type
  TPosDirPoint = class
  public
    Name      : AnsiString;
    Position  : TVector;
    Direction : TVector;
    Up        : TVector;
  end;

  TPosDirPointList = class
  public

  end;

  TTypeMesh = (tmStatic, tmDynamic);
  TBaseObject = class(TGLQuadricObject)
  public
    Geom         : TGLCustomSceneObject;
    FName        : AnsiString;
    Action       : AnsiString;
    FileName     : AnsiString;
    TypeMesh     : TTypeMesh;
    MaterialData : TMaterialData;
  end;

  TMesh = class(TGLFreeForm)
  public
    //Geom         : TGLCustomSceneObject;
    FName        : AnsiString;
    Action       : AnsiString;
    FileName     : AnsiString;
    TypeMesh     : TTypeMesh;
    MaterialData : TMaterialData;
  end;

  TAnimatedMesh = class(TBaseObject)
  public
    Geom     : TGLActor;
    constructor Create(AOwner: TComponent); override;
  end;

  TMeshItem = record
    Name      : AnsiString;
    FName     : AnsiString;
    FileName  : AnsiString;
    TypeMesh  : TTypeMesh;
    Animation : Boolean;
  end;

  TMeshList = class
  public
    Items : array of TMeshItem;
    Count : Integer;

    constructor Create;

    function    GetItemByName(iName: AnsiString): TMeshItem;
    function    AddItem(iFileName: AnsiString; iTypeMesh: TTypeMesh; iAnimation: Boolean): TMeshItem; overload;
    function    AddItem: TMeshItem; overload;
    procedure   SaveList(FileName: AnsiString);
    procedure   LoadList(FileName: AnsiString);
    procedure   ClearList;
  end;

implementation

//* TStaticMesh *//


//* TMeshItem *//

function TMeshList.AddItem(iFileName: AnsiString; iTypeMesh: TTypeMesh;
  iAnimation: Boolean) : TMeshItem;
begin
  SetLength(Items,Count+1);

  Items[Count].Name      := 'Mesh_'+IntToStr(Count);
  Items[Count].FName     := Items[Count].Name+ExtractFileExt(iFileName);
  Items[Count].FileName  := iFileName;
  Items[Count].TypeMesh  := iTypeMesh;
  Items[Count].Animation := iAnimation;

  Result:= Items[Count];

  Count:= Count + 1;
end;

function TMeshList.AddItem: TMeshItem;
begin
  SetLength(Items,Count+1);

  Items[Count].Name      := 'Mesh_'+IntToStr(Count);
  Items[Count].FName     := Items[Count].Name;
  Items[Count].FileName  := '';
  Items[Count].TypeMesh  := tmStatic;
  Items[Count].Animation := False;

  Result:= Items[Count];
  Count:= Count + 1;
end;

constructor TMeshList.Create;
begin
  Count:= 0;
end;

function TMeshList.GetItemByName(iName: AnsiString): TMeshItem;
var
  i: Integer;
begin
  if Count <> 0 then
  for i:= 0 to Count -1 do
    if Items[i].Name = iName then
    begin
      Result:= Items[i];
      Exit;
    end;
end;

procedure TMeshList.SaveList(FileName: AnsiString);
var
  i: Integer;
  F: TextFile;
begin
  AssignFile(F,FileName);
  Rewrite(F);

  for i:= 0 to Count - 1 do
  begin
    WriteLn(F,Items[i].Name);
    WriteLn(F,Items[i].FName);

    case Items[i].TypeMesh of
      tmStatic  : WriteLn(F,'0');
      tmDynamic : WriteLn(F,'1');
    end;

    WriteLn(F,Items[i].Animation);
  end;

  CloseFile(F);
end;

procedure TMeshList.LoadList(FileName: AnsiString);
var
  F   : TextFile;
  i   : Integer;
  Dir : AnsiString;
  Tag : AnsiString;
  MeshItem: TMeshItem;
begin
  ClearList;
  Dir:= ExtractFileDir(FileName)+'\';
  
  AssignFile(F,FileName);
  Reset(F);

  while not EOF(F) do
  begin
    MeshItem:= AddItem;

    with MeshItem do
    begin
      ReadLn(F,Name);
      ReadLn(F,FName);
      ReadLn(F,i);

      case i of
        0 : TypeMesh:= tmStatic;
        1 : TypeMesh:= tmDynamic;
      end;

      ReadLn(F,Tag);

      if Tag = 'TRUE' then Animation:= True
      else Animation:= False;

      FileName:= Dir+FName;
    end;

    Items[Count-1]:= MeshItem;
  end;

  CloseFile(F);
end;

procedure TMeshList.ClearList;
begin
  SetLength(Items,0);
  Count:= 0;
end;

//* TAnimatedMesh *//

constructor TAnimatedMesh.Create(AOwner: TComponent);
begin
  inherited;
  Geom:= TGLActor(AddNewChild(TGLActor));
end;

end. 
