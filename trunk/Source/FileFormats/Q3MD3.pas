{ 
  Q3MD3 - Quake3 MD3 vector file format loading classes
  
  By Stuart Gooding
}

unit Q3MD3;

interface

uses
  Classes,SysUtils,ApplicationFileIO,
  Geometry,VectorTypes,GLVectorFileObjects,GLScene,GLObjects,
  GLTexture,GLMisc,VectorLists;

type
  // Quake3 MD3 structure types

  TMD3Tag = record
    strName   : array[0..63] of char;
    vPosition : TVector3f;
    rotation  : TMatrix3f;
  end;

  // I've seen this part of the MD3 structure called 2 things:
  // A frame and a bone. It doesn't matter because we don't use it
  {TMD3Frame = record
    min_bound,max_bounds,
    local_origin  : TVector3f;
    radius        : single;
    name          : array[0..15] of char;
  end;}
  TMD3Bone = record
    mins,maxs,
    position  : TVector3f;
    scale     : single;
    creator   : array[0..15] of char;
  end;

  TMD3Triangle = record
    vertex : array[0..2] of SmallInt; // value/64 to get real number position
    normal : array[0..1] of Byte;     // Latitude,Longitude
  end;

  TMD3Face = record
    vertexIndices : array[0..2] of integer;
  end;

  TMD3TexCoord = record
    textureCoord : array[0..1] of single;
  end;

  TMD3Skin = record
    strName : array[0..67] of char;
  end;

  TMD3Header = record
    fileID     : array[0..3] of char;
    version    : integer;
    strFile    : array[0..67] of char;
    numFrames,
    numTags,
    numMeshes,
    numMaxSkins,
    headerSize,
    tagStart,
    tagEnd,
    fileSize   : integer;
  end;

  TMD3MeshHeader = record
    meshID        : array[0..3] of char;
    strName       : array[0..67] of char;
    numMeshFrames,
    numSkins,
    numVertices,
    numTriangles,
    triStart,
    headerSize,
    uvStart,
    vertexStart,
    meshSize      : integer;
  end;

  TMD3MeshData = record
    MeshHeader : TMD3MeshHeader;
    Skins      : array of TMD3Skin;
    Triangles  : array of TMD3Face;
    TexCoords  : array of TMD3TexCoord;
    Vertices   : array of TMD3Triangle;
  end;

  // MD3 Main file class

  TFileMD3 = class
    public
      ModelHeader: TMD3Header;
      Bones    : array of TMD3Bone;
      Tags     : array of TMD3Tag;
      MeshData : array of TMD3MeshData;

      procedure LoadFromStream(aStream : TStream);
  end;

  // Vector file format class
  // This needs to be registered before loading models and
  // unregistered when finished loading models
  TGLMD3VectorFile = class (TVectorFile)
    public
      procedure LoadFromStream(aStream : TStream); override;
  end;

  // This class is used to extract the tag transform information
  // stroed in the MD3 files. The data is used to offset each
  // part of the model based on the parent parts animation state.
  TMD3TagList = class
    private
      FTags : array of TMD3Tag;
      FNumTags,
      FNumFrames : Integer;
    function GetTag(index: integer): TMD3Tag;
    public
      procedure LoadFromFile(FileName:String);
      procedure LoadFromStream(AStream:TStream);
      function GetTransform(TagName:string; Frame:integer):TMatrix;
      property TagCount : integer read FNumTags;
      property FrameCount : integer read FNumFrames;
      property Tags[index:integer]:TMD3Tag read GetTag;
  end;

// These procedures are helpers to load the Quake3 animation file data
// into an animation list. The NamePrefix parameter is used to determine
// which class of animaiton extracted. eg NamePrefix='TORSO' will load
// all animations starting with 'TORSO_' like 'TORSO_STAND'
procedure LoadQ3Anims(Animations:TActorAnimations;
            FileName:string; NamePrefix:string); overload;
procedure LoadQ3Anims(Animations:TActorAnimations;
            Strings:TStrings; NamePrefix:string); overload;

implementation

procedure LoadQ3Anims(Animations:TActorAnimations;
            FileName:string; NamePrefix:string);
var
  AnimStrings:TStrings;
begin
  AnimStrings:=TStringList.Create;
  AnimStrings.LoadFromFile(FileName);
  LoadQ3Anims(Animations,AnimStrings,NamePrefix);
  AnimStrings.Free;
end;

procedure LoadQ3Anims(Animations:TActorAnimations;
            Strings:TStrings; NamePrefix:string);
var
  anim :TStringList;
  val : array[0..3] of integer;
  strindex,valindex,i : integer;
  GotValues:Boolean;
  commatext,str1 : string;

  function StrIsNumber(str:string):boolean;
  var
    i : integer;
  begin
    result:=false;
    for i:=1 to Length(str) do
      if (Ord(str[i])>=Ord('0'))
      and (Ord(str[i])<=Ord('9')) then
        result:=true
      else begin
        result:=false;
        break;
      end;
  end;

begin
  anim:=TStringList.Create;

  for strindex:=0 to Strings.Count-1 do begin
    commatext:=Strings.Strings[strindex];
    while Pos('  ',commatext)>0 do
      commatext:=StringReplace(commatext,'  ',' ',[rfReplaceAll]);
    commatext:=StringReplace(commatext,' ',',',[rfReplaceAll]);
    anim.CommaText:=commatext;
    GotValues:=False;
    valindex:=0;
    str1:='';
    if anim.Count>=5 then begin
      for i:=0 to Anim.Count-1 do begin
        if GotValues then begin
          if (Anim.Strings[i]<>'//')
          and (Pos(NamePrefix+'_',Anim.Strings[i])>0) then begin
            str1:=StringReplace(Anim.Strings[i],'//','',[rfReplaceAll]);
            break;
          end;
        end else begin
          if StrIsNumber(Anim.Strings[i]) then begin
            val[valindex]:=StrToInt(Anim.Strings[i]);
            Inc(valindex);
            if valindex=4 then GotValues:=True;
          end else break;
        end;
      end;
    end;
    if GotValues and (str1<>'') then begin
      // Values ready for new animation.
      with Animations.Add do begin
        Name:=str1;
        StartFrame:=val[0];
        EndFrame:=val[0]+val[1]-1;
        Reference:=aarMorph;
        // Need a way in TActorAnimation to tell whether it is
        // a looping type animation or a play once type and
        // the framerate (interval) it uses. Both of these can
        // be determined here and loaded.
      end;
    end;
  end;
  anim.Free;
end;

// ------------------
// ------------------ TFileMD3 ------------------
// ------------------

// LoadFromStream
//
procedure TFileMD3.LoadFromStream(aStream : TStream);
var
  i : Integer;
  meshOffset : LongInt;
begin
  aStream.Read(ModelHeader, sizeof(ModelHeader));

  // Test for correct file ID and version
  if (ModelHeader.fileID<>'IDP3') or (ModelHeader.version<>15) then begin
    //exit;
  end;

  // Read in the bones
  SetLength(Bones,ModelHeader.numFrames);
  aStream.Read(Bones[0],sizeof(TMD3Bone)*ModelHeader.numFrames);

  // Read in the Tags
  SetLength(Tags,ModelHeader.numFrames*ModelHeader.numTags);
  aStream.Read(Tags[0],sizeof(TMD3Tag)*ModelHeader.numFrames*ModelHeader.numTags);

  // Read in the Mesh data
  meshOffset:=aStream.Position;
  SetLength(MeshData,ModelHeader.numMeshes);
  for i:=0 to ModelHeader.numMeshes-1 do begin
    with MeshData[i] do begin
      aStream.Position:=meshOffset;
      aStream.Read(MeshHeader,sizeof(MeshHeader));
      // Set up the dynamic arrays
      SetLength(Skins,MeshHeader.numSkins);
      SetLength(Triangles,MeshHeader.numTriangles);
      SetLength(TexCoords,MeshHeader.numVertices);
      SetLength(Vertices,MeshHeader.numVertices*MeshHeader.numMeshFrames);
      // Skins
      aStream.read(Skins[0],sizeof(TMD3Skin)*MeshHeader.numSkins);
      // Face data
      aStream.Position:=meshOffset+MeshHeader.triStart;
      aStream.read(Triangles[0],sizeof(TMD3Face)*MeshHeader.numTriangles);
      // Texture coordinates
      aStream.Position:=meshOffset+meshHeader.uvStart;
      aStream.read(TexCoords[0],sizeof(TMD3TexCoord)*meshHeader.numVertices);
      // Vertices
      aStream.Position:=meshOffset+meshHeader.vertexStart;
      aStream.read(Vertices[0],sizeof(TMD3Triangle)*MeshHeader.numMeshFrames*MeshHeader.numVertices);
      // Increase the offset
      meshOffset:=meshOffset+MeshHeader.meshSize;
    end;
  end;
end;


// ------------------
// ------------------ TGLMD3VectorFile ------------------
// ------------------

// LoadFromStream
//
procedure TGLMD3VectorFile.LoadFromStream(aStream : TStream);
var
  i,j,k,
  numVerts,
  numtris     : Integer;
  MD3File     : TFileMD3;
  mesh        : TMorphableMeshObject;
  faceGroup   : TFGIndexTexCoordList;
  morphTarget : TMeshMorphTarget;
  str         : string;

  function GetNormalFromMD3Normal(n : array of Byte) : TAffineVector;
  var
    lat,lng : single;
  begin
    // The MD3 normal is a latitude/longitude value that needs
    // to be calculated into cartesian space.
    lat:=(n[0])*(2*pi)/255; lng:=(n[1])*(2*pi)/255;
    result[0]:=cos(lat)*sin(lng);
    result[1]:=sin(lat)*sin(lng);
    result[2]:=cos(lng);
  end;

  procedure AllocateMaterial(meshname,skins:string);
  var
    LibMat : TGLLibMaterial;
  begin
    // If a material library is assigned to the actor/freeform the
    // mesh name will be added as a material.
    if Assigned(Owner.MaterialLibrary) then with Owner.MaterialLibrary do begin
      if Assigned(Materials.GetLibMaterialByName(meshname)) then exit;
      LibMat:=Materials.Add;
      LibMat.name:=meshname;
      LibMat.Material.Texture.Disabled:=False;
    end;
  end;

begin
  MD3File:=TFileMD3.Create;
  MD3File.LoadFromStream(aStream);
  try
    for i:=0 to MD3File.ModelHeader.numMeshes-1 do begin
      mesh:=TMorphableMeshObject.CreateOwned(Owner.MeshObjects);
      mesh.Name:=trim(MD3File.MeshData[i].MeshHeader.strName);
      with mesh, MD3File do begin
        Mode:=momFaceGroups;
        faceGroup:=TFGIndexTexCoordList.CreateOwned(FaceGroups);
        with faceGroup do begin
          str:='';
          for j:=0 to MeshData[i].MeshHeader.numSkins-1 do begin
            if j>0 then str:=str+',';
            str:=lowercase(trim(MeshData[i].Skins[j].strName));
          end;
          AllocateMaterial(mesh.Name,str);
          MaterialName:=mesh.Name;
          numTris:=MeshData[i].MeshHeader.numTriangles;
          VertexIndices.Capacity:=numTris*3;
          TexCoords.Capacity:=numTris*3;
          // Get the vertex indices and texture coordinates
          for j:=0 to MeshData[i].MeshHeader.numTriangles-1 do begin
            with MeshData[i].Triangles[j] do begin
              Add(vertexIndices[0],
                  MeshData[i].TexCoords[vertexIndices[0]].textureCoord[0],
                  -MeshData[i].TexCoords[vertexIndices[0]].textureCoord[1]);
              Add(vertexIndices[2],
                  MeshData[i].TexCoords[vertexIndices[2]].textureCoord[0],
                  -MeshData[i].TexCoords[vertexIndices[2]].textureCoord[1]);
              Add(vertexIndices[1],
                  MeshData[i].TexCoords[vertexIndices[1]].textureCoord[0],
                  -MeshData[i].TexCoords[vertexIndices[1]].textureCoord[1]);
            end;
          end;
        end;

        // Get the mesh data for each morph frame
        for j:=0 to ModelHeader.numFrames-1 do begin
          morphTarget:=TMeshMorphTarget.CreateOwned(MorphTargets);
          morphTarget.Name:=Trim(MeshData[i].MeshHeader.strName)+'['+IntToStr(j)+']';
          numVerts:=MeshData[i].MeshHeader.numVertices;
          morphTarget.Vertices.Capacity:=numVerts;
          for k:=numVerts*j to numVerts*(j+1)-1 do begin
            morphTarget.Vertices.Add(
              MeshData[i].Vertices[k].Vertex[0]/64,
              MeshData[i].Vertices[k].Vertex[1]/64,
              MeshData[i].Vertices[k].Vertex[2]/64);
            morphTarget.Normals.Add(
              GetNormalFromMD3Normal(MeshData[i].Vertices[k].normal));
          end;
        end;
      end;
      if mesh.MorphTargets.Count>0 then
        mesh.MorphTo(0);
    end;
  finally
    MD3File.Free;
  end;
end;

// ------------------
// ------------------ TMD3TagList ------------------
// ------------------

// LoadFromFile
//
procedure TMD3TagList.LoadFromFile(FileName:String);
var
  fs : TStream;
begin
  if fileName<>'' then begin
    fs:=CreateFileStream(FileName, fmOpenRead+fmShareDenyWrite);
    try
      LoadFromStream(fs);
    finally
      fs.Free;
    end;
  end;
end;

// LoadFromStream
//
procedure TMD3TagList.LoadFromStream(aStream:TStream);
var
  MD3File     : TFileMD3;
begin
  MD3File:=TFileMD3.Create;
  try
    MD3File.LoadFromStream(aStream);
    FNumTags:=MD3File.ModelHeader.numTags;
    FNumFrames:=MD3File.ModelHeader.numFrames;
    SetLength(FTags,FNumTags*FNumFrames);
    System.Move(MD3File.Tags[0],FTags[0],FNumTags*FNumFrames*SizeOf(TMD3Tag));
  finally
    MD3File.Free;
  end;
end;

// GetTag
//
function TMD3TagList.GetTag(index: integer): TMD3Tag;
begin
  Result:=FTags[index];
end;

// GetTransform
//
function TMD3TagList.GetTransform(TagName: string;
  Frame: integer): TMatrix;
var
  TagIdx,i,j : integer;
  Tag : TMD3Tag;
begin
  Result:=IdentityHMGMatrix;
  TagIdx:=-1;
  for i:=0 to FNumTags do
    if lowercase(trim(TagName))=lowercase(trim(FTags[i].strName)) then begin
      TagIdx:=i;
      Break;
    end;
  if TagIdx=-1 then exit;
  Tag:=FTags[TagIdx+Frame*FNumTags];
  for j:=0 to 2 do
    for i:=0 to 2 do
      Result[i][j]:=Tag.rotation[i][j];
  for i:=0 to 2 do
    Result[3][i]:=Tag.vPosition[i];
end;

end.
