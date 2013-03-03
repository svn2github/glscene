//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  GLFileMD3 - Code for loading animated MD3 files into GLScene
              FreeForms and Actors.

  History :
    <li>03/03/13 - Yar - Added textures loading (thanks to Tamahome)
    <li>10/11/12 - PW - Added CPP compatibility: changed vector arrays to records
    <li>02/08/04 - LR, YHC - BCB corrections: use record instead array
    <li>21/08/03 - EG - Fixed GetNormalFromMD3Normal (lat/lon were inverted)
    <li>28/02/03 - SG - Creation
}
unit GLFileMD3;

interface

uses
  Classes, SysUtils, GLVectorFileObjects, GLMaterial, ApplicationFileIO,
  VectorGeometry, FileMD3, GLTexture;

type

  TGLMD3VectorFile = class (TVectorFile)
    public
      class function Capabilities : TDataFileCapabilities; override;
      procedure LoadFromStream(aStream : TStream); override;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLMD3VectorFile ------------------
// ------------------

// Capabilities
//
class function TGLMD3VectorFile.Capabilities : TDataFileCapabilities;
begin
  Result:=[dfcRead];
end;

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

  function GetNormalFromMD3Normal(n : array of Byte) : TAffineVector;
  var
    lat,lng : single;
  begin
    // The MD3 normal is a latitude/longitude value that needs
    // to be calculated into cartesian space.
    lat:=(n[1])*(2*pi)/255; lng:=(n[0])*(2*pi)/255;
    result.V[0]:=cos(lat)*sin(lng);
    result.V[1]:=sin(lat)*sin(lng);
    result.V[2]:=cos(lng);
  end;

  procedure AllocateMaterial(meshname:string; texName:string);
  var
    LibMat : TGLLibMaterial;
    matLib : TGLMaterialLibrary;
    textureFilename: string;
  begin
    // If a material library is assigned to the actor/freeform the
    // mesh name will be added as a material.
    if Assigned(Owner.MaterialLibrary) then begin
      matLib:=Owner.MaterialLibrary;
      if Assigned(matLib.Materials.GetLibMaterialByName(meshname)) then exit;
        LibMat:=matlib.Materials.Add;
        LibMat.name:=meshname;
        // load texture from md3 file. TODO: check and parsing shader file
        if texName <> '' then begin
          try
            texName:= ExtractFileName(texName); // get only file name
            // add material lib path or use current directory
            if matLib.TexturePaths = EmptyStr then
              textureFilename := texName
            else
              textureFilename := IncludeTrailingPathDelimiter(matLib.TexturePaths) + texName;
            with libMat.Material.Texture do begin
              Image.LoadFromFile(textureFilename);

              Disabled := False;
              TextureMode := tmModulate;
              libMat.Material.MaterialOptions := [moNoLighting];
            end;
          except
              on E: ETexture do begin
            // no need raise - for continue mesh loading
              if not TGLBaseMesh(GetOwner).IgnoreMissingTextures then
                raise;
              end;
          end;
        end;
    end;
  end;

begin
  MD3File:=TFileMD3.Create;
  MD3File.LoadFromStream(aStream);
  try
    for i:=0 to MD3File.ModelHeader.numMeshes-1 do begin
      mesh:=TMorphableMeshObject.CreateOwned(Owner.MeshObjects);
      mesh.Name:=trim(string(MD3File.MeshData[i].MeshHeader.strName));
      // add text coords, vertex and normals to mesh (added in to facesgroup)
{
      for j:=0 to MD3File.MeshData[i].MeshHeader.numVertices-1 do begin
        mesh.TexCoords.Add(MD3File.MeshData[i].TexCoords[j].textureCoord.V[0]*0.75,1-MD3File.MeshData[i].TexCoords[j].textureCoord.V[1],0);
        mesh.Vertices.Add(MD3File.MeshData[i].Vertices[j].vertex.X/64,MD3File.MeshData[i].Vertices[j].vertex.Y/64,MD3File.MeshData[i].Vertices[j].vertex.Z/64);
        mesh.Binormals.AddVector(GetNormalFromMD3Normal(MD3File.MeshData[i].Vertices[j].normal.V));
      end;
}
      with mesh, MD3File do begin
        Mode:=momFaceGroups;
        faceGroup:=TFGIndexTexCoordList.CreateOwned(FaceGroups);
        with faceGroup do begin
          MaterialName:=mesh.Name;
          if(Length(MeshData[i].Skins)>0) then
            AllocateMaterial(mesh.Name,MeshData[i].Skins[0].strName)
          else AllocateMaterial(mesh.Name,'');
          numTris:=MeshData[i].MeshHeader.numTriangles;
          VertexIndices.Capacity:=numTris*3;
          TexCoords.Capacity:=numTris*3;
          // Get the vertex indices and texture coordinates
          for j:=0 to MeshData[i].MeshHeader.numTriangles-1 do begin
            with MeshData[i].Triangles[j] do begin
              Add(vertexIndices.V[0],
                  MeshData[i].TexCoords[vertexIndices.V[0]].textureCoord.V[0],
                  (1-MeshData[i].TexCoords[vertexIndices.V[0]].textureCoord.V[1]));
              Add(vertexIndices.V[2],
                  MeshData[i].TexCoords[vertexIndices.V[2]].textureCoord.V[0],
                  (1-MeshData[i].TexCoords[vertexIndices.V[2]].textureCoord.V[1]));
              Add(vertexIndices.V[1],
                  MeshData[i].TexCoords[vertexIndices.V[1]].textureCoord.V[0],
                  (1-MeshData[i].TexCoords[vertexIndices.V[1]].textureCoord.V[1]));
            end;
          end;
        end;

        // Get the mesh data for each morph frame
        for j:=0 to ModelHeader.numFrames-1 do begin
          morphTarget:=TMeshMorphTarget.CreateOwned(MorphTargets);
          morphTarget.Name:=Trim(string(MeshData[i].MeshHeader.strName))+'['+IntToStr(j)+']';
          numVerts:=MeshData[i].MeshHeader.numVertices;
          morphTarget.Vertices.Capacity:=numVerts;
          for k:=numVerts*j to numVerts*(j+1)-1 do begin
            morphTarget.Vertices.Add(
              MeshData[i].Vertices[k].Vertex.V[0]/64,
              MeshData[i].Vertices[k].Vertex.V[1]/64,
              MeshData[i].Vertices[k].Vertex.V[2]/64);
            morphTarget.Normals.Add(
              GetNormalFromMD3Normal(MeshData[i].Vertices[k].normal.V));
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('md3', 'MD3 files', TGLMD3VectorFile);

end.
