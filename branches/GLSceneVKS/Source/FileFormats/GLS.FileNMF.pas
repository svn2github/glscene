//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
  GLS.FileNMF - NormalMapper loading into VKScene FreeForms/Actors
    .
}
unit GLS.FileNMF;

interface

uses
  System.Classes,
  //GLS
  GLS.VectorFileObjects, GLS.VectorGeometry,
  GLS.VectorLists, GLS.ApplicationFileIO,
  FileNMF;

type
  TVKNMFVectorFile = class (TVectorFile)
    public
      class function Capabilities : TDataFileCapabilities; override;
      procedure LoadFromStream(aStream : TStream); override;
      procedure SaveToStream(aStream : TStream); override;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TVKNMFVectorFile ------------------
// ------------------

// Capabilities
//
class function TVKNMFVectorFile.Capabilities : TDataFileCapabilities;
begin
  Result:=[dfcRead, dfcWrite];
end;

// LoadFromStream
//
procedure TVKNMFVectorFile.LoadFromStream(aStream : TStream);
var
  i,j  : Integer;
  mesh : TMeshObject;
  nmf  : TFileNMF;
begin
  nmf:=TFileNMF.Create;
  try
    nmf.LoadFromStream(aStream);
    mesh:=TMeshObject.CreateOwned(Owner.MeshObjects);
    mesh.Mode:=momTriangles;
    for i:=0 to nmf.NumTris-1 do begin
      for j:=0 to 2 do begin
        mesh.Vertices.Add(nmf.RawTriangles[i].vert[j]);
        mesh.Normals.Add(nmf.RawTriangles[i].norm[j]);
        mesh.TexCoords.Add(nmf.RawTriangles[i].texCoord[j]);
      end;
    end;
  finally
    nmf.Free;
  end;
end;

// SaveToStream
//
procedure TVKNMFVectorFile.SaveToStream(aStream : TStream);
var
  i,j  : Integer;
  nmf  : TFileNMF;
  Vertices,
  TempVertices,
  Normals,
  TexCoords : TAffineVectorList;
begin
  nmf:=TFileNMF.Create;
  Vertices:=TAffineVectorList.Create;
  Normals:=TAffineVectorList.Create;
  TexCoords:=TAffineVectorList.Create;
  try
    for i:=0 to Owner.MeshObjects.Count-1 do begin
      TempVertices:=Owner.MeshObjects[i].ExtractTriangles(TexCoords,Normals);
      Vertices.Add(TempVertices);
      TempVertices.Free;
    end;

    nmf.NumTris:=(Vertices.count div 3);
    SetLength(nmf.RawTriangles,nmf.NumTris);
    for i:=0 to nmf.NumTris-1 do begin
      for j:=0 to 2 do begin
        nmf.RawTriangles[i].vert[j]:=Vertices[3*i+j];
        nmf.RawTriangles[i].norm[j]:=Normals[3*i+j];
        nmf.RawTriangles[i].texCoord[j].S:=TexCoords[3*i+j].V[0];
        nmf.RawTriangles[i].texCoord[j].T:=TexCoords[3*i+j].V[1];
      end;
    end;
    nmf.SaveToStream(aStream);
  finally
    Vertices.Free;
    Normals.Free;
    TexCoords.Free;
    nmf.Free;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterVectorFileFormat('nmf', 'NormalMapper files', TVKNMFVectorFile);
  
end.