{
  GLFileNMF - NormalMapper loading into GLScene FreeForms/Actors
  
  Notes:
    NormalMapper can be found at http://www.ati.com/developer/tools.html
    
  History:
    16/05/2003 - SG - Creation
}
unit GLFileNMF;

interface

uses
  Classes, GLVectorFileObjects, GLMisc, Geometry, FileNMF;

type
  TGLNMFVectorFile = class (TVectorFile)
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
// ------------------ TGLNMFVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLNMFVectorFile.Capabilities : TDataFileCapabilities;
begin
  Result:=[dfcRead, dfcWrite];
end;

// LoadFromStream
//
procedure TGLNMFVectorFile.LoadFromStream(aStream : TStream);
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
procedure TGLNMFVectorFile.SaveToStream(aStream : TStream);
var
  i,j  : Integer;
  mesh : TMeshObject;
  nmf  : TFileNMF;
begin
  nmf:=TFileNMF.Create;
  try
    mesh:=Owner.MeshObjects[0];
    if mesh.Mode<>momTriangles then exit;
    nmf.NumTris:=(mesh.Vertices.count div 3);
    SetLength(nmf.RawTriangles,nmf.NumTris);
    for i:=0 to nmf.NumTris-1 do begin
      for j:=0 to 2 do begin
        nmf.RawTriangles[i].vert[j]:=mesh.Vertices[3*i+j];
        nmf.RawTriangles[i].norm[j]:=mesh.Normals[3*i+j];
        nmf.RawTriangles[i].texCoord[j].S:=mesh.TexCoords[3*i+j][0];
        nmf.RawTriangles[i].texCoord[j].T:=mesh.TexCoords[3*i+j][1];
      end;
    end;
    nmf.SaveToStream(aStream);
  finally
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

  RegisterVectorFileFormat('nmf', 'NormalMapper files', TGLNMFVectorFile);
  
end.