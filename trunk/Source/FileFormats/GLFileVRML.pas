//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileVRML<p>

   Preliminary VRML vector file support for GLScene.<p>

   <b>History :</b><font size=-1><ul>
      <li>18/01/05 - SG - Added polygon tessellation routine to decompose 
                          a polygon mesh to a triangle mesh
      <li>14/01/05 - SG - Added to CVS
   </ul></font>
}
unit GLFileVRML;

interface

uses
  Classes, SysUtils, GLVectorFileObjects, GLMisc, GLTexture, ApplicationFileIO,
  VectorGeometry, VectorLists, VRMLParser, MeshUtils;

type

  TGLVRMLVectorFile = class (TVectorFile)
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

// TessellatePolygon
//
procedure TessellatePolygon(PolyVerts : TAffineVectorList;
  PolyIndices, TriIndices : TIntegerList);

  function IsPolyClockWise : Boolean;
  var
    i : Integer;
    det : Single;
    mat : TAffineMatrix;
  begin
    for i:=0 to PolyIndices.Count-1 do begin
      mat[0]:=PolyVerts[PolyIndices[i]];
      mat[1]:=PolyVerts[PolyIndices[i+1]];
      if i = PolyIndices.Count-1 then
        mat[2]:=PolyVerts[PolyIndices[0]]
      else
        mat[2]:=PolyVerts[PolyIndices[i+2]];
      det:=det+MatrixDeterminant(mat);
    end;
    Result:=(det<0);
  end;

  function IsTriClockWise(v0, v1, v2 : TAffineVector) : Boolean;
  var
    mat : TAffineMatrix;
  begin
    mat[0]:=v0;
    mat[1]:=v1;
    mat[2]:=v2;
    Result:=(MatrixDeterminant(mat)<0);
  end;

  function PointInTriangle(p,v0,v1,v2 : TAffineVector;
    IsClockWise : Boolean = False) : Boolean;
  begin
    Result:=not ((IsTriClockWise(v1,v0,p) = IsClockWise) or
                 (IsTriClockWise(v0,v2,p) = IsClockWise) or
                 (IsTriClockWise(v2,v1,p) = IsClockWise));
  end;

var
  i, j,
  prev, next,
  min_vert, min_prev, min_next : Integer;
  PolyCW, NoPointsInTriangle : Boolean;
  v : TAffineMatrix;
  temp : TIntegerList;
  min_dist, d : Single;
begin
  temp:=TIntegerList.Create;
  try
    PolyCW:=IsPolyClockWise;
    temp.Assign(PolyIndices);
    while temp.Count>3 do begin
      min_dist:=10e7;
      min_vert:=-1;
      for i:=0 to temp.Count-1 do begin
        prev:=i-1;
        next:=i+1;
        if prev<0 then prev:=temp.Count-1;
        if next>temp.Count-1 then next:=0;
        v[0]:=PolyVerts[temp[prev]];
        v[1]:=PolyVerts[temp[i]];
        v[2]:=PolyVerts[temp[next]];
        if IsTriClockWise(v[0], v[1], v[2]) = PolyCW then begin
          NoPointsInTriangle:=True;
          for j:=0 to temp.Count-1 do begin
            if (j<>i) and (j<>prev) and (j<>next) then begin
              if PointInTriangle(PolyVerts[temp[j]], v[0], v[1], v[2], PolyCW) then begin
                NoPointsInTriangle:=False;
                Break;
              end;
            end;
          end;
          if NoPointsInTriangle then begin
            d:=VectorDistance2(v[0], v[2]);
            if d<min_dist then begin
              min_dist:=d;
              min_prev:=prev;
              min_vert:=i;
              min_next:=next;
            end;
          end;
        end;
      end;
      if min_vert = -1 then begin
        raise Exception.Create('Failed to tessellate polygon.');
      end else begin
        TriIndices.Add(temp[min_prev], temp[min_vert], temp[min_next]);
        temp.Delete(min_vert);
      end;
    end;
    TriIndices.Add(temp[0], temp[1], temp[2]);
  finally
    temp.Free;
  end;
end;


// ------------------
// ------------------ TGLVRMLVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLVRMLVectorFile.Capabilities : TDataFileCapabilities;
begin
  Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGLVRMLVectorFile.LoadFromStream(aStream : TStream);
var
  mesh : TMeshObject;
  uniqueMatID : Integer;
  currentMaterial : TGLLibMaterial;
  currentTransform : TMatrix;

  function GetUniqueMaterialName : String;
  var
    libMat : TGLLibMaterial;
  begin
    repeat
      Result:='UntitledMaterial'+IntToStr(uniqueMatID);
      Inc(uniqueMatID);
      libMat:=Owner.MaterialLibrary.Materials.GetLibMaterialByName(Result);
    until not Assigned(libMat);
  end;

  function AddMaterialToLibrary(VRMLMaterial : TVRMLMaterial) : TGLLibMaterial;
  var
    matname : String;
  begin
    Result:=nil;
    if not Assigned(Owner.MaterialLibrary) then Exit;

    if VRMLMaterial.DefName = '' then
      matname:=GetUniqueMaterialName
    else
      matname:=VRMLMaterial.DefName;

    Result:=Owner.MaterialLibrary.Materials.GetLibMaterialByName(matname);
    if not Assigned(Result) then begin
      Result:=Owner.MaterialLibrary.Materials.Add;
      Result.Name:=matname;
    end;

    // Assign values from the current material
    if Assigned(currentMaterial) then
      Result.Material.FrontProperties.Assign(currentMaterial.Material.FrontProperties);

    with Result.Material.FrontProperties do begin
      if VRMLMaterial.HasDiffuse then
        Diffuse.Color:=VectorMake(VRMLMaterial.DiffuseColor, Diffuse.Color[3]);
      if VRMLMaterial.HasAmbient then
        Ambient.Color:=VectorMake(VRMLMaterial.AmbientColor, Ambient.Color[3]);
      if VRMLMaterial.HasSpecular then
        Specular.Color:=VectorMake(VRMLMaterial.SpecularColor, Specular.Color[3]);
      if VRMLMaterial.HasEmissive then
        Emission.Color:=VectorMake(VRMLMaterial.EmissiveColor, Emission.Color[3]);
      if Shininess = 0 then Shininess:=16;
      if VRMLMaterial.HasShininess then
        Shininess:=Floor(128*VRMLMaterial.Shininess);
      if VRMLMaterial.HasTransparency then begin
        Diffuse.Color:=VectorMake(AffineVectorMake(Diffuse.Color),
                                  1-VRMLMaterial.Transparency);
        Ambient.Color:=VectorMake(AffineVectorMake(Ambient.Color),
                                  1-VRMLMaterial.Transparency);
        Specular.Color:=VectorMake(AffineVectorMake(Specular.Color),
                                  1-VRMLMaterial.Transparency);
        Emission.Color:=VectorMake(AffineVectorMake(Emission.Color),
                                  1-VRMLMaterial.Transparency);
      end;
    end;
    if VRMLMaterial.HasTransparency then
      Result.Material.BlendingMode:=bmTransparency;
  end;

  procedure RecursNodes(node : TVRMLNode);
  var
    i,j,n : Integer;
    points : TSingleList;
    indices : TIntegerList;
    fg : TFGVertexIndexList;
    face : TIntegerList;
    normals : TAffineVectorList;
    tempLibMat : TGLLibMaterial;
    saveTransform, mat : TMatrix;
    saveMaterial : TGLLibMaterial;
    axis : TAffineVector;
    angle : Single;
  begin
    // Store current transform and material
    saveTransform:=currentTransform;
    saveMaterial:=currentMaterial;

    // Look for a child node data (transforms and materials)
    for i:=0 to node.Count-1 do
      if node[i] is TVRMLTransform then begin
        if not VectorEquals(TVRMLTransform(node[i]).Rotation, NullHMGVector) then begin
          axis:=AffineVectorMake(TVRMLTransform(node[i]).Rotation);
          angle:=TVRMLTransform(node[i]).Rotation[3];
          mat:=MatrixMultiply(CreateRotationMatrix(axis, angle),
                              CreateRotationMatrixZ(Pi/2));
        end else
          mat:=IdentityHMGMatrix;
        for j:=0 to 2 do
          mat[j]:=VectorScale(mat[j], TVRMLTransform(node[i]).ScaleFactor[j]);
        mat[3]:=PointMake(TVRMLTransform(node[i]).Center);
        currentTransform:=MatrixMultiply(mat, currentTransform);
      end else if node[i] is TVRMLMaterial then begin
        currentMaterial:=AddMaterialToLibrary(TVRMLMaterial(node[i]));
      end else if node[i] is TVRMLUse then begin
        tempLibMat:=Owner.MaterialLibrary.Materials.GetLibMaterialByName(TVRMLUse(node[i]).Value);
        if Assigned(tempLibMat) then
          currentMaterial:=tempLibMat;
      end;

    // Read node data
    if (node.Name = 'Coordinate3') and (node.Count>0) then begin
      mesh:=TMeshObject.CreateOwned(Owner.MeshObjects);
      points:=TVRMLSingleArray(node[0]).Values;
      for i:=0 to (points.Count div 3) - 1 do
        mesh.Vertices.Add(points[3*i], points[3*i+1], points[3*i+2]);
      mesh.Vertices.TransformAsPoints(currentTransform);
      mesh.Normals.Count:=mesh.Vertices.Count;

    end else if (node.Name = 'IndexedFaceSet') and (node.Count>0) and Assigned(mesh) then begin
      face:=TIntegerList.Create;
      fg:=TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
      mesh.Mode:=momFaceGroups;
      if Assigned(currentMaterial) then
        fg.MaterialName:=currentMaterial.Name;
      indices:=TVRMLIntegerArray(node[0]).Values;
      i:=0;
      n:=0;
      while i<indices.Count do begin
        if indices[i] = -1 then begin
          if face.Count<=4 then begin
            for j:=0 to face.Count-3 do
              fg.VertexIndices.Add(face[0], face[j+1], face[j+2]);
          end else begin
            TessellatePolygon(mesh.Vertices, face, fg.VertexIndices);
          end;
          face.Clear;
        end else begin
          face.Add(indices[i]);
        end;
        i:=i+1;
      end;
      normals:=BuildNormals(mesh.Vertices, fg.VertexIndices);
      for i:=0 to fg.VertexIndices.Count-1 do
        mesh.Normals[fg.VertexIndices[i]]:=normals[fg.VertexIndices[i]];
      normals.Free;
      face.Free;

    end else begin
      // Continue recursion through child nodes
      for i:=0 to node.Count-1 do
        RecursNodes(node[i]);
    end;

    // Load transform and material from stored values
    currentTransform:=saveTransform;
    currentMaterial:=saveMaterial;
  end;

var
  str : TStringList;
  parser : TVRMLParser;
begin
  str:=TStringList.Create;
  parser:=TVRMLParser.Create;

  currentMaterial:=nil;
  currentTransform:=IdentityHMGMatrix;

  try
    str.LoadFromStream(aStream);
    parser.Parse(str.Text);

    currentMaterial:=nil;
    RecursNodes(parser.RootNode);

  finally
    str.Free;
    parser.Free;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('wrl', 'VRML files', TGLVRMLVectorFile);

end.