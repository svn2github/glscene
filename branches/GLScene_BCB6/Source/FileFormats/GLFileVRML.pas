//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileVRML<p>

   Preliminary VRML vector file support for GLScene.<p>

   <b>History :</b><font size=-1><ul>
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
    face : array of Integer;
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
      fg:=TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
      mesh.Mode:=momFaceGroups;
      if Assigned(currentMaterial) then
        fg.MaterialName:=currentMaterial.Name;
      indices:=TVRMLIntegerArray(node[0]).Values;
      i:=0;
      n:=0;
      while i<indices.Count do begin
        if indices[i] = -1 then begin
          for j:=0 to n-3 do
            fg.VertexIndices.Add(face[0], face[j+1], face[j+2]);
          n:=0;
        end else begin
          if n > Length(face)-1 then
            SetLength(face, n+1);
          face[n]:=indices[i];
          n:=n+1;
        end;
        i:=i+1;
      end;
      normals:=BuildNormals(mesh.Vertices, fg.VertexIndices);
      for i:=0 to fg.VertexIndices.Count-1 do
        mesh.Normals[fg.VertexIndices[i]]:=normals[fg.VertexIndices[i]];
      normals.Free;

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
