// BumpMapping
{: Some useful methods for setting up bump maps.<p>

   <b>History : </b><font size=-1><ul>
      <li>28/07/03 - SG - Creation
   </ul></font>
}
unit BumpMapping;

interface

uses
  Geometry, VectorLists;

// Object space
procedure CalcObjectSpaceLightVectors(Light : TAffineVector;
                                      Vertices: TAffineVectorList;
                                      Colors: TVectorList);

// Tangent space
procedure SetupTangentSpace(Vertices, Normals, TexCoords,
                            Tangents, BiNormals : TAffineVectorList);
procedure CalcTangentSpaceLightVectors(Light : TAffineVector;
                                       Vertices, Normals,
                                       Tangents, BiNormals : TAffineVectorList;
                                       Colors: TVectorList);

implementation

// CalcObjectSpaceLightVectors
//
procedure CalcObjectSpaceLightVectors(Light : TAffineVector;
                                      Vertices: TAffineVectorList;
                                      Colors: TVectorList);
var
  i   : Integer;
  vec : TAffineVector;
begin
  Colors.Clear;
  for i:=0 to Vertices.Count-1 do begin
    vec:=VectorNormalize(VectorSubtract(Light,Vertices[i]));
    Colors.Add(VectorAdd(VectorScale(vec,0.5),0.5),1);
  end;
end;

// SetupTangentSpace
//
procedure SetupTangentSpace(Vertices, Normals, TexCoords,
                            Tangents, BiNormals : TAffineVectorList);
var
  i,j        : Integer;
  v,n,t      : TAffineMatrix;
  vt,tt      : TAffineVector;
  interp,dot : Single;

  procedure SortVertexData(sortidx : Integer);
  begin
    if t[0][sortidx]<t[1][sortidx] then begin
      vt:=v[0];   tt:=t[0];
      v[0]:=v[1]; t[0]:=t[1];
      v[1]:=vt;   t[1]:=tt;
    end;
    if t[0][sortidx]<t[2][sortidx] then begin
      vt:=v[0];   tt:=t[0];
      v[0]:=v[2]; t[0]:=t[2];
      v[2]:=vt;   t[2]:=tt;
    end;
    if t[1][sortidx]<t[2][sortidx] then begin
      vt:=v[1];   tt:=t[1];
      v[1]:=v[2]; t[1]:=t[2];
      v[2]:=vt;   t[2]:=tt;
    end;
  end;

begin
  for i:=0 to (Vertices.Count div 3)-1 do begin
    // Get triangle data
    for j:=0 to 2 do begin
      v[j]:=Vertices[3*i+j];
      n[j]:=Normals[3*i+j];
      t[j]:=TexCoords[3*i+j];
    end;

    for j:=0 to 2 do begin
      // Compute tangent
      SortVertexData(1);

      if (t[2][1]-t[0][1]) = 0 then interp:=1
      else interp:=(t[1][1]-t[0][1])/(t[2][1]-t[0][1]);

      vt:=VectorLerp(v[0],v[2],interp);
      interp:=t[0][0]+(t[2][0]-t[0][0])*interp;
      vt:=VectorSubtract(vt,v[1]);
      if t[1][0]<interp then vt:=VectorNegate(vt);
      dot:=VectorDotProduct(vt,n[j]);
      vt[0]:=vt[0]-n[j][0]*dot;
      vt[1]:=vt[1]-n[j][1]*dot;
      vt[2]:=vt[2]-n[j][2]*dot;
      Tangents.Add(VectorNormalize(vt));

      // Compute Bi-Normal
      SortVertexData(0);

      if (t[2][0]-t[0][0]) = 0 then interp:=1
      else interp:=(t[1][0]-t[0][0])/(t[2][0]-t[0][0]);

      vt:=VectorLerp(v[0],v[2],interp);
      interp:=t[0][1]+(t[2][1]-t[0][1])*interp;
      vt:=VectorSubtract(vt,v[1]);
      if t[1][1]<interp then vt:=VectorNegate(vt);
      dot:=VectorDotProduct(vt,n[j]);
      vt[0]:=vt[0]-n[j][0]*dot;
      vt[1]:=vt[1]-n[j][1]*dot;
      vt[2]:=vt[2]-n[j][2]*dot;
      BiNormals.Add(VectorNormalize(vt));
    end;
  end;
end;

// CalcTangentSpaceLightVectors
//
procedure CalcTangentSpaceLightVectors(Light : TAffineVector;
                                       Vertices, Normals,
                                       Tangents, BiNormals : TAffineVectorList;
                                       Colors: TVectorList);
var
  i   : Integer;
  mat : TAffineMatrix;
  vec : TAffineVector;
begin
  Colors.Clear;
  for i:=0 to Vertices.Count-1 do begin
    mat[0]:=Tangents[i];
    mat[1]:=BiNormals[i];
    mat[2]:=Normals[i];
    TransposeMatrix(mat);
    vec:=VectorNormalize(VectorTransform(VectorSubtract(Light,Vertices[i]),mat));
    vec[0]:=-vec[0];
    Colors.Add(VectorAdd(VectorScale(vec,0.5),0.5),1);
  end;
end;

end.