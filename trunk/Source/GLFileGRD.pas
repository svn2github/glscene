//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLFileGRD<p>

  GRD (Grid Text Format) vector file format implementation.<p>

  <b>History :</b><font size=-1><ul>
  <li>05/12/14 - PW - Added to GLScene_Runtime.dpk
  <li>10/04/09 - PW - Separated from GLVectorFileObjects.pas
  </ul></font>
}
unit GLFileGRD;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,

  // GLScene
  GLVectorGeometry, GLVectorTypes, GLVectorFileObjects,
  GLApplicationFileIO, GLGraph, GLTypes;

type
  // GLFileGRD
  //
  { : The GRD file represents ascii grid formats in 2D/3D.<p>
    This is a format for storing regular grids as a
    matrices of cell centers. The format is extensible, supports variations and
    subformats. This importer only works for the simplest variant (triangles
    without specified normals, and will ignore most header specifications. }
  TGLGRDVectorFile = class(TVectorFile)
  public
    { Public Declarations }
    HeightField: TGLHeightField;
    GridArray: TGLPoint3DArray;
    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure HeightFieldGetHeight(const X, Y: Single; var Z: Single;
      var Color: TVector4f; var TexPoint: TTexPoint);
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------
// ------------------ TGLGRDVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLGRDVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TGLGRDVectorFile.HeightFieldGetHeight(const X, Y: Single;
  var Z: Single; var Color: TVector4f; var TexPoint: TTexPoint);

// This assigns the heights from the grid in memory
var
  Heit: Single;

begin
  (*
    Heit := GridArray[Trunc(Y) + (GRD.Height div 2),Trunc(X) + (GRD.Width div 2)];
    if Heit = GRD.null then
    Z := 0
    else
    Z := (Heit / GRD.CellSize) * Vertical_exaggeration;

    {The equation above is based on fact that each x,y is a step on the grid and
    in real terms, each step is cellsize metres wide}
    with HeightField do
    begin
    TexPoint.S := (x - XSamplingScale.min)/GRD.Width;
    TexPoint.T := (y - YSamplingScale.min)/GRD.Height;
    end;
  *)
end;

// LoadFromStream
//
procedure TGLGRDVectorFile.LoadFromStream(aStream: TStream);
var
  i, j: Integer;
  sl, tl: TStringList;

  // mesh : TMeshObject;
  // fg : TFGVertexIndexList;
  // n1, n2, n3 : Integer;
  // v1, v2, v3, n : TAffineVector;
  // p : PChar;

  Nx, Ny: Integer;
  Dx, Dy: Single;
  Xo, Xe, Yo, Ye, Zo, Ze: Single;

  S: String;

begin
  sl := TStringList.Create;
  tl := TStringList.Create;
  try
    sl.LoadFromStream(aStream);
    S := Copy(sl[0], 1, 5);
    if (sl[0] <> 'DSAA') and (S <> 'ncols') then
    begin
      raise Exception.Create('Not a valid grd file !');
      Exit;
    end;

    HeightField := TGLHeightField.Create(Owner);

    SetLength(GridArray, sl.Count);
    Zo := 3 * 10E38; // Low
    Ze := -3 * 10E38; // High

    if (sl[0] = 'DSAA') then // Surfer grid
    begin
      tl.DelimitedText := sl[1];
      Nx := StrToInt(tl[0]);
      Ny := StrToInt(tl[1]); // Nx Ny

      tl.DelimitedText := sl[2];
      Xo := StrToInt(tl[0]);
      Xe := StrToInt(tl[1]); // Xo Xe

      tl.DelimitedText := sl[3];
      Yo := StrToInt(tl[0]);
      Ye := StrToInt(tl[1]); // Yo Ye

      tl.DelimitedText := sl[4];
      Zo := StrToInt(tl[0]);
      Ze := StrToInt(tl[1]); // Zo Ze

      for i := 0 to Ny - 1 do
      begin
        tl.DelimitedText := sl[i];
        GridArray[i].X := Xo + (i mod Nx) * Dx;
        GridArray[i].Y := Yo + Dy * (i div Nx);
        GridArray[i].Z := Zo + StrToFloat(tl[i]);

        {
          while i < sl.Count do begin
          //
          end;
        }
      end;

    end
    else // ArcInfo grid
    begin
      tl.DelimitedText := sl[0];
      Nx := StrToInt(tl[1]); // ncols
      tl.DelimitedText := sl[1];
      Ny := StrToInt(tl[1]); // nrows
      tl.DelimitedText := sl[2];
      Xo := StrToFloat(tl[1]); // xllcorner
      tl.DelimitedText := sl[3];
      Yo := StrToFloat(tl[1]); // yllcorner
      tl.DelimitedText := sl[4];
      Dx := StrToFloat(tl[1]); // cellsize
      Dy := Dx;
      // NODATA_value :=  -9999

    end;

    (*
      grid := TGLHeightField.Create();
      grid.Mode := momFaceGroups;
      while i<sl.Count do begin
      if sl[i]='end_header' then Break;
      if Copy(sl[i], 1, 14)='element vertex' then
      nbVertices:=StrToIntDef(Copy(sl[i], 16, MaxInt), 0);
      if Copy(sl[i], 1, 12)='element face' then
      nbFaces:=StrToIntDef(Copy(sl[i], 14, MaxInt), 0);
      Inc(i);
      end;
      Inc(i);
      // vertices
      grid.Vertices.Capacity:=nbVertices;
      while (i<sl.Count) and (nbVertices>0) do begin
      p:=PChar(sl[i]);
      grid.Vertices.Add(ParseFloat(p), ParseFloat(p), ParseFloat(p));//AffineVectorMake(StrToFloatDef(tl[0]), StrToFloatDef(tl[1]), StrToFloatDef(tl[2])));}
      Dec(nbVertices);
      Inc(i);
      end;
      // faces
      fg:=TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
      fg.Mode:=fgmmTriangles;
      fg.VertexIndices.Capacity:=nbFaces*3;
      while (i<sl.Count) and (nbFaces>0) do begin
      p:=PChar(sl[i]);
      ParseInteger(p); // skip index
      fg.VertexIndices.Add(ParseInteger(p), ParseInteger(p), ParseInteger(p));
      Dec(nbFaces);
      Inc(i);
      end;
      grid.BuildNormals(fg.VertexIndices, momTriangles);
    *)
  finally
    tl.Free;
    sl.Free;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

RegisterVectorFileFormat('grd', 'ArcInfo/Surfer grids', TGLGRDVectorFile);

end.
