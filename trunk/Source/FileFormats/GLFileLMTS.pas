//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileLMTS<p>

	<b>History : </b><font size=-1><ul>
      <li>02/01/07 - PvD - Dealing with non empty material libraries.
      <li>02/01/07 - PvD - Mirrored mesh in X to original orientation.
      <li>01/01/07 - Dave Gravel - Modification to make it work.
      <li>10/09/03 - Domin - Creation
   </ul><p>
}
unit GLFileLMTS;

interface

uses Windows, Graphics, Classes, SysUtils, GLVectorFileObjects, TGA, JPEG;

const
  C_LMTS_ID   = $53544D4C;
  C_LMTS_VER  = 4;
  C_LMTS_SUBS = $53425553;
  C_LMTS_TEXT = $54584554;
  C_LMTS_TRIS = $53495254;

  C_LMTS_TEXFNLEN = 255; // max texture filename length

type
  PLMTS_Header = ^TLMTS_Header;
  TLMTS_Header = record //packed
    ID         : cardinal;
    Ver        : cardinal;
    headerSize : cardinal;
    nTexts     : word; // # of textures
    nSubsets   : word;
    nTris      : cardinal;
    subSize    : word;
    vtxSize    : word;
  end;

  PLMTS_TexData = ^TLMTS_TexData;
  TLMTS_TexData = record //packed
    FName : array[0..C_LMTS_TEXFNLEN] of char;
    Flags : word;
  end;

  PLMTS_Subset = ^TLMTS_Subset;
  TLMTS_Subset = record //packed
    Offset  : longint;
    Count   : longint;
    TextID1 : word;
    TextID2 : word;
  end;

  PLMTS_Vertex = ^TLMTS_Vertex;
  TLMTS_Vertex =  record //packed
    x, y, z : single;
    u1, v1, u2, v2 : single;
  end;

  PLMTS = ^TLMTS;
  TLMTS = record
    header  : TLMTS_Header;
    usrData : pointer;
    usrSize : cardinal;
    texData : pointer;
    subsets : pointer;
    tris    : pointer;
    ok      : boolean;
  end;

  TGLLMTSVectorFile = class(TVectorFile)
  public
    procedure LoadFromStream(aStream: TStream); override;
  end;


implementation

uses GLUtils, GLTexture, GLMisc;

procedure TGLLMTSVectorFile.LoadFromStream(aStream: TStream);
var
  MO: TMeshObject;
  FG: TFGVertexIndexList;
  LL: TGLMaterialLibrary;
  ML: TGLMaterialLibrary;
  LMTS: TLMTS;
  T: TLMTS_TexData;
  V: array [0..2] of TLMTS_Vertex;
  S: TLMTS_Subset;
  _4cc: cardinal;
  C: integer;
  NBT: Integer;
  LLI: Integer;
  MLI: Integer;
begin

  MO := TMeshObject.CreateOwned(Owner.MeshObjects);
  MO.Mode := momFaceGroups;

  LLI := 0;
  MLI := 0;

  LL := Owner.LightmapLibrary;
  if assigned(LL) then
    LLI := LL.Materials.Count;
  ML := Owner.MaterialLibrary;
  if assigned(ML) then
    MLI := ML.Materials.Count;

  try
    // read header...
    aStream.Read(LMTS.header, SizeOf(TLMTS_Header));
    // verify...
    if (LMTS.header.ID <> C_LMTS_ID) or
       (LMTS.header.Ver <> C_LMTS_VER) or
       (LMTS.header.headerSize < SizeOf(TLMTS_Header)) or
       (LMTS.header.subSize < SizeOf(TLMTS_Subset)) or
       (LMTS.header.vtxSize < SizeOf(TLMTS_Vertex)) then
      raise Exception.Create('Error in header');

    // read "user" data - actually skip this data...
    LMTS.usrSize := LMTS.header.headerSize - SizeOf(TLMTS_Header);
    if (LMTS.usrSize > 0) then
      aStream.Seek(LMTS.usrSize, soFromCurrent);

    // read texture filenames data...
    aStream.Read(_4cc, SizeOf(_4cc));
    if (_4cc <> C_LMTS_TEXT) then
      raise Exception.Create('Texture data not found');

    NBT := 0;
    for C := 0 to LMTS.header.nTexts - 1 do
    begin
      aStream.Read(T, SizeOf(TLMTS_TexData));
      if T.Flags = 0 then
      begin
        if Assigned(ML) then begin
          with ML.AddTextureMaterial(T.FName,T.FName) do begin
            Material.Texture.TextureMode := tmModulate;
          end;
        end;
        Inc(NBT);
      end else
      if Assigned(LL) then
      begin
        with LL.AddTextureMaterial(T.FName,T.FName).Material.Texture do
        begin
          MinFilter := miLinear;
          TextureWrap := twNone;
          TextureFormat := tfRGB;
          TextureMode := tmModulate;
        end;
      end;
    end;

    // read subset data...
    aStream.Read(_4cc, SizeOf(_4cc));
    if (_4cc <> C_LMTS_SUBS) then
      raise Exception.Create('Subset data not found');
    for C := LMTS.header.nSubsets - 1 downto 0 do
    begin
      aStream.Read(S, LMTS.header.subSize);
      FG := TFGVertexIndexList.CreateOwned(MO.FaceGroups);
      FG.Mode := fgmmTriangles;
      FG.VertexIndices.AddSerie(S.Offset*3, 1, S.Count*3);
      if Assigned(ML) and (S.TextID1 <> $FFFF) then
        FG.MaterialName := ML.Materials[S.TextID1 + MLI].Name;
      if Assigned(LL) and (S.TextID2 <> $FFFF) then
        if LL = ML then
          FG.LightMapIndex := LL.Materials[S.TextID2].ID + LLI
        else
          FG.LightMapIndex := LL.Materials[S.TextID2-NBT + LLI].ID;
    end;
    // read vertex data...
    aStream.Read(_4cc, SizeOf(_4cc));
    if (_4cc <> C_LMTS_TRIS) then
      raise Exception.Create('Vertex data not found');
    for c:=0 to Integer(LMTS.header.nTris)-1 do
    begin

      aStream.Read(V[0], LMTS.header.vtxSize);
      aStream.Read(V[1], LMTS.header.vtxSize);
      aStream.Read(V[2], LMTS.header.vtxSize);

      MO.Vertices.Add(V[0].x, V[0].z, V[0].y);
      MO.TexCoords.Add(V[0].u1,-V[0].v1);
      MO.LightmapTexCoords.Add(V[0].u2, 1-V[0].v2);

      MO.Vertices.Add(V[2].x, V[2].z, V[2].y);
      MO.TexCoords.Add(V[2].u1,-V[2].v1);
      MO.LightmapTexCoords.Add(V[2].u2, 1-V[2].v2);

      MO.Vertices.Add(V[1].x, V[1].z, V[1].y);
      MO.TexCoords.Add(V[1].u1,-V[1].v1);
      MO.LightmapTexCoords.Add(V[1].u2, 1-V[1].v2);
    end;
  except
    MO.Free;
  end;
end;

initialization

  RegisterVectorFileFormat('lmts', 'Pulsar Studio LMTS File Format', TGLLMTSVectorFile);

end.
