{: GLFileLWO<p>

    Support-code to load Lightwave LWO Files (v6.0+, partial support).<p>

	<b>History : </b><font size=-1><ul>
      <li>19/11/02 - EG - Changes by BJ
      <li>14/11/02 - EG - Added header, fixed warnings
   </ul><p>

   Original code: "Brian Johns" <brianjohns1@hotmail.com>
}
unit GLFileLWO;

interface

uses Classes, GLVectorFileObjects, LWObjects;

type
  TGLLWOVectorFile = class (TVectorFile)
  private
    FLWO: TLWObjectFile;
    FPnts: TLWPnts;
    procedure AddLayr(Layr: TLWLayr; LWO: TLWObjectFile);
    procedure AddSurf(Surf: TLWSurf; LWO: TLWObjectFile);
    procedure AddPnts(Pnts: TLWPnts; Mesh: TMeshObject);
    procedure AddPols(Pols: TLWPols; Mesh: TMeshObject);
    procedure AddVMap(VMap: TLWVMap; Mesh: TMeshObject);
  public
    procedure LoadFromStream(aStream: TStream); override;
  end;

implementation

uses SysUtils, Geometry, GLTexture, VectorTypes;

type
  PVector3f = ^TVector3f;

function CalcTriNorm(v1,v2,v3: TVec12): TVector3f;
var
  e1, e2: TVector3f;
begin


//  WriteAppLog('CalcTriNorm',AppLog);
//  WriteAppLog(Format('v1 x: %1.1f y: %1.1f z: %1.1f',[v1[0],v1[1],v1[2]]),AppLog);
//  WriteAppLog(Format('v2 x: %1.1f y: %1.1f z: %1.1f',[v2[0],v2[1],v2[2]]),AppLog);
//  WriteAppLog(Format('v3 x: %1.1f y: %1.1f z: %1.1f',[v3[0],v3[1],v3[2]]),AppLog);

  e1 := VectorSubtract(PVector3f(@v2)^,PVector3f(@v1)^);
  e2 := VectorSubtract(PVector3f(@v3)^,PVector3f(@v1)^);

//  WriteAppLog(Format('e1 x: %1.1f y: %1.1f z: %1.1f',[e1[0],e1[1],e1[2]]),AppLog);
//  WriteAppLog(Format('e2 x: %1.1f y: %1.1f z: %1.1f',[e2[0],e2[1],e2[2]]),AppLog);

  VectorCrossProduct(e1,e2,result);

  result := VectorNormalize(result);

//  WriteAppLog(Format('normal x: %1.1f y: %1.1f z: %1.1f',[result[0],result[1],result[2]]),AppLog);


end;

type
  TNormBuffer = record
    count,lasttag: TU2;
  end;
  TNormBufferDynArray = array of TNormBuffer;

{ TGLLWOVectorFile }

{
******************************* TGLLWOVectorFile *******************************
}
procedure TGLLWOVectorFile.AddLayr(Layr: TLWLayr; LWO: TLWObjectFile);
var
  Idx: Integer;
  Mesh: TMeshObject;
  Pnts: TLWPnts;
begin
  // Add mesh
  Mesh := TMeshObject.CreateOwned(Owner.MeshObjects);

  with Mesh do
  begin
    Name := Layr.Name;
    Mode := momFaceGroups;

    // pnts
    Idx := Layr.Items.FindChunk(FindByChunkId,@ID_PNTS);

    Pnts := TLWPnts(Layr.Items[Idx]);

    if Idx <> -1 then
      AddPnts(Pnts,Mesh);

    // vertex maps
    Idx := TLWPnts(Layr.Items[Idx]).Items.FindChunk(FindByChunkId,@ID_VMAP);

    while Idx <> -1 do
    begin
      AddVMap(TLWVMap(Pnts.Items[Idx]),Mesh);
      Idx := Pnts.Items.FindChunk(FindByChunkId,@ID_VMAP,Idx + 1);
    end;

    // Polygons
    Idx := Layr.Items.FindChunk(FindByChunkId,@ID_POLS);
    while Idx <> -1 do
    begin
      AddPols(TLWPols(Layr.Items[Idx]),Mesh);
      Idx := Layr.Items.FindChunk(FindByChunkId,@ID_POLS, Idx + 1);
    end;
//    Normals.Normalize;
  end;
  FPnts := nil;
end;

procedure TGLLWOVectorFile.AddPnts(Pnts: TLWPnts; Mesh: TMeshObject);
var
  i: Integer;
begin

  FPnts := Pnts;
  with Mesh do
  begin
    Vertices.Capacity := Pnts.PntsCount;
    TexCoords.Capacity := Pnts.PntsCount;
    TexCoords.AddNulls(Pnts.PntsCount);

    for i := 0 to Pnts.PntsCount - 1 do
        Vertices.Add(PAffineVector(@Pnts.Pnts[i])^);

  end;
end;

procedure TGLLWOVectorFile.AddPols(Pols: TLWPols; Mesh: TMeshObject);
var
  Idx: Integer;
  i,j,k, PolyIdx, NormIdx: Integer;
  TagPolys: TU2DynArray;
  FaceGrp: TFGVertexNormalTexIndexList;
  VertPolys: TU2DynArray;
begin
   SetLength(VertPolys, 0);
  with Pols do
  begin
    // face type pols chunk
    if PolsType = POLS_TYPE_FACE then
    begin

      Idx := Items.FindChunk(FindByChunkId,@ID_PTAG);
      while Idx <> -1 do
      begin
        with TLWPTag(Items[Idx]) do
        begin

          if MapType = PTAG_TYPE_SURF then
          begin

            // for each tag
            for i := 0 to TagCount - 1 do
            begin

              // get polygons using this tag
              if GetPolsByTag(Tags[i],TagPolys) > 0 then
              begin
                // make the facegroup and set the material name
                FaceGrp := TFGVertexNormalTexIndexList.CreateOwned(Mesh.FaceGroups);
                FaceGrp.MaterialName := FLWO.SurfaceByTag[Tags[i]].Name;
                FaceGrp.Mode := fgmmTriangles;

                // for each polygon in the current surface Tags[i]
                for j := 0 to Length(TagPolys) - 1 do
                begin
                  PolyIdx := PolsByIndex[TagPolys[j]];
                  // is it a triangle?
                  if (Indices[PolyIdx] = 3) then
                  begin
                    for k := 1 to 3 do
                    begin
                      NormIdx := Mesh.Normals.Add(PVector3f(@PolsInfo[TagPolys[j]].vnorms[k-1])^);
                      FaceGrp.Add(Indices[PolyIdx + k],NormIdx,0);
                    end;
                  end;

                end;

                SetLength(TagPolys,0);

              end;

            end;

          end else
          if MapType = PTAG_TYPE_PART then
          begin
            {Todo: PTag PART}


          end else

          if MapType = PTAG_TYPE_SMGP then
          begin
            {Todo: PTag Smooth Group}

          end;
          Idx :=  Items.FindChunk(FindByChunkId,@ID_PTAG,Idx + 1);
        end;
      end;
    end else

    // curv type pols chunk (catmull-rom splines)
    if PolsType = POLS_TYPE_CURV then
    begin
      {Todo: CURV Pols import}

    end else

    // nurbs patch pols type chunk
    if PolsType = POLS_TYPE_PTCH then
    begin
      {Todo: NURBS Patch Pols import}

    end else

    // metaball pols type chunk
    if PolsType = POLS_TYPE_MBAL then
    begin
      {Todo: MetaBall type Pols import}

    end else

    // bone pols type chunk
    if PolsType = POLS_TYPE_BONE then
    begin
      {Todo: Bone Pols import}

    end;
    SetLength(TagPolys,0);
  end;
end;

procedure TGLLWOVectorFile.AddSurf(Surf: TLWSurf; LWO: TLWObjectFile);
var
  matLib: TGLMaterialLibrary;
  libMat: TGLLibMaterial;
  pdata: Pointer;
  colr: TColr12;
  FloatParm,tran: TF4;
  WordParm: TU2;

begin
  {Todo: implement surface inheritance}

  if GetOwner is TGLBaseMesh then
  begin
    matLib:=TGLBaseMesh(GetOwner).MaterialLibrary;

    if Assigned(matLib) then
    begin

      libMat:=matLib.Materials.GetLibMaterialByName(Surf.Name);

      if not Assigned(libMat) then
      begin

        libMat:=matLib.Materials.Add;
        libMat.Name:=Surf.Name;


        with libMat.Material.FrontProperties do
        begin

          pdata := Surf.ParamAddr[ID_TRAN];

          if pdata <> nil then
          begin
            tran := PF4(pdata)^;
            ReverseByteOrder(@tran,4,1);
          end else
            tran := 0;

          if tran <> 0 then
            libMat.Material.BlendingMode := bmTransparency;

          pdata := Surf.ParamAddr[ID_COLR];

          if pdata <> nil then
          begin

            colr := PColr12(pdata)^;
            ReverseByteOrder(@colr[0],4,3);

          end else
          begin
            colr[0] := 0.60;
            colr[1] := 0.60;
            colr[2] := 0.60;
          end;

//          Ambient.Color := VectorMake(colr[0],colr[1],colr[2],1);
          Ambient.Color := VectorMake(0,0,0,1);


          (* Diffuse *)
          pdata := Surf.ParamAddr[ID_DIFF];

          if pdata <> nil then
          begin

            FloatParm := PF4(pdata)^;
            ReverseByteOrder(@FloatParm,4,1);

          end else

            FloatParm := 1.0;

          Diffuse.Color:=VectorMake(colr[0] * FloatParm,colr[1] * FloatParm,colr[2] * FloatParm,tran);

          (* Luminosity -> Emission *)
          pdata := Surf.ParamAddr[ID_LUMI];

          if pdata <> nil then
          begin

            FloatParm := PF4(pdata)^;
            ReverseByteOrder(@FloatParm,4,1);

          end else

            FloatParm := 0.0;

          Emission.Color:=VectorMake(colr[0] * FloatParm,colr[1] * FloatParm, colr[2] * FloatParm, 1);


          (* Specularity *)
          pdata := Surf.ParamAddr[ID_SPEC];
          if pdata <> nil then
          begin

            FloatParm := PF4(pdata)^;
            ReverseByteOrder(@FloatParm,4,1);

          end else

            FloatParm := 0.8;

          Specular.Color:=VectorMake(colr[0] * FloatParm,colr[1] * FloatParm, colr[2] * FloatParm, 1);

          pdata := Surf.ParamAddr[ID_GLOS];

          if pdata <> nil then
          begin

            FloatParm := PF4(pdata)^;
            ReverseByteOrder(@FloatParm,4,1);

          end else

            FloatParm := 0.4;

          Shininess := Round(Power(2,7*FloatParm));

          pdata := Surf.ParamAddr[ID_SIDE];

          if pdata <> nil then
          begin
            WordParm := PU2(pdata)^;
            ReverseByteOrder(@FloatParm,2,1);
          end else
            WordParm := 0;

          if (WordParm and SIDE_BACK) = SIDE_BACK then
            AssignTo(libMat.Material.BackProperties);



//          WriteAppLog(Format('%s Surface imported',[Surf.Name]),AppLog);
//          WriteAppLog(Format('Ambient: r: %1.1f, g: %1.1f, b:%1.1f',[Ambient.Color[0],Ambient.Color[1],Ambient.Color[2]]),AppLog);
//          WriteAppLog(Format('Diffuse: r: %1.1f, g: %1.1f, b:%1.1f',[Diffuse.Color[0],Diffuse.Color[1],Diffuse.Color[2]]),AppLog);
//          WriteAppLog(Format('Specular: r: %1.1f, g: %1.1f, b:%1.1f',[Specular.Color[0],Specular.Color[1],Specular.Color[2]]),AppLog);
//          WriteAppLog(Format('Emission: r: %1.1f, g: %1.1f, b:%1.1f',[Emission.Color[0],Emission.Color[1],Emission.Color[2]]),AppLog);
//          WriteAppLog(Format('Glossiness: %d',[Shininess]),AppLog);


        end;

      end;

    end;

  end;

end;

procedure TGLLWOVectorFile.AddVMap(VMap: TLWVMap; Mesh: TMeshObject);
var
  i: integer;
begin

  with VMap, Mesh do
  begin

    // texture coords
    if VMapType = VMAP_TYPE_TXUV then
    begin

      for i := 0 to ValueCount - 1 do
        TexCoords.Items[Value[i].vert] := AffineVectorMake(Value[i].values[0], Value[i].values[1], 0);
  
    end else
  
    // vertex weight map
    if VMapType = VMAP_TYPE_WGHT then
    begin
      {Todo: WeightMap import}
  
    end else
  
    // vertex morph (relative)
    if VMapType = VMAP_TYPE_MORF then
    begin
      {Todo: Morph target (relative) import}
  
  
    end else
  
    // vertex morph (absolute)
    if VMapType = VMAP_TYPE_SPOT then
    begin
      {Todo: Morph target (absolute) import}
  
  
    end;

  end;
end;

procedure TGLLWOVectorFile.LoadFromStream(aStream: TStream);
var
  Ind: Integer;
begin
  FLWO := TLWObjectFile.Create;
  
  with FLWO do
  try
  
    LoadFromStream(aStream);
  
    // Add Surfaces to material list
    Ind := Chunks.FindChunk(FindByChunkId,@ID_SURF,0);

    while Ind <> -1 do
    begin

      AddSurf(TLWSurf(Chunks[Ind]), FLWO);
  
      Ind := Chunks.FindChunk(FindByChunkId,@ID_SURF,Ind + 1);

    end;
  
    // Lw layer
    Ind := Chunks.FindChunk(FindByChunkId,@ID_LAYR,0);
  
    while Ind <> -1 do
    begin

      AddLayr(TLWLayr(Chunks[Ind]),FLWO);
  
      Ind := Chunks.FindChunk(FindByChunkId,@ID_LAYR,Ind + 1);
  
    end;
  
  finally
  
    FreeAndNil(FLWO);
  
  end;
end;

initialization
   RegisterVectorFileFormat('lwo', 'Lightwave3D object file (6.0 or above)', TGLLWOVectorFile);

finalization


end.
