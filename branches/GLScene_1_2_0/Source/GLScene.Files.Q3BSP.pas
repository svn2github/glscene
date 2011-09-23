//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLFileQ3BSP<p>

  Support-code to load Q3BSP Files into TGLFreeForm-Components in GLScene.<p>
  Note that you must manually add this unit to one of your project's uses
  to enable support for OBJ & OBJF at run-time.<p>

  <b>History : </b><font size=-1><ul>
  <li>06/06/10 - Yar - Added VectorTypes to uses
  <li>22/01/10 - Yar - Added GLTextureFormat to uses
  <li>31/03/07 - DaStr - Added $I GLScene.inc
  <li>31/01/03 - EG - Materials support
  <li>30/01/03 - EG - Creation
  </ul><p>
}
unit GLScene.Files.Q3BSP;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene.Vector.FileObjects,
  GLScene.Base.FileIO;

type

  // TGLQ3BSPVectorFile
  //
  { : The Q3BSP vector file (Quake III BSP).<p> }
  TGLQ3BSPVectorFile = class(TVectorFile)
  public
    { Public Declarations }
    class function Capabilities: TDataFileCapabilities; override;

    procedure LoadFromStream(aStream: TStream); override;
  end;

var
  // Q3 lightmaps are quite dark, we brighten them a lot by default
  vQ3BSPLightmapGammaCorrection: Single = 2.5;
  vQ3BSPLightmapBrightness: Single = 2; // scaling factor, 1.0 = unchanged
  vGLFileQ3BSPLoadMaterials: boolean = True;
  // Mrqzzz : Flag to avoid loading materials (useful for IDE Extentions like GlaredX)

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  GLScene.Files.Q3BSP.Types,
  GLScene.Base.Vector.Geometry,
  GLScene.Base.Vector.Types,
  GLScene.Base.Vector.Lists,
  SysUtils,
  GLScene.BSP,
  GLScene.Texture,
  GLScene.Graphics,
  GLScene.Platform,
  GLScene.Base.GLStateMachine,
  GLScene.Utils,
  GLScene.Material,
  GLScene.Texture.Format;

// ------------------
// ------------------ TGLSTLVectorFile ------------------
// ------------------

// Capabilities
//

class function TGLQ3BSPVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

// LoadFromStream
//

procedure TGLQ3BSPVectorFile.LoadFromStream(aStream: TStream);

  function LocateTextureFile(const texName: string): string;
  begin
    if FileStreamExists(texName + '.bmp') then
      Result := texName + '.bmp'
    else if FileStreamExists(texName + '.jpg') then
      Result := texName + '.jpg'
    else if FileStreamExists(texName + '.tga') then
      Result := texName + '.tga'
    else
      Result := '';
  end;

  function GetOrAllocateMaterial(const matName: string): string;
  var
    matLib: TGLMaterialLibrary;
    libMat: TGLLibMaterial;
    texName: string;
  begin
    if GetOwner is TGLBaseMesh then
    begin
      // got a linked material library?
      matLib := TGLBaseMesh(GetOwner).MaterialLibrary;
      if Assigned(matLib) then
      begin
        Result := matName;
        libMat := matLib.Materials.GetLibMaterialByName(matName);
        if not Assigned(libMat) then
        begin
          if Pos('.', matName) < 1 then
          begin
            texName := LocateTextureFile(matName);
            if texName = '' then
              texName := LocateTextureFile
                (Copy(matName, LastDelimiter('\/', matName) + 1, MaxInt));
          end
          else
            texName := matName;
          with matLib.AddTextureMaterial(matName, texName) do
          begin
            Material.Texture.TextureMode := tmModulate;
          end;
        end;
      end
      else
        Result := '';
    end
    else
      Result := '';
  end;

var
  BSP: TQ3BSP;
  mo: TBSPMeshObject;
  fg, lastfg: TFGBSPNode;
  i, j, n, y: Integer;
  facePtr: PBSPFace;
  lightmapLib: TGLMaterialLibrary;
  lightmapBmp: TGLBitmap;
  libMat: TGLLibMaterial;
  bspLightMap: PBSPLightmap;
  plane: THmgPlane;
begin
  BSP := TQ3BSP.Create(aStream);
  try
    mo := TBSPMeshObject.CreateOwned(Owner.MeshObjects);

    // import all materials
    if vGLFileQ3BSPLoadMaterials then
    begin
      for i := 0 to High(BSP.Textures) do
      begin
        GetOrAllocateMaterial
          (Trim(string(StrPas(BSP.Textures[i].TextureName))));
      end;
    end;

    // import all lightmaps
    lightmapLib := Owner.LightmapLibrary;
    if Assigned(lightmapLib) and vGLFileQ3BSPLoadMaterials then
    begin
      // import lightmaps
      n := BSP.NumOfLightmaps;
      lightmapBmp := TGLBitmap.Create;
      try
        lightmapBmp.PixelFormat := glpf24bit;
        lightmapBmp.Width := 128;
        lightmapBmp.Height := 128;
        for i := 0 to n - 1 do
        begin
          bspLightMap := @BSP.Lightmaps[i];
          // apply brightness correction if ant
          if vQ3BSPLightmapBrightness <> 1 then
            BrightenRGBArray(@bspLightMap.imageBits[0], 128 * 128,
              vQ3BSPLightmapBrightness);
          // apply gamma correction if any
          if vQ3BSPLightmapGammaCorrection <> 1 then
            GammaCorrectRGBArray(@bspLightMap.imageBits[0], 128 * 128,
              vQ3BSPLightmapGammaCorrection);
          // convert RAW RGB to BMP
          for y := 0 to 127 do
            BGR24ToRGB24(@bspLightMap.imageBits[y * 128 * 3],
              lightmapBmp.ScanLine[127 - y], 128);
          // spawn lightmap
          libMat := lightmapLib.AddTextureMaterial(IntToStr(i), lightmapBmp);
          with libMat.Material.Texture do
          begin
            MinFilter := miLinear;
            TextureWrap := twNone;
            TextureFormat := tfRGB;
          end;
        end;
      finally
        lightmapBmp.Free;
      end;
    end;

    // import all geometry
    mo.Vertices.AdjustCapacityToAtLeast(BSP.NumOfVerts);
    mo.Normals.AdjustCapacityToAtLeast(BSP.NumOfVerts);
    mo.TexCoords.AdjustCapacityToAtLeast(BSP.NumOfVerts);
    for i := 0 to BSP.NumOfVerts - 1 do
    begin
      mo.Vertices.Add(BSP.Vertices[i].Position);
      mo.Normals.Add(BSP.Vertices[i].Normal);
      mo.TexCoords.Add(BSP.Vertices[i].TextureCoord);
      if Assigned(lightmapLib) and vGLFileQ3BSPLoadMaterials then
        mo.LightMapTexCoords.Add(BSP.Vertices[i].LightmapCoord)
    end;
    mo.TexCoords.Scale(AffineVectorMake(1, -1, 0));
    mo.TexCoords.Translate(YVector);
    mo.RenderSort := rsBackToFront;
    // Q3 BSP separates tree nodes from leafy nodes, we don't,
    // so we place nodes first, then all leafs afterwards
    for i := 0 to BSP.NumOfNodes - 1 do
    begin
      fg := TFGBSPNode.CreateOwned(mo.FaceGroups);
      plane := BSP.Planes[BSP.Nodes[i].plane];
      plane := VectorMake(plane[0], plane[1], plane[2], plane[3]);
      fg.SplitPlane := plane;
      fg.PositiveSubNodeIndex := BSP.Nodes[i].Children[0];
      if fg.PositiveSubNodeIndex < 0 then
        fg.PositiveSubNodeIndex := BSP.NumOfNodes - fg.PositiveSubNodeIndex - 1;
      Assert(fg.PositiveSubNodeIndex < BSP.NumOfNodes + BSP.NumOfLeaves);
      Assert(fg.PositiveSubNodeIndex > 0);
      fg.NegativeSubNodeIndex := BSP.Nodes[i].Children[1];
      if fg.NegativeSubNodeIndex < 0 then
        fg.NegativeSubNodeIndex := BSP.NumOfNodes - fg.NegativeSubNodeIndex - 1;
      Assert(fg.NegativeSubNodeIndex < BSP.NumOfNodes + BSP.NumOfLeaves);
      Assert(fg.NegativeSubNodeIndex > 0);
    end;
    // import all leaves
    for i := 0 to BSP.NumOfLeaves - 1 do
      TFGBSPNode.CreateOwned(mo.FaceGroups);
    // import all faces into leaves & subnodes
    for i := 0 to BSP.NumOfLeaves - 1 do
    begin
      lastfg := nil;
      for j := 0 to BSP.Leaves[i].NumFaces - 1 do
      begin
        n := BSP.Leaves[i].FirstFace + j;
        if n >= BSP.NumOfFaces then
          Break; // corrupted BSP?
        facePtr := @BSP.Faces[n];
        if facePtr.FaceType = FACE_POLYGON then
        begin
          if lastfg = nil then
            fg := TFGBSPNode(mo.FaceGroups[i + BSP.NumOfNodes])
          else
          begin
            lastfg.PositiveSubNodeIndex := mo.FaceGroups.Count;
            fg := TFGBSPNode.CreateOwned(mo.FaceGroups);
          end;
          // check for BSP corruption
          if Cardinal(facePtr.textureID) <= Cardinal(BSP.NumOfTextures) then
            fg.MaterialName :=
              Trim(string(StrPas(BSP.Textures[facePtr.textureID].TextureName)));
          if Assigned(lightmapLib) and vGLFileQ3BSPLoadMaterials then
            fg.LightMapIndex := facePtr.lightmapID;
          lastfg := fg;
          // Q3 Polygon Faces are actually fans, but winded the other way around!
          fg.Mode := fgmmTriangleFan;
          fg.VertexIndices.Add(facePtr.startVertIndex);
          fg.VertexIndices.AddSerie(facePtr.startVertIndex + facePtr.NumOfVerts
            - 1, -1, facePtr.NumOfVerts - 1);
          // there are also per-leaf mesh references... dunno what they
          // are for, did not encounter them so far... If you have a BSP
          // that has some, and if you know how to make use of them, shout!

          // Copy the cluster index, used for visibility determination
          fg.Cluster := BSP.Leaves[i].Cluster;
        end;
      end;
    end;

    // Copy the visibility data
    if BSP.VisData.numOfClusters > 0 then
      mo.ClusterVisibility.SetData(@BSP.VisData.bitSets[0],
        BSP.VisData.numOfClusters);
  finally
    BSP.Free;
  end;
  // Some BSP end up with empty nodes/leaves (information unused, incorrept BSP...)
  // This call takes care of cleaning up all the empty nodes
  mo.CleanupUnusedNodes;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

RegisterVectorFileFormat('q3bsp', 'Quake3 BSP files', TGLQ3BSPVectorFile);

// registering this extension too might be a little abusive right now...
RegisterVectorFileFormat('bsp', 'BSP files', TGLQ3BSPVectorFile);

end.
