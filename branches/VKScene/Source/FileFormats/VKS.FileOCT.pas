//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
    Support-code to load OCT Files into TVKFreeForm-Components in GLScene.
    (OCT being the format output from FSRad, http://www.fluidstudios.com/fsrad.html).

}
unit VKS.FileOCT;

interface

{$I VKScene.inc}

uses
  System.SysUtils,
  System.Classes,
  //VKS
  VKS.VectorFileObjects, VKS.VectorGeometry,
  VKS.ApplicationFileIO,
  FileOCT;

type

  // TVKOCTVKVectorFile

  { The OCT vector file (FSRad output).  }
  TVKOCTVKVectorFile = class(TVKVectorFile)
  public
    { Public Declarations }
    class function Capabilities: TVKDataFileCapabilities; override;

    procedure LoadFromStream(aStream: TStream); override;
  end;

var
  vGLFileOCTLightmapBrightness: single = 1;
  // Mrqzzz : scaling factor, 1.0 = unchanged
  vGLFileOCTLightmapGammaCorrection: single = 1;
  // Mrqzzz : scaling factor, 1.0 = unchanged
  vGLFileOCTAllocateMaterials: boolean = True;
// Mrqzzz : Flag to avoid loading materials (useful for IDE Extensions or scene editors)

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  VKS.Texture, VKS.Material, VKS.Graphics, VKS.CrossPlatform, VKS.State,
  VKS.Utils, VKS.TextureFormat;

// ------------------
// ------------------ TVKOCTVKVectorFile ------------------
// ------------------

// Capabilities

class function TVKOCTVKVectorFile.Capabilities: TVKDataFileCapabilities;
begin
  Result := [dfcRead];
end;

// LoadFromStream

procedure TVKOCTVKVectorFile.LoadFromStream(aStream: TStream);
var
  i, y, n: integer;
  oct: TOCTFile;
  octFace: POCTFace;
  octLightmap: POCTLightmap;
  mo: TVKMeshObject;
  fg: TFGVertexIndexList;
  lightmapLib: TVKMaterialLibrary;
  lightmapBmp: TVKBitmap;
  libMat: TVKLibMaterial;
begin
  oct := TOCTFile.Create(aStream);
  try
    mo := TVKMeshObject.CreateOwned(Owner.MeshObjects);
    mo.Mode := momFaceGroups;

    lightmapLib := Owner.LightmapLibrary;
    if (Assigned(lightmapLib)) and (vGLFileOCTAllocateMaterials) then
    begin
      // import lightmaps
      n := oct.Header.numLightmaps;
      lightmapBmp := TVKBitmap.Create;
      try
        { TODO : E2129 Cannot assign to a read-only property }
        (*lightmapBmp.PixelFormat := glpf24bit;*)
        lightmapBmp.Width := 128;
        lightmapBmp.Height := 128;
        for i := 0 to n - 1 do
        begin
          octLightmap := @oct.Lightmaps[i];
          // Brightness correction
          if vGLFileOCTLightmapBrightness <> 1.0 then
            BrightenRGBArray(@octLightmap.map,
              lightmapBmp.Width * lightmapBmp.Height,
              vGLFileOCTLightmapBrightness);
          // Gamma correction
          if vGLFileOCTLightmapGammaCorrection <> 1.0 then
            GammaCorrectRGBArray(@octLightmap.map,
              lightmapBmp.Width * lightmapBmp.Height,
              vGLFileOCTLightmapGammaCorrection);
          // convert RAW RGB to BMP
          for y := 0 to 127 do
          { TODO : E2003 Undeclared identifier: 'ScanLine' }
          (*
          Move(octLightmap.map[y * 128 * 3],
             lightmapBmp.ScanLine[127 - y]^, 128 * 3);
          *)
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

    // import geometry
    n := oct.Header.numVerts;

    mo.Vertices.AdjustCapacityToAtLeast(n);
    mo.TexCoords.AdjustCapacityToAtLeast(n);
    mo.LightMapTexCoords.AdjustCapacityToAtLeast(n);
    for i := 0 to n - 1 do
      with oct.Vertices[i] do
      begin
        mo.Vertices.Add(pos.X, pos.Y, pos.Z);
        mo.TexCoords.Add(tv.s, tv.t);
        mo.LightMapTexCoords.Add(lv.s, lv.t);
      end;
    // import faces
    n := oct.Header.numFaces;
    for i := 0 to n - 1 do
    begin
      octFace := @oct.Faces[i];
      fg := TFGVertexIndexList.CreateOwned(mo.FaceGroups);
      fg.Mode := fgmmTriangleFan;
      fg.VertexIndices.AddSerie(octFace.start, 1, octFace.num);
      if (Assigned(lightmapLib)) and (vGLFileOCTAllocateMaterials) then
        fg.LightMapIndex := octFace.lid;
    end;

  finally
    oct.Free;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterVectorFileFormat('oct', 'FSRad OCT files', TVKOCTVKVectorFile);

end.

