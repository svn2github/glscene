//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
    Support-code to load OCT Files into TVXFreeForm-Components in GLScene.
    (OCT being the format output from FSRad, http://www.fluidstudios.com/fsrad.html).

}
unit VXS.FileOCT;

interface

{$I VXScene.inc}

uses
  System.SysUtils,
  System.Classes,
  FMX.Graphics,

  VXS.VectorFileObjects,
  VXS.VectorGeometry,
  VXS.ApplicationFileIO,
  VXS.Texture,
  VXS.Material,
  VXS.Graphics,
  VXS.CrossPlatform,
  VXS.State,
  VXS.Utils,
  VXS.TextureFormat,

  uFileOCT;

type

  { The OCT vector file (FSRad output).  }
  TVXOCTVXVectorFile = class(TVXVectorFile)
  public
    class function Capabilities: TVXDataFileCapabilities; override;
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
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TVXOCTVXVectorFile ------------------
// ------------------

// Capabilities

class function TVXOCTVXVectorFile.Capabilities: TVXDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TVXOCTVXVectorFile.LoadFromStream(aStream: TStream);
var
  i, y, n: integer;
  oct: TOCTFile;
  octFace: POCTFace;
  octLightmap: POCTLightmap;
  mo: TVXMeshObject;
  fg: TFGVertexIndexList;
  lightmapLib: TVXMaterialLibrary;
  lightmapBmp: TBitmap;
  libMat: TVXLibMaterial;
begin
  oct := TOCTFile.Create(aStream);
  try
    mo := TVXMeshObject.CreateOwned(Owner.MeshObjects);
    mo.Mode := momFaceGroups;

    lightmapLib := Owner.LightmapLibrary;
    if (Assigned(lightmapLib)) and (vGLFileOCTAllocateMaterials) then
    begin
      // import lightmaps
      n := oct.Header.numLightmaps;
      lightmapBmp := TBitmap.Create;
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

  RegisterVectorFileFormat('oct', 'FSRad OCT files', TVXOCTVXVectorFile);

end.

