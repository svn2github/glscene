//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFPSMovement<p>

   FPS-like movement behaviour and manager.<p>

	<b>History : </b><font size=-1><ul>	
      <li>09/06/04 - Mathx - Addition to GLScene (created by Kenneth Poulter)
	</ul></font>
}

{
   Author : Kenneth Poulter (aka SpiriT aka Difacane)
   Base : none, apart from glscene materiallibrary

   Description : Material Script Batch loader for GLScene's TGLMaterialLibrary for runtime purposes usually

   History :
   26/06/2004 - KP - started basic script idea using repeat statements
   26/06/2004 - KP - script is now half functional and method proved to be effective
   27/06/2004 - KP - finished script, but not dynamic, error handling needs some work
   28/06/2004 - KP - cleaned it all up, nearly ready for realease
   29/06/2004 - KP - Converted to a component class, ready for release

   Notes :
   1.ImageClass Script is theoritecally correct, hasn't been tested yet, only Persistent image has been checked.
   2.Keep to the guidelines of the script with syntax involved etc, meaning, don't bash my scripter around :P
   3.The script isn't very dynamic at all, but works if used correctly, follow the demos and sure to work
   4.Reference to materiallibraries in script isn't implemented yet, need collection classes
   5.Reference to shaders in script isn't implemented yet, need collection classes

   Future notes :
   Implementation of variables
   Implementation of constants
   Effects addition (already in source, but not implemented, have to write an "effectslibrary"

   This source falls under the GNU GPL license, unless stated otherwise by the author(Kenneth Poulter).

   Additions are welcome

}

unit GLMaterialScript;

interface

uses
  Windows, Messages, SysUtils, Classes, jpeg, tga, StdCtrls, GLTexture, GLMisc,
  ExtCtrls, GLUtils;

type
   TGLMaterialScripter = class(TComponent)
   protected
   { Protected declarations }

      FScript : TStrings;
      FMemo: TMemo;
      FMaterialLibrary: TGLMaterialLibrary;

      Count  : longint;
      infini : longint;
      done : boolean;

      NewMat : TGLLibMaterial;

      tmpcoords : TGLCoordinates;
      tmpcolor : TGLColor;
      tmpcoords4 : TGLCoordinates4;
      tmpstr : string;

      procedure SetScript(const Value: TStrings);
      procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
      procedure SetMemo(const Value: TMemo);

      // error checking
      procedure CheckError;
      function  ClassExists(arguement : string) : boolean;
      function  CheckRepeatDone : boolean;
      // extraction functions
      function  ExtractValue : string;
      procedure ExtractCoords3;
      procedure ExtractCoords4;
      procedure ExtractColors;
      function  DeleteSpaces(value : string) : string;
      function  SubstrExists(substr : string) : boolean;
      function  ValueExists(value : string) : boolean;
      // these are our viable scripts
      procedure ZMaterial;
      procedure ZEffect;
      // internally called scripts for value extraction
      procedure XMaterial;
      procedure XName;
      procedure XShader;
      procedure XTexture2Name;
      procedure XTextureOffset;
      procedure XTextureScale;
      procedure XTexture;
      procedure XCompression;
      procedure XEnvColor;
      procedure XFilteringQuality;
      procedure XImageAlpha;
      procedure XImageBrightness;
      procedure XImageClass;
      procedure XImageGamma;
      procedure XMagFilter;
      procedure XMappingMode;
      procedure XMappingSCoordinates;
      procedure XMappingTCoordinates;
      procedure XMinFilter;
      procedure XNormalMapScale;
      procedure XTextureFormat;
      procedure XTextureMode;
      procedure XTextureWrap;
      procedure XBlendingMode;
      procedure XFacingCulling;
      procedure XLibMaterialName;
      procedure XMaterialOptions;
      procedure XMaterialLibrary;
      procedure XBackProperties;
      procedure XBackAmbient;
      procedure XBackDiffuse;
      procedure XBackEmission;
      procedure XBackPolygonMode;
      procedure XBackShininess;
      procedure XBackSpecular;
      procedure XFrontProperties;
      procedure XFrontAmbient;
      procedure XFrontDiffuse;
      procedure XFrontEmission;
      procedure XFrontPolygonMode;
      procedure XFrontShininess;
      procedure XFrontSpecular;
      procedure XPersistantImage;
      procedure XBlankImage;
      procedure XPictureFileName;
      procedure XPicturePX;
      procedure XPictureNX;
      procedure XPicturePY;
      procedure XPictureNY;
      procedure XPicturePZ;
      procedure XPictureNZ;
   private
   { Private declarations }


   public
   { Public declarations }

      property DebugMemo : TMemo read FMemo write SetMemo;
      constructor Create(AOwner : TComponent); override;
      destructor  Destroy; override;

      procedure CompileScript;

   published
   { Published declarations }
      property Script : TStrings read FScript write SetScript;
      property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;

   end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GLScene Utils', [TGLMaterialScripter]);
end;

procedure TGLMaterialScripter.CompileScript;
begin

   done := false;
   NewMat := nil;
   count := 0;
   infini := 0;
   tmpcoords := nil;
   tmpcoords4 := nil;
   tmpcolor := nil;
   tmpstr := '';

   repeat

      inc(count);

      if pos('{',FScript.Strings[count]) > 0 then
      begin

         if substrexists('material') then ZMaterial;
         if substrexists('effect') then ZEffect;

      end;
      checkerror;

   until checkrepeatdone;

end;

procedure TGLMaterialScripter.SetMaterialLibrary(
  const Value: TGLMaterialLibrary);
begin
  FMaterialLibrary := Value;
end;

procedure TGLMaterialScripter.SetMemo(const Value: TMemo);
begin
  FMemo := Value;
end;

procedure TGLMaterialScripter.SetScript(const Value: TStrings);
begin
  FScript := Value;
end;



procedure TGLMaterialScripter.CheckError;
begin
   if count >= FScript.Count then done := true;
   if done then raise exception.Create('User Error : No closing "}"');
   inc(infini);
   if infini > 1280000 then
   begin
      raise exception.Create('Internal Error : Infinate Loop');
      done := true;
      exit;
   end;
end;

function TGLMaterialScripter.CheckRepeatDone: boolean;
begin
   checkrepeatdone := false;
   if pos('}',FScript.Strings[count]) > 0 then
   begin
      checkrepeatdone := true;
      inc(count);
   end;

   if done then checkrepeatdone := true;
end;

function TGLMaterialScripter.ClassExists(arguement: string): boolean;
var temp : string;
    i : word;
begin

   classexists := false;
   if (pos(uppercase(arguement), uppercase(FScript.Strings[count])) > 0) and // check if there is an arguement
      (pos('=', FScript.Strings[count]) > pos(uppercase(arguement), uppercase(FScript.Strings[count])))and // check if it is before '='
      (pos('=', FScript.Strings[count]) > 0) then // check if there even is a '='
      begin

         temp := FScript.Strings[count];
         for i := 0 to length(temp) do
            if pos(' ', temp) = 1 then
            delete(temp,1,1);

         if pos(uppercase(arguement),uppercase(temp)) = 1 then
         if (temp[length(arguement) + 1] = ' ') or (temp[length(arguement) + 1] = '=') then
         begin
            classexists := true;
            if assigned(FMemo) then Fmemo.Lines.Add('Stage is at : ' + arguement);
         end;
      end;

end;
function TGLMaterialScripter.SubstrExists(substr: string): boolean;
begin
   if pos(uppercase(substr),uppercase(FScript.Strings[count])) > 0 then result := true
   else
   result := false;
end;


constructor TGLMaterialScripter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScript := TStringList.Create;
end;

function TGLMaterialScripter.DeleteSpaces(value: string): string;
var i : byte;
begin
   result := value;

   for i := 0 to length(result) do
   if pos(' ',result) > 0 then
   delete(result,pos(' ',result), 1);
end;

destructor TGLMaterialScripter.Destroy;
begin
  inherited Destroy;
end;

procedure TGLMaterialScripter.ExtractColors;
var val : string;
begin
   val := Extractvalue;
   if pos('(',val) > 0 then
   begin
      tmpcolor.Alpha := strtofloatDef(copy(val, pos('(',val) + 1, pos(';',val) - 2));
      delete(val,1,pos(';',val));
      tmpcolor.Red := strtofloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcolor.Green := strtofloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcolor.Blue := strtofloatDef(copy(val, 1, pos(')',val) - 1));
   end;
end;

procedure TGLMaterialScripter.ExtractCoords3;
var val : string;
begin
   val := Extractvalue;
   if pos('(',val) > 0 then
   begin
      tmpcoords.X := strtofloatDef(copy(val, pos('(',val) + 1, pos(';',val) - 2));
      delete(val,1,pos(';',val));
      tmpcoords.Y := strtofloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcoords.Z := strtofloatDef(copy(val, 1, pos(')',val) - 1));
   end;
end;

procedure TGLMaterialScripter.ExtractCoords4;
var val : string;
begin
   val := Extractvalue;
   if pos('(',val) > 0 then
   begin
      tmpcoords4.W := strtofloatDef(copy(val, pos('(',val) + 1, pos(';',val) - 2));
      delete(val,1,pos(';',val));
      tmpcoords4.X := strtofloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcoords4.Y := strtofloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcoords4.Z := strtofloatDef(copy(val, 1, pos(')',val) - 1));
   end;
end;

function TGLMaterialScripter.ExtractValue: string;
begin
   extractvalue := copy(FScript.Strings[count], pos('=',FScript.Strings[count]) + 1, length(FScript.Strings[count]) - pos('=',FScript.Strings[count]));
end;
procedure TGLMaterialScripter.XPersistantImage;
begin
   if classexists('file') then
   begin
      if (extractvalue <> '') and (fileexists(extractvalue)) then
      begin
         with NewMat.Material.Texture.Image as TGLPersistentImage do
            LoadFromFile(extractvalue);
         NewMat.Material.Texture.Disabled := false;
         if assigned(FMemo) then FMemo.Lines.Add('File loaded : ' + extractvalue);
      end;
   end;
end;
procedure TGLMaterialScripter.XBlankImage;
begin
   if classexists('file') then
   begin
      if (extractvalue <> '') and (fileexists(extractvalue)) then
      begin
         with NewMat.Material.Texture.Image as TGLBlankImage do // heres the difference
            LoadFromFile(extractvalue);
         NewMat.Material.Texture.Disabled := false;
         if assigned(FMemo) then FMemo.Lines.Add('File loaded : ' + extractvalue);
      end;
   end;
end;

procedure TGLMaterialScripter.XPictureFileName;
begin
   if classexists('picturefilename') then
      with NewMat.Material.Texture.Image as TGLPicFileImage do
         if fileexists(extractvalue) then picturefilename := extractvalue;
end;

procedure TGLMaterialScripter.XPictureNX;
begin
   if classexists('picturenx') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TGLCubeMapImage do
         Picture[cmtNX].LoadFromFile(extractvalue);
end;

procedure TGLMaterialScripter.XPictureNY;
begin
   if classexists('pictureny') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TGLCubeMapImage do
         Picture[cmtNY].LoadFromFile(extractvalue);
end;

procedure TGLMaterialScripter.XPictureNZ;
begin
   if classexists('picturenz') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TGLCubeMapImage do
         Picture[cmtNZ].LoadFromFile(extractvalue);
end;

procedure TGLMaterialScripter.XPicturePX;
begin
   if classexists('picturepx') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TGLCubeMapImage do
         Picture[cmtPX].LoadFromFile(extractvalue);
end;

procedure TGLMaterialScripter.XPicturePY;
begin
   if classexists('picturepy') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TGLCubeMapImage do
         Picture[cmtPY].LoadFromFile(extractvalue);
end;

procedure TGLMaterialScripter.XPicturePZ;
begin
   if classexists('picturepz') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TGLCubeMapImage do
         Picture[cmtPZ].LoadFromFile(extractvalue);
end;

function TGLMaterialScripter.ValueExists(value: string): boolean;
begin
   if uppercase(tmpstr) = uppercase(value) then result := true
   else
   result := false;
end;

procedure TGLMaterialScripter.XMaterialLibrary;
begin
   // again, we need collection class items
end;

procedure TGLMaterialScripter.XShader;
begin
   // can't do this yet, need collection class, add shader library etc etc
end;

procedure TGLMaterialScripter.ZEffect;
begin
   // we have an effects script here
end;

procedure TGLMaterialScripter.ZMaterial;
var i : byte;
begin

   if assigned(FMaterialLibrary) then
   begin
      NewMat := FMaterialLibrary.Materials.Add;
      repeat

         inc(count);
         XMaterial;
         if pos('{',FScript.Strings[count]) > 0 then
         for i := 0 to 2 do // need repair : something went wrong, and now we have to check 3 times over :/
         begin
            XTexture;
            XBackProperties;
            XFrontProperties;
         end;
         checkerror;

      until checkrepeatdone;
      NewMat := nil;  // prepare for next material in script

   end;

end;

///////////////////////////
// extraction procedures //
///////////////////////////

procedure TGLMaterialScripter.XBackAmbient;
begin
   if classexists('ambient') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Ambient;
      extractcolors;
      NewMat.Material.BackProperties.Ambient := tmpcolor;
   end;
end;

procedure TGLMaterialScripter.XBackDiffuse;
begin
   if classexists('diffuse') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Diffuse;
      extractcolors;
      NewMat.Material.BackProperties.Diffuse := tmpcolor;
   end;

end;

procedure TGLMaterialScripter.XBackEmission;
begin
   if classexists('emission') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Emission;
      extractcolors;
      NewMat.Material.BackProperties.Emission := tmpcolor;
   end;
end;

procedure TGLMaterialScripter.XBackPolygonMode;
begin
   if classexists('polygonmode') then
   begin
      tmpstr := extractvalue;
      if valueexists('pmFill') then Newmat.Material.BackProperties.PolygonMode := pmFill;
      if valueexists('pmLines') then Newmat.Material.BackProperties.PolygonMode := pmLines;
      if valueexists('pmPoints') then Newmat.Material.BackProperties.PolygonMode := pmPoints;
   end;
end;


procedure TGLMaterialScripter.XBackShininess;
begin
   if classexists('shininess') then
   if extractvalue <> '' then
      NewMat.Material.BackProperties.Shininess := strtoint(extractvalue);
end;

procedure TGLMaterialScripter.XBackSpecular;
begin
   if classexists('specular') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Specular;
      extractcolors;
      NewMat.Material.BackProperties.Specular := tmpcolor;
   end;
end;

procedure TGLMaterialScripter.XBlendingMode;
begin
   if classexists('blendingmode') then
   begin
      tmpstr := extractvalue;
      if valueexists('bmOpaque') then Newmat.Material.BlendingMode := bmOpaque;
      if valueexists('bmTransparency') then Newmat.Material.BlendingMode := bmTransparency;
      if valueexists('bmAdditive') then Newmat.Material.BlendingMode := bmAdditive;
      if valueexists('bmAlphaTest100') then Newmat.Material.BlendingMode := bmAlphaTest100;
      if valueexists('bmAlphaTest50') then Newmat.Material.BlendingMode := bmAlphaTest50;
   end;
end;

procedure TGLMaterialScripter.XCompression;
begin
   if classexists('compression') then
   begin
      tmpstr := extractvalue;
      if valueexists('tcDefault') then Newmat.Material.Texture.Compression := tcDefault;
      if valueexists('tcHighQuality') then Newmat.Material.Texture.Compression := tcHighQuality;
      if valueexists('tcHighSpeed') then Newmat.Material.Texture.Compression := tcHighSpeed;
      if valueexists('tcNone') then Newmat.Material.Texture.Compression := tcNone;
      if valueexists('tcStandard') then Newmat.Material.Texture.Compression := tcStandard;
   end;
end;

procedure TGLMaterialScripter.XEnvColor;
begin
   if classexists('envcolor') then
   begin
      tmpcolor := NewMat.Material.Texture.EnvColor;
      extractcolors;
      NewMat.Material.Texture.EnvColor := tmpcolor;
   end;
end;

procedure TGLMaterialScripter.XFacingCulling;
begin
   if classexists('faceculling') then
   begin
      tmpstr := extractvalue;
      if valueexists('fcBufferDefault') then Newmat.Material.FaceCulling := fcBufferDefault;
      if valueexists('fcCull') then Newmat.Material.FaceCulling := fcCull;
      if valueexists('fcNoCull') then Newmat.Material.FaceCulling := fcNoCull;
   end;
end;

procedure TGLMaterialScripter.XFilteringQuality;
begin
   if classexists('filteringquality') then
   begin
      tmpstr := extractvalue;
      if valueexists('tfIsotropic') then Newmat.Material.Texture.FilteringQuality := tfIsotropic;
      if valueexists('tfAnisotropic') then Newmat.Material.Texture.FilteringQuality := tfAnisotropic;
   end;
end;


procedure TGLMaterialScripter.XfrontAmbient;
begin
   if classexists('ambient') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Ambient;
      extractcolors;
      NewMat.Material.frontProperties.Ambient := tmpcolor;
   end;
end;

procedure TGLMaterialScripter.XfrontDiffuse;
begin
   if classexists('diffuse') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Diffuse;
      extractcolors;
      NewMat.Material.frontProperties.Diffuse := tmpcolor;
   end;

end;

procedure TGLMaterialScripter.XfrontEmission;
begin
   if classexists('emission') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Emission;
      extractcolors;
      NewMat.Material.frontProperties.Emission := tmpcolor;
   end;
end;

procedure TGLMaterialScripter.XfrontPolygonMode;
begin
   if classexists('polygonmode') then
   begin
      tmpstr := extractvalue;
      if valueexists('pmFill') then Newmat.Material.frontProperties.PolygonMode := pmFill;
      if valueexists('pmLines') then Newmat.Material.frontProperties.PolygonMode := pmLines;
      if valueexists('pmPoints') then Newmat.Material.frontProperties.PolygonMode := pmPoints;
   end;
end;


procedure TGLMaterialScripter.XfrontShininess;
begin
   if classexists('shininess') then
   if extractvalue <> '' then
      NewMat.Material.frontProperties.Shininess := strtoint(extractvalue);
end;

procedure TGLMaterialScripter.XfrontSpecular;
begin
   if classexists('specular') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Specular;
      extractcolors;
      NewMat.Material.frontProperties.Specular := tmpcolor;
   end;
end;


procedure TGLMaterialScripter.XImageAlpha;
begin
   if classexists('imagealpha') then
   begin
      tmpstr := extractvalue;
      if valueexists('tiaDefault') then Newmat.Material.Texture.ImageAlpha := tiaDefault;
      if valueexists('tiaInverseLuminance') then Newmat.Material.Texture.ImageAlpha := tiaInverseLuminance;
      if valueexists('tiaInverseLuminanceSqrt') then Newmat.Material.Texture.ImageAlpha := tiaInverseLuminanceSqrt;
      if valueexists('tiaLuminance') then Newmat.Material.Texture.ImageAlpha := tiaLuminance;
      if valueexists('tiaLuminanceSqrt') then Newmat.Material.Texture.ImageAlpha := tiaLuminanceSqrt;
      if valueexists('tiaOpaque') then Newmat.Material.Texture.ImageAlpha := tiaOpaque;
      if valueexists('tiaSuperBlackTransparent') then Newmat.Material.Texture.ImageAlpha := tiaSuperBlackTransparent;
      if valueexists('tiaTopLeftPointColorTransparent') then Newmat.Material.Texture.ImageAlpha := tiaTopLeftPointColorTransparent;
      if valueexists('tiaAlphaFromIntensity') then Newmat.Material.Texture.ImageAlpha := tiaAlphaFromIntensity;
   end;
end;

procedure TGLMaterialScripter.XImageBrightness;
begin
   if classexists('imagebrightness') then
   if extractvalue <> '' then
      NewMat.Material.Texture.ImageBrightness := strtofloatdef(extractvalue);
end;


procedure TGLMaterialScripter.XImageGamma;
begin
   if classexists('imagegamma') then
   if extractvalue <> '' then
      NewMat.Material.Texture.ImageGamma := strtofloatdef(extractvalue);
end;

procedure TGLMaterialScripter.XLibMaterialName;
begin
   if classexists('libmaterialname') then NewMat.Material.LibMaterialName := extractvalue;
end;

procedure TGLMaterialScripter.XMagFilter;
begin
   if classexists('magfilter') then
   begin
      tmpstr := extractvalue;
      if valueexists('maLinear') then Newmat.Material.Texture.MagFilter := maLinear;
      if valueexists('maNearest') then Newmat.Material.Texture.MagFilter := maNearest;
   end;
end;

procedure TGLMaterialScripter.XMappingMode;
begin
   if classexists('mappingmode') then
   begin
      tmpstr := extractvalue;
      if valueexists('tmmUser') then Newmat.Material.Texture.MappingMode := tmmUser;
      if valueexists('tmmCubeMapCamera') then Newmat.Material.Texture.MappingMode := tmmCubeMapCamera;
      if valueexists('tmmCubeMapLight0') then Newmat.Material.Texture.MappingMode := tmmCubeMapLight0;
      if valueexists('tmmCubeMapNormal') then Newmat.Material.Texture.MappingMode := tmmCubeMapNormal;
      if valueexists('tmmCubeMapReflection') then Newmat.Material.Texture.MappingMode := tmmCubeMapReflection;
      if valueexists('tmmEyeLinear') then Newmat.Material.Texture.MappingMode := tmmEyeLinear;
      if valueexists('tmmObjectLinear') then Newmat.Material.Texture.MappingMode := tmmObjectLinear;
      if valueexists('tmmSphere') then Newmat.Material.Texture.MappingMode := tmmSphere;
   end;
end;

procedure TGLMaterialScripter.XMappingSCoordinates;
begin
   if classexists('mappingscoordinates') then
   begin
      tmpcoords4 := NewMat.Material.Texture.MappingSCoordinates;
      extractcoords4;
      NewMat.Material.Texture.MappingSCoordinates := tmpcoords4;
   end;
end;

procedure TGLMaterialScripter.XMappingTCoordinates;
begin
   if classexists('mappingtcoordinates') then
   begin
      tmpcoords4 := NewMat.Material.Texture.MappingTCoordinates;
      extractcoords4;
      NewMat.Material.Texture.MappingTCoordinates := tmpcoords4;
   end;
end;


procedure TGLMaterialScripter.XMaterialOptions;
var a,b : boolean;
begin
   a:= false; b:= false;
   if classexists('materialoptions') then
   begin
      tmpstr := extractvalue;
      if uppercase(copy(tmpstr, pos('[',tmpstr) + 1, pos(',',tmpstr) - 2)) = uppercase('true') then a := true
      else
      if uppercase(copy(tmpstr, pos('[',tmpstr) + 1, pos(',',tmpstr) - 2)) = uppercase('false') then a := false;

      delete(tmpstr,1,pos(',',tmpstr));

      if uppercase(copy(tmpstr, 1, pos(']',tmpstr) - 1)) = uppercase('true') then b := true
      else
      if uppercase(copy(tmpstr, 1, pos(']',tmpstr) - 1)) = uppercase('false') then b := false;

      if a then NewMat.Material.MaterialOptions := NewMat.Material.MaterialOptions + [moIgnoreFog];
      if b then NewMat.Material.MaterialOptions := NewMat.Material.MaterialOptions + [moNoLighting];

   end;
end;

procedure TGLMaterialScripter.XMinFilter;
begin
   if classexists('minfilter') then
   begin
      tmpstr := extractvalue;
      if valueexists('miLinearMipmapLinear') then Newmat.Material.Texture.MinFilter := miLinearMipmapLinear;
      if valueexists('miLinearMipmapNearest') then Newmat.Material.Texture.MinFilter := miLinearMipmapNearest;
      if valueexists('miNearest') then Newmat.Material.Texture.MinFilter := miNearest;
      if valueexists('miNearestMipmapLinear') then Newmat.Material.Texture.MinFilter := miNearestMipmapLinear;
      if valueexists('miNearestMipmapNearest') then Newmat.Material.Texture.MinFilter := miNearestMipmapNearest;
      if valueexists('miLinear') then Newmat.Material.Texture.MinFilter := miLinear;
   end;
end;

procedure TGLMaterialScripter.XName;
begin
   if classexists('name') then NewMat.Name := Extractvalue;
end;

procedure TGLMaterialScripter.XNormalMapScale;
begin
   if classexists('normalmapscale') then
   if extractvalue <> '' then
      NewMat.Material.Texture.NormalMapScale := strtofloatdef(extractvalue);
end;


procedure TGLMaterialScripter.XTexture2Name;
begin
   if classexists('texture2name') then NewMat.Texture2Name := ExtractValue;
end;

procedure TGLMaterialScripter.XTextureFormat;
begin
   if classexists('textureformat') then
   begin
      tmpstr := extractvalue;
      if valueexists('tfDefault') then Newmat.Material.Texture.TextureFormat := tfDefault;
      if valueexists('tfIntensity') then Newmat.Material.Texture.TextureFormat := tfIntensity;
      if valueexists('tfLuminance') then Newmat.Material.Texture.TextureFormat := tfLuminance;
      if valueexists('tfLuminanceAlpha') then Newmat.Material.Texture.TextureFormat := tfLuminanceAlpha;
      if valueexists('tfNormalMap') then Newmat.Material.Texture.TextureFormat := tfNormalMap;
      if valueexists('tfRGB') then Newmat.Material.Texture.TextureFormat := tfRGB;
      if valueexists('tfRGB16') then Newmat.Material.Texture.TextureFormat := tfRGB16;
      if valueexists('tfRGBA') then Newmat.Material.Texture.TextureFormat := tfRGBA;
      if valueexists('tfRGBA16') then Newmat.Material.Texture.TextureFormat := tfRGBA16;
      if valueexists('tfAlpha') then Newmat.Material.Texture.TextureFormat := tfAlpha;
   end;
end;

procedure TGLMaterialScripter.XTextureMode;
begin
   if classexists('texturemode') then
   begin
      tmpstr := extractvalue;
      if valueexists('tmDecal') then Newmat.Material.Texture.TextureMode := tmDecal;
      if valueexists('tmModulate') then Newmat.Material.Texture.TextureMode := tmModulate;
      if valueexists('tmReplace') then Newmat.Material.Texture.TextureMode := tmReplace;
      if valueexists('tmBlend') then Newmat.Material.Texture.TextureMode := tmBlend;
   end;
end;

procedure TGLMaterialScripter.XTextureOffset;
begin
   if classexists('textureoffset') then // i hate this, delphi doesn't allow var object reference for procs
   begin
      tmpcoords := Newmat.TextureOffset;
      extractcoords3;
      Newmat.TextureOffset := tmpcoords;
   end;
end;

procedure TGLMaterialScripter.XTextureScale;
begin
   if classexists('texturescale') then
   begin
      tmpcoords := Newmat.TextureScale;
      extractcoords3;
      NewMat.TextureScale := tmpcoords;
   end;
end;

procedure TGLMaterialScripter.XTextureWrap;
begin
   if classexists('texturewrap') then
   begin
      tmpstr := extractvalue;
      if valueexists('twBoth') then Newmat.Material.Texture.TextureWrap := twBoth;
      if valueexists('twHorizontal') then Newmat.Material.Texture.TextureWrap := twHorizontal;
      if valueexists('twNone') then Newmat.Material.Texture.TextureWrap := twNone;
      if valueexists('twVertical') then Newmat.Material.Texture.TextureWrap := twVertical;
   end;
end;

///////////////////////////////////////
// sub routines : substr{arguements} //
///////////////////////////////////////

procedure TGLMaterialScripter.XTexture;
begin
   if substrexists('texture') then
   begin
      if assigned(FMemo) then FMemo.Lines.Add('texture');
      repeat

         inc(count);
         XCompression;
         XEnvColor;
         XFilteringQuality;
         XImageAlpha;
         XImageBrightness;
         XImageClass;
         XImageGamma;
         XMagFilter;
         XMappingMode;
         XMappingSCoordinates;
         XMappingTCoordinates;
         XMinFilter;
         XNormalMapScale;
         XTextureFormat;
         XTextureMode;
         XTextureWrap;

         checkerror;
      until checkrepeatdone;
   end;
end;

procedure TGLMaterialScripter.XMaterial;
begin
   XName;
   XShader;
   XTexture2Name;
   XTextureOffset;
   XTextureScale;
   XMaterialOptions;
   XLibMaterialName;
   XBlendingMode;
   XMaterialLibrary;
end;


procedure TGLMaterialScripter.XfrontProperties;
begin

   if substrexists('frontProperties') then
   begin
      if assigned(FMemo) then FMemo.Lines.Add('frontproperties');

      repeat

         inc(count);

         XfrontAmbient;
         XfrontDiffuse;
         XfrontEmission;
         XfrontPolygonMode;
         XfrontShininess;
         XfrontSpecular;

         checkerror;

      until checkrepeatdone;
   end;
end;

procedure TGLMaterialScripter.XImageClass; // reckon this will be most difficult to get right
begin
   if classexists('imageclassname') then
   begin

      tmpstr := extractvalue;
      // set these first, so we have the class already generated by SetImageClassName in GLTexture.pas
      if valueexists('Persistent Image') then Newmat.Material.Texture.ImageClassName := TGLPersistentImage.ClassName;
      if valueexists('Blank Image') then Newmat.Material.Texture.ImageClassName := TGLBlankImage.ClassName;
      if valueexists('PicFile Image') then Newmat.Material.Texture.ImageClassName := TGLPicFileImage.ClassName;
      if valueexists('CubeMap Image') then Newmat.Material.Texture.ImageClassName := TGLCubeMapImage.ClassName;

      tmpstr := deletespaces(tmpstr);

      if valueexists('persistentimage{') then // loadfromfile
      repeat
         inc(count);
         XPersistantImage;
         checkerror;
      until checkrepeatdone;

      if substrexists('blankimage{') then // loadfromfile
      repeat
         inc(count);
         XBlankImage;
         checkerror;
      until checkrepeatdone;

      if substrexists('picfileimage{') then //picturefilename
      repeat
         inc(count);
         XPictureFilename;
         checkerror;
      until checkrepeatdone;

      if substrexists('cubemapimage{') then  // px, nx, py, ny, pz, nz
      repeat
         inc(count);
         XPicturePX;
         XPictureNX;
         XPicturePY;
         XPictureNY;
         XPicturePZ;
         XPictureNZ;
         checkerror;
      until checkrepeatdone;

      // procedural noise not supported by GLTexture yet
   end;
end;

procedure TGLMaterialScripter.XBackProperties;
begin

   if substrexists('BackProperties') then
   begin
      if assigned(FMemo) then FMemo.Lines.Add('backproperties');

      repeat

         inc(count);

         XBackAmbient;
         XBackDiffuse;
         XBackEmission;
         XBackPolygonMode;
         XBackShininess;
         XBackSpecular;

         checkerror;

      until checkrepeatdone;
   end;
end;


end.
