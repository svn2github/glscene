//
// This unit is part of the GLScene Project   
//
{: VKS.MaterialScript<p>

   Material Script Batch loader for TVKMaterialLibrary for runtime.<p>

	<b>History : </b><font size=-1><ul>
      <li>22/04/10 - Yar - Fixes after VKS.State revision
      <li>22/01/10 - Yar   - Added VKS.TextureFormat to uses
      <li>24/03/08 - DaStr - Moved TVKMinFilter and TVKMagFilter from VKS.Utils.pas
                              to VKS.Graphics.pas (BugTracker ID = 1923844)
      <li>02/04/07 - DaStr - TVKMaterialScripter is now notified of
                               DebugMemo's and MaterialLibrary's destruction
                             TVKShaderItems and TVKMaterialLibraryItems now
                               descent from TOwnedCollection
                             Removed unused stuff from "uses" section
                             Alligned and formated the "interface" section
      <li>29/01/07 - DaStr - Moved registration to GLSceneRegister.pas
      <li>09/06/04 - Mathx - Addition to GLScene (created by Kenneth Poulter)
	</ul></font>
}
{
   Author : Kenneth Poulter (aka SpiriT aka Difacane)
   Base : none, apart from glscene materiallibrary

   History :
   26/06/2004 - KP - started basic script idea using repeat statements
   26/06/2004 - KP - script is now half functional and method proved to be effective
   27/06/2004 - KP - finished script, but not dynamic, error handling needs some work
   28/06/2004 - KP - cleaned it all up, nearly ready for realease
   29/06/2004 - KP - Converted to a component class, ready for release
   29/06/2004 - KP - Updated strtofloat to strtofloatdef and replaced "," with ";"
   29/06/2004 - KP - Added MaterialLibraries and Shaders for use
   06/07/2004 - KP - Added Append and Overwrite
   
   Future notes :
   Implementation of variables
   Implementation of constants

   This source falls under the GNU GPL license, unless stated otherwise by the author(Kenneth Poulter).

   Additions are welcome

}

unit VKS.MaterialScript;

interface

{$I VKScene.inc}

uses
  System.SysUtils, System.Classes,
  FMX.StdCtrls, FMX.Memo,

  VKS.Texture, VKS.TextureFormat, VKS.Graphics, VKS.Utils, VKS.Color, VKS.Coordinates,
  VKS.Material, VKS.State;

type
  TVKShaderItem = class(TCollectionItem)
  private
    FShader: TVKShader;
    FName: string;
    procedure SetShader(const Value: TVKShader);
    procedure SetName(const Value: string);
    { Private Declarations }

  protected
    { Protected Declarations }
    function GetDisplayName: string; override;

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property Shader: TVKShader read FShader write SetShader;
    property Name: string read FName write SetName;
  end;

  TVKShaderItems = class(TOwnedCollection)
  private
    { Protected Declarations }
    procedure SetItems(Index: Integer; const Val: TVKShaderItem);
    function GetItems(Index: Integer): TVKShaderItem;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TVKShaderItem read GetItems write SetItems; default;

  end;

  TVKMaterialLibraryItem = class(TCollectionItem)
  private
    FMaterialLibrary: TVKMaterialLibrary;
    FName: string;
    procedure SetMaterialLibrary(const Value: TVKMaterialLibrary);
    procedure SetName(const Value: string);
    { Private Declarations }

  protected
    { Protected Declarations }
    function GetDisplayName: string; override;

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property MaterialLibrary: TVKMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property Name: string read FName write SetName;
  end;

  TVKMaterialLibraryItems = class(TOwnedCollection)
  private
    { Protected Declarations }
    procedure SetItems(Index: Integer; const Val: TVKMaterialLibraryItem);
    function GetItems(Index: Integer): TVKMaterialLibraryItem;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TVKMaterialLibraryItem read GetItems write SetItems; default;

  end;


  TVKMaterialScripter = class(TComponent)
  private
    { Private declarations }
    FShaderItems: TVKShaderItems;
    FMaterialLibraryItems: TVKMaterialLibraryItems;
    FAppend: Boolean;
    FOverwrite: Boolean;

    FScript: TStrings;
    FMemo: TMemo;
    FMaterialLibrary: TVKMaterialLibrary;

    Count: Longint;
    infini: Longint;
    done: Boolean;

    NewMat: TVKLibMaterial;

    tmpcoords: TVKCoordinates;
    tmpcolor: TVKColor;
    tmpcoords4: TVKCoordinates4;
    tmpstr: string;

    procedure SeTVKShaderItems(const Value: TVKShaderItems);
    procedure SeTVKMaterialLibraryItems(const Value: TVKMaterialLibraryItems);
    procedure SetAppend(const Value: Boolean);
    procedure SetOverwrite(const Value: Boolean);

    procedure SetScript(const Value: TStrings);
    procedure SetMaterialLibrary(const Value: TVKMaterialLibrary);
    procedure SetMemo(const Value: TMemo);

    // error checking
    procedure CheckError;
    function ClassExists(arguement: string): Boolean;
    function CheckRepeatDone: Boolean;
    // extraction functions
    function ExtractValue: string;
    procedure ExtractCoords3;
    procedure ExtractCoords4;
    procedure ExtractColors;
    function DeleteSpaces(Value: string): string;
    function SubstrExists(substr: string): Boolean;
    function ValueExists(Value: string): Boolean;
    // these are our viable scripts
    procedure ZMaterial;
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
    procedure XPolygonMode;
    procedure XFacingCulling;
    procedure XLibMaterialName;
    procedure XMaterialOptions;
    procedure XMaterialLibrary;
    procedure XBackProperties;
    procedure XBackAmbient;
    procedure XBackDiffuse;
    procedure XBackEmission;
    procedure XBackShininess;
    procedure XBackSpecular;
    procedure XFrontProperties;
    procedure XFrontAmbient;
    procedure XFrontDiffuse;
    procedure XFrontEmission;
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

  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    { Public declarations }
    property DebugMemo: TMemo read FMemo write SetMemo;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CompileScript;

  published
    { Published declarations }
    property Script: TStrings read FScript write SetScript;
    property MaterialLibrary: TVKMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property Shaders: TVKShaderItems read FShaderItems write SeTVKShaderItems;
    property MaterialLibraries: TVKMaterialLibraryItems read FMaterialLibraryItems write SeTVKMaterialLibraryItems;
    property AppendToMaterialLibrary: Boolean read FAppend write SetAppend;
    property OverwriteToMaterialLibrary: Boolean read FOverwrite write SetOverwrite;

  end;

implementation

procedure TVKShaderItem.SetShader(const Value: TVKShader);
begin
  if assigned(Value) then
  begin
     FShader := Value;
     FName := FShader.Name;
  end;
end;

procedure TVKShaderItem.Assign(Source: TPersistent);
begin
  if Source is TVKShaderItem then
  begin
     FShader := TVKShaderItem(Source).FShader;
  end;
  inherited Destroy;
end;

constructor TVKShaderItem.Create(Collection: TCollection);
begin
   inherited Create(Collection);
   FName := 'Shader';
end;

destructor TVKShaderItem.Destroy;
begin
  inherited Destroy;
end;

function TVKShaderItem.GetDisplayName : String;
begin
   if FName = '' then
   Result:='Shader'
   else
   Result := FName;
end;

{ TVKShaderItems }

constructor TVKShaderItems.Create(AOwner: TPersistent);
begin
   inherited Create(AOwner, TVKShaderItem);
end;

function TVKShaderItems.GetItems(index: Integer): TVKShaderItem;
begin
   Result:=TVKShaderItem(inherited Items[index]);
end;


procedure TVKShaderItems.SetItems(index: Integer; const val: TVKShaderItem);
begin
   inherited Items[index]:=val;
end;

procedure TVKMaterialScripter.SeTVKShaderItems(const Value: TVKShaderItems);
begin
   FShaderItems.Assign(Value);
end;

procedure TVKShaderItem.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TVKMaterialScripter.CompileScript;
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

      end;
      checkerror;

   until checkrepeatdone;

end;

procedure TVKMaterialScripter.SetMaterialLibrary(
  const Value: TVKMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if FMaterialLibrary <> nil then FMaterialLibrary.FreeNotification(Self);
end;

procedure TVKMaterialScripter.SetMemo(const Value: TMemo);
begin
  if FMemo <> nil then FMemo.RemoveFreeNotification(Self);
  FMemo := Value;
  if FMemo <> nil then FMemo.FreeNotification(Self);
end;

procedure TVKMaterialScripter.SetScript(const Value: TStrings);
begin
  if assigned(value) then
  FScript.Assign(Value);
end;

procedure TVKMaterialScripter.CheckError;
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

function TVKMaterialScripter.CheckRepeatDone: boolean;
begin
   checkrepeatdone := false;
   if pos('}',FScript.Strings[count]) > 0 then
   begin
      checkrepeatdone := true;
      inc(count);
   end;

   if done then checkrepeatdone := true;
end;

function TVKMaterialScripter.ClassExists(arguement: string): boolean;
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
function TVKMaterialScripter.SubstrExists(substr: string): boolean;
begin
   if pos(uppercase(substr),uppercase(FScript.Strings[count])) > 0 then result := true
   else
   result := false;
end;


constructor TVKMaterialScripter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScript := TStringList.Create;
  FShaderItems:=TVKShaderItems.Create(Self);
  FMaterialLibraryItems:=TVKMaterialLibraryItems.Create(Self);
  FAppend := true;
  FOverwrite := false;
end;

function TVKMaterialScripter.DeleteSpaces(value: string): string;
var i : byte;
begin
   result := value;

   for i := 0 to length(result) do
   if pos(' ',result) > 0 then
   delete(result,pos(' ',result), 1);
end;

destructor TVKMaterialScripter.Destroy;
begin
  FShaderItems.Free;
  FMaterialLibraryItems.Free;
  FScript.Free;
  inherited Destroy;
end;

procedure TVKMaterialScripter.ExtractColors;
var val : string;
begin
   val := Extractvalue;
   if pos('(',val) > 0 then
   begin
      tmpcolor.Alpha := VKS.Utils.StrToFloatDef(copy(val, pos('(',val) + 1, pos(';',val) - 2));
      delete(val,1,pos(';',val));
      tmpcolor.Red := VKS.Utils.StrToFloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcolor.Green := VKS.Utils.StrToFloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcolor.Blue := VKS.Utils.StrToFloatDef(copy(val, 1, pos(')',val) - 1));
   end;
end;

procedure TVKMaterialScripter.ExtractCoords3;
var val : string;
begin
   val := Extractvalue;
   if pos('(',val) > 0 then
   begin
      tmpcoords.X := VKS.Utils.StrToFloatDef(copy(val, pos('(',val) + 1, pos(';',val) - 2));
      delete(val,1,pos(';',val));
      tmpcoords.Y := VKS.Utils.StrToFloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcoords.Z := VKS.Utils.StrToFloatDef(copy(val, 1, pos(')',val) - 1));
   end;
end;

procedure TVKMaterialScripter.ExtractCoords4;
var val : string;
begin
   val := Extractvalue;
   if pos('(',val) > 0 then
   begin
      tmpcoords4.W := VKS.Utils.StrToFloatDef(copy(val, pos('(',val) + 1, pos(';',val) - 2));
      delete(val,1,pos(';',val));
      tmpcoords4.X := VKS.Utils.StrToFloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcoords4.Y := VKS.Utils.StrToFloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcoords4.Z := VKS.Utils.StrToFloatDef(copy(val, 1, pos(')',val) - 1));
   end;
end;

function TVKMaterialScripter.ExtractValue: string;
begin
   extractvalue := copy(FScript.Strings[count], pos('=',FScript.Strings[count]) + 1, length(FScript.Strings[count]) - pos('=',FScript.Strings[count]));
end;
procedure TVKMaterialScripter.XPersistantImage;
begin
   if classexists('file') then
   begin
      if (extractvalue <> '') and (fileexists(extractvalue)) then
      begin
         with NewMat.Material.Texture.Image as TVKPersistentImage do
            LoadFromFile(extractvalue);
         NewMat.Material.Texture.Disabled := false;
         if assigned(FMemo) then FMemo.Lines.Add('File loaded : ' + extractvalue);
      end;
   end;
end;
procedure TVKMaterialScripter.XBlankImage;
begin
   if classexists('file') then
   begin
      if (extractvalue <> '') and (fileexists(extractvalue)) then
      begin
         with NewMat.Material.Texture.Image as TVKBlankImage do // heres the difference
            LoadFromFile(extractvalue);
         NewMat.Material.Texture.Disabled := false;
         if assigned(FMemo) then FMemo.Lines.Add('File loaded : ' + extractvalue);
      end;
   end;
end;

procedure TVKMaterialScripter.XPictureFileName;
begin
   if classexists('picturefilename') then
      with NewMat.Material.Texture.Image as TVKPicFileImage do
         if fileexists(extractvalue) then
         begin
            picturefilename := extractvalue;
            NewMat.Material.Texture.Disabled := false;
         end;
end;

procedure TVKMaterialScripter.XPictureNX;
begin
   if classexists('picturenx') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TVKCubeMapImage do
         Picture[cmtNX].Bitmap.LoadFromFile(extractvalue);
end;

procedure TVKMaterialScripter.XPictureNY;
begin
   if classexists('pictureny') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TVKCubeMapImage do
         Picture[cmtNY].Bitmap.LoadFromFile(extractvalue);
end;

procedure TVKMaterialScripter.XPictureNZ;
begin
   if classexists('picturenz') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TVKCubeMapImage do
         Picture[cmtNZ].Bitmap.LoadFromFile(extractvalue);
end;

procedure TVKMaterialScripter.XPicturePX;
begin
   if classexists('picturepx') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TVKCubeMapImage do
         Picture[cmtPX].Bitmap.LoadFromFile(extractvalue);
end;

procedure TVKMaterialScripter.XPicturePY;
begin
   if classexists('picturepy') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TVKCubeMapImage do
         Picture[cmtPY].Bitmap.LoadFromFile(extractvalue);
end;

procedure TVKMaterialScripter.XPicturePZ;
begin
   if classexists('picturepz') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TVKCubeMapImage do
         Picture[cmtPZ].Bitmap.LoadFromFile(extractvalue);
end;

function TVKMaterialScripter.ValueExists(value: string): boolean;
begin
   if uppercase(tmpstr) = uppercase(value) then result := true
   else
   result := false;
end;

procedure TVKMaterialScripter.XMaterialLibrary;
var i : word;
begin
   if classexists('materiallibrary') then
   if MaterialLibraries.count > 0 then
      for i := 0 to MaterialLibraries.Count - 1 do
         if assigned(MaterialLibraries.Items[i].MaterialLibrary) then
            if uppercase(MaterialLibraries.Items[i].MaterialLibrary.Name) = uppercase(extractvalue) then
            NewMat.Material.MaterialLibrary := MaterialLibraries.Items[i].MaterialLibrary;
end;

procedure TVKMaterialScripter.XShader;
var i : word;
begin
   if classexists('shader') then
   if Shaders.count > 0 then
      for i := 0 to Shaders.Count - 1 do
         if assigned(Shaders.Items[i].Shader) then
            if uppercase(Shaders.Items[i].Shader.Name) = uppercase(extractvalue) then
            NewMat.Shader := Shaders.Items[i].Shader;
end;

procedure TVKMaterialScripter.ZMaterial;
var i : byte;
    exists : boolean;
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

      // now we use append and overwrite settings to find out what is what

      tmpstr := NewMat.Name;
      delete(tmpstr,1,3); // removes the "TAG" not to confuse the system

      exists := false;
      if FMaterialLibrary.Materials.Count > 0 then
      for i := 0 to FMaterialLibrary.Materials.Count - 1 do
      if tmpstr = FMaterialLibrary.Materials.Items[i].Name then
         exists := true;

      if Exists then // does exist
      begin
         if FOverwrite then
         begin
            FMaterialLibrary.Materials.Delete(FMaterialLibrary.LibMaterialByName(tmpstr).Index);
            NewMat.Name := tmpstr;
         end
         else
         if FAppend then
         begin
            NewMat.Free;
         end;
      end
      else           // doesn't exist
      begin
         NewMat.Name := tmpstr;
         if not FAppend then
         NewMat.Free;
      end;

   end;

end;

///////////////////////////
// extraction procedures //
///////////////////////////

procedure TVKMaterialScripter.XBackAmbient;
begin
   if classexists('ambient') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Ambient;
      extractcolors;
      NewMat.Material.BackProperties.Ambient := tmpcolor;
   end;
end;

procedure TVKMaterialScripter.XBackDiffuse;
begin
   if classexists('diffuse') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Diffuse;
      extractcolors;
      NewMat.Material.BackProperties.Diffuse := tmpcolor;
   end;

end;

procedure TVKMaterialScripter.XBackEmission;
begin
   if classexists('emission') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Emission;
      extractcolors;
      NewMat.Material.BackProperties.Emission := tmpcolor;
   end;
end;


procedure TVKMaterialScripter.XBackShininess;
begin
   if classexists('shininess') then
   if extractvalue <> '' then
      NewMat.Material.BackProperties.Shininess := strtoint(extractvalue);
end;

procedure TVKMaterialScripter.XBackSpecular;
begin
   if classexists('specular') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Specular;
      extractcolors;
      NewMat.Material.BackProperties.Specular := tmpcolor;
   end;
end;

procedure TVKMaterialScripter.XBlendingMode;
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

procedure TVKMaterialScripter.XPolygonMode;
begin
   if classexists('polygonmode') then
   begin
      tmpstr := extractvalue;
      if valueexists('pmFill') then Newmat.Material.PolygonMode := pmFill;
      if valueexists('pmLines') then Newmat.Material.PolygonMode := pmLines;
      if valueexists('pmPoints') then Newmat.Material.PolygonMode := pmPoints;
   end;
end;

procedure TVKMaterialScripter.XCompression;
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

procedure TVKMaterialScripter.XEnvColor;
begin
   if classexists('envcolor') then
   begin
      tmpcolor := NewMat.Material.Texture.EnvColor;
      extractcolors;
      NewMat.Material.Texture.EnvColor := tmpcolor;
   end;
end;

procedure TVKMaterialScripter.XFacingCulling;
begin
   if classexists('faceculling') then
   begin
      tmpstr := extractvalue;
      if valueexists('fcBufferDefault') then Newmat.Material.FaceCulling := fcBufferDefault;
      if valueexists('fcCull') then Newmat.Material.FaceCulling := fcCull;
      if valueexists('fcNoCull') then Newmat.Material.FaceCulling := fcNoCull;
   end;
end;

procedure TVKMaterialScripter.XFilteringQuality;
begin
   if classexists('filteringquality') then
   begin
      tmpstr := extractvalue;
      if valueexists('tfIsotropic') then Newmat.Material.Texture.FilteringQuality := tfIsotropic;
      if valueexists('tfAnisotropic') then Newmat.Material.Texture.FilteringQuality := tfAnisotropic;
   end;
end;


procedure TVKMaterialScripter.XfrontAmbient;
begin
   if classexists('ambient') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Ambient;
      extractcolors;
      NewMat.Material.frontProperties.Ambient := tmpcolor;
   end;
end;

procedure TVKMaterialScripter.XfrontDiffuse;
begin
   if classexists('diffuse') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Diffuse;
      extractcolors;
      NewMat.Material.frontProperties.Diffuse := tmpcolor;
   end;

end;

procedure TVKMaterialScripter.XfrontEmission;
begin
   if classexists('emission') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Emission;
      extractcolors;
      NewMat.Material.frontProperties.Emission := tmpcolor;
   end;
end;

procedure TVKMaterialScripter.XfrontShininess;
begin
   if classexists('shininess') then
   if extractvalue <> '' then
      NewMat.Material.frontProperties.Shininess := strtoint(extractvalue);
end;

procedure TVKMaterialScripter.XfrontSpecular;
begin
   if classexists('specular') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Specular;
      extractcolors;
      NewMat.Material.frontProperties.Specular := tmpcolor;
   end;
end;


procedure TVKMaterialScripter.XImageAlpha;
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

procedure TVKMaterialScripter.XImageBrightness;
begin
   if classexists('imagebrightness') then
   if extractvalue <> '' then
      NewMat.Material.Texture.ImageBrightness := VKS.Utils.StrToFloatDef(extractvalue);
end;


procedure TVKMaterialScripter.XImageGamma;
begin
   if classexists('imagegamma') then
   if extractvalue <> '' then
      NewMat.Material.Texture.ImageGamma := VKS.Utils.StrToFloatDef(extractvalue);
end;

procedure TVKMaterialScripter.XLibMaterialName;
begin
   if classexists('libmaterialname') then NewMat.Material.LibMaterialName := extractvalue;
end;

procedure TVKMaterialScripter.XMagFilter;
begin
   if classexists('magfilter') then
   begin
      tmpstr := extractvalue;
      if valueexists('maLinear') then Newmat.Material.Texture.MagFilter := maLinear;
      if valueexists('maNearest') then Newmat.Material.Texture.MagFilter := maNearest;
   end;
end;

procedure TVKMaterialScripter.XMappingMode;
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

procedure TVKMaterialScripter.XMappingSCoordinates;
begin
   if classexists('mappingscoordinates') then
   begin
      tmpcoords4 := NewMat.Material.Texture.MappingSCoordinates;
      extractcoords4;
      NewMat.Material.Texture.MappingSCoordinates := tmpcoords4;
   end;
end;

procedure TVKMaterialScripter.XMappingTCoordinates;
begin
   if classexists('mappingtcoordinates') then
   begin
      tmpcoords4 := NewMat.Material.Texture.MappingTCoordinates;
      extractcoords4;
      NewMat.Material.Texture.MappingTCoordinates := tmpcoords4;
   end;
end;


procedure TVKMaterialScripter.XMaterialOptions;
var a,b : boolean;
begin
   if classexists('materialoptions') then
   begin
      a := false;
      b := false;
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

procedure TVKMaterialScripter.XMinFilter;
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

procedure TVKMaterialScripter.XName;
begin
   if classexists('name') then NewMat.Name := 'TAG' + Extractvalue; // we gonna use for appending and such, quick fix style
end;

procedure TVKMaterialScripter.XNormalMapScale;
begin
   if classexists('normalmapscale') then
   if extractvalue <> '' then
      NewMat.Material.Texture.NormalMapScale := VKS.Utils.StrToFloatDef(extractvalue);
end;


procedure TVKMaterialScripter.XTexture2Name;
begin
   if classexists('texture2name') then NewMat.Texture2Name := ExtractValue;
end;

procedure TVKMaterialScripter.XTextureFormat;
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

procedure TVKMaterialScripter.XTextureMode;
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

procedure TVKMaterialScripter.XTextureOffset;
begin
   if classexists('textureoffset') then // i hate this, delphi doesn't allow var object reference for procs
   begin
      tmpcoords := Newmat.TextureOffset;
      extractcoords3;
      Newmat.TextureOffset := tmpcoords;
   end;
end;

procedure TVKMaterialScripter.XTextureScale;
begin
   if classexists('texturescale') then
   begin
      tmpcoords := Newmat.TextureScale;
      extractcoords3;
      NewMat.TextureScale := tmpcoords;
   end;
end;

procedure TVKMaterialScripter.XTextureWrap;
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

procedure TVKMaterialScripter.XTexture;
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

procedure TVKMaterialScripter.XMaterial;
begin
   XName;
   XShader;
   XTexture2Name;
   XTextureOffset;
   XTextureScale;
   XMaterialOptions;
   XLibMaterialName;
   XBlendingMode;
   XPolygonMode;
   XFacingCulling;
   XMaterialLibrary;
end;


procedure TVKMaterialScripter.XfrontProperties;
begin

   if substrexists('frontProperties') then
   begin
      if assigned(FMemo) then FMemo.Lines.Add('frontproperties');

      repeat

         inc(count);

         XfrontAmbient;
         XfrontDiffuse;
         XfrontEmission;
         XfrontShininess;
         XfrontSpecular;

         checkerror;

      until checkrepeatdone;
   end;
end;

procedure TVKMaterialScripter.XImageClass; // reckon this will be most difficult to get right
begin
   if classexists('imageclassname') then
   begin

      tmpstr := extractvalue;
      tmpstr := deletespaces(tmpstr);

      if valueexists('persistentimage{') then // loadfromfile
      repeat
         inc(count);
         Newmat.Material.Texture.ImageClassName := TVKPersistentImage.ClassName;
         XPersistantImage;
         checkerror;
      until checkrepeatdone;

      if valueexists('blankimage{') then // loadfromfile
      repeat
         inc(count);
         Newmat.Material.Texture.ImageClassName := TVKBlankImage.ClassName;
         XBlankImage;
         checkerror;
      until checkrepeatdone;

      if valueexists('picfileimage{') then //picturefilename
      repeat
         inc(count);
         Newmat.Material.Texture.ImageClassName := TVKPicFileImage.ClassName;
         XPictureFilename;
         checkerror;
      until checkrepeatdone;

      if valueexists('cubemapimage{') then  // px, nx, py, ny, pz, nz
      repeat
         inc(count);
         Newmat.Material.Texture.ImageClassName := TVKCubeMapImage.ClassName;
         XPicturePX;
         XPictureNX;
         XPicturePY;
         XPictureNY;
         XPicturePZ;
         XPictureNZ;
         NewMat.Material.Texture.Disabled := false;
         checkerror;
      until checkrepeatdone;

      // procedural noise not supported by VKS.Texture yet
   end;
end;

procedure TVKMaterialScripter.XBackProperties;
begin

   if substrexists('BackProperties') then
   begin
      if assigned(FMemo) then FMemo.Lines.Add('backproperties');

      repeat

         inc(count);

         XBackAmbient;
         XBackDiffuse;
         XBackEmission;
         XBackShininess;
         XBackSpecular;

         checkerror;

      until checkrepeatdone;
   end;
end;


{ TVKMaterialLibraryItems }

constructor TVKMaterialLibraryItems.Create(AOwner: TPersistent);
begin
   inherited Create(AOwner, TVKMaterialLibraryItem);
end;

function TVKMaterialLibraryItems.GetItems(index: Integer): TVKMaterialLibraryItem;
begin
   Result:=TVKMaterialLibraryItem(inherited Items[index]);

end;

procedure TVKMaterialLibraryItems.SetItems(index: Integer;
  const val: TVKMaterialLibraryItem);
begin
   inherited Items[index]:=val;
end;

{ TVKMaterialLibraryItem }

procedure TVKMaterialLibraryItem.Assign(Source: TPersistent);
begin
  if Source is TVKMaterialLibraryItem then
  begin
     FMaterialLibrary := TVKMaterialLibraryItem(Source).FMaterialLibrary;
  end;
  inherited Destroy;
end;

constructor TVKMaterialLibraryItem.Create(Collection: TCollection);
begin
   inherited Create(Collection);
   FName := 'MaterialLibrary';
end;

destructor TVKMaterialLibraryItem.Destroy;
begin
  inherited Destroy;
end;

function TVKMaterialLibraryItem.GetDisplayName: String;
begin
   if FName = '' then
   Result:='MaterialLibrary'
   else
   Result := FName;
end;

procedure TVKMaterialLibraryItem.SetMaterialLibrary(
  const Value: TVKMaterialLibrary);
begin
  if assigned(Value) then
  begin
     FMaterialLibrary := Value;
     FName := FMaterialLibrary.Name;
  end;
end;

procedure TVKMaterialLibraryItem.SetName(const Value: String);
begin
   FName := Value;
end;

procedure TVKMaterialScripter.SeTVKMaterialLibraryItems(
  const Value: TVKMaterialLibraryItems);
begin
   FMaterialLibraryItems.Assign(Value);
end;

procedure TVKMaterialScripter.SetAppend(const Value: boolean);
begin
  FAppend := Value;
end;

procedure TVKMaterialScripter.SetOverwrite(const Value: boolean);
begin
  FOverwrite := Value;
end;

procedure TVKMaterialScripter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil
    else if AComponent = FMemo then
      FMemo := nil;
  end;
end;

end.