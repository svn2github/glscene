{: GLBumpShader<p>

   A shader that applies bump mapping.<p>

   Notes:
   The normal map is expected to be the primary texture.<p>

   The secondary texture is used for the diffuse texture,
   to enable set boDiffuseTexture2 in the BumpOptions property.<p>

   Quaternion tangents and Basic ARBFP are currently
   incompatible, if set bump method will be forced to Dot3.<p>

   External tangent bump space expects tangent data under
   GL_TEXTURE1_ARB and binormal data under GL_TEXTURE2_ARB.<p>

   <b>History : </b><font size=-1><ul>
      <li>02/10/04 - SG - Changed render order a little, minimum texture units
                          is now 2 for dot3 texcombiner bump method,
                          Changed vertex programs to accept local program
                          params, now only 1 vertex and 1 fragment program is
                          required for all lights.
      <li>30/09/04 - SG - Added fragment program logic,
                          Added bmBasicARBFP bump method, bsTangentExternal
                          bump space and associated ARB programs,
                          Various name changes and fixes
      <li>28/09/04 - SG - Vertex programs now use ARB_position_invariant option.
      <li>29/06/04 - SG - Quaternion tangent space fix in tangent bump vertex
                          program.
      <li>23/06/04 - SG - Added bsTangent option to TBumpSpace,
                          Added tangent space light vector vertex program.
      <li>22/06/04 - SG - Creation.
   </ul></font>
}
unit GLBumpShader;

interface

uses
   Classes, SysUtils, GLTexture, GLContext, GLGraphics, GLUtils,
   VectorGeometry, OpenGL1x, VectorLists;

type
   TBumpMethod = (bmDot3TexCombiner, bmBasicARBFP);

   TBumpSpace = (bsObject, bsTangentExternal, bsTangentQuaternion);

   TBumpOption = (boDiffuseTexture2);
   TBumpOptions = set of TBumpOption;

   // TGLBumpShader
   //
   {: A generic bump shader.<p> }
   TGLBumpShader = class (TGLShader)
      private
         FVertexProgramHandles,
         FFragmentProgramHandles : array of Cardinal;
         FLightIDs : TIntegerList;
         FLightsEnabled : Integer;
         FBumpMethod : TBumpMethod;
         FBumpSpace : TBumpSpace;
         FBumpOptions : TBumpOptions;
         FDesignTimeEnabled : Boolean;
         FAmbientPass : Boolean;
         FDiffusePass : Boolean;

         procedure DoLightPass(lightID : Cardinal);

      protected
         procedure SetBumpMethod(const Value : TBumpMethod);
         procedure SetBumpSpace(const Value : TBumpSpace);
         procedure SetBumpOptions(const Value : TBumpOptions);
         procedure SetDesignTimeEnabled(const Value : Boolean);
         procedure Loaded; override;
         procedure DeleteVertexPrograms;
         procedure DeleteFragmentPrograms;

      public
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
         function DoUnApply(var rci: TRenderContextInfo) : Boolean; override;

      published
         property BumpMethod : TBumpMethod read FBumpMethod write SetBumpMethod;
         property BumpSpace : TBumpSpace read FBumpSpace write SetBumpSpace;
         property BumpOptions : TBumpOptions read FBumpOptions write SetBumpOptions;
         property DesignTimeEnabled : Boolean read FDesignTimeEnabled write SetDesignTimeEnabled;

   end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cObjectToDot3 =
      '!!ARBvp1.0'+#13#10+
      'OPTION ARB_position_invariant;'+#13#10+
      'PARAM mvinv[4] = { state.matrix.modelview.inverse };'+
      'PARAM lightPos = program.local[0];'+
      'TEMP temp, light, eye;'+

      // Get light vector in object space
      '   DP4 light.x, mvinv[0], lightPos;'+
      '   DP4 light.y, mvinv[1], lightPos;'+
      '   DP4 light.z, mvinv[2], lightPos;'+
      '   ADD light, light, -vertex.position;'+

      // Normalize the light vector
      '   DP3 temp.x, light, light;'+
      '   RSQ temp, temp.x;'+
      '   MUL light, temp.x, light;'+

      // Scale and bias the light vector for storing in the primary color
      '   ADD temp, light, 1.0;'+
      '   MUL temp, 0.5, temp;'+

      // Output
      '   MOV result.texcoord[0], vertex.texcoord[0];'+
      '   MOV result.texcoord[1], vertex.texcoord[0];'+
      '   MOV result.color, temp;'+
      '   MOV result.color.w, 1.0;'+

      'END';

   cObjectToBasicARBFP =
      '!!ARBvp1.0'+#13#10+
      'OPTION ARB_position_invariant;'+#13#10+
      'PARAM mvinv[4] = { state.matrix.modelview.inverse };'+
      'PARAM mvit[4] = { state.matrix.modelview.invtrans };'+
      'PARAM lightPos = program.local[0];'+
      'TEMP temp, light, eye;'+

      // Get light vector in object space
      '   DP4 light.x, lightPos, mvinv[0];'+
      '   DP4 light.y, lightPos, mvinv[1];'+
      '   DP4 light.z, lightPos, mvinv[2];'+
      '   ADD light, light, -vertex.position;'+
      '   MOV light.w, 0.0;'+

      // Get eye vector
      '   ADD eye, mvit[3], -vertex.position;'+
      '   MOV eye.w, 0.0;'+

      // Output
      '   MOV result.texcoord[0], vertex.texcoord[0];'+
      '   MOV result.texcoord[1], light;'+
      '   MOV result.texcoord[2], eye;'+

      'END';

   cTangentQuaternionToDot3 =
      '!!ARBvp1.0'+
      'OPTION ARB_position_invariant;'+#13#10+
      'PARAM c0 = { 0, 0, 1, 0 };'+
      'PARAM c1 = { 0.5, 0.0, 0, 0 };'+
      'TEMP R0, R1, R2;'+
      'ATTRIB v24 = vertex.texcoord[0];'+
      'ATTRIB v18 = vertex.normal;'+
      'ATTRIB v16 = vertex.position;'+
      'PARAM lightPos = program.local[0];'+
      'PARAM s359[4] = { state.matrix.modelview[0].inverse };'+
      '   MOV result.texcoord[0], v24;'+
      '   MOV result.texcoord[1], v24;'+
      '   MOV R1, s18;'+
      '   DP4 R0.x, s359[0], R1;'+
      '   DP4 R0.y, s359[1], R1;'+
      '   DP4 R0.z, s359[2], R1;'+
      '   DP4 R0.w, s359[3], R1;'+
      '   ADD R1, R0, -v16;'+
      '   DP4 R0.x, R1, R1;'+
      '   RSQ R0.x, R0.x;'+
      '   MUL R2.xyz, R0.x, R1;'+
      '   ADD R1.y, c0.x, -v18.x;'+
      '   MOV R1.z, c0.x;'+
      '   MOV R1.x, v18.yzzz;'+
      '   DP3 R0.x, R1.xyzx, R2.xyzx;'+
      '   MUL R0.x, R1.y, R2.z;'+
      '   MAD R0.y, v18.z, R2.x, R0.x;'+
      '   MUL R0.x, v18.y, R2.z;'+
      '   MAD R0.z, v18.z, R2.y, -R0.x;'+
      '   MUL R0.x, v18.y, R2.y;'+
      '   MAD R0.x, v18.z, R2.z, R0.x;'+
      '   MAD R0.w, -R1.y, R2.x, R0.x;'+
      '   ADD R0.xyz, R0.yzwy, c0.z;'+
      '   MUL result.color.xyz, R0.xyzx, c1.x;'+
      '   MOV result.color.w, c0.zxxz;'+
      'END';

   cTangentExternalToDot3 =
      '!!ARBvp1.0'+#13#10+
      'OPTION ARB_position_invariant;'+#13#10+
      'PARAM mvinv[4] = { state.matrix.modelview.inverse };'+
      'PARAM lightPos = program.local[0];'+
      'ATTRIB tangent = vertex.texcoord[1];'+
      'ATTRIB binormal = vertex.texcoord[2];'+
      'ATTRIB normal = vertex.normal;'+
      'TEMP temp, light, eye;'+

      // Get light position in object space
      '   DP4 light.x, mvinv[0], lightPos;'+
      '   DP4 light.y, mvinv[1], lightPos;'+
      '   DP4 light.z, mvinv[2], lightPos;'+
      '   ADD light, light, -vertex.position;'+

      // Transform into tangent space
      '   DP3 temp.x, light, tangent;'+
      '   DP3 temp.y, light, binormal;'+
      '   DP3 temp.z, light, normal;'+
      '   MOV light, temp;'+

      // Normalize the light vector
      '   DP3 temp.x, light, light;'+
      '   RSQ temp, temp.x;'+
      '   MUL light, temp.x, light;'+

      // Pack the light vector into the primary color for the
      // dot3 texture combiner operation
      '   ADD temp, light, 1.0;'+
      '   MUL temp, 0.5, temp;'+

      // Output
      '   MOV result.texcoord[0], vertex.texcoord[0];'+
      '   MOV result.texcoord[1], vertex.texcoord[0];'+
      '   MOV result.color, temp;'+
      '   MOV result.color.w, 1.0;'+

      'END';

   cTangentExternalToBasicARBFP =
      '!!ARBvp1.0'+#13#10+
      'OPTION ARB_position_invariant;'+#13#10+
      'PARAM mvinv[4] = { state.matrix.modelview.inverse };'+
      'PARAM mvit[4] = { state.matrix.modelview.invtrans };'+
      'PARAM lightPos = program.local[0];'+
      'ATTRIB tangent = vertex.texcoord[1];'+
      'ATTRIB binormal = vertex.texcoord[2];'+
      'ATTRIB normal = vertex.normal;'+
      'TEMP temp, light, eye;'+

      // Get light vector in object space
      '   DP4 light.x, lightPos, mvinv[0];'+
      '   DP4 light.y, lightPos, mvinv[1];'+
      '   DP4 light.z, lightPos, mvinv[2];'+
      '   ADD light, light, -vertex.position;'+

      // Transform light vector into tangent space
      '   DP3 temp.x, light, tangent;'+
      '   DP3 temp.y, light, binormal;'+
      '   DP3 temp.z, light, normal;'+
      '   MOV light, temp;'+
      '   MOV light.w, 0.0;'+

      // Get eye vector in object space
      '   ADD eye, mvit[3], -vertex.position;'+

      // Transform eye vector into tangent space
      '   DP3 temp.x, eye, tangent;'+
      '   DP3 temp.y, eye, binormal;'+
      '   DP3 temp.z, eye, normal;'+
      '   MOV eye, temp;'+
      '   MOV eye.w, 0.0;'+

      // Output
      '   MOV result.texcoord[0], vertex.texcoord[0];'+
      '   MOV result.texcoord[1], light;'+
      '   MOV result.texcoord[2], eye;'+

      'END';

   cBasicARBFP =
      '!!ARBfp1.0'+#13#10+
      'PARAM lightDiffuse = program.local[0];'+
      'PARAM lightSpecular = program.local[1];'+
      'PARAM materialDiffuse = state.material.diffuse;'+
      'PARAM materialSpecular = state.material.specular;'+
      'PARAM shininess = state.material.shininess;'+
      'TEMP temp, tex, light, eye, normal, col, diff, spec;'+

      // Get the normalized normal vector
      '   TEX normal, fragment.texcoord[0], texture[0], 2D;'+
      '   MAD normal, normal, 2.0, -1.0;'+
      '   DP3 temp, normal, normal;'+
      '   RSQ temp, temp.x;'+
      '   MUL normal, normal, temp.x;'+

      // Get the normalized light vector
      '   DP3 light, fragment.texcoord[1], fragment.texcoord[1];'+
      '   RSQ light, light.x;'+
      '   MUL light, fragment.texcoord[1], light.x;'+

      // Get the normalized half vector (eye+light)
      '   DP3 eye, fragment.texcoord[2], fragment.texcoord[2];'+
      '   RSQ eye, eye.x;'+
      '   MUL eye, fragment.texcoord[2], eye.x;'+
      '   ADD eye, eye, light;'+
      '   DP3 temp, eye, eye;'+
      '   RSQ temp, temp.x;'+
      '   MUL eye, eye, temp.x;'+

      // Calculate the diffuse color
      '   DP3 diff, normal, light;'+
      '   MUL diff, diff, lightDiffuse;'+
      '   MUL diff, diff, materialDiffuse;'+

      // Calculate the specular color
      '   DP3 spec, normal, eye;'+
      '   POW spec, spec.x, shininess.x;'+
      '   MUL spec, spec, materialSpecular;'+
      '   MUL spec, spec, lightSpecular;'+

      // Output
      '   ADD_SAT result.color, diff, spec;'+
      '   MOV result.color.w, 1.0;'+

      'END';

   cTexturedARBFP =
      '!!ARBfp1.0'+#13#10+
      'PARAM lightDiffuse = program.local[0];'+
      'PARAM lightSpecular = program.local[1];'+
      'PARAM materialDiffuse = state.material.diffuse;'+
      'PARAM materialSpecular = state.material.specular;'+
      'PARAM shininess = state.material.shininess;'+
      'TEMP temp, tex, light, eye, normal, col, diff, spec, textureDiffuse;'+

      // Get the normalized normal vector
      '   TEX normal, fragment.texcoord[0], texture[0], 2D;'+
      '   MAD normal, normal, 2.0, -1.0;'+
      '   DP3 temp, normal, normal;'+
      '   RSQ temp, temp.x;'+
      '   MUL normal, normal, temp.x;'+

      // Get the normalized light vector
      '   DP3 light, fragment.texcoord[1], fragment.texcoord[1];'+
      '   RSQ light, light.x;'+
      '   MUL light, fragment.texcoord[1], light.x;'+

      // Get the normalized half vector (eye+light)
      '   DP3 eye, fragment.texcoord[2], fragment.texcoord[2];'+
      '   RSQ eye, eye.x;'+
      '   MUL eye, fragment.texcoord[2], eye.x;'+
      '   ADD eye, eye, light;'+
      '   DP3 temp, eye, eye;'+
      '   RSQ temp, temp.x;'+
      '   MUL eye, eye, temp.x;'+

      // Get the diffuse texture color
      '   TEX textureDiffuse, fragment.texcoord[0], texture[1], 2D;'+

      // Calculate the diffuse color
      '   DP3 diff, normal, light;'+
      '   MUL diff, diff, lightDiffuse;'+
      '   MUL diff, diff, materialDiffuse;'+
      '   MUL diff, diff, textureDiffuse;'+

      // Calculate the specular color
      '   DP3 spec, normal, eye;'+
      '   POW spec, spec.x, shininess.x;'+
      '   MUL spec, spec, materialSpecular;'+
      '   MUL spec, spec, lightSpecular;'+

      // Output
      '   ADD_SAT result.color, diff, spec;'+
      '   MOV result.color.w, 1.0;'+

      'END';

// Register
//
procedure Register;
begin
  RegisterComponents('GLScene Shaders', [TGLBumpShader]);
end;

// ------------------
// ------------------ TGLBumpShader ------------------
// ------------------

// Create
//
constructor TGLBumpShader.Create(AOwner : TComponent);
begin
   inherited;
   FLightIDs:=TIntegerList.Create;
   FBumpMethod:=bmDot3TexCombiner;
   FBumpSpace:=bsObject;
   FBumpOptions:=[];
   ShaderStyle:=ssLowLevel;
end;

// Destroy
//
destructor TGLBumpShader.Destroy;
begin
   DeleteVertexPrograms;
   DeleteFragmentPrograms;
   FLightIDs.Free;
   inherited;
end;

// Loaded
//
procedure TGLBumpShader.Loaded;
begin
   inherited;
end;

// DoLightPass
//
procedure TGLBumpShader.DoLightPass(lightID : Cardinal);
var
   lightDiffuse, materialDiffuse : TColorVector;
   dummyHandle, tempHandle : Integer;
   light : TVector;
begin
   glEnable(GL_VERTEX_PROGRAM_ARB);
   glBindProgramARB(GL_VERTEX_PROGRAM_ARB, FVertexProgramHandles[0]);
   glGetLightfv(GL_LIGHT0+FLightIDs[0], GL_POSITION, @light[0]);
   glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 0, @light[0]);

   case FBumpMethod of
      bmDot3TexCombiner : begin
         glActiveTextureARB(GL_TEXTURE0_ARB);
         glGetIntegerv(GL_TEXTURE_BINDING_2D, @dummyHandle);
         glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
         glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_DOT3_RGB_ARB);
         glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_TEXTURE0_ARB);
         glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_PRIMARY_COLOR_ARB);

         glActiveTextureARB(GL_TEXTURE1_ARB);
         glEnable(GL_TEXTURE_2D);
         glGetIntegerv(GL_TEXTURE_BINDING_2D, @tempHandle);
         if tempHandle = 0 then
            glBindTexture(GL_TEXTURE_2D, dummyHandle);
         glGetLightfv(GL_LIGHT0+FLightIDs[0], GL_DIFFUSE, @lightDiffuse);
         glGetMaterialfv(GL_FRONT, GL_DIFFUSE, @materialDiffuse);
         lightDiffuse[0]:=lightDiffuse[0]*materialDiffuse[0];
         lightDiffuse[1]:=lightDiffuse[1]*materialDiffuse[1];
         lightDiffuse[2]:=lightDiffuse[2]*materialDiffuse[2];
         lightDiffuse[3]:=lightDiffuse[3]*materialDiffuse[3];
         glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @lightDiffuse);
         glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
         glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_MODULATE);
         glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_PREVIOUS_ARB);
         glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_CONSTANT_COLOR_ARB);

         glActiveTextureARB(GL_TEXTURE0_ARB);
      end;

      bmBasicARBFP : begin
         glEnable(GL_FRAGMENT_PROGRAM_ARB);
         glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, FFragmentProgramHandles[0]);
         glGetLightfv(GL_LIGHT0+FLightIDs[0], GL_DIFFUSE, @light[0]);
         glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_ARB, 0, @light[0]);
         glGetLightfv(GL_LIGHT0+FLightIDs[0], GL_SPECULAR, @light[0]);
         glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_ARB, 1, @light[0]);
      end;

  else
     Assert(False, 'Invalid bump method!');
  end;
end;

// DoApply
//
procedure TGLBumpShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);

   procedure LoadARBProgram(target : GLenum; vptext : String; var vphandle : cardinal);
   var
      errPos : Integer;
      errString : String;
   begin
      if (target = GL_VERTEX_PROGRAM_ARB) and not GL_ARB_vertex_program then
         raise Exception.Create('GL_ARB_vertex_program required!');
      if (target = GL_FRAGMENT_PROGRAM_ARB) and not GL_ARB_fragment_program then
         raise Exception.Create('GL_ARB_fragment_program required!');
      glGenProgramsARB(1, @vphandle);
      glBindProgramARB(target, vphandle);
      glProgramStringARB(target, GL_PROGRAM_FORMAT_ASCII_ARB,
         Length(vptext), PChar(vptext));
      glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, @errPos);
      if errPos>-1 then begin
         errString:=glGetString(GL_PROGRAM_ERROR_STRING_ARB);
         raise Exception.CreateFmt('ARB Program Error - [Handle: %d][Pos: %d][Error %s]', [vphandle, errPos, errString]);
      end;
      CheckOpenGLError;
   end;

var
   maxLights, maxTextures, i : Integer;
   lightEnabled : GLboolean;
   ambient, materialAmbient : TColorVector;
   success : Boolean;
begin
   if (csDesigning in ComponentState) and not DesignTimeEnabled then exit;
   if not Enabled then exit;

   glGetIntegerv(GL_MAX_LIGHTS, @maxLights);
   glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @maxTextures);

   success:=False;
   try
      if not GL_ARB_multitexture then
         raise Exception.Create('This shader requires GL_ARB_multitexture.');
      if  (maxTextures<3) and ((BumpMethod<>bmDot3TexCombiner) or (BumpSpace=bsTangentExternal)) then
         raise Exception.Create('The current shader settings require 3 or more texture units.');

      if Length(FVertexProgramHandles) = 0 then begin
         SetLength(FVertexProgramHandles, 1);
         case FBumpSpace of
            bsObject : begin
               case FBumpMethod of
                  bmDot3TexCombiner :
                     LoadARBProgram(GL_VERTEX_PROGRAM_ARB, cObjectToDot3, FVertexProgramHandles[0]);
                  bmBasicARBFP :
                     LoadARBProgram(GL_VERTEX_PROGRAM_ARB, cObjectToBasicARBFP, FVertexProgramHandles[0]);
               end;
            end;
            bsTangentQuaternion :
               LoadARBProgram(GL_VERTEX_PROGRAM_ARB, cTangentQuaternionToDot3, FVertexProgramHandles[0]);
            bsTangentExternal : begin
               case FBumpMethod of
                  bmDot3TexCombiner :
                     LoadARBProgram(GL_VERTEX_PROGRAM_ARB, cTangentExternalToDot3, FVertexProgramHandles[0]);
                  bmBasicARBFP :
                     LoadARBProgram(GL_VERTEX_PROGRAM_ARB, cTangentExternalToBasicARBFP, FVertexProgramHandles[0]);
               end;
            end;
         end;
      end;

      if Length(FFragmentProgramHandles) = 0 then
         if FBumpMethod = bmBasicARBFP then begin
            SetLength(FFragmentProgramHandles, 1);
            if boDiffuseTexture2 in FBumpOptions then
               LoadARBProgram(GL_FRAGMENT_PROGRAM_ARB, cTexturedARBFP, FFragmentProgramHandles[0])
            else
               LoadARBProgram(GL_FRAGMENT_PROGRAM_ARB, cBasicARBFP, FFragmentProgramHandles[0]);
         end;

      success:=True;

   finally
      if not success then begin
         Enabled:=False;
         DesignTimeEnabled:=False;
      end;
   end;

   glPushAttrib(GL_ENABLE_BIT or
                GL_TEXTURE_BIT or
                GL_DEPTH_BUFFER_BIT or
                GL_COLOR_BUFFER_BIT);

   FLightIDs.Clear;
   glActiveTextureARB(GL_TEXTURE0_ARB);
   if glIsEnabled(GL_TEXTURE_2D) then
      for i:=0 to maxLights-1 do begin
         glGetBooleanv(GL_LIGHT0+i, @lightEnabled);
         if lightEnabled then
            FLightIDs.Add(i);
   end;
   FLightsEnabled:=FLightIDs.Count;

   FAmbientPass:=False;
   FDiffusePass:=False;

   if FLightIDs.Count>0 then begin

      glDepthFunc(GL_LEQUAL);
      glDisable(GL_BLEND);
      DoLightPass(FLightIDs[0]);
      FLightIDs.Delete(0);

   end else begin

      glDisable(GL_LIGHTING);
      glActiveTextureARB(GL_TEXTURE0_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE1_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE0_ARB);

      glGetFloatv(GL_LIGHT_MODEL_AMBIENT, @ambient);
      glGetMaterialfv(GL_FRONT, GL_AMBIENT, @materialAmbient);
      ambient[0]:=ambient[0]*materialAmbient[0];
      ambient[1]:=ambient[1]*materialAmbient[1];
      ambient[2]:=ambient[2]*materialAmbient[2];
      glColor3fv(@ambient);

      FAmbientPass:=True;

   end;
end;

// DoUnApply
//
function TGLBumpShader.DoUnApply(var rci: TRenderContextInfo) : Boolean;
var
   ambient, materialAmbient : TVector;
begin
   Result:=False;
   if (csDesigning in ComponentState) and not DesignTimeEnabled then exit;
   if not Enabled then exit;

   if FLightIDs.Count>0 then begin

      glDepthFunc(GL_LEQUAL);
      glEnable(GL_BLEND);
      glBlendFunc(GL_ONE, GL_ONE);

      DoLightPass(FLightIDs[0]);
      FLightIDs.Delete(0);
      Result:=True;
      Exit;

   end else if not FDiffusePass and (FLightsEnabled <> 0)
   and (boDiffuseTexture2 in BumpOptions)
   and (BumpMethod = bmDot3TexCombiner) then begin

      glEnable(GL_BLEND);
      glBlendFunc(GL_DST_COLOR, GL_ZERO);
      glActiveTextureARB(GL_TEXTURE1_ARB);
      glEnable(GL_TEXTURE_2D);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
      glActiveTextureARB(GL_TEXTURE0_ARB);
      glDisable(GL_TEXTURE_2D);

      FDiffusePass:=True;
      Result:=True;
      Exit;

   end else if not FAmbientPass then begin

      glDisable(GL_VERTEX_PROGRAM_ARB);
      if BumpMethod = bmBasicARBFP then
         glDisable(GL_FRAGMENT_PROGRAM_ARB);

      glDisable(GL_LIGHTING);
      glActiveTextureARB(GL_TEXTURE0_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE1_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE0_ARB);

      glDepthFunc(GL_LEQUAL);
      glEnable(GL_BLEND);
      glBlendFunc(GL_ONE, GL_ONE);

      glGetFloatv(GL_LIGHT_MODEL_AMBIENT, @ambient);
      glGetMaterialfv(GL_FRONT, GL_AMBIENT, @materialAmbient);
      ambient[0]:=ambient[0]*materialAmbient[0];
      ambient[1]:=ambient[1]*materialAmbient[1];
      ambient[2]:=ambient[2]*materialAmbient[2];
      glColor3fv(@ambient);

      FAmbientPass:=True;
      Result:=True;
      Exit;

   end;

   glDisable(GL_VERTEX_PROGRAM_ARB);
   if BumpMethod = bmBasicARBFP then
      glDisable(GL_FRAGMENT_PROGRAM_ARB);

   glPopAttrib;
end;

// DeleteVertexPrograms
//
procedure TGLBumpShader.DeleteVertexPrograms;
begin
   if Length(FVertexProgramHandles) > 0 then begin
      glDeleteProgramsARB(
        Length(FVertexProgramHandles), @FVertexProgramHandles[0]);
      SetLength(FVertexProgramHandles, 0);
   end;
end;

// DeleteFragmentPrograms
//
procedure TGLBumpShader.DeleteFragmentPrograms;
begin
   if Length(FFragmentProgramHandles) > 0 then begin
      glDeleteProgramsARB(
        Length(FFragmentProgramHandles), @FFragmentProgramHandles[0]);
      SetLength(FFragmentProgramHandles, 0);
   end;
end;

// SetBumpMethod
//
procedure TGLBumpShader.SetBumpMethod(const Value: TBumpMethod);
begin
   if Value<>FBumpMethod then begin
      if FBumpSpace = bsTangentQuaternion then
         FBumpMethod:=bmDot3TexCombiner
      else
         FBumpMethod:=Value;
      DeleteVertexPrograms;
      DeleteFragmentPrograms;
      NotifyChange(Self);
   end;
end;

// SetBumpSpace
//
procedure TGLBumpShader.SetBumpSpace(const Value: TBumpSpace);
begin
   if Value<>FBumpSpace then begin
      FBumpSpace:=Value;
      DeleteVertexPrograms;
      DeleteFragmentPrograms;
      if FBumpSpace = bsTangentQuaternion then
         BumpMethod:=bmDot3TexCombiner;
      NotifyChange(Self);
   end;
end;

// SetBumpOptions
//
procedure TGLBumpShader.SetBumpOptions(const Value: TBumpOptions);
begin
   if Value<>FBumpOptions then begin
      FBumpOptions:=Value;
      DeleteVertexPrograms;
      DeleteFragmentPrograms;
      NotifyChange(Self);
   end;
end;

// SetDesignTimeEnabled
//
procedure TGLBumpShader.SetDesignTimeEnabled(const Value: Boolean);
begin
   if Value<>FDesignTimeEnabled then begin
      FDesignTimeEnabled:=Value;
      NotifyChange(Self);
   end;
end;

end.
