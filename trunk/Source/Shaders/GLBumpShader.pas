{: GLBumpShader<p>

   A shader that applies bump mapping.<p>

   Note:
   The normal map is expected to be the primary texture.
   The secondary texture is currently ignored.<p>

   This unit is still experimental so use at your own risk!<p>

   <b>History : </b><font size=-1><ul>
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
   TBumpMethod = (bmDot3TexCombiner);

   TBumpSpace = (bsObject, bsTangent);

   // TGLBumpShader
   //
   {: A generic bump shader.<p> }
   TGLBumpShader = class (TGLShader)
      private
         FVertexProgramHandles : array of Cardinal;
         FLightIDs : TIntegerList;
         FLightsEnabled : Integer;
         FBumpMethod : TBumpMethod;
         FBumpSpace : TBumpSpace;

      protected
         procedure SetBumpMethod(const Value : TBumpMethod);
         procedure SetBumpSpace(const Value : TBumpSpace);
         procedure Loaded; override;
         procedure DeleteVertexPrograms;

      public
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
         function DoUnApply(var rci: TRenderContextInfo) : Boolean; override;

      published
         property BumpMethod : TBumpMethod read FBumpMethod write SetBumpMethod;
         property BumpSpace : TBumpSpace read FBumpSpace write SetBumpSpace;

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
   cCalcObjectSpaceLightVectorVertexProgram =
      '!!ARBvp1.0'+#13#10+
      'PARAM mvproj[4] = { state.matrix.mvp };'+
      'PARAM mvinv[4] = { state.matrix.modelview[0].inverse };'+
      'PARAM lightPos = state.light[%d].position;'+
      'PARAM vals = { 1.0, 1.0, 0.99999, 0.5 };'+
      'TEMP R0, light;'+

      '   DP4 result.position.x, mvproj[0], vertex.position;'+
      '   DP4 result.position.y, mvproj[1], vertex.position;'+
      '   DP4 R0.z, mvproj[2], vertex.position;'+
      '   MUL result.position.z, R0.z, vals.z;'+
      '   DP4 result.position.w, mvproj[3], vertex.position;'+

      '   MOV result.texcoord[0], vertex.texcoord[0];'+

      '   DP4 light.x, mvinv[0], lightPos;'+
      '   DP4 light.y, mvinv[1], lightPos;'+
      '   DP4 light.z, mvinv[2], lightPos;'+
      '   ADD light, light, -vertex.position;'+
      '   DP3 R0.x, light, light;'+
      '   RSQ R0.x, R0.x;'+
      '   MUL light, R0.x, light;'+

      '   ADD R0, light, vals;'+
      '   MUL R0, vals.w, R0;'+
      '   MOV R0.w, vals.w;'+
      '   MOV result.color, R0;'+
      'END';

   // Cg compiled arbvp1
   cCalcTangentSpaceLightVectorVertexProgram =
      '!!ARBvp1.0'+
      'PARAM c0 = { 0, 0, 1, 0 };'+
      'PARAM c1 = { 0.5, 0.99999, 0, 0 };'+
      'TEMP R0, R1, R2;'+
      'ATTRIB v24 = vertex.texcoord[0];'+
      'ATTRIB v18 = vertex.normal;'+
      'ATTRIB v16 = vertex.position;'+
      'PARAM s18 = state.light[%d].position;'+
      'PARAM s259[4] = { state.matrix.mvp };'+
      'PARAM s359[4] = { state.matrix.modelview[0].inverse };'+
      '   MOV result.texcoord[0].xy, v24;'+
      '   DP4 result.position.x, s259[0], v16;'+
      '   DP4 result.position.y, s259[1], v16;'+
      '   DP4 R1.z, s259[2], v16;'+
      '   MUL result.position.z, R1.z, c1.y;'+
      '   DP4 result.position.w, s259[3], v16;'+
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
      '   MUL result.color.front.primary.xyz, R0.xyzx, c1.x;'+
      '   MOV result.color.front.primary.w, c0.zxxz;'+
      'END';
// Register
//
procedure Register;
begin
  RegisterComponents('GLScene Shaders', [TGLBumpShader]);
end;

// LoadARBVertexProgram
//
procedure LoadARBVertexProgram(VPText : String; var VPHandle : cardinal);
var
   errPos : Integer;
   errString : String;
begin
   if not GL_ARB_vertex_program then
      raise Exception.Create('GL_ARB_vertex_program required!');
   glGenProgramsARB(1, @VPHandle);
   glBindProgramARB(GL_VERTEX_PROGRAM_ARB,VPHandle);
   glProgramStringARB(GL_VERTEX_PROGRAM_ARB,GL_PROGRAM_FORMAT_ASCII_ARB,
      Length(VPText), PChar(VPText));
   glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, @errPos);
   if errPos>-1 then begin
      errString:=glGetString(GL_PROGRAM_ERROR_STRING_ARB);
      raise Exception.Create(PChar(errString));
   end;
   CheckOpenGLError;
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
   ShaderStyle:=ssLowLevel;
end;

// Destroy
//
destructor TGLBumpShader.Destroy;
begin
   DeleteVertexPrograms;
   FLightIDs.Free;
   inherited;
end;

// Loaded
//
procedure TGLBumpShader.Loaded;
begin
   inherited;
end;

// DoApply
//
procedure TGLBumpShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
var
   maxLights, i : Integer;
   lightEnabled : GLboolean;
   ambient, materialAmbient : TColorVector;
begin
   if (csDesigning in ComponentState) then exit;

   glGetIntegerv(GL_MAX_LIGHTS, @maxLights);

   glPushAttrib(GL_ENABLE_BIT or
                GL_TEXTURE_BIT or
                GL_DEPTH_BUFFER_BIT or
                GL_COLOR_BUFFER_BIT);

   if Length(FVertexProgramHandles) = 0 then begin
      SetLength(FVertexProgramHandles, maxLights);
      for i:=0 to maxLights-1 do
        case FBumpSpace of

           bsObject :
              LoadARBVertexProgram(Format(cCalcObjectSpaceLightVectorVertexProgram,[i]),
                                   FVertexProgramHandles[i]);
           bsTangent :
              LoadARBVertexProgram(Format(cCalcTangentSpaceLightVectorVertexProgram,[i]),
                                   FVertexProgramHandles[i]);

        end;
   end;

   FLightIDs.Clear;
   for i:=0 to maxLights-1 do begin
      glGetBooleanv(GL_LIGHT0+i, @lightEnabled);
      if lightEnabled then
         FLightIDs.Add(i);
   end;
   FLightsEnabled:=FLightIDs.Count;

   glDisable(GL_LIGHTING);
   glDisable(GL_TEXTURE_2D);

   glGetFloatv(GL_LIGHT_MODEL_AMBIENT, @ambient);
   glGetMaterialfv(GL_FRONT, GL_AMBIENT, @materialAmbient);
   ambient[0]:=ambient[0]*materialAmbient[0];
   ambient[1]:=ambient[1]*materialAmbient[1];
   ambient[2]:=ambient[2]*materialAmbient[2];
   glColor3fv(@ambient);
end;

// DoUnApply
//
function TGLBumpShader.DoUnApply(var rci: TRenderContextInfo) : Boolean;
var
   lightDiffuse, materialDiffuse : TColorVector;
   dummyHandle : Integer;
begin
   Result:=False;
   if (csDesigning in ComponentState) then exit;

   if FLightIDs.Count>0 then begin
      glDepthFunc(GL_LEQUAL);
      glEnable(GL_BLEND);
      glBlendFunc(GL_ONE, GL_ONE);

      glEnable(GL_VERTEX_PROGRAM_ARB);
      glBindProgramARB(GL_VERTEX_PROGRAM_ARB, FVertexProgramHandles[FLightIDs[0]]);

      case FBumpMethod of
         bmDot3TexCombiner : begin
            glActiveTextureARB(GL_TEXTURE0_ARB);
            glEnable(GL_TEXTURE_2D);
            glGetIntegerv(GL_TEXTURE_BINDING_2D, @dummyHandle);
            glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
            glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_DOT3_RGB_ARB);
            glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_TEXTURE0_ARB);
            glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_PRIMARY_COLOR_ARB);

            glActiveTextureARB(GL_TEXTURE1_ARB);
            glEnable(GL_TEXTURE_2D);
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

         else Assert(False, 'Invalid bump method!');

      end;

      FLightIDs.Delete(0);

      Result:=True;
      Exit;
   end;

   glPopAttrib;
end;

// DeleteVertexPrograms
//
procedure TGLBumpShader.DeleteVertexPrograms;
var
   i : Integer;
begin
   if Length(FVertexProgramHandles) > 0 then begin
      for i:=0 to Length(FVertexProgramHandles)-1 do
         glDeleteProgramsARB(1, @FVertexProgramHandles[i]);
      SetLength(FVertexProgramHandles, 0);
   end;
end;

// SetBumpMethod
//
procedure TGLBumpShader.SetBumpMethod(const Value: TBumpMethod);
begin
   if Value<>FBumpMethod then begin
      FBumpMethod:=Value;
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
      NotifyChange(Self);
   end;
end;

end.
