{: GLCelShader<p>

   A shader that applies cel shading through a vertex program
   and shade definition texture.<p>

   <b>History : </b><font size=-1><ul>
      <li>28/05/04 - SG - Creation.
   </ul></font>
}
unit GLCelShader;

interface

uses
   Classes, SysUtils, GLTexture, GLContext, GLGraphics, GLUtils,
   VectorGeometry, OpenGL1x;

type
   // TGLCelShaderOption
   //
   {: Cel shading options.<p>
      csoOutlines: Render a second outline pass.
      csoTextured: Allows for a primary texture that the cel shading
                   is modulated with and forces the shade definition
                   to render as a second texture. }
   TGLCelShaderOption = (csoOutlines, csoTextured, csoNoBuildShadeTexture);
   TGLCelShaderOptions = set of TGLCelShaderOption;

   // TGLCelShaderGetIntensity
   //
   //: An event for user defined cel intensity.
   TGLCelShaderGetIntensity = procedure (Sender : TObject; var intensity : Byte) of object;

   // TGLCelShader
   //
   {: A generic cel shader.<p> }
   TGLCelShader = class (TGLShader)
      private
         FOutlineWidth : Single;
         FCelShaderOptions : TGLCelShaderOptions;
         FVertexProgramHandle,
         FVertexProgram2Handle : cardinal;
         FShadeTexture : TGLTexture;
         FOnGetIntensity : TGLCelShaderGetIntensity;
         FLightPosition : TVector;
         FOutlinePass,
         FUnApplyShadeTexture : Boolean;
    procedure SetLightPosition(const Value: TVector);

      protected
         procedure SetCelShaderOptions(const val : TGLCelShaderOptions);
         procedure SetOutlineWidth(const val : Single);
         procedure BuildShadeTexture;
         procedure Loaded; override;

      public
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
         function DoUnApply(var rci: TRenderContextInfo) : Boolean; override;

         property LightPosition : TVector read FLightPosition write SetLightPosition;
         property ShadeTexture : TGLTexture read FShadeTexture;

      published
         property CelShaderOptions : TGLCelShaderOptions read FCelShaderOptions write SetCelShaderOptions;
         property OutlineWidth : Single read FOutlineWidth write SetOutlineWidth;
         property OnGetIntensity : TGLCelShaderGetIntensity read FOnGetIntensity write FOnGetIntensity;
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
   cDotToTex1DVertexProgram =
      '!!ARBvp1.0'+#13#10+
      'TEMP R0, R1;'+#13#10+
      'ATTRIB v19 = vertex.color;'+#13#10+
      'ATTRIB v18 = vertex.normal;'+#13#10+
      'ATTRIB v16 = vertex.position;'+#13#10+
      'PARAM c4 = program.local[4];'+#13#10+
      'PARAM c0[4] = { program.local[0..3] };'+#13#10+
      '  MOV result.color.front.primary, v19;'+#13#10+
      '  DP4 result.position.x, c0[0], v16;'+#13#10+
      '  DP4 result.position.y, c0[1], v16;'+#13#10+
      '  DP4 result.position.z, c0[2], v16;'+#13#10+
      '  DP4 result.position.w, c0[3], v16;'+#13#10+
      '  ADD R1, c4, -v16;'+#13#10+
      '  DP4 R0.x, R1, R1;'+#13#10+
      '  RSQ R0.x, R0.x;'+#13#10+
      '  MUL R0, R0.x, R1;'+#13#10+
      '  DP4 result.texcoord[0].x, v18, R0;'+#13#10+
      'END';

   cDotToTex1DVertexProgramWithTexture =
      '!!ARBvp1.0'+#13#10+
      'TEMP R0, R1;'+#13#10+
      'ATTRIB v24 = vertex.texcoord[0];'+#13#10+
      'ATTRIB v18 = vertex.normal;'+#13#10+
      'ATTRIB v16 = vertex.position;'+#13#10+
      'PARAM c4 = program.local[4];'+#13#10+
      'PARAM c0[4] = { program.local[0..3] };'+#13#10+
      '  MOV result.texcoord[0].xyz, v24;'+#13#10+
      '  DP4 result.position.x, c0[0], v16;'+#13#10+
      '  DP4 result.position.y, c0[1], v16;'+#13#10+
      '  DP4 result.position.z, c0[2], v16;'+#13#10+
      '  DP4 result.position.w, c0[3], v16;'+#13#10+
      '  ADD R1, c4, -v16;'+#13#10+
      '  DP4 R0.x, R1, R1;'+#13#10+
      '  RSQ R0.x, R0.x;'+#13#10+
      '  MUL R0, R0.x, R1;'+#13#10+
      '  DP4 result.texcoord[1].x, v18, R0;'+#13#10+
      'END';

// Register
//
procedure Register;
begin
  RegisterComponents('GLScene Shaders', [TGLCelShader]);
end;


// ------------------
// ------------------ TGLUserShader ------------------
// ------------------

// Create
//
constructor TGLCelShader.Create(AOwner : TComponent);
begin
   inherited;

   FOutlineWidth:=3;
   FCelShaderOptions:=[csoOutlines];
   FLightPosition:=NullHMGPoint;
   FShadeTexture:=TGLTexture.Create(Self);
   with FShadeTexture do begin
      Enabled:=True;
      MinFilter:=miNearest;
      MagFilter:=maNearest;
      TextureWrap:=twNone;
      TextureMode:=tmModulate;
   end;

   ShaderStyle:=ssLowLevel;
end;

// Destroy
//
destructor TGLCelShader.Destroy;
begin
   if FVertexProgramHandle > 0 then
      glDeleteProgramsARB(1, @FVertexProgramHandle);
   FShadeTexture.Free;
   inherited;
end;

// Loaded
//
procedure TGLCelShader.Loaded;
begin
   inherited;
   BuildShadeTexture;
end;

// BuildShadeTexture
//
procedure TGLCelShader.BuildShadeTexture;
var
   bmp32 : TGLBitmap32;
   i : Integer;
   intensity : Byte;
begin
   if csoNoBuildShadeTexture in FCelShaderOptions then exit;
   
   with FShadeTexture do begin
      ImageClassName:='TGLBlankImage';
      TGLBlankImage(Image).Width:=128;
      TGLBlankImage(Image).Height:=1;
   end;

   bmp32:=FShadeTexture.Image.GetBitmap32(GL_TEXTURE_2D);
   for i:=0 to bmp32.Width-1 do begin
      intensity:=i*(256 div bmp32.Width);

      if Assigned(FOnGetIntensity) then
         FOnGetIntensity(Self, intensity)
      else begin
         if intensity>230 then intensity:=255
         else if intensity>150 then intensity:=230
         else if intensity>100 then intensity:=intensity+50
         else intensity:=150;
      end;

      bmp32.ScanLine[0][i].r:=intensity;
      bmp32.ScanLine[0][i].g:=intensity;
      bmp32.ScanLine[0][i].b:=intensity;
      bmp32.ScanLine[0][i].a:=1;
   end;
end;

// DoApply
//
procedure TGLCelShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);

   procedure LoadVertexProgram(VPText : String; var VPHandle : cardinal);
   var
      errPos : Integer;
      errString : String;
   begin
      if not GL_ARB_vertex_program then begin
         Exception.Create('GL_ARB_vertex_program required!');
         Exit;
      end;
      glGenProgramsARB(1, @VPHandle);
      glBindProgramARB(GL_VERTEX_PROGRAM_ARB,VPHandle);
      glProgramStringARB(GL_VERTEX_PROGRAM_ARB,GL_PROGRAM_FORMAT_ASCII_ARB,
         Length(VPText), PChar(VPText));
      glGetIntegerv(OpenGL1x.GL_PROGRAM_ERROR_POSITION_ARB, @errPos);
      if errPos>-1 then begin
         errString:=glGetString(GL_PROGRAM_ERROR_STRING_ARB);
         Exception.Create(PChar(errString));
      end;
      CheckOpenGLError;
   end;

var
   mat,modelview,projection : TMatrix;
   light : TVector;
   VertexProgram : Cardinal;
begin
   if (csDesigning in ComponentState) then exit;

   if not (csoTextured in FCelShaderOptions) then begin
      if FVertexProgramHandle = 0 then
         LoadVertexProgram(cDotToTex1DVertexProgram, FVertexProgramHandle);
      VertexProgram:=FVertexProgramHandle;
   end else begin
      if FVertexProgram2Handle = 0 then
         LoadVertexProgram(cDotToTex1DVertexProgramWithTexture, FVertexProgram2Handle);
      VertexProgram:=FVertexProgram2Handle;
   end;

   glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_COLOR_BUFFER_BIT
      or GL_HINT_BIT or GL_LINE_BIT or GL_POLYGON_BIT or GL_DEPTH_BUFFER_BIT);

   glDisable(GL_LIGHTING);

   glEnable(GL_VERTEX_PROGRAM_ARB);
   glBindProgramARB(GL_VERTEX_PROGRAM_ARB,VertexProgram);

   glGetFloatv(GL_MODELVIEW_MATRIX,@modelview[0][0]);
   glGetFloatv(GL_PROJECTION_MATRIX,@projection[0][0]);

   mat:=MatrixMultiply(modelview, projection);
   TransposeMatrix(mat);
   glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB,0,@mat[0][0]);
   glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB,1,@mat[1][0]);
   glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB,2,@mat[2][0]);
   glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB,3,@mat[3][0]);

   mat:=rci.modelViewMatrix^;
   InvertMatrix(mat);
   mat:=MatrixMultiply(modelview, mat);
   InvertMatrix(Mat);
   light:=VectorTransform(FLightPosition, mat);
   glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB,4,@light[0]);

   if (csoTextured in FCelShaderOptions) then
      FShadeTexture.ApplyAsTexture2(rci, nil)
   else begin
      FShadeTexture.Apply(rci);
      if Assigned(Sender) then
         if Sender is TGLLibMaterial then
            glColor4fv(TGLLibMaterial(Sender).Material.FrontProperties.Diffuse.AsAddress);
   end;

   FOutlinePass:=csoOutlines in FCelShaderOptions;
   FUnApplyShadeTexture:=True;
end;

// DoUnApply
//
function TGLCelShader.DoUnApply(var rci: TRenderContextInfo) : Boolean;
begin
   Result:=False;
   if (csDesigning in ComponentState) then exit;

   if FUnApplyShadeTexture then begin
      if (csoTextured in FCelShaderOptions) then
         FShadeTexture.UnApplyAsTexture2(rci, nil)
      else
         FShadeTexture.UnApply(rci);
      FUnApplyShadeTexture:=False;
   end;

   if FOutlinePass then begin
      glDisable(GL_VERTEX_PROGRAM_ARB);
      glDisable(GL_TEXTURE_2D);

      glEnable(GL_BLEND);
      glEnable(GL_LINE_SMOOTH);
      glEnable(GL_CULL_FACE);
      glEnable(GL_COLOR_MATERIAL);

      glPolygonMode(GL_BACK, GL_LINE);
      glCullFace(GL_FRONT);
      glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);
      glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
      glDepthFunc(GL_LESS);
      glColor4f(0,0,0,1);
      glLineWidth(FOutlineWidth);

      Result:=True;
      FOutlinePass:=False;
      Exit;
   end;

   glPopAttrib;
end;

// SetCelShaderOptions
//
procedure TGLCelShader.SetCelShaderOptions(const val: TGLCelShaderOptions);
begin
   if val<>FCelShaderOptions then begin
      FCelShaderOptions:=val;
      BuildShadeTexture;
      NotifyChange(Self);
   end;
end;

// SetOutlineWidth
//
procedure TGLCelShader.SetOutlineWidth(const val : Single);
begin
   if val<>FOutlineWidth then begin
      FOutlineWidth:=val;
      NotifyChange(Self);
   end;
end;

// SetLightPosition
//
procedure TGLCelShader.SetLightPosition(const Value: TVector);
begin
  FLightPosition := Value;
end;

end.
