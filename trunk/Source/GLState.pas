{: GLState<p>

   Miscellaneous support routines & classes.<p>

	<b>History : </b><font size=-1><ul>
      <li>05/09/03 - EG - Creation from GLMisc split
   </ul></font>
}
unit GLState;

interface

uses Classes, VectorGeometry, SysUtils, OpenGL1x;

{$i GLScene.inc}

type

	TGLMinFilter   = (miNearest, miLinear, miNearestMipmapNearest,
							miLinearMipmapNearest, miNearestMipmapLinear,
							miLinearMipmapLinear);
	TGLMagFilter   = (maNearest, maLinear);

	// used to describe what kind of winding has a front face
	TFaceWinding = (fwCounterClockWise, fwClockWise);

	// used to reflect all relevant (binary) states of OpenGL subsystem
	TGLState = (stAlphaTest, stAutoNormal,
					stBlend, stColorMaterial, stCullFace, stDepthTest, stDither,
					stFog, stLighting, stLineSmooth, stLineStipple,
					stLogicOp, stNormalize, stPointSmooth, stPolygonSmooth,
					stPolygonStipple, stScissorTest, stStencilTest,
					stTexture1D, stTexture2D, stTextureCubeMap);
	TGLStates = set of TGLState;

//: Update the GLState machine if necessary
procedure SetGLState(var states : TGLStates; const aState : TGLState);
//: Update the GLState machine if necessary
procedure UnSetGLState(var states : TGLStates; const aState : TGLState);

//: Defines the GLPolygonMode if necessary
procedure SetGLPolygonMode(const aFace, mode : TGLEnum);
//: Reset GLPolygonMode, next calls to SetGLPolygonMode WILL do something
procedure ResetGLPolygonMode;

procedure SetGLMaterialColors(const aFace : TGLEnum;
                        const emission, ambient, diffuse, specular : PGLFloat;
                        const shininess : Integer);
procedure SetGLMaterialAlphaChannel(const aFace : TGLEnum; const alpha : TGLFloat);
procedure ResetGLMaterialColors;

procedure SetGLCurrentTexture(const textureUnit, target, handle : Integer);
procedure ResetGLCurrentTexture;

{: Defines the OpenGL texture matrix.<p>
   Assumed texture mode is GL_MODELVIEW. }
procedure SetGLTextureMatrix(const matrix : TMatrix);
{: Resets the OpenGL texture matrix to Identity.<p>
   Assumed texture mode is GL_MODELVIEW. }
procedure ResetGLTextureMatrix;

{: Inverts front face winding (CCW/CW). }
procedure InvertGLFrontFace;
{: Reset to default front face winding (CCW). }
procedure ResetGLFrontFace;
{: Set front face winding to ClockWise. }
procedure SetGLFrontFaceCW;
{: Set front face winding to Counter-ClockWise. }
procedure SetGLFrontFaceCCW;

//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------
implementation
//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------

const
	cGLStateToGLEnum : array [stAlphaTest..stTextureCubeMap] of TGLEnum =
		(GL_ALPHA_TEST, GL_AUTO_NORMAL, GL_BLEND, GL_COLOR_MATERIAL, GL_CULL_FACE,
		 GL_DEPTH_TEST, GL_DITHER, GL_FOG, GL_LIGHTING, GL_LINE_SMOOTH,
		 GL_LINE_STIPPLE, GL_LOGIC_OP, GL_NORMALIZE, GL_POINT_SMOOTH,
		 GL_POLYGON_SMOOTH, GL_POLYGON_STIPPLE, GL_SCISSOR_TEST, GL_STENCIL_TEST,
		 GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_CUBE_MAP_ARB);

// SetGLPolygonMode
//
var
   vLastFrontMode, vLastBackMode : TGLEnum;
procedure SetGLPolygonMode(const aFace, mode : TGLEnum);
begin
   case aFace of
      GL_FRONT :
         if mode<>vLastFrontMode then begin
            glPolygonMode(aFace, mode);
            vLastFrontMode:=mode;
         end;
      GL_BACK :
         if mode<>vLastBackMode then begin
            glPolygonMode(aFace, mode);
            vLastBackMode:=mode;
         end;
      GL_FRONT_AND_BACK :
         if (mode<>vLastFrontMode) or (mode<>vLastBackMode) then begin
            glPolygonMode(aFace, mode);
            vLastFrontMode:=mode;
            vLastBackMode:=mode;
         end;
   end;
end;

// ResetGLPolygonMode
//
procedure ResetGLPolygonMode;
begin
   vLastFrontMode:=0;
   vLastBackMode:=0;
end;

// SetGLMaterialColors
//
type
   THomogeneousFltVectorArray = array [0..3] of THomogeneousFltVector;
   PHomogeneousFltVectorArray = ^THomogeneousFltVectorArray;
var
   vFrontColors, vBackColors : THomogeneousFltVectorArray;
   vFrontShininess, vBackShininess : Integer;
procedure SetGLMaterialColors(const aFace : TGLEnum;
                              const emission, ambient, diffuse, specular : PGLFloat;
                              const shininess : Integer);
var
   ar : PHomogeneousFltVectorArray;
begin
   if aFace=GL_FRONT then begin
      ar:=@vFrontColors;
      if vFrontShininess<>shininess then begin
       	glMateriali(AFace, GL_SHININESS, shininess);
         vFrontShininess:=shininess;
      end;
   end else begin
      ar:=@vBackColors;
      if vBackShininess<>shininess then begin
       	glMateriali(AFace, GL_SHININESS, shininess);
         vBackShininess:=shininess;
      end;
   end;
   if not VectorEquals(PAffineVector(@ar[0])^, PAffineVector(emission)^) then begin
     	glMaterialfv(aFace, GL_EMISSION, emission);
      SetVector(ar[0], PHomogeneousFltVector(emission)^);
   end;
   if not VectorEquals(PAffineVector(@ar[1])^, PAffineVector(ambient)^) then begin
     	glMaterialfv(aFace, GL_AMBIENT, ambient);
      SetVector(ar[1], PHomogeneousFltVector(ambient)^);
   end;
   if not VectorEquals(PVector(@ar[2])^, PVector(diffuse)^) then begin
     	glMaterialfv(aFace, GL_DIFFUSE, diffuse);
      SetVector(ar[2], PHomogeneousFltVector(diffuse)^);
   end;
   if not VectorEquals(PAffineVector(@ar[3])^, PAffineVector(specular)^) then begin
     	glMaterialfv(aFace, GL_SPECULAR, specular);
      SetVector(ar[3], PHomogeneousFltVector(specular)^);
   end;
end;

// SetGLMaterialAlphaChannel
//
procedure SetGLMaterialAlphaChannel(const aFace : TGLEnum; const alpha : TGLFloat);
var
   ar : PHomogeneousFltVectorArray;
begin
   if aFace=GL_FRONT then
      ar:=@vFrontColors
   else ar:=@vBackColors;
   if ar[2][3]<>alpha then begin
      ar[2][3]:=alpha;
     	glMaterialfv(aFace, GL_DIFFUSE, @ar[2]);
   end;
end;

// ResetGLMaterialColors
//
procedure ResetGLMaterialColors;
const
   clrBlack  : TVector = (0,    0,    0,    1);
   clrGray20 : TVector = (0.20, 0.20, 0.20, 1);
   clrGray80 : TVector = (0.80, 0.80, 0.80, 1);
begin
  	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @clrGray20);
  	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @clrGray80);
  	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @clrBlack);
  	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @clrBlack);
 	glMateriali(GL_FRONT_AND_BACK,  GL_SHININESS, 0);
   FillChar(vFrontColors, SizeOf(THomogeneousFltVectorArray), 127);
   FillChar(vBackColors, SizeOf(THomogeneousFltVectorArray), 127);
   vFrontShininess:=0;
   vBackShininess:=0;
end;

// SetGLCurrentTexture
//
var
   lastTextureHandle : array [0..7] of Integer;
procedure SetGLCurrentTexture(const textureUnit, target, handle : Integer);
begin
   if handle<>lastTextureHandle[textureUnit] then begin
      glBindTexture(target, Handle);
      lastTextureHandle[textureUnit]:=handle;
   end;
end;

// ResetGLCurrentTexture
//
procedure ResetGLCurrentTexture;
var
   i : Integer;
begin
   for i:=0 to 7 do
      lastTextureHandle[i]:=-1;
end;

// SetGLTextureMatrix
//
var
   vTextureMatrixIsIdenty : Boolean = True;
procedure SetGLTextureMatrix(const matrix : TMatrix);
begin
   vTextureMatrixIsIdenty:=False;
   glMatrixMode(GL_TEXTURE);
   glLoadMatrixf(PGLFloat(@matrix[0][0]));
   glMatrixMode(GL_MODELVIEW);
end;

// ResetGLTextureMatrix
//
procedure ResetGLTextureMatrix;
begin
   if not vTextureMatrixIsIdenty then begin
      glMatrixMode(GL_TEXTURE);
      glLoadIdentity;
      glMatrixMode(GL_MODELVIEW);
      vTextureMatrixIsIdenty:=True;
   end;
end;

// InvertGLFrontFace
//
var
   vFrontFaceCCW : Boolean = True;
procedure InvertGLFrontFace;
begin
   vFrontFaceCCW:=not vFrontFaceCCW;
   if vFrontFaceCCW then
      glFrontFace(GL_CCW)
   else glFrontFace(GL_CW);
end;

// ResetGLFrontFace
//
procedure ResetGLFrontFace;
begin
   glFrontFace(GL_CCW);
   vFrontFaceCCW:=True;
end;

// SetGLFrontFaceCW
//
procedure SetGLFrontFaceCW;
begin
   if vFrontFaceCCW then begin
      glFrontFace(GL_CW);
      vFrontFaceCCW:=False;
   end;
end;

// SetGLFrontFaceCCW
//
procedure SetGLFrontFaceCCW;
begin
   if not vFrontFaceCCW then begin
      glFrontFace(GL_CCW);
      vFrontFaceCCW:=True;
   end;
end;

// SetGLState
//
procedure SetGLState(var states : TGLStates; const aState : TGLState);
begin
	if not (aState in states) then begin
		glEnable(cGLStateToGLEnum[aState]);
		Include(states, aState);
	end;
end;

// UnSetGLState
//
procedure UnSetGLState(var states : TGLStates; const aState : TGLState);
begin
	if (aState in states) then begin
		glDisable(cGLStateToGLEnum[aState]);
		Exclude(states, aState);
	end;
end;

end.
