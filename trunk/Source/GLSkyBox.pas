{: GLSkyBox<p>

   A TGLImmaterialSceneObject drawing 6 quads (plus another quad as "Cloud" plane)
   for use as a skybox always centered on the camera.<p>

	<b>History : </b><font size=-1><ul>
      <li>27/11/03 - EG - Cleanup and fixes
      <li>09/11/03 - MRQZZZ - mandatory changes suggested by Eric.
      <li>02/09/03 - MRQZZZ - Creation
   </ul></font>
}
unit GLSkyBox;

interface

uses
   Classes, GLScene, GLTexture, VectorTypes, VectorGeometry, OpenGL1x, GLMisc,
   XOpenGL;

type

   // TGLSkyBox
   //
   TGLSkyBox = class(TGLImmaterialSceneObject)
	   private
	      { Private Declarations }
         FMatNameTop : String;
         FMatNameRight : String;
         FMatNameFront : String;
         FMatNameLeft : String;
         FMatNameBack : String;
         FMatNameBottom : String;
         FMatNameClouds : String;
         FMaterialLibrary : TGLMaterialLibrary;
         FCloudsPlaneOffset : Single;
         FCloudsPlaneSize : Single;

	   protected
			{ Protected Declarations }
         procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
         procedure SetMatNameBack(const Value: string);
         procedure SetMatNameBottom(const Value: string);
         procedure SetMatNameFront(const Value: string);
         procedure SetMatNameLeft(const Value: string);
         procedure SetMatNameRight(const Value: string);
         procedure SetMatNameTop(const Value: string);
         procedure SetMatNameClouds(const Value: string);
         procedure SetCloudsPlaneOffset(const Value: single);
         procedure SetCloudsPlaneSize(const Value: single);

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor  Destroy; override;
         
         procedure DoRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

      published
	      { Published Declarations }
         property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
         property MatNameTop : String read FMatNameTop write SetMatNameTop;
         property MatNameBottom : String read FMatNameBottom write SetMatNameBottom;
         property MatNameLeft : String read FMatNameLeft write SetMatNameLeft;
         property MatNameRight : String read FMatNameRight write SetMatNameRight;
         property MatNameFront : String read FMatNameFront write SetMatNameFront;
         property MatNameBack : String read FMatNameBack write SetMatNameBack;
         property MatNameClouds : String read FMatNameClouds write SetMatNameClouds;
         property CloudsPlaneOffset : Single read FCloudsPlaneOffset write SetCloudsPlaneOffset;
         property CloudsPlaneSize : Single read FCloudsPlaneSize write SetCloudsPlaneSize;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses GLState;

// ------------------
// ------------------ TGLSkyBox ------------------
// ------------------

// Create
//
constructor TGLSkyBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
  FCloudsPlaneOffset:=0.2; // this should be set far enough to avoid near plane clipping
  FCloudsPlaneSize:=32;    // the bigger, the more this extends the clouds cap to the horizon
end;

// Destroy
//
destructor TGLSkyBox.Destroy;
begin
   inherited;
end;

// Notification
//
procedure TGLSkyBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FMaterialLibrary) then
      MaterialLibrary:=nil;
  inherited;
end;

// DoRender
//
procedure TGLSkyBox.DoRender(var rci : TRenderContextInfo;
                             renderSelf, renderChildren : Boolean);
var
   f, cps, cof1 : Single;
   mvMat : TMatrix;
   oldStates : TGLStates;
   libMat : TGLLibMaterial;
   transMat : TMatrix;
begin
   if FMaterialLibrary=nil then Exit;

   oldStates:=rci.currentStates;
   UnSetGLState(rci.currentStates, stDepthTest);
   UnSetGLState(rci.currentStates, stLighting);
   UnSetGLState(rci.currentStates, stFog);
   glDepthMask(False);

   glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);
   SetVector(transMat[0], LeftVector);
   SetVector(transMat[1], Up.AsVector);
   SetVector(transMat[2], Direction.AsVector);
   SetVector(transMat[3], rci.cameraPosition);
   glMultMatrixf(@transMat);
   
   with Scene.CurrentGLCamera do
      f:=(NearPlane+DepthOfView)*0.5;
   glScalef(f, f, f);

   glGetFloatv(GL_MODELVIEW_MATRIX, @mvMat);
   Scene.CurrentBuffer.PushModelViewMatrix(mvMat);
   try
      // FRONT
      libMat:=MaterialLibrary.LibMaterialByName(FMatNameFront);
      if libMat<>nil then begin
         libMat.Apply(rci);
         glBegin(GL_QUADS);
            xglTexCoord2f(0.002, 0.998);  glVertex3f(-1,  1, -1);
            xglTexCoord2f(0.002, 0.002);  glVertex3f(-1, -1, -1);
            xglTexCoord2f(0.998, 0.002);  glVertex3f( 1, -1, -1);
            xglTexCoord2f(0.998, 0.998);  glVertex3f( 1,  1, -1);
         glEnd;
         libMat.UnApply(rci);
      end;
      // BACK
      libMat:=MaterialLibrary.LibMaterialByName(FMatNameBack);
      if libMat<>nil then begin
         libMat.Apply(rci);
         glBegin(GL_QUADS);
            xglTexCoord2f(0.002, 0.998);  glVertex3f( 1,  1,  1);
            xglTexCoord2f(0.002, 0.002);  glVertex3f( 1, -1,  1);
            xglTexCoord2f(0.998, 0.002);  glVertex3f(-1, -1,  1);
            xglTexCoord2f(0.998, 0.998);  glVertex3f(-1,  1,  1);
         glEnd;
         libMat.UnApply(rci);
      end;
      // TOP
      libMat:=MaterialLibrary.LibMaterialByName(FMatNameTop);
      if libMat<>nil then begin
         libMat.Apply(rci);
         glBegin(GL_QUADS);
            xglTexCoord2f(0.002, 0.998);  glVertex3f(-1,  1,  1);
            xglTexCoord2f(0.002, 0.002);  glVertex3f(-1,  1, -1);
            xglTexCoord2f(0.998, 0.002);  glVertex3f( 1,  1, -1);
            xglTexCoord2f(0.998, 0.998);  glVertex3f( 1,  1,  1);
         glEnd;
         libMat.UnApply(rci);
      end;
      // BOTTOM
      libMat:=MaterialLibrary.LibMaterialByName(FMatNameBottom);
      if libMat<>nil then begin
         libMat.Apply(rci);
         glBegin(GL_QUADS);
            xglTexCoord2f(0.002, 0.998);  glVertex3f(-1, -1, -1);
            xglTexCoord2f(0.002, 0.002);  glVertex3f(-1, -1,  1);
            xglTexCoord2f(0.998, 0.002);  glVertex3f( 1, -1,  1);
            xglTexCoord2f(0.998, 0.998);  glVertex3f( 1, -1, -1);
         glEnd;
         libMat.UnApply(rci);
      end;
      // LEFT
      libMat:=MaterialLibrary.LibMaterialByName(FMatNameLeft);
      if libMat<>nil then begin
         libMat.Apply(rci);
         glBegin(GL_QUADS);
            xglTexCoord2f(0.002, 0.998);  glVertex3f(-1,  1,  1);
            xglTexCoord2f(0.002, 0.002);  glVertex3f(-1, -1,  1);
            xglTexCoord2f(0.998, 0.002);  glVertex3f(-1, -1, -1);
            xglTexCoord2f(0.998, 0.998);  glVertex3f(-1,  1, -1);
         glEnd;
         libMat.UnApply(rci);
      end;
      // RIGHT
      libMat:=MaterialLibrary.LibMaterialByName(FMatNameRight);
      if libMat<>nil then begin
         libMat.Apply(rci);
         glBegin(GL_QUADS);
            xglTexCoord2f(0.002, 0.998);  glVertex3f(1,  1, -1);
            xglTexCoord2f(0.002, 0.002);  glVertex3f(1, -1, -1);
            xglTexCoord2f(0.998, 0.002);  glVertex3f(1, -1,  1);
            xglTexCoord2f(0.998, 0.998);  glVertex3f(1,  1,  1);
         glEnd;
         libMat.UnApply(rci);
      end;
      // CLOUDS CAP PLANE
      libMat:=MaterialLibrary.LibMaterialByName(FMatNameClouds);
      if libMat<>nil then begin
         // pre-calculate possible values to speed up
         cps := FCloudsPlaneSize*0.5;
         cof1 := FCloudsPlaneOffset;

         libMat.Apply(rci);
         glBegin(GL_QUADS);
            xglTexCoord2f(0, 1);  glVertex3f(-cps, cof1,  cps);
            xglTexCoord2f(0, 0);  glVertex3f(-cps, cof1, -cps);
            xglTexCoord2f(1, 0);  glVertex3f( cps, cof1, -cps);
            xglTexCoord2f(1, 1);  glVertex3f( cps, cof1,  cps);
         glEnd;
         libMat.UnApply(rci);
      end;

      glDepthMask(True); // restore
      if stLighting in oldStates then
         SetGLState(rci.currentStates, stLighting);
      if stFog in oldStates then
         SetGLState(rci.currentStates, stFog);

      // process children
      if renderChildren then begin
         f:=1/f;
         glScalef(f, f, f);
         Self.RenderChildren(0, Count-1, rci);
      end;

      if stDepthTest in oldStates then
         SetGLState(rci.currentStates, stDepthTest);


   finally
      Scene.CurrentBuffer.PopModelViewMatrix;
   end;
end;

procedure TGLSkyBox.SetCloudsPlaneOffset(const Value: single);
begin
  FCloudsPlaneOffset := Value;
  StructureChanged;
end;

procedure TGLSkyBox.SetCloudsPlaneSize(const Value: single);
begin
  FCloudsPlaneSize := Value;
  StructureChanged;
end;

// SetMaterialLibrary
//
procedure TGLSkyBox.SetMaterialLibrary(const value : TGLMaterialLibrary);
begin
   FMaterialLibrary:=value;
   StructureChanged;
end;

procedure TGLSkyBox.SetMatNameBack(const Value: string);
begin
  FMatNameBack := Value;
  StructureChanged;
end;

procedure TGLSkyBox.SetMatNameBottom(const Value: string);
begin
  FMatNameBottom := Value;
  StructureChanged;
end;

procedure TGLSkyBox.SetMatNameClouds(const Value: string);
begin
  FMatNameClouds := Value;
  StructureChanged;
end;

procedure TGLSkyBox.SetMatNameFront(const Value: string);
begin
  FMatNameFront := Value;
  StructureChanged;
end;

procedure TGLSkyBox.SetMatNameLeft(const Value: string);
begin
  FMatNameLeft := Value;
  StructureChanged;
end;

procedure TGLSkyBox.SetMatNameRight(const Value: string);
begin
  FMatNameRight := Value;
  StructureChanged;
end;

procedure TGLSkyBox.SetMatNameTop(const Value: string);
begin
  FMatNameTop := Value;
  StructureChanged;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClass(TGLSkyBox);

end.
