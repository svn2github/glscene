// GLMirror
{: Implements a basic, stencil-based mirror (as in Mark Kilgard's demo).<p>

	<b>History : </b><font size=-1><ul>
      <li>07/12/01 - Egg - Creation
   </ul></font>
}
unit GLMirror;

interface

uses Classes, GLScene, Geometry, OpenGL12, GLMisc, GLTexture;

type

   // TMirrorOptions
   //
   TMirrorOption = (moUseStencil, moOpaque);
   TMirrorOptions = set of TMirrorOption;

const
   cDefaultMirrorOptions = [moUseStencil];

type

   // TGLMirror
   //
   {: A simple plane mirror.<p>
      This mirror requires a stencil buffer!<p>
      The object is a mix between a plane and a proxy object, in that the plane
      defines the mirror's surface, while the proxy part is used to reference
      the objects that should be mirrored (it is legal to self-mirror, but no
      self-mirror visuals will be rendered). }
	TGLMirror = class (TGLSceneObject)
	   private
			{ Private Declarations }
         FRendering : Boolean;
         FMasterObject : TGLBaseSceneObject;
         FProxyOptions : TGLProxyObjectOptions;
			FWidth, FHeight : TGLFloat;
         FMirrorOptions : TMirrorOptions;

		protected
			{ Protected Declarations }
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure SetMasterObject(const val : TGLBaseSceneObject);
         procedure SetProxyOptions(const val : TGLProxyObjectOptions);
         procedure SetMirrorOptions(const val : TMirrorOptions);

		   procedure SetHeight(AValue: TGLFloat);
		   procedure SetWidth(AValue: TGLFloat);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
		   procedure BuildList(var rci : TRenderContextInfo); override;

		   procedure Assign(Source: TPersistent); override;
         function AxisAlignedDimensions : TVector; override;

		published
			{ Public Declarations }
         property MasterObject : TGLBaseSceneObject read FMasterObject write SetMasterObject;
         property ProxyOptions : TGLProxyObjectOptions read FProxyOptions write SetProxyOptions default cDefaultProxyOptions;
         property MirrorOptions : TMirrorOptions read FMirrorOptions write SetMirrorOptions default cDefaultMirrorOptions;

			property Height: TGLFloat read FHeight write SetHeight;
         property Width: TGLFloat read FWidth write SetWidth;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

// ------------------
// ------------------ TGLMirror ------------------
// ------------------

// Create
//
constructor TGLMirror.Create(AOwner:Tcomponent);
begin
   inherited Create(AOwner);
   FWidth:=1;
   FHeight:=1;
   FProxyOptions:=cDefaultProxyOptions;
   FMirrorOptions:=cDefaultMirrorOptions;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   Material.FrontProperties.Diffuse.Initialize(VectorMake(1, 1, 1, 0.1));
   Material.BlendingMode:=bmTransparency;
end;

// DoRender
//
procedure TGLMirror.DoRender(var rci : TRenderContextInfo;
                          renderSelf, renderChildren : Boolean);
var
   gotMaster, masterGotEffects, oldProxySubObject : Boolean;
   refMat : TMatrix;
   clipPlane : TDoubleHmgPlane;
   bgColor : TColorVector;
begin
   if FRendering then Exit;
   FRendering:=True;
   try
      gotMaster:=Assigned(FMasterObject);
      masterGotEffects:=gotMaster and (pooEffects in FProxyOptions)
                        and (FMasterObject.Effects.Count>0);
      if gotMaster then begin
         if pooObjects in FProxyOptions then begin
            oldProxySubObject:=rci.proxySubObject;
            rci.proxySubObject:=True;
            if pooTransformation in FProxyOptions then
               glMultMatrixf(@FMasterObject.LocalMatrix);

            glPushMatrix;
            glPushAttrib(GL_ENABLE_BIT);

            // "Render" stencil mask
            if MirrorOptions<>[] then begin
               if (moUseStencil in MirrorOptions) then begin
                  glClearStencil(1);
                  glEnable(GL_STENCIL_TEST);
                  glStencilFunc(GL_ALWAYS, 0, 0);
                  glStencilOp(GL_ZERO, GL_ZERO, GL_ZERO);
               end;
               if (moOpaque in MirrorOptions) then begin
                  bgColor:=ConvertWinColor(Scene.CurrentBuffer.BackgroundColor);
                  SetGLMaterialColors(GL_FRONT, @bgColor, @clrBlack, @clrBlack, @clrBlack, 0);
                  UnSetGLState(rci.currentStates, stTexture2D);
               end else begin
                  glColorMask(False, False, False, False);
               end;
//               glDisable(GL_DEPTH_TEST);
               glDepthMask(False);

               BuildList(rci);

               glDepthMask(True);
               glEnable(GL_DEPTH_TEST);
               if not (moOpaque in MirrorOptions) then
                  glColorMask(True, True, True, True);
               if (moUseStencil in MirrorOptions) then begin
                  glStencilFunc(GL_EQUAL, 0, 1);
                  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
               end;
            end;

            // Mirror lights
            glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);
            refMat:=MakeReflectionMatrix(AffineVectorMake(AbsolutePosition),
                                         AffineVectorMake(AbsoluteUp));
            glMultMatrixf(@refMat);
            Scene.SetupLights(Scene.CurrentBuffer.LimitOf[limLights]);

            // mirror geometry and render master
            glPopMatrix;
            glPushMatrix;
            refMat:=MakeReflectionMatrix(NullVector, Up.AsAffineVector);
            glMultMatrixf(@refMat);

            glDisable(GL_CULL_FACE);
            glEnable(GL_NORMALIZE);

            glEnable(GL_CLIP_PLANE0);
            SetPlane(clipPlane, PlaneMake(Position.AsAffineVector,
                                          Up.AsAffineVector));
            glClipPlane(GL_CLIP_PLANE0, @clipPlane);

            FMasterObject.DoRender(rci, renderSelf, RenderChildren);

            glDisable(GL_CLIP_PLANE0);

            if moUseStencil in MirrorOptions then begin
               glDisable(GL_STENCIL_TEST);
            end;

            // Restore to "normal"
            glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);
            Scene.SetupLights(Scene.CurrentBuffer.LimitOf[limLights]);

            glPopAttrib;
            glPopMatrix;
            ResetGLMaterialColors;
            ResetGLCurrentTexture;

            rci.proxySubObject:=oldProxySubObject;
         end;
      end;
      // start rendering self
      if renderSelf then begin
         Material.Apply(rci);
         BuildList(rci);
         Material.UnApply(rci);
      end;
      if renderChildren and (Count>0) then
         Self.RenderChildren(0, Count-1, rci);
      if masterGotEffects then
         FMasterObject.Effects.RenderPostEffects(Scene.CurrentBuffer, rci);
   finally
      FRendering:=False;
   end;
end;

// BuildList
//
procedure TGLMirror.BuildList(var rci : TRenderContextInfo);
var
   hw, hh : TGLFloat;
begin
   hw:=FWidth*0.5;
   hh:=FHeight*0.5;
   glNormal3fv(@YVector);
   glBegin(GL_QUADS);
      glVertex3f( hw, 0, hh);
      glVertex3f( hw, 0, -hh);
      glVertex3f(-hw, 0, -hh);
      glVertex3f(-hw, 0, hh);
   glEnd;
end;

// Notification
//
procedure TGLMirror.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation = opRemove) and (AComponent = FMasterObject) then
      MasterObject:=nil;
   inherited;
end;

// SetMasterObject
//
procedure TGLMirror.SetMasterObject(const val : TGLBaseSceneObject);
begin
   if FMasterObject<>val then begin
      if Assigned(FMasterObject) then
         FMasterObject.RemoveFreeNotification(Self);
      FMasterObject:=val;
      if Assigned(FMasterObject) then
         FMasterObject.FreeNotification(Self);
      StructureChanged;
   end;
end;

// SetProxyOptions
//
procedure TGLMirror.SetProxyOptions(const val : TGLProxyObjectOptions);
begin
   if FProxyOptions<>val then begin
      FProxyOptions:=val;
      StructureChanged;
   end;
end;

// SetWidth
//
procedure TGLMirror.SetWidth(AValue : TGLFloat);
begin
   if AValue<>FWidth then begin
      FWidth:=AValue;
	   StructureChanged;
   end;
end;

// SetHeight
//
procedure TGLMirror.SetHeight(AValue : TGLFloat);
begin
   if AValue<>FHeight then begin
      FHeight:=AValue;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLMirror.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLMirror) then begin
      FWidth:=TGLMirror(Source).FWidth;
      FHeight:=TGLMirror(Source).FHeight;
      FMirrorOptions:=TGLMirror(Source).FMirrorOptions;
      FProxyOptions:=TGLMirror(Source).FProxyOptions;
      MasterObject:=TGLMirror(Source).MasterObject;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLMirror.AxisAlignedDimensions: TVector;
begin
   Result:=VectorMake(0.5*Abs(FWidth)*Scale.DirectX,
                      0.5*Abs(FHeight)*Scale.DirectY, 0);
end;

// SetMirrorOptions
//
procedure TGLMirror.SetMirrorOptions(const val : TMirrorOptions);
begin
   if FMirrorOptions<>val then begin
      FMirrorOptions:=val;
      StructureChanged;
   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLMirror]);

end.
