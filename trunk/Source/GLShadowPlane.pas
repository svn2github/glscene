// GLShadowPlane
{: Implements a basic shadow plane.<p>

   It is strongly recommended to read and understand the explanations in the
   materials/mirror demo before using this component.<p>

	<b>History : </b><font size=-1><ul>
      <li>02/10/02 - EG - Added spoScissor
      <li>23/09/02 - EG - Creation (from GLMirror and Mattias FagerLund ShadowPlane.pas)
   </ul></font>
}
unit GLShadowPlane;

interface

uses Classes, GLScene, Geometry, OpenGL12, GLMisc, GLTexture, GLObjects,
   GLCrossPlatform;

type

   // TShadowPlaneOptions
   //
   TShadowPlaneOption = (spoUseStencil, spoScissor);
   TShadowPlaneOptions = set of TShadowPlaneOption;

const
   cDefaultShadowPlaneOptions = [spoUseStencil, spoScissor];

type

   // TGLShadowPlane
   //
   {: A simple shadow plane.<p>
      This mirror requires a stencil buffer for optimal rendering!<p>
      The object is a mix between a plane and a proxy object, in that the plane
      defines where the shadows are cast, while the proxy part is used to reference
      the objects that should be shadowing (it is legal to self-shadow, but no
      self-shadow visuals will be rendered).<br>
      If stenciling isn't used, the shadow will 'paint' the ShadowColor instead
      of blending it transparently.<p>
      You can have lower quality shadow geometry: add a dummycube, set it to
      invisible (so it won't be rendered in the "regular" pass), and under
      it place another visible dummycube under which you have all your
      low quality objects, use it as shadowing object. Apply the same movements
      to the low-quality objects that you apply to the visible, high-quality ones.
      }
	TGLShadowPlane = class (TGLPlane)
	   private
			{ Private Declarations }
         FRendering : Boolean;
         FShadowingObject : TGLBaseSceneObject;
         FShadowedLight : TGLLightSource;
         FShadowColor : TGLColor;
         FShadowOptions : TShadowPlaneOptions;

		protected
			{ Protected Declarations }
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure SetShadowingObject(const val : TGLBaseSceneObject);
         procedure SetShadowedLight(const val : TGLLightSource);
         procedure SetShadowColor(const val : TGLColor);
         procedure SetShadowOptions(const val : TShadowPlaneOptions);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;

		   procedure Assign(Source: TPersistent); override;

		published
			{ Public Declarations }
         {: Selects the object to mirror.<p>
            If nil, the whole scene is mirrored. }
         property ShadowingObject : TGLBaseSceneObject read FShadowingObject write SetShadowingObject;
         {: The light which casts shadows.<p>
            The light must be enabled otherwise shadows won't be cast. }
         property ShadowedLight : TGLLightSource read FShadowedLight write SetShadowedLight;
         {: The shadow's color.<p>
            This color is transparently blended to make shadowed area darker. }
         property ShadowColor : TGLColor read FShadowColor write SetShadowColor;

         {: Controls rendering options.<p>
            <ul>
            <li>spoUseStencil: plane area is stenciled, prevents shadowing
               objects to be visible on the sides of the mirror (stencil buffer
               must be active in the viewer too). It also allows shadows to
               be partial (blended).
            <li>spoScissor: plane area is 'scissored', this should improve
               rendering speed by limiting rendering operations and fill rate,
               may have adverse effects on old hardware in rare cases
            </ul>
         }
         property ShadowOptions : TShadowPlaneOptions read FShadowOptions write SetShadowOptions default cDefaultShadowPlaneOptions;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

// ------------------
// ------------------ TGLShadowPlane ------------------
// ------------------

// Create
//
constructor TGLShadowPlane.Create(AOwner:Tcomponent);
const
   cDefaultShadowColor : TColorVector = (0, 0, 0, 0.5);
begin
   inherited Create(AOwner);
   FShadowOptions:=cDefaultShadowPlaneOptions;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FShadowColor:=TGLColor.CreateInitialized(Self, cDefaultShadowColor);
end;

// Destroy
//
destructor TGLShadowPlane.Destroy;
begin
   inherited;
   FShadowColor.Free;
end;

// DoRender
//
procedure TGLShadowPlane.DoRender(var rci : TRenderContextInfo;
                                  renderSelf, renderChildren : Boolean);
var
   oldProxySubObject, oldIgnoreMaterials : Boolean;
   curMat, shadowMat : TMatrix;
   sr : TGLRect;
begin
   if FRendering then Exit;
   FRendering:=True;
   try
      oldProxySubObject:=rci.proxySubObject;
      rci.proxySubObject:=True;

      if renderSelf and (VectorDotProduct(VectorSubtract(rci.cameraPosition, AbsolutePosition), AbsoluteDirection)>0) then begin

         // "Render" stencil mask
         if spoScissor in ShadowOptions then begin
            sr:=ScreenRect;
            InflateGLRect(sr, 1, 1);
            IntersectGLRect(sr, GLRect(0, 0, rci.viewPortSize.cx, rci.viewPortSize.cy));
            glScissor(sr.Left, sr.Top, sr.Right-sr.Left, sr.Bottom-sr.Top);
            glEnable(GL_SCISSOR_TEST);
         end;

         if (spoUseStencil in ShadowOptions) then begin
            glClearStencil(0);
            glEnable(GL_STENCIL_TEST);
            glStencilFunc(GL_ALWAYS, 1, 1);
            glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
         end;

         Material.Apply(rci);
         repeat
            BuildList(rci);
         until not Material.UnApply(rci);

         if Assigned(FShadowedLight) then begin

            glPushAttrib(GL_ENABLE_BIT);
            glPushMatrix;
            glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);

            shadowMat:=MakeShadowMatrix(AbsolutePosition, AbsoluteDirection,
                                        ShadowedLight.AbsolutePosition);

            glMultMatrixf(@shadowMat);

            glGetFloatv(GL_MODELVIEW_MATRIX, @curMat);
            glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);
            Scene.CurrentBuffer.PushModelViewMatrix(curMat);

            glDisable(GL_CULL_FACE);
            glEnable(GL_NORMALIZE);

            oldIgnoreMaterials:=rci.ignoreMaterials;
            rci.ignoreMaterials:=True;
            glPolygonOffset(-1, 0);
            glEnable(GL_POLYGON_OFFSET_FILL);
            glDisable(GL_TEXTURE_2D);
            glDisable(GL_LIGHTING);

            glColor4fv(ShadowColor.AsAddress);

            if (spoUseStencil in ShadowOptions) then begin
               glEnable(GL_BLEND);
               glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
               glStencilFunc(GL_EQUAL, 1, 1);
               glStencilOp(GL_KEEP, GL_KEEP, GL_ZERO);
            end;

            if Assigned(FShadowingObject) then begin
               if FShadowingObject.Parent<>nil then
                  glMultMatrixf(PGLFloat(FShadowingObject.Parent.AbsoluteMatrixAsAddress));
               glMultMatrixf(@shadowMat);
               glMultMatrixf(@FShadowingObject.LocalMatrix);
               FShadowingObject.DoRender(rci, renderSelf, renderChildren);
            end else begin
               glMultMatrixf(@shadowMat);
               Scene.Objects.DoRender(rci, renderSelf, renderChildren);
            end;

            rci.ignoreMaterials:=oldIgnoreMaterials;

            // Restore to "normal"
            Scene.CurrentBuffer.PopModelViewMatrix;
            glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);

            glPopMatrix;
            glPopAttrib;

         end;

         if spoUseStencil in ShadowOptions then begin
            glDisable(GL_STENCIL_TEST);
         end;
         if spoScissor in ShadowOptions then begin
            glDisable(GL_SCISSOR_TEST);
         end;

      end;

      rci.proxySubObject:=oldProxySubObject;

      if renderChildren and (Count>0) then
         Self.RenderChildren(0, Count-1, rci);
   finally
      FRendering:=False;
   end;
end;

// Notification
//
procedure TGLShadowPlane.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
      if AComponent=FShadowingObject then
         ShadowingObject:=nil
      else if AComponent=FShadowedLight then
         ShadowedLight:=nil;
   end;
   inherited;
end;

// SetShadowingObject
//
procedure TGLShadowPlane.SetShadowingObject(const val : TGLBaseSceneObject);
begin
   if FShadowingObject<>val then begin
      if Assigned(FShadowingObject) then
         FShadowingObject.RemoveFreeNotification(Self);
      FShadowingObject:=val;
      if Assigned(FShadowingObject) then
         FShadowingObject.FreeNotification(Self);
      NotifyChange(Self);
   end;
end;

// SetShadowedLight
//
procedure TGLShadowPlane.SetShadowedLight(const val : TGLLightSource);
begin
   if FShadowedLight<>val then begin
      if Assigned(FShadowedLight) then
         FShadowedLight.RemoveFreeNotification(Self);
      FShadowedLight:=val;
      if Assigned(FShadowedLight) then
         FShadowedLight.FreeNotification(Self);
      NotifyChange(Self);
   end;
end;

// SetShadowColor
//
procedure TGLShadowPlane.SetShadowColor(const val : TGLColor);
begin
   FShadowColor.Assign(val);
end;

// Assign
//
procedure TGLShadowPlane.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLShadowPlane) then begin
      FShadowOptions:=TGLShadowPlane(Source).FShadowOptions;
      ShadowingObject:=TGLShadowPlane(Source).ShadowingObject;
      ShadowedLight:=TGLShadowPlane(Source).ShadowedLight;
      ShadowColor:=TGLShadowPlane(Source).ShadowColor;
   end;
   inherited Assign(Source);
end;

// SetShadowOptions
//
procedure TGLShadowPlane.SetShadowOptions(const val : TShadowPlaneOptions);
begin
   if FShadowOptions<>val then begin
      FShadowOptions:=val;
      NotifyChange(Self);
   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLShadowPlane]);

end.
