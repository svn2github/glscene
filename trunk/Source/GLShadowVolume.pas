// GLShadowVolumes
{: Implements basic shadow volumes support.<p>

   Be aware that only objects that support silhouette determination have a chance
   to cast correct shadows. Transparent/blended/shader objects among the receivers
   or the casters will be rendered incorrectly.<p>

	<b>History : </b><font size=-1><ul>
      <li>04/06/03 - EG - Creation (based on code from Mattias Fagerlund)
   </ul></font>
}
unit GLShadowVolume;

interface

uses Classes, GLScene, Geometry, OpenGL12, GLMisc, GLTexture, GLObjects,
   GLCrossPlatform;

type

   TGLShadowVolume = class;

   TGLShadowVolumeCapping = (svcDefault, svcAlways, svcNever);

   // TGLShadowVolumeCaster
   //
   {: Specifies an individual shadow caster.<p>
      Can be a light or an opaque object. }
   TGLShadowVolumeCaster = class (TCollectionItem)
	   private
			{ Private Declarations }
         FCaster : TGLBaseSceneObject;
         FEffectiveRadius : Single;
         FCapping : TGLShadowVolumeCapping;

		protected
			{ Protected Declarations }
         procedure SetCaster(const val : TGLBaseSceneObject);
         procedure RemoveNotification(aComponent : TComponent);
         function GetDisplayName : String; override;

		public
			{ Public Declarations }
         constructor Create(Collection: TCollection); override;

         procedure Assign(Source: TPersistent); override;

         {: Shadow casting object.<p>
            Can be an opaque object or a lightsource. }
         property Caster : TGLBaseSceneObject read FCaster write SetCaster;
         
         {: Radius beyond which the caster can be ignored.<p>
            Zero (default value) means the caster can never be ignored. }
         property EffectiveRadius : Single read FEffectiveRadius write FEffectiveRadius;
         {: Specifies if the shadow volume should be capped.<p>
            Capping helps solve shadowing artefacts, at the cost of performance. }
         property Capping : TGLShadowVolumeCapping read FCapping write FCapping default svcDefault;
   end;

   // TGLShadowVolumeCasters
   //
   {: Collection of TGLShadowVolumeCaster. }
   TGLShadowVolumeCasters = class (TCollection)
	   private
			{ Private Declarations }
         FOwner : TGLShadowVolume;

		protected
			{ Protected Declarations }
         function GetOwner : TPersistent; override;
         function GetItems(index : Integer) : TGLShadowVolumeCaster;
         procedure RemoveNotification(aComponent : TComponent);

		public
			{ Public Declarations }
         procedure AddCaster(obj : TGLBaseSceneObject; effectiveRadius : Single = 0);
         procedure RemoveCaster(obj : TGLBaseSceneObject);

         property Items[index : Integer] : TGLShadowVolumeCaster read GetItems; default;
   end;

   // TGLShadowVolume
   //
   {: Simple shadow volumes.<p>
      Shadow receiving objects are the ShadowVolume's children, shadow casters
      (opaque objects or lights) must be explicitly specified in the Casters
      collection. }
	TGLShadowVolume = class (TGLImmaterialSceneObject)
	   private
			{ Private Declarations }
         FRendering : Boolean;
         FCasters : TGLShadowVolumeCasters;
         FCapping : TGLShadowVolumeCapping;

		protected
			{ Protected Declarations }
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;

		   procedure Assign(Source: TPersistent); override;

		published
			{ Public Declarations }
         property Casters : TGLShadowVolumeCasters read FCasters;

         {: Specifies if the shadow volume should be capped.<p>
            Capping helps solve shadowing artefacts, at the cost of performance. }
         property Capping : TGLShadowVolumeCapping read FCapping write FCapping default svcAlways;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses SysUtils, VectorLists;

// ------------------
// ------------------ TGLShadowVolumeCaster ------------------
// ------------------

// Create
//
constructor TGLShadowVolumeCaster.Create(Collection: TCollection);
begin
   inherited Create(Collection);
   FCapping:=svcAlways;
end;

// Assign
//
procedure TGLShadowVolumeCaster.Assign(Source: TPersistent);
begin
   if Source is TGLShadowVolumeCaster then begin
      FCaster:=TGLShadowVolumeCaster(Source).FCaster;
      FEffectiveRadius:=TGLShadowVolumeCaster(Source).FEffectiveRadius;
      FCapping:=TGLShadowVolumeCaster(Source).FCapping;
      TGLShadowVolume(TGLShadowVolumeCaster(Collection).GetOwner).StructureChanged;
   end;
   inherited;
end;

// SetCaster
//
procedure TGLShadowVolumeCaster.SetCaster(const val : TGLBaseSceneObject);
begin
   if FCaster<>val then begin
      FCaster:=val;
      TGLShadowVolume(TGLShadowVolumeCaster(Collection).GetOwner).StructureChanged;
   end;
end;

// RemoveNotification
//
procedure TGLShadowVolumeCaster.RemoveNotification(aComponent : TComponent);
begin
   if aComponent=FCaster then
      FCaster:=nil;
end;

// GetDisplayName
//
function TGLShadowVolumeCaster.GetDisplayName : String;
begin
   if Assigned(FCaster) then begin
      if FCaster is TGLLightSource then
         Result:='[Light]'
      else Result:='[Object]';
      Result:=Result+' '+FCaster.Name;
      if EffectiveRadius>0 then
         Result:=Result+Format(' (%.1f)', [EffectiveRadius]);
   end else Result:='nil';
end;

// ------------------
// ------------------ TGLShadowVolumeCasters ------------------
// ------------------

// GetOwner
//
function TGLShadowVolumeCasters.GetOwner : TPersistent;
begin
   Result:=FOwner;
end;

// RemoveNotification
//
procedure TGLShadowVolumeCasters.RemoveNotification(aComponent : TComponent);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].RemoveNotification(aComponent);
end;

// GetItems
//
function TGLShadowVolumeCasters.GetItems(index : Integer) : TGLShadowVolumeCaster;
begin
   Result:=TGLShadowVolumeCaster(inherited Items[index]);
end;

// AddCaster
//
procedure TGLShadowVolumeCasters.AddCaster(obj : TGLBaseSceneObject; effectiveRadius : Single = 0);
var
   newCaster : TGLShadowVolumeCaster;
begin
   newCaster:=TGLShadowVolumeCaster(Add);
   newCaster.Caster:=obj;
   newCaster.EffectiveRadius:=effectiveRadius;
end;

// RemoveCaster
//
procedure TGLShadowVolumeCasters.RemoveCaster(obj : TGLBaseSceneObject);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      if Items[i].Caster=obj then begin
         Delete(i);
         Break;
      end;
end;

// ------------------
// ------------------ TGLShadowVolume ------------------
// ------------------

// Create
//
constructor TGLShadowVolume.Create(AOwner:Tcomponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle-[osDirectDraw]+[osNoVisibilityCulling];
   FCasters:=TGLShadowVolumeCasters.Create(TGLShadowVolumeCaster);
   FCasters.FOwner:=Self;
   FCapping:=svcAlways;
end;

// Destroy
//
destructor TGLShadowVolume.Destroy;
begin
   FCasters.Free;
   inherited;
end;

// Notification
//
procedure TGLShadowVolume.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then
      FCasters.RemoveNotification(AComponent);
   inherited;
end;

// Assign
//
procedure TGLShadowVolume.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLShadowVolume) then begin
      FCasters.Assign(TGLShadowVolume(Source).Casters);
      FCapping:=TGLShadowVolume(Source).FCapping;
      StructureChanged;
   end;
   inherited Assign(Source);
end;

// DoRender
//
procedure TGLShadowVolume.DoRender(var rci : TRenderContextInfo;
                                   renderSelf, renderChildren : Boolean);
var
   i, j, k, n : Integer;
   lightSource : TGLLightSource;
   sil : TGLBaseSilhouette;
   lightID : Cardinal;
   obj : TGLBaseSceneObject;
   caster : TGLShadowVolumeCaster;
   lights, opaques, opaqueCapping : TList;
   silParams : TGLSilhouetteParameters;
   extrudedVertices : TVectorList;
   mat : TMatrix;
begin
   if FRendering then Exit;
   if not (renderSelf or renderChildren) then Exit;
   lights:=TList.Create;
   opaques:=TList.Create;
   opaqueCapping:=TList.Create;
   extrudedVertices:=TVectorList.Create;
   FRendering:=True;
   try
      // collect visible/shining casters
      for i:=0 to Casters.Count-1 do begin
         caster:=Casters[i];
         obj:=caster.Caster;
         if     Assigned(obj)
            and ((caster.EffectiveRadius<=0)
                 or (obj.DistanceTo(rci.cameraPosition)<caster.EffectiveRadius)) then begin
            if obj is TGLLightSource then begin
               if TGLLightSource(obj).Shining then
                  lights.Add(obj)
            end else begin
               if obj.Visible then begin
                  opaques.Add(obj);
                  opaqueCapping.Add(Pointer(caster.Capping=svcAlways));
               end;
            end;
         end;
      end;
      // render the shadow volumes
      glPushAttrib(GL_ENABLE_BIT);

      // first turn of all the shadow casting lights diffuse and specular
      for i:=0 to lights.Count-1 do begin
         lightID:=TGLLightSource(lights[i]).LightID;
         glLightfv(lightID, GL_DIFFUSE, @NullHmgVector);
         glLightfv(lightID, GL_SPECULAR, @NullHmgVector);
      end;
      // render shadow receivers with ambient lighting
      Self.RenderChildren(0, Count-1, rci);

      glDepthMask(False);
      rci.ignoreBlendingRequests:=True;

      glEnable(GL_STENCIL_TEST);
      glEnable(GL_DEPTH_TEST);

      // turn off *all* lights (they'll be reenabled by the PopAttrib)
      glGetIntegerv(GL_MAX_LIGHTS, @n);
      for i:=0 to n-1 do
         glDisable(GL_LIGHT0+i);

      // render contribution of all shadow casting lights
      for i:=0 to lights.Count-1 do begin
         lightSource:=TGLLightSource(lights[i]);
         lightID:=lightSource.LightID;

         SetVector(silParams.LightDirection, lightSource.SpotDirection.DirectVector);
         case lightSource.LightStyle of
            lsParallel : silParams.Style:=ssParallel
         else
            silParams.Style:=ssOmni;
         end;
         silParams.CappingRequired:=True;

         // clear the stencil and prepare for shadow volume pass
         glClear(GL_STENCIL_BUFFER_BIT);
         glStencilFunc(GL_ALWAYS, 0, 0);
         glDepthFunc(GL_LESS);

{         glColor3f(0, 0, 1);
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE); }

         SetGLState(rci.currentStates, stCullFace);

         glColorMask(False, False, False, False);
         glDisable(GL_BLEND);
         glDisable(GL_LIGHTING);

         // for all opaque shadow casters
         for k:=0 to opaques.Count-1 do begin
            obj:=TGLBaseSceneObject(opaques[k]);

            SetVector(silParams.SeenFrom, obj.AbsoluteToLocal(lightSource.AbsolutePosition));

            sil:=obj.GenerateSilhouette(silParams);
            try
               glPushMatrix;

               mat:=obj.AbsoluteMatrix;
               glMultMatrixf(@mat);

               // extrude vertices to infinity
               extrudedVertices.Count:=sil.Vertices.Count;
               for n:=0 to extrudedVertices.Count-1 do begin
                  VectorSubtract(sil.Vertices.List[n], silParams.SeenFrom,
                                 extrudedVertices.List[n]);
                  extrudedVertices.List[n][3]:=0;
               end;

               if Boolean(opaqueCapping[k]) then begin
                  // z-fail
                  glCullFace(GL_FRONT);
                  glStencilOp(GL_KEEP, GL_INCR, GL_KEEP);

                  glBegin(GL_QUADS);
                  n:=0; while n<sil.Indices.Count do begin
                     glVertex3fv(@sil.Vertices.List[sil.Indices[n]]);
                     glVertex3fv(@sil.Vertices.List[sil.Indices[n+1]]);
                     glVertex4fv(@extrudedVertices.List[sil.Indices[n+1]]);
                     glVertex4fv(@extrudedVertices.List[sil.Indices[n]]);
                     Inc(n, 2);
                  end;
                  glEnd;
                  glBegin(GL_TRIANGLES);
                  for n:=sil.CapIndices.Count-1 downto 0 do
                     glVertex3fv(@sil.Vertices.List[sil.CapIndices[n]]);
                  glEnd;
                  glBegin(GL_TRIANGLES);
                  for n:=0 to sil.CapIndices.Count-1 do
                     glVertex4fv(@extrudedVertices.List[sil.CapIndices[n]]);
                  glEnd;

                  glCullFace(GL_BACK);
                  glStencilOp(GL_KEEP, GL_DECR, GL_KEEP);

                  glBegin(GL_QUADS);
                  n:=0; while n<sil.Indices.Count do begin
                     glVertex3fv(@sil.Vertices.List[sil.Indices[n]]);
                     glVertex3fv(@sil.Vertices.List[sil.Indices[n+1]]);
                     glVertex4fv(@extrudedVertices.List[sil.Indices[n+1]]);
                     glVertex4fv(@extrudedVertices.List[sil.Indices[n]]);
                     Inc(n, 2);
                  end;
                  glEnd;
                  glBegin(GL_TRIANGLES);
                  for n:=sil.CapIndices.Count-1 downto 0 do
                     glVertex3fv(@sil.Vertices.List[sil.CapIndices[n]]);
                  glEnd;
                  glBegin(GL_TRIANGLES);
                  for n:=0 to sil.CapIndices.Count-1 do
                     glVertex4fv(@extrudedVertices.List[sil.CapIndices[n]]);
                  glEnd;
               end else begin
                  // z-pass
                  glCullFace(GL_FRONT);
                  glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);

                  glBegin(GL_QUADS);
                  n:=0; while n<sil.Indices.Count do begin
                     glVertex3fv(@sil.Vertices.List[sil.Indices[n]]);
                     glVertex3fv(@sil.Vertices.List[sil.Indices[n+1]]);
                     glVertex4fv(@extrudedVertices.List[sil.Indices[n+1]]);
                     glVertex4fv(@extrudedVertices.List[sil.Indices[n]]);
                     Inc(n, 2);
                  end;
                  glEnd;

                  glCullFace(GL_BACK);
                  glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);

                  glBegin(GL_QUADS);
                  n:=0; while n<sil.Indices.Count do begin
                     glVertex3fv(@sil.Vertices.List[sil.Indices[n]]);
                     glVertex3fv(@sil.Vertices.List[sil.Indices[n+1]]);
                     glVertex4fv(@extrudedVertices.List[sil.Indices[n+1]]);
                     glVertex4fv(@extrudedVertices.List[sil.Indices[n]]);
                     Inc(n, 2);
                  end;
                  glEnd;
               end;
               glPopMatrix;
            finally
               sil.Free;
            end;
         end;

         glColorMask(True, True, True, True);
         glDepthFunc(GL_EQUAL);
         glStencilFunc(GL_EQUAL, 0, 255);
         glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);

         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE);

         // re-enable light's diffuse and specular, but no ambient
         glEnable(lightID);
         glLightfv(lightID, GL_AMBIENT, @NullHmgVector);
         glLightfv(lightID, GL_DIFFUSE, lightSource.Diffuse.AsAddress);
         glLightfv(lightID, GL_SPECULAR, lightSource.Specular.AsAddress);
         glEnable(GL_LIGHTING);

         glCullFace(GL_BACK);

         Self.RenderChildren(0, Count-1, rci);

         glDisable(lightID);
      end;

      // restore OpenGL state
      glPopAttrib;
      glDepthMask(True);
      glDepthFunc(GL_LESS);
      rci.ignoreBlendingRequests:=False;
   finally
      FRendering:=False;
      extrudedVertices.Free;
      opaques.Free;
      opaqueCapping.Free;
      lights.Free;
   end;
end;

end.
