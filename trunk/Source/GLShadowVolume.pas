// GLShadowVolumes
{: Implements basic shadow volumes support.<p>

   Be aware that only objects that support silhouette determination have a chance
   to cast correct shadows. Transparent/blended/shader objects among the receivers
   or the casters will be rendered incorrectly.<p>

	<b>History : </b><font size=-1><ul>
      <li>11/06/03 - EG - Added silhouette cache
      <li>04/06/03 - EG - Creation (based on code from Mattias Fagerlund)
   </ul></font>
}
unit GLShadowVolume;

interface

uses Classes, GLScene, Geometry, OpenGL1x, GLMisc, GLSilhouette, GLTexture,
  GLCrossPlatform,  PersistentClasses;

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
         
		published
			{ Published Declarations }

         {: Radius beyond which the caster can be ignored.<p>
            Zero (default value) means the caster can never be ignored. }
         property EffectiveRadius : Single read FEffectiveRadius write FEffectiveRadius;
         {: Specifies if the shadow volume should be capped.<p>
            Capping helps solve shadowing artefacts, at the cost of performance. }
         property Capping : TGLShadowVolumeCapping read FCapping write FCapping default svcDefault;
   end;

   // TGLShadowVolumeOccluder
   //
   {: Specifies an individual shadow casting occluder.<p> }
   TGLShadowVolumeOccluder = class (TGLShadowVolumeCaster)
		published
			{ Published Declarations }
         property Caster;
   end;

   // TGLShadowVolumeLight
   //
   {: Specifies an individual shadow casting light.<p> }
   TGLShadowVolumeLight = class (TGLShadowVolumeCaster)
	   private
			{ Private Declarations }
         FSilhouettes : TPersistentObjectList;

		protected
			{ Protected Declarations }
         function GetLightSource : TGLLightSource;
         procedure SetLightSource(const ls : TGLLightSource);

         function GetCachedSilhouette(index : Integer) : TGLSilhouette;
         procedure StoreCachedSilhouette(index : Integer; sil : TGLSilhouette);

		public
			{ Public Declarations }
         constructor Create(Collection: TCollection); override;
         destructor Destroy; override;

         procedure FlushSilhouetteCache;

		published
			{ Published Declarations }
         {: Shadow casting lightsource.<p> }
         property LightSource : TGLLightSource read GetLightSource write SetLightSource;

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

   // TGLShadowVolumeOption
   //
   TGLShadowVolumeOption = (svoShowVolumes, svoCacheSilhouettes);
   TGLShadowVolumeOptions = set of TGLShadowVolumeOption;

   // TGLShadowVolumeMode
   //
   {: Shadow rendering modes.<p>
      <ul>
      <li>svmAccurate : will render the scene with ambient lighting only, then
         for each light will make a diffuse+specular pass
      <li>svmDarkening : renders the scene with lighting on as usual, then darkens
         shadowed areas (i.e. inaccurate lighting, but will "shadow" objects
         that don't honour to diffuse or specular lighting)
      <li>svmOff : no shadowing will take place
      </ul> }
   TGLShadowVolumeMode = (svmAccurate, svmDarkening, svmOff);

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
         FLights : TGLShadowVolumeCasters;
         FOccluders : TGLShadowVolumeCasters;
         FCapping : TGLShadowVolumeCapping;
         FOptions : TGLShadowVolumeOptions;
         FMode : TGLShadowVolumeMode;
         FDarkeningColor : TGLColor;

		protected
			{ Protected Declarations }
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         procedure SetLights(const val : TGLShadowVolumeCasters);
         procedure SetOccluders(const val : TGLShadowVolumeCasters);
         procedure SetOptions(const val : TGLShadowVolumeOptions);
         procedure SetMode(const val : TGLShadowVolumeMode);
         procedure SetDarkeningColor(const val : TGLColor);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;

		   procedure Assign(Source: TPersistent); override;

         procedure FlushSilhouetteCache;

		published
			{ Public Declarations }
         {: Lights that cast shadow volumes. }
         property Lights : TGLShadowVolumeCasters read FLights write SetLights;
         {: Occluders that cast shadow volumes. }
         property Occluders : TGLShadowVolumeCasters read FOccluders write SetOccluders;

         {: Specifies if the shadow volume should be capped.<p>
            Capping helps solve shadowing artefacts, at the cost of performance. }
         property Capping : TGLShadowVolumeCapping read FCapping write FCapping default svcAlways;
         {: Shadow volume rendering options. }
         property Options : TGLShadowVolumeOptions read FOptions write SetOptions default [svoCacheSilhouettes];
         {: Shadow rendering mode. }
         property Mode : TGLShadowVolumeMode read FMode write SetMode default svmAccurate;
         {: Darkening color used in svmDarkening mode. }
         property DarkeningColor : TGLColor read FDarkeningColor write SetDarkeningColor;
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
   FCapping:=svcDefault;
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
// ------------------ TGLShadowVolumeLight ------------------
// ------------------

// Create
//
constructor TGLShadowVolumeLight.Create(Collection: TCollection);
begin
   inherited Create(Collection);
   FSilhouettes:=TPersistentObjectList.Create;
end;

// Destroy
//
destructor TGLShadowVolumeLight.Destroy;
begin
   FlushSilhouetteCache;
   FSilhouettes.Free;
   inherited;
end;

// FlushSilhouetteCache
//
procedure TGLShadowVolumeLight.FlushSilhouetteCache;
begin
   FSilhouettes.Clean;
end;

// Create
//
function TGLShadowVolumeLight.GetLightSource : TGLLightSource;
begin
   Result:=TGLLightSource(Caster);
end;

// SetLightSource
//
procedure TGLShadowVolumeLight.SetLightSource(const ls : TGLLightSource);
begin
   SetCaster(ls);
end;

// GetCachedSilhouette
//
function TGLShadowVolumeLight.GetCachedSilhouette(index : Integer) : TGLSilhouette;
begin
   if index<FSilhouettes.Count then
      Result:=TGLSilhouette(FSilhouettes[index])
   else Result:=nil;
end;

// StoreCachedSilhouette
//
procedure TGLShadowVolumeLight.StoreCachedSilhouette(index : Integer; sil : TGLSilhouette);
begin
   while index>=FSilhouettes.Count do FSilhouettes.Add(nil);
   if sil<>FSilhouettes[index] then begin
      FSilhouettes[index].Free;
      FSilhouettes[index]:=sil;
   end;
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
   FLights:=TGLShadowVolumeCasters.Create(TGLShadowVolumeLight);
   FLights.FOwner:=Self;
   FOccluders:=TGLShadowVolumeCasters.Create(TGLShadowVolumeOccluder);
   FOccluders.FOwner:=Self;
   FCapping:=svcAlways;
   FMode:=svmAccurate;
   FOptions:=[svoCacheSilhouettes];
   FDarkeningColor:=TGLColor.CreateInitialized(Self, VectorMake(0, 0, 0, 0.5));
end;

// Destroy
//
destructor TGLShadowVolume.Destroy;
begin
   FDarkeningColor.Free;
   FLights.Free;
   FOccluders.Free;
   inherited;
end;

// Notification
//
procedure TGLShadowVolume.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
      FLights.RemoveNotification(AComponent);
      FOccluders.RemoveNotification(AComponent);
   end;
   inherited;
end;

// Assign
//
procedure TGLShadowVolume.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLShadowVolume) then begin
      FLights.Assign(TGLShadowVolume(Source).Lights);
      FOccluders.Assign(TGLShadowVolume(Source).Occluders);
      FCapping:=TGLShadowVolume(Source).FCapping;
      StructureChanged;
   end;
   inherited Assign(Source);
end;

// FlushSilhouetteCache
//
procedure TGLShadowVolume.FlushSilhouetteCache;
var
   i : Integer;
begin
   for i:=0 to Lights.Count-1 do
      (Lights[i] as TGLShadowVolumeLight).FlushSilhouetteCache;
end;

// SetLights
//
procedure TGLShadowVolume.SetLights(const val : TGLShadowVolumeCasters);
begin
   Assert(val.ItemClass=TGLShadowVolumeLight);
   FLights.Assign(val);
end;

// SetOccluders
//
procedure TGLShadowVolume.SetOccluders(const val : TGLShadowVolumeCasters);
begin
   Assert(val.ItemClass=TGLShadowVolumeOccluder);
   FOccluders.Assign(val);
end;

// SetOptions
//
procedure TGLShadowVolume.SetOptions(const val : TGLShadowVolumeOptions);
begin
   if FOptions<>val then begin
      FOptions:=val;
      if not (svoCacheSilhouettes in FOptions) then
         FlushSilhouetteCache;
      StructureChanged;
   end;
end;

// SetMode
//
procedure TGLShadowVolume.SetMode(const val : TGLShadowVolumeMode);
begin
   if FMode<>val then begin
      FMode:=val;
      StructureChanged;
   end;
end;

// SetDarkeningColor
//
procedure TGLShadowVolume.SetDarkeningColor(const val : TGLColor);
begin
   FDarkeningColor.Assign(val);
end;

// DoRender
//
procedure TGLShadowVolume.DoRender(var rci : TRenderContextInfo;
                                   renderSelf, renderChildren : Boolean);
var
   i, k, n     : Integer;
   lightSource : TGLLightSource;
   lightCaster : TGLShadowVolumeLight;
   sil : TGLSilhouette;
   lightID : Cardinal;
   obj : TGLBaseSceneObject;
   caster : TGLShadowVolumeCaster;
   opaques, opaqueCapping : TList;
   silParams : TGLSilhouetteParameters;
   mat : TMatrix;
begin
   if FRendering then Exit;
   if not (renderSelf or renderChildren) then Exit;
   if    (csDesigning in ComponentState) or (Mode=svmOff)
      or (rci.drawState=dsPicking) then begin
      inherited;
      Exit;
   end;
   opaques:=TList.Create;
   opaqueCapping:=TList.Create;
   FRendering:=True;
   try
      // collect visible casters
      for i:=0 to Occluders.Count-1 do begin
         caster:=Occluders[i];
         obj:=caster.Caster;
         if     Assigned(obj)
            and obj.Visible
            and ((caster.EffectiveRadius<=0)
                 or (obj.DistanceTo(rci.cameraPosition)<caster.EffectiveRadius)) then begin
            opaques.Add(obj);
            opaqueCapping.Add(Pointer(   (caster.Capping=svcAlways)
                                      or ((caster.Capping=svcDefault)
                                          and (Capping=svcAlways))));
         end else begin
            opaques.Add(nil);
            opaqueCapping.Add(nil);
         end;
      end;
      
      // render the shadow volumes
      glPushAttrib(GL_ENABLE_BIT);

      if Mode=svmAccurate then begin
         // first turn off all the shadow casting lights diffuse and specular
         for i:=0 to Lights.Count-1 do begin
            lightCaster:=TGLShadowVolumeLight(Lights[i]);
            lightSource:=lightCaster.LightSource;
            if Assigned(lightSource) and (lightSource.Shining) then begin
               lightID:=lightSource.LightID;
               glLightfv(lightID, GL_DIFFUSE, @NullHmgVector);
               glLightfv(lightID, GL_SPECULAR, @NullHmgVector);
            end;
         end;
      end;

      // render shadow receivers with ambient lighting
      Self.RenderChildren(0, Count-1, rci);

      glDepthMask(False);
      rci.ignoreBlendingRequests:=True;
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      glEnable(GL_STENCIL_TEST);
      glEnable(GL_DEPTH_TEST);

      // turn off *all* lights (they'll be reenabled by the PopAttrib)
      glGetIntegerv(GL_MAX_LIGHTS, @n);
      for i:=0 to n-1 do
         glDisable(GL_LIGHT0+i);
      glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @NullHmgPoint);

      // render contribution of all shadow casting lights
      for i:=0 to Lights.Count-1 do begin
         lightCaster:=TGLShadowVolumeLight(lights[i]);
         lightSource:=lightCaster.LightSource;

         if    (not Assigned(lightSource)) or (not lightSource.Shining)
            or ((lightCaster.EffectiveRadius>0)
                and (lightSource.DistanceTo(rci.cameraPosition)>lightCaster.EffectiveRadius)) then
            Continue;

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

         if svoShowVolumes in Options then begin
            glColor3f(0.05*i, 0.1, 0);
            glEnable(GL_BLEND);
         end else begin
            glColorMask(False, False, False, False);
            glDisable(GL_BLEND);
         end;
         SetGLState(rci.currentStates, stCullFace);

         glDisable(GL_LIGHTING);
         glEnableClientState(GL_VERTEX_ARRAY);
         glPolygonOffset(1, 1);

         // for all opaque shadow casters
         for k:=0 to opaques.Count-1 do begin
            obj:=TGLBaseSceneObject(opaques[k]);
            if obj=nil then Continue;

            SetVector(silParams.SeenFrom, obj.AbsoluteToLocal(lightSource.AbsolutePosition));

            sil:=lightCaster.GetCachedSilhouette(k);
            if (not Assigned(sil)) or (not CompareMem(@sil.Parameters, @silParams, SizeOf(silParams))) then begin
               sil:=obj.GenerateSilhouette(silParams);
               sil.Parameters:=silParams;
               // extrude vertices to infinity
               sil.ExtrudeVerticesToInfinity(silParams.SeenFrom);
            end;
            if Assigned(sil) then try
               // render the silhouette
               glPushMatrix;

               glLoadMatrixf(PGLFloat(rci.modelViewMatrix));
               mat:=obj.AbsoluteMatrix;
               glMultMatrixf(@mat);

               glVertexPointer(4, GL_FLOAT, 0, sil.Vertices.List);

               if Boolean(opaqueCapping[k]) then begin
                  // z-fail
                  glCullFace(GL_FRONT);
                  glStencilOp(GL_KEEP, GL_INCR, GL_KEEP);

                  with sil do begin
                     glDrawElements(GL_QUADS, Indices.Count, GL_UNSIGNED_INT, Indices.List);
                     glEnable(GL_POLYGON_OFFSET_FILL);
                     glDrawElements(GL_TRIANGLES, CapIndices.Count, GL_UNSIGNED_INT, CapIndices.List);
                     glDisable(GL_POLYGON_OFFSET_FILL);
                  end;

                  glCullFace(GL_BACK);
                  glStencilOp(GL_KEEP, GL_DECR, GL_KEEP);

                  with sil do begin
                     glDrawElements(GL_QUADS, Indices.Count, GL_UNSIGNED_INT, Indices.List);
                     glEnable(GL_POLYGON_OFFSET_FILL);
                     glDrawElements(GL_TRIANGLES, CapIndices.Count, GL_UNSIGNED_INT, CapIndices.List);
                     glDisable(GL_POLYGON_OFFSET_FILL);
                  end;
               end else begin
                  // z-pass
                  glCullFace(GL_BACK);
                  glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);

                  glDrawElements(GL_QUADS, sil.Indices.Count, GL_UNSIGNED_INT, sil.Indices.List);

                  glCullFace(GL_FRONT);
                  glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);

                  glDrawElements(GL_QUADS, sil.Indices.Count, GL_UNSIGNED_INT, sil.Indices.List);
               end;

               glPopMatrix;
            finally
               if (svoCacheSilhouettes in Options) and (not (osDirectDraw in ObjectStyle)) then
                  lightCaster.StoreCachedSilhouette(k, sil)
               else sil.Free;
            end;
         end;

         glDisableClientState(GL_VERTEX_ARRAY);

         // re-enable light's diffuse and specular, but no ambient
         glEnable(lightID);
         glLightfv(lightID, GL_AMBIENT, @NullHmgVector);
         glLightfv(lightID, GL_DIFFUSE, lightSource.Diffuse.AsAddress);
         glLightfv(lightID, GL_SPECULAR, lightSource.Specular.AsAddress);

         glColorMask(True, True, True, True);
         glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);

         glEnable(GL_BLEND);

         glCullFace(GL_BACK);

         if Mode=svmAccurate then begin
            glStencilFunc(GL_EQUAL, 0, 255);
            glDepthFunc(GL_EQUAL);
            glEnable(GL_LIGHTING);

            Self.RenderChildren(0, Count-1, rci)
         end else begin
            glStencilFunc(GL_NOTEQUAL, 0, 255);
            glDepthFunc(GL_ALWAYS);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

            glPushMatrix;
            glLoadIdentity;
            glMatrixMode(GL_PROJECTION);
            glPushMatrix;
            glLoadIdentity;
            gluOrtho2D(0, 1, 1, 0);

            glColor4fv(FDarkeningColor.AsAddress);
            glBegin(GL_QUADS);
               glVertex2f(0, 0); glVertex2f(0, 1);
               glVertex2f(1, 1); glVertex2f(1, 0);
            glEnd;

            glPopMatrix;
            glMatrixMode(GL_MODELVIEW);
            glPopMatrix;

            glBlendFunc(GL_SRC_ALPHA, GL_ONE);
         end;

         // disable light, but restore its ambient component
         glDisable(lightID);
         glLightfv(lightID, GL_DIFFUSE, lightSource.Ambient.AsAddress);
      end;

      // restore OpenGL state
      glPopAttrib;
      glDepthMask(True);
      glDepthFunc(GL_LESS);
      glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @rci.sceneAmbientColor);
      rci.ignoreBlendingRequests:=False;
   finally
      FRendering:=False;
      opaques.Free;
      opaqueCapping.Free;
   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLShadowVolume]);

end.
