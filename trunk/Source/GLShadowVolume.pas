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

uses SysUtils;

// ------------------
// ------------------ TGLShadowVolumeCaster ------------------
// ------------------

constructor TGLShadowVolumeCaster.Create(Collection: TCollection);
begin
   ..
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

// DoRender
//
procedure TGLShadowVolume.DoRender(var rci : TRenderContextInfo;
                                   renderSelf, renderChildren : Boolean);
var
   i : Integer;
   obj : TGLBaseSceneObject;
   caster : TGLShadowVolumeCaster;
   lights, opaques : TList;
begin
   if FRendering then Exit;
   lights:=TList.Create;
   opaques:=TList.Create;
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
               if obj.Visible then
                  opaques.Add(obj);
            end;
         end;
      end;
      // render
      // ...
   finally
      FRendering:=False;
      opaques.Free;
      lights.Free;
   end;
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

end.
