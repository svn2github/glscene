unit GLLazarusRegister;

interface

uses
   {$IFDEF WIN32}Windows,{$ENDIF} Classes, Controls, StdCtrls, GLScene;


type

   PSceneObjectEntry = ^TGLSceneObjectEntry;
   // holds a relation between an scene object class, its global identification,
   // its location in the object stock and its icon reference
   TGLSceneObjectEntry = record
      ObjectClass : TGLSceneObjectClass;
      Name : String[32];     // type name of the object
      Category : String[32]; // category of object
      Index,                 // index into "FObjectStock"
      ImageIndex : Integer;  // index into "FObjectIcons"
   end;

   // TObjectManager
   //
   TObjectManager = class (TObject)
      private
         { Private Declarations }
         FSceneObjectList : TList;
         FObjectIcons : TImageList;       // a list of icons for scene objects
         {$ifdef WIN32}
         FOverlayIndex,                   // indices into the object icon list
         {$endif}
         FSceneRootIndex,
         FCameraRootIndex,
         FLightsourceRootIndex,
         FObjectRootIndex,
         FStockObjectRootIndex : Integer;

      protected
			{ Protected Declarations }
         procedure CreateDefaultObjectIcons;
         procedure DestroySceneObjectList;
         function FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
            const ASceneObject: String = '') : PSceneObjectEntry;

      public
         { Public Declarations }
         constructor Create;
         destructor Destroy; override;

         function GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
         function GetImageIndex(ASceneObject: TGLSceneObjectClass) : Integer;
         function GetCategory(ASceneObject: TGLSceneObjectClass) : String;
         procedure GetRegisteredSceneObjects(ObjectList: TStringList);
         //: Registers a stock object and adds it to the stock object list
         procedure RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory : String);
         //: Unregisters a stock object and removes it from the stock object list
         procedure UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);
//         procedure Notify(Sender: TPlugInManager; Operation: TOperation; PlugIn: Integer); override;

         property ObjectIcons: TImageList read FObjectIcons;
         property SceneRootIndex: Integer read FSceneRootIndex;
         property LightsourceRootIndex: Integer read FLightsourceRootIndex;
         property CameraRootIndex: Integer read FCameraRootIndex;
         property ObjectRootIndex: Integer read FObjectRootIndex;

   end;

procedure Register;

//: Auto-create for object manager
function ObjectManager : TObjectManager;

implementation

uses
   VectorGeometry, SysUtils, Graphics, 
   ComponentEditors, PropEdits, GLLazarusSceneEdit, FVectorEditor,

   {$IFDEF WIN32}GLWin32Viewer,{$ENDIF}
   {$IFDEF LINUX}GLLinuxViewer,{$ENDIF}

   GLMisc, GLCrossPlatform, GLStrings, GLObjects, GLGeomObjects,
   GLTeapot, GLTexture;

var
   vObjectManager : TObjectManager;

function ObjectManager : TObjectManager;
begin
   if not Assigned(vObjectManager) then
      vObjectManager:=TObjectManager.Create;
   Result:=vObjectManager;
end;

type

   // TGLSceneViewerEditor
   //
   TGLSceneViewerEditor = class(TComponentEditor)
      public
         { Public Declarations }
         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): String; override;
         function GetVerbCount: Integer; override;
   end;

   // TGLSceneEditor
   //
   TGLSceneEditor = class (TComponentEditor)
      public
         { Public Declarations }
         procedure Edit; override;

         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): String; override;
         function GetVerbCount: Integer; override;
   end;

   // TGLCoordinatesProperty
   //
   TGLCoordinatesProperty = class(TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;


//----------------- TObjectManager ---------------------------------------------

// Create
//
constructor TObjectManager.Create;
begin
  inherited;
  FSceneObjectList:=TList.Create;
  CreateDefaultObjectIcons;
end;

// Destroy
//
destructor TObjectManager.Destroy;
begin
   DestroySceneObjectList;
   FObjectIcons.Free;
   inherited Destroy;
end;

// Notify
//
//procedure TObjectManager.Notify(Sender: TPlugInManager; Operation: TOperation; PlugIn: Integer);
//begin
//end;

// FindSceneObjectClass
//
function TObjectManager.FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
                           const aSceneObject: String = '') : PSceneObjectEntry;
var
   I     : Integer;
   Found : Boolean;
begin
   Result:=nil;
   Found:=False;
   with FSceneObjectList do begin
      for I:=0 to Count-1 do
         with TGLSceneObjectEntry(Items[I]^) do
         if (ObjectClass = AObjectClass) and (Length(ASceneObject) = 0)
               or (CompareText(Name, ASceneObject) = 0) then begin
            Found:=True;
            Break;
         end;
      if Found then Result:=Items[I];
   end;
end;

// GetClassFromIndex
//
function TObjectManager.GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
begin
   if Index<0 then
      Index:=0;
   if Index>FSceneObjectList.Count-1 then
      Index:=FSceneObjectList.Count-1;
  Result:=TGLSceneObjectEntry(FSceneObjectList.Items[Index+1]^).ObjectClass;
end;

// GetImageIndex
//
function TObjectManager.GetImageIndex(ASceneObject: TGLSceneObjectClass) : Integer;
var
   classEntry : PSceneObjectEntry;
begin
   classEntry:=FindSceneObjectClass(ASceneObject);
   if Assigned(classEntry) then
      Result:=classEntry^.ImageIndex
   else Result:=0;
end;

// GetCategory
//
function TObjectManager.GetCategory(ASceneObject: TGLSceneObjectClass) : String;
var
   classEntry : PSceneObjectEntry;
begin
   classEntry:=FindSceneObjectClass(ASceneObject);
   if Assigned(classEntry) then
      Result:=classEntry^.Category
   else Result:='';
end;

// GetRegisteredSceneObjects
//
procedure TObjectManager.GetRegisteredSceneObjects(objectList: TStringList);
var
   i : Integer;
begin
   if Assigned(objectList) then with objectList do begin
      Clear;
      for i:=1 to FSceneObjectList.Count-1 do
         with TGLSceneObjectEntry(FSceneObjectList.Items[I]^) do
            AddObject(Name, Pointer(ObjectClass));
   end;
end;

// RegisterSceneObject
//
procedure TObjectManager.RegisterSceneObject(ASceneObject: TGLSceneObjectClass;
                                             const aName, aCategory : String);
var
   newEntry  : PSceneObjectEntry;
   pic       : TPicture;
   resBitmapName : String;
   bmp : TBitmap;
begin
   RegisterNoIcon([aSceneObject]);
   with FSceneObjectList do begin
      // make sure no class is registered twice
      if Assigned(FindSceneObjectClass(ASceneObject, AName)) then Exit;
      New(NewEntry);
      pic:=TPicture.Create;
      try
         with NewEntry^ do begin
            // object stock stuff
            // registered objects list stuff
            ObjectClass:=ASceneObject;
            NewEntry^.Name:=aName;
            NewEntry^.Category:=aCategory;
            Index:=FSceneObjectList.Count;
            resBitmapName:=ASceneObject.ClassName;
            GLLoadBitmapFromInstance(Pic.Bitmap,resBitmapName);
            bmp:=TBitmap.Create;
            bmp.PixelFormat:=glpf24bit;
            bmp.Width:=24; bmp.Height:=24;
            bmp.Canvas.Draw(0, 0, Pic.Bitmap);
            Pic.Bitmap:=bmp;
            bmp.Free;
            if Cardinal(Pic.Bitmap.Handle)<>0 then begin
               FObjectIcons.AddMasked(Pic.Bitmap, Pic.Bitmap.Canvas.Pixels[0, 0]);
               ImageIndex:=FObjectIcons.Count-1;
            end else ImageIndex:=0;
		   end;
         Add(NewEntry);
      finally
         pic.Free;
      end;
   end;
end;

// UnRegisterSceneObject
//
procedure TObjectManager.UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);
var
   oldEntry : PSceneObjectEntry;
begin
   // find the class in the scene object list
   OldEntry:=FindSceneObjectClass(ASceneObject);
   // found?
   if assigned(OldEntry) then begin
      // remove its entry from the list of registered objects
      FSceneObjectList.Remove(OldEntry);
      // finally free the memory for the entry
      Dispose(OldEntry);
   end;
end;

// CreateDefaultObjectIcons
//
procedure TObjectManager.CreateDefaultObjectIcons;
var
   pic : TPicture;
begin
   pic:=TPicture.Create;
   // load first pic to get size
   GLLoadBitmapFromInstance(Pic.Bitmap,'gls_cross');
   //FObjectIcons:=TImageList.CreateSize(Pic.Width, Pic.height);
   FObjectIcons:=TImageList.CreateSize(16, 16);

   with FObjectIcons, pic.Bitmap.Canvas do begin
      try
         {$ifdef FPC}Exit;{$endif}
         // There's a more direct way for loading images into the image list, but
         // the image quality suffers too much
         AddMasked(Pic.Bitmap, Pixels[0, 0]);
         {$ifdef WIN32}
         FOverlayIndex:=Count-1;
         //Overlay(FOverlayIndex, 0); // used as indicator for disabled objects
         {$endif}
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_root');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FSceneRootIndex:=Count-1;
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_camera');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FCameraRootIndex:=Count-1;
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_lights');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FLightsourceRootIndex:=Count-1;
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_objects');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FObjectRootIndex:=Count-1;
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FStockObjectRootIndex:=Count-1;
      finally
         Pic.Free;
      end;
   end;
end;

// DestroySceneObjectList
//
procedure TObjectManager.DestroySceneObjectList;
var
   i : Integer;
begin
   with FSceneObjectList do begin
      for i:=0 to Count-1 do
         Dispose(PSceneObjectEntry(Items[I]));
      Free;
   end;
end;


//----------------- TGLSceneViewerEditor ---------------------------------------

// ExecuteVerb
//
procedure TGLSceneViewerEditor.ExecuteVerb(Index : Integer);
var
  source : TGLSceneViewer;
begin
  source:=Component as TGLSceneViewer;
  case Index of
    0 : source.Buffer.ShowInfo;
  end;
end;

// GetVerb
//
function TGLSceneViewerEditor.GetVerb(Index : Integer) : String;
begin
  case Index of
    0 : Result:='Show context info';
  end;
end;

// GetVerbCount
//
function TGLSceneViewerEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;


//----------------- TGLSceneEditor ---------------------------------------------

// Edit
//
procedure TGLSceneEditor.Edit;
begin
   with GLSceneEditorForm do begin
      SetScene(Self.Component as TGLScene, TComponentEditorDesigner(Self.Designer));
      Show;
   end;
end;

// ExecuteVerb
//
procedure TGLSceneEditor.ExecuteVerb(Index : Integer);
begin
   case Index of
      0 : Edit;
   end;
end;

// GetVerb
//
function TGLSceneEditor.GetVerb(Index : Integer) : String;
begin
   case Index of
      0 : Result:='Show Scene Editor';
   end;
end;

// GetVerbCount
//
function TGLSceneEditor.GetVerbCount: Integer;
begin
   Result:=1;
end;


//----------------- TGLCoordinatesProperty -------------------------------------

// GetAttributes
//
function TGLCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog, paSubProperties];
end;

// Edit;
//
procedure TGLCoordinatesProperty.Edit;
var
   glc : TGLCoordinates;
   x, y, z : Single;
begin
   glc:=TGLCoordinates(GetOrdValue);
   x:=glc.x;
   y:=glc.y;
   z:=glc.z;
   if VectorEditorForm.Execute(x, y, z) then begin
      glc.AsVector:=VectorMake(x, y, z);
      Modified;
   end;
end;


procedure Register;
begin
   RegisterComponents('GLScene',
                      [TGLScene,
                       TGLSceneViewer,
                       TGLMaterialLibrary
                      ]);

   RegisterComponentEditor(TGLSceneViewer, TGLSceneViewerEditor);
   RegisterComponentEditor(TGLScene, TGLSceneEditor);

   RegisterClasses([TGLCoordinates]);

   RegisterPropertyEditor(TypeInfo(TGLCoordinates), nil, '', TGLCoordinatesProperty);
end;

initialization

   GLMisc.vUseDefaultSets:=True;
   //ReadVideoModes;

   with ObjectManager do begin
      RegisterSceneObject(TGLCamera, 'Camera', '');
      RegisterSceneObject(TGLLightSource, 'LightSource', '');
      RegisterSceneObject(TGLDummyCube, 'DummyCube', '');

      RegisterSceneObject(TGLSprite, 'Sprite', glsOCBasicGeometry);
      RegisterSceneObject(TGLPoints, 'Points', glsOCBasicGeometry);
      RegisterSceneObject(TGLLines, 'Lines', glsOCBasicGeometry);
      RegisterSceneObject(TGLPlane, 'Plane', glsOCBasicGeometry);
      RegisterSceneObject(TGLPolygon, 'Polygon', glsOCBasicGeometry);
      RegisterSceneObject(TGLCube, 'Cube', glsOCBasicGeometry);
      RegisterSceneObject(TGLFrustrum, 'Frustrum', glsOCBasicGeometry);
      RegisterSceneObject(TGLSphere, 'Sphere', glsOCBasicGeometry);
      RegisterSceneObject(TGLDisk, 'Disk', glsOCBasicGeometry);
      RegisterSceneObject(TGLCone, 'Cone', glsOCBasicGeometry);
      RegisterSceneObject(TGLCylinder, 'Cylinder', glsOCBasicGeometry);
      //RegisterSceneObject(TGLDodecahedron, 'Dodecahedron', glsOCBasicGeometry);
      //RegisterSceneObject(TGLIcosahedron, 'Icosahedron', glsOCBasicGeometry);
      
      RegisterSceneObject(TGLTeapot, 'Teapot', glsOCDoodad);
   end;

finalization

   ObjectManager.Free;

end.
