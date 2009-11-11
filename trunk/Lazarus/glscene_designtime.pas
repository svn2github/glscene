unit GLScene_DesignTime; 

interface

uses
    GLObjectManager, GLSceneRegisterLCL, FVectorEditor, Info, 
  FMaterialEditorForm, FRMaterialPreview, FLibMaterialPicker, FRTextureEdit, 
  FRColorEditor, FRFaceEditor, FRTrackBarEdit, RegisterXCollection, 
  FXCollectionEditorLCL, GLSceneEditLCL, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSceneRegisterLCL', @GLSceneRegisterLCL.Register); 
  RegisterUnit('RegisterXCollection', @RegisterXCollection.Register); 
end; 

initialization
  RegisterPackage('GLScene_DesignTime', @Register); 
end.
