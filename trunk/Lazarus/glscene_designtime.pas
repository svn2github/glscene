unit glscene_designtime; 

interface

uses
    GLObjectManager, GLSceneRegisterLCL, FVectorEditorLCL, InfoLCL, 
  FMaterialEditorFormLCL, FRMaterialPreviewLCL, FLibMaterialPickerLCL, 
  FRTextureEditLCL, FRColorEditorLCL, FRFaceEditorLCL, FRTrackBarEditLCL, 
  RegisterXCollection, FXCollectionEditorLCL, GLSceneEditLCL, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSceneRegisterLCL', @GLSceneRegisterLCL.Register); 
  RegisterUnit('RegisterXCollection', @RegisterXCollection.Register); 
end; 

initialization
  RegisterPackage('GLScene_DesignTime', @Register); 
end.
