unit GLScene_DesignTime; 

interface

uses
    GLObjectManager, GLSceneRegister, FVectorEditor, Info, 
  FMaterialEditorForm, FRMaterialPreview, FLibMaterialPicker, FRTextureEdit, 
  FRColorEditor, FRFaceEditor, FRTrackBarEdit, RegisterXCollection, 
  FXCollectionEditor, GLSceneEdit, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSceneRegister', @GLSceneRegister.Register); 
  RegisterUnit('RegisterXCollection', @RegisterXCollection.Register); 
end; 

initialization
  RegisterPackage('GLScene_DesignTime', @Register);

end.
