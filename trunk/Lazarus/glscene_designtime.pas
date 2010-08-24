{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_DesignTime; 

interface

uses
    GLObjectManager, GLSceneRegisterLCL, FVectorEditorLCL, InfoLCL, 
  FMaterialEditorFormLCL, FRMaterialPreviewLCL, FLibMaterialPickerLCL, 
  FRTextureEditLCL, FRColorEditorLCL, FRFaceEditorLCL, FRTrackBarEditLCL, 
  RegisterXCollection, FXCollectionEditorLCL, GLSceneEditLCL, 
  GL3xMaterialPreview, GL3xMaterialCode, GL3xMaterialEditor, 
  GL3xMaterialNodePaletteLCL, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSceneRegisterLCL', @GLSceneRegisterLCL.Register); 
  RegisterUnit('RegisterXCollection', @RegisterXCollection.Register); 
end; 

initialization
  RegisterPackage('GLScene_DesignTime', @Register); 
end.
