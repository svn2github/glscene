{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_DesignTime; 

interface

uses
    FXCollectionEditorLCL, GLExpandPanels, GLSceneEditLCL, GLSceneRegisterLCL, 
  GLSIDENotifierLCL, RegisterXCollection, GL3xMaterialCode, 
  GL3xMaterialEditor, GL3xMaterialNodePaletteLCL, GL3xMaterialPreview, 
  GLSceneFormDesign, GLObjectManager, FLibMaterialPickerLCL, 
  FMaterialEditorFormLCL, FRColorEditorLCL, FRFaceEditorLCL, 
  FRMaterialPreviewLCL, FRTextureEditLCL, FRTrackBarEditLCL, FVectorEditorLCL, 
  InfoLCL, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLExpandPanels', @GLExpandPanels.Register); 
  RegisterUnit('GLSceneRegisterLCL', @GLSceneRegisterLCL.Register); 
  RegisterUnit('GLSIDENotifierLCL', @GLSIDENotifierLCL.Register); 
  RegisterUnit('RegisterXCollection', @RegisterXCollection.Register); 
  RegisterUnit('GLSceneFormDesign', @GLSceneFormDesign.Register); 
end; 

initialization
  RegisterPackage('GLScene_DesignTime', @Register); 
end.
