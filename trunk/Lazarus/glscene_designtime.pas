{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_DesignTime; 

interface

uses
    FXCollectionEditorLCL, GLSceneEditLCL, GLSceneRegisterLCL, 
  RegisterXCollection, GLObjectManager, FLibMaterialPickerLCL, 
  FMaterialEditorFormLCL, FRColorEditorLCL, FRFaceEditorLCL, 
  FRMaterialPreviewLCL, FRTextureEditLCL, FRTrackBarEditLCL, FVectorEditorLCL, 
  InfoLCL, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSceneRegisterLCL', @GLSceneRegisterLCL.Register); 
  RegisterUnit('RegisterXCollection', @RegisterXCollection.Register); 
end; 

initialization
  RegisterPackage('GLScene_DesignTime', @Register); 
end.
