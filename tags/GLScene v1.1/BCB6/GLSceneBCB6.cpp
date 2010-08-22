//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\Source\DesignTime\FRColorEditor.pas", Frcoloreditor, RColorEditor); /* TFrame: File Type */
USEFORMNS("..\Source\DesignTime\FRTrackBarEdit.pas", Frtrackbaredit, RTrackBarEdit); /* TFrame: File Type */
USEFORMNS("..\Source\DesignTime\FRFaceEditor.pas", Frfaceeditor, RFaceEditor); /* TFrame: File Type */
USEFORMNS("..\Source\DesignTime\FRTextureEdit.pas", Frtextureedit, RTextureEdit); /* TFrame: File Type */
USEFORMNS("..\Source\DesignTime\FVectorEditor.pas", Fvectoreditor, VectorEditorForm);
USEFORMNS("..\Source\DesignTime\GLSceneEdit.pas", Glsceneedit, GLSceneEditorForm);
USEFORMNS("..\Source\DesignTime\Info.pas", Info, InfoForm);
USEFORMNS("..\Source\PlugIn\PlugInManagerPropEditor.pas", Pluginmanagerpropeditor, PlugInManagerPropForm);
USEFORMNS("..\Source\DesignTime\FXCollectionEditor.pas", Fxcollectioneditor, XCollectionEditor);
USEFORMNS("..\Source\DesignTime\FRMaterialPreview.pas", Frmaterialpreview, RMaterialPreview); /* TFrame: File Type */
USEFORMNS("..\Source\DesignTime\FMaterialEditorForm.pas", Fmaterialeditorform, MaterialEditorForm);
USEFORMNS("..\Source\DesignTime\FLibMaterialPicker.pas", Flibmaterialpicker, LibMaterialPicker);
USEFORMNS("..\Source\DesignTime\GuiSkinEditorFormUnit.pas", Guiskineditorformunit, GUISkinEditor);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------
 