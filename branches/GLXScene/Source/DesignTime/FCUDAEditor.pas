//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
   Editor of TGLSCUDA
}

unit FCUDAEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation,
  
  GLX.Strings,
  GLX.CUDA, GLX.CUDAFFTPlan, GLX.CUDAGraphics;

type
  TGLSCUDAEditorForm = class(TForm)
    ToolBar1: TToolBar;
    SBOpen: TSpeedButton;
    Image1: TImage;
    SBSave: TSpeedButton;
    Image2: TImage;
    SBHelp: TSpeedButton;
    Image3: TImage;
    ListBox1: TListBox;
  private
    
  public
    
  end;

var
  GLSCUDAEditorForm: TGLSCUDAEditorForm;

function GLSCUDAEditorForm: TGLSCUDAEditorForm;
procedure ReleaseGLSCUDAEditorForm;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

{$R *.fmx}

const
  cRegistryKey = 'Software\GLScene\GLSCUDAEditor';

var
  vGLSCUDAEditorForm: TGLSCUDAEditorForm;

function GLSCUDAEditorForm: TGLSCUDAEditorForm;
begin
  if not Assigned(vGLSCUDAEditorForm) then
    vGLSCUDAEditorForm := TGLSCUDAEditorForm.Create(nil);
  Result := vGLSCUDAEditorForm;
end;

procedure ReleaseGLSCUDAEditorForm;
begin
  if Assigned(vGLSCUDAEditorForm) then
  begin
    vGLSCUDAEditorForm.Free;
    vGLSCUDAEditorForm := nil;
  end;
end;

end.
