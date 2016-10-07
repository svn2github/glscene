//
// VKScene project, http://glscene.sourceforge.net
//
{
   Editor of TVKSCUDA
}

unit FCUDAEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation,
  //VKS
  VKS.Strings,
  VKS.CUDA, VKS.CUDAFFTPlan, VKS.CUDAGraphics;

type
  TVKSCUDAEditorForm = class(TForm)
    ToolBar1: TToolBar;
    SBOpen: TSpeedButton;
    Image1: TImage;
    SBSave: TSpeedButton;
    Image2: TImage;
    SBHelp: TSpeedButton;
    Image3: TImage;
    ListBox1: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GLSCUDAEditorForm: TVKSCUDAEditorForm;

function GLSCUDAEditorForm: TVKSCUDAEditorForm;
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
  vGLSCUDAEditorForm: TVKSCUDAEditorForm;

function GLSCUDAEditorForm: TVKSCUDAEditorForm;
begin
  if not Assigned(vGLSCUDAEditorForm) then
    vGLSCUDAEditorForm := TVKSCUDAEditorForm.Create(nil);
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