//
// This unit is part of the GLScene Project   
//
{: GLSCUDAEditor<p>

   Editor of TGLSCUDA.<p>

	<b>History : </b><font size=-1><ul>
      <li>06/01/15 - PW - Updated to use with FMX
      <li>22/08/10 - Yar - Some improvements for FPC (thanks Predator)
      <li>19/03/10 - Yar - Creation
	</ul></font>
}
unit FCUDAEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Objects, FMX.StdCtrls,

  GLS.CUDA, GLS.CUDAFFTPlan, GLS.CUDAGraphics;

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
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GLSCUDAEditorForm: TGLSCUDAEditorForm;

function GLSCUDAEditorForm: TGLSCUDAEditorForm;
procedure ReleaseGLSCUDAEditorForm;

implementation

{$R *.fmx}

resourcestring
  cCUDAEditor = 'GLScene CUDA Component Editor';

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
