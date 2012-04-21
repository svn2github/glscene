unit uDev;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene_Cadencer, GLScene_Objects, GLScene_Core, GLScene_Base_Coordinates,
  GLScene_Platform, GLScene_Base_Classes, GLScene_Viewer_Form, GLScene_Material,
  GLScene_MaterialEx, GLScene_SimpleNavigation;

type
  TGLMainForm5 = class(TGLSceneForm)
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLCube1: TGLCube;
    GLMaterialLibraryEx1: TGLMaterialLibraryEx;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GLMainForm5 : TGLMainForm5;

implementation

{$R *.lfm}

procedure TGLMainForm5.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  Invalidate;
end;

end.