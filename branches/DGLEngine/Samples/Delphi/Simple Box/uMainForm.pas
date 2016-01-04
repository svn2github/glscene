unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DGLVectorObjects, DGLCoordinates, DGLScene, DGLWin32Viewer, DGLCrossPlatform, DGLBaseClasses, DGLCadencer;

type
  TForm1 = class(TForm)
    DGLScene1: TDGLScene;
    DGLSceneViewer1: TDGLSceneViewer;
    DGLCadencer1: TDGLCadencer;
    DGLCamera1: TDGLCamera;
    DGLCube1: TDGLCube;
    procedure DGLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DGLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
//  DGLCamera1.MoveAroundTarget(15*deltatime,0);
//  DGLSceneViewer1.Invalidate;
end;

end.
