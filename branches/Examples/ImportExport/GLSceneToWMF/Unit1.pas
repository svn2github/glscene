unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
   
  GLObjects, GLScene, GLWin32Viewer, GLCoordinates,
  GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLines1: TGLLines;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  MF : TMetaFile;
  MFC : TMetaFileCanvas;
  i: Integer;
begin
  MF := TMetafile.Create;
  MF.Width := 500;
  MF.Height := 500;
  MFC := TMetafileCanvas.CreateWithComment(MF, getwindowdc(0), '', '');

  //whatever logic you need here
  for i := 0 to GLLines1.Nodes.Count - 1 do
  begin
    if i = 0 then
      MFC.MoveTo(round(GLLines1.Nodes.Items[i].X * 100) + 250,round(GLLines1.Nodes.Items[i].Y * 100)+ 250);
    MFC.LineTo(round(GLLines1.Nodes.Items[i].X * 100) + 250,round(GLLines1.Nodes.Items[i].Y * 100)+ 250);
  end;

  
  MFC.Free;
  MF.SaveToFile('D:\test.wmf');
  MF.destroy;
end;

end.
