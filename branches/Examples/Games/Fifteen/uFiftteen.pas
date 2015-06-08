unit uFiftteen;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Math, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls,
  Vcl.Imaging.Jpeg,
  //GLS
  GLScene, GLObjects, GLWin32Viewer, GLVectorFileObjects, GLVectortypes,
  GLVectorGeometry, GLTexture, GLMaterial, GLCoordinates, GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLCube1: TGLCube;
    GLCube2: TGLCube;
    GLLightSource1: TGLLightSource;
    GLCube3: TGLCube;
    GLCube4: TGLCube;
    GLCube5: TGLCube;
    GLMaterialLibrary1: TGLMaterialLibrary;
    StatusBar1: TStatusBar;
    GLCube6: TGLCube;
    GLCube7: TGLCube;
    GLCube8: TGLCube;
    GLCube9: TGLCube;
    GLCube10: TGLCube;
    GLCube11: TGLCube;
    GLCube12: TGLCube;
    GLCube13: TGLCube;
    GLCube14: TGLCube;
    GLCube15: TGLCube;
    GLCube16: TGLCube;
    GLDummyCube2: TGLDummyCube;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mdx, mdy : Integer;
  pickdown : TGLCustomSceneObject;
  pickup : TGLCustomSceneObject;
  cor_antiga, cor_nova: Tvector4f;
  cor_a, cor_n: Tvector4f;

Const
  clrWhite: TVector = (X:1; Y:1; Z:1; W:1);
  
implementation

{$R *.dfm}

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then
      GLCamera1.MoveAroundTarget(mdy-y, mdx-x);
	mdx:=x; mdy:=y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
   f : Single;
begin
   if (WheelDelta>0) or (GLCamera1.Position.VectorLength>0.90) then begin
      f:=Power(1.05, WheelDelta*(1/120));
      GLCamera1.AdjustDistanceToTarget(f);
   end;
   Handled:=True;

end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if Button = TMouseButton(mbLeft) then   //testa se o botão do mouse clicado foi o esquerdo
 begin
   pickdown:=(GLSceneViewer1.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
   {if Assigned(pickdown) and (pickdown is TGLCube) then
        begin
        cor_antiga:=pickdown.Material.FrontProperties.Emission.Color;
        cor_nova:=vectorlerp(cor_antiga, clrwhite, 0.25);
  //       if pickdown.Name='Base1' then
   //      pickdown.Material.FrontProperties.Emission.Color:=cor_antiga
    //     else
         pickdown.Material.FrontProperties.Emission.Color:=cor_nova;
        end;}
 end;
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
Pos: TGLCoordinates;
Ze: single;
begin
if Button = TMouseButton(mbLeft) then  //testa se o botão do mouse clicado foi o esquerdo
 begin
  pickup:=(GLSceneViewer1.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
  if Assigned(pickup)  then
       if  (pickup=pickdown) and (pickup is TGLCube) then
           begin
         //  pickup.Material.FrontProperties.Emission.Color:=cor_antiga;
            if (pickup.Position.X=GLCube1.Position.X) and  (pickup.Position.Y=GLCube1.Position.Y)
            and  ((pickup.Position.z=GLCube1.Position.z+1)  or (pickup.Position.z=GLCube1.Position.z-1) )
            then
              begin
              Ze:=pickup.Position.z;
              pickup.Position.z:=GLCube1.Position.z;
              GLCube1.Position.z:=Ze;
              end;
            if (pickup.Position.Z=GLCube1.Position.Z) and  (pickup.Position.Y=GLCube1.Position.Y)
            and  ((pickup.Position.X=GLCube1.Position.X+1)  or (pickup.Position.X=GLCube1.Position.X-1) )
            then
              begin
              Ze:=pickup.Position.x;
              pickup.Position.x:=GLCube1.Position.x;
              GLCube1.Position.x:=Ze;
              end;
          {  if (pickup.Position.X=GLCube1.Position.X) and  (pickup.Position.Z=GLCube1.Position.Z)
            and  ((pickup.Position.Y=GLCube1.Position.Y+1)  or (pickup.Position.Y=GLCube1.Position.Y-1) )
            then
              begin
              Ze:=pickup.Position.z;
              pickup.Position.z:=GLCube1.Position.z;
              GLCube1.Position.z:=Ze;
              end;}
         end;




  //     if (pickdown is TGLCube) and (pickup<>pickdown) then
   //    pickdown.Material.FrontProperties.Emission.Color:=cor_antiga;

 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
i: integer;
 bmp : TBitmap;
begin
Glmateriallibrary1.Materials[0].Material.Texture.Image.LoadFromFile('Ys.jpg');
Glmateriallibrary1.Materials[1].Material.Texture.Image.LoadFromFile('Yi.jpg');
Glmateriallibrary1.Materials[2].Material.Texture.Image.LoadFromFile('Ym.jpg');
Glmateriallibrary1.Materials[3].Material.Texture.Image.LoadFromFile('Gb.jpg');
Glmateriallibrary1.Materials[4].Material.Texture.Image.LoadFromFile('Go.jpg');
Glmateriallibrary1.Materials[5].Material.Texture.Image.LoadFromFile('Gl.jpg');
Glmateriallibrary1.Materials[6].Material.Texture.Image.LoadFromFile('Ga.jpg');
Glmateriallibrary1.Materials[7].Material.Texture.Image.LoadFromFile('Op.jpg');
Glmateriallibrary1.Materials[8].Material.Texture.Image.LoadFromFile('Oo.jpg');
Glmateriallibrary1.Materials[9].Material.Texture.Image.LoadFromFile('Ot.jpg');
Glmateriallibrary1.Materials[10].Material.Texture.Image.LoadFromFile('Oe.jpg');
Glmateriallibrary1.Materials[11].Material.Texture.Image.LoadFromFile('Pc.jpg');
Glmateriallibrary1.Materials[12].Material.Texture.Image.LoadFromFile('Pr.jpg');
Glmateriallibrary1.Materials[13].Material.Texture.Image.LoadFromFile('Pu.jpg');
Glmateriallibrary1.Materials[14].Material.Texture.Image.LoadFromFile('Pa.jpg');


{ for i:=1 to 16 do begin
      bmp:=TBitmap.Create;
      bmp.PixelFormat:=pf24bit;
      bmp.Width:=60;
      bmp.Height:=60;
      bmp.Canvas.Font.Name:='Arial';
      bmp.Canvas.Font.Height:=36;
      bmp.Canvas.TextOut(15, 5, IntToStr(i));
      GLMaterialLibrary1.AddTextureMaterial('IMG'+IntToStr(i), bmp);
      bmp.Free;
   end;
  GLCube1.Material.LibMaterialName:='IMG1';
  GLCube2.Material.LibMaterialName:='IMG2';
  GLCube3.Material.LibMaterialName:='IMG3';
  GLCube4.Material.LibMaterialName:='IMG4';
  GLCube5.Material.LibMaterialName:='IMG5';
  GLCube6.Material.LibMaterialName:='IMG6';
  GLCube7.Material.LibMaterialName:='IMG7';
  GLCube8.Material.LibMaterialName:='IMG8';
  GLCube9.Material.LibMaterialName:='IMG9';
  GLCube10.Material.LibMaterialName:='IMG10';
  GLCube11.Material.LibMaterialName:='IMG11';
  GLCube12.Material.LibMaterialName:='IMG12';
  GLCube13.Material.LibMaterialName:='IMG13';
  GLCube14.Material.LibMaterialName:='IMG14';
  GLCube15.Material.LibMaterialName:='IMG15';
  GLCube16.Material.LibMaterialName:='IMG16'; }
end;

end.
