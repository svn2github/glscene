unit Unit1;


interface


uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Math,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,

  GLVectorTypes,
  GLScene,
  GLObjects,
  GLWin32Viewer,
  GLTexture,
  GLContext,
  GLVectorGeometry,
  GLCadencer,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLRenderContextInfo,
  Texture_3D_Unit;


type
  TForm1 = class (TForm)
    GLScene : TGLScene;
    GLCamera : TGLCamera;
    GLDirectOpenGL : TGLDirectOpenGL;
    GLCadencer : TGLCadencer;
    Timer : TTimer;
    Panel1 : TPanel;
    Panel2 : TPanel;
    GLSceneViewer : TGLSceneViewer;
    GLLightSource : TGLLightSource;
    Frame : TGLLines;
    GLCube1 : TGLCube;
    Panel3 : TPanel;
    Panel4 : TPanel;
    Panel5 : TPanel;
    Panel6 : TPanel;
    Cutting_Plane_Pos_TB : TTrackBar;
    Projection_N_TB : TTrackBar;
    Panel7 : TPanel;
    Label1 : TLabel;
    Label2 : TLabel;
    Cutting_Plane_Pos_L : TLabel;
    Projection_N_L : TLabel;
    Box_CB : TCheckBox;
    Pseudocolor_CB : TCheckBox;
    Opaque_Hull_CB : TCheckBox;
    Panel8: TPanel;
    Panel9: TPanel;
    Label3: TLabel;
    Alpha_Threshold_L: TLabel;
    Alpha_Threshold_TB: TTrackBar;
    procedure GLDirectOpenGLRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormCreate (Sender : TObject);
    procedure FormDestroy (Sender : TObject);
    procedure GLSceneViewerMouseDown (Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure GLSceneViewerMouseMove (Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure GLCadencerProgress (Sender : TObject; Const deltaTime, newTime : Double);
    procedure TimerTimer (Sender : TObject);
    procedure Cutting_Plane_Pos_TBChange (Sender : TObject);
    procedure Projection_N_TBChange (Sender : TObject);
    procedure Box_CBClick (Sender : TObject);
    procedure Pseudocolor_CBClick (Sender : TObject);
    procedure Opaque_Hull_CBClick (Sender : TObject);
    procedure Alpha_Threshold_TBChange(Sender: TObject);
  protected
    procedure Calculate_Transfer_Function;
  public
    M_mx : Integer;
    M_my : Integer;
    M_3D_Texture : TGLTextureHandle;
    M_Refresh : Boolean;
    M_Input_Texture_3D : TTexture_3D;
    M_Output_Texture_3D : TTexture_3D;
    M_CLUT : Array [0..255] Of Integer;
  end;


var
  Form1 : TForm1;


implementation


{$R *.dfm}


const
  TEXTURE_INPUT_FILENAME = 'Head.tx3';
  DIAGONAL_LENGTH = 1.732;


type
  P_Byte = ^Byte;
  P_Longword = ^Longword;


var
  clip0 : Array [0..3] Of Double = (-1.0, 0.0, 0.0, 1 / 2.0);
  clip1 : Array [0..3] Of Double = (1.0, 0.0, 0.0, 1 / 2.0);
  clip2 : Array [0..3] Of Double = (0.0, -1.0, 0.0, 1 / 2.0);
  clip3 : Array [0..3] Of Double = (0.0, 1.0, 0.0, 1 / 2.0);
  clip4 : Array [0..3] Of Double = (0.0, 0.0, -1.0, 1 / 2.0);
  clip5 : Array [0..3] Of Double = (0.0, 0.0, 1.0, 1 / 2.0);
  Border_Colors : Array [0..3] Of Integer = (0, 0, 0, 0);



procedure TForm1.FormCreate (Sender : TObject);
  function HSLtoRGB (H, S, L : Single) : Longword;
  const
    OneOverThree = 1 / 3;
  var
    M1, M2 : Single;
    R, G, B : Byte;

    function HueToColor (Hue : Single) : Byte;
    Var
      V : Double;
    begin
      Hue := Hue - Floor (Hue);
      If 6 * Hue < 1 Then
        V := M1 + (M2 - M1) * Hue * 6
      Else
      If 2 * Hue < 1 Then
          V := M2
        Else
        If 3 * Hue < 2 Then
            V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
          Else
            V := M1;
      Result := Round (255 * V);
    end;

  begin
    If S = 0 then
      begin
        R := Round (255 * L);
        G := R;
        B := R;
      End
    else
      begin
        If L <= 0.5 then
          M2 := L * (1 + S)
        else
          M2 := L + S - L * S;
        M1 := 2 * L - M2;
        R := HueToColor (H + OneOverThree);
        G := HueToColor (H);
        B := HueToColor (H - OneOverThree)
      end;
    Result := R + (G Shl 8) + (B Shl 16);
  end;

const
  COLOR_CONSTANT_LUMINANCE_HSL_H_OFFSET = 2 / 3;
  COLOR_CONSTANT_LUMINANCE_HSL_H_FACTOR = 0.85;
  COLOR_CONSTANT_LUMINANCE_HSL_S = 0.9;
  COLOR_CONSTANT_LUMINANCE_HSL_L = 0.6;

var
  N_begin : Integer;
  N_End : Integer;
  Left_Value : Integer;
  Right_Value : Integer;
  I : Integer;
  Value : Longword;
  H_0 : Double;
  H_1 : Double;

begin
  M_3D_Texture := TGLTextureHandle.Create;

  M_Input_Texture_3D := TTexture_3D.Create;
  M_Input_Texture_3D.Load_From_File (TEXTURE_INPUT_FILENAME);

  M_Output_Texture_3D := TTexture_3D.Create;
  M_Output_Texture_3D.Data_Type := GL_RGBA;
  M_Output_Texture_3D.X_Size := M_Input_Texture_3D.X_Size;
  M_Output_Texture_3D.Y_Size := M_Input_Texture_3D.Y_Size;
  M_Output_Texture_3D.Z_Size := M_Input_Texture_3D.Z_Size;

  M_Refresh := TRUE;
  Cutting_Plane_Pos_TB.Position := M_Output_Texture_3D.Y_Size Div 2;
  Alpha_Threshold_TB.Position := 60;
  Projection_N_TB.Position := M_Output_Texture_3D.Y_Size;

  { Calculate Color Lookup Table }
  N_begin := 0;
  N_End := 255;
  Left_Value := 60;
  Right_Value := 255;

  for I := N_begin To Left_Value - 1 Do
    begin { For }
      H_1 := COLOR_CONSTANT_LUMINANCE_HSL_H_OFFSET;

      Value := HSLToRGB (H_1, COLOR_CONSTANT_LUMINANCE_HSL_S, COLOR_CONSTANT_LUMINANCE_HSL_L);

      M_CLUT [I] := Value;
    end; { For }
  for I := Left_Value To Right_Value Do
    begin { For }
      Value := I;

      H_0 := (Int (Value) - Int (Left_Value)) / Int (256);
      H_1 := COLOR_CONSTANT_LUMINANCE_HSL_H_OFFSET - (H_0 * COLOR_CONSTANT_LUMINANCE_HSL_H_FACTOR);

      Value := HSLToRGB (H_1, COLOR_CONSTANT_LUMINANCE_HSL_S, COLOR_CONSTANT_LUMINANCE_HSL_L);

      M_CLUT [I] := Value;
    end; { For }
  for I := Right_Value + 1 To N_End Do
    begin { For }
      H_1 := COLOR_CONSTANT_LUMINANCE_HSL_H_OFFSET - (COLOR_CONSTANT_LUMINANCE_HSL_H_FACTOR);

      Value := HSLToRGB (H_1, COLOR_CONSTANT_LUMINANCE_HSL_S, COLOR_CONSTANT_LUMINANCE_HSL_L);

      M_CLUT [I] := Value;
    end; { For }
end;

procedure TForm1.FormDestroy (Sender : TObject);
begin
  M_3D_Texture.Free;
  M_Input_Texture_3D.Free;
  M_Output_Texture_3D.Free;
end;

procedure TForm1.GLSceneViewerMouseDown (Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  M_mx := x;
  M_my := y;
end;

procedure TForm1.GLSceneViewerMouseMove (Sender : TObject; Shift : TShiftState; X, Y : Integer);
begin
  If Shift = [ssLeft] Then
    begin { then }
      GLCamera.MoveAroundTarget (M_my - y, M_mx - x);
    end; { then }

  If Shift = [ssRight] Then
    begin { then }
      GLCamera.Position.AddScaledVector ((M_my - y) * 0.01, GLCamera.AbsoluteVectorToTarget);
    end; { then }

  M_mx := x;
  M_my := y;
end;

procedure TForm1.GLCadencerProgress (Sender : TObject; Const deltaTime, newTime : Double);
begin
  GLSceneViewer.Invalidate;
end;

procedure TForm1.TimerTimer (Sender : TObject);
begin
  Caption := 'Texture 3D  ' +GLSceneViewer.FramesPerSecondText;
  GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TForm1.Calculate_Transfer_function;
var
  X : Integer;
  Y : Integer;
  Z : Integer;
  Index : Integer;
  Value : Integer;
  Alpha : Integer;

begin
  { Set texture values }
  For Z := 0 To M_Output_Texture_3D.Z_Size - 1 Do
    begin { For }
      For Y := 0 To M_Output_Texture_3D.Y_Size - 1 Do
        begin { For }
          For X := 0 To M_Output_Texture_3D.X_Size - 1 Do
            begin { For }
              Index := (Z * M_Output_Texture_3D.Y_Size * M_Output_Texture_3D.X_Size) + (Y * M_Output_Texture_3D.X_Size) + X;
              Value := P_Byte (Integer (PChar (M_Input_Texture_3D.Data)) + Index)^ * 2;

              If (Value < Alpha_Threshold_TB.Position) Or (Y > Cutting_Plane_Pos_TB.Position) Or (X = 0) Or (X = M_Output_Texture_3D.X_Size - 1) Or (Y = 0) Or (Y = M_Output_Texture_3D.Y_Size - 1) Or (Z = 0) Or (Z = M_Output_Texture_3D.Z_Size - 1) then
                begin { then }
                  Alpha := 0;
                end { then }
              else
                begin { else }
                  If Opaque_Hull_CB.Checked = TRUE Then
                    begin { then }
                      Alpha := 255;
                    end { then }
                  else
                    begin { else }
                      Alpha := Value;
                    end; { else }
                end; { else }

              If Pseudocolor_CB.Checked = TRUE then
                begin { then }
                  P_Longword (Integer (PChar (M_Output_Texture_3D.Data)) + (Index * 4))^ := M_Clut [Value];
                end { then }
              else
                begin { else }
                  P_Longword (Integer (PChar (M_Output_Texture_3D.Data)) + (Index * 4))^ := Value + (Value Shl 8) + (Value Shl 16);
                end; { else }

              P_Byte (Integer (PChar (M_Output_Texture_3D.Data)) + (Index * 4) + 3)^ := Alpha
            end; { For }
        end; { For }
    end; { For }
end;

procedure TForm1.GLDirectOpenGLRender(Sender: TObject; var rci: TGLRenderContextInfo);

Var
  i : Integer;
  step : Single;
  z : Single;
  mat : TMatrix;
  v : TVector;
  vx, vy, vz : TAffineVector;

begin
  if M_3D_Texture.Handle = 0 then
    begin
      //Assert (GL_EXT_texture3D, 'Graphic card does not support 3D textures.');

      M_3D_Texture.AllocateHandle;

      gl.BindTexture (GL_TEXTURE_3D, M_3D_Texture.Handle);
      gl.TexParameterf (GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP);
      gl.TexParameterf (GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP);
      gl.TexParameterf (GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP);
      gl.TexParameteri (GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      gl.TexParameteri (GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      gl.TexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE); //GL_REPLACE);

      gl.TexImage3D (GL_TEXTURE_3D, 0, GL_RGBA8, M_Output_Texture_3D.X_Size, M_Output_Texture_3D.Y_Size, M_Output_Texture_3D.Z_Size, 0, M_Output_Texture_3D.Data_Type, GL_UNSIGNED_BYTE, PChar (M_Output_Texture_3D.Data));
    end;

  If M_Refresh = TRUE Then
    begin { then }
      M_Refresh := FALSE;
      Calculate_Transfer_function;
      gl.TexSubImage3D (GL_TEXTURE_3D, 0, 0, 0, 0, M_Output_Texture_3D.X_Size, M_Output_Texture_3D.Y_Size, M_Output_Texture_3D.Z_Size, M_Output_Texture_3D.Data_Type, GL_UNSIGNED_BYTE, PChar (M_Output_Texture_3D.Data));
    end; { then }

  gl.PushAttrib (GL_ENABLE_BIT);
  gl.PushMatrix;

  gl.Enable (GL_BLEND);
  gl.BlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  //  glDisable (GL_CULL_FACE);
  //  glDisable (GL_LIGHTING);

  gl.TexGenf (GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
  gl.TexGenf (GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
  gl.TexGenf (GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
  SetVector (v, XVector, 0.5);
  gl.TexGenfv (GL_S, GL_OBJECT_PLANE, @v);
  SetVector (v, YVector, 0.5);
  gl.TexGenfv (GL_T, GL_OBJECT_PLANE, @v);
  SetVector (v, ZVector, 0.5);
  gl.TexGenfv (GL_R, GL_OBJECT_PLANE, @v);
  gl.Enable (GL_TEXTURE_GEN_S);
  gl.Enable (GL_TEXTURE_GEN_T);
  gl.Enable (GL_TEXTURE_GEN_R);
  (*
    glClipPlane(GL_CLIP_PLANE0, @clip0);
    glClipPlane(GL_CLIP_PLANE1, @clip1);
    glClipPlane(GL_CLIP_PLANE2, @clip2);
    glClipPlane(GL_CLIP_PLANE3, @clip3);
    glClipPlane(GL_CLIP_PLANE4, @clip4);
    glClipPlane(GL_CLIP_PLANE5, @clip5);

    glEnable(GL_CLIP_PLANE0);
    glEnable(GL_CLIP_PLANE1);
    glEnable(GL_CLIP_PLANE2);
    glEnable(GL_CLIP_PLANE3);
    glEnable(GL_CLIP_PLANE4);
    glEnable(GL_CLIP_PLANE5);
  *)
  gl.BindTexture (GL_TEXTURE_3D, M_3D_Texture.Handle);
  gl.Enable (GL_TEXTURE_3D);

  gl.GetFloatv (GL_MODELVIEW_MATRIX, @mat);
  vx.X := mat.X.X;
  vy.X := mat.X.Y;
  vz.X := mat.X.Z;
  vx.Y := mat.Y.X;
  vy.Y := mat.Y.Y;
  vz.Y := mat.Y.Z;
  vx.Z := mat.Z.X;
  vy.Z := mat.Z.Y;
  vz.Z := mat.Z.Z;
  ScaleVector (vx, DIAGONAL_LENGTH * 0.5 / VectorLength (vx));
  ScaleVector (vy, DIAGONAL_LENGTH * 0.5 / VectorLength (vy));
  ScaleVector (vz, DIAGONAL_LENGTH * 0.5 / VectorLength (vz));

  step := DIAGONAL_LENGTH / Projection_N_TB.Position;
  z := -DIAGONAL_LENGTH / 2;
  gl.begin_ (GL_QUADS);
  For i := 0 To Projection_N_TB.Position - 1 Do
    begin
      gl.Color4f (1.0, 1.0, 1.0, 1.0);

      gl.Normal3f (-GLCamera.AbsoluteVectorToTarget.X, -GLCamera.AbsoluteVectorToTarget.Y, -GLCamera.AbsoluteVectorToTarget.Z);

      gl.Vertex3f (vx.X + vy.X + vz.X * z, vx.Y + vy.Y + vz.Y * z, vx.Z + vy.Z + vz.Z * z);
      gl.Vertex3f (-vx.X + vy.X + vz.X * z, -vx.Y + vy.Y + vz.Y * z, -vx.Z + vy.Z + vz.Z * z);
      gl.Vertex3f (-vx.X - vy.X + vz.X * z, -vx.Y - vy.Y + vz.Y * z, -vx.Z - vy.Z + vz.Z * z);
      gl.Vertex3f (vx.X - vy.X + vz.X * z, vx.Y - vy.Y + vz.Y * z, vx.Z - vy.Z + vz.Z * z);
      z := z + step;
    end;
  gl.End_;

  gl.PopMatrix;
  gl.PopAttrib;
end;

procedure TForm1.Cutting_Plane_Pos_TBChange (Sender : TObject);
begin
  Cutting_Plane_Pos_L.Caption := IntToStr (Cutting_Plane_Pos_TB.Position);
  M_Refresh := TRUE;
end;

procedure TForm1.Alpha_Threshold_TBChange(Sender: TObject);
begin
  Alpha_Threshold_L.Caption := IntToStr (Alpha_Threshold_TB.Position);
  M_Refresh := TRUE;
end;

procedure TForm1.Projection_N_TBChange (Sender : TObject);
begin
  Projection_N_L.Caption := IntToStr (Projection_N_TB.Position);
end;

procedure TForm1.Box_CBClick (Sender : TObject);
begin
  Frame.Visible := Box_CB.Checked;
end;

procedure TForm1.Pseudocolor_CBClick (Sender : TObject);
begin
  M_Refresh := TRUE;
end;

procedure TForm1.Opaque_Hull_CBClick (Sender : TObject);
begin
  M_Refresh := TRUE;
end;

end.

