object Form1: TForm1
  Left = 194
  Top = 128
  ClientHeight = 375
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 484
    Height = 375
    Cursor = -1
    Camera = cam
    Buffer.BackgroundColor = 2763306
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    FieldOfView = 136.397186279296900000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = vpMouseDown
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object dc_cam: TGLDummyCube
      Position.Coordinates = {0000000000000040000000000000803F}
      CubeSize = 1.000000000000000000
      object cam: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 75.000000000000000000
        TargetObject = dc_cam
        Position.Coordinates = {000080C000008040000020410000803F}
      end
    end
    object dc_world: TGLDummyCube
      CubeSize = 1.000000000000000000
      object ff1: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
      end
      object ff2: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Visible = False
      end
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    Mode = cmApplicationIdle
    SleepLength = 1
    OnProgress = cadProgress
    Left = 40
    Top = 8
  end
  object matLib: TGLMaterialLibrary
    Left = 8
    Top = 40
  end
end
