object Form1: TForm1
  Left = 197
  Top = 118
  ClientHeight = 561
  ClientWidth = 854
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 854
    Height = 561
    Camera = cam
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    FieldOfView = 159.786010742187500000
    Align = alClient
    OnMouseDown = vpMouseDown
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object cam: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = dc_model
      Position.Coordinates = {00000000000000410000C8410000803F}
      object light: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {000000000000A040000000000000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object dc_world: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object dc_model: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      Visible = False
      CubeSize = 1.000000000000000000
      object ff_G: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
      end
      object ff_L: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
      end
      object ff_S: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
      end
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    OnProgress = cadProgress
    Left = 40
    Top = 8
  end
  object at: TGLAsyncTimer
    Enabled = True
    Interval = 800
    OnTimer = atTimer
    Left = 8
    Top = 40
  end
end
