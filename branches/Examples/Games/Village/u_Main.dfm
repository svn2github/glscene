object Form1: TForm1
  Left = 387
  Top = 472
  ClientHeight = 499
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 800
    Height = 499
    Camera = cam
    Buffer.AmbientColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roNoColorBufferClear]
    FieldOfView = 146.538314819335900000
    Align = alClient
    OnMouseDown = vpMouseDown
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object sky: TGLSphere
      Material.MaterialLibrary = matlib
      Material.LibMaterialName = 'sky'
      Position.Coordinates = {0000000000008040000000000000803F}
      NormalDirection = ndInside
      Radius = 500.000000000000000000
    end
    object dc_cam: TGLDummyCube
      Position.Coordinates = {0000000000008040000000000000803F}
      CubeSize = 1.000000000000000000
      object cam: TGLCamera
        DepthOfView = 500.000000000000000000
        FocalLength = 75.000000000000000000
        NearPlaneBias = 0.100000001490116100
        Up.Coordinates = {000000800000803F0000000000000000}
      end
    end
    object ff: TGLFreeForm
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
    end
    object ff_boat: TGLFreeForm
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {00000000CDCC4C3E000000000000803F}
      Up.Coordinates = {0000000000000000000080BF00000000}
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    Enabled = False
    Mode = cmApplicationIdle
    OnProgress = cadProgress
    Left = 40
    Top = 8
  end
  object matlib: TGLMaterialLibrary
    TexturePaths = 'textures'
    Left = 8
    Top = 40
  end
  object at: TGLAsyncTimer
    Enabled = True
    Interval = 800
    OnTimer = atTimer
    Left = 40
    Top = 40
  end
end
