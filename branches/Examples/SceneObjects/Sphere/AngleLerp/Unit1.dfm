object Form1: TForm1
  Left = 193
  Top = 108
  Caption = 'AngleLerp'
  ClientHeight = 468
  ClientWidth = 632
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
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 632
    Height = 468
    Camera = GLCamera1
    FieldOfView = 155.877380371093800000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object MainSphere: TGLSphere
      Material.FrontProperties.Ambient.Color = {9A93133FE4DB5B3FEBE0E03E0000803F}
      Radius = 1.000000000000000000
      object Sphere1: TGLSphere
        Material.FrontProperties.Ambient.Color = {CDCC4C3FF8FEFE3EACC8483E0000803F}
        Position.Coordinates = {3ACD133F3ACD133F3ACD133F0000803F}
        Radius = 0.100000001490116100
      end
      object Sphere2: TGLSphere
        Material.FrontProperties.Ambient.Color = {0000803F000000000000803F0000803F}
        Position.Coordinates = {0000803F00000000000000000000803F}
        Radius = 0.100000001490116100
      end
      object Sphere: TGLSphere
        Material.FrontProperties.Ambient.Color = {0000803F000000000000803F0000803F}
        Radius = 0.100000001490116100
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = MainSphere
      Position.Coordinates = {0000404000000040000000000000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 8
  end
end
