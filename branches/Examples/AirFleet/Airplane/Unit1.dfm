object Form1: TForm1
  Left = 283
  Top = 110
  Caption = 'Form1'
  ClientHeight = 596
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 862
    Height = 596
    Camera = GLCamera1
    FieldOfView = 160.950668334960900000
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 16
    object GLActor1: TGLActor
      Material.BackProperties.Ambient.Color = {0000000000000000000000000000803F}
      AnimationMode = aamBounceForward
      Interval = 100
      MaterialLibrary = GLMaterialLibrary1
    end
    object GLFreeForm1: TGLFreeForm
      MaterialLibrary = GLMaterialLibrary1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLActor1
      Position.Coordinates = {0000A0400000C040000040400000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 88
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 48
    Top = 16
  end
end
