object Form1: TForm1
  Left = 187
  Top = 153
  Width = 469
  Height = 358
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 461
    Height = 329
    Camera = GLCamera1
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000484200002042000070420000803F}
      SpotCutOff = 180
    end
    object ExtrusionSolid: TGLExtrusionSolid
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Contours = <>
      Parts = [espOutside, espStartPolygon, espStopPolygon]
      Height = 1
      MinSmoothAngle = 5
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 75
      TargetObject = ExtrusionSolid
      Position.Coordinates = {0000C04000000041000020410000803F}
      Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Left = 216
      Top = 152
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 48
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 88
  end
end
