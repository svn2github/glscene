object Form1: TForm1
  Left = 267
  Top = 108
  Width = 356
  Height = 308
  BorderWidth = 3
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 342
    Height = 273
    Camera = GLCamera1
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object CBSpline: TCheckBox
    Left = 272
    Top = 8
    Width = 57
    Height = 17
    Caption = 'Splines'
    TabOrder = 1
    OnClick = CBSplineClick
  end
  object CBFat: TCheckBox
    Left = 272
    Top = 32
    Width = 57
    Height = 17
    Caption = 'Fat/Slim'
    TabOrder = 2
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180
    end
    object Pipe1: TGLPipe
      Position.Coordinates = {00000000000080BF000000000000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Nodes = <
        item
        end
        item
          Y = 1
        end
        item
          X = -1
          Y = 2
        end>
      Parts = [ppOutside, ppStartDisk, ppStopDisk]
      Radius = 0.200000002980232
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {0000803F00004040000080400000803F}
      Left = 160
      Top = 120
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 40
  end
end
