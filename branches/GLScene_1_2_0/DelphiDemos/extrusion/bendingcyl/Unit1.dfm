object Form1: TForm1
  Left = 270
  Top = 106
  BorderWidth = 3
  Caption = 'Form1'
  ClientHeight = 275
  ClientWidth = 342
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
    Width = 342
    Height = 275
    Camera = GLCamera1
    Buffer.FaceCulling = False
    FieldOfView = 140.033782958984400000
    Align = alClient
    TabOrder = 0
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
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000A0400000A0400000A0400000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
      GlyphVisibleAtRunTime = True
    end
    object Pipe1: TGLPipe
      Position.Coordinates = {00000000000080BF000000000000803F}
      Nodes = <
        item
        end
        item
          Y = 1.000000000000000000
        end
        item
          X = -1.000000000000000000
          Y = 2.000000000000000000
        end>
      Parts = [ppOutside, ppStartDisk, ppStopDisk]
      Radius = 0.200000002980232200
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000803F00004040000080400000803F}
      Left = 160
      Top = 120
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 56
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Form1 - %FPS'
    Options = [snoMouseWheelHandled]
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 8
    Top = 104
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 152
  end
end
