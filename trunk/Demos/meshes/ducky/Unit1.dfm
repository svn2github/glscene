object Form1: TForm1
  Left = 84
  Top = 117
  Width = 666
  Height = 435
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 161
    Height = 406
    Align = alLeft
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 32
      Width = 50
      Height = 13
      Caption = 'Resolution'
    end
    object TrackBar1: TTrackBar
      Left = 16
      Top = 48
      Width = 129
      Height = 25
      Max = 50
      Min = 1
      Orientation = trHorizontal
      Frequency = 1
      Position = 20
      SelEnd = 0
      SelStart = 0
      TabOrder = 0
      ThumbLength = 10
      TickMarks = tmBottomRight
      TickStyle = tsManual
      OnChange = TrackBar1Change
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 161
    Top = 0
    Width = 497
    Height = 406
    Camera = GLCamera1
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1
      object GLCamera1: TGLCamera
        DepthOfView = 100
        FocalLength = 50
        TargetObject = GLDummyCube1
        Position.Coordinates = {000040C000004040000040400000803F}
        Direction.Coordinates = {00000000000080BF0000008000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1
          Specular.Color = {0000803F0000803F0000803F0000803F}
          SpotCutOff = 180
        end
      end
    end
    object GLFreeForm1: TGLFreeForm
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
      Material.FrontProperties.Shininess = 50
      Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
    end
  end
end
