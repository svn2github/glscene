object Form1: TForm1
  Left = 218
  Top = 126
  Caption = 'Form1'
  ClientHeight = 606
  ClientWidth = 992
  Color = clGreen
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 744
    Top = 48
    Width = 49
    Height = 13
    Caption = #1048#1085#1090#1077#1088#1074#1072#1083
  end
  object Label2: TLabel
    Left = 744
    Top = 136
    Width = 114
    Height = 13
    Caption = #1042#1088#1072#1097#1077#1085#1080#1077' '#1087#1077#1088#1089#1086#1085#1072#1078#1072':'
  end
  object Label3: TLabel
    Left = 744
    Top = 208
    Width = 152
    Height = 13
    Caption = #1053#1072#1082#1083#1086#1085' '#1090#1091#1083#1086#1074#1080#1097#1072' '#1087#1077#1088#1089#1086#1085#1072#1078#1072':'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 729
    Height = 601
    Camera = GLCamera1
    FieldOfView = 161.106277465820300000
    TabOrder = 0
  end
  object TrackBar1: TTrackBar
    Left = 736
    Top = 0
    Width = 249
    Height = 45
    Max = 100
    Position = 50
    TabOrder = 1
    OnChange = TrackBar1Change
  end
  object ComboBox1: TComboBox
    Left = 744
    Top = 96
    Width = 145
    Height = 21
    TabOrder = 2
    OnChange = ComboBox1Change
  end
  object TrackBar2: TTrackBar
    Left = 736
    Top = 152
    Width = 249
    Height = 45
    Max = 360
    Min = 1
    Position = 1
    TabOrder = 3
    OnChange = TrackBar2Change
  end
  object TrackBar3: TTrackBar
    Left = 736
    Top = 224
    Width = 249
    Height = 33
    Max = 360
    Position = 180
    TabOrder = 4
    OnChange = TrackBar3Change
  end
  object Memo1: TMemo
    Left = 744
    Top = 264
    Width = 233
    Height = 49
    Lines.Strings = (
      #1053#1072#1082#1083#1086#1085' '#1090#1091#1083#1086#1074#1080#1097#1072' '#1076#1077#1081#1089#1090#1074#1091#1077#1090' '#1090#1086#1083#1100#1082#1086' '#1087#1088#1080' '
      #1072#1085#1080#1084#1072#1094#1080#1103#1093' hero_a_atk_pistol '#1080' '
      'hero_a_idle_pistol')
    ReadOnly = True
    TabOrder = 5
  end
  object bigbtn: TButton
    Left = 735
    Top = 376
    Width = 146
    Height = 105
    Caption = #1041#1086#1083#1100#1096#1072#1103' '#1082#1085#1086#1087#1082#1072'!!!'
    TabOrder = 6
    OnClick = bigbtnClick
  end
  object GLScene1: TGLScene
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000000000000F041000000000000803F}
      SpotCutOff = 180.000000000000000000
    end
    object hero: TGLActor
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Reference = aarSkeleton
      AnimationMode = aamLoop
      Interval = 100
      OnFrameChanged = heroFrameChanged
      object inhander: TGLFreeForm
        object GLLines1: TGLLines
          LineColor.Color = {0000803F00000000000000000000803F}
          NodeColor.Color = {00000000000000000000000000000000}
          Nodes = <
            item
              X = 1.000000000000000000
              Z = 0.500000000000000000
            end
            item
              X = 10.000000000000000000
              Z = 0.500000000000000000
            end>
          Options = []
        end
      end
    end
    object dummy: TGLCube
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {00000000000020410000A0C10000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 32
  end
  object fpser: TTimer
    Interval = 500
    OnTimer = fpserTimer
    Left = 64
  end
end
