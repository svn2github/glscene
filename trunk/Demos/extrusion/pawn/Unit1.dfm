object Form1: TForm1
  Left = 166
  Top = 89
  Width = 391
  Height = 308
  AutoSize = True
  BorderWidth = 3
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
  object Label1: TLabel
    Left = 296
    Top = 8
    Width = 59
    Height = 18
    Caption = 'Options'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 272
    Top = 240
    Width = 22
    Height = 13
    Caption = 'Stop'
  end
  object Label3: TLabel
    Left = 272
    Top = 152
    Width = 28
    Height = 13
    Caption = 'Slices'
  end
  object Label4: TLabel
    Left = 272
    Top = 192
    Width = 42
    Height = 13
    Caption = 'Divisions'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 257
    Height = 273
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object CheckBox1: TCheckBox
    Left = 264
    Top = 48
    Width = 113
    Height = 17
    Caption = 'Spline interpolation'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 264
    Top = 72
    Width = 113
    Height = 17
    Caption = 'Normals smoothing'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 264
    Top = 96
    Width = 113
    Height = 17
    Caption = 'Texture map'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox3Click
  end
  object TrackBar1: TTrackBar
    Left = 264
    Top = 256
    Width = 113
    Height = 17
    Max = 360
    Min = 30
    Orientation = trHorizontal
    Frequency = 45
    Position = 360
    SelEnd = 0
    SelStart = 0
    TabOrder = 4
    ThumbLength = 10
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object CheckBox4: TCheckBox
    Left = 264
    Top = 120
    Width = 113
    Height = 17
    Caption = 'Modulate texture'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox4Click
  end
  object TrackBar2: TTrackBar
    Left = 264
    Top = 168
    Width = 113
    Height = 17
    Max = 64
    Min = 4
    Orientation = trHorizontal
    Frequency = 16
    Position = 24
    SelEnd = 0
    SelStart = 0
    TabOrder = 6
    ThumbLength = 10
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar2Change
  end
  object TrackBar3: TTrackBar
    Left = 264
    Top = 208
    Width = 113
    Height = 17
    Max = 30
    Min = 1
    Orientation = trHorizontal
    Frequency = 10
    Position = 10
    SelEnd = 0
    SelStart = 0
    TabOrder = 7
    ThumbLength = 10
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar3Change
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1
      object RotationSolid1: TGLRevolutionSolid
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Nodes = <
          item
            Y = 0.899999976158142
          end
          item
            X = 0.400000005960464
            Y = 0.800000011920929
          end
          item
            X = 0.449999988079071
            Y = 0.5
          end
          item
            X = 0.25
            Y = 0.300000011920929
          end
          item
            X = 0.25
            Y = -0.100000001490116
          end
          item
            X = 0.600000023841858
            Y = -0.5
          end
          item
            X = 0.600000023841858
            Y = -0.899999976158142
          end
          item
            X = 0.589999973773956
            Y = -0.949999988079071
          end
          item
            X = 0.600000023841858
            Y = -1
          end
          item
            X = 0.600000023841858
            Y = -1
          end
          item
            X = 0.5
            Y = -1
          end
          item
            Y = -1
          end>
        SplineMode = lsmCubicSpline
        Slices = 24
        Normals = nsSmooth
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {0000804000000000000000000000803F}
      Left = 208
      Top = 136
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 40
  end
end
