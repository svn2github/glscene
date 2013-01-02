object Form1: TForm1
  Left = 196
  Top = 94
  Width = 441
  Height = 447
  AutoSize = True
  BorderWidth = 4
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
    Left = 0
    Top = 312
    Width = 44
    Height = 13
    Caption = 'X extents'
  end
  object Label2: TLabel
    Left = 0
    Top = 344
    Width = 44
    Height = 13
    Caption = 'Y extents'
  end
  object Label3: TLabel
    Left = 0
    Top = 376
    Width = 22
    Height = 13
    Caption = 'Step'
  end
  object Label4: TLabel
    Left = 336
    Top = 336
    Width = 54
    Height = 13
    Caption = 'Color Mode'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 417
    Height = 297
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 112.087295532226600000
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object TrackBar1: TTrackBar
    Left = 64
    Top = 304
    Width = 169
    Height = 33
    Max = 50
    Position = 10
    TabOrder = 1
    ThumbLength = 10
    TickMarks = tmBoth
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 64
    Top = 336
    Width = 169
    Height = 33
    Max = 50
    Position = 10
    TabOrder = 2
    ThumbLength = 10
    TickMarks = tmBoth
    OnChange = TrackBar2Change
  end
  object CheckBox1: TCheckBox
    Left = 336
    Top = 312
    Width = 73
    Height = 17
    Caption = 'Two-sided'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object TrackBar3: TTrackBar
    Left = 64
    Top = 368
    Width = 169
    Height = 33
    Max = 160
    Min = 10
    Frequency = 10
    Position = 80
    TabOrder = 4
    ThumbLength = 10
    TickMarks = tmBoth
    OnChange = TrackBar3Change
  end
  object RadioGroup1: TRadioGroup
    Left = 240
    Top = 304
    Width = 81
    Height = 89
    Caption = 'Formula'
    ItemIndex = 0
    Items.Strings = (
      'Formula 1'
      'Formula 2'
      'Dynamic')
    TabOrder = 5
    OnClick = RadioGroup1Click
  end
  object ComboBox1: TComboBox
    Left = 336
    Top = 352
    Width = 81
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
    OnChange = ComboBox1Change
    Items.Strings = (
      'none'
      'emission'
      'diffuse')
  end
  object CheckBox2: TCheckBox
    Left = 336
    Top = 376
    Width = 65
    Height = 17
    Caption = 'Lighting'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = CheckBox2Click
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object HeightField1: TGLHeightField
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      XSamplingScale.Min = -1.000000000000000000
      XSamplingScale.Max = 1.000000000000000000
      XSamplingScale.Step = 0.079999998211860660
      YSamplingScale.Min = -1.000000000000000000
      YSamplingScale.Max = 1.000000000000000000
      YSamplingScale.Step = 0.079999998211860660
      object Sphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
        Position.Coordinates = {0000803F0000803F000000000000803F}
        Visible = False
        OnProgress = Sphere1Progress
        Radius = 0.100000001490116100
        Slices = 12
        Stacks = 9
        object Lines1: TGLLines
          Nodes = <
            item
            end
            item
              Z = -1.500000000000000000
            end>
          NodesAspect = lnaInvisible
          Options = []
        end
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000F041000048420000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = HeightField1
      Position.Coordinates = {0000404000008040000000410000803F}
      Left = 208
      Top = 168
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 56
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    Left = 16
    Top = 96
  end
end
