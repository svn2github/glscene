object Form1: TForm1
  Left = 264
  Top = 108
  Width = 523
  Height = 445
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
    Width = 408
    Height = 375
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    Buffer.Lighting = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 408
    Top = 0
    Width = 107
    Height = 375
    Align = alRight
    BevelOuter = bvNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 107
      Height = 48
      Align = alTop
      Alignment = taCenter
      Caption = #13#10'Basic Skeletal'#13#10'Animation'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object BULongJump: TButton
      Left = 16
      Top = 152
      Width = 81
      Height = 25
      Caption = 'Long Jump'
      TabOrder = 0
      OnClick = BULongJumpClick
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 240
      Width = 81
      Height = 17
      Caption = 'Skeleton'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object BUHighJump: TButton
      Left = 16
      Top = 192
      Width = 81
      Height = 25
      Caption = 'High Jump'
      TabOrder = 2
      OnClick = BUHighJumpClick
    end
    object RBWalk: TRadioButton
      Left = 16
      Top = 88
      Width = 65
      Height = 17
      Caption = 'Walk'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = RBWalkClick
    end
    object RBRun: TRadioButton
      Left = 16
      Top = 112
      Width = 73
      Height = 17
      Caption = 'Run'
      TabOrder = 4
      OnClick = RBRunClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 375
    Width = 515
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object TrackBar1: TTrackBar
      Left = 64
      Top = 0
      Width = 451
      Height = 34
      Anchors = [akLeft, akTop, akRight, akBottom]
      Max = 100
      Orientation = trHorizontal
      Frequency = 1
      Position = 50
      SelEnd = 0
      SelStart = 0
      TabOrder = 0
      TickMarks = tmBoth
      TickStyle = tsAuto
      OnChange = TrackBar1Change
    end
    object CBBlend: TCheckBox
      Left = 8
      Top = 12
      Width = 49
      Height = 17
      Caption = 'Blend'
      TabOrder = 1
      OnClick = CBBlendClick
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 8
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1
      Position.Coordinates = {0000C8420000C8420000C8420000803F}
      SpotCutOff = 180
    end
    object Actor1: TGLActor
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {0000003F0000003F0000003F00000000}
      Up.Coordinates = {0000000000000080000080BF00000000}
      StartFrame = 0
      EndFrame = 0
      CurrentFrame = 0
      AnimationMode = aamLoop
      Interval = 100
      OnEndFrameReached = Actor1EndFrameReached
      Animations = <>
      MaterialLibrary = GLMaterialLibrary1
      OverlaySkeleton = False
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1
    end
    object XYZGrid1: TGLXYZGrid
      Position.Coordinates = {00000000000090C1000000000000803F}
      LineColor.Color = {1283803E1283003F1283003F0000803F}
      XSamplingScale.Min = -12
      XSamplingScale.Max = 12
      XSamplingScale.Step = 4
      YSamplingScale.Step = 1
      ZSamplingScale.Min = -20
      ZSamplingScale.Max = 20
      ZSamplingScale.Step = 4
      Parts = [gpX, gpZ]
      LinesSmoothing = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 500
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {000048420000A0410000A0400000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {000000000000803F0000008000000000}
      Left = 224
      Top = 160
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <>
    Left = 16
    Top = 40
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 56
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 40
  end
  object AnimationControler1: TAnimationControler
    Left = 40
    Top = 80
  end
end
