object Form1: TForm1
  Left = 173
  Top = 96
  Width = 511
  Height = 357
  BorderWidth = 2
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 382
    Top = 0
    Width = 2
    Height = 324
    Align = alRight
    AutoSize = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 382
    Height = 324
    Camera = GLCamera1
    Buffer.BackgroundColor = 13619151
    Buffer.FaceCulling = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 384
    Top = 0
    Width = 115
    Height = 324
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 1
    object LASubdivideTime: TLabel
      Left = 8
      Top = 168
      Width = 97
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'LASubdivideTime'
    end
    object BULoad: TButton
      Left = 16
      Top = 8
      Width = 83
      Height = 25
      Caption = 'Load'
      TabOrder = 0
      OnClick = BULoadClick
    end
    object BUSubdivide: TButton
      Left = 8
      Top = 112
      Width = 99
      Height = 25
      Caption = 'Subdivide'
      Enabled = False
      TabOrder = 1
      OnClick = BUSubdivideClick
    end
    object TrackBar1: TTrackBar
      Left = 8
      Top = 144
      Width = 97
      Height = 17
      Hint = 'Subdivision smoothness'
      Orientation = trHorizontal
      Frequency = 1
      Position = 5
      SelEnd = 0
      SelStart = 0
      TabOrder = 2
      ThumbLength = 10
      TickMarks = tmBottomRight
      TickStyle = tsAuto
    end
    object RBWireFrame: TRadioButton
      Left = 16
      Top = 40
      Width = 81
      Height = 17
      Caption = 'Wireframe'
      TabOrder = 3
      OnClick = RBWireFrameClick
    end
    object RBSolid: TRadioButton
      Left = 16
      Top = 56
      Width = 89
      Height = 17
      Caption = 'Solid'
      Checked = True
      TabOrder = 4
      TabStop = True
      OnClick = RBSolidClick
    end
    object CBAnimate: TCheckBox
      Left = 16
      Top = 80
      Width = 81
      Height = 17
      Caption = 'Animate'
      TabOrder = 5
      OnClick = CBAnimateClick
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLActor1: TGLActor
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Material.Texture.TextureMode = tmReplace
      Interval = 100
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = GLActor1
      Position.Coordinates = {0000A04000008040000040400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 48
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 80
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 56
  end
end
