object Form1: TForm1
  Left = 174
  Top = 93
  Width = 582
  Height = 462
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
  object viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 574
    Height = 336
    Camera = Camera1
    Align = alClient
    OnMouseDown = viewerMouseDown
    OnMouseMove = viewerMouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 336
    Width = 574
    Height = 97
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 56
      Width = 22
      Height = 13
      Caption = 'Step'
    end
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 47
      Height = 13
      Caption = 'MaxSteps'
    end
    object Label3: TLabel
      Left = 424
      Top = 8
      Width = 31
      Height = 13
      Caption = 'Speed'
    end
    object CheckOn: TCheckBox
      Left = 488
      Top = 64
      Width = 41
      Height = 17
      Caption = 'On'
      TabOrder = 0
      OnClick = CheckOnClick
    end
    object Button1: TButton
      Left = 408
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Reset'
      TabOrder = 1
      OnClick = Button1Click
    end
    object StepBar: TProgressBar
      Left = 8
      Top = 72
      Width = 393
      Height = 16
      Min = 0
      Max = 200
      Smooth = True
      TabOrder = 2
    end
    object MaxStepsBar: TTrackBar
      Left = 0
      Top = 24
      Width = 409
      Height = 25
      Max = 200
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 3
      ThumbLength = 15
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = MaxStepsBarChange
    end
    object SpeedBar: TTrackBar
      Left = 416
      Top = 24
      Width = 121
      Height = 25
      Max = 20
      Orientation = trHorizontal
      Frequency = 1
      Position = 1
      SelEnd = 0
      SelStart = 0
      TabOrder = 4
      ThumbLength = 15
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = SpeedBarChange
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object mesh: TGLFreeForm
      Scale.Coordinates = {CDCCCC3ECDCCCC3ECDCCCC3E00000000}
      NormalsOrientation = mnoInvert
      EffectsData = {0201060F54474C424578706C6F73696F6E46580200}
    end
    object Camera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = mesh
      Position.Coordinates = {0000000000004842000000000000803F}
      Direction.Coordinates = {00000000000080BF0000000000000000}
      Up.Coordinates = {E8DC723F000000009BE8A1BE00000000}
      Left = 328
      Top = 216
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1
        SpotCutOff = 180
        SpotDirection.Coordinates = {00000000000000000000803F00000000}
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 48
  end
end
