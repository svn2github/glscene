object Form1: TForm1
  Left = 192
  Top = 107
  Width = 551
  Height = 402
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
    Left = 8
    Top = 288
    Width = 47
    Height = 13
    Caption = 'MaxSteps'
  end
  object Label2: TLabel
    Left = 8
    Top = 336
    Width = 22
    Height = 13
    Caption = 'Step'
  end
  object Label3: TLabel
    Left = 424
    Top = 288
    Width = 31
    Height = 13
    Caption = 'Speed'
  end
  object viewer: TGLSceneViewer
    Left = 8
    Top = 8
    Width = 529
    Height = 273
    Camera = Camera1
    OnMouseDown = viewerMouseDown
    OnMouseMove = viewerMouseMove
  end
  object MaxStepsBar: TTrackBar
    Left = 0
    Top = 304
    Width = 409
    Height = 25
    Max = 200
    Orientation = trHorizontal
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 1
    ThumbLength = 15
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = MaxStepsBarChange
  end
  object StepBar: TProgressBar
    Left = 8
    Top = 352
    Width = 393
    Height = 16
    Min = 0
    Max = 200
    Smooth = True
    TabOrder = 2
  end
  object SpeedBar: TTrackBar
    Left = 416
    Top = 304
    Width = 121
    Height = 25
    Max = 20
    Orientation = trHorizontal
    Frequency = 1
    Position = 1
    SelEnd = 0
    SelStart = 0
    TabOrder = 3
    ThumbLength = 15
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = SpeedBarChange
  end
  object Button1: TButton
    Left = 408
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 4
    OnClick = Button1Click
  end
  object CheckOn: TCheckBox
    Left = 488
    Top = 344
    Width = 41
    Height = 17
    Caption = 'On'
    TabOrder = 5
    OnClick = CheckOnClick
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
