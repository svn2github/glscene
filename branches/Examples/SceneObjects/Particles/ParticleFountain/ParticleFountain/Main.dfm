object Form1: TForm1
  Left = 216
  Top = 3
  Caption = 'Fountain Demo'
  ClientHeight = 659
  ClientWidth = 752
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 544
    Height = 640
    Camera = Cam
    VSync = vsmSync
    Buffer.FogEnvironment.FogStart = 25.000000000000000000
    Buffer.FogEnvironment.FogEnd = 25.000000000000000000
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 143.383056640625000000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 544
    Top = 0
    Width = 208
    Height = 640
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 0
      Top = 523
      Width = 208
      Height = 117
      Align = alBottom
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 10
        Width = 61
        Height = 13
        Caption = 'Color Start'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 8
        Top = 42
        Width = 56
        Height = 13
        Caption = 'Color End'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label3: TLabel
        Left = 8
        Top = 74
        Width = 71
        Height = 13
        Caption = 'BackGround'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object PStartColor: TPanel
        Left = 8
        Top = 24
        Width = 129
        Height = 17
        Color = clRed
        TabOrder = 0
        OnClick = PStartColorClick
      end
      object PEndColor: TPanel
        Left = 8
        Top = 56
        Width = 129
        Height = 17
        Color = clYellow
        TabOrder = 1
        OnClick = PEndColorClick
      end
      object PBackColor: TPanel
        Left = 8
        Top = 88
        Width = 129
        Height = 17
        Color = clBlack
        TabOrder = 2
        OnClick = PBackColorClick
      end
    end
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 208
      Height = 523
      ActivePage = TabSheet1
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object TabSheet1: TTabSheet
        Caption = 'Fontaine Setting'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label14: TLabel
          Left = 8
          Top = 472
          Width = 105
          Height = 13
          Caption = 'Particles Size Max'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label13: TLabel
          Left = 8
          Top = 432
          Width = 102
          Height = 13
          Caption = 'Particles Size Min'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label12: TLabel
          Left = 8
          Top = 392
          Width = 74
          Height = 13
          Caption = 'Times Factor'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label11: TLabel
          Left = 8
          Top = 352
          Width = 62
          Height = 13
          Caption = 'Life Factor'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label10: TLabel
          Left = 8
          Top = 312
          Width = 64
          Height = 13
          Caption = 'Angle Start'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label9: TLabel
          Left = 8
          Top = 272
          Width = 73
          Height = 13
          Caption = 'Velocity Max'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label8: TLabel
          Left = 8
          Top = 232
          Width = 70
          Height = 13
          Caption = 'Velocity Min'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label7: TLabel
          Left = 8
          Top = 192
          Width = 77
          Height = 13
          Caption = 'Max Particles'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label6: TLabel
          Left = 8
          Top = 152
          Width = 29
          Height = 13
          Caption = 'Floor'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label5: TLabel
          Left = 8
          Top = 112
          Width = 94
          Height = 13
          Caption = 'Bounding Factor'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label4: TLabel
          Left = 8
          Top = 72
          Width = 77
          Height = 13
          Caption = 'Particle Mass'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label15: TLabel
          Left = 8
          Top = 32
          Width = 33
          Height = 13
          Caption = 'Scale'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object EdPSizeMax: TEdit
          Left = 8
          Top = 488
          Width = 129
          Height = 21
          TabOrder = 0
          Text = '130'
          OnChange = EdPSizeMaxChange
        end
        object EdPSizeMin: TEdit
          Left = 8
          Top = 448
          Width = 129
          Height = 21
          TabOrder = 1
          Text = '110'
          OnChange = EdPSizeMinChange
        end
        object EdTimesFact: TEdit
          Left = 8
          Top = 408
          Width = 129
          Height = 21
          TabOrder = 2
          Text = '0.00005'
          OnChange = EdTimesFactChange
        end
        object EdLifeFact: TEdit
          Left = 8
          Top = 368
          Width = 129
          Height = 21
          TabOrder = 3
          Text = '0.025'
          OnChange = EdLifeFactChange
        end
        object EdAngleStart: TEdit
          Left = 8
          Top = 328
          Width = 129
          Height = 21
          TabOrder = 4
          Text = '360'
          OnChange = EdAngleStartChange
        end
        object EdVelMax: TEdit
          Left = 8
          Top = 288
          Width = 129
          Height = 21
          TabOrder = 5
          Text = '15'
          OnChange = EdVelMaxChange
        end
        object EdVelMin: TEdit
          Left = 8
          Top = 248
          Width = 129
          Height = 21
          TabOrder = 6
          Text = '5'
          OnChange = EdVelMinChange
        end
        object EdMaxP: TEdit
          Left = 8
          Top = 208
          Width = 129
          Height = 21
          TabOrder = 7
          Text = '60'
          OnChange = EdMaxPChange
        end
        object EdFloor: TEdit
          Left = 8
          Top = 168
          Width = 129
          Height = 21
          TabOrder = 8
          Text = '0.0'
          OnChange = EdFloorChange
        end
        object EdBound: TEdit
          Left = 8
          Top = 128
          Width = 129
          Height = 21
          TabOrder = 9
          Text = '100.0'
          OnChange = EdBoundChange
        end
        object EdMass: TEdit
          Left = 8
          Top = 88
          Width = 129
          Height = 21
          TabOrder = 10
          Text = '5.0'
          OnChange = EdMassChange
        end
        object CheckActived: TCheckBox
          Left = 0
          Top = 8
          Width = 65
          Height = 17
          Caption = 'Actived'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          State = cbChecked
          TabOrder = 11
          OnClick = CheckActivedClick
        end
        object CheckBound: TCheckBox
          Left = 72
          Top = 8
          Width = 73
          Height = 17
          Caption = 'Bounding'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 12
          OnClick = CheckBoundClick
        end
        object TrackBar1: TTrackBar
          Left = 0
          Top = 48
          Width = 150
          Height = 15
          Min = 1
          PageSize = 1
          Position = 2
          TabOrder = 13
          ThumbLength = 12
          OnChange = TrackBar1Change
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Fontaine Style'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 200
          Height = 495
          Align = alClient
          TabOrder = 0
          object RadioButton1: TRadioButton
            Left = 8
            Top = 16
            Width = 73
            Height = 17
            Caption = 'Style Fire'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = RadioButtonClick
          end
          object RadioButton2: TRadioButton
            Left = 8
            Top = 40
            Width = 81
            Height = 17
            Caption = 'Style Water'
            TabOrder = 1
            OnClick = RadioButtonClick
          end
          object RadioButton3: TRadioButton
            Left = 8
            Top = 64
            Width = 73
            Height = 17
            Caption = 'Style Ball'
            TabOrder = 2
            OnClick = RadioButtonClick
          end
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 640
    Width = 752
    Height = 19
    Panels = <>
  end
  object GLScene1: TGLScene
    Top = 112
    object Scene: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLTorus1: TGLTorus
        Material.FrontProperties.Diffuse.Color = {0000000000000000ABAAAA3E0000803F}
        MajorRadius = 1.500000000000000000
        MinorRadius = 0.250000000000000000
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object GLPlane1: TGLPlane
        Material.FrontProperties.Diffuse.Color = {0000000000000000F1F0703E0000803F}
        Position.Coordinates = {0000000000000000000080C00000803F}
        Height = 20.000000000000000000
        Width = 20.000000000000000000
      end
    end
    object Light: TGLLightSource
      Ambient.Color = {BEC0403FBEC0403FBEC0403F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000000000000000000020410000803F}
      LightStyle = lsOmni
      Specular.Color = {BEC0403FBEC0403FBEC0403F0000803F}
      SpotCutOff = 180.000000000000000000
      SpotDirection.Coordinates = {000000000000803F0000000000000000}
    end
    object Cam: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 90.000000000000000000
      TargetObject = Scene
      Position.Coordinates = {0000000000007041000040400000803F}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    FixedDeltaTime = 0.016500000000000000
    SleepLength = 0
    OnProgress = GLCadencer1Progress
    Top = 144
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Top = 176
  end
  object ColorDialog1: TColorDialog
    Left = 504
    Top = 416
  end
  object MainMenu1: TMainMenu
    Left = 40
    Top = 112
    object File1: TMenuItem
      Caption = 'File'
      object Texture1: TMenuItem
        Caption = 'Texture...'
        OnClick = Texture1Click
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 40
    Top = 144
  end
end
