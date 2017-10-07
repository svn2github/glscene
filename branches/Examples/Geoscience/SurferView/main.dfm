object Form1: TForm1
  Left = 280
  Top = 266
  Caption = 'GridGLS Viewer'
  ClientHeight = 394
  ClientWidth = 676
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
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSurferView: TPanel
    Left = 0
    Top = 0
    Width = 207
    Height = 394
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 388
    object lblVerticalExaggeration: TLabel
      Left = 10
      Top = 8
      Width = 94
      Height = 13
      Caption = 'Vertical Exageration'
    end
    object lblCameraHeight: TLabel
      Left = 8
      Top = 201
      Width = 44
      Height = 13
      Caption = 'Camera z'
    end
    object Label1: TLabel
      Left = 8
      Top = 60
      Width = 44
      Height = 13
      Caption = 'Camera x'
    end
    object Label2: TLabel
      Left = 8
      Top = 132
      Width = 44
      Height = 13
      Caption = 'Camera y'
    end
    object lblColour1: TLabel
      Left = 22
      Top = 294
      Width = 50
      Height = 13
      Caption = 'Min Colour'
    end
    object lblMaxColour: TLabel
      Left = 20
      Top = 326
      Width = 53
      Height = 13
      Caption = 'Max Colour'
    end
    object Edit1: TEdit
      Left = 24
      Top = 32
      Width = 79
      Height = 21
      TabOrder = 0
      Text = '1'
      OnChange = Edit1Change
    end
    object TrackBarz: TTrackBar
      Left = 8
      Top = 230
      Width = 193
      Height = 27
      Min = 1
      Position = 1
      TabOrder = 1
      TickMarks = tmTopLeft
      TickStyle = tsNone
      OnChange = TrackBarzChange
    end
    object TrackBarx: TTrackBar
      Left = 8
      Top = 83
      Width = 193
      Height = 30
      Min = 1
      Position = 1
      TabOrder = 2
      TickMarks = tmTopLeft
      TickStyle = tsNone
      OnChange = TrackBarxChange
    end
    object TrackBary: TTrackBar
      Left = 8
      Top = 148
      Width = 193
      Height = 27
      Min = 1
      Position = 1
      TabOrder = 3
      TickMarks = tmTopLeft
      TickStyle = tsNone
      OnChange = TrackBaryChange
    end
    object pnlMin: TPanel
      Left = 88
      Top = 279
      Width = 77
      Height = 31
      BiDiMode = bdLeftToRight
      Color = clNavy
      Ctl3D = True
      ParentBiDiMode = False
      ParentBackground = False
      ParentCtl3D = False
      TabOrder = 4
      OnClick = pnlMinClick
    end
    object pnlMax: TPanel
      Left = 88
      Top = 316
      Width = 77
      Height = 31
      ParentCustomHint = False
      BiDiMode = bdLeftToRight
      Color = clWhite
      Ctl3D = True
      DoubleBuffered = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentBiDiMode = False
      ParentBackground = False
      ParentCtl3D = False
      ParentDoubleBuffered = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 5
      OnClick = pnlMaxClick
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 207
    Top = 0
    Width = 469
    Height = 394
    Camera = GLCamera
    Buffer.BackgroundColor = clHighlightText
    FieldOfView = 151.517288208007800000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 1
  end
  object MainMenu1: TMainMenu
    Left = 382
    Top = 16
    object miFile: TMenuItem
      Caption = 'File'
      object miOpenSurfer: TMenuItem
        Caption = 'Open Surfer Grid...'
        OnClick = miOpenSurferClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miClose: TMenuItem
        Caption = 'Close'
        OnClick = miCloseClick
      end
    end
    object miHelp: TMenuItem
      Caption = 'Help'
      object miAbout: TMenuItem
        Caption = 'About'
        OnClick = miAboutClick
      end
    end
  end
  object GLScene1: TGLScene
    Left = 254
    Top = 16
    object HeightField1: TGLHeightField
      Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F39B4283F}
      Material.BlendingMode = bmTransparency
      ShowAxes = True
      XSamplingScale.Step = 0.100000001490116100
      YSamplingScale.Step = 0.100000001490116100
      ColorMode = hfcmDiffuse
    end
    object DummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
        SpotDirection.Coordinates = {00000000000000000000803F00000000}
      end
      object GLCamera: TGLCamera
        DepthOfView = 100000.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = DummyCube
        Position.Coordinates = {000000000000C8420000803F0000803F}
        Direction.Coordinates = {000000000000803F0000008000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        Left = 286
        Top = 186
      end
    end
  end
  object OpenSurfer: TOpenDialog
    DefaultExt = '*.grd'
    Filter = 'Surfer Grid|*.grd|All Files|*.*'
    Title = 'Open Surfer Grid'
    Left = 536
    Top = 80
  end
  object ColorDialog: TColorDialog
    Left = 536
    Top = 16
  end
end
