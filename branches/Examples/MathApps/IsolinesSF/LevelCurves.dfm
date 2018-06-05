object LevelCurvesForm: TLevelCurvesForm
  Left = 197
  Top = 111
  Caption = 'Level curves'
  ClientHeight = 664
  ClientWidth = 952
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 16
  object StatusBar: TStatusBar
    Left = 0
    Top = 645
    Width = 952
    Height = 19
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    AutoHint = True
    Panels = <>
    SimplePanel = True
    ExplicitWidth = 928
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 952
    Height = 24
    BorderWidth = 1
    Color = clBtnFace
    Indent = 5
    ParentColor = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Wrapable = False
    ExplicitWidth = 928
    object ToolButton9: TToolButton
      Left = 5
      Top = 0
      Hint = 'New|Create a new file'
      Caption = '&New'
      ImageIndex = 6
      OnClick = FileNew1Execute
    end
    object ToolButton1: TToolButton
      Left = 28
      Top = 0
      Hint = 'Open|Open a file'
      Caption = '&Open'
      ImageIndex = 7
      OnClick = FileOpen1Execute
    end
    object ToolButton2: TToolButton
      Left = 51
      Top = 0
      Hint = 'Save|Save current file'
      Caption = '&Save'
      ImageIndex = 8
      OnClick = FileSave1Execute
    end
    object ToolButton3: TToolButton
      Left = 74
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 82
      Top = 0
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      Caption = 'Cu&t'
      ImageIndex = 0
    end
    object ToolButton5: TToolButton
      Left = 105
      Top = 0
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      Caption = '&Copy'
      ImageIndex = 1
    end
    object ToolButton6: TToolButton
      Left = 128
      Top = 0
      Hint = 'Paste|Inserts Clipboard contents'
      Caption = '&Paste'
      ImageIndex = 2
    end
  end
  object pnlGLscene2: TPanel
    Left = 0
    Top = 24
    Width = 800
    Height = 621
    Align = alClient
    Caption = 'pnlGLscene2'
    Color = clBlack
    ParentBackground = False
    TabOrder = 2
    object StaticText1: TStaticText
      Left = 936
      Top = 4
      Width = 58
      Height = 20
      BorderStyle = sbsSingle
      Caption = '??? FPS'
      TabOrder = 0
    end
    object View: TGLSceneViewer
      Left = 1
      Top = 1
      Width = 798
      Height = 619
      Camera = cam
      Buffer.BackgroundColor = clBlack
      Buffer.FaceCulling = False
      FieldOfView = 161.646209716796900000
      PenAsTouch = False
      Align = alClient
      OnMouseDown = ViewMouseDown
      OnMouseMove = ViewMouseMove
      OnMouseWheel = ViewMouseWheel
      TabOrder = 1
    end
    object RGpanning: TRadioGroup
      Left = 7
      Top = 6
      Width = 209
      Height = 35
      Color = clWhite
      Columns = 2
      ItemIndex = 1
      Items.Strings = (
        'Panning'
        'Stopped')
      ParentBackground = False
      ParentColor = False
      TabOrder = 2
      OnClick = RGpanningClick
    end
  end
  object pnlDimensoesGL: TPanel
    Left = 800
    Top = 24
    Width = 152
    Height = 621
    Align = alRight
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
    ExplicitLeft = 805
    ExplicitTop = 35
    object Label2: TLabel
      Left = 51
      Top = 270
      Width = 48
      Height = 40
      Caption = 'Plane Position'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial Narrow'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object lbNL: TLabel
      Left = 13
      Top = 116
      Width = 115
      Height = 16
      Caption = 'Number of Levels'
    end
    object RGexamples: TRadioGroup
      Left = 6
      Top = 6
      Width = 131
      Height = 75
      Caption = 'Examples Input Data'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        'Example 1'
        'Example 2'
        'Example3')
      ParentFont = False
      TabOrder = 0
      OnClick = RGexamplesClick
    end
    object SeeIsolines: TCheckBox
      Left = 12
      Top = 83
      Width = 125
      Height = 25
      Caption = 'Draw isolines'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial Narrow'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 1
      WordWrap = True
      OnClick = RGexamplesClick
    end
    object TrackBarNC: TTrackBar
      Left = 5
      Top = 136
      Width = 132
      Height = 33
      Max = 20
      Min = 1
      Position = 20
      TabOrder = 2
      OnChange = TrackBarNCChange
    end
    object rgNodeAspect: TRadioGroup
      Left = 6
      Top = 173
      Width = 131
      Height = 83
      Caption = 'Nodes Aspect'
      ItemIndex = 0
      Items.Strings = (
        'Invisible'
        'Cube'
        'Axis')
      TabOrder = 3
      OnClick = rgNodeAspectClick
    end
    object TrackBarPosition: TTrackBar
      Left = 6
      Top = 262
      Width = 43
      Height = 152
      Max = 20
      Min = 1
      Orientation = trVertical
      Position = 5
      TabOrder = 4
      OnChange = TrackBarPositionChange
    end
    object rgPlaneSelection: TRadioGroup
      Left = 45
      Top = 329
      Width = 73
      Height = 73
      Caption = 'Plane'
      Color = clYellow
      Ctl3D = True
      DoubleBuffered = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        'XY'
        'YZ'
        'ZX')
      ParentBackground = False
      ParentColor = False
      ParentCtl3D = False
      ParentDoubleBuffered = False
      ParentFont = False
      TabOrder = 5
      OnClick = rgPlaneSelectionClick
    end
    object rgSplineModes: TRadioGroup
      Left = -7
      Top = 428
      Width = 144
      Height = 133
      Caption = 'Spline Modes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'System'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        'Segments'
        'CubicSpline'
        'BezierSpline'
        'NURBSCurve'
        'Lines'
        'Loop')
      ParentFont = False
      TabOrder = 6
      OnClick = rgSplineModesClick
    end
  end
  object MainMenu: TMainMenu
    Left = 296
    Top = 48
    object File1: TMenuItem
      Caption = '&File'
      Hint = 'File related commands'
      object FileNewItem: TMenuItem
        Caption = '&New'
        Hint = 'New|Create a new file'
        ImageIndex = 6
        ShortCut = 16462
        OnClick = FileNew1Execute
      end
      object FileOpenItem: TMenuItem
        Caption = '&Open'
        Hint = 'Open|Open a file'
        ImageIndex = 7
        ShortCut = 16463
        OnClick = FileOpen1Execute
      end
      object FileSaveItem: TMenuItem
        Caption = '&Save'
        Hint = 'Save|Save current file'
        ImageIndex = 8
        ShortCut = 16467
        OnClick = FileSave1Execute
      end
      object FileSaveAsItem: TMenuItem
        Caption = 'Save &As...'
        Hint = 'Save As|Save current file with different name'
        OnClick = FileSave1Execute
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object FileExitItem: TMenuItem
        Caption = 'E&xit'
        Hint = 'Exit|Exit application'
        OnClick = FileExit1Execute
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      Hint = 'Edit commands'
      object CutItem: TMenuItem
        Caption = 'Cu&t'
        Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
        ImageIndex = 0
        ShortCut = 16472
      end
      object CopyItem: TMenuItem
        Caption = '&Copy'
        Hint = 'Copy|Copies the selection and puts it on the Clipboard'
        ImageIndex = 1
        ShortCut = 16451
      end
      object PasteItem: TMenuItem
        Caption = '&Paste'
        Hint = 'Paste|Inserts Clipboard contents'
        ImageIndex = 2
        ShortCut = 16470
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      Hint = 'Help topics'
      object HelpAboutItem: TMenuItem
        Caption = '&About...'
        Hint = 
          'About|Displays program information, version number, and copyrigh' +
          't'
        OnClick = HelpAbout1Execute
      end
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'All Files (*.*)|*.*'
    Left = 80
    Top = 192
  end
  object SaveDialog: TSaveDialog
    Filter = 'All Files (*.*)|*.*'
    Left = 176
    Top = 192
  end
  object GLScene: TGLScene
    Left = 376
    Top = 56
    object DC_utils: TGLDummyCube
      CubeSize = 1.000000000000000000
      object dc_cam: TGLDummyCube
        VisibilityCulling = vcNone
        CubeSize = 0.079999998211860660
        VisibleAtRunTime = True
        object cam: TGLCamera
          DepthOfView = 100.000000000000000000
          FocalLength = 50.000000000000000000
          TargetObject = PlaneXY
          Position.Coordinates = {0000C841000050410000C8410000803F}
          Left = 216
          Top = 152
          object GLLightSource1: TGLLightSource
            ConstAttenuation = 1.000000000000000000
            Position.Coordinates = {0000484200002042000070420000803F}
            SpotCutOff = 180.000000000000000000
          end
        end
      end
    end
    object DC_world: TGLDummyCube
      CubeSize = 1.000000000000000000
      object PlaneXY: TGLFreeForm
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Palette'
        ShowAxes = True
        MaterialLibrary = GLMaterialLibrary1
        LightmapLibrary = GLMaterialLibrary1
      end
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Palette'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.BackProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {CFCECE3ECFCECE3EC3C2C23ED9CE773F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.Image.Picture.Data = {
          07544269746D617066000000424D660000000000000036000000280000001000
          0000010000000100180000000000300000000000000000000000000000000000
          0000C200C2FF00FFFC0100FFFF0100FF0100FFFF007FFF0002FBC0C0C0E3E5E5
          E3E5E5E3E5E5E3E5E5E3E5E5E3E5E5E3E5E5}
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureWrap = twNone
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {0000003F0000803F0000803F00000000}
      end
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {C6BF3F3FDCD8583FDCD8583F0000803F}
        Material.FrontProperties.Diffuse.Color = {EBE0E03EE4DB5B3F9A93133F0000803F}
        Material.FrontProperties.Emission.Color = {0000000000000000000000009A99993E}
        Material.FrontProperties.Specular.Color = {EBE0E03EE4DB5B3FE4DB5B3F0000803F}
        Material.BlendingMode = bmTransparency
      end>
    Left = 480
    Top = 48
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene
    Enabled = False
    Left = 584
    Top = 49
  end
  object cad: TGLCadencer
    Scene = GLScene
    Enabled = False
    Mode = cmApplicationIdle
    OnProgress = cadProgress
    Left = 32
    Top = 88
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    Interval = 800
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpIdle
    Left = 104
    Top = 88
  end
end
