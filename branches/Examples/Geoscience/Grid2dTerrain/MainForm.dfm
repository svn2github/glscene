object frmMain: TfrmMain
  Left = 178
  Top = 210
  Caption = 'Height Field Grid2D'
  ClientHeight = 458
  ClientWidth = 704
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 573
    Height = 439
    Cursor = crHandPoint
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {C1C0403FC1C0403FC1C0403F0000803F}
    Buffer.FogEnvironment.FogStart = -100.000000000000000000
    Buffer.FogEnvironment.FogEnd = 100.000000000000000000
    Buffer.BackgroundColor = 13478450
    Buffer.ContextOptions = [roDoubleBuffer, roTwoSideLighting]
    Buffer.FaceCulling = False
    Buffer.ShadeModel = smSmooth
    FieldOfView = 154.335067749023400000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 439
    Width = 704
    Height = 19
    Panels = <
      item
        Width = 120
      end
      item
        Width = 120
      end
      item
        Width = 100
      end
      item
        Width = 100
      end>
  end
  object Panel1: TPanel
    Left = 573
    Top = 0
    Width = 131
    Height = 439
    Align = alRight
    TabOrder = 2
    ExplicitTop = -6
    object Label1: TLabel
      Left = 7
      Top = 104
      Width = 98
      Height = 26
      Caption = 'Vertical'#13#10'Exaggeration'
    end
    object rgViewMode: TRadioGroup
      Left = 6
      Top = 29
      Width = 115
      Height = 69
      Caption = 'View Mode'
      ItemIndex = 0
      Items.Strings = (
        'Fly'
        'Inspect')
      TabOrder = 0
      OnClick = rgViewModeClick
    end
    object VEedit: TEdit
      Left = 7
      Top = 130
      Width = 33
      Height = 21
      TabOrder = 1
      Text = '3.0'
      OnChange = VEeditChange
    end
    object CheckBoxTexture: TCheckBox
      Left = 22
      Top = 228
      Width = 97
      Height = 17
      Caption = 'Texure'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBoxTextureClick
    end
    object rgShadeModel: TRadioGroup
      Left = 6
      Top = 264
      Width = 115
      Height = 65
      Caption = 'Shade Model'
      ItemIndex = 0
      Items.Strings = (
        'Flat'
        'Smooth')
      TabOrder = 3
      OnClick = rgShadeModelClick
    end
    object rgRelief: TRadioGroup
      Left = 6
      Top = 164
      Width = 115
      Height = 58
      Caption = 'Relief'
      ItemIndex = 0
      Items.Strings = (
        'Mountain'
        'Pit')
      TabOrder = 4
      OnClick = rgReliefClick
    end
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 48
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000A0C1000096430000A0410000803F}
      LightStyle = lsOmni
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      Direction.Coordinates = {0000803F000000000000000000000000}
      CubeSize = 1.000000000000000000
      object HeightField1: TGLHeightField
        Material.BackProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Ambient.Color = {8180003F8180003F8180003F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        ObjectsSorting = osRenderFarthestFirst
        Direction.Coordinates = {000000000000803F0000000000000000}
        ShowAxes = True
        Up.Coordinates = {000080BF000000000000000000000000}
        XSamplingScale.Min = -150.000000000000000000
        XSamplingScale.Max = 150.000000000000000000
        XSamplingScale.Step = 5.000000000000000000
        YSamplingScale.Min = -150.000000000000000000
        YSamplingScale.Max = 150.000000000000000000
        YSamplingScale.Step = 5.000000000000000000
        ColorMode = hfcmAmbient
        Options = [hfoTextureCoordinates]
        OnGetHeight = HeightField1GetHeight
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 6000.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {00000000000070410000C8410000803F}
      Direction.Coordinates = {0000803F000000000000000000000000}
      Left = 416
      Top = 296
    end
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    Interval = 10
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 160
    Top = 48
  end
  object MainMenu1: TMainMenu
    Left = 296
    Top = 40
    object File1: TMenuItem
      Caption = '&File'
      object miOpenArcInfoGrid: TMenuItem
        Caption = 'Open &ArcInfo Grid...'
        OnClick = miOpenArcInfoGridClick
      end
      object miOpenSurferGrid: TMenuItem
        Caption = 'Open &Surfer Grid...'
        OnClick = miOpenSurferGridClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 392
    Top = 40
  end
end
