object Form1: TForm1
  Left = 420
  Top = 163
  Caption = 'Automatic Watter Drone'
  ClientHeight = 393
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 391
    Top = 0
    Width = 145
    Height = 393
    Align = alRight
    TabOrder = 0
    object FPSl: TLabel
      Left = 16
      Top = 8
      Width = 23
      Height = 13
      Caption = 'FPS:'
    end
    object Button1: TButton
      Left = 16
      Top = 32
      Width = 113
      Height = 25
      Caption = 'Map load'
      TabOrder = 0
      OnClick = Button1Click
    end
    object CamType: TRadioGroup
      Left = 8
      Top = 64
      Width = 129
      Height = 97
      Caption = 'Camera Type'
      ItemIndex = 0
      Items.Strings = (
        'Free Camera'
        'Drone Camera'
        'ThirdPerson Camera')
      TabOrder = 1
      OnClick = CamTypeClick
    end
  end
  object Viewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 391
    Height = 393
    Camera = Cam1
    FieldOfView = 151.307739257812500000
    Align = alClient
    TabOrder = 1
  end
  object Scene: TGLScene
    Left = 64
    Top = 40
    object Light01: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      SpotCutOff = 180.000000000000000000
    end
    object cam_cube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Cam1: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 50.000000000000000000
        Direction.Coordinates = {00000000000000800000803F00000000}
      end
    end
    object Cam2: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {00000000000040C00000A0C00000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {CA86363DE6BE7FBF0000000000000000}
    end
    object Cam3: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {0000000000000040000040C00000803F}
      Direction.Coordinates = {9999193F00000080CDCC4C3F00000000}
    end
    object SkyBox: TGLSkyBox
      Direction.Coordinates = {2CBD3B272EBD3B330000803F00000000}
      PitchAngle = 90.000000000000000000
      TurnAngle = -90.000000000000000000
      Up.Coordinates = {2CBD3B330000803F2EBD3BB300000000}
      MaterialLibrary = MLSkyBox
      MatNameTop = 'top'
      MatNameLeft = 'west'
      MatNameRight = 'east'
      MatNameFront = 'north'
      MatNameBack = 'south'
      CloudsPlaneOffset = 0.200000002980232200
      CloudsPlaneSize = 32.000000000000000000
      Style = sbsTopHalfClamped
    end
    object Terrain1: TGLTerrainRenderer
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'ground'
      Direction.Coordinates = {26A4ECA6000000000000803F00000000}
      Up.Coordinates = {7A65FC990000803F0000000000000000}
      HeightDataSource = HTF
      TileSize = 64
      TilesPerTexture = 1.000000000000000000
      QualityDistance = 10.000000000000000000
      CLODPrecision = 1000
      ContourWidth = 0
      BehavioursData = {
        0458434F4C02010201060C54474C444345537461746963020102001200000000
        020012000000000203020009090F0000A0410F00000000020008}
    end
  end
  object Cad1: TGLCadencer
    Scene = Scene
    OnProgress = l
    Left = 104
    Top = 40
  end
  object Terrain: TGLTerrainRenderer
    Direction.Coordinates = {26A4ECA6000000000000803F00000000}
    Position.Coordinates = {000000000000A041000000000000803F}
    Up.Coordinates = {7A65FC990000803F0000000000000000}
    TileSize = 64
    TilesPerTexture = 1.000000000000000000
    QualityDistance = 10.000000000000000000
    CLODPrecision = 1000
    ContourWidth = 0
  end
  object MLSkyBox: TGLMaterialLibrary
    Materials = <
      item
        Name = 'north'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.MinFilter = miLinearMipmapNearest
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twNone
        Material.Texture.TextureFormat = tfRGB
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
        Material.MaterialLibrary = MLSkyBox
      end
      item
        Name = 'east'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.MinFilter = miLinearMipmapNearest
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twNone
        Material.Texture.TextureFormat = tfRGB
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
      end
      item
        Name = 'south'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.MinFilter = miLinearMipmapNearest
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twNone
        Material.Texture.TextureFormat = tfRGB
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
      end
      item
        Name = 'west'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.MinFilter = miLinearMipmapNearest
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twNone
        Material.Texture.TextureFormat = tfRGB
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
      end
      item
        Name = 'top'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.MinFilter = miLinearMipmapNearest
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twNone
        Material.Texture.TextureFormat = tfRGB
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
      end>
    Left = 64
    Top = 96
  end
  object HTF: TGLHeightTileFileHDS
    MaxPoolSize = 0
    Left = 104
    Top = 96
  end
  object DroneMaterial: TGLMaterialLibrary
    Left = 64
    Top = 160
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'ground'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Emission.Color = {9A99993E9A99993E9A99993E0000803F}
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Compression = tcStandard
        Material.Texture.Disabled = False
        Texture2Name = 'details'
      end
      item
        Name = 'details'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureFormat = tfLuminance
        Material.Texture.Compression = tcStandard
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {00000043000000430000004300000000}
      end>
    Left = 184
    Top = 40
  end
  object IDDecoder: TIdDecoderMIME
    FillChar = '='
    Left = 341
    Top = 96
  end
  object OD: TOpenDialog
    Left = 344
    Top = 152
  end
  object FPSTimer: TGLAsyncTimer
    Enabled = True
    Interval = 500
    OnTimer = FPSTimerTimer
    Left = 184
    Top = 96
  end
  object PopupMenu1: TPopupMenu
    Left = 344
    Top = 40
  end
end
