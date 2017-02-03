object Form1: TForm1
  Left = 192
  Top = 109
  Caption = 'Terrain Morphing'
  ClientHeight = 465
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 640
    Height = 465
    Camera = Camera
    Buffer.FogEnvironment.FogStart = 45.000000000000000000
    Buffer.FogEnvironment.FogEnd = 50.000000000000000000
    Buffer.BackgroundColor = clBlack
    Buffer.AmbientColor.Color = {0000000000000000000000000000803F}
    Buffer.ContextOptions = [roDoubleBuffer]
    Buffer.Lighting = False
    FieldOfView = 146.488494873046900000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object Scene: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLTerrainRenderer1: TGLTerrainRenderer
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Terrain'
        Direction.Coordinates = {000000000000803F0000000000000000}
        Scale.Coordinates = {0000003F0000003FCDCC4C3D00000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        HeightDataSource = GLBitmapHDS1
        TileSize = 32
        TilesPerTexture = 8.000000000000000000
        QualityDistance = 150.000000000000000000
        QualityStyle = hrsTesselated
        OnGetTerrainBounds = GLTerrainRenderer1GetTerrainBounds
        ContourWidth = 0
      end
    end
    object Cam: TGLDummyCube
      Position.Coordinates = {0000804200002041000000000000803F}
      CubeSize = 1.000000000000000000
      object Camera: TGLCamera
        DepthOfView = 300.000000000000000000
        FocalLength = 70.000000000000000000
      end
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
    object fpscounter: TGLHUDText
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Terrain'
        Tag = 0
        Material.Texture.MinFilter = miLinearMipmapNearest
        Material.Texture.TextureFormat = tfRGB16
        Material.Texture.Compression = tcHighSpeed
        Material.Texture.Disabled = False
      end>
    Left = 40
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 72
    Top = 8
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 104
    Top = 8
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Ranges = <
      item
        StartASCII = '0'
        StopASCII = '9'
        StartGlyphIdx = 0
      end>
    Left = 136
    Top = 8
  end
  object GLBitmapHDS1: TGLBitmapHDS
    InfiniteWrap = False
    MaxPoolSize = 0
    Left = 168
    Top = 8
  end
end
