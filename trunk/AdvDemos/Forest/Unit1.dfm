object Form1: TForm1
  Left = 249
  Top = 100
  Align = alClient
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 413
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object SceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 505
    Height = 413
    Camera = Camera
    Buffer.FogEnvironment.FogColor.Color = {E5D0423FD9CE573F6210783F0000803F}
    Buffer.FogEnvironment.FogStart = 3000
    Buffer.FogEnvironment.FogEnd = 5000
    Buffer.BackgroundColor = 16242626
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roDestinationAlpha]
    Buffer.FogEnable = True
    Buffer.Lighting = False
    Buffer.AntiAliasing = aaNone
    Align = alClient
  end
  object GLScene: TGLScene
    ObjectsSorting = osNone
    Left = 8
    Top = 8
    object GLRenderPoint: TGLRenderPoint
    end
    object DOInitializeReflection: TGLDirectOpenGL
      UseBuildList = False
      OnRender = DOInitializeReflectionRender
    end
    object EarthSkyDome: TGLEarthSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Bands = <>
      Stars = <>
      SunElevation = 60
      Turbidity = 15
      Slices = 48
      Stacks = 24
    end
    object Light: TGLLightSource
      Ambient.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
      ConstAttenuation = 1
      LightStyle = lsParallel
      SpotCutOff = 180
      SpotDirection.Coordinates = {00000000000000000000803F00000000}
    end
    object Terrain: TGLTerrainRenderer
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {0000204500008442000020C50000803F}
      Scale.Coordinates = {00002041000020410000204000000000}
      Up.Coordinates = {00000000000000800000803F00000000}
      HeightDataSource = GLBitmapHDS1
      TileSize = 64
      TilesPerTexture = 8
      MaterialLibrary = MLTerrain
      QualityDistance = 500
      QualityStyle = hrsTesselated
      CLODPrecision = 15
      OnGetTerrainBounds = TerrainGetTerrainBounds
    end
    object DOClassicWaterPlane: TGLDirectOpenGL
      UseBuildList = False
      OnRender = DOClassicWaterPlaneRender
    end
    object DOGLSLWaterPlane: TGLDirectOpenGL
      UseBuildList = False
      OnRender = DOGLSLWaterPlaneRender
    end
    object RenderTrees: TGLParticleFXRenderer
      ZCull = False
      BlendingMode = bmTransparency
    end
    object DOTrees: TGLDirectOpenGL
      UseBuildList = False
      OnRender = DOTreesRender
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000A0410000A041000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Alignment = taLeftJustify
      Layout = tlTop
      ModulateColor.Color = {48E13A3F0AD7233E8FC2753F0000803F}
    end
    object Camera: TGLCamera
      DepthOfView = 5000
      FocalLength = 50
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {0080D4C3000000000080BBC40000803F}
      Direction.Coordinates = {F304353F00000000F304353F00000000}
      Up.Coordinates = {00000000FFFF7F3F0000008000000000}
    end
  end
  object MLTrees: TGLMaterialLibrary
    Left = 208
    Top = 8
  end
  object MLTerrain: TGLMaterialLibrary
    Left = 104
    Top = 8
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    OnProgress = GLCadencerProgress
    Left = 8
    Top = 40
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Left = 40
    Top = 8
  end
  object SIBTree: TGLStaticImposterBuilder
    RenderPoint = GLRenderPoint
    BackColor.Color = {0000803E0000003F0000000000000000}
    BuildOffset.Coordinates = {000000009A99193F000000000000803F}
    ImposterOptions = [impoBlended, impoAlphaTest, impoPerspectiveCorrection]
    ImposterReference = irBottom
    AlphaTreshold = 0.5
    OnLoadingImposter = SIBTreeLoadingImposter
    OnImposterLoaded = SIBTreeImposterLoaded
    Coronas = <
      item
        Samples = 13
        Elevation = -15
      end
      item
        Samples = 13
      end
      item
        Samples = 13
        Elevation = 15
      end
      item
        Samples = 13
        Elevation = 28
      end
      item
        Samples = 12
        Elevation = 40
      end>
    SampleSize = 128
    SamplingRatioBias = 1.14999997615814
    SamplesAlphaScale = 5
    Left = 208
    Top = 40
  end
  object PFXTrees: TGLCustomPFXManager
    Cadencer = GLCadencer
    Renderer = RenderTrees
    OnCreateParticle = PFXTreesCreateParticle
    Friction = 1
    BlendingMode = bmTransparency
    OnBeginParticles = PFXTreesBeginParticles
    OnRenderParticle = PFXTreesRenderParticle
    OnEndParticles = PFXTreesEndParticles
    OnProgress = PFXTreesProgress
    OnGetParticleCountEvent = PFXTreesGetParticleCountEvent
    ParticleSize = 1
    LifeColors = <
      item
        LifeTime = 3
        SizeScale = 1
      end>
    Left = 208
    Top = 72
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 72
  end
  object MLWater: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Water'
        Material.BackProperties.Diffuse.Color = {CDCC4C3ECDCCCC3ECDCC4C3FCDCCCC3E}
        Material.FrontProperties.Diffuse.Color = {CDCC4C3E0000003F0000803F0000803F}
        Material.BlendingMode = bmTransparency
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'media\caustics.bmp'
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureFormat = tfLuminance
        Material.Texture.Disabled = False
        Material.FaceCulling = fcNoCull
        Tag = 0
      end
      item
        Name = 'WaterBump'
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'media\caustics.bmp'
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureFormat = tfNormalMap
        Material.Texture.Disabled = False
        Tag = 0
      end>
    Left = 152
    Top = 8
  end
  object GLBitmapHDS1: TGLBitmapHDS
    MaxPoolSize = 0
    Left = 104
    Top = 40
  end
end
