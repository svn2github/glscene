object Form1: TForm1
  Left = 192
  Top = 114
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
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
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
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 8
    Top = 8
    object GLRenderPoint: TGLRenderPoint
    end
    object GLEarthSkyDome1: TGLEarthSkyDome
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
      Up.Coordinates = {00000000000000000000803F00000000}
      TilesPerTexture = 1
      MaterialLibrary = MLTerrain
      QualityDistance = 500
      QualityStyle = hrsTesselated
      CLODPrecision = 30
      OnGetTerrainBounds = TerrainGetTerrainBounds
    end
    object Blended: TGLDummyCube
      ObjectsSorting = osNone
      OnProgress = BlendedProgress
      CubeSize = 1
      object WaterPlane: TGLPlane
        Material.MaterialLibrary = MLWater
        Material.LibMaterialName = 'Water'
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        Hint = 'Please make me look nicer'
        Height = 1
        Width = 1
        NoZWrite = False
      end
      object RenderTrees: TGLParticleFXRenderer
        BlendingMode = bmTransparency
      end
      object DOTrees: TGLDirectOpenGL
        UseBuildList = False
        OnRender = DOTreesRender
      end
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
      Direction.Coordinates = {F304353F00000000F304353F00000000}
      Up.Coordinates = {00000000FFFF7F3F0000008000000000}
    end
  end
  object MLTrees: TGLMaterialLibrary
    Left = 40
    Top = 8
  end
  object MLTerrain: TGLMaterialLibrary
    Left = 72
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object GLNavigator1: TGLNavigator
    MoveUpWhenMovingForward = False
    InvertHorizontalSteeringWhenUpsideDown = False
    VirtualUp.Coordinates = {000000000000803F000000000000803F}
    MovingObject = Camera
    UseVirtualUp = True
    AutoUpdateObject = True
    AngleLock = False
    Left = 40
    Top = 40
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Left = 72
    Top = 40
  end
  object GLUserInterface1: TGLUserInterface
    InvertMouse = False
    MouseSpeed = 40
    GLNavigator = GLNavigator1
    GLVertNavigator = GLNavigator1
    Left = 40
    Top = 72
  end
  object GLStaticImposterBuilder1: TGLStaticImposterBuilder
    RenderPoint = GLRenderPoint
    BackColor.Color = {0000803E0000003F0000000000000000}
    BuildOffset.Coordinates = {000000000000403F000000000000803F}
    ImposterOptions = [impoBlended, impoAlphaTest, impoPerspectiveCorrection]
    ImposterReference = irBottom
    AlphaTreshold = 0.5
    OnLoadingImposter = GLStaticImposterBuilder1LoadingImposter
    OnImposterLoaded = GLStaticImposterBuilder1ImposterLoaded
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
    SamplingRatioBias = 1.20000004768372
    SamplesAlphaScale = 5
    Left = 104
    Top = 40
  end
  object PFXTrees: TGLCustomPFXManager
    Cadencer = GLCadencer1
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
    Left = 104
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
        Material.FrontProperties.Diffuse.Color = {CDCC4C3ECDCCCC3ECDCC4C3FCDCCCC3E}
        Material.BlendingMode = bmTransparency
        Material.FaceCulling = fcNoCull
        Tag = 0
      end>
    Left = 104
    Top = 8
  end
end
