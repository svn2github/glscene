object Form1: TForm1
  Left = 203
  Top = 108
  Width = 392
  Height = 430
  Caption = 'Virtual Beer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 384
    Height = 401
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.AmbientColor.Color = {0000000000000000000000000000803F}
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aa2x
    Buffer.ShadeModel = smSmooth
    Align = alClient
    OnDblClick = GLSceneViewer1DblClick
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    VisibilityCulling = vcHierarchical
    Left = 16
    Top = 16
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {000000C00000003F000040400000803F}
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
      object GLCylinder1: TGLCylinder
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {3333733F6666E63E000000000000003F}
        Material.MaterialOptions = [moNoLighting]
        Position.Coordinates = {00000000CDCC4C3D000000000000803F}
        Scale.Coordinates = {00000040CDCC0C400000004000000000}
        BottomRadius = 0.500000000000000000
        Height = 1.049999952316284000
        Slices = 32
        Stacks = 1
        TopRadius = 0.500000000000000000
        Parts = [cySides, cyBottom]
        EffectsData = {
          0201061254474C536F7572636550465845666665637402000614474C506F6C79
          676F6E5046584D616E6167657231020302000900000000CDCCCC3D0000000000
          00000002000900000000666666BF00000000000000000200090000803F9A9999
          3E0000803F00000000050000000000CDCCCCFA3F0500000000003333F3FE3F05
          00000000008FC2F5F93F0200020109}
      end
      object GLParticleFXRenderer2: TGLParticleFXRenderer
        ZTest = False
        BlendingMode = bmTransparency
      end
      object GLCylinder2: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moNoLighting]
        Position.Coordinates = {000000000000C03F000000000000803F}
        Scale.Coordinates = {000000400000003F0000004000000000}
        BottomRadius = 0.500000000000000000
        Height = 1.200000047683716000
        Slices = 32
        Stacks = 1
        TopRadius = 0.500000000000000000
        Parts = [cySides, cyTop]
      end
      object GLDummyCube3: TGLDummyCube
        Position.Coordinates = {00000000CDCCEC3F000000000000803F}
        CubeSize = 1.000000000000000000
        EffectsData = {
          0201061254474C536F7572636550465845666665637402000613474C5065726C
          696E5046584D616E616765723102030200080200080200090000803F9A99193E
          0000803F000000000500000000000000000000050000000000000080FF3F0500
          000000000AD7A3F93F0200020109}
      end
      object GLParticleFXRenderer1: TGLParticleFXRenderer
        BlendingMode = bmTransparency
      end
      object GLFreeForm1: TGLFreeForm
        Material.BlendingMode = bmAdditive
        Material.Texture.MappingMode = tmmSphere
        Material.Texture.Disabled = False
      end
    end
    object GLShadowPlane1: TGLShadowPlane
      Material.Texture.Disabled = False
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {0000000085EBB1BF000000000000803F}
      Scale.Coordinates = {00002041000020410000803F00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 1.000000000000000000
      Width = 1.000000000000000000
      NoZWrite = False
      ShadowingObject = GLFreeForm1
      ShadowedLight = GLLightSource1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000404000004040000040400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 16
  end
  object GLPerlinPFXManager1: TGLPerlinPFXManager
    Cadencer = GLCadencer1
    Renderer = GLParticleFXRenderer1
    Friction = 1.000000000000000000
    BlendingMode = bmTransparency
    Smoothness = 1.000000000000000000
    Brightness = 3.000000000000000000
    Gamma = 1.399999976158142000
    NoiseScale = 200
    NoiseAmplitude = 100
    ParticleSize = 0.300000011920929000
    ColorInner.Color = {0000803F0000803F0000803F00000000}
    ColorOuter.Color = {0000803F0000803F0000803F00000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000803F0000803F0000803F}
        ColorOuter.Color = {0000803F0000803F0000803F00000000}
        LifeTime = 0.100000001490116100
        SizeScale = 1.000000000000000000
      end
      item
        ColorInner.Color = {0000803F0000803F0000803F0000803F}
        ColorOuter.Color = {0000803F0000803F0000803F00000000}
        LifeTime = 4.000000000000000000
        SizeScale = 1.000000000000000000
      end
      item
        ColorInner.Color = {0000803F0000803F0000803F00000000}
        ColorOuter.Color = {0000803F0000803F0000803F00000000}
        LifeTime = 5.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 112
    Top = 16
  end
  object GLPolygonPFXManager1: TGLPolygonPFXManager
    Cadencer = GLCadencer1
    Renderer = GLParticleFXRenderer2
    Acceleration.Coordinates = {000000009A99993E0000000000000000}
    Friction = 1.000000000000000000
    BlendingMode = bmTransparency
    NbSides = 5
    ParticleSize = 0.029999999329447750
    ColorInner.Color = {0000803F000000000000000000000000}
    ColorOuter.Color = {0000803F0000803F0000000000000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000000000000000CDCCCC3E}
        ColorOuter.Color = {0000803F0000803F0000000000000000}
        LifeTime = 0.250000000000000000
        SizeScale = 1.000000000000000000
      end
      item
        ColorInner.Color = {0000803F00000000000000009A99193F}
        ColorOuter.Color = {0000803F0000803F0000000000000000}
        LifeTime = 3.500000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 152
    Top = 16
  end
end
