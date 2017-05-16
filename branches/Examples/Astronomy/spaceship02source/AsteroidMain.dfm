object fAsteroidField: TfAsteroidField
  Left = 196
  Top = 108
  Caption = 'Asteroid Field'
  ClientHeight = 431
  ClientWidth = 678
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 678
    Height = 431
    Camera = camPilot
    PostRender = GLSceneViewer1PostRender
    Buffer.BackgroundColor = clBackground
    Buffer.AntiAliasing = aaNone
    Buffer.ColorDepth = cd8bits
    FieldOfView = 156.410339355468800000
    Align = alClient
    TabOrder = 0
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 16
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 48
    object oglStarDome: TGLDirectOpenGL
      ObjectsSorting = osRenderFarthestFirst
      VisibilityCulling = vcNone
      UseBuildList = True
      OnRender = oglStarDomeRender
      Blend = False
    end
    object dumWorld: TGLDummyCube
      ObjectsSorting = osRenderFarthestFirst
      CubeSize = 1.000000000000000000
      object camExternal: TGLCamera
        DepthOfView = 1000000.000000000000000000
        FocalLength = 50.000000000000000000
        SceneScale = 0.699999988079071000
        TargetObject = dumStation
        Position.Coordinates = {0000FA440000FAC30000803F0000803F}
      end
      object dumStation: TGLDummyCube
        Position.Coordinates = {0000FA4400000000000000000000803F}
        CubeSize = 1.000000000000000000
        object Center: TGLCylinder
          Material.MaterialLibrary = mlMaterials
          Material.LibMaterialName = 'txStationCentre'
          BottomRadius = 100.000000000000000000
          Height = 50.000000000000000000
          Stacks = 2
          TopRadius = 100.000000000000000000
        end
        object Annulus: TGLAnnulus
          Material.MaterialLibrary = mlMaterials
          Material.LibMaterialName = 'txShip'
          BottomRadius = 450.000000000000000000
          Height = 50.000000000000000000
          Slices = 24
          BottomInnerRadius = 400.000000000000000000
          TopInnerRadius = 400.000000000000000000
          TopRadius = 450.000000000000000000
        end
        object Arm1: TGLCylinder
          Material.MaterialLibrary = mlMaterials
          Material.LibMaterialName = 'txShip'
          Direction.Coordinates = {0000803F000000000000000000000000}
          Position.Coordinates = {000000000000000000007A430000803F}
          Up.Coordinates = {0000000000000000000080BF00000000}
          BottomRadius = 24.000000000000000000
          Height = 302.000000000000000000
          TopRadius = 24.000000000000000000
        end
        object Arm2: TGLCylinder
          Material.MaterialLibrary = mlMaterials
          Material.LibMaterialName = 'txShip'
          Position.Coordinates = {00007AC300000000000000000000803F}
          Up.Coordinates = {0000803F000000000000008000000000}
          BottomRadius = 24.000000000000000000
          Height = 302.000000000000000000
          TopRadius = 24.000000000000000000
        end
        object Arm3: TGLCylinder
          Material.MaterialLibrary = mlMaterials
          Material.LibMaterialName = 'txShip'
          Direction.Coordinates = {0000803F000000000000000000000000}
          Position.Coordinates = {000000000000000000007AC30000803F}
          Up.Coordinates = {0000000000000000000080BF00000000}
          BottomRadius = 24.000000000000000000
          Height = 302.000000000000000000
          TopRadius = 24.000000000000000000
        end
        object Arm4: TGLCylinder
          Material.MaterialLibrary = mlMaterials
          Material.LibMaterialName = 'txShip'
          Position.Coordinates = {00007A4300000000000000000000803F}
          Up.Coordinates = {0000803F000000000000008000000000}
          BottomRadius = 24.000000000000000000
          Height = 302.000000000000000000
          TopRadius = 24.000000000000000000
        end
      end
      object dumCollisionAsteroid: TGLDummyCube
        Position.Coordinates = {00401C4600401C4600401C460000803F}
        Visible = False
        CubeSize = 1.000000000000000000
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          000200060B636F6C41737465726F696402030200}
      end
      object dumTrajectory: TGLDummyCube
        CubeSize = 1.000000000000000000
        object oglSpeedVector: TGLDirectOpenGL
          UseBuildList = False
          OnRender = oglSpeedVectorRender
          Blend = False
        end
      end
      object dumShip: TGLDummyCube
        Position.Coordinates = {0000000000000000000020410000803F}
        CubeSize = 1.000000000000000000
        object dumOwnShip: TGLDummyCube
          VisibilityCulling = vcNone
          Direction.Coordinates = {000000000000803F0000000000000000}
          Scale.Coordinates = {00002041000020410000204100000000}
          Up.Coordinates = {0000000000000000000080BF00000000}
          CubeSize = 1.000000000000000000
          BehavioursData = {
            0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
            000200060B636F6C41737465726F696402030200}
          object OwnHull: TGLFrustrum
            Material.FaceCulling = fcNoCull
            Material.MaterialLibrary = mlMaterials
            Material.LibMaterialName = 'txShip'
            VisibilityCulling = vcNone
            Direction.Coordinates = {000000003A5E563D2FA67F3F00000000}
            Scale.Coordinates = {CDCCCC3E0000803FCDCCCC3D00000000}
            Up.Coordinates = {000000002FA67F3F3A5E56BD00000000}
            FrustrumSize = {0000803F0000803F0000803FCDCC4C3F}
            object ReactorFlame: TGLCylinder
              Material.MaterialLibrary = mlMaterials
              Material.LibMaterialName = 'txFlamme'
              Direction.Coordinates = {0000000000000000000080BF00000000}
              Visible = False
              BottomRadius = 0.029999999329447750
              Height = 1.000000000000000000
              Slices = 7
              Stacks = 2
              TopRadius = 0.004999999888241291
              Parts = [cySides]
              Alignment = caBottom
            end
          end
          object OwnStbdReactor: TGLCylinder
            Material.MaterialLibrary = mlMaterials
            Material.LibMaterialName = 'txReactor'
            Position.Coordinates = {EC51383ECDCCCC3D000000000000803F}
            BottomRadius = 0.039999999105930330
            Height = 0.400000005960464500
            Slices = 12
            Stacks = 2
            TopRadius = 0.039999999105930330
            object stbbakFlame: TGLProxyObject
              MasterObject = ReactorFlame
              Direction.Coordinates = {0000000000000000000080BF00000000}
              Position.Coordinates = {00000000CDCC4CBE000000000000803F}
              Scale.Coordinates = {6666A63F0000803F6666A63F00000000}
              Up.Coordinates = {00000000000080BF0000000000000000}
            end
            object stbfwdFlame: TGLProxyObject
              MasterObject = ReactorFlame
              Position.Coordinates = {000000003D0A573E000000000000803F}
              Scale.Coordinates = {0000803F0000003F0000803F00000000}
            end
          end
          object OwnPortReactor: TGLCylinder
            Material.MaterialLibrary = mlMaterials
            Material.LibMaterialName = 'txReactor'
            Position.Coordinates = {EC5138BECDCCCC3D000000000000803F}
            BottomRadius = 0.039999999105930330
            Height = 0.400000005960464500
            Slices = 12
            Stacks = 2
            TopRadius = 0.039999999105930330
            object prtbakFlame: TGLProxyObject
              MasterObject = ReactorFlame
              Direction.Coordinates = {0000000000000000000080BF00000000}
              Position.Coordinates = {00000000CDCC4CBE000000000000803F}
              Scale.Coordinates = {6666A63F0000803F6666A63F00000000}
              Up.Coordinates = {00000000000080BF0000000000000000}
            end
            object prtfwdFlame: TGLProxyObject
              MasterObject = ReactorFlame
              Position.Coordinates = {000000003D0A573E000000000000803F}
              Scale.Coordinates = {0000803F0000003F0000803F00000000}
            end
          end
          object OwnTopReactor: TGLCylinder
            Material.MaterialLibrary = mlMaterials
            Material.LibMaterialName = 'txReactor'
            Position.Coordinates = {000000000AD7233DCDCC4C3E0000803F}
            BottomRadius = 0.039999999105930330
            Height = 0.200000002980232200
            Stacks = 2
            TopRadius = 0.039999999105930330
            object topbakFlame: TGLProxyObject
              MasterObject = ReactorFlame
              Direction.Coordinates = {0000000000000000000080BF00000000}
              Position.Coordinates = {00000000EC51B8BD000000000000803F}
              Scale.Coordinates = {CDCC8C3F0000803FCDCC8C3F00000000}
              Up.Coordinates = {00000000000080BF0000000000000000}
            end
            object topfwdFlame: TGLProxyObject
              MasterObject = ReactorFlame
              Position.Coordinates = {000000000AD7233C000000000000803F}
              Scale.Coordinates = {0000803F0000003F0000803F00000000}
            end
          end
          object OwnBotReactor: TGLFrustrum
            Material.MaterialLibrary = mlMaterials
            Material.LibMaterialName = 'txReactor'
            Direction.Coordinates = {00000000000080BF2EBD3BB300000000}
            Position.Coordinates = {00000000CDCC4C3EB81E85BD0000803F}
            Scale.Coordinates = {0000803FCDCC4C3E0000803F00000000}
            Up.Coordinates = {000000002EBD3BB30000803F00000000}
            FrustrumSize = {CDCC4C3E9A99993ECDCCCC3DCDCCCC3D}
          end
          object lsReactor: TGLLightSource
            ConstAttenuation = 1.000000000000000000
            Diffuse.Color = {0000000000000000000000000000803F}
            LinearAttenuation = 0.009999999776482582
            Position.Coordinates = {00000000CDCC4CBE000000000000803F}
            LightStyle = lsOmni
            SpotCutOff = 180.000000000000000000
          end
          object oglReticle: TGLDirectOpenGL
            Direction.Coordinates = {00000000000080BF0000000000000000}
            Position.Coordinates = {000000000000803F8FC2F53D0000803F}
            Up.Coordinates = {00000000000000000000803F00000000}
            UseBuildList = True
            OnRender = oglReticleRender
            Blend = False
          end
        end
        object camPilot: TGLCamera
          DepthOfView = 10000.000000000000000000
          FocalLength = 45.000000000000000000
          Position.Coordinates = {000000009A99993F000000BF0000803F}
        end
      end
      object objExplosion: TGLSphere
        Material.BackProperties.Ambient.Color = {00000000000000000000000000000000}
        Material.BackProperties.Diffuse.Color = {00000000000000000000000000000000}
        Material.BackProperties.Emission.Color = {00000000000000000000000000000000}
        Material.BackProperties.Specular.Color = {00000000000000000000000000000000}
        Material.FrontProperties.Ambient.Color = {00000000000000000000000000000000}
        Material.FrontProperties.Diffuse.Color = {00000000000000000000000000000000}
        Material.FrontProperties.Emission.Color = {00000000000000000000000000000000}
        Material.FrontProperties.Specular.Color = {00000000000000000000000000000000}
        Visible = False
        Radius = 0.500000000000000000
        Slices = 3
        Stacks = 3
        EffectsData = {
          0458434F4C02010201060A54474C424669726546580201020012000000000200
          0200060C6666784578706C6F73696F6E}
      end
      object lsSun: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsParallel
        SpotCutOff = 180.000000000000000000
        SpotDirection.Coordinates = {0000000000007AC40000000000000000}
      end
      object oglDetector: TGLDirectOpenGL
        UseBuildList = False
        OnRender = oglDetectorRender
        Blend = False
      end
      object oglDust: TGLDirectOpenGL
        ObjectsSorting = osRenderFarthestFirst
        VisibilityCulling = vcNone
        UseBuildList = True
        OnRender = oglDustRender
        Blend = False
      end
      object dumSpaceShip: TGLDummyCube
        Direction.Coordinates = {000000000000803F0000000000000000}
        Scale.Coordinates = {00002041000020410000204100000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Visible = False
        CubeSize = 1.000000000000000000
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          000200060B636F6C41737465726F696402000201}
        object Hull: TGLFrustrum
          Material.MaterialLibrary = mlMaterials
          Material.LibMaterialName = 'txShip'
          Direction.Coordinates = {000000003A5E563D2FA67F3F00000000}
          Scale.Coordinates = {CDCCCC3E0000803FCDCCCC3D00000000}
          Up.Coordinates = {000000002FA67F3F3A5E56BD00000000}
          FrustrumSize = {0000803F0000803F0000803FCDCC4C3F}
        end
        object StbdReactor: TGLCylinder
          Material.MaterialLibrary = mlMaterials
          Material.LibMaterialName = 'txReactor'
          Position.Coordinates = {CDCC4C3ECDCCCC3D000000000000803F}
          BottomRadius = 0.039999999105930330
          Height = 0.400000005960464500
          Slices = 6
          Stacks = 2
          TopRadius = 0.039999999105930330
        end
        object PortReactor: TGLCylinder
          Material.MaterialLibrary = mlMaterials
          Material.LibMaterialName = 'txReactor'
          Position.Coordinates = {CDCC4CBECDCCCC3D000000000000803F}
          BottomRadius = 0.039999999105930330
          Height = 0.400000005960464500
          Slices = 6
          Stacks = 2
          TopRadius = 0.039999999105930330
        end
        object TopReactor: TGLCylinder
          Material.MaterialLibrary = mlMaterials
          Material.LibMaterialName = 'txReactor'
          Position.Coordinates = {00000000CDCCCC3D0AD7A33D0000803F}
          BottomRadius = 0.039999999105930330
          Height = 0.400000005960464500
          Slices = 6
          Stacks = 2
          TopRadius = 0.039999999105930330
        end
        object BotReactor: TGLFrustrum
          Material.MaterialLibrary = mlMaterials
          Material.LibMaterialName = 'txReactor'
          Direction.Coordinates = {00000000000080BF2EBD3BB300000000}
          Position.Coordinates = {00000000CDCC4C3EB81E85BD0000803F}
          Scale.Coordinates = {0000803FCDCC4C3E0000803F00000000}
          Up.Coordinates = {000000002EBD3BB30000803F00000000}
          FrustrumSize = {CDCC4C3E9A99993ECDCCCC3DCDCCCC3D}
        end
      end
      object dumTest: TGLDummyCube
        Position.Coordinates = {0000C84300000000000000000000803F}
        Visible = False
        CubeSize = 1.000000000000000000
        object testAnnulus: TGLAnnulus
          Material.BackProperties.Ambient.Color = {0000003F0000003F0000003F0000803F}
          Material.BackProperties.Diffuse.Color = {0000000000000000000000000000803F}
          Material.BackProperties.Emission.Color = {9A99593F9A99593FCDCCCC3D0000803F}
          Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
          Material.FrontProperties.Emission.Color = {0000803FF8FEFE3E000000000000803F}
          Material.FaceCulling = fcNoCull
          Material.MaterialLibrary = mlMaterials
          Material.PolygonMode = pmLines
          VisibilityCulling = vcNone
          NormalDirection = ndInside
          BottomRadius = 500.000000000000000000
          Height = 100.000000000000000000
          Slices = 64
          Loops = 3
          BottomInnerRadius = 300.000000000000000000
          TopInnerRadius = 300.000000000000000000
          TopRadius = 500.000000000000000000
        end
      end
    end
    object dumMasters: TGLDummyCube
      Visible = False
      CubeSize = 1.000000000000000000
    end
  end
  object mlMaterials: TGLMaterialLibrary
    Materials = <
      item
        Name = 'CloseAsteroid'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {CDCC4C3DCDCC4C3DCDCC4C3D0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'FarAsteroid'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803E0000803E0000803E0000803F}
      end
      item
        Name = 'txShip'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000003F0000003F0000003F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {00004842000048420000803F00000000}
      end
      item
        Name = 'txReactor'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {CDCCCC3ECDCCCC3ECDCCCC3E0000803F}
        Material.MaterialLibrary = mlMaterials
      end
      item
        Name = 'txtTransparent'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {00000000000000000000000000000000}
        Material.FrontProperties.Diffuse.Color = {00000000000000000000000000000000}
        Material.FrontProperties.Emission.Color = {00000000000000000000000000000000}
        Material.MaterialLibrary = mlMaterials
      end
      item
        Name = 'txFlamme'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {00000000000000000000000000000000}
        Material.FrontProperties.Diffuse.Color = {0000003F0000003F9A99193F3333333F}
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.MinFilter = miNearest
        Material.Texture.TextureMode = tmBlend
        Material.Texture.TextureWrap = twNone
        Material.Texture.TextureFormat = tfAlpha
        Material.Texture.Disabled = False
        Material.MaterialLibrary = mlMaterials
      end
      item
        Name = 'txBlack'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.LibMaterialName = 'txBlack'
      end
      item
        Name = 'txStationCentre'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {8180003F8180003F8180003F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.Disabled = False
      end>
    Left = 16
    Top = 80
  end
  object TimerLOD: TTimer
    Interval = 200
    OnTimer = TimerLODTimer
    Left = 16
    Top = 112
  end
  object colAsteroid: TGLCollisionManager
    OnCollision = colAsteroidCollision
    Left = 16
    Top = 144
  end
  object ffxExplosion: TGLFireFXManager
    Cadencer = GLCadencer1
    ParticleSize = 0.200000002980232200
    FireDensity = 1.000000000000000000
    FireEvaporation = 0.860000014305114700
    FireRadius = 20.000000000000000000
    Disabled = True
    Paused = False
    ParticleInterval = 0.100000001490116100
    UseInterval = True
    Left = 16
    Top = 176
  end
  object Joystick1: TGLJoystick
    Capture = True
    JoystickID = jidJoystick1
    Left = 16
    Top = 208
  end
  object TimerInput: TTimer
    Interval = 100
    OnTimer = TimerInputTimer
    Left = 48
    Top = 112
  end
end
