object Form1: TForm1
  Left = 198
  Top = 138
  Width = 692
  Height = 488
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 110
  TextHeight = 16
  object viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 684
    Height = 451
    Camera = camera
    Buffer.BackgroundColor = clSilver
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.DepthPrecision = dp32bits
    FieldOfView = 154.996200561523
    Align = alClient
    OnMouseDown = viewerMouseDown
    OnMouseMove = viewerMouseMove
    OnMouseUp = viewerMouseUp
  end
  object scene: TGLScene
    Left = 40
    Top = 24
    object ProjLight: TGLProjectedTextures
      Emitters = <>
      Style = ptsOriginal
      object scenery: TGLDummyCube
        CubeSize = 1
        object GLCube2: TGLCube
          Scale.Coordinates = {000020C1000020C1000020C100000000}
          NormalDirection = ndInside
        end
        object light2: TGLDummyCube
          Position.Coordinates = {0000803F0000803F000000400000803F}
          CubeSize = 0.200000002980232
          EdgeColor.Color = {000000000000803F000000000000803F}
          object GLSphere3: TGLSphere
            Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
            Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
            Radius = 0.0500000007450581
            Slices = 4
            Stacks = 4
          end
          object emitter2: TGLTextureEmitter
            FOVy = 90
            Aspect = 1
          end
        end
        object Light: TGLDummyCube
          Position.Coordinates = {0000000000000000000000400000803F}
          Up.Coordinates = {000000000000803F0000008000000000}
          CubeSize = 0.200000002980232
          EdgeColor.Color = {0000803F0000803F000000000000803F}
          object GLLightSource1: TGLLightSource
            ConstAttenuation = 1
            SpotCutOff = 180
            SpotDirection.Coordinates = {00000000000000000000803F00000000}
          end
          object GLSphere2: TGLSphere
            Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
            Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
            Radius = 0.0500000007450581
            Slices = 4
            Stacks = 4
          end
          object emitter1: TGLTextureEmitter
            FOVy = 90
            Aspect = 1
          end
        end
        object GLCube1: TGLCube
          CubeSize = {0000403F0000403F0000403F}
        end
        object GLPlane1: TGLPlane
          Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
          Material.FrontProperties.Diffuse.Color = {FFFEFE3EFFFEFE3EFFFEFE3E0000803F}
          Material.MaterialOptions = [moNoLighting]
          Material.Texture.TextureMode = tmModulate
          Material.Texture.Disabled = False
          Position.Coordinates = {000000000000003F0000C0BF0000803F}
          Height = 4
          Width = 4
          NoZWrite = False
        end
        object GLPlane2: TGLPlane
          Material.Texture.Disabled = False
          Direction.Coordinates = {0000803F000000000000000000000000}
          Position.Coordinates = {000000C00000003F0000003F0000803F}
          Height = 4
          Width = 4
          NoZWrite = False
        end
        object GLPlane3: TGLPlane
          Material.FrontProperties.Diffuse.Color = {FFFEFE3EFFFEFE3EFFFEFE3E0000803F}
          Material.MaterialOptions = [moNoLighting]
          Material.Texture.TextureMode = tmModulate
          Material.Texture.Disabled = False
          Direction.Coordinates = {000000000000803F0000000000000000}
          Position.Coordinates = {000000000000C0BF0000003F0000803F}
          Up.Coordinates = {00000000000000000000803F00000000}
          Height = 4
          Width = 4
          NoZWrite = False
        end
        object GLSphere1: TGLSphere
          Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FE1E0E03D0000803F}
          Position.Coordinates = {CDCC4C3E0000003F0000003F0000803F}
          Radius = 0.300000011920929
        end
      end
    end
    object camera: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = scenery
      Position.Coordinates = {000040400000003F000020400000803F}
      Left = 328
      Top = 216
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = scene
    OnProgress = GLCadencer1Progress
    Left = 120
    Top = 24
  end
  object matLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'spot'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureWrap = twNone
        Material.Texture.Disabled = False
        Tag = 0
      end
      item
        Name = 'spot2'
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureWrap = twNone
        Material.Texture.Disabled = False
        Tag = 0
      end>
    Left = 80
    Top = 24
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 160
    Top = 24
  end
end
