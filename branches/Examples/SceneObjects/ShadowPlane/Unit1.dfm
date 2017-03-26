object Form1: TForm1
  Left = 192
  Top = 107
  BorderStyle = bsToolWindow
  Caption = 'ShadowPlane with Cubes'
  ClientHeight = 473
  ClientWidth = 492
  Color = clBtnFace
  DefaultMonitor = dmPrimary
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 492
    Height = 473
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 156.125030517578100000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 24
    Top = 16
    object GLSkyDome1: TGLSkyDome
      VisibilityCulling = vcNone
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Bands = <
        item
          StartAngle = -30.000000000000000000
          StartColor.Color = {0000000000000000000000000000803F}
          StopColor.Color = {3333B33E3333B33E1F852B3F0000803F}
        end
        item
          StartColor.Color = {3333B33E3333B33E1F852B3F0000803F}
          StopAngle = 40.000000000000000000
          StopColor.Color = {ACC8483EACC8483ECDCC4C3F0000803F}
          Stacks = 4
        end>
      Stars = <>
    end
    object GLShadowPlane1: TGLShadowPlane
      Material.FrontProperties.Diffuse.Color = {8195033F8195033F8195033F0000803F}
      Material.BlendingMode = bmTransparency
      VisibilityCulling = vcNone
      Direction.Coordinates = {000000000000803F0000008000000000}
      Position.Coordinates = {00000000000000BF00000CC20000803F}
      Up.Coordinates = {00000000000000000000803F00000000}
      Height = 80.000000000000000000
      Width = 12.000000000000000000
      XTiles = 5
      YTiles = 50
      Style = [psTileTexture]
      ShadowingObject = gdcShadowing
      ShadowedLight = GLLightSource1
      ShadowColor.Color = {000000000000000000000000CDCC4C3E}
      ShadowOptions = [spoUseStencil]
    end
    object GLShadowPlane2: TGLShadowPlane
      Material.FrontProperties.Ambient.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Diffuse.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Emission.Color = {00000000000000000000000000000000}
      Material.BlendingMode = bmAdditive
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {00000000EE7CFFBE00000CC20000803F}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 80.000000000000000000
      Width = 10.000000000000000000
      Style = [psTileTexture]
      ShadowingObject = gdcShadowing
      ShadowedLight = GLLightSource2
      ShadowColor.Color = {000000000000000000000000CDCC4C3E}
      ShadowOptions = [spoUseStencil]
    end
    object GLShadowPlane3: TGLShadowPlane
      Material.FrontProperties.Ambient.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Diffuse.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Emission.Color = {00000000000000000000000000000000}
      Material.BlendingMode = bmAdditive
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {00000000A470FDBE00000CC20000803F}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 80.000000000000000000
      Width = 10.000000000000000000
      Style = []
      ShadowingObject = gdcShadowing
      ShadowedLight = GLLightSource3
      ShadowColor.Color = {000000000000000000000000CDCC4C3E}
      ShadowOptions = [spoUseStencil]
    end
    object gdcLights: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {9A99593F9A99593FCDCCCC3D0000803F}
        Position.Coordinates = {00004040000040400000F0C10000803F}
        SpotCutOff = 180.000000000000000000
      end
      object GLLightSource2: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {9A99593F9A99593FCDCCCC3D0000803F}
        Position.Coordinates = {00004040000040400000A0C10000803F}
        SpotCutOff = 180.000000000000000000
      end
      object GLLightSource3: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {9A99593F9A99593FCDCCCC3D0000803F}
        Position.Coordinates = {0000404000004040000020C10000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object gdcShadowing: TGLDummyCube
      ObjectsSorting = osNone
      CubeSize = 1.000000000000000000
      object GLDummyCube1: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLCube2: TGLCube
          Material.FrontProperties.Diffuse.Color = {9A99193FACC8483ECDCC4C3F0000803F}
          ObjectsSorting = osNone
          VisibilityCulling = vcNone
          CubeSize = {0000003F0000003F0000003F}
        end
        object GLCube1: TGLCube
          Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3FCDCCCC3E}
          Material.BlendingMode = bmTransparency
          Material.FaceCulling = fcNoCull
          ObjectsSorting = osNone
          VisibilityCulling = vcNone
        end
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {000000000000003F000080400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 100.000000000000000000
    OnProgress = GLCadencer1Progress
    Left = 112
    Top = 16
  end
end
