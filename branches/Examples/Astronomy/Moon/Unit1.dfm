object Form1: TForm1
  Left = 126
  Top = 93
  Align = alClient
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 400
  ClientWidth = 603
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 603
    Height = 400
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.FogEnvironment.FogStart = 200.000000000000000000
    Buffer.FogEnvironment.FogEnd = 650.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clDefault
    Buffer.FogEnable = True
    Buffer.Lighting = False
    Buffer.AntiAliasing = aa2x
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLBitmapHDS1: TGLBitmapHDS
    MaxPoolSize = 0
    Left = 56
    Top = 16
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 56
    Top = 56
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLDummyCube2: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLTerrainRenderer1: TGLTerrainRenderer
      TilesPerTexture = 1.000000000000000000
      ContourWidth = 0
    end
    object GLSkyDome1: TGLSkyDome
      Bands = <
        item
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 15.000000000000000000
        end
        item
          StartAngle = 15.000000000000000000
          StopAngle = 90.000000000000000000
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Stacks = 4
        end>
      Stars = <>
    end
    object GLFreeForm1: TGLFreeForm
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 56
    Top = 96
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 16
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
    Left = 16
    Top = 56
  end
  object GLFireFXManager1: TGLFireFXManager
    FireDir.Coordinates = {00000000000000000000803F00000000}
    InitialDir.Coordinates = {00000000000000000000803F00000000}
    Cadencer = GLCadencer1
    InnerColor.Color = {0000803F0000003F000000000000803F}
    OuterColor.Color = {0000803F0000803E000000000000803F}
    FireDensity = 1.000000000000000000
    FireEvaporation = 0.860000014305114800
    ParticleLife = 1
    FireBurst = 4.000000000000000000
    FireRadius = 0.500000000000000000
    Disabled = False
    Paused = False
    ParticleInterval = 0.000099999997473788
    UseInterval = False
    Left = 16
    Top = 96
  end
end
