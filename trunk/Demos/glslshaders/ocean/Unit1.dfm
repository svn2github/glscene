object Form1: TForm1
  Left = 63
  Top = 64
  Width = 676
  Height = 471
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 668
    Height = 442
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.FaceCulling = False
    Buffer.Lighting = False
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {000000400000A040000000000000803F}
      SpotCutOff = 180
      object GLSphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Visible = False
        Radius = 0.0500000007450581
      end
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
    end
    object DOOceanPlane: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      OnRender = DOOceanPlaneRender
    end
    object GLHeightField1: TGLHeightField
      Material.MaterialLibrary = MatLib
      Material.LibMaterialName = 'water'
      XSamplingScale.Min = -100
      XSamplingScale.Max = 100
      XSamplingScale.Step = 2
      YSamplingScale.Min = -100
      YSamplingScale.Max = 100
      YSamplingScale.Step = 2
      OnGetHeight = GLHeightField1GetHeight
    end
    object GLSphere2: TGLSphere
      Material.MaterialLibrary = MatLib
      Material.LibMaterialName = 'cubeMap'
      Up.Coordinates = {00000000000080BF0000008000000000}
      Radius = 150
    end
    object GLCamera1: TGLCamera
      DepthOfView = 200
      FocalLength = 30
      SceneScale = 2
      TargetObject = GLHeightField1
      Position.Coordinates = {0000C84100007041000020410000803F}
      Direction.Coordinates = {0000803F000000000000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object MatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'water'
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureFormat = tfNormalMap
        Material.Texture.Disabled = False
        Material.Texture.NormalMapScale = 0.100000001490116
        Tag = 0
        Shader = GLUserShader1
      end
      item
        Name = 'cubeMap'
        Material.Texture.ImageClassName = 'TGLCubeMapImage'
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twNone
        Material.Texture.MappingMode = tmmCubeMapNormal
        Material.Texture.Disabled = False
        Tag = 0
      end>
    Left = 16
    Top = 56
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 136
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 176
  end
  object GLUserShader1: TGLUserShader
    OnDoApply = GLUserShader1DoApply
    OnDoUnApply = GLUserShader1DoUnApply
    ShaderStyle = ssLowLevel
    Left = 16
    Top = 96
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Camera = CameraCubeMap
    Width = 128
    Height = 128
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roDestinationAlpha]
    Left = 64
    Top = 56
  end
  object GLScene2: TGLScene
    Left = 64
    Top = 16
    object GLEarthSkyDome1: TGLEarthSkyDome
      Direction.Coordinates = {FEFF7F27000080A7FFFF7F3F00000000}
      RollAngle = -45
      Up.Coordinates = {F30435BFF304353FD413D42600000000}
      Bands = <>
      Stars = <>
      SunElevation = 25
      Turbidity = 75
      SunDawnColor.Color = {0000803F0000803F0000403F00000000}
      Slices = 48
      Stacks = 24
    end
    object CameraCubeMap: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      Direction.Coordinates = {00000000000000800000803F00000000}
    end
  end
end
