object Form1: TForm1
  Left = 169
  Top = 106
  Width = 544
  Height = 375
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
    Width = 536
    Height = 346
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.FaceCulling = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 24
    object GLSphere1: TGLSphere
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'CubeMap'
      Radius = 50
      Slices = 9
      Stacks = 9
    end
    object DCTarget: TGLDummyCube
      Position.Coordinates = {00000040000000000000803F0000803F}
      CubeSize = 1
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
    end
    object GLHeightField1: TGLHeightField
      Material.Texture.MappingMode = tmmObjectLinear
      Material.Texture.MappingSCoordinates.Coordinates = {CDCC4C3D000000000000000000000000}
      Material.Texture.MappingTCoordinates.Coordinates = {00000000CDCC4C3D0000000000000000}
      Material.Texture.Disabled = False
      Direction.Coordinates = {00000000FFFF7F3F0100003300000000}
      Position.Coordinates = {00000000000080BF000000000000803F}
      Scale.Coordinates = {1F85EB3D1F85EB3D0000003F00000000}
      Up.Coordinates = {2FBD3B3302000033000080BF00000000}
      XSamplingScale.Min = -63
      XSamplingScale.Max = 63
      XSamplingScale.Step = 1
      YSamplingScale.Min = -63
      YSamplingScale.Max = 63
      YSamplingScale.Step = 1
      OnGetHeight = GLHeightField1GetHeight
    end
    object GLWaterPlane1: TGLWaterPlane
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'CubeMap'
      Position.Coordinates = {00000000000080BF000000000000803F}
      Scale.Coordinates = {000070410000803F0000704100000000}
      RainForce = 5000
      Viscosity = 0.990000009536743
      MaxWaveAmp = 900000
      Elastic = 10
      Resolution = 128
      SimulationFrequency = 100
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000E04000007041000040400000803F}
      SpotCutOff = 180
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 30
      TargetObject = DCTarget
      Position.Coordinates = {0000E04000008040000040400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 72
    Top = 24
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 24
    Top = 64
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'CubeMap'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000003F}
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.MappingMode = tmmCubeMapNormal
        Material.Texture.Disabled = False
        Tag = 0
        Shader = GLUserShader1
      end>
    Left = 120
    Top = 24
  end
  object GLUserShader1: TGLUserShader
    OnDoApply = GLUserShader1DoApply
    OnDoUnApply = GLUserShader1DoUnApply
    ShaderStyle = ssLowLevel
    Left = 120
    Top = 64
  end
end
