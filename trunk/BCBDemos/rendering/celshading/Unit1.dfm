object Form1: TForm1
  Left = 192
  Top = 114
  Width = 408
  Height = 434
  Caption = 'Cel Shading Demo'
  Color = clBtnFace
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
    Width = 400
    Height = 404
    Camera = GLCamera1
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1
      object GLCamera1: TGLCamera
        DepthOfView = 100
        FocalLength = 50
        TargetObject = GLDummyCube1
        Position.Coordinates = {00000000000000400000A0400000803F}
        Direction.Coordinates = {00000000000000000000803F00000000}
        Up.Coordinates = {000000000000803F0000008000000000}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1
          LightStyle = lsOmni
          SpotCutOff = 180
        end
      end
    end
    object GLActor1: TGLActor
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'TexturedCellMat'
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      PitchAngle = 90
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      Interval = 100
    end
    object GLTorus1: TGLTorus
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'ColoredCelMat'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      MajorRadius = 2.5
      MinorRadius = 0.25
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'TexturedCellMat'
        Material.FrontProperties.Emission.Color = {0000803F0000003F000000000000803F}
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.Disabled = False
        Tag = 0
        Shader = GLTexturedCelShader
      end
      item
        Name = 'ColoredCelMat'
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Tag = 0
        Shader = GLColoredCelShader
      end>
    Left = 40
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object AsyncTimer1: TAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 8
    Top = 72
  end
  object GLTexturedCelShader: TGLCelShader
    CelShaderOptions = [csoOutlines, csoTextured]
    OutlineWidth = 3
    Left = 40
    Top = 40
  end
  object GLColoredCelShader: TGLCelShader
    CelShaderOptions = [csoOutlines]
    OutlineWidth = 3
    Left = 40
    Top = 72
  end
end
