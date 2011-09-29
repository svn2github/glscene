object Form1: TForm1
  Left = 160
  Top = 79
  Caption = 'Form1'
  ClientHeight = 359
  ClientWidth = 511
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object SceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 511
    Height = 359
    Camera = GLCamera
    VSync = vsmSync
    Buffer.BackgroundColor = clBlack
    Buffer.FaceCulling = False
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 143.034362792968800000
    Align = alClient
    OnMouseDown = SceneViewerMouseDown
    OnMouseMove = SceneViewerMouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 24
    Top = 24
    object GLDummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLArrowLine1: TGLArrowLine
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        BottomRadius = 0.100000001490116100
        Height = 1.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000484200004842000000000000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object DirectOpenGL: TGLDirectOpenGL
      UseBuildList = False
      OnRender = DirectOpenGLRender
      Blend = False
    end
    object GLPoints: TGLPoints
      Static = False
      NoZWrite = False
      Size = 5.000000000000000000
      Style = psSmooth
    end
    object GLPlane: TGLPlane
      Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000803FCDCC4C3F}
      Material.DepthProperties.DepthWrite = False
      Material.BlendingMode = bmTransparency
      Material.MaterialOptions = [moNoLighting]
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Height = 6.000000000000000000
      Width = 6.000000000000000000
      object GLXYZGrid1: TGLXYZGrid
        XSamplingScale.Min = -3.000000000000000000
        XSamplingScale.max = 3.000000000000000000
        XSamplingScale.Origin = 1.000000000000000000
        XSamplingScale.step = 0.500000000000000000
        YSamplingScale.Min = -3.000000000000000000
        YSamplingScale.max = 3.000000000000000000
        YSamplingScale.step = 0.500000000000000000
        ZSamplingScale.step = 0.100000001490116100
        LineColor.Color = {0000803F0000803F0000803F0000003F}
      end
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 60.000000000000000000
      TargetObject = GLDummyCube
      Position.Coordinates = {0000E0400000A040000040400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 2
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 136
  end
  object GLMaterialLibraryEx1: TGLMaterialLibraryEx
    Materials = <
      item
        Name = 'PointMaterial'
        Tag = 0
        FixedFunction.Enabled = True
        FixedFunction.LineProperties.Enabled = False
        FixedFunction.Texture.Enabled = False
        Multitexturing.Enabled = False
        Multitexturing.Texture0.Enabled = False
        Multitexturing.Texture1.Enabled = False
        Multitexturing.Texture2.Enabled = False
        Multitexturing.Texture3.Enabled = False
        ShaderModel3.Enabled = False
        ShaderModel4.Enabled = False
        ShaderModel5.Enabled = False
      end>
    Left = 24
    Top = 80
  end
end
