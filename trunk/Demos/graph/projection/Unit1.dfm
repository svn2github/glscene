object Form1: TForm1
  Left = 160
  Top = 79
  Width = 519
  Height = 386
  Caption = 'Form1'
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
    Height = 357
    Camera = GLCamera
    Buffer.BackgroundColor = clBlack
    Buffer.FaceCulling = False
    Buffer.AntiAliasing = aa4xHQ
    Align = alClient
    OnMouseDown = SceneViewerMouseDown
    OnMouseMove = SceneViewerMouseMove
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 24
    Top = 24
    object GLDummyCube: TGLDummyCube
      CubeSize = 1
      object GLArrowLine1: TGLArrowLine
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        BottomRadius = 0.100000001490116
        Height = 1
        TopRadius = 0.100000001490116
        TopArrowHeadHeight = 0.5
        TopArrowHeadRadius = 0.200000002980232
        BottomArrowHeadHeight = 0.5
        BottomArrowHeadRadius = 0.200000002980232
      end
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1
        Position.Coordinates = {0000484200004842000000000000803F}
        SpotCutOff = 180
      end
    end
    object DirectOpenGL: TGLDirectOpenGL
      UseBuildList = False
      OnRender = DirectOpenGLRender
    end
    object GLPoints: TGLPoints
      NoZWrite = False
      Static = False
      Size = 5
      Style = psSmooth
    end
    object GLPlane: TGLPlane
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000803FCDCC4C3F}
      Material.BlendingMode = bmTransparency
      Material.MaterialOptions = [moNoLighting]
      Height = 6
      Width = 6
      object GLXYZGrid1: TGLXYZGrid
        LineColor.Color = {0000803F0000803F0000803F0000003F}
        XSamplingScale.Min = -3
        XSamplingScale.Max = 3
        XSamplingScale.Origin = 1
        XSamplingScale.Step = 0.5
        YSamplingScale.Min = -3
        YSamplingScale.Max = 3
        YSamplingScale.Step = 0.5
        ZSamplingScale.Step = 0.100000001490116
        LinesSmoothing = False
      end
    end
    object GLCamera: TGLCamera
      DepthOfView = 100
      FocalLength = 60
      TargetObject = GLDummyCube
      Position.Coordinates = {0000E0400000A040000040400000803F}
    end
  end
end
