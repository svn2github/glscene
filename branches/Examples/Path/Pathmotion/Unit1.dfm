object Form1: TForm1
  Left = 192
  Top = 124
  Caption = 'PathMotion'
  ClientHeight = 527
  ClientWidth = 762
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 762
    Height = 527
    Camera = cam
    Buffer.BackgroundColor = 986895
    Buffer.AntiAliasing = aa4x
    FieldOfView = 158.511352539062500000
    Align = alClient
    OnMouseDown = vpMouseDown
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object dc_cam: TGLDummyCube
      CubeSize = 1.000000000000000000
      object cam: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = dc_cam
        Position.Coordinates = {0000004000004040000080400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object ship: TGLCube
      Material.FrontProperties.Diffuse.Color = {0000803F1283003F000000000000803F}
      Position.Coordinates = {000040C000000000000000000000803F}
      CubeSize = {CDCCCC3DCDCC4C3DCDCC4C3E}
      object GLCube2: TGLCube
        Material.FrontProperties.Diffuse.Color = {EBE0E03EE4DB5B3F9A93133F0000803F}
        Position.Coordinates = {00000000CDCC4C3DCDCCCCBD0000803F}
        CubeSize = {0AD7A33CCDCCCC3DCDCCCC3D}
      end
    end
    object path: TGLLines
      Nodes = <>
      NodesAspect = lnaCube
      NodeSize = 0.200000002980232200
      SplineMode = lsmCubicSpline
      Options = []
    end
    object GLSphere1: TGLSphere
      Material.FrontProperties.Diffuse.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
      ShowAxes = True
      Radius = 0.500000000000000000
      Slices = 24
      object GLSphere2: TGLSphere
        Material.FrontProperties.Diffuse.Color = {986E923E6ABCF43E6ABCF43E0000803F}
        Material.BlendingMode = bmAdditive
        Radius = 0.850000023841857900
        Slices = 24
      end
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    Enabled = False
    Mode = cmApplicationIdle
    OnProgress = cadProgress
    Left = 40
    Top = 8
  end
end
