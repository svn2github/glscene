object Form1: TForm1
  Left = 256
  Top = 473
  Caption = 'PathFinder'
  ClientHeight = 544
  ClientWidth = 826
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
    Width = 826
    Height = 544
    Camera = cam
    Buffer.BackgroundColor = 12615680
    Buffer.AntiAliasing = aa6x
    FieldOfView = 149.169174194335900000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object dc_cam: TGLDummyCube
      Position.Coordinates = {0000884100000000000020410000803F}
      CubeSize = 1.000000000000000000
      object cam: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 75.000000000000000000
        TargetObject = dc_cam
        Position.Coordinates = {0000A0C000007041000020410000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          LightStyle = lsParallel
          SpotCutOff = 180.000000000000000000
          SpotDirection.Coordinates = {0000803F00004040000000C000000000}
        end
      end
    end
    object dc_world: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
      object path: TGLLines
        AntiAliased = True
        LineColor.Color = {1283003F00000000000000000000803F}
        LineWidth = 2.000000000000000000
        Nodes = <>
        NodesAspect = lnaInvisible
        NodeSize = 0.300000011920929000
        Division = 2
        Options = []
      end
      object floor: TGLCube
        Material.FrontProperties.Diffuse.Color = {8195033F6ABC343F6ABC343F0000803F}
        Position.Coordinates = {00008C41000000BF000018410000803F}
        CubeSize = {000010420000803F0000A041}
      end
      object player: TGLSphere
        Material.FrontProperties.Diffuse.Color = {EBE0E03EE4DB5B3F9A93133F0000803F}
        Position.Coordinates = {0000803F9A99993E000010410000803F}
        Radius = 0.100000001490116100
        Slices = 12
        Stacks = 8
      end
      object target: TGLSphere
        Material.FrontProperties.Diffuse.Color = {0000803F0000803E000000000000803F}
        Position.Coordinates = {000008429A99993E000020410000803F}
        Radius = 0.100000001490116100
        Slices = 12
        Stacks = 8
      end
      object grid1: TGLXYZGrid
        Position.Coordinates = {000000BF6F12833A000000BF0000803F}
        LineColor.Color = {0000803F0000803F0000803FCDCC4C3E}
        XSamplingScale.Max = 36.000000000000000000
        XSamplingScale.Step = 0.333000004291534400
        YSamplingScale.Step = 0.100000001490116100
        ZSamplingScale.Max = 20.000000000000000000
        ZSamplingScale.Step = 0.333000004291534400
        Parts = [gpX, gpZ]
      end
      object grid2: TGLXYZGrid
        Position.Coordinates = {000000BF6F12033B000000BF0000803F}
        LineColor.Color = {0000803F0000803F0000803FCDCCCC3E}
        XSamplingScale.Max = 36.000000000000000000
        XSamplingScale.Step = 1.000000000000000000
        YSamplingScale.Step = 0.100000001490116100
        ZSamplingScale.Max = 20.000000000000000000
        ZSamplingScale.Step = 1.000000000000000000
        Parts = [gpX, gpZ]
      end
      object dc_walls: TGLDummyCube
        CubeSize = 1.000000000000000000
      end
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    Enabled = False
    SleepLength = 1
    OnProgress = cadProgress
    Left = 40
    Top = 8
  end
end
