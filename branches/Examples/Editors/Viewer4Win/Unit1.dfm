object Form1: TForm1
  Left = 192
  Top = 124
  Caption = 'Viewer with 4 windows'
  ClientHeight = 436
  ClientWidth = 722
  Color = clGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 272
    Top = 96
    Width = 73
    Height = 73
    Pen.Color = 33023
    Pen.Width = 3
  end
  object pos_r: TImage
    Left = 400
    Top = 48
    Width = 17
    Height = 25
    Cursor = crSizeWE
  end
  object pos_t: TImage
    Left = 424
    Top = 24
    Width = 25
    Height = 17
    Cursor = crSizeNS
  end
  object pos_c: TImage
    Left = 400
    Top = 24
    Width = 17
    Height = 17
    Cursor = crSizeAll
  end
  object Panel1: TPanel
    Left = 537
    Top = 0
    Width = 185
    Height = 436
    Align = alRight
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 0
  end
  object vp: TGLSceneViewer
    Left = 130
    Top = 120
    Width = 95
    Height = 79
    Camera = cam
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aa6x
    FieldOfView = 76.617469787597660000
    OnMouseDown = vpMouseDown
    OnMouseMove = vpMouseMove
    TabOrder = 1
  end
  object vp2: TGLSceneViewer
    Left = 128
    Top = 16
    Width = 97
    Height = 89
    Camera = cam2
    BeforeRender = vp2BeforeRender
    AfterRender = vp2AfterRender
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aa6x
    FieldOfView = 154.669876098632800000
    TabOrder = 2
  end
  object vp1: TGLSceneViewer
    Left = 8
    Top = 16
    Width = 89
    Height = 81
    Camera = cam1
    BeforeRender = vp1BeforeRender
    AfterRender = vp1AfterRender
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aa6x
    FieldOfView = 152.260620117187500000
    TabOrder = 3
  end
  object vp3: TGLSceneViewer
    Left = 16
    Top = 120
    Width = 89
    Height = 73
    Camera = cam3
    BeforeRender = vp3BeforeRender
    AfterRender = vp3AfterRender
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aa6x
    FieldOfView = 149.356979370117200000
    TabOrder = 4
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    Interval = 20
    OnTimer = AsyncTimer1Timer
    Left = 552
    Top = 8
  end
  object GLScene1: TGLScene
    Left = 553
    Top = 40
    object dc_world: TGLDummyCube
      CubeSize = 1.000000000000000000
      object dc_light: TGLDummyCube
        CubeSize = 1.000000000000000000
        object light1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          Position.Coordinates = {00004843000096430000C8430000803F}
          SpotCutOff = 180.000000000000000000
        end
      end
      object dc_cam: TGLDummyCube
        CubeSize = 1.000000000000000000
        object cam: TGLCamera
          DepthOfView = 100.000000000000000000
          FocalLength = 50.000000000000000000
          TargetObject = dc_cam
          Position.Coordinates = {0000C04000000041000020410000803F}
        end
      end
      object dc_helpers: TGLDummyCube
        CubeSize = 1.000000000000000000
        object xyz_grid: TGLXYZGrid
          Position.Coordinates = {00000000000000BF000000000000803F}
          ShowAxes = True
          LineColor.Color = {0000000000000000000000000000803F}
          XSamplingScale.Min = -5.000000000000000000
          XSamplingScale.Max = 5.000000000000000000
          XSamplingScale.Step = 1.000000000000000000
          YSamplingScale.Step = 0.100000001490116100
          ZSamplingScale.Min = -5.000000000000000000
          ZSamplingScale.Max = 5.000000000000000000
          ZSamplingScale.Step = 1.000000000000000000
          Parts = [gpX, gpZ]
        end
        object xy_grid: TGLXYZGrid
          Visible = False
          LineColor.Color = {1283003F1283003F1283003F0000803F}
          XSamplingScale.Min = -20.000000000000000000
          XSamplingScale.Max = 20.000000000000000000
          XSamplingScale.Step = 1.000000000000000000
          YSamplingScale.Min = -20.000000000000000000
          YSamplingScale.Max = 20.000000000000000000
          YSamplingScale.Step = 1.000000000000000000
          ZSamplingScale.Step = 0.100000001490116100
        end
        object xz_grid: TGLXYZGrid
          Visible = False
          LineColor.Color = {1283003F1283003F1283003F0000803F}
          XSamplingScale.Min = -20.000000000000000000
          XSamplingScale.Max = 20.000000000000000000
          XSamplingScale.Step = 1.000000000000000000
          YSamplingScale.Step = 1.000000000000000000
          ZSamplingScale.Min = -20.000000000000000000
          ZSamplingScale.Max = 20.000000000000000000
          ZSamplingScale.Step = 1.000000000000000000
          Parts = [gpX, gpZ]
        end
        object yz_grid: TGLXYZGrid
          Visible = False
          LineColor.Color = {1283003F1283003F1283003F0000803F}
          XSamplingScale.Step = 1.000000000000000000
          YSamplingScale.Min = -20.000000000000000000
          YSamplingScale.Max = 20.000000000000000000
          YSamplingScale.Step = 1.000000000000000000
          ZSamplingScale.Min = -20.000000000000000000
          ZSamplingScale.Max = 20.000000000000000000
          ZSamplingScale.Step = 1.000000000000000000
          Parts = [gpY, gpZ]
        end
      end
      object GLCube1: TGLCube
      end
      object GLCylinder1: TGLCylinder
        Position.Coordinates = {000000400000003F000000000000803F}
        BottomRadius = 0.500000000000000000
        Height = 2.000000000000000000
        Slices = 6
        Stacks = 1
        TopRadius = 0.300000011920929000
      end
      object GLSphere1: TGLSphere
        Position.Coordinates = {000000C000000000000000000000803F}
        Radius = 0.800000011920929000
        Stacks = 8
      end
    end
    object dc_views: TGLDummyCube
      CubeSize = 1.000000000000000000
      object cam1: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 10.000000000000000000
        CameraStyle = csOrthogonal
        Position.Coordinates = {000000000000FA43000000000000803F}
        Direction.Coordinates = {00000000000080BF0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
      end
      object cam2: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 10.000000000000000000
        CameraStyle = csOrthogonal
        Position.Coordinates = {00000000000000000000FA430000803F}
      end
      object cam3: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 10.000000000000000000
        CameraStyle = csOrthogonal
        Position.Coordinates = {0000FA4300000000000000000000803F}
        Direction.Coordinates = {000080BF000000000000000000000000}
      end
    end
  end
end
