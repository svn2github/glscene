object Form1: TForm1
  Left = 199
  Top = 134
  Caption = 'ClickPoly'
  ClientHeight = 501
  ClientWidth = 767
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
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 767
    Height = 501
    Camera = cam
    Buffer.BackgroundColor = 3374614
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aa4x
    FieldOfView = 140.475158691406300000
    Align = alClient
    OnMouseDown = vpMouseDown
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object dc: TGLDummyCube
      Position.Coordinates = {0000000000004040000000000000803F}
      CubeSize = 1.000000000000000000
      object cam: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 90.000000000000000000
        TargetObject = dc
        Position.Coordinates = {0000204100004041000070410000803F}
      end
    end
    object poly_floor: TGLPolygon
      Material.Texture.ImageClassName = 'TGLBlankImage'
      Material.Texture.Image.ColorFormat = 6408
      Material.Texture.Disabled = False
      Nodes = <>
      Parts = [ppBottom]
    end
    object poly_wall: TGLPolygon
      Material.FrontProperties.Emission.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
      Nodes = <>
      Parts = [ppBottom]
    end
    object GLXYZGrid1: TGLXYZGrid
      XSamplingScale.Min = -10.000000000000000000
      XSamplingScale.Max = 10.000000000000000000
      XSamplingScale.Step = 1.000000000000000000
      YSamplingScale.Step = 0.100000001490116100
      ZSamplingScale.Min = -10.000000000000000000
      ZSamplingScale.Max = 10.000000000000000000
      ZSamplingScale.Step = 1.000000000000000000
      Parts = [gpX, gpZ]
    end
    object dc_point: TGLDummyCube
      CubeSize = 1.000000000000000000
      object point: TGLFrustrum
        Material.FrontProperties.Emission.Color = {1283003F0000803F000000000000803F}
        Position.Coordinates = {000000000000803E000000000000803F}
        Scale.Coordinates = {CDCCCC3ECDCCCC3ECDCCCC3E00000000}
        object GLCube1: TGLCube
          Material.FrontProperties.Emission.Color = {A9A5253FB1A8283EB1A8283E0000803F}
          Position.Coordinates = {000000000000803F000000000000803F}
          CubeSize = {CDCCCC3D00000040CDCCCC3D}
        end
      end
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    OnProgress = cadProgress
    Left = 40
    Top = 8
  end
end
