object Form1: TForm1
  Left = 173
  Top = 110
  Width = 457
  Height = 316
  BorderWidth = 3
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 443
    Height = 278
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object DCSphere: TGLDummyCube
      CubeSize = 1
      object Sphere: TGLSphere
        Material.FrontProperties.Diffuse.Color = {000000000000803F000000000000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {0000804000000000000000000000803F}
        Radius = 0.5
      end
      object Lines2: TGLLines
        LineColor.Color = {000000000000803F000000000000803F}
        Nodes = <
          item
            X = 4
            Y = -4
          end
          item
            X = 4
            Y = 4
          end>
        NodesAspect = lnaInvisible
        Options = []
      end
    end
    object GLLightSource1: TGLLightSource
      Ambient.Color = {C1C0403FC1C0403FC1C0403F0000803F}
      ConstAttenuation = 1
      Position.Coordinates = {0000A04100007041000040410000803F}
      SpotCutOff = 180
    end
    object DCArrow: TGLDummyCube
      CubeSize = 1
      object ArrowLine: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {C9C8C83EC9C8C83E0000803F0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {000080BF00000000000000000000803F}
        BottomRadius = 0.100000001490116
        Height = 1
        TopRadius = 0.100000001490116
        TopArrowHeadHeight = 0.5
        TopArrowHeadRadius = 0.200000002980232
        BottomArrowHeadHeight = 0.5
        BottomArrowHeadRadius = 0.200000002980232
        object Lines1: TGLLines
          LineColor.Color = {D6D5553FD6D5553F0000803F0000803F}
          Nodes = <
            item
              Z = -1
            end
            item
              Z = 7
            end>
          NodesAspect = lnaInvisible
          Options = []
        end
        object Plane1: TGLPlane
          Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
          Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
          Direction.Coordinates = {000000800000803F0000000000000000}
          Position.Coordinates = {0000000000000000CDCC4CBE0000803F}
          Up.Coordinates = {0000803F000000000000000000000000}
          Height = 1
          Width = 0.300000011920929
          NoZWrite = False
        end
      end
    end
    object Disk1: TGLDisk
      Material.FrontProperties.Diffuse.Color = {00000000000000008180003F0000803F}
      Material.FrontProperties.Emission.Color = {00000000000000008180003F0000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {E9DC72BF000000009BE8A13E00000000}
      InnerRadius = 0.899999976158142
      Loops = 1
      OuterRadius = 1
      SweepAngle = 360
    end
    object Disk2: TGLDisk
      Material.FrontProperties.Diffuse.Color = {000000008180003F000000000000803F}
      Material.FrontProperties.Emission.Color = {000000008180803E000000000000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {E9DC72BF000000009BE8A13E00000000}
      InnerRadius = 3.90000009536743
      Loops = 1
      OuterRadius = 4
      Slices = 32
      SweepAngle = 360
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DCSphere
      Position.Coordinates = {0000A0400000E040000000410000803F}
      Left = 208
      Top = 128
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 16
  end
end
