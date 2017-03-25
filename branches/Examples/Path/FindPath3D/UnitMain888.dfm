object FormShoot: TFormShoot
  Left = 196
  Top = 127
  BorderIcons = [biSystemMenu]
  Caption = 'DCEShootTemplate by [EViL] & Xeno'
  ClientHeight = 590
  ClientWidth = 790
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object SceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 535
    Height = 590
    Camera = Camera
    Buffer.BackgroundColor = clGray
    FieldOfView = 158.825363159179700000
    Align = alClient
    OnMouseDown = SceneViewerMouseDown
    OnMouseMove = SceneViewerMouseMove
    TabOrder = 0
  end
  object Button2: TButton
    Left = 412
    Top = 8
    Width = 121
    Height = 49
    Caption = 'Run'
    TabOrder = 1
    OnClick = Button2Click
  end
  object PageControl1: TPageControl
    Left = 535
    Top = 0
    Width = 255
    Height = 590
    ActivePage = TabSheet2
    Align = alRight
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Collisions'
      ExplicitWidth = 313
      ExplicitHeight = 269
      object Label2: TLabel
        Left = 16
        Top = 16
        Width = 32
        Height = 13
        Caption = 'Label2'
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Coordinates'
      ImageIndex = 1
      ExplicitTop = 28
      object Label4: TLabel
        Left = 8
        Top = 24
        Width = 32
        Height = 13
        Caption = 'Label4'
      end
      object Label5: TLabel
        Left = 8
        Top = 48
        Width = 32
        Height = 13
        Caption = 'Label5'
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Other'
      ImageIndex = 2
      ExplicitLeft = 6
      ExplicitWidth = 148
      object Label1: TLabel
        Left = 24
        Top = 40
        Width = 32
        Height = 13
        Caption = 'Label1'
      end
      object Label6: TLabel
        Left = 24
        Top = 64
        Width = 32
        Height = 13
        Caption = 'Label6'
      end
      object Label7: TLabel
        Left = 24
        Top = 80
        Width = 32
        Height = 13
        Caption = 'Label7'
      end
      object Label8: TLabel
        Left = 24
        Top = 176
        Width = 32
        Height = 13
        Caption = 'Label8'
      end
      object Label9: TLabel
        Left = 24
        Top = 112
        Width = 32
        Height = 13
        Caption = 'Label9'
      end
      object Label10: TLabel
        Left = 24
        Top = 136
        Width = 38
        Height = 13
        Caption = 'Label10'
      end
      object Label11: TLabel
        Left = 24
        Top = 192
        Width = 38
        Height = 13
        Caption = 'Label11'
      end
      object Label12: TLabel
        Left = 24
        Top = 208
        Width = 38
        Height = 13
        Caption = 'Label12'
      end
      object Label13: TLabel
        Left = 24
        Top = 224
        Width = 38
        Height = 13
        Caption = 'Label13'
      end
      object Label14: TLabel
        Left = 24
        Top = 240
        Width = 38
        Height = 13
        Caption = 'Label14'
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'TabSheet4'
      ImageIndex = 3
      ExplicitWidth = 313
      ExplicitHeight = 269
      object Label15: TLabel
        Left = 16
        Top = 48
        Width = 38
        Height = 13
        Caption = 'Label15'
      end
      object Label16: TLabel
        Left = 16
        Top = 64
        Width = 38
        Height = 13
        Caption = 'Label16'
      end
      object Label17: TLabel
        Left = 16
        Top = 80
        Width = 38
        Height = 13
        Caption = 'Label17'
      end
      object Label18: TLabel
        Left = 16
        Top = 96
        Width = 38
        Height = 13
        Caption = 'Label18'
      end
      object Label19: TLabel
        Left = 16
        Top = 112
        Width = 38
        Height = 13
        Caption = 'Label19'
      end
      object Label20: TLabel
        Left = 16
        Top = 128
        Width = 38
        Height = 13
        Caption = 'Label20'
      end
      object Label21: TLabel
        Left = 16
        Top = 144
        Width = 38
        Height = 13
        Caption = 'Label21'
      end
      object Label22: TLabel
        Left = 16
        Top = 160
        Width = 38
        Height = 13
        Caption = 'Label22'
      end
      object Label23: TLabel
        Left = 16
        Top = 176
        Width = 38
        Height = 13
        Caption = 'Label23'
      end
      object Label24: TLabel
        Left = 16
        Top = 208
        Width = 38
        Height = 13
        Caption = 'Label24'
      end
      object Label3: TLabel
        Left = 16
        Top = 16
        Width = 32
        Height = 13
        Caption = 'Label3'
      end
    end
  end
  object StringGrid1: TStringGrid
    Left = 392
    Top = 112
    Width = 137
    Height = 41
    ColCount = 15
    RowCount = 15
    TabOrder = 3
    Visible = False
    ColWidths = (
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64)
    RowHeights = (
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24)
  end
  object GLScene: TGLScene
    Left = 48
    Top = 40
    object World: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLC: TGLCube
        Position.Coordinates = {00003CC200000000000001C30000803F}
        Visible = False
        CubeSize = {000000400000004000000040}
      end
      object Sky: TGLEarthSkyDome
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Bands = <>
        Stars = <>
        SunElevation = 75.000000000000000000
        Turbidity = 15.000000000000000000
        ExtendedOptions = []
        Slices = 48
        Stacks = 24
      end
      object Map: TGLFreeForm
        Material.BlendingMode = bmAlphaTest100
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Direction.Coordinates = {BAF46E33000C5F1B0000803F00000000}
        Up.Coordinates = {82DD7E260000803F31D003A600000000}
        BehavioursData = {
          0458434F4C02010201060C54474C444345537461746963020102001200000000
          0200060D474C4443454D616E61676572310202020009090F000020410F000000
          00020008}
      end
      object Wall: TGLFreeForm
        Tag = 20
        BehavioursData = {
          0458434F4C02010201060C54474C444345537461746963020102001200000000
          0200060D474C4443454D616E61676572310202020009090F0000803F0F000000
          00020008}
      end
      object VV: TGLSphere
        Radius = 0.500000000000000000
      end
      object GLDummyCube2: TGLDummyCube
        CubeSize = 1.000000000000000000
        object VP1: TGLSphere
          Position.Coordinates = {0000B8C1000000000000E8C20000803F}
          Radius = 0.500000000000000000
        end
        object VP2: TGLSphere
          Position.Coordinates = {000098C1000000000000FAC20000803F}
          Radius = 0.500000000000000000
        end
        object VP3: TGLSphere
          Position.Coordinates = {000008C200000000000008C30000803F}
          Radius = 0.500000000000000000
        end
        object VP4: TGLSphere
          Position.Coordinates = {000050C200000000000008C30000803F}
          Radius = 0.500000000000000000
        end
        object VP5: TGLSphere
          Position.Coordinates = {00006CC2000000000000F2C20000803F}
          Radius = 0.500000000000000000
        end
        object VP6: TGLSphere
          Position.Coordinates = {000090C2000000000000FAC20000803F}
          Radius = 0.500000000000000000
        end
        object VP7: TGLSphere
          Position.Coordinates = {000090C2000000000000EAC20000803F}
          Radius = 0.500000000000000000
        end
        object VP8: TGLSphere
          Position.Coordinates = {000054C2000000000000DAC20000803F}
          Radius = 0.500000000000000000
        end
        object VP9: TGLSphere
          Position.Coordinates = {000014C2000000000000F4C20000803F}
          Radius = 0.500000000000000000
        end
        object VP10: TGLSphere
          Position.Coordinates = {000070C200000000000001C30000803F}
          Radius = 0.500000000000000000
        end
        object VP11: TGLSphere
          Position.Coordinates = {00006CC2000000000000EAC20000803F}
          Radius = 0.500000000000000000
        end
        object VP12: TGLSphere
          Position.Coordinates = {00005CC2000000000000EAC20000803F}
          Radius = 0.500000000000000000
        end
        object VP13: TGLSphere
          Position.Coordinates = {00003CC200000000000002C30000803F}
          Radius = 0.500000000000000000
        end
        object VP14: TGLSphere
          Position.Coordinates = {000010C200000000000002C30000803F}
          Radius = 0.500000000000000000
        end
      end
      object Sphere: TGLSphere
        Tag = 20
        Material.FrontProperties.Diffuse.Color = {EEED6D3F8B8A8A3EA1A0A03D00000000}
        Material.FrontProperties.Emission.Color = {000000000000000000000000B4C8B63E}
        Material.BlendingMode = bmTransparency
        Material.MaterialOptions = [moIgnoreFog, moNoLighting]
        Position.Coordinates = {000048420000C842000048C20000803F}
        Radius = 0.800000011920928900
        Stacks = 15
        BehavioursData = {
          0458434F4C02010201060D54474C44434544796E616D69630201020012000000
          000200060D474C4443454D616E616765723102050909090F0000803F0F000000
          0002050200020008}
        object GLLines1: TGLLines
          LineColor.Color = {0000803F0000803E000000000000803F}
          LineWidth = 10.000000000000000000
          Nodes = <
            item
            end
            item
            end>
          NodeSize = 5.000000000000000000
          SplineMode = lsmCubicSpline
          Options = []
        end
        object Actor: TGLActor
          Material.Texture.Disabled = False
          Direction.Coordinates = {000000000000803F0000000000000000}
          Up.Coordinates = {0000803F000000800000000000000000}
          Interval = 100
        end
      end
      object CameraCube: TGLDummyCube
        CubeSize = 1.000000000000000000
        object Camera: TGLCamera
          DepthOfView = 10000.000000000000000000
          FocalLength = 50.000000000000000000
          TargetObject = CameraCube
          Position.Coordinates = {000000000000C8410000C8410000803F}
          object Light: TGLLightSource
            ConstAttenuation = 1.000000000000000000
            SpotCutOff = 180.000000000000000000
          end
        end
      end
      object TargetPoint: TGLSphere
        Material.FrontProperties.Ambient.Color = {00000000ADAC2C3F000000000000803F}
        Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3FCDCCCC3E}
        Material.BlendingMode = bmTransparency
        Radius = 0.500000000000000000
      end
      object VPstart: TGLSphere
        Radius = 0.500000000000000000
      end
      object S1: TGLSphere
        Radius = 0.500000000000000000
        BehavioursData = {
          0458434F4C02010201060D54474C44434544796E616D69630201020012000000
          000200060D474C4443454D616E616765723102000809080F000000400F000000
          0002050200020008}
      end
    end
  end
  object Cadencer: TGLCadencer
    Scene = GLScene
    OnProgress = CadencerProgress
    Left = 80
    Top = 40
  end
  object GLDCEManager1: TGLDCEManager
    Gravity = -100.000000000000000000
    WorldScale = 1.000000000000000000
    MovimentScale = 1.000000000000000000
    StandardiseLayers = ccsHybrid
    ManualStep = False
    Left = 112
    Top = 40
  end
end
