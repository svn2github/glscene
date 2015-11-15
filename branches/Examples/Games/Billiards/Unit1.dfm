object Form1: TForm1
  Left = 198
  Top = 153
  Caption = 'Billiards'
  ClientHeight = 601
  ClientWidth = 749
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyPress = FormKeyPress
  DesignSize = (
    749
    601)
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 673
    Height = 570
    Camera = GLCamera1
    FieldOfView = 160.098739624023400000
    Anchors = [akLeft, akTop, akBottom]
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object CBspot1: TCheckBox
    Left = 680
    Top = 32
    Width = 65
    Height = 17
    Caption = 'Spot 1'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CBspot1Click
  end
  object CBspot2: TCheckBox
    Left = 680
    Top = 64
    Width = 65
    Height = 17
    Caption = 'Spot 2'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = CBspot2Click
  end
  object BtnJouer: TButton
    Left = 680
    Top = 120
    Width = 65
    Height = 25
    Caption = '&Jouer'
    TabOrder = 3
    OnClick = BtnJouerClick
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {0000803E0000803E0000803E0000803F}
      Position.Coordinates = {0000000000000041000000000000803F}
      Shining = False
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Socle: TGLCube
        Material.FrontProperties.Diffuse.Color = {E4E3633FBFBE3E3FF3F2F23E0000803F}
        Material.FrontProperties.Emission.Color = {8584043FC3C2C23E000000000000803F}
        Position.Coordinates = {000000009A99993E000000000000803F}
        CubeSize = {00002040CDCCCC3E0000C03F}
        object Pied1: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {DDDC5C3FBFBE3E3FF3F2F23E0000803F}
          Material.FrontProperties.Emission.Color = {8584043FC3C2C23E000000000000803F}
          Position.Coordinates = {CDCC8C3FCDCCCCBE9A99193F0000803F}
          BottomRadius = 0.100000001490116100
          Height = 0.800000011920928900
          Slices = 64
          TopRadius = 0.100000001490116100
        end
        object pied2: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {DDDC5C3FBFBE3E3FF3F2F23E0000803F}
          Material.FrontProperties.Emission.Color = {8584043FC3C2C23E000000000000803F}
          Position.Coordinates = {CDCC8CBFCDCCCCBE9A99193F0000803F}
          BottomRadius = 0.100000001490116100
          Height = 0.800000011920928900
          Slices = 64
          TopRadius = 0.100000001490116100
        end
        object pied3: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {DDDC5C3FBFBE3E3FF3F2F23E0000803F}
          Material.FrontProperties.Emission.Color = {8584043FC3C2C23E000000000000803F}
          Position.Coordinates = {CDCC8CBFCDCCCCBE9A9919BF0000803F}
          BottomRadius = 0.100000001490116100
          Height = 0.800000011920928900
          Slices = 64
          TopRadius = 0.100000001490116100
        end
        object pied4: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {DDDC5C3FBFBE3E3FF3F2F23E0000803F}
          Material.FrontProperties.Emission.Color = {8584043FC3C2C23E000000000000803F}
          Position.Coordinates = {CDCC8C3FCDCCCCBE9A9919BF0000803F}
          BottomRadius = 0.100000001490116100
          Height = 0.800000011920928900
          Slices = 64
          TopRadius = 0.100000001490116100
        end
        object bord1: TGLCube
          Material.FrontProperties.Emission.Color = {DDDC5C3E9998183E000000000000803F}
          Position.Coordinates = {0000C0BFCDCC4C3E000000000000803F}
          CubeSize = {CDCCCC3DCDCC4C3E66660640}
        end
        object bord2: TGLCube
          Material.FrontProperties.Emission.Color = {DDDC5C3E9998183E000000000000803F}
          Position.Coordinates = {0000C03FCDCC4C3E000000000000803F}
          CubeSize = {CDCCCC3DCDCC4C3E66660640}
        end
        object bord3: TGLCube
          Material.FrontProperties.Emission.Color = {DDDC5C3E9998183E000000000000803F}
          Direction.Coordinates = {0000803F000000002EBD3BB300000000}
          Position.Coordinates = {00000000CDCC4C3E0000803F0000803F}
          CubeSize = {CDCCCC3DCDCC4C3E66664640}
        end
        object bord4: TGLCube
          Material.FrontProperties.Emission.Color = {DDDC5C3E9998183E000000000000803F}
          Direction.Coordinates = {0000803F000000002EBD3BB300000000}
          Position.Coordinates = {00000000CDCC4C3E000080BF0000803F}
          CubeSize = {CDCCCC3DCDCC4C3E66664640}
        end
      end
      object Plateau: TGLCube
        Material.FrontProperties.Emission.Color = {EDEC6C3E9F9E1E3FB5B4343E0000803F}
        Position.Coordinates = {000000000000003F000000000000803F}
        CubeSize = {00004040CDCC4C3D00000040}
      end
      object lampe1: TGLFrustrum
        Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F3333533F}
        Material.FrontProperties.Emission.Color = {00000000A9A8283E000000000000803F}
        Material.BlendingMode = bmTransparency
        Position.Coordinates = {9A99193F0000C03F000000000000803F}
        NormalDirection = ndInside
        FrustrumSize = {9A99193F9A99193F9A99193FCDCC4C3E}
        object lum1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          Diffuse.Color = {CDCCCC3ECDCCCC3ECDCCCC3E0000803F}
          SpotCutOff = 180.000000000000000000
          object amp1: TGLSphere
            Material.FrontProperties.Emission.Color = {E4E3633FDCDB5B3F8786063F0000803F}
            Radius = 0.050000000745058060
          end
        end
      end
      object lampe2: TGLFrustrum
        Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F3333533F}
        Material.FrontProperties.Emission.Color = {00000000A9A8283E000000000000803F}
        Material.BlendingMode = bmTransparency
        Position.Coordinates = {9A9919BF0000C03F000000000000803F}
        NormalDirection = ndInside
        FrustrumSize = {9A99193F9A99193F9A99193FCDCC4C3E}
        object lum2: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          Diffuse.Color = {CDCCCC3ECDCCCC3ECDCCCC3E0000803F}
          SpotCutOff = 180.000000000000000000
          object amp2: TGLSphere
            Material.FrontProperties.Emission.Color = {E4E3633FDCDB5B3F8786063F0000803F}
            Radius = 0.050000000745058060
          end
        end
      end
      object B1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {F7F6F63EA1A0203FCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {E4E3633F00000000000000000000803F}
        Position.Coordinates = {00000000D7A3103F000000000000803F}
        Radius = 0.039999999105930330
        Slices = 64
      end
      object B2: TGLSphere
        Material.FrontProperties.Diffuse.Color = {F7F6F63EA1A0203FCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {0000000000000000B7B6363F0000803F}
        Position.Coordinates = {0000803FD7A3103FCDCCCC3D0000803F}
        Radius = 0.039999999105930330
        Slices = 64
      end
      object B3: TGLSphere
        Material.FrontProperties.Diffuse.Color = {F7F6F63EA1A0203FCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {C5C4443FC3C2423F000000000000803F}
        Position.Coordinates = {0000803FD7A3103F0000003F0000803F}
        Radius = 0.039999999105930330
        Slices = 64
      end
      object queue: TGLCylinder
        Direction.Coordinates = {9E067F322D8D0E260000803F00000000}
        Position.Coordinates = {52B8DE3FAE47213FCDCCCC3D0000803F}
        Up.Coordinates = {5B607FBF81DC8EBD96677E3200000000}
        BottomRadius = 0.014999999664723870
        Height = 1.399999976158142000
        Slices = 64
        TopRadius = 0.014999999664723870
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000004000000040000040400000803F}
    end
  end
end
