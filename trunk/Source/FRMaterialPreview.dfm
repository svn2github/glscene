object RMaterialPreview: TRMaterialPreview
  Left = 0
  Top = 0
  Width = 202
  Height = 229
  AutoSize = True
  TabOrder = 0
  object CBObject: TComboBox
    Left = 0
    Top = 0
    Width = 60
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = CBObjectChange
    Items.Strings = (
      'Cube'
      'Sphere'
      'Cone'
      'Teapot')
  end
  object CBBackground: TComboBox
    Left = 60
    Top = 0
    Width = 142
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = CBBackgroundChange
    Items.Strings = (
      'on a pattern background'
      'on a white background'
      'on a black background'
      'on a blue background'
      'on a red background'
      'on a green background')
  end
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 26
    Width = 202
    Height = 203
    Camera = Camera
    FieldOfView = 71.615516662597660000
    TabOrder = 2
  end
  object GLScene: TGLScene
    ObjectsSorting = osNone
    Left = 160
    Top = 32
    object BackGroundSprite: TGLHUDSprite
      Material.MaterialOptions = [moNoLighting]
      Material.Texture.Image.Picture.Data = {
        07544269746D617076080000424D760800000000000076000000280000004000
        0000400000000100040000000000000800000000000000000000100000000000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555CCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBAAAAAAAAAAAAAAAA555555555555
        5555EEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDDEEEEEEEEEEEEEEEE33333333333333332222222222222222DDDDDDDDDDDD
        DDDD4444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        11114444444444444444CCCCCCCCCCCCCCCC9999999999999999111111111111
        1111000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF000000000000000088888888888888887777777777777777FFFFFFFFFFFF
        FFFF}
      Material.Texture.Disabled = False
      Rotation = 0.000000000000000000
    end
    object World: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Cube: TGLCube
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'LibMaterial'
        Direction.Coordinates = {FCFAF0B1D8B35D3FFEFFFF3E00000000}
        Up.Coordinates = {D7B35DBFFFFF7F3ED7B3DDBE00000000}
      end
      object Sphere: TGLSphere
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'LibMaterial'
        Radius = 0.800000011920928900
      end
      object Cone: TGLCone
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'LibMaterial'
        BottomRadius = 0.500000000000000000
        Height = 1.000000000000000000
      end
      object Teapot: TGLTeapot
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'LibMaterial'
        Scale.Coordinates = {00000040000000400000004000000000}
      end
    end
    object Light: TGLDummyCube
      Position.Coordinates = {0000000000004040000020410000803F}
      CubeSize = 1.000000000000000000
      object LightSource: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
      object FireSphere: TGLSphere
        Material.BackProperties.Shininess = 47
        Material.FrontProperties.Ambient.Color = {A3A2223FCDCC4C3ECDCC4C3E0000803F}
        Material.FrontProperties.Emission.Color = {D3D2523FA1A0203F000000000000803F}
        Radius = 0.300000011920929000
      end
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 140.000000000000000000
      TargetObject = Cube
      Position.Coordinates = {0000000000000000000020410000803F}
    end
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
      end>
    Left = 128
    Top = 32
  end
end
