object Form1: TForm1
  Left = 201
  Top = 110
  Width = 429
  Height = 380
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
    Width = 421
    Height = 353
    Camera = GLCamera
    Buffer.BackgroundColor = clBlack
    FieldOfView = 148.366622924804700000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object DCCamera: TGLDummyCube
      Position.Coordinates = {000000400000803F0000803F0000803F}
      CubeSize = 0.500000000000000000
      object GLCamera: TGLCamera
        DepthOfView = 30.000000000000000000
        FocalLength = 50.000000000000000000
        NearPlaneBias = 0.100000001490116100
        TargetObject = DCCamera
        Position.Coordinates = {CDCCCC3E9A99993ECDCC4C3E0000803F}
      end
    end
    object PLGround: TGLPlane
      Material.MaterialLibrary = GLMaterialLibrary
      Material.LibMaterialName = 'walkway'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 3.000000000000000000
      Width = 3.000000000000000000
      XTiles = 3
      YTiles = 3
      object GLCube1: TGLCube
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'rawwall'
        Position.Coordinates = {000000BF0000803F0000803E0000803F}
        CubeSize = {00000040CDCC4C3E0000003F}
      end
    end
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000000000004842000020420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DCPositionInvariant: TGLDummyCube
      CubeSize = 1.000000000000000000
      CamInvarianceMode = cimPosition
      object GLCylinder1: TGLCylinder
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'marbletiles'
        NormalDirection = ndInside
        BottomRadius = 6.000000000000000000
        Height = 3.000000000000000000
        Slices = 24
        TopRadius = 6.000000000000000000
        Parts = [cySides]
      end
    end
    object DCOrientationInvariant: TGLDummyCube
      CubeSize = 1.000000000000000000
      CamInvarianceMode = cimOrientation
      object GLArrowLine1: TGLArrowLine
        Direction.Coordinates = {00000000000000BFD6B35D3F00000000}
        PitchAngle = -30.000000000000000000
        Position.Coordinates = {000000009A99993E3333333F0000803F}
        Scale.Coordinates = {CDCC4C3ECDCC4C3ECDCC4C3E00000000}
        Up.Coordinates = {00000000D7B35D3F0100003F00000000}
        BottomRadius = 0.100000001490116100
        Height = 1.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
    end
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'walkway'
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'walkway.jpg'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
      end
      item
        Name = 'rawwall'
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'rawwall.jpg'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
        TextureScale.Coordinates = {000000400000803F0000803F00000000}
      end
      item
        Name = 'marbletiles'
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'marbletiles.jpg'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Tag = 0
        TextureScale.Coordinates = {000040410000803F0000803F00000000}
      end>
    TexturePaths = '..\..\media\'
    Left = 48
    Top = 8
  end
end
