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
    Height = 351
    Camera = GLCamera
    Buffer.BackgroundColor = clBlack
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object DCCamera: TGLDummyCube
      Position.Coordinates = {000000400000803F0000803F0000803F}
      CubeSize = 0.5
      object GLCamera: TGLCamera
        DepthOfView = 30
        FocalLength = 50
        NearPlaneBias = 0.100000001490116
        TargetObject = DCCamera
        Position.Coordinates = {CDCCCC3E9A99993ECDCC4C3E0000803F}
      end
    end
    object PLGround: TGLPlane
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Material.MaterialLibrary = GLMaterialLibrary
      Material.LibMaterialName = 'walkway'
      Height = 3
      Width = 3
      XTiles = 3
      YTiles = 3
      NoZWrite = False
      object GLCube1: TGLCube
        Position.Coordinates = {000000BF0000803F0000803E0000803F}
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'rawwall'
        CubeSize = {00000040CDCC4C3E0000003F}
      end
    end
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1
      Position.Coordinates = {0000000000004842000020420000803F}
      SpotCutOff = 180
    end
    object DCPositionInvariant: TGLDummyCube
      CubeSize = 1
      CamInvarianceMode = cimPosition
      object GLCylinder1: TGLCylinder
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'marbletiles'
        NormalDirection = ndInside
        BottomRadius = 6
        Height = 3
        Slices = 24
        TopRadius = 6
        Parts = [cySides]
      end
    end
    object DCOrientationInvariant: TGLDummyCube
      CubeSize = 1
      CamInvarianceMode = cimOrientation
      object GLArrowLine1: TGLArrowLine
        Direction.Coordinates = {00000000000000BFD6B35D3F00000000}
        PitchAngle = -30
        Position.Coordinates = {000000009A99993E3333333F0000803F}
        Scale.Coordinates = {CDCC4C3ECDCC4C3ECDCC4C3E00000000}
        Up.Coordinates = {00000000D7B35D3F0100003F00000000}
        BottomRadius = 0.100000001490116
        Height = 1
        TopRadius = 0.100000001490116
        TopArrowHeadHeight = 0.5
        TopArrowHeadRadius = 0.200000002980232
        BottomArrowHeadHeight = 0.5
        BottomArrowHeadRadius = 0.200000002980232
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
