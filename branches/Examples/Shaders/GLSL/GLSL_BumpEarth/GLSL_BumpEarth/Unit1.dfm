object FBumpEarth: TFBumpEarth
  Left = 265
  Top = 103
  Caption = 'GLSL BumpEarth'
  ClientHeight = 427
  ClientWidth = 668
  Color = clBtnFace
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
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 668
    Height = 427
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 129.804718017578100000
    Align = alClient
    PopupMenu = PopupMenu
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000000000000000000070410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000A0C00000A0C00000A0C00000803F}
      SpotCutOff = 180.000000000000000000
      SpotDirection.Coordinates = {00000000000080BF0000000000000000}
    end
    object DOInitialize: TGLDirectOpenGL
      UseBuildList = False
      OnRender = DOInitializeRender
      Blend = False
    end
    object DORender: TGLDirectOpenGL
      UseBuildList = False
      OnRender = DORenderRender
      Blend = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DORender
      Position.Coordinates = {0000E0C00000A040000000000000803F}
    end
  end
  object MatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'decal'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'earth.jpg'
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twNone
        Material.Texture.Disabled = False
      end
      item
        Name = 'heightmap'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'earth_bump.bmp'
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twNone
        Material.Texture.TextureFormat = tfIntensity
        Material.Texture.Disabled = False
      end
      item
        Name = 'normalmap'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'earth_bump.bmp'
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twNone
        Material.Texture.TextureFormat = tfRGB
        Material.Texture.Disabled = False
        Material.Texture.NormalMapScale = 0.050000000745058060
      end>
    Left = 56
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 112
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 144
    Top = 16
  end
  object PopupMenu: TPopupMenu
    Left = 24
    Top = 104
    object MIDot3: TMenuItem
      AutoCheck = True
      Caption = 'DOT3 BumpMapping'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      OnClick = MIDot3Click
    end
    object MIParallax: TMenuItem
      AutoCheck = True
      Caption = 'Parallax Bump Mapping'
      GroupIndex = 1
      RadioItem = True
      OnClick = MIParallaxClick
    end
    object N1: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object CBWireFrame: TMenuItem
      AutoCheck = True
      Caption = 'Wireframe'
      GroupIndex = 1
    end
  end
end
