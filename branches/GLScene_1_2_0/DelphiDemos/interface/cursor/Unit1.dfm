object Form1: TForm1
  Left = 201
  Top = 119
  Caption = 'Form1'
  ClientHeight = 310
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 497
    Height = 3
    Align = alTop
    Shape = bsTopLine
    ExplicitWidth = 489
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 3
    Width = 497
    Height = 288
    Camera = GLCamera1
    AfterRender = GLSceneViewer1AfterRender
    Buffer.DepthTest = False
    FieldOfView = 141.703720092773400000
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 291
    Width = 497
    Height = 19
    Panels = <>
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 8
    Top = 16
    object HSBitmap: TGLHUDSprite
      Width = 256.000000000000000000
      Height = 256.000000000000000000
      AlphaChannel = 1.000000000000000000
      Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
      Material.Texture.Disabled = False
      Position.Coordinates = {0000484200004842000000000000803F}
    end
    object GLParticles1: TGLParticles
      CubeSize = 1.000000000000000000
      OnActivateParticle = GLParticles1ActivateParticle
      object HSParticle: TGLHUDSprite
        AlphaChannel = 1.000000000000000000
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'cursor'
        MaterialLibrary = GLMaterialLibrary1
        LibMaterialName = 'cursor'
        OnProgress = HSParticleProgress
      end
    end
    object HSCursor: TGLHUDSprite
      AlphaChannel = 1.000000000000000000
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'cursor'
      MaterialLibrary = GLMaterialLibrary1
      LibMaterialName = 'cursor'
      Position.Coordinates = {0000484200004842000000000000803F}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Left = 232
      Top = 152
    end
  end
  object MainMenu1: TMainMenu
    Left = 40
    Top = 16
    object MIFile: TMenuItem
      Caption = 'File'
      object MILoadImage: TMenuItem
        Caption = 'Load image...'
        OnClick = MILoadImageClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MIExit: TMenuItem
        Caption = 'Exit'
        OnClick = MIExitClick
      end
    end
    object O1: TMenuItem
      Caption = 'Options'
      object MITrail: TMenuItem
        Caption = 'Trail'
        OnClick = MITrailClick
      end
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 72
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 104
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'cursor'
        Tag = 0
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.BlendingMode = bmTransparency
        Material.MaterialOptions = [moIgnoreFog, moNoLighting]
        Material.Texture.ImageAlpha = tiaTopLeftPointColorTransparent
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end>
    Left = 136
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 168
    Top = 16
  end
end
