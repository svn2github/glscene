object Form1: TForm1
  Left = 201
  Top = 119
  Width = 505
  Height = 356
  Caption = 'Form1'
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
    Width = 489
    Height = 3
    Align = alTop
    Shape = bsTopLine
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 3
    Width = 489
    Height = 276
    Camera = GLCamera1
    AfterRender = GLSceneViewer1AfterRender
    Buffer.DepthTest = False
    FieldOfView = 140.167190551757800000
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 279
    Width = 489
    Height = 19
    Panels = <>
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 8
    Top = 16
    object HSBitmap: TGLHUDSprite
      Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
      Material.Texture.Disabled = False
      Position.Coordinates = {0000484200004842000000000000803F}
      Width = 256.000000000000000000
      Height = 256.000000000000000000
    end
    object GLParticles1: TGLParticles
      CubeSize = 1.000000000000000000
      OnActivateParticle = GLParticles1ActivateParticle
      object HSParticle: TGLHUDSprite
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'cursor'
        OnProgress = HSParticleProgress
      end
    end
    object HSCursor: TGLHUDSprite
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'cursor'
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
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.ImageAlpha = tiaTopLeftPointColorTransparent
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Tag = 0
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
