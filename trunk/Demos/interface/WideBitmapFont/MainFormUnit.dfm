object MainForm: TMainForm
  Left = 284
  Top = 250
  Width = 870
  Height = 640
  Caption = 'Unicode Bitmap Font'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 862
    Height = 606
    Camera = GLCamera1
    Buffer.BackgroundColor = clNavy
    FieldOfView = 161.259384155273
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 104
    Top = 104
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000C8420000C842000000000000803F}
      Up.Coordinates = {9C0A4F240000803F0000008000000000}
      BitmapFont = GLWideBitmapFont1
      Alignment = taCenter
      Layout = tlCenter
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      Position.Coordinates = {0000000000000000000020410000803F}
    end
  end
  object GLWideBitmapFont1: TGLWideBitmapFont
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    WideCharacters = 'a'
    Left = 136
    Top = 104
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 24
  end
end
