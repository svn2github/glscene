object Form1: TForm1
  Left = 193
  Top = 135
  Width = 629
  Height = 403
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 621
    Height = 354
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
    OnClick = GLSceneViewer1Click
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180
    end
    object Teapot1: TTeapot
      Direction.Coordinates = {EE83843E00000000EA46773F00000000}
      Scale.Coordinates = {0000A0400000A0400000A04000000000}
      Material.FrontProperties.Diffuse.Color = {ADAC2C3FAAA9293FF0EF6F3F0000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
    end
    object HUDText1: THUDText
      Position.Coordinates = {0000A0410000C841000000000000803F}
      BitmapFont = WindowsBitmapFont1
      Text = 'Hello World'
      Alignment = taLeftJustify
      Layout = tlTop
    end
    object HUDText2: THUDText
      Position.Coordinates = {0000164300009643000000000000803F}
      BitmapFont = WindowsBitmapFont1
      Text = 'Spin'
      Alignment = taCenter
      Layout = tlCenter
    end
    object HUDText3: THUDText
      Position.Coordinates = {0000FA4300009643000000000000803F}
      BitmapFont = WindowsBitmapFont1
      Text = 'Scale'
      Alignment = taCenter
      Layout = tlCenter
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = Teapot1
      Position.Coordinates = {0000A04000004040000080400000803F}
      Left = 240
      Top = 152
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 48
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 80
  end
  object WindowsBitmapFont1: TWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Left = 48
    Top = 16
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 48
    object MIPickFont: TMenuItem
      Caption = 'Pick Font'
      OnClick = MIPickFontClick
    end
    object MIViewTexture: TMenuItem
      Caption = 'View Texture'
      OnClick = MIViewTextureClick
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 112
    Top = 16
  end
end
