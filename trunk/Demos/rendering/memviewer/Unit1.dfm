object Form1: TForm1
  Left = 173
  Top = 99
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 307
  ClientWidth = 479
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 392
    Top = 8
    Width = 60
    Height = 19
    Caption = 'Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 384
    Top = 64
    Width = 87
    Height = 14
    Caption = 'Texture framerate'
  end
  object CheckBox1: TCheckBox
    Left = 384
    Top = 200
    Width = 57
    Height = 17
    Caption = 'VSync'
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 8
    Width = 369
    Height = 289
    Camera = GLCamera1
    AfterRender = GLSceneViewer1AfterRender
    Buffer.BackgroundColor = clGray
  end
  object RB1to1: TRadioButton
    Tag = 1
    Left = 400
    Top = 88
    Width = 41
    Height = 17
    Caption = '1:1'
    Checked = True
    TabOrder = 2
    TabStop = True
    OnClick = RB1to1Click
  end
  object RB1to2: TRadioButton
    Tag = 2
    Left = 400
    Top = 112
    Width = 41
    Height = 17
    Caption = '1:2'
    TabOrder = 3
    OnClick = RB1to1Click
  end
  object RB1to10: TRadioButton
    Tag = 10
    Left = 400
    Top = 136
    Width = 41
    Height = 17
    Caption = '1:10'
    TabOrder = 4
    OnClick = RB1to1Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 144
    Top = 16
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object DummyCube1: TGLDummyCube
      CubeSize = 1
      object Cube1: TGLCube
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 150
      TargetObject = DummyCube1
      Position.Coordinates = {0000A040000080400000A0400000803F}
    end
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Camera = GLCamera1
    Buffer.BackgroundColor = clRed
    Left = 72
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 192
    Top = 16
  end
end
