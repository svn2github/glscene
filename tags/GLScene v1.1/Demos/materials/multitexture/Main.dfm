object Form1: TForm1
  Left = 144
  Top = 81
  BorderStyle = bsSingle
  Caption = 'Multitexture'
  ClientHeight = 409
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 416
    Top = 56
    Width = 128
    Height = 128
    Stretch = True
    OnClick = Image1Click
  end
  object Image2: TImage
    Left = 416
    Top = 208
    Width = 128
    Height = 128
    Stretch = True
    OnClick = Image2Click
  end
  object Label1: TLabel
    Left = 416
    Top = 40
    Width = 69
    Height = 13
    Caption = 'Texture Map 1'
  end
  object Label2: TLabel
    Left = 416
    Top = 192
    Width = 69
    Height = 13
    Caption = 'Texture Map 2'
  end
  object Label3: TLabel
    Left = 416
    Top = 368
    Width = 60
    Height = 13
    Caption = 'Map 2 Scale'
  end
  object Label4: TLabel
    Left = 432
    Top = 8
    Width = 94
    Height = 19
    Caption = 'MultiTexture'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 8
    Width = 393
    Height = 393
    Camera = GLCamera1
    Enabled = False
  end
  object TrackBar1: TTrackBar
    Left = 416
    Top = 384
    Width = 126
    Height = 25
    Max = 30
    Min = 5
    Orientation = trHorizontal
    Frequency = 1
    Position = 10
    SelEnd = 0
    SelStart = 0
    TabOrder = 1
    ThumbLength = 10
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object CBClampTex2: TCheckBox
    Left = 416
    Top = 344
    Width = 97
    Height = 17
    Caption = 'Clamp Texture 2'
    TabOrder = 2
    OnClick = CBClampTex2Click
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 32
    object Plane1: TGLPlane
      Position.Coordinates = {0000000000000000000080BF0000803F}
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'Base'
      Height = 1
      Width = 1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      Left = 192
      Top = 192
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Base'
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
        Tag = 0
        Texture2Name = 'Second'
      end
      item
        Name = 'Second'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Tag = 0
      end>
    Left = 48
    Top = 64
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 480
    Top = 8
  end
end
