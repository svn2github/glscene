object Form1: TForm1
  Left = 215
  Top = 104
  Width = 532
  Height = 381
  Caption = 'Form1'
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
    Width = 316
    Height = 352
    Camera = GLCamera1
    AfterRender = GLSceneViewer1AfterRender
    Align = alClient
  end
  object Panel1: TPanel
    Left = 316
    Top = 0
    Width = 208
    Height = 352
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 206
      Height = 56
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'TexFormat'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object Label2: TLabel
      Left = 16
      Top = 148
      Width = 32
      Height = 13
      Caption = 'Format'
    end
    object Label3: TLabel
      Left = 16
      Top = 180
      Width = 60
      Height = 13
      Caption = 'Compression'
    end
    object Label4: TLabel
      Left = 16
      Top = 84
      Width = 29
      Height = 13
      Caption = 'Image'
    end
    object LAPicSize: TLabel
      Left = 88
      Top = 112
      Width = 48
      Height = 13
      Caption = 'LAPicSize'
    end
    object Label5: TLabel
      Left = 16
      Top = 272
      Width = 58
      Height = 13
      Caption = 'Render Size'
    end
    object LAUsedMemory: TLabel
      Left = 16
      Top = 228
      Width = 65
      Height = 13
      Caption = 'Used Memory'
    end
    object LARGB32: TLabel
      Left = 16
      Top = 212
      Width = 65
      Height = 13
      Caption = 'Used Memory'
    end
    object LACompression: TLabel
      Left = 16
      Top = 244
      Width = 65
      Height = 13
      Caption = 'Used Memory'
    end
    object CBFormat: TComboBox
      Left = 88
      Top = 144
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = CBImageChange
      Items.Strings = (
        'RGB    (24 bits)'
        'RGBA  (32 bits)'
        'RGB    (16 bits)'
        'RGBA  (16 bits)')
    end
    object CBCompression: TComboBox
      Left = 88
      Top = 176
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = CBImageChange
      Items.Strings = (
        'None'
        'Standard'
        'Nicest'
        'Fastest')
    end
    object CBImage: TComboBox
      Left = 88
      Top = 80
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = CBImageChange
    end
    object RBDefault: TRadioButton
      Left = 88
      Top = 270
      Width = 57
      Height = 17
      Caption = '100 %'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = CBImageChange
    end
    object RBDouble: TRadioButton
      Left = 87
      Top = 294
      Width = 58
      Height = 17
      Caption = '200 %'
      TabOrder = 4
      OnClick = CBImageChange
    end
    object RBQuad: TRadioButton
      Left = 87
      Top = 318
      Width = 58
      Height = 17
      Caption = '400 %'
      TabOrder = 5
      OnClick = CBImageChange
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 16
    object HUDSprite1: TGLHUDSprite
      Material.Texture.MinFilter = miLinear
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Material.Texture.Disabled = False
      Width = 256
      Height = 256
      NoZWrite = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 1
      CameraStyle = csOrthogonal
      Position.Coordinates = {0000000000000000000070410000803F}
      Left = 256
      Top = 160
    end
  end
end
