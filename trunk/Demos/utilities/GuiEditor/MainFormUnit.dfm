object Form1: TForm1
  Left = 140
  Top = 116
  Width = 561
  Height = 391
  Caption = 'Gui Skin Editor and Manager'
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
  object ListBox: TListBox
    Left = 8
    Top = 40
    Width = 233
    Height = 297
    ItemHeight = 13
    PopupMenu = ListPopup
    TabOrder = 0
    OnClick = ListBoxClick
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 248
    Top = 8
    Width = 297
    Height = 329
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
  end
  object Edit3: TEdit
    Left = 8
    Top = 8
    Width = 233
    Height = 21
    TabOrder = 2
    Text = 'Newly Added'
    OnChange = Edit3Change
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 48
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = 'Open'
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = 'Save'
        OnClick = Save1Click
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Import1: TMenuItem
        Caption = 'Import'
        OnClick = Import1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object EditLayout1: TMenuItem
        Caption = 'Edit Layout'
        OnClick = EditLayout1Click
      end
    end
    object Image1: TMenuItem
      Caption = 'Image'
      object Load1: TMenuItem
        Caption = 'Load'
        OnClick = Load1Click
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.layout'
    Filter = 'Layouts|*.layout'
    Title = 'Open Layout'
    Left = 24
    Top = 88
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.layout'
    Filter = 'Layouts|*.layout'
    Title = 'Save Layout'
    Left = 24
    Top = 120
  end
  object GLGuiLayout1: TGLGuiLayout
    BitmapFont = WindowsBitmapFont1
    Material.MaterialLibrary = GLMaterialLibrary1
    Material.LibMaterialName = 'LibMaterial'
    GuiComponents = <>
    Left = 80
    Top = 88
  end
  object ImportDialog: TOpenDialog
    DefaultExt = '*.layout'
    Filter = 'Layouts|*.layout'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 24
    Top = 168
  end
  object ListPopup: TPopupMenu
    Left = 128
    Top = 120
    object Add1: TMenuItem
      Caption = 'Add'
      OnClick = Add1Click
    end
    object Remove1: TMenuItem
      Caption = 'Remove'
      OnClick = Remove1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Edit2: TMenuItem
      Caption = 'Edit'
      OnClick = Edit2Click
    end
  end
  object GLScene1: TGLScene
    Left = 184
    Top = 88
    object HUDSprite1: THUDSprite
      Position.Coordinates = {0000164300001643000000000000803F}
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Width = 200
      Height = 200
      NoZWrite = False
    end
    object GLPanel1: TGLPanel
      Position.Coordinates = {0000484200004842000000000000803F}
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Width = 200
      Height = 200
      NoZWrite = False
      RedrawAtOnce = False
      GuiLayout = GLGuiLayout1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
    end
  end
  object WindowsBitmapFont1: TWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = []
    Ranges = <
      item
        StartASCII = 'A'
        StopASCII = 'Z'
        StartGlyphIdx = 0
      end
      item
        StartASCII = 'a'
        StopASCII = 'z'
        StartGlyphIdx = 0
      end
      item
        StartASCII = '0'
        StopASCII = '9'
        StartGlyphIdx = 26
      end
      item
        StartASCII = ' '
        StopASCII = ' '
        StartGlyphIdx = 36
      end>
    Left = 80
    Top = 48
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 128
    Top = 88
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Material.Texture.Disabled = False
        Tag = 0
      end>
    Left = 184
    Top = 48
  end
end
