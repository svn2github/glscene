object ATextureCombinerForm: TATextureCombinerForm
  Left = 228
  Top = -1
  Caption = 
    'Texture Combiner : Left click to load  image: Right click to Sav' +
    'e Combined'
  ClientHeight = 530
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000777
    7777777777777777777777777777FBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF7BFBF
    BFBFBFBFBFBFBFBFBFBFBFBFBFB7FBFBFBFB7777777BFBF7777777FBFBF7BFBF
    BFB00000007FBF00000007BFBFB7FBFBFBF0D515107BFB0D515107FBFBF7BFBF
    BFB0D151507FBF0D151507BFBFB7FBFBFBF0D515107BFB0D515107FBFBF7BFBF
    BFB099D9D0BFBF099D9D0FBFBFB7FBFBFBF0000000FBFB0000000BFBFBF7BFBF
    BFBFB0BFBFBFBFBFB0BFBFBFBFB7FBFBFBFBFB0B0B0B0B0B0BFBFBFBFBF7BFBF
    BFBFBFBFBFB0BFBFBFBFBFBFBFB7FB7777777BFBF7707777FBFB77777777B000
    00007FBF00000007BFB000000077F0D515107BFB0D515107FBF0D5151077B0D1
    51507FBF0D151507BFB0D1515077F0D515107BFB0D515107FBF0D5151077B099
    D9D0BFBF099D9D0FBFB099D9D0B7F0000000FBFB0000000BFBF0000000F7BFBF
    B0BFBFBFBFB0BFBFBFBFB0BFBFB7FBFBFB0B0B0B0B0B0B0B0B0B0BFBFBF7BFBF
    BFBFB0BFBFBFBFBFBFBFBFBFBFB7FBFBFB77707777777BFBFBFBFBFBFBF7BFBF
    0000000000007FBFBFBFBFBFBFB7FBFB0D51515151507BFBFBFBFBFBFBF7BFBF
    0D15151515107FBFBFBFBFBFBFB7FBFB0D51515151507BFBFBFBFBFBFBF7BFBF
    09D9D9D9D9D0BFBFBFBFBFBFBFB7FBFB000000000000FBFBFBFBFBFBFBF0BFBF
    BFBFBFBFBFBFBFBFBFBFBFBFBFB0000000000000000000000000000000008000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000100000001FFFFFFFF}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 514
    Height = 530
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = '16 (128x128) into 1 (512x512)'
      PopupMenu = PopupMenu1
      object Image16: TImage
        Left = 384
        Top = 384
        Width = 128
        Height = 128
        OnClick = Image16Click
      end
      object Image15: TImage
        Left = 256
        Top = 384
        Width = 128
        Height = 128
        OnClick = Image15Click
      end
      object Image14: TImage
        Left = 128
        Top = 384
        Width = 128
        Height = 128
        OnClick = Image14Click
      end
      object Image13: TImage
        Left = 0
        Top = 384
        Width = 128
        Height = 128
        OnClick = Image13Click
      end
      object Image9: TImage
        Left = 0
        Top = 256
        Width = 128
        Height = 128
        OnClick = Image9Click
      end
      object Image10: TImage
        Left = 128
        Top = 256
        Width = 128
        Height = 128
        OnClick = Image10Click
      end
      object Image11: TImage
        Left = 256
        Top = 256
        Width = 128
        Height = 128
        OnClick = Image11Click
      end
      object Image12: TImage
        Left = 384
        Top = 256
        Width = 128
        Height = 128
        OnClick = Image12Click
      end
      object Image8: TImage
        Left = 384
        Top = 128
        Width = 128
        Height = 128
        OnClick = Image8Click
      end
      object Image7: TImage
        Left = 256
        Top = 128
        Width = 128
        Height = 128
        OnClick = Image7Click
      end
      object Image6: TImage
        Left = 128
        Top = 128
        Width = 128
        Height = 128
        OnClick = Image6Click
      end
      object Image5: TImage
        Left = 0
        Top = 128
        Width = 128
        Height = 128
        OnClick = Image5Click
      end
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 128
        Height = 128
        OnClick = Image1Click
      end
      object Image2: TImage
        Left = 128
        Top = 0
        Width = 128
        Height = 128
        OnClick = Image2Click
      end
      object Image3: TImage
        Left = 256
        Top = 0
        Width = 128
        Height = 128
        OnClick = Image3Click
      end
      object Image4: TImage
        Left = 384
        Top = 0
        Width = 128
        Height = 128
        OnClick = Image4Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = '4 (?x?) into 1 (2?x2?)'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label104: TLabel
        Left = 240
        Top = 136
        Width = 23
        Height = 13
        Caption = '1  2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label105: TLabel
        Left = 240
        Top = 152
        Width = 23
        Height = 13
        Caption = '3  4'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object CombineTex1Btn: TSpeedButton
        Left = 168
        Top = 184
        Width = 23
        Height = 22
        Caption = '1'
        OnClick = CombineTex1BtnClick
      end
      object CombineTex2Btn: TSpeedButton
        Left = 168
        Top = 208
        Width = 23
        Height = 22
        Caption = '2'
        OnClick = CombineTex2BtnClick
      end
      object CombineTex3Btn: TSpeedButton
        Left = 168
        Top = 232
        Width = 23
        Height = 22
        Caption = '3'
        OnClick = CombineTex3BtnClick
      end
      object CombineTex4Btn: TSpeedButton
        Left = 168
        Top = 256
        Width = 23
        Height = 22
        Caption = '4'
        OnClick = CombineTex4BtnClick
      end
      object CombineTexALLBtn: TSpeedButton
        Left = 168
        Top = 296
        Width = 153
        Height = 22
        Caption = 'Combine 4 (?x?) into 1 (2?x2?)'
        OnClick = CombineTexALLBtnClick
      end
      object Label1: TLabel
        Left = 128
        Top = 336
        Width = 240
        Height = 52
        Caption = 
          'Used to combine 4 images into 1 file. The 4 images can be Single' +
          ' images  OR  They can be Combined images from the Previous page.' +
          ' This results in 1 image containing 4 or 64 images.'
        WordWrap = True
      end
      object CombineTex1Edit: TEdit
        Left = 200
        Top = 184
        Width = 121
        Height = 21
        TabOrder = 0
      end
      object CombineTex2Edit: TEdit
        Left = 200
        Top = 208
        Width = 121
        Height = 21
        TabOrder = 1
      end
      object CombineTex3Edit: TEdit
        Left = 200
        Top = 232
        Width = 121
        Height = 21
        TabOrder = 2
      end
      object CombineTex4Edit: TEdit
        Left = 200
        Top = 256
        Width = 121
        Height = 21
        TabOrder = 3
      end
      object ImageSizeRG: TRadioGroup
        Left = 152
        Top = 80
        Width = 185
        Height = 49
        Caption = 'Select 4   ___ x ___ image files'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          '128'
          '256'
          '512')
        TabOrder = 4
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 8
    Top = 24
    object Exit1: TMenuItem
      Caption = 'Exit'
      OnClick = Exit1Click
    end
    object Save1: TMenuItem
      Caption = 'Save...'
      OnClick = Save1Click
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 8
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    Left = 8
    Top = 96
  end
  object OpenDialog1: TOpenDialog
    Left = 80
    Top = 24
  end
end
