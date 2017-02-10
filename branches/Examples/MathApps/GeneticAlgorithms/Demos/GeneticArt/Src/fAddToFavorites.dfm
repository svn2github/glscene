object frmAddToFavorites: TfrmAddToFavorites
  Left = 364
  Top = 147
  BorderStyle = bsDialog
  Caption = 'Add To Favorites'
  ClientHeight = 127
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  DesignSize = (
    294
    127)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 46
    Height = 13
    Caption = 'Collection'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object cbo_Collection: TComboBox
    Left = 8
    Top = 24
    Width = 278
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'cbo_Collection'
  end
  object Edit_Name: TEdit
    Left = 8
    Top = 64
    Width = 276
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object Button1: TButton
    Left = 72
    Top = 96
    Width = 75
    Height = 21
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object Button_Cancel: TButton
    Left = 152
    Top = 96
    Width = 75
    Height = 21
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
