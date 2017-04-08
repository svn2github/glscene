object AnnotationForm: TAnnotationForm
  Left = 320
  Top = 140
  Width = 224
  Height = 101
  Hint = 'Type text and then mouse location'
  HelpContext = 333
  Caption = 'Annotation Text'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000BBBBBBB70000000BBB000BB7000000BBBBB0BBB700000BBBBBB0BBB70000
    BBBBBBB0BBB700000BBBB0B0B0B7000000BBB00000B70000000BBBBBBBB70000
    0000BBBBBBB7000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000FFFF0000FF000000FE000000FC000000FC00000030000000C000
    0000F0000000F8000000FC000000FE000000FF000000FFFF0000FFFF0000}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 32
    Width = 156
    Height = 13
    Caption = 'then click the image to place text'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 115
    Height = 13
    Caption = 'or... press Cancel button'
  end
  object AnnoEdit: TEdit
    Left = 8
    Top = 8
    Width = 201
    Height = 21
    TabOrder = 0
    Text = 'Mouse into and type here'
  end
  object ATextCancelBtn: TBitBtn
    Left = 136
    Top = 48
    Width = 75
    Height = 25
    TabOrder = 1
    OnClick = ATextCancelBtnClick
    Kind = bkCancel
  end
end
