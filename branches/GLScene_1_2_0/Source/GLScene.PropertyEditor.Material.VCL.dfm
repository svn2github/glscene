object MaterialEditorForm: TMaterialEditorForm
  Left = 143
  Top = 100
  BorderStyle = bsDialog
  Caption = 'Material Editor'
  ClientHeight = 289
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 261
    Width = 71
    Height = 13
    Caption = 'Blending Mode'
  end
  object Label2: TLabel
    Left = 8
    Top = 231
    Width = 68
    Height = 13
    Caption = 'Polygon Mode'
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 8
    Width = 313
    Height = 217
    ActivePage = TSTexture
    Style = tsButtons
    TabOrder = 0
    object TSFront: TTabSheet
      Caption = 'Front'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TSBack: TTabSheet
      Caption = 'Back'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TSTexture: TTabSheet
      Caption = 'Texture'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
  object GroupBox1: TGroupBox
    Left = 320
    Top = 8
    Width = 233
    Height = 241
    Caption = 'Material Preview'
    TabOrder = 1
  end
  object BBOk: TBitBtn
    Left = 376
    Top = 256
    Width = 83
    Height = 25
    Caption = 'OK'
    Default = True
    DoubleBuffered = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    ModalResult = 1
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object BBCancel: TBitBtn
    Left = 472
    Top = 256
    Width = 83
    Height = 25
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object CBBlending: TComboBox
    Left = 88
    Top = 258
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 4
    OnChange = OnMaterialChanged
  end
  object CBPolygonMode: TComboBox
    Left = 88
    Top = 227
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 5
    OnChange = OnMaterialChanged
  end
end
