object EvaluateForm: TEvaluateForm
  Left = 1198
  Top = 362
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 248
  ClientWidth = 320
  Color = clCream
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 5
    Width = 305
    Height = 235
    Caption = '  Evaluate function  '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object Labe1: TLabel
      Left = 19
      Top = 20
      Width = 68
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Enter X value:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 19
      Top = 42
      Width = 68
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Enter Y value:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 17
      Top = 65
      Width = 68
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Z value:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 4
      Top = 203
      Width = 56
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Line width:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object ColorButton: TSpeedButton
      Left = 122
      Top = 196
      Width = 100
      Height = 29
      Caption = 'Line Color...'
      Glyph.Data = {
        F6060000424DF606000000000000360000002800000018000000180000000100
        180000000000C00600000000000000000000000000000000000082C9F77FC8F7
        86CBF78CCEF792D0F898D3F89FD6F8A5D8F9AADBF9B1DDFAB7E0FABCE3FBC2E5
        FBC7E7FBCDE9FCD2ECFCD7EEFCDCF0FDE1F3FDE6F5FDE9F7FEEDF8FEF2F9FEF5
        FBFE70C3F570C2F677C5F67EC8F784CBF78BCDF792D0F898D3F89ED6F9A6D9F9
        ACDBFAB2DEFAB8E1FBBFE3FBC4E6FBCAE9FCCFEBFCD5EDFCDAEFFDDFF2FDE4F4
        FDE8F6FEEDF7FEF2F9FE6DC1F66ABFF571C2F678C5F6FF0000FF0000FF0000FF
        0000FF00009FD6F9FF0000FF0000FF0000B8E1FAFF0000C4E6FBFF0000FF0000
        D5EDFCDAF0FDDFF2FDE4F4FDE8F6FEEEF8FE67BEF563BDF56BC0F571C3F6FF00
        007EC8F785CBF78CCDF892D0F7FF00009FD6F9A6D9F9FF0000B2DEFAFF0000BF
        E4FBFF0000CAE8FCCFEBFCD5EDFCDAF0FDDFF2FDE4F4FDEAF7FE60BBF55EBAF4
        64BDF56BC0F5FF000077C5F67EC8F785CBF78CCEF7FF000098D3F89FD6F9FF00
        00ABDCFAFF0000B9E1FBFF0000C4E6FBCAE9FCCFEBFCD5EDFCDAEFFDDFF2FDE6
        F5FD5BB9F557B7F45EBAF464BDF5FF0000FF0000FF0000FF0000FF0000FF0000
        C0C0C099D4F8FF0000C0C0C0FF0000C0C0C0FF0000BFE3FBC4E6FBCAE9FCD0EA
        FCD5EDFCDAF0FDE2F3FD56B7F451B5F458B8F45EBAF5FF00006BC0F5FF00FFFF
        00FFFF00FFFF00FFFF0000FF0000FF000000FFFFFF0000FF0000FF0000FF0000
        BFE4FBC4E6FBCAE9FCD0EBFCD6EEFCDDF1FD50B5F44AB2F351B5F457B7F4FF00
        00FF00FFFF00FFFF00FFFF00FFFF00FF0000FF0000FFFF000000FFFF00FFFF00
        FFFFFF000000FFFF00FFFFBFE3FBC5E6FBCAE9FCCFEBFCD8EFFC49B2F344B0F3
        4BB2F352B5F4FF0000FF0000FF0000FF0000FF0000FF00FF0000FF0000FFFF00
        000000FFFF000000FFFF00FFFF00FFFF00FFFFB9E1FBBFE4FBC5E6FBCAE9FCD3
        ECFC45AFF33FADF346AFF34CB2F3FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        0000FF0000FF0000FF0000FF00FFFF00FFFF00FFFF00FFFF00FFFFC0C0C0B9E1
        FBBFE4FBC5E6FBCEE9FC3FADF339AAF240ADF346B0F3FF00FFFF00FFFF00FFFF
        00FFFF00FF80008000008000000000000000008000808000FFFF00FFFF00FFFF
        00FFFF00FFFFB3DEFAB9E1FBBFE4FBC8E7FB3AABF234A8F239ABF23FADF2FF00
        FFFF00FFFF00FFFF00FFFF00FF80000000000000000000000000000000800000
        FF0000FFFF00FFFF00FFFFC0C0C0ADDCFAB3DFFABAE1FBC4E6FB36A8F22EA6F1
        34A8F23AAAF240ADF2FF00FFFF00FFFF00FFFF0000FF00000000000000000000
        0000000000FF0000FF0000FFFF00FFFF00FFFFA0D6F9A7D9F9ADDCFAB4DFFABE
        E3FB30A7F229A3F12FA6F134A9F239ABF2FF00FFFF00FFFF0000FF0000FF0000
        80000000000000000000800000FF0000FF0000FF0000FFFF00FFFF99D4F8A0D7
        F9A6D9F9ADDCFAB8E1FA2CA6F124A2F029A4F12FA6F134A8F23AABF2FF00FFFF
        0000FF0000FF0000FF000000000000800000FF0000FF0000FF0000FF0000FFFF
        8DCEF894D1F89AD4F8A1D7F9A7DAF9B3DEFA28A3F11F9FF024A1F129A3F12FA5
        F134A9F239ABF2FFFF00FF0000FF0000FF0000FFFF00FFFF0000FF0000FF0000
        FF00FFFF0080C8F786CCF78DCEF894D1F89AD4F9A1D7F9ACDBF924A1F11A9DF0
        1FA0F024A2F129A4F12FA6F134A8F2FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF0079C6F680C9F786CBF78DCEF893D1F89AD4F8A7
        D9F920A0F0169BF01A9EF00000FF0000FF2AA3F12FA7F1C0C0C00000FF0000FF
        FFFF00FFFF000000FFFFFF00FFFF000000FF0000FF72C3F67AC6F60000FF87CC
        F78DCFF893D1F8A1D7F81C9DF01299EF0000FF1A9EF020A0F00000FF29A4F100
        00FFFFFF00FFFF000000FFFFFF000000FFFFFF000000FFFFFF0066BEF50000FF
        73C3F60000FF80C9F787CCF78DCFF89AD4F8189CF00E98EF0000FF169BF01B9E
        F01F9FF025A1F10000FF2FA6F1FFFF000000FFFFFF000000FFFFFF000000FF59
        B8F45FBBF50000FF6DC1F60000FF79C6F680C9F787CBF794D1F8159BF00A97EF
        0000FF129AEF169CF00000FF1FA0F00000FF2AA4F12FA7F10000FFC0C0C00000
        FF47B0F30000FF53B6F459B8F40000FF66BEF50000FF73C3F67AC6F681C9F78E
        CFF71299EF0695EF0A97EF0000FF0000FF169CF01B9DF01FA0F00000FF0000FF
        2FA6F134A8F20000FF41AEF347B0F30000FF0000FF59B8F45FBBF50000FF0000
        FF0000FF7AC6F688CCF70C97EF0394EE0795EF0A96EF0E98EF1299EF169CF01B
        9DF0209FF025A1F12AA4F12FA7F10000FF3AABF240AEF347B0F34CB3F352B6F4
        59B8F460BBF566BEF56DC1F573C3F680C8F61E9FF01098EF129AEF159BF0189D
        F01C9EF020A1F024A1F128A3F12DA6F132A7F237A9F20000FF41AEF346B0F34C
        B3F452B5F457B7F45DBAF563BCF569BFF66FC2F676C5F683CBF7}
      OnClick = ColorButtonClick
    end
    object Label4: TLabel
      Left = 8
      Top = 87
      Width = 75
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'dz/dx:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Top = 109
      Width = 75
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'dz/dy:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 171
      Top = 172
      Width = 56
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Arrow size:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object EditX: TEdit
      Tag = 1
      Left = 89
      Top = 17
      Width = 200
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnKeyPress = FloatKeyPress
      OnKeyUp = EditXKeyUp
    end
    object EditY: TEdit
      Tag = 2
      Left = 89
      Top = 39
      Width = 200
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnKeyPress = FloatKeyPress
      OnKeyUp = EditYKeyUp
    end
    object EditZ: TEdit
      Left = 89
      Top = 62
      Width = 200
      Height = 21
      Color = clCream
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
    end
    object Coordinates: TCheckBox
      Left = 29
      Top = 133
      Width = 145
      Height = 17
      Caption = 'Show coordinate lines'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      State = cbChecked
      TabOrder = 5
      OnClick = CoordinatesClick
    end
    object ToGrids: TCheckBox
      Left = 29
      Top = 152
      Width = 190
      Height = 17
      Caption = 'Show coordinate lines to grids'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
      OnClick = ToGridsClick
    end
    object EditCoordWidth: TEdit
      Tag = 1
      Left = 64
      Top = 200
      Width = 56
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnKeyPress = IntPress
      OnKeyUp = EditCoordWidthKeyUp
    end
    object Editdzdx: TEdit
      Left = 89
      Top = 84
      Width = 200
      Height = 21
      Hint = 'Right click for Popup menu'
      Color = clCream
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      PopupMenu = PopupMenu1
      ReadOnly = True
      TabOrder = 3
    end
    object Editdzdy: TEdit
      Left = 89
      Top = 106
      Width = 200
      Height = 21
      Hint = 'Right click for Popup menu'
      Color = clCream
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      PopupMenu = PopupMenu1
      ReadOnly = True
      TabOrder = 4
    end
    object dzdx_dzdy: TCheckBox
      Left = 29
      Top = 171
      Width = 135
      Height = 17
      Caption = 'Show dz/dx && dz/dy'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 8
      OnClick = dzdx_dzdyClick
    end
    object EditArrow: TEdit
      Tag = 1
      Left = 231
      Top = 170
      Width = 56
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      OnKeyPress = EditArrowKeyPress
      OnKeyUp = EditArrowKeyUp
    end
  end
  object BitBtn1: TBitBtn
    Left = 232
    Top = 201
    Width = 75
    Height = 29
    Caption = '&Close'
    DoubleBuffered = False
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      040000000000800000000000000000000000100000001000000000000000FFFF
      000000008400FF00FF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00330000000000
      0333330244444444033333022444444403333302224444440333330222044444
      0333330222044444033333022204444403333302220444440333330222044444
      0333330222044444033333022240444403333302220444440333330222044444
      0333330212044444033333021104444403333300000000000333}
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object ColorDialog: TColorDialog
    Options = [cdFullOpen]
    Left = 16
    Top = 59
  end
  object PopupMenu1: TPopupMenu
    Left = 16
    Top = 103
    object Slope1: TMenuItem
      Caption = 'Tangent'
      Checked = True
      OnClick = MenuClick
    end
    object Degree1: TMenuItem
      Tag = 1
      Caption = 'Degree'
      OnClick = MenuClick
    end
    object Radian1: TMenuItem
      Tag = 2
      Caption = 'Radian'
      OnClick = MenuClick
    end
  end
end
