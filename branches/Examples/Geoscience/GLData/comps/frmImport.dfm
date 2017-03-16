object formGEImporter: TformGEImporter
  Left = 347
  Top = 227
  Caption = 'Import'
  ClientHeight = 395
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object dbgImport: TDBGrid
    Left = 0
    Top = 0
    Width = 569
    Height = 314
    Align = alClient
    DataSource = dsImport
    DefaultDrawing = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnDrawColumnCell = dbgImportDrawColumnCell
    OnTitleClick = dbgImportTitleClick
  end
  object pnlSettings: TPanel
    Left = 0
    Top = 333
    Width = 569
    Height = 62
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      569
      62)
    object lblDateFormat: TLabel
      Left = 8
      Top = 37
      Width = 60
      Height = 13
      Caption = 'Date Format'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblHeaderSkip: TLabel
      Left = 178
      Top = 12
      Width = 57
      Height = 13
      Caption = 'Header Skip'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblDelimiter: TLabel
      Left = 164
      Top = 37
      Width = 71
      Height = 13
      Caption = 'Strip Delimiters'
    end
    object ebDateFormat: TEdit
      Left = 74
      Top = 33
      Width = 85
      Height = 21
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object cbxOverwrite: TCheckBox
      Left = 8
      Top = 10
      Width = 121
      Height = 17
      Caption = 'Empty before import'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object bOk: TBitBtn
      Left = 492
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Accept data for import'
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Kind = bkOK
      NumGlyphs = 2
      ParentFont = False
      TabOrder = 2
      OnClick = bOkClick
    end
    object bCancel: TBitBtn
      Left = 492
      Top = 33
      Width = 75
      Height = 25
      Hint = 'Ignore data'
      Anchors = [akTop, akRight]
      Caption = '&Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Kind = bkCancel
      NumGlyphs = 2
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object bClear: TBitBtn
      Left = 414
      Top = 33
      Width = 75
      Height = 25
      Hint = 'Ignore all columns'
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Clear All'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000CE0E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00222222222222
        2222222222222222222222222222222222222800000082222222207777770022
        2222207777770802222228077777088022222207F7F7F088022222207F7F7F08
        8022222207F7F7F088822222207F7F7F080222222207F7F7F802222222207F7F
        7702222222228000088222222222222222222222222222222222}
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = bClearClick
    end
    object cbDelimiter: TComboBox
      Left = 240
      Top = 33
      Width = 65
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      OnChange = cbDelimiterChange
      Items.Strings = (
        ''
        #39
        '"'
        #39'...'#39
        '"..."')
    end
    object cbxGroupDelimiter: TCheckBox
      Left = 314
      Top = 33
      Width = 95
      Height = 25
      Caption = 'Group Delimiters'
      TabOrder = 6
      OnClick = cbxGroupDelimiterClick
    end
    object bReload: TBitBtn
      Left = 414
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Reload data'
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Reload'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Glyph.Data = {
        E6000000424DE60000000000000076000000280000000E0000000E0000000100
        0400000000007000000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DD00DDDDD4444DDDDD00DDD44444444DDD00DD444DDDD444DD00DD44DDDDDD44
        DD00D44DDDDDDDD44D00D44DDDDDDDD44D00D44DDDDDDDD44D00D44DDDDDDDD4
        4D00DD44DDDD4D44DD00DD44DDDD4444DD00DDDDDDDD444DDD00DDDDDDDD4444
        DD00DDDDDDDDDDDDDD00}
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = bReloadClick
    end
    object ebHeaderSkip: TEdit
      Left = 240
      Top = 8
      Width = 65
      Height = 21
      TabOrder = 8
      Text = '0'
      OnExit = ebHeaderSkipExit
    end
    object upHeaderSkip: TUpDown
      Left = 314
      Top = 6
      Width = 16
      Height = 25
      TabOrder = 9
      OnClick = upHeaderSkipClick
    end
  end
  object sBarImport: TStatusBar
    Left = 0
    Top = 314
    Width = 569
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object ActionManager: TActionManager
    Left = 304
    Top = 76
    StyleName = 'XP Style'
  end
  object dsImport: TDataSource
    Left = 204
    Top = 76
  end
end
