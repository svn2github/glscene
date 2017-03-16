object formexporter: Tformexporter
  Left = 584
  Top = 494
  Caption = 'Export'
  ClientHeight = 381
  ClientWidth = 559
  Color = clBtnFace
  Constraints.MinHeight = 420
  Constraints.MinWidth = 575
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 294
    Height = 381
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 393
    object Splitter1: TSplitter
      Left = 158
      Top = 1
      Height = 391
      Beveled = True
      Color = 16776176
      ParentColor = False
    end
    object pnlAvailable: TPanel
      Left = 1
      Top = 1
      Width = 157
      Height = 391
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object pnlOrder: TPanel
        Left = 132
        Top = 27
        Width = 25
        Height = 364
        Align = alRight
        BevelOuter = bvNone
        Constraints.MaxWidth = 25
        Constraints.MinWidth = 25
        TabOrder = 0
        object bDown: TSpeedButton
          Left = 2
          Top = 26
          Width = 23
          Height = 22
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Glyph.Data = {
            6E000000424D6E000000000000003E000000280000000C0000000C0000000100
            010000000000300000000000000000000000020000000200000000000000FFFF
            FF00FFF00000FFF00000FFF00000FDF00000F8F00000F0700000E0300000F8F0
            0000F8F00000F8F00000FFF00000FFF00000}
          ParentFont = False
          OnClick = bDownClick
        end
        object bUp: TSpeedButton
          Left = 2
          Top = 2
          Width = 23
          Height = 22
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Glyph.Data = {
            6E000000424D6E000000000000003E000000280000000C0000000C0000000100
            010000000000300000000000000000000000020000000200000000000000FFFF
            FF00FFF00000FFF00000F8F00000F8F00000F8F00000E0300000F0700000F8F0
            0000FDF00000FFF00000FFF00000FFF00000}
          ParentFont = False
          OnClick = bUpClick
        end
        object bRemoveAll: TButton
          Left = 0
          Top = 193
          Width = 25
          Height = 25
          Hint = 'Remove All'
          Caption = '<<'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = bRemoveAllClick
        end
        object bRemove: TButton
          Left = 0
          Top = 164
          Width = 25
          Height = 25
          Hint = 'Remove Selected'
          Caption = '<'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = bRemoveClick
        end
        object bAddAll: TButton
          Left = 0
          Top = 134
          Width = 25
          Height = 25
          Hint = 'Add All'
          Caption = '>>'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = bAddAllClick
        end
        object bAdd: TButton
          Left = 0
          Top = 105
          Width = 25
          Height = 25
          Hint = 'Add Selected'
          Caption = '>'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = bAddClick
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 157
        Height = 27
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object lblAvailableFields: TLabel
          Left = 6
          Top = 8
          Width = 87
          Height = 13
          Caption = 'Available Fields'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object listboxavailable: TListBox
        Left = 0
        Top = 27
        Width = 132
        Height = 364
        Style = lbOwnerDrawFixed
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnDblClick = listboxavailableDblClick
        OnDrawItem = listboxavailableDrawItem
      end
    end
    object pnlExport: TPanel
      Left = 161
      Top = 1
      Width = 132
      Height = 379
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 391
      object listboxfields: TListBox
        Left = 0
        Top = 27
        Width = 132
        Height = 364
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = listboxfieldsClick
        OnDblClick = listboxfieldsDblClick
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 132
        Height = 27
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          Left = 6
          Top = 8
          Width = 72
          Height = 13
          Caption = 'Export Fields'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
    end
  end
  object pnlOptions: TPanel
    Left = 294
    Top = 0
    Width = 265
    Height = 381
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 273
    ExplicitHeight = 393
    object pnlTop: TPanel
      Left = 1
      Top = 1
      Width = 263
      Height = 88
      Align = alTop
      TabOrder = 0
      ExplicitWidth = 271
      object lblExportOptions: TLabel
        Left = 8
        Top = 9
        Width = 43
        Height = 13
        Caption = 'Options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object cbxOutputHeader: TCheckBox
        Left = 8
        Top = 56
        Width = 119
        Height = 17
        Caption = 'Export field names'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object cbxOpenAfterExport: TCheckBox
        Left = 8
        Top = 32
        Width = 125
        Height = 17
        Caption = 'Open file after export'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
    end
    object pnlDelimit: TPanel
      Left = 1
      Top = 89
      Width = 263
      Height = 36
      Align = alTop
      TabOrder = 1
      ExplicitWidth = 271
      object lblDelimiter: TLabel
        Left = 6
        Top = 13
        Width = 67
        Height = 13
        Caption = 'Add delimiters'
      end
      object cbxOnlySTrings: TCheckBox
        Left = 148
        Top = 11
        Width = 113
        Height = 17
        Caption = 'Alphanumeric only'
        TabOrder = 0
      end
      object cbDelimiter: TComboBox
        Left = 78
        Top = 9
        Width = 61
        Height = 21
        Style = csDropDownList
        TabOrder = 1
        Items.Strings = (
          ''
          #39'...'#39
          '"..."')
      end
    end
    object pnlDateFiltering: TPanel
      Left = 1
      Top = 125
      Width = 263
      Height = 122
      Align = alTop
      TabOrder = 2
      ExplicitWidth = 271
      object lblDateFilter: TLabel
        Left = 8
        Top = 8
        Width = 87
        Height = 13
        Caption = 'Set date range on'
      end
      object cbDateTimeField: TComboBox
        Left = 106
        Top = 4
        Width = 161
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnChange = cbDateTimeFieldChange
      end
      object cbxLowerBound: TCheckBox
        Left = 8
        Top = 54
        Width = 97
        Height = 17
        Caption = 'Lower bound'
        TabOrder = 1
        OnClick = cbxLowerBoundClick
      end
      object cbxUpperBound: TCheckBox
        Left = 140
        Top = 54
        Width = 97
        Height = 17
        Caption = 'Upper bound'
        TabOrder = 2
        OnClick = cbxUpperBoundClick
      end
      object dtpLowerDate: TDateTimePicker
        Left = 8
        Top = 74
        Width = 125
        Height = 21
        Date = 37351.689680416700000000
        Time = 37351.689680416700000000
        Enabled = False
        TabOrder = 3
        OnChange = dtpLowerDateChange
      end
      object dtpUpperDate: TDateTimePicker
        Left = 140
        Top = 74
        Width = 125
        Height = 21
        Date = 37351.689680416700000000
        Time = 37351.689680416700000000
        Enabled = False
        TabOrder = 4
        OnChange = dtpUpperDateChange
      end
      object dtpLowerTime: TDateTimePicker
        Left = 8
        Top = 96
        Width = 125
        Height = 21
        Date = 37351.689680416700000000
        Time = 37351.689680416700000000
        Enabled = False
        Kind = dtkTime
        TabOrder = 5
        OnChange = dtpLowerTimeChange
      end
      object dtpUpperTime: TDateTimePicker
        Left = 140
        Top = 96
        Width = 125
        Height = 21
        Date = 37351.689680416700000000
        Time = 37351.689680416700000000
        Enabled = False
        Kind = dtkTime
        TabOrder = 6
        OnChange = dtpUpperTimeChange
      end
      object cbxAcceptDateBounds: TCheckBox
        Left = 8
        Top = 32
        Width = 123
        Height = 17
        Caption = 'Inclusive date bounds'
        TabOrder = 7
      end
      object cbxUseDateLimits: TCheckBox
        Left = 140
        Top = 32
        Width = 97
        Height = 17
        Caption = 'Use Date Limits'
        TabOrder = 8
        OnClick = cbxUseDateLimitsClick
      end
    end
    object pnlLatex: TPanel
      Left = 1
      Top = 247
      Width = 263
      Height = 106
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      ExplicitWidth = 271
      object lblColumnType: TLabel
        Left = 148
        Top = 54
        Width = 62
        Height = 13
        Caption = 'Column Type'
        Enabled = False
      end
      object Label2: TLabel
        Left = 8
        Top = 32
        Width = 60
        Height = 13
        Caption = 'Environment'
      end
      object Label3: TLabel
        Left = 8
        Top = 10
        Width = 84
        Height = 13
        Caption = 'LaTeX Settings'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object cbxvRules: TCheckBox
        Left = 8
        Top = 77
        Width = 121
        Height = 17
        Caption = 'Vertical rule columns'
        TabOrder = 0
      end
      object cbxhlines: TCheckBox
        Left = 8
        Top = 52
        Width = 97
        Height = 17
        Caption = 'Insert "/hlines"'
        TabOrder = 1
      end
      object cbLatexColType: TComboBox
        Left = 216
        Top = 50
        Width = 45
        Height = 21
        Style = csDropDownList
        Enabled = False
        Sorted = True
        TabOrder = 2
        Items.Strings = (
          'c'
          'l'
          'r')
      end
      object cbLatexEnvironment: TComboBox
        Left = 72
        Top = 28
        Width = 111
        Height = 21
        Style = csDropDownList
        Sorted = True
        TabOrder = 3
        Items.Strings = (
          'longtable'
          'tabular')
      end
    end
    object bOK: TBitBtn
      Left = 104
      Top = 362
      Width = 75
      Height = 25
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 4
      OnClick = bOKClick
    end
    object bCancel: TBitBtn
      Left = 192
      Top = 362
      Width = 75
      Height = 25
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 5
    end
  end
end
