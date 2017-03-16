object formSort: TformSort
  Left = 562
  Top = 341
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Sort Fields'
  ClientHeight = 233
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 344
    Height = 201
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lblSortField1: TLabel
      Left = 8
      Top = 13
      Width = 54
      Height = 13
      Caption = 'Sort Field 1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblSortField2: TLabel
      Left = 7
      Top = 77
      Width = 54
      Height = 13
      Caption = 'Sort Field 2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblSortField3: TLabel
      Left = 8
      Top = 143
      Width = 54
      Height = 13
      Caption = 'Sort Field 3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object bv3: TBevel
      Left = 0
      Top = 132
      Width = 344
      Height = 66
      Align = alTop
    end
    object bv1: TBevel
      Left = 0
      Top = 0
      Width = 344
      Height = 66
      Align = alTop
    end
    object bv2: TBevel
      Left = 0
      Top = 66
      Width = 344
      Height = 66
      Align = alTop
    end
    object cbField1: TComboBox
      Left = 70
      Top = 9
      Width = 161
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 0
    end
    object cbField2: TComboBox
      Left = 69
      Top = 74
      Width = 161
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 1
    end
    object cbField3: TComboBox
      Left = 70
      Top = 139
      Width = 161
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 2
    end
    object cbxDescField1: TCheckBox
      Left = 238
      Top = 11
      Width = 82
      Height = 17
      Caption = 'Descending'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object cbxDescField2: TCheckBox
      Left = 238
      Top = 75
      Width = 84
      Height = 17
      Caption = 'Descending'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
    object cbxDescField3: TCheckBox
      Left = 238
      Top = 141
      Width = 82
      Height = 17
      Caption = 'Descending'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object cbxCIField1: TCheckBox
      Left = 238
      Top = 28
      Width = 101
      Height = 17
      Caption = 'Case Insensitive'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
    end
    object cbxINField1: TCheckBox
      Left = 238
      Top = 45
      Width = 83
      Height = 17
      Caption = 'Ignore Nulls'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
    end
    object cbxCIField2: TCheckBox
      Left = 238
      Top = 92
      Width = 101
      Height = 17
      Caption = 'Case Insensitive'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
    end
    object cbxINField2: TCheckBox
      Left = 238
      Top = 109
      Width = 83
      Height = 17
      Caption = 'Ignore Nulls'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
    end
    object cbxINField3: TCheckBox
      Left = 238
      Top = 175
      Width = 83
      Height = 17
      Caption = 'Ignore Nulls'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
    end
    object cbxCIField3: TCheckBox
      Left = 238
      Top = 158
      Width = 101
      Height = 17
      Caption = 'Case Insensitive'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 201
    Width = 344
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object bOk: TBitBtn
      Left = 189
      Top = 4
      Width = 75
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Kind = bkOK
    end
    object bCancel: TBitBtn
      Left = 266
      Top = 4
      Width = 75
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Kind = bkCancel
    end
  end
end
