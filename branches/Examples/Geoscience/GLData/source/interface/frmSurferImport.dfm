object formSurferImport: TformSurferImport
  Left = 513
  Top = 478
  BorderStyle = bsSingle
  Caption = 'Surfer Grid Import Settings'
  ClientHeight = 148
  ClientWidth = 467
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
  object pnlGridBounds: TPanel
    Left = 0
    Top = 27
    Width = 467
    Height = 89
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object lblXField: TLabel
      Left = 8
      Top = 24
      Width = 31
      Height = 13
      Caption = 'X Field'
    end
    object lblYField: TLabel
      Left = 8
      Top = 46
      Width = 31
      Height = 13
      Caption = 'Y Field'
    end
    object lblZField: TLabel
      Left = 8
      Top = 68
      Width = 31
      Height = 13
      Caption = 'Z Field'
    end
    object lblMinimum: TLabel
      Left = 46
      Top = 4
      Width = 40
      Height = 13
      Caption = 'Minimum'
    end
    object lblMaximum: TLabel
      Left = 168
      Top = 4
      Width = 44
      Height = 13
      Caption = 'Maximum'
    end
    object lblSpacing: TLabel
      Left = 290
      Top = 4
      Width = 37
      Height = 13
      Caption = 'Spacing'
    end
    object lblNoNodes: TLabel
      Left = 412
      Top = 4
      Width = 41
      Height = 13
      Caption = 'Node No'
    end
    object lblTotalNodes: TLabel
      Left = 351
      Top = 68
      Width = 57
      Height = 13
      Alignment = taRightJustify
      Caption = 'Total Nodes'
    end
    object ebMinX: TEdit
      Left = 46
      Top = 20
      Width = 121
      Height = 21
      Color = clSkyBlue
      ReadOnly = True
      TabOrder = 0
    end
    object ebMinY: TEdit
      Left = 46
      Top = 42
      Width = 121
      Height = 21
      Color = clSkyBlue
      ReadOnly = True
      TabOrder = 4
    end
    object ebMinZ: TEdit
      Left = 46
      Top = 64
      Width = 121
      Height = 21
      Color = clSkyBlue
      ReadOnly = True
      TabOrder = 8
    end
    object ebMaxX: TEdit
      Left = 168
      Top = 20
      Width = 121
      Height = 21
      Color = clSkyBlue
      ReadOnly = True
      TabOrder = 1
    end
    object ebMaxY: TEdit
      Left = 168
      Top = 42
      Width = 121
      Height = 21
      Color = clSkyBlue
      ReadOnly = True
      TabOrder = 5
    end
    object ebMaxZ: TEdit
      Left = 168
      Top = 64
      Width = 121
      Height = 21
      Color = clSkyBlue
      ReadOnly = True
      TabOrder = 9
    end
    object ebSpacingX: TEdit
      Left = 290
      Top = 20
      Width = 121
      Height = 21
      Color = clSkyBlue
      ReadOnly = True
      TabOrder = 2
    end
    object ebSpacingY: TEdit
      Left = 290
      Top = 42
      Width = 121
      Height = 21
      Color = clSkyBlue
      ReadOnly = True
      TabOrder = 6
    end
    object ebTotalNo: TEdit
      Left = 412
      Top = 64
      Width = 50
      Height = 21
      Color = clSkyBlue
      ReadOnly = True
      TabOrder = 10
    end
    object ebNoY: TEdit
      Left = 412
      Top = 42
      Width = 50
      Height = 21
      Color = clSkyBlue
      ReadOnly = True
      TabOrder = 7
    end
    object ebNoX: TEdit
      Left = 412
      Top = 20
      Width = 50
      Height = 21
      Color = clSkyBlue
      ReadOnly = True
      TabOrder = 3
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 116
    Width = 467
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object bOK: TBitBtn
      Left = 310
      Top = 5
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
      Left = 389
      Top = 5
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
    object pnlBlankedNodes: TPanel
      Left = 0
      Top = 0
      Width = 292
      Height = 32
      Align = alLeft
      BevelOuter = bvLowered
      TabOrder = 2
      object lblBlankedNodes: TLabel
        Left = 6
        Top = 10
        Width = 70
        Height = 13
        Caption = 'Blanked Nodes'
      end
      object lblBlankedValue: TLabel
        Left = 136
        Top = 10
        Width = 26
        Height = 13
        Caption = 'Value'
      end
      object ebNoBlanks: TEdit
        Left = 79
        Top = 6
        Width = 50
        Height = 21
        Color = clSkyBlue
        ReadOnly = True
        TabOrder = 0
      end
      object geBlankedValue: TGEFloatEdit
        Left = 168
        Top = 6
        Width = 121
        Height = 21
        Text = '0'
        ReturnStop = False
        UseMinBound = False
        UseMaxBound = False
        Validate = vdNone
        TabOrder = 1
      end
    end
  end
  object pnlGridName: TPanel
    Left = 0
    Top = 0
    Width = 467
    Height = 27
    Align = alClient
    TabOrder = 0
    object lblGridName: TLabel
      Left = 6
      Top = 6
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object ebGridName: TEdit
      Left = 42
      Top = 2
      Width = 422
      Height = 21
      TabOrder = 0
    end
  end
end
