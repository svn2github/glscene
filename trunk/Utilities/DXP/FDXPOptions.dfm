object DXPOptions: TDXPOptions
  Left = 227
  Top = 106
  BorderStyle = bsDialog
  BorderWidth = 5
  Caption = 'DXP Options'
  ClientHeight = 311
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 277
    Width = 396
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      396
      34)
    object BUOk: TButton
      Left = 232
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object BUCancel: TButton
      Left = 320
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 396
    Height = 277
    ActivePage = TSFreePascal
    Align = alClient
    TabOrder = 1
    object TSFreePascal: TTabSheet
      Caption = 'FreePascal'
      DesignSize = (
        388
        249)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 103
        Height = 13
        Caption = 'Compiler binaries path'
        FocusControl = EDFPCBinary
      end
      object Label2: TLabel
        Left = 8
        Top = 56
        Width = 63
        Height = 13
        Caption = 'Source paths'
        FocusControl = EDFPCBinary
      end
      object EDFPCBinary: TEdit
        Left = 8
        Top = 24
        Width = 337
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'EDFPCBinary'
      end
      object BUFPCBinary: TButton
        Left = 352
        Top = 24
        Width = 27
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = BUFPCBinaryClick
      end
      object EDFPCSourcePaths: TEdit
        Left = 8
        Top = 72
        Width = 337
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'EDFPCSourcePaths'
      end
      object Button1: TButton
        Left = 352
        Top = 72
        Width = 27
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 3
        OnClick = Button1Click
      end
    end
  end
end
