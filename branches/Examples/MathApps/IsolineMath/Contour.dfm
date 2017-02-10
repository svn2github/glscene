object ContourForm: TContourForm
  Left = 274
  Top = 236
  Caption = 'Isolines'
  ClientHeight = 441
  ClientWidth = 801
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -10
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GraphImage: TMathImage
    Left = 0
    Top = 0
    Width = 678
    Height = 422
    Hint = 'Right Click for Menu'
    Align = alClient
    Brush.Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clSilver
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    OnResize = GraphImageResize
    ShowHint = True
    ParentShowHint = False
    Version = '6.1 May 2015'
    RecordMetafile = False
    d2WorldX1 = -1.000000000000000000
    d2WorldXW = 2.000000000000000000
    d2WorldY1 = -1.000000000000000000
    d2WorldYW = 2.000000000000000000
    d3WorldX1 = -1.000000000000000000
    d3WorldXW = 2.000000000000000000
    d3WorldY1 = -1.000000000000000000
    d3WorldYW = 2.000000000000000000
    d3WorldZ1 = -1.000000000000000000
    d3WorldZW = 2.000000000000000000
    d3Xscale = 1.000000000000000000
    d3Yscale = 1.000000000000000000
    d3Zscale = 1.000000000000000000
    d3Zrotation = 45.000000000000000000
    d3Yrotation = 20.000000000000000000
    d3ViewDist = 16.500000000000000000
    d3ViewAngle = 0.600000000000000000
    d3AspectRatio = True
    ExplicitWidth = 280
    ExplicitHeight = 266
  end
  object Panel1: TPanel
    Left = 678
    Top = 0
    Width = 123
    Height = 422
    Align = alRight
    TabOrder = 0
    object Label1: TLabel
      Left = 29
      Top = 58
      Width = 36
      Height = 13
      Caption = 'Legend'
    end
    object ContourlinesButton: TButton
      Left = 23
      Top = 0
      Width = 85
      Height = 23
      Caption = 'Isolines'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = ContourlinesButtonClick
    end
    object FilledContoursButton: TButton
      Left = 23
      Top = 25
      Width = 85
      Height = 24
      Caption = 'Filled Isolines'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = FilledContoursButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 422
    Width = 801
    Height = 19
    Align = alBottom
    TabOrder = 1
  end
  object ColorDialog1: TColorDialog
    Color = clTeal
    Left = 152
    Top = 344
  end
end
