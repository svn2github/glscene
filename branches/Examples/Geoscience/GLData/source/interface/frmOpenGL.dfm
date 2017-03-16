object formOpenGL: TformOpenGL
  Left = 297
  Top = 245
  BorderStyle = bsToolWindow
  Caption = 'OpenGL Context Information'
  ClientHeight = 293
  ClientWidth = 472
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
  object pcInfo: TPageControl
    Left = 0
    Top = 0
    Width = 472
    Height = 265
    ActivePage = tsCommon
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object tsCommon: TTabSheet
      Caption = 'Common'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object memCommon: TMemo
        Left = 0
        Top = 0
        Width = 464
        Height = 237
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
    end
    object tsBufferPixelDepths: TTabSheet
      Caption = 'Buffer and Pixel Depths'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object memBuffer: TMemo
        Left = 0
        Top = 0
        Width = 464
        Height = 237
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
    end
    object tsMaxValues: TTabSheet
      Caption = 'Maximum Values'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object memValues: TMemo
        Left = 0
        Top = 0
        Width = 464
        Height = 237
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
    end
    object tsExtensions: TTabSheet
      Caption = 'Supported Extensions'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbExtensions: TListBox
        Left = 0
        Top = 0
        Width = 464
        Height = 237
        Cursor = crHandPoint
        Hint = 
          'Double-click an extension to go to its OpenGL Extension registry' +
          ' page'
        Align = alClient
        BorderStyle = bsNone
        Color = clBtnHighlight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = cl3DDkShadow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ItemHeight = 13
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnDblClick = lbExtensionsDblClick
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 265
    Width = 472
    Height = 28
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object bOK: TBitBtn
      Left = 397
      Top = 2
      Width = 75
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Kind = bkOK
      NumGlyphs = 2
      ParentFont = False
      TabOrder = 0
    end
  end
end
