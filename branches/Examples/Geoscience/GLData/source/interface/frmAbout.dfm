object formAbout: TformAbout
  Left = 553
  Top = 398
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'About glData...'
  ClientHeight = 348
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlAboutTop: TPanel
    Left = 0
    Top = 0
    Width = 391
    Height = 82
    Align = alTop
    TabOrder = 0
    object lblUrl: TLabel
      Left = 8
      Top = 65
      Width = 144
      Height = 13
      Cursor = crHandPoint
      Caption = 'http://gldata.sourceforge.net'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      OnClick = lblUrlClick
    end
    object lblCopyright: TLabel
      Left = 7
      Top = 49
      Width = 230
      Height = 13
      Caption = 'Created by Aaron Hochwimmer, Copyright 2003'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblTitle: TLabel
      Left = 60
      Top = 2
      Width = 92
      Height = 39
      Caption = 'glData'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 33023
      Font.Height = -32
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      OnClick = lblTitleClick
    end
    object lblVersion: TLabel
      Left = 161
      Top = 9
      Width = 103
      Height = 13
      Caption = 'Pre-Release vx.x.x.x'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      IsControl = True
    end
    object lblBuildDate: TLabel
      Left = 161
      Top = 25
      Width = 48
      Height = 13
      Caption = 'Build Date'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object bOk: TBitBtn
      Left = 313
      Top = 55
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 82
    Width = 391
    Height = 266
    ActivePage = tsCredits
    Align = alClient
    TabOrder = 1
    object tsCredits: TTabSheet
      Caption = 'Credits'
      object jvspCredits: TMemo
        Left = 0
        Top = 0
        Width = 383
        Height = 238
        Align = alClient
        Alignment = taCenter
        Lines.Strings = (
          '~~~~~ DEVELOPED BY ~~~~~ '
          ''
          'Aaron Hochwimmer [aaron@graphic-edge.co.nz]'
          ''
          '~~~~~ CODE CONTRIBUTIONS ~~~~~ '
          ''
          'Dave Kerr'
          'Phillipp Pammler'
          'Phil Scadden'
          ''
          '~~~~~ TESTING, FEEDBACK, BUG REPORTS ~~~~~ '
          ''
          'Richard Beitelmair'
          'Dave Kerr'
          'Heinz-Georg Wassing'
          ''
          '~~~~~ THANKS ~~~~~ '
          ''
          'Eric Grange'
          'Ivan Lee Herring'
          ''
          '~~~~~ CODE USAGE ~~~~~ '
          ''
          'GLScene - OpenGL Library for Delphi'
          'http://www.glscene.org'
          'http://www.caperaven.co.za/'
          ''
          'Graphics 32 (Alex Denisov)'
          'http://www.g32.org'
          ''
          'GraphicEx (Mike Lischke)'
          'http://www.delphi-gems.com/Graphics.php'
          ''
          'kbmMemTable'
          'http://www.components4developers.com'
          ''
          'PB Power (NZ) Ltd. '
          'http://software.pbpower.net'
          ''
          'OpenXML'
          'http://www.philo.de/xml'
          ''
          '~~~~~ ADDITIONAL THANKS ~~~~~ '
          ''
          'SourceForge - hosting'
          'http://www.sourceforge.net'
          ''
          'Graphic Edge - website'
          'http://www.graphic-edge.co.nz'
          ''
          'Kukjae Hapkido - martial arts'
          'http://www.hapkido.org.nz')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        OnDblClick = jvspCreditsDblClick
      end
    end
    object tsChanges: TTabSheet
      Caption = 'Changes'
      ImageIndex = 1
      object memChanges: TMemo
        Left = 0
        Top = 0
        Width = 383
        Height = 236
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object tsSecretPage: TTabSheet
      Caption = 'Strict Quality Contril'
      ImageIndex = 2
      TabVisible = False
      object memQuality: TMemo
        Left = 0
        Top = 0
        Width = 383
        Height = 238
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Courier'
        Font.Style = []
        Lines.Strings = (
          'This product is under strict quality '
          'contril with perfect packing and quality '
          'when leaving the factory.please keep away '
          'from damp.high temperature or sun '
          'expose.If found any detectives when '
          'purchasing.please return the productby '
          'airmail to our administrative section and '
          'inform the time.place.and store of this '
          'purchase for our improvement.We shall '
          'give you a satisfactory reply.Thanks for '
          'your patronage and welcome your comments.')
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
    end
    object tsLicence: TTabSheet
      Caption = 'Licence'
      ImageIndex = 3
      object memLicence: TMemo
        Left = 0
        Top = 0
        Width = 383
        Height = 236
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
end
