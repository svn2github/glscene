object InfoForm: TInfoForm
  Left = 355
  Top = 251
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsToolWindow
  BorderWidth = 3
  Caption = 'OpenGL Driver Info'
  ClientHeight = 265
  ClientWidth = 453
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  DesignSize = (
    453
    265)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 450
    Height = 230
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    HotTrack = True
    TabIndex = 4
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = 'GLScene'
      ImageIndex = 4
      object Label19: TLabel
        Left = 120
        Top = 16
        Width = 208
        Height = 56
        Caption = 'GLScene'
        Font.Charset = ANSI_CHARSET
        Font.Color = clSilver
        Font.Height = -48
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label21: TLabel
        Left = 126
        Top = 67
        Width = 197
        Height = 18
        Caption = 'OpenGL Solution for Delphi'
        Font.Charset = ANSI_CHARSET
        Font.Color = clSilver
        Font.Height = -15
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label22: TLabel
        Left = 118
        Top = 14
        Width = 208
        Height = 56
        Caption = 'GLScene'
        Font.Charset = ANSI_CHARSET
        Font.Color = 4227327
        Font.Height = -48
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label24: TLabel
        Left = 124
        Top = 65
        Width = 197
        Height = 18
        Caption = 'OpenGL Solution for Delphi'
        Font.Charset = ANSI_CHARSET
        Font.Color = 4227327
        Font.Height = -15
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label30: TLabel
        Left = 167
        Top = 96
        Width = 38
        Height = 13
        Caption = 'Version:'
      end
      object VersionLbl: TLabel
        Left = 224
        Top = 96
        Width = 48
        Height = 13
        Caption = 'versionNo'
      end
      object WebsiteLbl: TLabel
        Left = 167
        Top = 120
        Width = 113
        Height = 13
        Cursor = crHandPoint
        Caption = 'http://www.glscene.org'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = WebsiteLblClick
      end
    end
    object Sheet1: TTabSheet
      Caption = 'Common'
      object Label1: TLabel
        Left = 10
        Top = 10
        Width = 82
        Height = 13
        Caption = 'OpenGL vendor :'
        Transparent = True
      end
      object Label2: TLabel
        Left = 10
        Top = 54
        Width = 65
        Height = 13
        Caption = 'Acceleration :'
        Transparent = True
      end
      object Label3: TLabel
        Left = 10
        Top = 77
        Width = 62
        Height = 13
        Caption = 'Version Info :'
        Transparent = True
      end
      object Label5: TLabel
        Left = 10
        Top = 99
        Width = 82
        Height = 13
        Caption = 'Double buffered :'
        Transparent = True
      end
      object Label6: TLabel
        Left = 10
        Top = 144
        Width = 96
        Height = 13
        Caption = 'Buffer switch mode :'
        Transparent = True
      end
      object VendorLabel: TLabel
        Left = 160
        Top = 10
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object AccLabel: TLabel
        Left = 160
        Top = 54
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object VersionLabel: TLabel
        Left = 160
        Top = 77
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object CopyLabel: TLabel
        Left = 160
        Top = 144
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object DoubleLabel: TLabel
        Left = 160
        Top = 99
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label7: TLabel
        Left = 10
        Top = 121
        Width = 78
        Height = 13
        Caption = 'Stereo enabled :'
        Transparent = True
      end
      object StereoLabel: TLabel
        Left = 160
        Top = 121
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label13: TLabel
        Left = 10
        Top = 32
        Width = 50
        Height = 13
        Caption = 'Renderer :'
        Transparent = True
      end
      object RendererLabel: TLabel
        Left = 160
        Top = 32
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
    end
    object Sheet2: TTabSheet
      Caption = 'Buffer and pixel depths'
      object Label8: TLabel
        Left = 10
        Top = 10
        Width = 60
        Height = 13
        Caption = 'Color buffer :'
        Transparent = True
      end
      object Label9: TLabel
        Left = 10
        Top = 33
        Width = 65
        Height = 13
        Caption = 'Depth buffer :'
        Transparent = True
      end
      object Label10: TLabel
        Left = 10
        Top = 55
        Width = 68
        Height = 13
        Caption = 'Stencil buffer :'
        Transparent = True
      end
      object Label11: TLabel
        Left = 10
        Top = 78
        Width = 100
        Height = 13
        Caption = 'Accumulation buffer :'
        Transparent = True
      end
      object Label12: TLabel
        Left = 10
        Top = 101
        Width = 81
        Height = 13
        Caption = 'Auxilliary buffers :'
        Transparent = True
      end
      object ColorLabel: TLabel
        Left = 160
        Top = 10
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object DepthLabel: TLabel
        Left = 160
        Top = 33
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object StencilLabel: TLabel
        Left = 160
        Top = 55
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object AuxLabel: TLabel
        Left = 160
        Top = 101
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object AccumLabel: TLabel
        Left = 160
        Top = 78
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object SubLabel: TLabel
        Left = 160
        Top = 124
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label37: TLabel
        Left = 10
        Top = 124
        Width = 51
        Height = 13
        Caption = 'Subpixels :'
        Transparent = True
      end
      object Label18: TLabel
        Left = 10
        Top = 146
        Width = 95
        Height = 13
        Caption = 'supported overlays :'
        Transparent = True
      end
      object OverlayLabel: TLabel
        Left = 160
        Top = 146
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object UnderlayLabel: TLabel
        Left = 160
        Top = 170
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label20: TLabel
        Left = 10
        Top = 170
        Width = 101
        Height = 13
        Caption = 'supported underlays :'
        Transparent = True
      end
    end
    object Sheet3: TTabSheet
      Caption = 'Maximum values'
      object Label14: TLabel
        Left = 10
        Top = 10
        Width = 57
        Height = 13
        Caption = 'Clip planes :'
        Transparent = True
      end
      object Label15: TLabel
        Left = 10
        Top = 33
        Width = 83
        Height = 13
        Caption = 'Evaluation order :'
        Transparent = True
      end
      object Label16: TLabel
        Left = 10
        Top = 55
        Width = 69
        Height = 13
        Caption = 'Light sources :'
        Transparent = True
      end
      object Label17: TLabel
        Left = 10
        Top = 78
        Width = 59
        Height = 13
        Caption = 'List nesting :'
        Transparent = True
      end
      object ClipLabel: TLabel
        Left = 160
        Top = 10
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object EvalLabel: TLabel
        Left = 160
        Top = 33
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object ListLabel: TLabel
        Left = 160
        Top = 78
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object LightLabel: TLabel
        Left = 160
        Top = 55
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label23: TLabel
        Left = 10
        Top = 101
        Width = 86
        Height = 13
        Caption = 'Modelview stack :'
        Transparent = True
      end
      object ModelLabel: TLabel
        Left = 160
        Top = 101
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label25: TLabel
        Left = 195
        Top = 10
        Width = 63
        Height = 13
        Caption = 'Name stack :'
        Transparent = True
      end
      object Label26: TLabel
        Left = 195
        Top = 33
        Width = 74
        Height = 13
        Caption = 'Pixel map table:'
        Transparent = True
      end
      object Label27: TLabel
        Left = 195
        Top = 55
        Width = 82
        Height = 13
        Caption = 'Projection stack :'
        Transparent = True
      end
      object Label28: TLabel
        Left = 195
        Top = 78
        Width = 60
        Height = 13
        Caption = 'Texture size:'
        Transparent = True
      end
      object Label29: TLabel
        Left = 195
        Top = 101
        Width = 71
        Height = 13
        Caption = 'Texture stack :'
        Transparent = True
      end
      object NameLabel: TLabel
        Left = 338
        Top = 10
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object PixelLabel: TLabel
        Left = 338
        Top = 33
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object ProjLabel: TLabel
        Left = 338
        Top = 55
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object TexStackLabel: TLabel
        Left = 338
        Top = 101
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object TexSizeLabel: TLabel
        Left = 338
        Top = 78
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label35: TLabel
        Left = 10
        Top = 124
        Width = 104
        Height = 13
        Caption = 'Viewport Dimensions :'
        Transparent = True
      end
      object ViewLabel: TLabel
        Left = 160
        Top = 124
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label4: TLabel
        Left = 195
        Top = 124
        Width = 67
        Height = 13
        Caption = 'Texture units :'
        Transparent = True
      end
      object TexUnitsLabel: TLabel
        Left = 338
        Top = 124
        Width = 21
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Supported extensions'
      ImageIndex = 3
      object Extensions: TListBox
        Left = 0
        Top = 0
        Width = 442
        Height = 202
        Hint = 
          'Double-click and extension to go to its OpenGL Extension registr' +
          'y page'
        Align = alClient
        BorderStyle = bsNone
        Color = clBtnHighlight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = cl3DDkShadow
        Font.Height = -12
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ItemHeight = 14
        ParentFont = False
        TabOrder = 0
        OnClick = ExtensionsClick
        OnDblClick = ExtensionsDblClick
        OnKeyPress = ExtensionsKeyPress
      end
    end
  end
  object CloseButton: TButton
    Left = 376
    Top = 237
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Close'
    TabOrder = 1
    OnClick = CloseButtonClick
  end
  object PMWebLink: TPopupMenu
    Left = 24
    Top = 48
    object MIRegistryLink: TMenuItem
      Caption = 'MIRegistryLink'
      OnClick = ExtensionsDblClick
    end
    object MIDelphi3D: TMenuItem
      Caption = 'MIDelphi3D'
      OnClick = MIDelphi3DClick
    end
  end
end
