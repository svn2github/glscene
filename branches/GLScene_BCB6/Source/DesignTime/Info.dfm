object InfoForm: TInfoForm
  Left = 337
  Top = 107
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsToolWindow
  BorderWidth = 3
  Caption = 'OpenGL Driver Info'
  ClientHeight = 256
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object CloseButton: TSpeedButton
    Left = 0
    Top = 232
    Width = 404
    Height = 23
    Caption = '&Close'
    Flat = True
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      0400000000000001000000000000000000001000000010000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      555555555555555555555555555555555555555555FF55555555555559055555
      55555555577FF5555555555599905555555555557777F5555555555599905555
      555555557777FF5555555559999905555555555777777F555555559999990555
      5555557777777FF5555557990599905555555777757777F55555790555599055
      55557775555777FF5555555555599905555555555557777F5555555555559905
      555555555555777FF5555555555559905555555555555777FF55555555555579
      05555555555555777FF5555555555557905555555555555777FF555555555555
      5990555555555555577755555555555555555555555555555555}
    NumGlyphs = 2
    OnClick = CloseButtonClick
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 404
    Height = 225
    ActivePage = TabSheet1
    HotTrack = True
    TabIndex = 3
    TabOrder = 0
    object Sheet1: TTabSheet
      Caption = 'Common'
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 396
        Height = 197
        Align = alClient
      end
      object Label1: TLabel
        Left = 10
        Top = 10
        Width = 92
        Height = 14
        Caption = 'OpenGL vendor :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label2: TLabel
        Left = 10
        Top = 54
        Width = 75
        Height = 14
        Caption = 'Acceleration :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label3: TLabel
        Left = 10
        Top = 77
        Width = 74
        Height = 14
        Caption = 'Version Info :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label5: TLabel
        Left = 10
        Top = 99
        Width = 95
        Height = 14
        Caption = 'Double buffered :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label6: TLabel
        Left = 10
        Top = 144
        Width = 115
        Height = 14
        Caption = 'Buffer switch mode :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object VendorLabel: TLabel
        Left = 137
        Top = 10
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object AccLabel: TLabel
        Left = 137
        Top = 54
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object VersionLabel: TLabel
        Left = 137
        Top = 77
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object CopyLabel: TLabel
        Left = 137
        Top = 144
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object DoubleLabel: TLabel
        Left = 137
        Top = 99
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label7: TLabel
        Left = 10
        Top = 121
        Width = 90
        Height = 14
        Caption = 'Stereo enabled :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object StereoLabel: TLabel
        Left = 137
        Top = 121
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label13: TLabel
        Left = 10
        Top = 32
        Width = 58
        Height = 14
        Caption = 'Renderer :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object RendererLabel: TLabel
        Left = 137
        Top = 32
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
    end
    object Sheet2: TTabSheet
      Caption = 'Buffer and pixel depths'
      object Image2: TImage
        Left = 0
        Top = 0
        Width = 396
        Height = 197
        Align = alClient
      end
      object Label8: TLabel
        Left = 10
        Top = 10
        Width = 73
        Height = 14
        Caption = 'Color buffer :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label9: TLabel
        Left = 10
        Top = 33
        Width = 75
        Height = 14
        Caption = 'Depth buffer :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label10: TLabel
        Left = 10
        Top = 55
        Width = 80
        Height = 14
        Caption = 'Stencil buffer :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label11: TLabel
        Left = 10
        Top = 78
        Width = 118
        Height = 14
        Caption = 'Accumulation buffer :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label12: TLabel
        Left = 10
        Top = 101
        Width = 100
        Height = 14
        Caption = 'Auxilliary buffers :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object ColorLabel: TLabel
        Left = 137
        Top = 10
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object DepthLabel: TLabel
        Left = 137
        Top = 33
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object StencilLabel: TLabel
        Left = 137
        Top = 55
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object AuxLabel: TLabel
        Left = 137
        Top = 101
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object AccumLabel: TLabel
        Left = 137
        Top = 78
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object SubLabel: TLabel
        Left = 137
        Top = 124
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label37: TLabel
        Left = 10
        Top = 124
        Width = 60
        Height = 14
        Caption = 'Subpixels :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label18: TLabel
        Left = 10
        Top = 146
        Width = 114
        Height = 14
        Caption = 'supported overlays :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object OverlayLabel: TLabel
        Left = 137
        Top = 146
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object UnderlayLabel: TLabel
        Left = 137
        Top = 170
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label20: TLabel
        Left = 10
        Top = 170
        Width = 122
        Height = 14
        Caption = 'supported underlays :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
    end
    object Sheet3: TTabSheet
      Caption = 'Maximum values'
      object Image3: TImage
        Left = 0
        Top = 0
        Width = 396
        Height = 197
        Align = alClient
      end
      object Label14: TLabel
        Left = 10
        Top = 10
        Width = 67
        Height = 14
        Caption = 'Clip planes :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label15: TLabel
        Left = 10
        Top = 33
        Width = 95
        Height = 14
        Caption = 'Evaluation order :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label16: TLabel
        Left = 10
        Top = 55
        Width = 83
        Height = 14
        Caption = 'Light sources :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label17: TLabel
        Left = 10
        Top = 78
        Width = 72
        Height = 14
        Caption = 'List nesting :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object ClipLabel: TLabel
        Left = 137
        Top = 10
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object EvalLabel: TLabel
        Left = 137
        Top = 33
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object ListLabel: TLabel
        Left = 137
        Top = 78
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object LightLabel: TLabel
        Left = 137
        Top = 55
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label23: TLabel
        Left = 10
        Top = 101
        Width = 99
        Height = 14
        Caption = 'Modelview stack :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object ModelLabel: TLabel
        Left = 137
        Top = 101
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label25: TLabel
        Left = 195
        Top = 10
        Width = 70
        Height = 14
        Caption = 'Name stack :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label26: TLabel
        Left = 195
        Top = 33
        Width = 86
        Height = 14
        Caption = 'Pixel map table:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label27: TLabel
        Left = 195
        Top = 55
        Width = 95
        Height = 14
        Caption = 'Projection stack :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label28: TLabel
        Left = 195
        Top = 78
        Width = 72
        Height = 14
        Caption = 'Texture size:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label29: TLabel
        Left = 195
        Top = 101
        Width = 82
        Height = 14
        Caption = 'Texture stack :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object NameLabel: TLabel
        Left = 322
        Top = 10
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object PixelLabel: TLabel
        Left = 322
        Top = 33
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object ProjLabel: TLabel
        Left = 322
        Top = 55
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object TexStackLabel: TLabel
        Left = 322
        Top = 101
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object TexSizeLabel: TLabel
        Left = 322
        Top = 78
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label35: TLabel
        Left = 10
        Top = 124
        Width = 126
        Height = 14
        Caption = 'Viewport Dimensions :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object ViewLabel: TLabel
        Left = 137
        Top = 124
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label4: TLabel
        Left = 195
        Top = 124
        Width = 80
        Height = 14
        Caption = 'Texture units :'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object TexUnitsLabel: TLabel
        Left = 322
        Top = 124
        Width = 22
        Height = 13
        Caption = '???'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
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
        Width = 396
        Height = 197
        Hint = 
          'Double-click and extension to go to its OpenGL Extension registr' +
          'y page'
        Align = alClient
        BorderStyle = bsNone
        Color = clBtnHighlight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = cl3DDkShadow
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ItemHeight = 15
        ParentFont = False
        TabOrder = 0
        OnClick = ExtensionsClick
        OnDblClick = ExtensionsDblClick
        OnKeyPress = ExtensionsKeyPress
      end
    end
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
