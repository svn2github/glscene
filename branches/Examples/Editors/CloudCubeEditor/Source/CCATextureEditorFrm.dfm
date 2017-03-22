object ATextureEditorForm: TATextureEditorForm
  Left = 200
  Top = 114
  Caption = 'Texture Editor'
  ClientHeight = 244
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000077
    777777770000000000000000000004000000004700000000000000000000004C
    E6C4040700000000000000000000004CE6C4040700000000000000000000004C
    E6C404070000000000000000D000004CE6C404070000000000000000D000004C
    E6C404070000000000000005D000004CE6C40407000000000000000DD500004C
    E6C404070000000550000000D500004CE6C404070000000D50000000D500004C
    E6C404070000000DD5000000D500004CE6C4040700000000D500000DD500004C
    E6C4040700000000D500000DD500004CE6C4040700000000D505000DD500004C
    E6C4040700000000D50D50DDD500004CE6C4040700000000DD5D50DED500004C
    E6C4040700000000DD5DD5DDD500004CE6C4040700000000DDDDDDDDD500004C
    E6C4040700000000DDDDDDDDD500004CE6C4040700000005DEDDDDDDD5500400
    0000004000000DDDDDDDDDDDDD550008F8877000000000DDDDDDDDDDDDD50000
    8F87000000D50DDDDDDDDDDDDDD500000110000D0000DDDDDDDDDDDDDDD50000
    09100D00D0D00DDDDDDDDDDDDDD50000090DD0D0000D0DEDDDDDDDDDDDD50000
    09900D500D050DDDDDDDDDDDDDD500000000000D00D000DEDDDDDDDDDD500000
    00000000D000D0DDEEDDDDDDDD50000000000000000D000DDDDDDDDDDD000000
    00000000000000D5DDEDDEDDD0000000000000000000000000DDDDD00000C00F
    FFFF800FFFFF800FFFFF800FFFFF800FFFF7800FFFF7800FFFE7800FFFE3800F
    E7F3800FE7F3800FE3F3800FF3E3800FF3E3800FF2E3800FF243800FF043800F
    F003800FF003800FF003800FE001801F8000C03FC000E07C8000F0EF0000F9B5
    8000F85E8000F99A8000FFEDC001FFF74001FFFEE003FFFFC007FFFFFC1F}
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PreviewImage: TImage
    Left = 0
    Top = 0
    Width = 256
    Height = 256
    Cursor = crCross
    AutoSize = True
    OnMouseDown = PreviewImageMouseDown
    OnMouseMove = PreviewImageMouseMove
    OnMouseUp = PreviewImageMouseUp
  end
  object AguiPanel: TPanel
    Left = 248
    Top = 0
    Width = 265
    Height = 244
    Align = alRight
    TabOrder = 0
    ExplicitLeft = 256
    ExplicitHeight = 256
    object Image321: TImage32
      Left = 0
      Top = 0
      Width = 256
      Height = 256
      AutoSize = True
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
    end
    object IOriginalBottomPanel: TPanel
      Tag = 10
      Left = 1
      Top = 0
      Width = 264
      Height = 257
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object openmaskBtn: TSpeedButton
        Left = 4
        Top = 4
        Width = 32
        Height = 22
        Caption = 'open'
        OnClick = openmaskBtnClick
      end
      object UndoBtn: TSpeedButton
        Left = 176
        Top = 4
        Width = 23
        Height = 22
        Hint = 'Undo'
        Glyph.Data = {
          26040000424D2604000000000000360000002800000012000000120000000100
          180000000000F0030000880B0000880B00000000000000000000008080008080
          0080800080800080800080808000008000008000008000008000008000000080
          8000808000808000808000808000808000300080800080008000000080800080
          8080000000800000800000800000800000800000800080000080000000808000
          8080008080008080003000808000800000800080000080000000800000800000
          8000008000008000008000008000008000008000800000008080008080008080
          003000808000800000800000800000800000800000800000FF0000FF0000FF00
          00FF0000FF000080000080000080008000000080800080800030008080008000
          00800000800000800000800000FF0000808000808000808000808000808000FF
          0000800000800000800080000000808000300080800080000080000080000080
          0000800000808000808000808000808000808000808000808000FF0000800000
          8000800000008080003000808000800000800000800000800000800000800000
          808000808000808000808000808000808000FF00800000800000800000008080
          003000808000FF0000FF0000FF0000FF0000FF0000FF0000FF00008080008080
          0080800080800080800080800080800080800080800080800030008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          8000808000808000808000808000808000300080800080800080800080800080
          8000808000808000808000808000808000808000808080000080000080000080
          0000800000800000003000808000FF0080000080000080000000808000808000
          808000808000808000808000FF00008000008000008000008000008000800000
          003000808000FF00008000008000800000008080008080008080008080008080
          00808000808000FF000080000080000080000080008000000030008080008080
          00FF000080000080008000000080800080800080800080800080808000008000
          00008000008000008000008000800000003000808000808000FF000080000080
          0000800080000080000080000080000080000000800000800000800000800000
          8000008000800000003000808000808000808000FF0000800000800000800000
          800000800000800000800000800000800000800000FF0000FF00008000800000
          003000808000808000808000808000FF0000FF00008000008000008000008000
          00800000800000FF0000FF0000808000808000FF000080800030008080008080
          00808000808000808000808000FF0000FF0000FF0000FF0000FF0000FF000080
          8000808000808000808000808000808000300080800080800080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          80800080800080800030}
        OnClick = UndoBtnClick
      end
      object HelpBtn: TSpeedButton
        Left = 204
        Top = 4
        Width = 23
        Height = 22
        Caption = '?'
        OnClick = HelpBtnClick
      end
      object ExitBtn: TSpeedButton
        Left = 232
        Top = 4
        Width = 29
        Height = 22
        Caption = 'Exit'
        OnClick = ExitBtnClick
      end
      object SmoothAllBtn: TSpeedButton
        Left = 16
        Top = 223
        Width = 57
        Height = 26
        Hint = 'Smooth All'
        Caption = 'Smooth'
        OnClick = SmoothAllBtnClick
      end
      object MaskFilenameEdit: TEdit
        Left = 38
        Top = 4
        Width = 139
        Height = 21
        TabOrder = 0
      end
      object PaintCloneRG: TRadioGroup
        Left = 2
        Top = 64
        Width = 119
        Height = 33
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Paint'
          'Clone')
        TabOrder = 1
        OnClick = PaintCloneRGClick
      end
      object SaveAlphaRG: TRadioGroup
        Left = 2
        Top = 32
        Width = 257
        Height = 33
        Hint = '24or32S: Bmp, 32Aor32spr:Tga'
        Caption = 'Save as 32Bit Alpha'
        Columns = 5
        ItemIndex = 4
        Items.Strings = (
          '24bit'
          '32S'
          '32A'
          '32Sqr'
          'NA')
        TabOrder = 2
        OnClick = SaveAlphaRGClick
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 96
        Width = 129
        Height = 121
        PopupMenu = PaintPopupMenu
        TabOrder = 3
        object Shape1: TShape
          Left = 16
          Top = 34
          Width = 8
          Height = 24
        end
        object SpinEdit1: TSpinEdit
          Left = 26
          Top = 10
          Width = 44
          Height = 22
          Hint = 'Brush size'
          MaxValue = 66
          MinValue = 1
          TabOrder = 0
          Value = 5
        end
        object IsWhiteCB: TCheckBox
          Left = 31
          Top = 36
          Width = 17
          Height = 17
          Hint = 'Unchecked is Paint it Black'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = IsWhiteCBClick
        end
        object PaintColorPanel: TPanel
          Left = 48
          Top = 36
          Width = 33
          Height = 21
          Hint = 'Click to change Paint Color'
          Color = clSilver
          TabOrder = 2
          OnClick = PaintColorPanelClick
        end
        object IsColorCB: TCheckBox
          Left = 87
          Top = 36
          Width = 17
          Height = 17
          Hint = 'Unchecked is Paint it Black'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object DotBoxRG: TRadioGroup
          Left = 8
          Top = 58
          Width = 113
          Height = 61
          Hint = 'Brush type'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'Dot'
            'Box'
            'Circle'
            'Flood'
            'Edge'
            'Paint')
          TabOrder = 4
        end
      end
      object GroupBox2: TGroupBox
        Left = 134
        Top = 96
        Width = 129
        Height = 121
        TabOrder = 4
        object intensTrackBar: TTrackBar
          Left = 12
          Top = 89
          Width = 100
          Height = 24
          Hint = 'Wand'
          HelpContext = 14600
          Max = 100
          Min = 1
          PageSize = 5
          Frequency = 5
          Position = 50
          TabOrder = 0
          ThumbLength = 15
        end
        object trasTrackBar: TTrackBar
          Left = 11
          Top = 65
          Width = 100
          Height = 24
          Hint = 'Wand'
          HelpContext = 14600
          Max = 100
          Min = 1
          PageSize = 5
          Frequency = 5
          Position = 10
          TabOrder = 1
          ThumbLength = 15
        end
        object WandTrackBar: TTrackBar
          Left = 11
          Top = 41
          Width = 100
          Height = 24
          Hint = 'Wand'
          HelpContext = 14600
          Max = 100
          Min = 1
          PageSize = 5
          Frequency = 5
          Position = 30
          TabOrder = 2
          ThumbLength = 15
        end
        object WandCoB: TComboBox
          Left = 21
          Top = 12
          Width = 83
          Height = 21
          Hint = 'Wand'
          HelpContext = 14600
          TabOrder = 3
          Text = 'Color (rgb)'
          Items.Strings = (
            'Color (rgb)'
            'Color (hue)'
            '+Light'
            '+Saturation')
        end
      end
      object ClonePanel: TPanel
        Left = 140
        Top = 74
        Width = 121
        Height = 23
        PopupMenu = ClonePopupMenu
        TabOrder = 5
      end
      object TLCloneBox: TCheckBox
        Left = 122
        Top = 74
        Width = 17
        Height = 17
        Hint = 'Unchecked :TL is 0,0'
        TabOrder = 6
      end
      object SmoothRG: TRadioGroup
        Left = 80
        Top = 217
        Width = 169
        Height = 32
        Hint = 'Smoothers'
        Columns = 5
        ItemIndex = 0
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5')
        TabOrder = 7
      end
    end
  end
  object ClonePopupMenu: TPopupMenu
    Left = 472
    Top = 67
    object CloneSize1: TMenuItem
      Caption = 'Clone Size'
      Enabled = False
      OnClick = CloneSize1Click
    end
    object CloneBoxorCircleMenu: TMenuItem
      Caption = 'Box or Circle'
      Checked = True
      Enabled = False
      OnClick = CloneBoxorCircleMenuClick
    end
    object CloneXorShapeMenu: TMenuItem
      Caption = 'Clone X or Shape'
      Checked = True
      Enabled = False
      OnClick = CloneXorShapeMenuClick
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 219
    Top = 3
  end
  object ColorDialog1: TColorDialog
    Left = 216
    Top = 128
  end
  object SaveDialog1: TSaveDialog
    Left = 216
    Top = 40
  end
  object PaintPopupMenu: TPopupMenu
    Left = 225
    Top = 72
    object PaintResize: TMenuItem
      Caption = 'Resize to 128'
      OnClick = PaintResizeClick
    end
    object PaintCrop: TMenuItem
      Caption = 'Crop to 128'
      OnClick = PaintCropClick
    end
  end
end
