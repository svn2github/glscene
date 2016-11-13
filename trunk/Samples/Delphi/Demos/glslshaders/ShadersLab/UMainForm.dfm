object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 581
  ClientWidth = 950
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 353
    Height = 581
    Align = alLeft
    TabOrder = 0
    object Label54: TLabel
      Left = 148
      Top = 502
      Width = 32
      Height = 13
      Caption = 'Object'
    end
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 351
      Height = 496
      ActivePage = TabSheet1
      Align = alTop
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Fur'
        object lblFurDistance: TLabel
          Left = 3
          Top = 53
          Width = 55
          Height = 13
          Caption = 'Fur Length '
        end
        object Label10: TLabel
          Left = 3
          Top = 205
          Width = 51
          Height = 13
          Caption = 'Blend Dest'
        end
        object Label11: TLabel
          Left = 3
          Top = 179
          Width = 44
          Height = 13
          Caption = 'Blend Src'
        end
        object lblFurPassCount1: TLabel
          Left = 3
          Top = 28
          Width = 57
          Height = 13
          Caption = 'Pass Count '
        end
        object lblFurLength: TLabel
          Left = 287
          Top = 53
          Width = 16
          Height = 13
          Caption = '0.3'
        end
        object Label7: TLabel
          Left = 3
          Top = 75
          Width = 59
          Height = 13
          Caption = 'Max Length '
        end
        object lblFurMaxLength: TLabel
          Left = 287
          Top = 76
          Width = 16
          Height = 13
          Caption = '3.0'
        end
        object lblFurPassCount: TLabel
          Left = 287
          Top = 28
          Width = 12
          Height = 13
          Caption = '16'
        end
        object Label12: TLabel
          Left = 3
          Top = 97
          Width = 36
          Height = 13
          Caption = 'Density'
        end
        object lblFurDensity: TLabel
          Left = 287
          Top = 97
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label6: TLabel
          Left = 3
          Top = 233
          Width = 56
          Height = 13
          Caption = 'Light Power'
        end
        object lblFurLightPower: TLabel
          Left = 287
          Top = 233
          Width = 16
          Height = 13
          Caption = '2.5'
        end
        object Label8: TLabel
          Left = 3
          Top = 263
          Width = 56
          Height = 13
          Caption = 'Color Scale '
        end
        object Label9: TLabel
          Left = 3
          Top = 285
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape1: TShape
          Left = 83
          Top = 262
          Width = 64
          Height = 15
          OnMouseDown = Shape1MouseDown
        end
        object Shape2: TShape
          Left = 83
          Top = 283
          Width = 64
          Height = 15
          OnMouseDown = Shape2MouseDown
        end
        object Label63: TLabel
          Left = 2
          Top = 129
          Width = 59
          Height = 13
          Caption = 'Gravity XYZ '
        end
        object chkFurShader: TCheckBox
          Left = 3
          Top = 3
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkFurShaderClick
        end
        object tbFurLength: TTrackBar
          Left = 65
          Top = 49
          Width = 216
          Height = 26
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbFurLengthChange
        end
        object cbxFurBlendSrc: TComboBox
          Left = 72
          Top = 176
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 3
          TabOrder = 2
          Text = 'ONE MINUS SRC COLOR'
          OnChange = cbxFurBlendSrcChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
        object cbxFurBlendDest: TComboBox
          Left = 72
          Top = 203
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 7
          TabOrder = 3
          Text = 'MINUS SRC ALPHA'
          OnChange = cbxFurBlendDestChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
        object chkAnimateFur: TCheckBox
          Left = 72
          Top = 3
          Width = 75
          Height = 17
          Caption = 'Animate Fur'
          TabOrder = 4
        end
        object tbFurPassCount: TTrackBar
          Left = 65
          Top = 24
          Width = 216
          Height = 23
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 8
          Position = 16
          TabOrder = 5
          TickStyle = tsNone
          OnChange = tbFurPassCountChange
        end
        object tbFurMaxLength: TTrackBar
          Left = 65
          Top = 72
          Width = 216
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 300
          TabOrder = 6
          TickStyle = tsNone
          OnChange = tbFurMaxLengthChange
        end
        object chkFurRandomLength: TCheckBox
          Left = 72
          Top = 154
          Width = 201
          Height = 17
          Caption = 'Random Fur Length'
          TabOrder = 7
          OnClick = chkFurRandomLengthClick
        end
        object tbFurDensity: TTrackBar
          Left = 65
          Top = 94
          Width = 216
          Height = 26
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 8
          TickStyle = tsNone
          OnChange = tbFurDensityChange
        end
        object tbFurLightPower: TTrackBar
          Left = 65
          Top = 230
          Width = 216
          Height = 26
          Max = 1000
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 250
          TabOrder = 9
          TickStyle = tsNone
          OnChange = tbFurLightPowerChange
        end
        object Button8: TButton
          Left = 3
          Top = 315
          Width = 184
          Height = 25
          Caption = 'Load Main Texture'
          TabOrder = 10
          OnClick = Button8Click
        end
        object Button9: TButton
          Left = 3
          Top = 346
          Width = 184
          Height = 25
          Caption = 'Load Noise Texture'
          TabOrder = 11
          OnClick = Button9Click
        end
        object edtFurGravityX: TEdit
          Left = 71
          Top = 124
          Width = 57
          Height = 21
          TabOrder = 12
          Text = '0.0'
          OnChange = edtFurGravityXChange
          OnKeyPress = EditFloatKeyPress
        end
        object edtFurGravityY: TEdit
          Left = 141
          Top = 124
          Width = 57
          Height = 21
          TabOrder = 13
          Text = '-2.0'
          OnChange = edtFurGravityYChange
          OnKeyPress = EditFloatKeyPress
        end
        object edtFurGravityZ: TEdit
          Left = 216
          Top = 124
          Width = 57
          Height = 21
          TabOrder = 14
          Text = '0.0'
          OnChange = edtFurGravityZChange
          OnKeyPress = EditFloatKeyPress
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Lattice'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblLatticeScaleX: TLabel
          Left = 291
          Top = 41
          Width = 12
          Height = 13
          Caption = '10'
        end
        object lblLatticeThresholdX: TLabel
          Left = 289
          Top = 93
          Width = 22
          Height = 13
          Caption = '0.15'
        end
        object lblLatticeScaleY: TLabel
          Left = 290
          Top = 67
          Width = 12
          Height = 13
          Caption = '40'
        end
        object Label4: TLabel
          Left = 16
          Top = 147
          Width = 74
          Height = 13
          Caption = 'Specular Power'
        end
        object Label5: TLabel
          Left = 16
          Top = 172
          Width = 56
          Height = 13
          Caption = 'Light Power'
        end
        object lblLatticeThresholdY: TLabel
          Left = 290
          Top = 118
          Width = 22
          Height = 13
          Caption = '0.30'
        end
        object lblLatticeSpecularPower: TLabel
          Left = 290
          Top = 147
          Width = 16
          Height = 13
          Caption = '8.0'
        end
        object lblLatticeLightPower: TLabel
          Left = 290
          Top = 173
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label23: TLabel
          Left = 10
          Top = 41
          Width = 34
          Height = 13
          Caption = 'Scale X'
        end
        object Label29: TLabel
          Left = 10
          Top = 67
          Width = 34
          Height = 13
          Caption = 'Scale Y'
        end
        object Label31: TLabel
          Left = 10
          Top = 93
          Width = 56
          Height = 13
          Caption = 'Threshold X'
        end
        object Label33: TLabel
          Left = 10
          Top = 122
          Width = 56
          Height = 13
          Caption = 'Threshold Y'
        end
        object Label35: TLabel
          Left = 10
          Top = 200
          Width = 62
          Height = 13
          Caption = 'Diffuse Color'
        end
        object Shape10: TShape
          Left = 90
          Top = 198
          Width = 64
          Height = 15
          OnMouseDown = Shape10MouseDown
        end
        object Label38: TLabel
          Left = 10
          Top = 223
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape11: TShape
          Left = 90
          Top = 222
          Width = 64
          Height = 15
          Brush.Color = 1381653
          OnMouseDown = Shape11MouseDown
        end
        object Label39: TLabel
          Left = 10
          Top = 244
          Width = 69
          Height = 13
          Caption = 'Specular Color'
        end
        object Shape12: TShape
          Left = 90
          Top = 244
          Width = 64
          Height = 15
          OnMouseDown = Shape12MouseDown
        end
        object tbLatticeScaleX: TTrackBar
          Left = 73
          Top = 37
          Width = 215
          Height = 30
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 10
          TabOrder = 0
          TickStyle = tsNone
          OnChange = tbLatticeScaleXChange
        end
        object tbLatticeThresholdX: TTrackBar
          Left = 73
          Top = 89
          Width = 215
          Height = 27
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 15
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbLatticeThresholdXChange
        end
        object chkLatticeShader: TCheckBox
          Left = 16
          Top = 12
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 2
          OnClick = chkLatticeShaderClick
        end
        object tbLatticeScaleY: TTrackBar
          Left = 73
          Top = 63
          Width = 215
          Height = 32
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 40
          TabOrder = 3
          TickStyle = tsNone
          OnChange = tbLatticeScaleYChange
        end
        object tbLatticeThresholdY: TTrackBar
          Left = 73
          Top = 114
          Width = 211
          Height = 27
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 4
          TickStyle = tsNone
          OnChange = tbLatticeThresholdYChange
        end
        object tbLatticeSpecularPower: TTrackBar
          Left = 96
          Top = 143
          Width = 188
          Height = 32
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 800
          TabOrder = 5
          TickStyle = tsNone
          OnChange = tbLatticeSpecularPowerChange
        end
        object tbLatticeLightPower: TTrackBar
          Left = 96
          Top = 166
          Width = 188
          Height = 32
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 6
          TickStyle = tsNone
          OnChange = tbLatticeLightPowerChange
        end
        object Button7: TButton
          Left = 16
          Top = 275
          Width = 184
          Height = 25
          Caption = 'Load Main Texture'
          TabOrder = 7
          OnClick = Button3Click
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Erosion'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label1: TLabel
          Left = 8
          Top = 46
          Width = 67
          Height = 13
          Caption = 'Erosion factor'
        end
        object lblErosionFactor: TLabel
          Left = 295
          Top = 47
          Width = 22
          Height = 13
          Caption = '0.35'
        end
        object Label3: TLabel
          Left = 8
          Top = 79
          Width = 63
          Height = 13
          Caption = 'Erosion Scale'
        end
        object lblErosionScale: TLabel
          Left = 295
          Top = 79
          Width = 22
          Height = 13
          Caption = '0.03'
        end
        object Label25: TLabel
          Left = 8
          Top = 111
          Width = 84
          Height = 13
          Caption = 'Intensity factor 1'
        end
        object lblErosionIFactor1: TLabel
          Left = 295
          Top = 111
          Width = 22
          Height = 13
          Caption = '0.75'
        end
        object Label28: TLabel
          Left = 8
          Top = 142
          Width = 86
          Height = 13
          Caption = 'Intensity Factor 2'
        end
        object lblerosionIFactor2: TLabel
          Left = 295
          Top = 143
          Width = 22
          Height = 13
          Caption = '1.95'
        end
        object Label2: TLabel
          Left = 8
          Top = 200
          Width = 71
          Height = 13
          Caption = 'Ambient factor'
        end
        object lblErosionAmbientF: TLabel
          Left = 295
          Top = 200
          Width = 22
          Height = 13
          Caption = '0.80'
        end
        object Label27: TLabel
          Left = 8
          Top = 172
          Width = 66
          Height = 13
          Caption = 'Diffuse factor'
        end
        object lblErosionDiffuseF: TLabel
          Left = 295
          Top = 173
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label30: TLabel
          Left = 8
          Top = 228
          Width = 73
          Height = 13
          Caption = 'Specular factor'
        end
        object lblErosionSpecularF: TLabel
          Left = 295
          Top = 228
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label32: TLabel
          Left = 8
          Top = 260
          Width = 97
          Height = 13
          Caption = 'Specular Roughness'
        end
        object lblErosionSpecularR: TLabel
          Left = 297
          Top = 260
          Width = 22
          Height = 13
          Caption = '0.45'
        end
        object Label34: TLabel
          Left = 8
          Top = 292
          Width = 109
          Height = 13
          Caption = 'Anisotropic Roughness'
        end
        object lblErosionAnisoR: TLabel
          Left = 297
          Top = 292
          Width = 22
          Height = 13
          Caption = '0.35'
        end
        object Label36: TLabel
          Left = 6
          Top = 327
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape8: TShape
          Left = 86
          Top = 325
          Width = 64
          Height = 15
          Brush.Color = 2105376
          OnMouseDown = Shape8MouseDown
        end
        object Shape9: TShape
          Left = 86
          Top = 346
          Width = 64
          Height = 15
          Brush.Color = 14540253
          OnMouseDown = Shape9MouseDown
        end
        object Label37: TLabel
          Left = 6
          Top = 346
          Width = 69
          Height = 13
          Caption = 'Specular Color'
        end
        object chkErosionShader: TCheckBox
          Left = 24
          Top = 20
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkErosionShaderClick
        end
        object tbErosionFactor: TTrackBar
          Left = 98
          Top = 43
          Width = 191
          Height = 26
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 35
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbErosionFactorChange
        end
        object tberosionScale: TTrackBar
          Left = 98
          Top = 75
          Width = 191
          Height = 26
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 3
          TabOrder = 2
          TickStyle = tsNone
          OnChange = tberosionScaleChange
        end
        object tbErosionIFactor1: TTrackBar
          Left = 98
          Top = 107
          Width = 191
          Height = 26
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 75
          TabOrder = 3
          TickStyle = tsNone
          OnChange = tbErosionIFactor1Change
        end
        object tbErosionIFactor2: TTrackBar
          Left = 100
          Top = 139
          Width = 189
          Height = 26
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 195
          TabOrder = 4
          TickStyle = tsNone
          OnChange = tbErosionIFactor2Change
        end
        object tbErosionAmbientF: TTrackBar
          Left = 98
          Top = 194
          Width = 191
          Height = 28
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 5
          TickStyle = tsNone
          OnChange = tbErosionAmbientFChange
        end
        object tbErosionDiffuseF: TTrackBar
          Left = 99
          Top = 169
          Width = 190
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 6
          TickStyle = tsNone
          OnChange = tbErosionDiffuseFChange
        end
        object tbErosionSpecularF: TTrackBar
          Left = 98
          Top = 224
          Width = 191
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 7
          TickStyle = tsNone
          OnChange = tbErosionSpecularFChange
        end
        object tbErosionSpecularR: TTrackBar
          Left = 111
          Top = 256
          Width = 180
          Height = 26
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 45
          TabOrder = 8
          TickStyle = tsNone
          OnChange = tbErosionSpecularRChange
        end
        object tbErosionAnisoR: TTrackBar
          Left = 123
          Top = 288
          Width = 166
          Height = 26
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 35
          TabOrder = 9
          TickStyle = tsNone
          OnChange = tbErosionAnisoRChange
        end
        object Button5: TButton
          Left = 3
          Top = 367
          Width = 184
          Height = 25
          Caption = 'Load Main Texture'
          TabOrder = 10
          OnClick = Button5Click
        end
        object Button6: TButton
          Left = 3
          Top = 398
          Width = 184
          Height = 25
          Caption = 'Load Erosion Texture'
          TabOrder = 11
          OnClick = Button6Click
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Ivory'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object chkIvoryShader: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkIvoryShaderClick
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Gootch'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label13: TLabel
          Left = 16
          Top = 47
          Width = 62
          Height = 13
          Caption = 'Diffuse Color'
        end
        object Shape3: TShape
          Left = 96
          Top = 46
          Width = 64
          Height = 15
          Brush.Color = clSilver
          OnMouseDown = Shape3MouseDown
        end
        object Label14: TLabel
          Left = 16
          Top = 67
          Width = 56
          Height = 13
          Caption = 'Warm Color'
        end
        object Shape4: TShape
          Left = 96
          Top = 67
          Width = 64
          Height = 15
          Brush.Color = clFuchsia
          OnMouseDown = Shape4MouseDown
        end
        object Label15: TLabel
          Left = 16
          Top = 88
          Width = 49
          Height = 13
          Caption = 'Cool Color'
        end
        object Shape5: TShape
          Left = 96
          Top = 88
          Width = 64
          Height = 15
          Brush.Color = 1145343
          OnMouseDown = Shape5MouseDown
        end
        object Label16: TLabel
          Left = 16
          Top = 110
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape6: TShape
          Left = 96
          Top = 109
          Width = 64
          Height = 15
          Brush.Color = 3158064
          OnMouseDown = Shape6MouseDown
        end
        object Label17: TLabel
          Left = 16
          Top = 131
          Width = 69
          Height = 13
          Caption = 'Specular Color'
        end
        object Shape7: TShape
          Left = 96
          Top = 130
          Width = 64
          Height = 15
          OnMouseDown = Shape7MouseDown
        end
        object Label18: TLabel
          Left = 16
          Top = 163
          Width = 66
          Height = 13
          Caption = 'Diffuse factor'
        end
        object lblGoochDFactor: TLabel
          Left = 303
          Top = 164
          Width = 22
          Height = 13
          Caption = '0.80'
        end
        object Label20: TLabel
          Left = 16
          Top = 195
          Width = 60
          Height = 13
          Caption = 'Warm factor'
        end
        object lblGoochWFactor: TLabel
          Left = 303
          Top = 196
          Width = 22
          Height = 13
          Caption = '0.55'
        end
        object Label22: TLabel
          Left = 16
          Top = 227
          Width = 53
          Height = 13
          Caption = 'Cool factor'
        end
        object lblGoochCFactor: TLabel
          Left = 303
          Top = 228
          Width = 22
          Height = 13
          Caption = '0.30'
        end
        object Label24: TLabel
          Left = 16
          Top = 259
          Width = 71
          Height = 13
          Caption = 'Ambient factor'
        end
        object lblGoochAFactor: TLabel
          Left = 303
          Top = 260
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label26: TLabel
          Left = 16
          Top = 292
          Width = 73
          Height = 13
          Caption = 'Specular factor'
        end
        object lblGoochSFactor: TLabel
          Left = 303
          Top = 292
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label40: TLabel
          Left = 16
          Top = 323
          Width = 55
          Height = 13
          Caption = 'Blend Mode'
        end
        object Label41: TLabel
          Left = 164
          Top = 47
          Width = 27
          Height = 13
          Caption = 'Alpha'
        end
        object lblGoochAlpha: TLabel
          Left = 309
          Top = 47
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object chkGoochShader: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkGoochShaderClick
        end
        object tbGoochDFactor: TTrackBar
          Left = 96
          Top = 160
          Width = 201
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbGoochDFactorChange
        end
        object tbGoochWFactor: TTrackBar
          Left = 96
          Top = 192
          Width = 201
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 55
          TabOrder = 2
          TickStyle = tsNone
          OnChange = tbGoochWFactorChange
        end
        object tbGoochCFactor: TTrackBar
          Left = 96
          Top = 224
          Width = 201
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 3
          TickStyle = tsNone
          OnChange = tbGoochCFactorChange
        end
        object tbGoochAFactor: TTrackBar
          Left = 96
          Top = 256
          Width = 201
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 4
          TickStyle = tsNone
          OnChange = tbGoochAFactorChange
        end
        object tbGoochSFactor: TTrackBar
          Left = 96
          Top = 288
          Width = 201
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 5
          TickStyle = tsNone
          OnChange = tbGoochSFactorChange
        end
        object cbxGootchBlendMode: TComboBox
          Left = 104
          Top = 320
          Width = 193
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 6
          Text = 'Opaque'
          OnChange = cbxGootchBlendModeChange
          Items.Strings = (
            'Opaque'
            'Transparency'
            'Additive'
            'AlphaTest50'
            'AlphaTest100'
            'Modulate'
            'DestColorOne'
            'DestAlphaOne')
        end
        object tbGoochAlpha: TTrackBar
          Left = 196
          Top = 44
          Width = 114
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 7
          TickStyle = tsNone
          OnChange = tbGoochAlphaChange
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'S.E.M'
        ImageIndex = 5
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label19: TLabel
          Left = 16
          Top = 44
          Width = 66
          Height = 13
          Caption = 'Diffuse factor'
        end
        object lblSemDiffuseF: TLabel
          Left = 303
          Top = 45
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label42: TLabel
          Left = 16
          Top = 72
          Width = 71
          Height = 13
          Caption = 'Ambient factor'
        end
        object lblSemAmbientF: TLabel
          Left = 303
          Top = 71
          Width = 22
          Height = 13
          Caption = '0.80'
        end
        object Label44: TLabel
          Left = 16
          Top = 100
          Width = 73
          Height = 13
          Caption = 'Specular factor'
        end
        object lblSemSpecularF: TLabel
          Left = 303
          Top = 100
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label46: TLabel
          Left = 14
          Top = 130
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape13: TShape
          Left = 94
          Top = 128
          Width = 64
          Height = 15
          Brush.Color = 2105376
          OnMouseDown = Shape13MouseDown
        end
        object Label47: TLabel
          Left = 14
          Top = 149
          Width = 69
          Height = 13
          Caption = 'Specular Color'
        end
        object Shape14: TShape
          Left = 94
          Top = 149
          Width = 64
          Height = 15
          Brush.Color = 14540253
          OnMouseDown = Shape14MouseDown
        end
        object tbSemDiffuseF: TTrackBar
          Left = 107
          Top = 39
          Width = 190
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 0
          TickStyle = tsNone
          OnChange = tbSemDiffuseFChange
        end
        object tbSemAmbientF: TTrackBar
          Left = 106
          Top = 66
          Width = 191
          Height = 28
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbSemAmbientFChange
        end
        object tbSemSpecularF: TTrackBar
          Left = 106
          Top = 96
          Width = 191
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 2
          TickStyle = tsNone
          OnChange = tbSemSpecularFChange
        end
        object chkSEMShader: TCheckBox
          Left = 24
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 3
          OnClick = chkSEMShaderClick
        end
        object Button4: TButton
          Left = 16
          Top = 179
          Width = 184
          Height = 25
          Caption = 'Load MatCap Texture'
          TabOrder = 4
          OnClick = Button4Click
        end
      end
      object Displacement: TTabSheet
        Caption = 'Displacement'
        ImageIndex = 6
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label21: TLabel
          Left = 24
          Top = 52
          Width = 66
          Height = 13
          Caption = 'Diffuse factor'
        end
        object Label43: TLabel
          Left = 24
          Top = 80
          Width = 71
          Height = 13
          Caption = 'Ambient factor'
        end
        object Label45: TLabel
          Left = 24
          Top = 108
          Width = 73
          Height = 13
          Caption = 'Specular factor'
        end
        object lblVDSpecularF: TLabel
          Left = 311
          Top = 108
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object lblVDAmbientF: TLabel
          Left = 311
          Top = 79
          Width = 22
          Height = 13
          Caption = '0.80'
        end
        object lblVDDiffuseF: TLabel
          Left = 311
          Top = 53
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label51: TLabel
          Left = 22
          Top = 138
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape15: TShape
          Left = 102
          Top = 136
          Width = 64
          Height = 15
          Brush.Color = 2105376
          OnMouseDown = Shape13MouseDown
        end
        object Label52: TLabel
          Left = 22
          Top = 157
          Width = 69
          Height = 13
          Caption = 'Specular Color'
        end
        object Shape16: TShape
          Left = 102
          Top = 157
          Width = 64
          Height = 15
          Brush.Color = 14540253
          OnMouseDown = Shape14MouseDown
        end
        object Label48: TLabel
          Left = 22
          Top = 188
          Width = 26
          Height = 13
          Caption = 'Noise'
        end
        object lblVDNoise: TLabel
          Left = 311
          Top = 188
          Width = 22
          Height = 13
          Caption = '10.0'
        end
        object Label49: TLabel
          Left = 22
          Top = 211
          Width = 30
          Height = 13
          Caption = 'Period'
        end
        object lblVDPeriod: TLabel
          Left = 311
          Top = 211
          Width = 16
          Height = 13
          Caption = '5.0'
        end
        object Label53: TLabel
          Left = 22
          Top = 236
          Width = 54
          Height = 13
          Caption = 'Noise Scale'
        end
        object lblVDNScale: TLabel
          Left = 311
          Top = 237
          Width = 22
          Height = 13
          Caption = '0.05'
        end
        object Label55: TLabel
          Left = 22
          Top = 260
          Width = 53
          Height = 13
          Caption = 'Turbulence'
        end
        object lblVDTurb: TLabel
          Left = 311
          Top = 261
          Width = 16
          Height = 13
          Caption = '0.5'
        end
        object Label57: TLabel
          Left = 22
          Top = 284
          Width = 91
          Height = 13
          Caption = 'Displacement Scale'
        end
        object lblVDDispScale: TLabel
          Left = 311
          Top = 284
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label50: TLabel
          Left = 22
          Top = 307
          Width = 56
          Height = 13
          Caption = 'Time Factor'
        end
        object lblVDTimeF: TLabel
          Left = 311
          Top = 307
          Width = 22
          Height = 13
          Caption = '0.05'
        end
        object chkVDShader: TCheckBox
          Left = 32
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkVDShaderClick
        end
        object tbVDDiffuseF: TTrackBar
          Left = 115
          Top = 47
          Width = 190
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbVDDiffuseFChange
        end
        object tbVDAmbientF: TTrackBar
          Left = 114
          Top = 74
          Width = 191
          Height = 28
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 2
          TickStyle = tsNone
          OnChange = tbVDAmbientFChange
        end
        object tbVDSpecularF: TTrackBar
          Left = 114
          Top = 104
          Width = 191
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 3
          TickStyle = tsNone
          OnChange = tbVDSpecularFChange
        end
        object chkVDAnimate: TCheckBox
          Left = 128
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Animate'
          TabOrder = 4
        end
        object tbVDNoise: TTrackBar
          Left = 114
          Top = 184
          Width = 191
          Height = 26
          Max = 5000
          PageSize = 10
          Frequency = 10
          Position = 1000
          TabOrder = 5
          TickStyle = tsNone
          OnChange = tbVDNoiseChange
        end
        object tbVDPeriod: TTrackBar
          Left = 114
          Top = 207
          Width = 191
          Height = 26
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 500
          TabOrder = 6
          TickStyle = tsNone
          OnChange = tbVDPeriodChange
        end
        object tbVDNScale: TTrackBar
          Left = 114
          Top = 230
          Width = 191
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 5
          TabOrder = 7
          TickStyle = tsNone
          OnChange = tbVDNScaleChange
        end
        object tbVDTurb: TTrackBar
          Left = 114
          Top = 256
          Width = 191
          Height = 26
          Max = 200
          PageSize = 10
          Frequency = 10
          Position = 50
          TabOrder = 8
          TickStyle = tsNone
          OnChange = tbVDTurbChange
        end
        object tbVDDispScale: TTrackBar
          Left = 114
          Top = 280
          Width = 191
          Height = 26
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 9
          TickStyle = tsNone
          OnChange = tbVDDispScaleChange
        end
        object tbVDTimeF: TTrackBar
          Left = 114
          Top = 303
          Width = 191
          Height = 26
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 5
          TabOrder = 10
          TickStyle = tsNone
          OnChange = tbVDTimeFChange
        end
        object Button3: TButton
          Left = 16
          Top = 344
          Width = 113
          Height = 25
          Caption = 'Reset Time'
          TabOrder = 11
          OnClick = Button3Click
        end
        object Button1: TButton
          Left = 16
          Top = 375
          Width = 184
          Height = 25
          Caption = 'Load Main Texture'
          TabOrder = 12
          OnClick = Button1Click
        end
      end
      object TabSheet7: TTabSheet
        Caption = 'Glass'
        ImageIndex = 7
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label56: TLabel
          Left = 11
          Top = 36
          Width = 29
          Height = 13
          Caption = 'Depth'
        end
        object Label58: TLabel
          Left = 24
          Top = 66
          Width = 16
          Height = 13
          Caption = 'Mix'
        end
        object Label59: TLabel
          Left = 14
          Top = 146
          Width = 62
          Height = 13
          Caption = 'Diffuse Color'
        end
        object Shape17: TShape
          Left = 87
          Top = 145
          Width = 64
          Height = 15
          Brush.Color = 2105376
          OnMouseDown = Shape17MouseDown
        end
        object lblGlassDepth: TLabel
          Left = 285
          Top = 36
          Width = 16
          Height = 13
          Caption = '0.1'
        end
        object lblGlassMix: TLabel
          Left = 283
          Top = 66
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label60: TLabel
          Left = 10
          Top = 99
          Width = 27
          Height = 13
          Caption = 'Alpha'
        end
        object lblGlassAlpha: TLabel
          Left = 282
          Top = 99
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label61: TLabel
          Left = 18
          Top = 179
          Width = 44
          Height = 13
          Caption = 'Blend Src'
        end
        object Label62: TLabel
          Left = 18
          Top = 205
          Width = 51
          Height = 13
          Caption = 'Blend Dest'
        end
        object tbGlassDepth: TTrackBar
          Left = 46
          Top = 33
          Width = 235
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 10
          TabOrder = 0
          TickStyle = tsNone
          OnChange = tbGlassDepthChange
        end
        object tbGlassMix: TTrackBar
          Left = 45
          Top = 62
          Width = 236
          Height = 28
          Max = 200
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbGlassMixChange
        end
        object Button10: TButton
          Left = 3
          Top = 254
          Width = 184
          Height = 25
          Caption = 'Load Refraction Texture'
          TabOrder = 2
          OnClick = Button10Click
        end
        object chkGlassShader: TCheckBox
          Left = 24
          Top = 8
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 3
          OnClick = chkGlassShaderClick
        end
        object tbGlassAlpha: TTrackBar
          Left = 45
          Top = 96
          Width = 235
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 4
          TickStyle = tsNone
          OnChange = tbGlassAlphaChange
        end
        object cbxGlassBlendSrc: TComboBox
          Left = 87
          Top = 176
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 6
          TabOrder = 5
          Text = 'SRC ALPHA'
          OnChange = cbxGlassBlendSrcChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
        object cbxGlassBlendDst: TComboBox
          Left = 87
          Top = 203
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 8
          TabOrder = 6
          Text = 'DST ALPHA'
          OnChange = cbxGlassBlendDstChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
      end
      object TabSheet8: TTabSheet
        Caption = 'Toon'
        ImageIndex = 8
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label64: TLabel
          Left = 7
          Top = 51
          Width = 66
          Height = 13
          Caption = 'HighLight Size'
        end
        object lblToonHighlightSize: TLabel
          Left = 290
          Top = 51
          Width = 22
          Height = 13
          Caption = '0.95'
        end
        object Label66: TLabel
          Left = 7
          Top = 73
          Width = 38
          Height = 13
          Caption = 'Mid Size'
        end
        object lblToonMidSize: TLabel
          Left = 290
          Top = 73
          Width = 16
          Height = 13
          Caption = '0.5'
        end
        object Label68: TLabel
          Left = 7
          Top = 95
          Width = 60
          Height = 13
          Caption = 'Shadow Size'
        end
        object lblToonShadowSize: TLabel
          Left = 290
          Top = 95
          Width = 22
          Height = 13
          Caption = '0.25'
        end
        object Label70: TLabel
          Left = 7
          Top = 117
          Width = 65
          Height = 13
          Caption = 'Outline Width'
        end
        object lblToonOutlineWidth: TLabel
          Left = 290
          Top = 117
          Width = 22
          Height = 13
          Caption = '0.25'
        end
        object Label72: TLabel
          Left = 8
          Top = 146
          Width = 69
          Height = 13
          Caption = 'Highlight Color'
        end
        object Shape18: TShape
          Left = 118
          Top = 146
          Width = 64
          Height = 15
          Brush.Color = 15658734
          OnMouseDown = Shape18MouseDown
        end
        object Label73: TLabel
          Left = 8
          Top = 167
          Width = 44
          Height = 13
          Caption = 'Mid Color'
        end
        object Shape19: TShape
          Left = 118
          Top = 167
          Width = 64
          Height = 15
          Brush.Color = 13421772
          OnMouseDown = Shape19MouseDown
        end
        object Label74: TLabel
          Left = 8
          Top = 188
          Width = 104
          Height = 13
          Caption = 'Lighten Shadow Color'
        end
        object Shape20: TShape
          Left = 118
          Top = 188
          Width = 64
          Height = 15
          Brush.Color = clGray
          OnMouseDown = Shape20MouseDown
        end
        object Label75: TLabel
          Left = 8
          Top = 209
          Width = 103
          Height = 13
          Caption = 'Darken Shadow Color'
        end
        object Shape21: TShape
          Left = 118
          Top = 209
          Width = 64
          Height = 15
          Brush.Color = 3158064
          OnMouseDown = Shape21MouseDown
        end
        object Label76: TLabel
          Left = 7
          Top = 230
          Width = 62
          Height = 13
          Caption = 'Outline Color'
        end
        object Shape22: TShape
          Left = 118
          Top = 230
          Width = 64
          Height = 15
          Brush.Color = clBlack
          OnMouseDown = Shape22MouseDown
        end
        object chkToonShader: TCheckBox
          Left = 8
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkToonShaderClick
        end
        object tbToonHighlightSize: TTrackBar
          Left = 75
          Top = 48
          Width = 213
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 95
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbToonHighlightSizeChange
        end
        object tbToonMidSize: TTrackBar
          Left = 75
          Top = 70
          Width = 213
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 50
          TabOrder = 2
          TickStyle = tsNone
          OnChange = tbToonMidSizeChange
        end
        object tbToonShadowSize: TTrackBar
          Left = 75
          Top = 92
          Width = 213
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 25
          TabOrder = 3
          TickStyle = tsNone
          OnChange = tbToonShadowSizeChange
        end
        object tbToonOutlineWidth: TTrackBar
          Left = 75
          Top = 114
          Width = 213
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 25
          TabOrder = 4
          TickStyle = tsNone
          OnChange = tbToonOutlineWidthChange
        end
      end
    end
    object chkAnimScene: TCheckBox
      Left = 13
      Top = 499
      Width = 97
      Height = 17
      Caption = 'Animate Scene'
      TabOrder = 1
    end
    object chkLightmoving: TCheckBox
      Left = 13
      Top = 522
      Width = 97
      Height = 17
      Caption = 'Light moving'
      TabOrder = 2
    end
    object cbxObjects: TComboBox
      Left = 187
      Top = 497
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'Suzanne'
      OnChange = cbxObjectsChange
      Items.Strings = (
        'Suzanne'
        'Knot'
        'Spoutnik'
        'Rectangle Spirale'
        'Geode'
        'Syamil'
        'GLTorus'
        'GLSphere')
    end
    object Button2: TButton
      Left = 148
      Top = 524
      Width = 184
      Height = 25
      Caption = 'Choose Background Color'
      TabOrder = 4
      OnClick = Button2Click
    end
    object chkBackgroundImg: TCheckBox
      Left = 128
      Top = 555
      Width = 14
      Height = 17
      Hint = 'Show Background Texture'
      TabOrder = 5
      OnClick = chkBackgroundImgClick
    end
    object Button11: TButton
      Left = 148
      Top = 551
      Width = 184
      Height = 25
      Caption = 'Load Background Texture'
      Enabled = False
      TabOrder = 6
      OnClick = Button11Click
    end
  end
  object Viewer: TGLSceneViewer
    Left = 353
    Top = 0
    Width = 597
    Height = 581
    Camera = Camera
    Buffer.BackgroundColor = clBlack
    Buffer.AntiAliasing = aa2x
    FieldOfView = 160.468215942382800000
    Align = alClient
    TabOrder = 1
  end
  object MaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'MainTexture'
        Tag = 0
        Material.BackProperties.Shininess = 10
        Material.FrontProperties.Shininess = 10
        Material.FrontProperties.Specular.Color = {9998983E9998983E9998983E0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.Image.Picture.Data = {
          0A544A504547496D616765D9240000FFD8FFE000104A46494600010101004800
          480000FFDB004300080606070605080707070909080A0C140D0C0B0B0C191213
          0F141D1A1F1E1D1A1C1C20242E2720222C231C1C2837292C30313434341F2739
          3D38323C2E333432FFDB0043010909090C0B0C180D0D1832211C213232323232
          3232323232323232323232323232323232323232323232323232323232323232
          32323232323232323232323232FFC00011080100010003012200021101031101
          FFC4001F0000010501010101010100000000000000000102030405060708090A
          0BFFC400B5100002010303020403050504040000017D01020300041105122131
          410613516107227114328191A1082342B1C11552D1F02433627282090A161718
          191A25262728292A3435363738393A434445464748494A535455565758595A63
          6465666768696A737475767778797A838485868788898A92939495969798999A
          A2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6
          D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301
          01010101010101010000000000000102030405060708090A0BFFC400B5110002
          0102040403040705040400010277000102031104052131061241510761711322
          328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728
          292A35363738393A434445464748494A535455565758595A636465666768696A
          737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7
          A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3
          E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00C733
          FCDD6B46CA71C735CD35C73D6AED9DD618735E6D5A1789F5519392D4EB3CCCAD
          304B86AAB6F36F51CD4A579CD79EA924ECCB853B3B9A704B915714E6B220241A
          D2889C55CD251D0DDA2D2F153A4BB6ABAD0DD2BC2C4455CC670D0B125C8DB8CD
          62EA18914D4F3B9515973DCF504D65461ADD1CC958E6354B2DC49C5737736441
          3C576F74CAE0D61DD46BCD7D0616BCA2AC5A39296D88ED551E3C56FDCC6BCD66
          4D18C9AF5E954E61357334AF34A16A664E69315D1C97212B111149B79A9B142A
          64D1C9606AE22479AB315B927A53A28C6456A5B44BC573D49F2969115BD99247
          15D1E9964430E2A1B58578ADCB4DA8057938AC449AB0D9D069AA23515BB1DC00
          BD6B9882E71800D6A41216AF9FAD0D6EC86AE6A3CBB8556719A17A50D57878AB
          9D108684678AAB34B81534A702B3A7626BDEA6AF1B1BA446F2E5A9C1F0B55C03
          9E69934BB17AD44A926EC8C274EEC86F67001E6B21EE06EEB497D75C9E6B29A7
          39EB5E850C3D910DF2EC57049AB3039522AB21156E28F70E2BD3F669C7534845
          3D8D9B2BAC6326B6A0955C0E6B968C321AD2B5B82A464D7975E8754744558E9A
          1506AF46B8158F69741B15AD14A08AF22BF3C772A48B1D2826985B8A864936D7
          9B24E4EC6321B7232A6B9DBD0CA4D7405C30AA17700914D6D423CAF530924CE5
          A69C8CE6B2AE6E4F35BB7B6240240AE7AEADD813C57B7429C5EA657B19F34F9A
          A32499AB53C44566CB9535E9D2822653B0AC734C34C0F4F1CD75256214D30A7A
          F14C3C530BE286AE1CE9176393157A09B1593192C6B4208C9AE6AB045C67736A
          DAE7A56AC370481CD625B40C48E2B7ECAC99B048AF2EBD38AD4ABDCD2B32CE45
          7456A30A3359B676C100AD2570A2BC4AF1E676469148BA0D1D6AB24BBAA60DC5
          6114E2CDE23645C8AA132015764900159777721735E9D094E5A2378A209A4540
          6B22F2EB8201A75DDC96CE0D65CA59CD7AD42837AB264AE53B872E4D546AB924
          781CD557C0AF51534A3A1CF38A5B95A29326B52D1F902B1E2420F35A31064008
          AEDAF4B936D8CE849DB53A18605900A91ACD97902A8E9F7986018D7490149133
          C1AF1EB465177475292667DB178DC039ADA8189C542204DDD2AD243B704579B5
          EAD39BB1B37A16D09C7348E9B85119CD4C3915E656A0E0F991CD3927A1953B18
          B26AA7DB549C356A5DC2190F15CB5F6E818920D7A185A11AF1B2DCF3EACDD377
          34E431CA87A561DDC280938A8E3BC66C856AAF717FB32AF5D7430F284B9599CA
          A292BC4CBBC8D4E71583749826B62EEE108254D624D36E6E6BD78E1DC55CCA35
          94B46526254D3926C1E6A578C3AE4552752A6BA211535630AB3707745979C1A6
          03B8D40A0B1ABB1C41572689C5410539CA6CB16CB922B7ACE1518DD58114A158
          62B6ACE65C02CD5CF2A129EA6F2ACA3A23A4B4B7438C0ADE85638D074AE620BF
          5E153AD5A92F5D40DCD5E462284A52E589AC2A24AECE87ED8ABC03566190CB8A
          E76CDDA7618C9AEA6CA0DA8322B9711878D08EBB9A53A8EA3D0B11A6053D9B03
          8A7E00151C8702BCDA545D495CF420D2D0A53BB0CD62DD3B3B102B71E22FCF6A
          A8D6E9BFA57A94274E0EC74A7A18A2D59F922A396DC4639ADB955634CF1581A8
          5D804806BD2A4A52662E4919976D8240ACA964C1ABB3167C9ACC9C1CF15EBD0A
          5CE72D793B68591346FCF4357628D9932BCD73C1F2D906B634DBE28429E4575E
          2B0F2E4BA228D4B968868DB3C835BDA55E80A14B7351431437A98030D5564B19
          ECA60E8095CD78FCF09A74DE8CDE575AD8ECA1C48A09AB6146CC66B0F4FD443A
          AA38C1AD17790382BC835F2D8DC3CE150D21372562D2A95E94EF3C467E6A6C6E
          4E334DB887CE5C77ADA8D2A9285A461522D3BA256963917E520D64EA566B7084
          014D92DAE2D81652714906A0AE7CB938353858D4854BC19CD3A9092E591C95DD
          94B6AE4A1AC89DE49490C0E6BBBBEB2F3B2E8722B1A3B2877B2480035F590E5A
          91526B53CD57A727D8E1EEA37524826B35D981E6BAED4F4E09236CE95CDDC404
          120AD77C20DC4E4A951295D15125ED4C9727B5348DAC714E8E600E1AB2F64D3B
          A35956528D9891641E952BCBDB34C9265FE1151F2E46693A526EEC2159463A13
          A39CF15A16C2462324E2AB5BC24E30B5D0E9DA7991D77702B69C1A8E86509A94
          EEC4B791A12300935A96F6F35E382DD2AEC9A7C29B15002D5AFA7D814C337005
          7049469A73EA75C9BA8D25B16F4AB01020C8ADC46441C902B227BE4846C4E5A9
          8A973743392057CA62FDAD4A979E88F469CE105CB1DCD933AB9C2F34C70CD51D
          AC0615C3726A577C53A94EA429DA274D38B6EEC36FC98AA92811826944B234A7
          B28AA57DA8244ACBD4D73E1284E753CCE99CDC519DA9DE61080706B9C62D2367
          AD5D304F7D704E085CD597B58AD132FD6BEAD4A14E2A1BB328DE4EF631E44609
          C8C0AA123463A9AB7A95E83955E05604B21CE49AF6307424E376615AA5892187
          2D8356151A0901ED562111C91861F7AB423D3669A1DE172057A75B10A3A4B608
          514A29A65ED0EF23F302B1C135D8C71452A0CE18579BB44D6FF302430AE8F43D
          55D942BB6715F398CC04652F6B4CE95CEB46749FD9F1170CAB8AB8B1E0628B49
          E2B98FE53CD4FF0028EB5E5D6C2F36FD088CECFCC40A00CD0A46693393C528DA
          BD6BAA8E193A679F88C5F2CBDED110DE2B188E2B89BDBAF22ECEEF979AEE9D81
          FA561EB1A2C7791965186ACF0F8354EA36D6E72625AAB0738743362D481B7CAB
          678AC5BAD5079A777069AFA5DEDA16DB92B58F7FBCB61D4835EFE128D39368F1
          29579C1F24996AE6FBCCE55B359AF711B92AE0544F6F2469BD5B22A92A492C87
          35D526A2AC82A4F5B91DC4019CECAA0C8C8D8615A786824F9AA3955657E95929
          46F77B14AAA68CF0A5D8002AF5BDB9DCBBF814B1C6B1C9D2A562D2C984A1B8DE
          EB61FB5491A4924516D55009AD3B5BDD841638AE6996489C77357238E6997767
          02B58B525664427ADCEA6DF545F3473935BA751FF47C96C715C259128F800B1A
          D81697D761400429AE5C5D0846D71D5C44E5EE41D8D282EC4D76003BB9AED2C0
          37943231C56168BA12DB28924196AE92321781C015E062B06AA4D596C7B384B4
          20A7363988CD21404669C76B74A6E483CD5D4C2A54CECA18BE6969B11347C607
          7AA2FA74664DCE335AB8522A2B878E08F2E79AE5A185E5D5753D094D3DCA26DA
          2897E500572BAE5CC6AC541E6AF6B5AB34719119C572843DD317724935E9E0F0
          0B9FDACDE85B73B5914650D3C9C74AAD3C3838ADDFECF9638B7ECC0AA72468AA
          5DFAD7D251AF1D96C734E8DD36D9520CC7205CD757A56A2D1A8858020D733E49
          77C8E0D585927B670C41C0ADF15878D5565A974D726925A1DACBA1C77B0974EA
          4573C74EBBD36E58843B2BA1F0DEAEB3305735D4CF6D15DA1C4608AF9CAD52A6
          1A4E125745D56DC934F4391D0F5131DC057C8CFAD74B2B333A95E86B34E86A97
          3BC71835A9146E1947615C92AD09CB991551A7AA265528066992FCDD2AEEC564
          C1EA2AB90B9229E1EA6AEC7938B846AC79599CF318B39E94437B1CC0A938A9A7
          8036491C5664B6B80C53835954C5A8CF919F315713572EA9CB5358B2F3431B83
          C022B2351D16CE652CC003510BBBAB7246090285D463BAFDD4876B1AEFA719D3
          7ED16C73E2E54EADAB517B1CA6A5A53C4C7C9394AC198B40F82B835E9E74D530
          3107391C579F6B76CE976CA570335E8E1E6AA45A39F0D8D8621B83DD1505A9BB
          4DC3AD2359AC2BF3568E8E814856EF5B4DA2894166E41AE5C43709D93D0CEA62
          DE16A5A5B1C90B4598129D6956D1AD94BB5750BA279782BD2B33578C6DD8BDA8
          A0DCE7CAF61C71AF1353961B1908DE73F4C9ADAD3B4E96660AFF002A1ACED260
          66BA55DB9E6BD123D33FD1948F94E2BAABCD528A35C462E186B47AB22D37C3F6
          9100FC31ADC8ED638D400A00AC8FB6C762BB036E6A46D46E27C0504035E7D455
          2ABE77B236C23845BAD599B725D45026322916E3CCC6DE86B3E3B62EA0C84935
          A7040140C0E2BCF8E2D39F22DCE88E32A63AA7B3A3F0A2C45C7269EC376714E0
          ABC54EB1AAA67B9ADB11536B9F4D83A71A31E546746CC2521BA0AC1D7F51CC9B
          14F4AE86547DE47AD63C9A279D73E637393531AB08CB9A47AF4DA4EECE464B7B
          AD46655543B6B7AD7C3EB6D08793A8E6BAAB5B186D5066303DF1587E23D552D8
          1543C57552AD3AF25082B2269B7CCEECC4D4EFC08CDBA28C0AE42E9999CAE6AF
          4D732DCCACC80E0D5468995F7375AFA2C26163496BA1152D3D20B436E4B7889D
          D1914AD03B4782991596B33AB7078AE8747D42272239C0C545452A70BEF63BDC
          A36B58CA80C96736E424735E85E19D58491ED988AC8BAD16DEF137DBB0CFA0AC
          C816E74CBA0181D99AE19FB3C5C6CF7309469CE2E363D1278D679329C0A63426
          314DD3B50B7B8B64C11BF14D9EEF64E14F435E2CE9724F92DA1E7F24DDE0B61F
          6A49B8DAFD0D4DA85BAC3F32F7A96245914328E6A7B9B769AD483D40AE08D695
          3A975B1E3E269D487BD7B98B1307C8A8A684AB7038350EE7B698861DEAE4730B
          A5DAA39AD71338545CCB73CFAD3A78CA6E8CF7F330F5186480870B9535CD6A31
          ED91658F839AF429AD5E484ABAE4573573690C8CD11E1AB3C3664E153DE3E661
          4EA61EA3A4B62B59DF4A2241D4541E20B1496D3ED0A9F362B574EB00995619AD
          1BAB01359B46476AFA1C16269CD5E27875AB2A1884D773C812EDEDE7181D0D76
          DA65F0BAB407B8158B3E8C82F648DC77E2B5B438E3B694C0FD2B5C428D492B74
          3D6C756A75E926B7458BDBC5B6B5663E95C3DC5F9B8B83C704D769AFC714B882
          3EF5809A2A8B84451924D1878C69CDDFA865D569D18393DD9B5E1BD3D3C9FB43
          2F4E6B4EEB50708CABC015ADA7E9C20B054C76AA7A8D8029B54726B3C66229C5
          5E47951AEB11896DF739BB2469AE5A494E4035D1E9F1B5C3F0985155EDAC6287
          084FCC6BA5B5B530C0046BD6BE7F1399F34ED0D8F6EAC6A5692A5D08A2877363
          1C0AB0F88D714E320B55F9C726A8C92B5C480255619C2379CB73E970CE96029A
          A71DCD6B0805C364F6A6DE931CDB13A0ABD636EF0DB038E48A8E4880CBBF5ACE
          75E552A791E861A3567EF6C8A6B13483DE9F0C222972FD2A28AE819F62F4A9AF
          2EADE0B766761BB15DB1A7CD2E4B687B1CB34943A19FE22D4D2DE0C4446715E6
          F7B2CB7B365C92335AF7F34FA8DD3244095CD5CB3D0122024B86C7B1AF6E92A7
          848799E84214E115139F8A0289854A45B4463BA422B5756BBB7B7063800FAD73
          525CC8C4F3C576D2E6A90E6DAE6EA514B622405AA781CC520CD3D2205830A99E
          152335D9CB7DCD6145DAE8EABC3F729B802F9CF6AEB3FB360BD00320E6BCAEDA
          E9ED250EA4E01AF45F0CF8812E8A46E39AF1F17859C65ED21D0E4C52925CC913
          BE866CEE018C902ACCB6259013D45747776E26803AD5444063C11D2BCCC45594
          E1CDD51E64717CD1E78EE8A9628C30A6AF49E644D8232A6ACC3602440E9C1AD1
          1681E2018648AF32752125AAB9E76231D06F5390BDB35DE588E0D518545BCFB9
          4715D7DFE9A248F8ED59E749568491D4571F2C65513B9F2F9C61EA4A1EDA83D8
          A8270EBC8AE6B53B306EBCD438E6B5E5DD04A50D472C4B28CD7454C0FEF39975
          3E4A39A564B966EE54B68CA806ADBB12B8C5451A90702A4C1AF5F0782E4574CF
          32BD7E795D9CBEB160EB3F9EA2AADB41E6CA188C1AEB9EDFCF42AC38A8A2D2A3
          5CF15DEA13E6B33A238D4A9D9EE72D77008A5DE79352E9764F35DF9CCBC0AE8A
          7D251C54B0DA8B68C0502938CF9B950FEBB174ECB7268DB0BB7155AE10B82715
          6466A39013C571633052A8AED9CD42BF24EE8CDB0B2DF7BE639E01AEA44A91A8
          000E2B2E184463754AB21924082BC58602F56EFA1EACB35ADCB686E457405CCD
          CF4AB56764A64040E0568C7A48306F3D4D5FD3F4DD8B93594A0A355FBDB1F519
          351AAE3EDEBB2BA33BB796A381552F958640EB5D2AD9AC719DA39359D2D811B9
          DCE6BAE12A718DD687D5E1F194D3D3647350D932E5FB9AAAFA3C97B7043B12B5
          D1B22A467156AC2D82A191ABD3C3D5718B9F73D2962D463ED25F239A5D1EDEC7
          A20CD73BAFCEAA0857C63B5745E25D6A3B20CA07CD5E657D7F25ECCCC49C1AF4
          70B86A9525ED247A3857292E668A57529924C0E6AB3823AD5F8E01F7AA278B2F
          93D2BDAE56B44764A8C9C6EC125F980152BCC318AA084AD58B78CCB20CF34733
          5B842B4AD645CB4B392F250AA3826BD1FC37A0C569B1DBEF5606816E0105931E
          F5D3B6AB6F62065C6476AF1B1589A9397B381C98AE697BA99D35E5C88A108A6A
          A248163C935CDFF6D3DEDC0F2D495AB735EB22853D4D79B88A528C397AB3CD8E
          139572477674915F955544E6B4BED82388163C9AE62C19B863579D5E46DCC781
          5E64E14E2ACF43CEC460A9A7AEC59BFD48A47C77ACF3ABE212A3A9ACFBDBD5F3
          0A93C0AA5030B99F0BD2B8F9946A2B4743E5738AF5393D8D044F2069E42ED514
          D308862B4FC9545E48E95CC6A7799BBF2A319C1ADEA63BF79CABA1F2B1CAAB5A
          F5372FC6C4FCD5364D53B672C003571D7099CD7AF83C6BA8AC91E5E228724ECC
          865B8FB3A16635043AB2BE40AC1D62F9DEE7C853C556B69BCA9820E4D77A94F9
          AECE88E0A2E9DDEE74B71AB88C73535BDD8BA8C156AE52EE7F326D8C314FD32F
          1E0BC1096F94D0DCF9AE87F528FB3BADCEC074EB51C84819F4A9225CA6ECF5AA
          B72E532335C38CC64E92B347350A3ED276459827127CB5288CC6E1D6B0EC6F1A
          2BD08E3826BAB548E450411822BC5863DAAB67D4F5A594556BDCDC91356221F2
          CF5157F4FD4998106B98BB22D66CF51562CEF97CC001E0D66E7CD55DE3A33E9B
          26AB5611F615D1D92DEACA8403F30ACE96FD86E492A0546561223706AA5F1660
          4D75D354E51B23EB30F84A4DE9B3276915E338AB361740A98D8D7390DE33129D
          C55497559EC6E0B329DB5E9E1A93945C2FB1E93C2271F672F917BC47A4437C18
          FF00157996A1A74965330C1DB5E8B16B96F7BC1601AB0F5E83CC5CA26457A186
          C454A53F6733BF0A9C3DD6CE2D2703E5A8DE6C3E0F4A2F2231C9D306AAB927AD
          7B7CD7D51D92AD24ACC996DE473C0E2BA1D22CA087124E47D2B3A4BB8C9DB101
          43CC563CB3FE15C751CAA42DB5C8718DAF73A1BCD7A2B75F2EDD47D4550B44B8
          D4AE833925735916A925ECDB5012335E8BE1AD29608F33015C553D9E121E673C
          A54E117234EC2D2DE0B640AA3762993DA869B71E952DC4AB0C9FBBE94C3334A2
          BC4955E79F3DF43839A6AF35B16A2915142A9E6A7BB9DE1B5393C9159D6AA567
          DCFD054B7F70B39DA3B570C684AA54D363C7C4CAACFDDD918EB1BDC4A4B1EF57
          5615B54CA9E69230101355E59B7B707815B6263082508EE79F5E34B034DD596F
          E64F3DD34501691BAD735757D1465A4032C6ACEA12B4E76EFC28AE6F507324CB
          145CF35186CB5CEA5E7B1F334E7531137551D069BA80605DCE3D2B42EF5010D9
          3C99ED58B67A7C86242DC0AA7E21BF48EDBECEADCE2BDFC1E1E9455A2789568A
          C462125DCC29B5A5FB648EC7273C56CE833473B99E4AE3A3B237138C1E09AEDF
          4EB15B5B4007A56B8971A7256EA7AD8FA54E8D2515BB19E209A34513463915CE
          AEB59B84607041AEA6EED12E2D594FA570F7361F67B86F406961A51A9377E82C
          BA9D3AB0707BA3D4F4DD445C69EAF9E82AA6A37E366E43C8AC1F0DEA5188FECE
          EDD78AD9B9D39991990E411538CA34A4AD23CA54161F12D3EFA11DAEA09300CC
          3E615D2DA5D79F0028DD2B81B42F6D74D1CA0804D745612790F957F94F6AF031
          39672CEF0D8F6AABA9464AB6E747B16E14EF3922A94B098640529639B6B039E0
          D5A62245CD186E495E123E9B0AE963E92A8BE2356C2779ADB6E79029924A3E64
          73CD56B2B816EDC8EB45E29966DEBC66B3A987953A9E47A186F6B0F756A86476
          ABE7EF5A9AEA0825B7647404E2A1123443DE9D04C1E5FDE74AEC8D451973DCF6
          2F51A53E8703A95ACD6172D24790B9A9EC7C41B808E75047BD757E21D392EA0C
          C58CE2BCDAFEDE5B09B0C0E335EDD19D3C5C2CD6A7A109D39C1499B1AAC16B76
          A6488806B9A92D245278C8ABB14C1D32AFCFA50B7A233B5C645775252A70E55A
          D8DD460E3B98FE6957C2F5AB51C135C3A862706A8DB869240D8AEB74AD3C3A09
          9D8002B5C5E2634969A1C34FDFD64F4367C39A4A4055D8715D1DCDE416A87120
          1F8D7293EBCB691148FB715CFADDDD6A772C4B9D95F3B568D4C449CE4EC8BAA9
          F32B23AEFEDD125D6C5E726B62277CA9F5AE6B42D3774FB9C138F5AE925460E3
          6F005724A94212E58955125A2343CC0A9CF53506E5C934D52580C9A64BC74A78
          7A7ABB1E4E2E71A31E66413CE173CF159925CEEC88F24D5F680C99DDD2962B48
          A152D815954C2294F9DEE7CC4F09531F539EA7C28C45B0B89C924900D2FF0067
          C7663CC65DCC2B61EE63407240158BA8F882D2152A70C6BBE9BA951F22D91862
          D420D51A287B6A7881811B78AF3ED6A767BA66DF9E6AFEA3A9CB392D1FCA86B1
          241E73F2D935E8E1E0A9459CF87C1C30F79F5669E8EE0FCCDDAB59F5A30E4374
          15CB9B936A9B57AD21BD12AE1EB96BA739732D8CE784789A9CD3D8E9575B6948
          0BD2B3F5793726F5EA6B27ED823184A12E9AE0146A282709F33D86B05F57A9CD
          0D8B1A4CC56E95B7639AF428B5426DD429DC715E6AAA217EB835AFA7DFCD010E
          4EE415D75E31AB146B5F090C4DA5D51DC2DA25F2EE2986A46D2E58B05189C555
          D37C4B6F2E1180535BD15EC52282181CD79D57DA527C9D19B60D45C9D1AAB429
          24ED100B2035A56F70180C74A73DBC570809029AB6FB31B7B570470B153E75B9
          D0B053C154F6B47E165E0EBC5585943263BD528B9E0D48C76F435AE229F73E97
          055635A3CC432B317620F4AC4975B682EB638C006B6A346F3493C83585AFE9A0
          BEF51D69469C252E591EBD3B37666ED96A56F72A3320FA66B2BC43A647740B28
          18C571D70D73A7CCAE8E76FA56DDA7883CE88249F4AE9A742741AA907744D352
          E7773979ED24B799821231549A462FB5BAD75FA9D8C6D099D1C73DAB8FBA4657
          2D8AFA1C2629555AEA455B47586C5889D238C228E6AEAEA12C316CDD806B0619
          886CD588D9AE2400F4A8AD422FDE7B110AE9C6C917FCC7B93B14124D74FA2690
          E881A45C66A96876B1890311935D72DC450A0DC40AF9BC6E3D29FB2A68E98A9D
          AECBB6D1C76F1E147353654F5AC91A944640A87357D64CA8278CD7995F15CBBF
          522304D92E3078A50431C1A40E08C52A819AE9A38A4A99C15F09CD2D449140E7
          B573DACEB69688C8872D5B9785844707B5715776BE75D9CFCDCD6786C62A951A
          6F638F136A70E482330DEDF5D9639214D645F82AD96259ABB98F4EFF0047C05C
          7158975A62F9A47535EFE12BC22DB478B4B0F397BF25639479669136018154C1
          92290FAD74F7563B38518ACB78628C967209AEB92525742A90D6C660DD2C9F3D
          4533AC4FD68B9B8F9CECE2A833176CB1AC928DECF62FD9248BD14A9249CD4CE0
          A480A56564AB6455DB7B82197772286A37B2D83D9268B04492B806AE234F12ED
          1C8A9A310CC148201AD5B4B3DC403835AC5462AEC8843DEB19F64A5DF9C86AD7
          F32F6D4064625455A834D4F3402306B77FB387D9F0003C57262F11095AE555C3
          CD7BD0571344D77CE0239786AEA23C3608E86B868AD0437638DBCD765604F943
          2DC62BE7F178C54E6ADB33D8C27BD0509A2D31038029B824E4D39B19A6970A31
          5A54C52F6676D1C25A5A6C3B200E951CE1268F6B8A8DA4E090738ACF93534494
          ABF18AE5A18A72765D0EF9412D1997ACE92668C98C57232092CD8A382315E8A2
          F62997E420D733AE411B313B79AF4F098FB54F6535A16D4AD74CC317D2491ECD
          E71E95564954A947EB504E1A093E5AAB3CC49C9AFA6A3461F12D8E69D7E54D34
          2043BB815B5A658EE219B81510489781C9AB913B04E0E057998AAF2F67A051A7
          63652E21B24F97EF5546BAB8BE9C28242E6A8FCD2363AD745A559FC80B2F35E4
          72D3A6BDA3D646F2BB76B9A3A7E9A91A2B1396AD078A4670070A2922C46A055B
          0DF266BE531B5E73A86B0838AB82263151DCCDE52E475A7AB3353C40AE72D5B5
          2A95214EF239AA49B7646334B737208C10B4E82C122F9E4E5AB61D1117800563
          6A97AB02120D2C2BA952A5A0AC73CE1082E6915350BE316517802B163D421576
          790E4D54B9BB9AF2421735957113424EE249AFAB8B8D38A8F53CD49D4937D093
          53D48C9236CE0573971392492D562E5A462700E2B39D0E79AF429C9A89CB3827
          3B22124B31A7470AE72C6A4488F5A8E6C8E3358AAB26EC8D2545461762C91213
          F29A8F94229D0F27AD4CF09EB43A928BB30851528E84B6F3631CE2BA0D3B5068
          9D493915CCA4673C5685B09531C122B69CAF1D0CE30519D99DB49A9C328465E1
          AB5F4EBF662149C835C4DB4267206706B4E1967B2703922B81CA15138753AA49
          C1A6B63B79ACE3B85DCA30D5105BAB61F2E481516917E2741B8F35D04611C723
          22BE5318AAD2A9696A8F4A9C61523CCB72ADA4C6451BF8353BA669CD0A839518
          A8DCB2D152A549D3BC4E8A6DA76656104892939E0D54BEB08E65627835ABBBE4
          CF7AA92B090106B9F095AA42A1D32839238B779EC6E0ED24AE6AC9BC8AE93127
          5AD0D4ECF7464A8E6B9A75689C83C57D6250AB1537A3328DE2ED722D4AC97964
          3915812C4D9208AE85D98A1E722A8C810F0457AF83AD251B3D4C6B534C821624
          E4D6945B9F005528A3C1AD4B44C106B9ABD5E72A845DB534B4FB31B81615D1C2
          5234C7158715C2C605486ED9F806BC7ACE5276E87528A46D0B84DFD6ADA4C5F0
          074AC1B5567604D6D408C31C579D5E9D383BF5366B42F4631537005448BC7349
          23ED15E556AEEA3E5473CD25A905DCC150F35CB5E86B8620F35D0CC865AADF62
          55E48AF470D5E3423A6E79D529BA8F539D8EC5D72556ABCF600E5A4AE9A531C4
          87A5615DDC212726BAA85794E5CD2339D351564737790AE0855AC49A1DAC735D
          05E4CA33B6B02E9F249AF6235E5256328D151D595DE50AB81549C9635330DC69
          E908279ADE12505730A9094D95572A6AE4536570691E11DAA30BB4D39494D053
          A72A6CBD1441DB8ADBB38800030CD61DB360835BD6738E375734ABCA1A1D12A4
          A5A9B5058A1019383569EC1CA82C33515A5CA02306B7E0912441D2BC8C456946
          5CD13484135CACCAB58CC0C31915D4594F98C66A97D911F902AC45118AB96BE2
          635E1696E694E9BA6F4357208A8A4E6991BE4548CB91C579B4AB3A72B1E8C127
          A94E494A6476AA6D749BEAD4F1B1CD62DDC6CAC48AF570F1A737767425A17A47
          59131583A85A86248152FDA5E3EA69925C871CD7A34B9A2CC5C6E60CCAD1E456
          65C13935B976B9248ACA962C9AF62855E4D4E5AF1D34264C55B8E4C0E2AA0522
          ACC08588AE4F689475348492D8B71EE735A9696C5B1914DB2B5C8048AD986354
          02BCBAF5DEC8E88BB935A5B05C715AB14400AA30B8157E26C8AF22BA9CB56549
          8F2BC544F16EAB039A08AF32578B3191499028ACFBCB811A9AD1B938535CF5E8
          67635BD09733D4E79348CDBDBD63900D73F753B1279ADB9ADC9CE6B2AE6DBAD7
          B742A4568676B98B3C848ACE972C6B5E78319AA1247835E9D29A2651B948253C
          714F618A61AEA4EE4A8241D69A5334EA7AF343760E44C48C106AFC0E45411A64
          D5E821CE2B96ACD151858B96D2B0239ADFB2BC65C026B26DADBA56AC36C40181
          5E656AB17A14D58E8ECAE43819AD4450E2B9DB3564615D15A9CA8AF12BC945DD
          1A46489522DA6A50B4F028E95846F266F12192304565DDDB0606B5246C0AA334
          82BD3A319AD51BC59CEDDDA919C0ACA95590D7552AAB8AC8BDB5E0902BD6A15D
          AD193276306493B1AAAF8356AE232AC6A9B66BD455138E873CE49EE5F6B7E7A5
          5DB3B5C91C54C611BBA56959C038E2BCDAB5ED1338C7956A4F043B17A54849CE
          2AC6CC2D3562CB579F1AA9BBB2A152EEC3A0524D6944B8150C116055A5E2B49B
          4E373A1B255A56E948A726AC245BABC1C44B5309CF4336742C3A5664F6DD4915
          D3496E02F4AC4D4488D4D65467AD91CE9DCE7AEB6A03587752AE4D4FAA5E9563
          835CD5C5E924F35F4185A1292B96896E655E6B2E69064D365B827BD5579335EB
          D3A7CA26C733F3499A84B734A0D6FCF6213B9266955F9A889A4DDCD1CF706CD0
          8A41C569DB4CBC56023E2AC477041EB585487314A475F6B32E456EDA32BE2B83
          B7BC208E6BA3D32F8960335E562B0ED2B9476305B83820569431951D2A9E9920
          91456F45002BD2BE7AB49A76643640A38A1AACBC5B4556738ABC3CB53A29CF42
          095722B36E10826B54F355668F22BDFA7A46E6E999409CD3668B7AD5978B0D4E
          099159CAAD99CF3A96672F7D6BC9E2B25EDCE7A575B7B0020F158EF00DDD2BBE
          8623425FBDB1FFD9}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'NoiseTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'ShaderMaterial'
        Tag = 0
        Material.FrontProperties.Emission.Color = {77BE9F3D7368913D2506813D0000803F}
        Material.FrontProperties.Specular.Color = {B6F35D3F6DE75B3F6DE75B3F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Material.Texture.KeepImageAfterTransfer = True
      end
      item
        Name = 'ErosionNoiseTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'ErosionMainTexture'
        Tag = 0
        Material.BlendingMode = bmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'ErosionTexture'
        Tag = 0
        Material.BlendingMode = bmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'MatCapTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'ExplosionTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'EnvMap'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.Disabled = False
      end
      item
        Name = 'RefractMap'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'BackgroundTex'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'LibMaterial'
        Tag = 0
      end>
    Left = 440
    Top = 24
  end
  object GLScene1: TGLScene
    Left = 440
    Top = 88
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      NearPlaneBias = 0.001000000047497451
      TargetObject = World
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {0000A0400000A0410000C8420000803F}
    end
    object LightCube: TGLDummyCube
      Position.Coordinates = {000096C30000A040000096430000803F}
      OnProgress = LightCubeProgress
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
    object LightCube2: TGLDummyCube
      Position.Coordinates = {000096430000C842000096430000803F}
      OnProgress = LightCube2Progress
      CubeSize = 1.000000000000000000
      object GLLightSource2: TGLLightSource
        Ambient.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {EAEA6A3FEAEA6A3FA7AD2D3F0000803F}
        LightStyle = lsParallel
        Specular.Color = {0000803F0000003F0000003F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object World: TGLDummyCube
      CubeSize = 1.000000000000000000
      object ScreenBackGround: TGLHUDSprite
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'BackgroundTex'
        Visible = False
        Width = 256.000000000000000000
        Height = 256.000000000000000000
        Rotation = 0.000000000000000000
      end
      object Objects: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLSphere1: TGLSphere
          Material.MaterialLibrary = MaterialLibrary
          Material.LibMaterialName = 'ShaderMaterial'
          Visible = False
          Radius = 45.000000000000000000
          Slices = 64
          Stacks = 64
        end
        object FreeForm: TGLFreeForm
          AutoCentering = [macCenterX, macCenterY, macCenterZ, macUseBarycenter]
          AutoScaling.Coordinates = {0000484200004842000048420000803F}
        end
        object GLTorus1: TGLTorus
          Visible = False
          MajorRadius = 40.000000000000000000
          MinorRadius = 15.000000000000000000
          Rings = 64
          Sides = 64
          StopAngle = 360.000000000000000000
          Parts = [toSides, toStartDisk, toStopDisk]
        end
      end
    end
  end
  object Cadencer: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    OnProgress = CadencerProgress
    Left = 384
    Top = 88
  end
  object ColorDialog: TColorDialog
    Left = 381
    Top = 153
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    ZoomSpeed = 1.100000023841858000
    RotateTargetSpeed = 0.500000000000000000
    FormCaption = 'Shaders Lab - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 528
    Top = 24
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 472
    Top = 160
  end
end
