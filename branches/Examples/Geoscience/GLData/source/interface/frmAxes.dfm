object formAxes: TformAxes
  Left = 147
  Top = 273
  Width = 395
  Height = 480
  BorderStyle = bsSizeToolWin
  Caption = 'Coordinate Axes'
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ActionToolBar_Axes: TActionToolBar
    Left = 0
    Top = 0
    Width = 387
    Height = 26
    ActionManager = ActionManager1
    Caption = 'ActionToolBar_Axes'
    ColorMap.HighlightColor = 14410210
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 14410210
    Spacing = 0
  end
  object pnlAxesSettings: TPanel
    Left = 0
    Top = 26
    Width = 387
    Height = 291
    Align = alTop
    TabOrder = 1
    object lblAxesXOrigin: TLabel
      Left = 73
      Top = 41
      Width = 37
      Height = 13
      Caption = 'X Origin'
    end
    object lblAxesYOrigin: TLabel
      Left = 72
      Top = 120
      Width = 37
      Height = 13
      Caption = 'Y Origin'
    end
    object lblAxesZOrigin: TLabel
      Left = 73
      Top = 202
      Width = 37
      Height = 13
      Caption = 'Z Origin'
    end
    object lblAxesXLength: TLabel
      Left = 204
      Top = 41
      Width = 42
      Height = 13
      Caption = 'X Length'
    end
    object lblAxesYLength: TLabel
      Left = 204
      Top = 120
      Width = 42
      Height = 13
      Caption = 'Y Length'
    end
    object lblAxesZLength: TLabel
      Left = 204
      Top = 202
      Width = 42
      Height = 13
      Caption = 'Z Length'
    end
    object lblAxesXRadius: TLabel
      Left = 205
      Top = 63
      Width = 41
      Height = 13
      Caption = 'X Radius'
    end
    object lblAxesYRadius: TLabel
      Left = 205
      Top = 139
      Width = 41
      Height = 13
      Caption = 'Y Radius'
    end
    object lblAxesZRadius: TLabel
      Left = 205
      Top = 225
      Width = 41
      Height = 13
      Caption = 'Z Radius'
    end
    object lblAxesXColour: TLabel
      Left = 215
      Top = 15
      Width = 31
      Height = 13
      Caption = 'Colour'
    end
    object lblAxesYColour: TLabel
      Left = 215
      Top = 96
      Width = 31
      Height = 13
      Caption = 'Colour'
    end
    object Label1: TLabel
      Left = 215
      Top = 178
      Width = 31
      Height = 13
      Caption = 'Colour'
    end
    object cbxAxesXN: TCheckBox
      Left = 8
      Top = 14
      Width = 78
      Height = 17
      Caption = 'Left X Axis'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbxAxesXNClick
    end
    object cbxAxesYN: TCheckBox
      Left = 7
      Top = 94
      Width = 83
      Height = 17
      Caption = 'Left Y Axis'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = cbxAxesYNClick
    end
    object cbxAxesZN: TCheckBox
      Left = 9
      Top = 176
      Width = 97
      Height = 17
      Caption = 'Bottom Z Axis'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbxAxesZNClick
    end
    object cbxAxesZP: TCheckBox
      Left = 109
      Top = 176
      Width = 70
      Height = 17
      Caption = 'Top Z Axis'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = cbxAxesZPClick
    end
    object cbxAxesYP: TCheckBox
      Left = 99
      Top = 94
      Width = 80
      Height = 17
      Caption = 'Right Y Axis'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = cbxAxesYPClick
    end
    object cbxAxesXP: TCheckBox
      Left = 99
      Top = 14
      Width = 80
      Height = 17
      Caption = 'Right X Axis'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = cbxAxesXPClick
    end
    object geAxesXOrigin: TGEFloatEdit
      Left = 119
      Top = 37
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 6
      OnExit = geAxesXOriginExit
    end
    object geAxesYOrigin: TGEFloatEdit
      Left = 119
      Top = 116
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 7
      OnExit = geAxesYOriginExit
    end
    object geAxesZOrigin: TGEFloatEdit
      Left = 119
      Top = 198
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 8
      OnExit = geAxesZOriginExit
    end
    object geAxesXLength: TGEFloatEdit
      Left = 258
      Top = 37
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = True
      UseMaxBound = False
      Validate = vdSilent
      TabOrder = 9
      OnExit = geAxesXLengthExit
    end
    object geAxesYLength: TGEFloatEdit
      Left = 258
      Top = 116
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = True
      UseMaxBound = False
      Validate = vdSilent
      TabOrder = 10
      OnExit = geAxesYLengthExit
    end
    object geAxesZLength: TGEFloatEdit
      Left = 258
      Top = 198
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = True
      UseMaxBound = False
      Validate = vdSilent
      TabOrder = 11
      OnExit = geAxesZLengthExit
    end
    object geAxesXRadius: TGEFloatEdit
      Left = 258
      Top = 59
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = True
      UseMaxBound = False
      Validate = vdSilent
      TabOrder = 12
      OnExit = geAxesXRadiusExit
    end
    object geAxesYRadius: TGEFloatEdit
      Left = 258
      Top = 137
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = True
      UseMaxBound = False
      Validate = vdSilent
      TabOrder = 13
      OnExit = geAxesYRadiusExit
    end
    object geAxesZRadius: TGEFloatEdit
      Left = 258
      Top = 221
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = True
      UseMaxBound = False
      Validate = vdSilent
      TabOrder = 14
      OnExit = geAxesZRadiusExit
    end
    object pnlXAxesColour: TPanel
      Left = 258
      Top = 10
      Width = 21
      Height = 21
      Color = clRed
      TabOrder = 15
      OnClick = pnlXAxesColourClick
    end
    object pnlYAxesColour: TPanel
      Left = 258
      Top = 90
      Width = 21
      Height = 21
      Color = clGreen
      TabOrder = 16
      OnClick = pnlYAxesColourClick
    end
    object pnlZAxesColour: TPanel
      Left = 258
      Top = 172
      Width = 21
      Height = 21
      Color = clBlue
      TabOrder = 17
      OnClick = pnlZAxesColourClick
    end
  end
  object pnlAxesLabels: TPanel
    Left = 0
    Top = 317
    Width = 387
    Height = 136
    Align = alClient
    TabOrder = 2
    object lblAxesLabels: TLabel
      Left = 8
      Top = 6
      Width = 36
      Height = 13
      Caption = 'Labels'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblAxesXStart: TLabel
      Left = 11
      Top = 37
      Width = 33
      Height = 13
      Caption = 'X Start'
    end
    object lblAxesXStep: TLabel
      Left = 115
      Top = 36
      Width = 31
      Height = 13
      Caption = 'X Step'
    end
    object lblAxesXStop: TLabel
      Left = 222
      Top = 36
      Width = 31
      Height = 13
      Caption = 'X Stop'
    end
    object lblAxesYStart: TLabel
      Left = 11
      Top = 59
      Width = 33
      Height = 13
      Caption = 'Y Start'
    end
    object lblAxesYStep: TLabel
      Left = 115
      Top = 59
      Width = 31
      Height = 13
      Caption = 'Y Step'
    end
    object lblAxesYStop: TLabel
      Left = 222
      Top = 59
      Width = 31
      Height = 13
      Caption = 'Y Stop'
    end
    object lblAxesZStart: TLabel
      Left = 11
      Top = 80
      Width = 33
      Height = 13
      Caption = 'Z Start'
    end
    object lblAxesZStep: TLabel
      Left = 115
      Top = 80
      Width = 31
      Height = 13
      Caption = 'Z Step'
    end
    object lblAxesZStop: TLabel
      Left = 222
      Top = 80
      Width = 31
      Height = 13
      Caption = 'Z Stop'
    end
    object Label2: TLabel
      Left = 338
      Top = 17
      Width = 31
      Height = 13
      Caption = 'Colour'
    end
    object geAxesXStart: TGEFloatEdit
      Left = 47
      Top = 32
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 0
    end
    object geAxesXStep: TGEFloatEdit
      Left = 153
      Top = 32
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 1
    end
    object geAxesXStop: TGEFloatEdit
      Left = 258
      Top = 32
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 2
    end
    object bAxesUpdate: TButton
      Left = 226
      Top = 102
      Width = 93
      Height = 25
      Caption = 'Update Labels'
      TabOrder = 3
      OnClick = bAxesUpdateClick
    end
    object geAxesYStart: TGEFloatEdit
      Left = 47
      Top = 55
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 4
    end
    object geAxesYStep: TGEFloatEdit
      Left = 153
      Top = 55
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 5
    end
    object geAxesYStop: TGEFloatEdit
      Left = 258
      Top = 55
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 6
    end
    object geAxesZStart: TGEFloatEdit
      Left = 47
      Top = 76
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 7
    end
    object geAxesZStep: TGEFloatEdit
      Left = 153
      Top = 76
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 8
    end
    object geAxesZStop: TGEFloatEdit
      Left = 258
      Top = 76
      Width = 60
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 9
    end
    object pnlXLabelColour: TPanel
      Left = 338
      Top = 32
      Width = 21
      Height = 21
      Color = clRed
      TabOrder = 10
      OnClick = pnlXLabelColourClick
    end
    object pnlYLabelColour: TPanel
      Left = 338
      Top = 54
      Width = 21
      Height = 21
      Color = clGreen
      TabOrder = 11
      OnClick = pnlYLabelColourClick
    end
    object pnlZLabelColour: TPanel
      Left = 338
      Top = 76
      Width = 21
      Height = 21
      Color = clBlue
      TabOrder = 12
      OnClick = pnlZLabelColourClick
    end
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = acAxesVisible
            Caption = '&Visible'
            ImageIndex = 0
          end
          item
            Caption = '-'
          end
          item
            Action = acAxesX
            Caption = '&X'
            ImageIndex = 2
          end
          item
            Action = acAxesY
            Caption = '&Y'
            ImageIndex = 2
          end
          item
            Action = acAxesZ
            Caption = '&Z'
            ImageIndex = 2
          end
          item
            Caption = '-'
          end
          item
            Action = acAxesXYGrid
            ImageIndex = 1
          end
          item
            Action = acAxesYZGrid
            ImageIndex = 1
          end
          item
            Action = acAxesXZGrid
            ImageIndex = 1
          end>
        ActionBar = ActionToolBar_Axes
      end>
    Images = ImageList
    Left = 326
    Top = 59
    StyleName = 'XP Style'
    object acAxesVisible: TAction
      AutoCheck = True
      Caption = 'Visible'
      ImageIndex = 0
      OnExecute = acAxesVisibleExecute
    end
    object acAxesX: TAction
      AutoCheck = True
      Caption = 'X'
      ImageIndex = 2
      OnExecute = acAxesXExecute
    end
    object acAxesY: TAction
      AutoCheck = True
      Caption = 'Y'
      ImageIndex = 2
      OnExecute = acAxesYExecute
    end
    object acAxesZ: TAction
      AutoCheck = True
      Caption = 'Z'
      ImageIndex = 2
      OnExecute = acAxesZExecute
    end
    object acAxesXYGrid: TAction
      AutoCheck = True
      Caption = 'XY'
      ImageIndex = 1
      OnExecute = acAxesXYGridExecute
    end
    object acAxesYZGrid: TAction
      AutoCheck = True
      Caption = 'YZ'
      ImageIndex = 1
      OnExecute = acAxesYZGridExecute
    end
    object acAxesXZGrid: TAction
      AutoCheck = True
      Caption = 'XZ'
      ImageIndex = 1
      OnExecute = acAxesXZGridExecute
    end
  end
  object ImageList: TImageList
    Left = 352
    Top = 59
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000000000008000
      000000000000000000000000000000000000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000000000008000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000000000
      000000000000000000000000000000000000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000000000
      000000000000000000000000000000000000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000000000008000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFA8000000FFFFFFFFAEFB0000
      FFFF8823CCFB0000FFFFBEFBFEFB0000FF03BEFBFCFB0000FF87FFFF8EFB0000
      FF0FBEFBD8000000B8DFBEFB9EFB0000C7FF8823DCFB000093FFBEFBFEFB0000
      FCFFBEFBFCFB0000FF2FFFFF8EFB0000FF87BEFBD8000000FF03BEFBAEFB0000
      FFFF8823DCFB0000FFFFFFFFFEFB000000000000000000000000000000000000
      000000000000}
  end
  object ColourDialog: TColorDialog
    Left = 352
    Top = 33
  end
end
