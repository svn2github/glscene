object frmFitnessMonitor: TfrmFitnessMonitor
  Left = 677
  Top = 385
  Caption = 'DelphiNEAT : Fitness Monitor'
  ClientHeight = 550
  ClientWidth = 587
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 377
    Width = 587
    Height = 7
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 387
    ExplicitWidth = 595
  end
  object Chart_GenerationFitness: TChart
    Left = 0
    Top = 384
    Width = 587
    Height = 166
    BackWall.Brush.Style = bsClear
    Gradient.Direction = gdFromTopLeft
    Gradient.EndColor = 15133664
    Legend.ColorWidth = 10
    Legend.Shadow.HorizSize = 0
    Legend.Shadow.VertSize = 0
    Legend.Symbol.Width = 10
    Legend.TextStyle = ltsPlain
    Legend.TopPos = 5
    MarginBottom = 2
    MarginLeft = 1
    MarginRight = 1
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    Title.AdjustFrame = False
    BottomAxis.Automatic = False
    BottomAxis.AutomaticMaximum = False
    BottomAxis.AutomaticMinimum = False
    BottomAxis.Maximum = 65.000000000000000000
    BottomAxis.MinorTickCount = 1
    LeftAxis.Automatic = False
    LeftAxis.AutomaticMaximum = False
    LeftAxis.AutomaticMinimum = False
    LeftAxis.Maximum = 100.000000000000000000
    RightAxis.Automatic = False
    RightAxis.AutomaticMinimum = False
    View3D = False
    Align = alBottom
    BevelOuter = bvNone
    BevelWidth = 0
    TabOrder = 0
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series1: TFastLineSeries
      SeriesColor = clNavy
      Title = 'Best fitness'
      LinePen.Color = clNavy
      LinePen.Width = 2
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series4: TFastLineSeries
      SeriesColor = clNavy
      Title = 'Av. Fitness'
      LinePen.Color = clNavy
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TFastLineSeries
      SeriesColor = clRed
      Title = 'Size of best'
      VertAxis = aRightAxis
      LinePen.Color = clRed
      LinePen.Width = 2
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series3: TFastLineSeries
      SeriesColor = clRed
      Title = 'Av. size'
      VertAxis = aRightAxis
      LinePen.Color = clRed
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 28
    Width = 587
    Height = 349
    ActivePage = TabSheet5
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Cumulative successrate'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        579
        321)
      object Label1: TLabel
        Left = 6
        Top = 276
        Width = 77
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Runs completed'
        ExplicitTop = 296
      end
      object Label_Runs: TLabel
        Left = 119
        Top = 276
        Width = 8
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitTop = 296
      end
      object Label_Hits: TLabel
        Left = 183
        Top = 276
        Width = 8
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitTop = 296
      end
      object Label3: TLabel
        Left = 134
        Top = 276
        Width = 23
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '| Hits'
        ExplicitTop = 296
      end
      object Label2: TLabel
        Left = 198
        Top = 276
        Width = 26
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '| Hit%'
        ExplicitTop = 296
      end
      object Label_HitPercentage: TLabel
        Left = 243
        Top = 276
        Width = 35
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = '0,00%'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitTop = 296
      end
      object Label_Generation: TLabel
        Left = 366
        Top = 276
        Width = 8
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitTop = 296
      end
      object Label5: TLabel
        Left = 282
        Top = 276
        Width = 60
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '| Generation '
        ExplicitTop = 296
      end
      object Label4: TLabel
        Left = 378
        Top = 276
        Width = 67
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '| Avg. win gen'
        ExplicitTop = 296
      end
      object Label_AvgWinGeneration: TLabel
        Left = 494
        Top = 276
        Width = 8
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitTop = 296
      end
      object Label6: TLabel
        Left = 378
        Top = 292
        Width = 55
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '| Avg. evals'
        ExplicitTop = 312
      end
      object Label_AverageEvalsPerWin: TLabel
        Left = 494
        Top = 292
        Width = 8
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitTop = 312
      end
      object Label7: TLabel
        Left = 6
        Top = 292
        Width = 38
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Species'
        ExplicitTop = 312
      end
      object Label_SpeciesCount: TLabel
        Left = 118
        Top = 292
        Width = 8
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitTop = 312
      end
      object Label8: TLabel
        Left = 134
        Top = 292
        Width = 30
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '| MRel'
        ExplicitTop = 312
      end
      object Label_MinRelatedness: TLabel
        Left = 183
        Top = 292
        Width = 8
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitTop = 312
      end
      object Label9: TLabel
        Left = 197
        Top = 292
        Width = 44
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '| PopSize'
        ExplicitTop = 312
      end
      object Label_PopulationSize: TLabel
        Left = 269
        Top = 292
        Width = 8
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitTop = 312
      end
      object Chart_CumSuccess: TChart
        Left = 0
        Top = 0
        Width = 587
        Height = 277
        BackWall.Brush.Style = bsClear
        Legend.Visible = False
        MarginBottom = 0
        MarginLeft = 0
        MarginRight = 1
        MarginTop = 3
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        BottomAxis.Automatic = False
        BottomAxis.AutomaticMaximum = False
        BottomAxis.AutomaticMinimum = False
        BottomAxis.Maximum = 25.000000000000000000
        BottomAxis.MinorTickCount = 1
        LeftAxis.Automatic = False
        LeftAxis.AutomaticMaximum = False
        LeftAxis.AutomaticMinimum = False
        LeftAxis.Maximum = 105.000000000000000000
        LeftAxis.Minimum = -5.000000000000000000
        View3D = False
        BevelOuter = bvNone
        TabOrder = 0
        Anchors = [akLeft, akTop, akRight, akBottom]
        ExplicitHeight = 297
        DefaultCanvas = 'TGDIPlusCanvas'
        ColorPaletteIndex = 13
        object FastLineSeries2: TFastLineSeries
          SeriesColor = 4227072
          Title = 'Cumulative successrate'
          LinePen.Color = 4227072
          LinePen.Width = 2
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Fitness spread'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Chart_FitnessLine: TChart
        Left = 0
        Top = 0
        Width = 587
        Height = 326
        BackWall.Brush.Style = bsClear
        Gradient.Direction = gdFromTopLeft
        Gradient.EndColor = 15990012
        Gradient.Visible = True
        Legend.Visible = False
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        LeftAxis.Automatic = False
        LeftAxis.AutomaticMaximum = False
        LeftAxis.AutomaticMinimum = False
        LeftAxis.Axis.Width = 1
        LeftAxis.Axis.Visible = False
        LeftAxis.Maximum = 100.000000000000000000
        RightAxis.Axis.Visible = False
        View3D = False
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        DefaultCanvas = 'TGDIPlusCanvas'
        ColorPaletteIndex = 13
        object Series5: TLineSeries
          SeriesColor = clRed
          Brush.BackColor = clDefault
          Dark3D = False
          LinePen.Width = 2
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Report'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        579
        321)
      object Memo_History: TMemo
        Left = 0
        Top = 0
        Width = 587
        Height = 277
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitHeight = 297
      end
      object Button4: TButton
        Left = 490
        Top = 285
        Width = 89
        Height = 21
        Action = Action_CreateHistogram
        Anchors = [akRight, akBottom]
        TabOrder = 1
        ExplicitTop = 305
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Histogram'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo_Histogram: TMemo
        Left = 0
        Top = 0
        Width = 587
        Height = 326
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'ChampView'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        579
        321)
      object Image_WinnerView: TImage
        Left = 0
        Top = 0
        Width = 401
        Height = 321
      end
      object CheckBox_ShowNodeNumbers: TCheckBox
        Left = 408
        Top = 8
        Width = 145
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Show Node Numbers'
        TabOrder = 0
      end
      object CheckBox_ShowConnectNumbers: TCheckBox
        Left = 408
        Top = 24
        Width = 153
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Show Connect Numbers'
        TabOrder = 1
      end
      object CheckBox_ShowValues: TCheckBox
        Left = 408
        Top = 40
        Width = 153
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Show Values'
        TabOrder = 2
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'ChampCode'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo_ChampCode: TMemo
        Left = 0
        Top = 0
        Width = 587
        Height = 326
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object CoolBar1: TCoolBar
    Left = 0
    Top = 0
    Width = 587
    Height = 28
    Bands = <
      item
        Break = False
        Control = ToolBar1
        ImageIndex = -1
        MinHeight = 24
        Width = 581
      end>
    object ToolBar1: TToolBar
      Left = 11
      Top = 0
      Width = 572
      Height = 24
      ButtonHeight = 19
      ButtonWidth = 52
      Caption = 'ToolBar1'
      List = True
      ShowCaptions = True
      TabOrder = 0
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = Action_StartRun
        AutoSize = True
      end
      object ToolButton3: TToolButton
        Left = 56
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object ToolButton2: TToolButton
        Left = 64
        Top = 0
        Action = Action_StopRun
        AutoSize = True
      end
    end
  end
  object ActionManager1: TActionManager
    Left = 352
    Top = 136
    StyleName = 'XP Style'
    object Action_Close: TAction
      Category = 'File'
      Caption = '&Close'
      OnExecute = Action_CloseExecute
    end
    object Action_CreateHistogram: TAction
      Caption = 'Create &histogram'
      OnExecute = Action_CreateHistogramExecute
    end
    object Action_StopRun: TAction
      Category = 'Evolution'
      Caption = 'Sto&p run'
      Enabled = False
      ImageIndex = 1
      OnExecute = Action_StopRunExecute
    end
    object Action_StartRun: TAction
      Category = 'Evolution'
      Caption = '&Start run'
      ImageIndex = 0
      OnExecute = Action_StartRunExecute
    end
    object Action_SaveReport: TFileSaveAs
      Category = 'File'
      Caption = 'Save Report &As...'
      Dialog.DefaultExt = 'txt'
      Dialog.FileName = '*.txt'
      Dialog.Filter = '*.txt|Text file'
      Dialog.Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
      Hint = 'Save As|Saves the active file with a new name'
      ImageIndex = 30
      BeforeExecute = Action_SaveReportBeforeExecute
      OnAccept = Action_SaveReportAccept
    end
    object Action_AboutCambrianGP: TAction
      Category = 'Help'
      Caption = 'About CambrianGP'
      OnExecute = Action_AboutCambrianGPExecute
    end
  end
  object MainMenu1: TMainMenu
    Left = 240
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      object SaveAs1: TMenuItem
        Action = Action_SaveReport
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Action = Action_Close
      end
    end
    object Evolution1: TMenuItem
      Caption = '&Evolution'
      object Startrun1: TMenuItem
        Action = Action_StartRun
      end
      object Stoprun1: TMenuItem
        Action = Action_StopRun
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object AboutCambrianGP1: TMenuItem
        Action = Action_AboutCambrianGP
      end
    end
  end
end
