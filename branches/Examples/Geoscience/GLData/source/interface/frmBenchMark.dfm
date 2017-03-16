object formBenchMark: TformBenchMark
  Left = 24
  Top = 376
  BorderStyle = bsSizeToolWin
  Caption = 'Bench'
  ClientHeight = 441
  ClientWidth = 379
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splitterBench: TSplitter
    Left = 0
    Top = 174
    Width = 379
    Height = 4
    Cursor = crVSplit
    Align = alTop
    Beveled = True
    Color = clSkyBlue
    ParentColor = False
    ExplicitWidth = 387
  end
  object chartBench: TChart
    Left = 0
    Top = 178
    Width = 379
    Height = 263
    Legend.Brush.Gradient.Direction = gdTopBottom
    Legend.Brush.Gradient.Visible = True
    Legend.CurrentPage = False
    Legend.Font.Charset = ANSI_CHARSET
    Legend.Font.Name = 'Tahoma'
    Legend.LegendStyle = lsSeries
    Legend.Shadow.HorizSize = 0
    Legend.Shadow.VertSize = 0
    Legend.TopPos = 4
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.Automatic = False
    BottomAxis.AutomaticMaximum = False
    BottomAxis.AutomaticMinimum = False
    BottomAxis.LabelsFormat.Font.Charset = ANSI_CHARSET
    BottomAxis.LabelsFormat.Font.Name = 'Tahoma'
    BottomAxis.Maximum = 24.000000000000000000
    BottomAxis.Title.Caption = 'Seconds'
    BottomAxis.Title.Font.Charset = ANSI_CHARSET
    BottomAxis.Title.Font.Name = 'Tahoma'
    LeftAxis.LabelsFormat.Font.Charset = ANSI_CHARSET
    LeftAxis.LabelsFormat.Font.Name = 'Tahoma'
    LeftAxis.Title.Caption = 'FPS'
    LeftAxis.Title.Font.Charset = ANSI_CHARSET
    LeftAxis.Title.Font.Name = 'Tahoma'
    LeftAxis.TitleSize = 10
    Pages.ScaleLastPage = False
    RightAxis.Title.Caption = 'FPS'
    TopAxis.Title.Caption = 'Ticks'
    TopAxis.Title.Visible = False
    View3D = False
    Zoom.Pen.Mode = pmNotXor
    Align = alClient
    Color = clWhite
    TabOrder = 0
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series1: TLineSeries
      SeriesColor = clRed
      Title = 'Current'
      Brush.BackColor = clDefault
      LinePen.Color = clRed
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TLineSeries
      SeriesColor = clBlue
      Title = 'Average'
      Brush.BackColor = clDefault
      LinePen.Color = clBlue
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series3: TLineSeries
      Active = False
      SeriesColor = clLime
      Title = '<unused>'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series4: TLineSeries
      Active = False
      SeriesColor = 33023
      Title = '<unused>'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series5: TLineSeries
      Active = False
      SeriesColor = clBlack
      Title = '<unused>'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series6: TLineSeries
      Active = False
      SeriesColor = clPurple
      Title = '<unused>'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series7: TLineSeries
      Active = False
      SeriesColor = 16744703
      Title = '<unused>'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series8: TLineSeries
      Active = False
      SeriesColor = 16744576
      Title = '<unused>'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series9: TLineSeries
      Active = False
      SeriesColor = clMaroon
      Title = '<unused>'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series10: TLineSeries
      Active = False
      SeriesColor = clGray
      Title = '<unused>'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object pnlBenchMark: TPanel
    Left = 0
    Top = 0
    Width = 379
    Height = 174
    Align = alTop
    TabOrder = 1
    object pnlSettings: TPanel
      Left = 1
      Top = 30
      Width = 377
      Height = 140
      Align = alTop
      BevelOuter = bvNone
      Constraints.MinHeight = 140
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object memBenchmark: TMemo
        Left = 0
        Top = 0
        Width = 157
        Height = 140
        Align = alLeft
        Alignment = taRightJustify
        Color = clSkyBlue
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
      object clbSeries: TCheckListBox
        Left = 229
        Top = 0
        Width = 148
        Height = 140
        OnClickCheck = clbSeriesClickCheck
        Align = alRight
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 13
        Items.Strings = (
          'Current'
          'Average'
          '<unused>'
          '<unused>'
          '<unused>'
          '<unused>'
          '<unused>'
          '<unused>'
          '<unused>'
          '<unused>')
        ParentFont = False
        TabOrder = 1
      end
      object bCopyCurrent: TButton
        Left = 161
        Top = 1
        Width = 75
        Height = 25
        Caption = '> Current >'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = bCopyCurrentClick
      end
      object bCopyAverage: TButton
        Left = 161
        Top = 27
        Width = 75
        Height = 25
        Caption = '> Average >'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = bCopyAverageClick
      end
      object bRename: TButton
        Left = 173
        Top = 54
        Width = 50
        Height = 25
        Caption = 'Rename'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = bRenameClick
      end
      object bColour: TButton
        Left = 173
        Top = 80
        Width = 50
        Height = 25
        Caption = 'Colour'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        OnClick = bColourClick
      end
    end
    object pnlTop: TPanel
      Left = 1
      Top = 1
      Width = 377
      Height = 29
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      DesignSize = (
        377
        29)
      object lblTest: TLabel
        Left = 7
        Top = 8
        Width = 21
        Height = 13
        Caption = 'Test'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblN: TLabel
        Left = 125
        Top = 8
        Width = 40
        Height = 13
        Caption = 'Seconds'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object cbBenchMark: TComboBox
        Left = 34
        Top = 4
        Width = 83
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemIndex = 0
        ParentFont = False
        TabOrder = 0
        Text = 'Revolutions'
        Items.Strings = (
          'Revolutions')
      end
      object bTest: TBitBtn
        Left = 230
        Top = 2
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Start'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = bTestClick
      end
      object bOK: TBitBtn
        Left = 306
        Top = 2
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Kind = bkOK
        NumGlyphs = 2
        ParentFont = False
        TabOrder = 1
        OnClick = bOKClick
      end
    end
  end
  object BenchTimer: TTimer
    Enabled = False
    OnTimer = BenchTimerTimer
    Left = 6
    Top = 36
  end
  object GLCadencer: TGLCadencer
    Scene = formMain.GLScene
    Enabled = False
    OnProgress = GLCadencerProgress
    Left = 62
    Top = 36
  end
  object ColorDialog: TColorDialog
    Left = 34
    Top = 36
  end
end
