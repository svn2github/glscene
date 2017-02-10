object frmMainForm: TfrmMainForm
  Left = 194
  Top = 163
  Width = 819
  Height = 648
  Caption = 'frmMainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    811
    621)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 368
    Top = 216
    Width = 433
    Height = 393
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 8
    Top = 8
    Width = 353
    Height = 601
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Chart1: TChart
    Left = 368
    Top = 8
    Width = 433
    Height = 201
    AnimatedZoom = True
    AnimatedZoomSteps = 15
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    BackWall.Pen.Mode = pmMask
    BackWall.Pen.Style = psInsideFrame
    Gradient.Direction = gdFromTopLeft
    Gradient.EndColor = 14802389
    Gradient.Visible = True
    LeftWall.Brush.Color = clWhite
    LeftWall.Pen.Visible = False
    MarginBottom = 6
    MarginLeft = 2
    MarginRight = 2
    Title.AdjustFrame = False
    Title.Alignment = taRightJustify
    Title.Text.Strings = (
      'Regression')
    Title.Visible = False
    AxisVisible = False
    Frame.Mode = pmMask
    Frame.Style = psInsideFrame
    LeftAxis.Axis.Width = 1
    LeftAxis.Grid.Style = psSolid
    LeftAxis.Grid.Visible = False
    LeftAxis.LabelsSeparation = 20
    LeftAxis.TickLength = 3
    Legend.Alignment = laBottom
    Legend.ColorWidth = 15
    Legend.ShadowSize = 0
    Legend.TextStyle = ltsPlain
    Legend.TopPos = 0
    View3D = False
    View3DWalls = False
    BevelOuter = bvNone
    BorderWidth = 1
    Color = clBlack
    TabOrder = 2
    Anchors = [akLeft, akTop, akRight]
    object Series1: TFastLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = 5673385
      Title = 'Target'
      LinePen.Color = 5673385
      LinePen.Width = 3
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1
      YValues.Order = loNone
    end
    object Series2: TFastLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = 6711319
      Title = 'Best aprox'
      LinePen.Color = 6711319
      LinePen.Width = 3
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1
      YValues.Order = loNone
    end
  end
  object Population: TNNPopulation
    PopulationSize = 1000
    ReproduceMutateRate = 0.25
    MinSpeciesRelatedness = 3
    SelectionType = stLocalTournament
    InputNodeCount = 2
    OutputNodeCount = 1
    BinaryWeights = False
    OnCalculateFitness = PopulationCalculateFitness
    Left = 112
    Top = 72
  end
end
