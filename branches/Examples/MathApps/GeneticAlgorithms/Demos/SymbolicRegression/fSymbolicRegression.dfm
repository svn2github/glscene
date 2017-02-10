object frmMainForm: TfrmMainForm
  Left = 395
  Top = 218
  Caption = 'Symbolic Regression with NEAT'
  ClientHeight = 609
  ClientWidth = 869
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
    869
    609)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 392
    Top = 216
    Width = 459
    Height = 354
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
    Top = 232
    Width = 377
    Height = 338
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
    Left = 392
    Top = 8
    Width = 459
    Height = 201
    BackWall.Brush.Style = bsClear
    BackWall.Pen.Mode = pmMask
    BackWall.Pen.Style = psInsideFrame
    Gradient.Direction = gdFromTopLeft
    Gradient.EndColor = 14802389
    Gradient.Visible = True
    LeftWall.Pen.Visible = False
    Legend.Alignment = laBottom
    Legend.ColorWidth = 15
    Legend.Shadow.HorizSize = 0
    Legend.Shadow.VertSize = 0
    Legend.Symbol.Width = 15
    Legend.TextStyle = ltsPlain
    Legend.TopPos = 0
    MarginBottom = 6
    MarginLeft = 2
    MarginRight = 2
    Title.Alignment = taRightJustify
    Title.Text.Strings = (
      'Regression')
    Title.Visible = False
    Title.AdjustFrame = False
    AxisVisible = False
    Frame.Mode = pmMask
    Frame.Style = psInsideFrame
    LeftAxis.Axis.Width = 1
    LeftAxis.Grid.Visible = False
    LeftAxis.LabelsSeparation = 20
    LeftAxis.TickLength = 3
    View3D = False
    View3DWalls = False
    Zoom.Animated = True
    Zoom.AnimatedSteps = 15
    BevelOuter = bvNone
    BorderWidth = 1
    Color = clBlack
    TabOrder = 2
    Anchors = [akLeft, akTop, akRight]
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series1: TFastLineSeries
      SeriesColor = 5673385
      Title = 'Target'
      LinePen.Color = 5673385
      LinePen.Width = 3
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TFastLineSeries
      SeriesColor = 6711319
      Title = 'Best aprox'
      LinePen.Color = 6711319
      LinePen.Width = 3
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object RadioGroup_Function: TRadioGroup
    Left = 8
    Top = 8
    Width = 369
    Height = 169
    Caption = 'Function'
    Items.Strings = (
      '0 : f := power(x,3)*2 + sqr(x)+x/3;'
      '1 : f := x*x*x*3+sqr(x);'
      '2 : f := round(x);'
      '3 : f := AdjustedGaussian(x);'
      '4 : f := 2*exp(-sqr(x*2.5))-1;'
      '5 : if abs(x)>0.5 then f := -abs(x) else f := abs(x);//}'
      '6 : f := abs(x);'
      '7 : f := tanh(x*5);'
      '8 : f := IfThen(x>0, -sqr(x), round(-x));'
      '9 : f := sin(x);')
    TabOrder = 3
    OnClick = RadioGroup_FunctionClick
  end
  object CheckBox_AllowRecurrencies: TCheckBox
    Left = 8
    Top = 184
    Width = 201
    Height = 17
    Caption = 'Allow recurrencies (this is cheating)'
    TabOrder = 4
    OnClick = CheckBox_AllowRecurrenciesClick
  end
  object Button1: TButton
    Left = 8
    Top = 208
    Width = 75
    Height = 21
    Action = Action_Start
    TabOrder = 5
  end
  object Button2: TButton
    Left = 88
    Top = 208
    Width = 75
    Height = 21
    Action = Action_Stop
    TabOrder = 6
  end
  object Population: TNEATPopulation
    PopulationSize = 900
    SpeciesTargetCount = 60
    ReproduceMutateRate = 0.250000000000000000
    MinSpeciesRelatedness = 2.000000000000000000
    InputNodeCount = 2
    GenerationsToRun = 300
    TargetFitness = 1.000000000000000000
    ConnectionWeightMutationChance = 0.800000000000000000
    ConnectionMutationBigChangeChance = 0.050000000000000000
    ActivationIterations = -1
    AllowRecurrentLinks = False
    SurvivalThreshold = 0.400000000000000000
    SpeciesFitnessMethod = sfmHighestFitness
    ConnectionSplitChance = 0.030000000000000000
    ConnectionAddChance = 0.050000000000000000
    WeightUnrelatednessFactor = 0.400000005960464400
    WeightPeturbationFactor = 0.100000001490116100
    InitialWeightMagnitude = 1.000000000000000000
    MaxLinkWeight = 3.000000000000000000
    OnCalculateFitness = PopulationCalculateFitness
    OnShowBestIndividual = PopulationShowBestIndividual
    OnBeforeStartRun = PopulationBeforeStartRun
    OnAfterRunStopped = PopulationAfterRunStopped
    Left = 232
    Top = 192
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 280
    Top = 128
  end
  object ActionManager1: TActionManager
    Left = 288
    Top = 232
    StyleName = 'XP Style'
    object Action_Start: TAction
      Caption = '&Start'
      OnExecute = Action_StartExecute
    end
    object Action_Stop: TAction
      Caption = 'S&top'
      Enabled = False
      OnExecute = Action_StopExecute
    end
  end
end
