object frmXOR: TfrmXOR
  Left = 491
  Top = 111
  Width = 541
  Height = 649
  Caption = 'XOR'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    533
    595)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 304
    Top = 33
    Width = 229
    Height = 560
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 0
    Top = 33
    Width = 297
    Height = 560
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 533
    Height = 49
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      Left = 288
      Top = 12
      Width = 26
      Height = 13
      Caption = 'Tests'
    end
    object Label2: TLabel
      Left = 376
      Top = 12
      Width = 88
      Height = 13
      Caption = 'Initial weight range'
    end
    object CheckBox_SpeciesDetails: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Species Details'
      TabOrder = 0
    end
    object Button_LoadIndividual: TButton
      Left = 112
      Top = 8
      Width = 89
      Height = 21
      Caption = 'Load individual'
      TabOrder = 1
      OnClick = Button_LoadIndividualClick
    end
    object Button_ShowCode: TButton
      Left = 208
      Top = 8
      Width = 75
      Height = 21
      Caption = 'As Code'
      TabOrder = 2
      OnClick = Button_ShowCodeClick
    end
    object Edit_Tests: TEdit
      Left = 320
      Top = 8
      Width = 41
      Height = 21
      TabOrder = 3
      Text = '100'
    end
    object Edit_InitialWeightRange: TEdit
      Left = 472
      Top = 8
      Width = 25
      Height = 21
      TabOrder = 4
      Text = '1'
    end
  end
  object NEATPopulation: TNEATPopulation
    PopulationSize = 300
    SpeciesTargetCount = 0
    ReproduceMutateRate = 0.250000000000000000
    MinSpeciesRelatedness = 2.500000000000000000
    InputNodeCount = 3
    GenerationsToRun = 1000
    TargetFitness = 4.000000000000000000
    TournamentSize = 14
    ConnectionWeightMutationChance = 0.800000000000000000
    ConnectionMutationBigChangeChance = 0.100000000000000000
    ActivationIterations = 0
    AllowRecurrentLinks = False
    SurvivalThreshold = 0.200000000000000000
    ConnectionSplitChance = 0.030000000000000000
    ConnectionAddChance = 0.050000000000000000
    WeightUnrelatednessFactor = 0.400000005960464400
    WeightPeturbationFactor = 2.500000000000000000
    InitialWeightMagnitude = 1.000000000000000000
    MaxLinkWeight = 3.000000000000000000
    OnCalculateFitness = NEATPopulationCalculateFitness
    OnShowBestIndividual = NEATPopulationShowBestIndividual
    OnPrepareInitialGenotype = NEATPopulationPrepareInitialGenotype
    Left = 32
    Top = 48
  end
  object MainMenu1: TMainMenu
    Left = 72
    Top = 88
    object GO1: TMenuItem
      Caption = 'GO'
      OnClick = GO1Click
    end
    object BigGO1: TMenuItem
      Caption = '100 &Benchmark'
      OnClick = BigGO1Click
    end
  end
end
