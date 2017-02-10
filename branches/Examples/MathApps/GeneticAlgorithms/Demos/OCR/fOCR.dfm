object frmOCR: TfrmOCR
  Left = 164
  Top = 157
  Width = 444
  Height = 403
  Caption = 'Optical Character Recognition'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    436
    369)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 345
    Height = 33
  end
  object Memo_Report: TMemo
    Left = 8
    Top = 48
    Width = 313
    Height = 313
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object Memo_Hits: TMemo
    Left = 328
    Top = 48
    Width = 104
    Height = 313
    Anchors = [akTop, akRight, akBottom]
    TabOrder = 1
  end
  object Population: TNEATPopulation
    PopulationSize = 300
    SpeciesTargetCount = 0
    ReproduceMutateRate = 0.250000000000000000
    MinSpeciesRelatedness = 3.000000000000000000
    GenerationsToRun = 500
    TargetFitness = 100.000000000000000000
    ConnectionWeightMutationChance = 0.800000000000000000
    ConnectionMutationBigChangeChance = 0.100000000000000000
    ConnectNodesOfInitialPop = False
    ActivationIterations = 0
    SurvivalThreshold = 0.400000000000000000
    ConnectionSplitChance = 0.030000000000000000
    ConnectionAddChance = 0.050000000000000000
    WeightUnrelatednessFactor = 0.400000005960464400
    WeightPeturbationFactor = 0.200000002980232200
    InitialWeightMagnitude = 1.000000000000000000
    MaxLinkWeight = 3.000000000000000000
    OnCalculateFitness = PopulationCalculateFitness
    OnShowBestIndividual = PopulationShowBestIndividual
    OnPrepareInitialGenotype = PopulationPrepareInitialGenotype
    Left = 32
    Top = 72
  end
end
