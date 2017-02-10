object frmGeneticArt: TfrmGeneticArt
  Left = 116
  Top = 151
  ActiveControl = Button_NewGeneration
  Anchors = [akTop, akRight]
  Caption = 'NEAT Based Genetic Art'
  ClientHeight = 573
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  Visible = True
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  DesignSize = (
    763
    573)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 505
    Height = 13
    Caption = 
      'NOTE: Click on one or more images that you like, then click "New' +
      ' Generation". RIGHT click for zoom et al.'
  end
  object Button_Restart: TButton
    Left = 675
    Top = 48
    Width = 89
    Height = 21
    Hint = 'Restart the run with a completely random set of images'
    Anchors = [akTop, akRight]
    Caption = '&Restart'
    TabOrder = 0
    OnClick = Button_RestartClick
  end
  object Button_NewGeneration: TButton
    Left = 675
    Top = 24
    Width = 89
    Height = 21
    Hint = 'Create a new generation of images'
    Anchors = [akTop, akRight]
    Caption = '&New Generation'
    Default = True
    TabOrder = 1
    OnClick = Button_NewGenerationClick
  end
  object Panel_Images: TPanel
    Left = 8
    Top = 24
    Width = 660
    Height = 549
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    DesignSize = (
      660
      549)
    object Shape1: TShape
      Left = -464
      Top = -432
      Width = 1124
      Height = 981
      Anchors = [akLeft, akTop, akRight, akBottom]
      Brush.Color = clBlack
    end
  end
  object cbUseColor: TCheckBox
    Left = 675
    Top = 72
    Width = 73
    Height = 17
    Caption = 'Use Color'
    TabOrder = 3
    OnClick = cbUseColorClick
  end
  object GroupBox1: TGroupBox
    Left = 672
    Top = 96
    Width = 91
    Height = 73
    Anchors = [akTop, akRight]
    Caption = 'Inputs'
    TabOrder = 4
    object cbCoords: TCheckBox
      Left = 5
      Top = 16
      Width = 52
      Height = 17
      Hint = 'Use coords (pixel x and y) as input to the neural network'
      Caption = 'Coords'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbCoordsClick
    end
    object cbPerlinNoise: TCheckBox
      Left = 5
      Top = 32
      Width = 78
      Height = 17
      Hint = 'Use Noise as input to the neural network'
      Caption = 'Perlin Noise'
      TabOrder = 1
      OnClick = cbCoordsClick
    end
    object cbModifyPerlin: TCheckBox
      Left = 13
      Top = 48
      Width = 68
      Height = 17
      Hint = 
        'Modify perlin noise so that it'#39's different from genotype to geno' +
        'type'
      Caption = 'Modify P.'
      TabOrder = 2
      Visible = False
    end
  end
  object Button_Zoom: TButton
    Left = 680
    Top = 176
    Width = 75
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '&Zoom'
    TabOrder = 5
    OnClick = Button_ZoomClick
  end
  object Button_GodMode: TButton
    Left = 680
    Top = 200
    Width = 75
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '&God Mode'
    TabOrder = 6
    OnClick = Button_GodModeClick
  end
  object Button_Favorites: TButton
    Left = 680
    Top = 232
    Width = 75
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '&Favorites'
    TabOrder = 7
    OnClick = Button_FavoritesClick
  end
  object NEATPopulation: TNEATPopulation
    PopulationSize = 9
    SpeciesTargetCount = 0
    ReproduceMutateRate = 0.250000000000000000
    MinSpeciesRelatedness = 3.000000000000000000
    SelectionType = stFitProp
    InputNodeCount = 7
    GenerationsToRun = 10000
    TargetFitness = 10000.000000000000000000
    ConnectionWeightMutationChance = 0.500000000000000000
    ConnectionMutationBigChangeChance = 0.100000000000000000
    ActivationIterations = 4
    SurvivalThreshold = 0.400000000000000000
    ConnectionSplitChance = 0.060000000000000000
    ConnectionAddChance = 0.060000000000000000
    WeightUnrelatednessFactor = 0.400000005960464400
    WeightPeturbationFactor = 2.500000000000000000
    InitialWeightMagnitude = 1.000000000000000000
    MaxLinkWeight = 3.000000000000000000
    OnCalculateFitness = NEATPopulationCalculateFitness
    Left = 696
    Top = 312
  end
  object XPManifest1: TXPManifest
    Left = 696
    Top = 264
  end
  object PopupMenu_Image: TPopupMenu
    Left = 696
    Top = 256
    object RaiseFitness1: TMenuItem
      Caption = '&Raise Fitness'
      OnClick = RaiseFitness1Click
    end
    object LowerFitness1: TMenuItem
      Caption = '&Lower Fitness'
      OnClick = LowerFitness1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SaveGenotype1: TMenuItem
      Caption = '&Add To Favorites'
      OnClick = SaveGenotype1Click
    end
    object SaveImage1: TMenuItem
      Caption = '&Save Image'
      OnClick = SaveImage1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object ZoomImage1: TMenuItem
      Caption = '&Zoom Image'
      OnClick = ZoomImage1Click
    end
    object GodMode1: TMenuItem
      Caption = 'Ken'#39's &God Mode'
      OnClick = GodMode1Click
    end
  end
  object SaveDialog_SaveImage: TSaveDialog
    Filter = 'JPG Files|*.jpg|BMP Files|*.bmp'
    InitialDir = 'Output'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 696
    Top = 360
  end
end
