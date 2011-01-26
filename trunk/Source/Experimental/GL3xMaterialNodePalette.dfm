object NodePaletteForm: TNodePaletteForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Node Palette'
  ClientHeight = 735
  ClientWidth = 200
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 200
  DockSite = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object CategoryPanelGroup1: TCategoryPanelGroup
    Left = 0
    Top = 0
    Height = 735
    VertScrollBar.Position = 424
    VertScrollBar.Tracking = True
    Align = alClient
    ChevronHotColor = 1093631
    Color = clBlack
    Ctl3D = True
    GradientBaseColor = 1093615
    GradientColor = clCream
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -11
    HeaderFont.Name = 'Tahoma'
    HeaderFont.Style = []
    ParentCtl3D = False
    TabOrder = 0
    ExplicitHeight = 657
    object ConstantsPanel: TCategoryPanel
      Top = -424
      Height = 156
      Caption = 'Constants'
      TabOrder = 0
      object ConstantGroup: TButtonGroup
        Left = 0
        Top = 0
        Width = 179
        Height = 130
        Align = alClient
        BorderStyle = bsNone
        ButtonOptions = [gboAllowReorder, gboFullSize, gboShowCaptions]
        Items = <
          item
            Caption = 'Constant'
          end
          item
            Caption = 'Constant Vector2'
          end
          item
            Caption = 'Constant Vector3'
          end
          item
            Caption = 'Constant Vector4'
          end
          item
            Caption = 'Vertex Color'
          end>
        TabOrder = 0
        TabStop = False
        OnMouseDown = CommonMouseDown
      end
    end
    object CoordinatesPanel: TCategoryPanel
      Top = -268
      Height = 223
      Caption = 'Coordinates'
      TabOrder = 1
      object CoordinateGroup: TButtonGroup
        Left = 0
        Top = 0
        Width = 179
        Height = 197
        Align = alClient
        BorderStyle = bsNone
        ButtonOptions = [gboAllowReorder, gboFullSize, gboShowCaptions]
        Items = <
          item
            Caption = 'Object Position'
          end
          item
            Caption = 'World Position'
          end
          item
            Caption = 'World Normal'
          end
          item
            Caption = 'World Camera Position'
          end
          item
            Caption = 'Texture Coordinate'
          end
          item
            Caption = 'Panner'
          end
          item
            Caption = 'Rotator'
          end
          item
            Caption = 'Screen Position'
          end>
        TabOrder = 0
        TabStop = False
        OnMouseDown = CommonMouseDown
      end
    end
    object MathPanel: TCategoryPanel
      Top = -45
      Height = 422
      Caption = 'Math'
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 2
      object MathGroup: TButtonGroup
        Left = 0
        Top = 0
        Width = 179
        Height = 396
        Align = alClient
        BorderStyle = bsNone
        ButtonOptions = [gboAllowReorder, gboFullSize, gboShowCaptions]
        Items = <
          item
            Caption = 'Add'
          end
          item
            Caption = 'Subtract'
          end
          item
            Caption = 'Multiply'
          end
          item
            Caption = 'Divide'
          end
          item
            Caption = 'Normalize'
          end
          item
            Caption = 'Power'
          end
          item
            Caption = 'DotProduct'
          end
          item
            Caption = 'Sine'
          end
          item
            Caption = 'Cosine'
          end
          item
            Caption = 'Floor'
          end
          item
            Caption = 'Abs'
          end
          item
            Caption = 'Fract'
          end
          item
            Caption = 'OneMinus'
          end
          item
            Caption = 'SquareRoot'
          end
          item
            Caption = 'Sign'
          end
          item
            Caption = 'SmoothStep'
          end>
        TabOrder = 0
        TabStop = False
        OnMouseDown = CommonMouseDown
      end
    end
    object VectorsPanel: TCategoryPanel
      Top = 377
      Height = 136
      Caption = 'Vectors'
      TabOrder = 3
      object VectorsGroup: TButtonGroup
        Left = 0
        Top = 0
        Width = 179
        Height = 110
        Align = alClient
        BorderStyle = bsNone
        ButtonOptions = [gboAllowReorder, gboFullSize, gboShowCaptions]
        Items = <
          item
            Caption = 'World Normal'
          end
          item
            Caption = 'Light Vector'
          end
          item
            Caption = 'Camera Vector'
          end
          item
            Caption = 'Reflection Vector'
          end>
        TabOrder = 0
        TabStop = False
        OnMouseDown = CommonMouseDown
      end
    end
    object TexturePanel: TCategoryPanel
      Top = 513
      Height = 88
      Caption = 'Texture'
      TabOrder = 4
      object TextureGroup: TButtonGroup
        Left = 0
        Top = 0
        Width = 179
        Height = 62
        Align = alClient
        BorderStyle = bsNone
        ButtonOptions = [gboAllowReorder, gboFullSize, gboShowCaptions]
        Items = <
          item
            Caption = 'Texture Sampler'
          end
          item
            Caption = 'Texture Atlas'
          end>
        TabOrder = 0
        TabStop = False
        OnMouseDown = CommonMouseDown
      end
    end
    object UtilityPanel: TCategoryPanel
      Top = 601
      Height = 157
      Caption = 'Utility'
      TabOrder = 5
      object UtilityGroup: TButtonGroup
        Left = 0
        Top = 0
        Width = 179
        Height = 131
        Align = alClient
        BorderStyle = bsNone
        ButtonOptions = [gboAllowReorder, gboFullSize, gboShowCaptions]
        Items = <
          item
            Caption = 'Timer'
          end
          item
            Caption = 'ComponentMask'
          end
          item
            Caption = 'Clamp'
          end
          item
            Caption = 'ConstantClamp'
          end
          item
            Caption = 'AppendVector'
          end>
        TabOrder = 0
        TabStop = False
        OnMouseDown = CommonMouseDown
      end
    end
  end
end
