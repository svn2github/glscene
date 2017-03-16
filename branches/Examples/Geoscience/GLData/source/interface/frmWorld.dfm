object formWorld: TformWorld
  Left = 522
  Top = 537
  BorderStyle = bsSizeToolWin
  Caption = 'glData World'
  ClientHeight = 441
  ClientWidth = 379
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pcWorld: TPageControl
    Left = 0
    Top = 0
    Width = 379
    Height = 441
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    ExplicitWidth = 387
    ExplicitHeight = 453
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object GLSceneViewer1: TGLSceneViewer
        Left = 0
        Top = 29
        Width = 371
        Height = 386
        Camera = GLCamera
        Buffer.BackgroundColor = clWhite
        FieldOfView = 149.829803466796900000
        Align = alClient
        TabOrder = 0
      end
      object ActionToolBar1: TActionToolBar
        Left = 0
        Top = 0
        Width = 371
        Height = 29
        Caption = 'ActionToolBar1'
        Color = clMenuBar
        ColorMap.DisabledFontColor = 7171437
        ColorMap.HighlightColor = 14410210
        ColorMap.BtnSelectedColor = clBtnFace
        ColorMap.BtnSelectedFont = clBlack
        ColorMap.UnusedColor = 14410210
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Spacing = 0
        ExplicitWidth = 379
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
  object GLSceneWorld: TGLScene
    Left = 348
    Top = 400
    object Sun: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      LightStyle = lsParallel
      SpotCutOff = 180.000000000000000000
      SpotDirection.Coordinates = {0000803F000000000000000000000000}
    end
    object GLDummyCube: TGLDummyCube
      Direction.Coordinates = {0000000084F83FB20000803F00000000}
      ShowAxes = True
      Up.Coordinates = {000000000000803F84F83F3200000000}
      CubeSize = 1.000000000000000000
      object glPlanet: TGLSphere
        Radius = 1500.000000000000000000
        Slices = 32
        Stacks = 32
      end
    end
    object GLCamera: TGLCamera
      DepthOfView = 10000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube
      Position.Coordinates = {0000FA440000FA440000FA440000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
end
