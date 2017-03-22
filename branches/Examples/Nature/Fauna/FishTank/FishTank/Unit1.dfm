object Form1: TForm1
  Left = 324
  Top = 173
  Caption = '0'
  ClientHeight = 441
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 33
    Width = 680
    Height = 408
    Camera = GLCamera1
    FieldOfView = 152.456802368164100000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 680
    Height = 33
    Align = alTop
    Caption = 'if you can read this you sit too close to the screen'
    Font.Charset = GREEK_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 48
      Top = 10
      Width = 32
      Height = 14
      Caption = 'Label1'
      Font.Charset = GREEK_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 10
      Width = 34
      Height = 14
      Caption = 'Speed:'
      Font.Charset = GREEK_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Button1: TButton
      Left = 88
      Top = 8
      Width = 17
      Height = 17
      Caption = '+'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 112
      Top = 8
      Width = 17
      Height = 17
      Caption = '-'
      TabOrder = 1
      OnClick = Button2Click
    end
    object BtnForTarget1: TButton
      Left = 584
      Top = 8
      Width = 17
      Height = 17
      Caption = 'T1'
      TabOrder = 2
      OnClick = BtnForTarget1Click
    end
    object BtnForTarget2: TButton
      Left = 608
      Top = 8
      Width = 17
      Height = 17
      Caption = 'T2'
      TabOrder = 3
      OnClick = BtnForTarget2Click
    end
    object BtnForTarget3: TButton
      Left = 632
      Top = 8
      Width = 17
      Height = 17
      Caption = 'T3'
      TabOrder = 4
      OnClick = BtnForTarget3Click
    end
    object BtnForTarget4: TButton
      Left = 656
      Top = 8
      Width = 17
      Height = 17
      Caption = 'T4'
      TabOrder = 5
      OnClick = BtnForTarget4Click
    end
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 56
    object GLCube2: TGLCube
      Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
      CubeSize = {0000803F0000A0400000A040}
      object ArrowForDirection: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {0000000000000000000000400000803F}
        BottomRadius = 0.100000001490116100
        Height = 5.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLCamera2: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        Direction.Coordinates = {00000000000000000000803F00000000}
      end
      object BSphere: TGLSphere
        Material.BlendingMode = bmAdditive
        Radius = 0.500000000000000000
      end
      object LinesForDirection: TGLLines
        Nodes = <
          item
          end
          item
            Z = 10.000000000000000000
          end>
        Options = []
      end
    end
    object GLFreeForm1: TGLFreeForm
      Position.Coordinates = {000000000000A0C0000000000000803F}
      Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
      MaterialLibrary = GLMaterialLibrary1
      UseMeshMaterials = False
    end
    object GLDummyCube1: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
    end
    object CollisionSphere: TGLSphere
      Material.FrontProperties.Diffuse.Color = {9A99593F9A99593FCDCCCC3D0000803F}
      Position.Coordinates = {0000A04000000000000000000000803F}
      Radius = 1.000000000000000000
      object GLArrowLine4: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {9A99593F9A99593FCDCCCC3D0000803F}
        Position.Coordinates = {00000000000000000000A0400000803F}
        BottomRadius = 0.100000001490116100
        Height = 10.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
    end
    object GLDummyCube2: TGLDummyCube
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
    end
    object Target1: TGLDummyCube
      Position.Coordinates = {0000A04100000000000000000000803F}
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {ACC8483ECDCC4C3FACC8483E0000803F}
      VisibleAtRunTime = True
    end
    object Target2: TGLDummyCube
      Position.Coordinates = {0000A0C100000000000000000000803F}
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {ACC8483ECDCC4C3FACC8483E0000803F}
      VisibleAtRunTime = True
    end
    object Target3: TGLDummyCube
      Position.Coordinates = {00000000000000000000A0410000803F}
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {ACC8483ECDCC4C3FACC8483E0000803F}
      VisibleAtRunTime = True
    end
    object Target4: TGLDummyCube
      Position.Coordinates = {00000000000000000000A0C10000803F}
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {ACC8483ECDCC4C3FACC8483E0000803F}
      VisibleAtRunTime = True
    end
    object GLCube1: TGLCube
      Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
      CubeSize = {0000803F0000A0400000A040}
      object GLArrowLine1: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {0000000000000000000000400000803F}
        BottomRadius = 0.100000001490116100
        Height = 5.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLCamera3: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        Direction.Coordinates = {00000000000000000000803F00000000}
      end
      object GLSphere1: TGLSphere
        Material.BlendingMode = bmAdditive
        Radius = 0.500000000000000000
      end
      object GLLines1: TGLLines
        Nodes = <
          item
          end
          item
            Z = 10.000000000000000000
          end>
        Options = []
      end
    end
    object GLCube3: TGLCube
      Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
      CubeSize = {0000803F0000A0400000A040}
      object GLArrowLine2: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {0000000000000000000000400000803F}
        BottomRadius = 0.100000001490116100
        Height = 5.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLCamera4: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        Direction.Coordinates = {00000000000000000000803F00000000}
      end
      object GLSphere2: TGLSphere
        Material.BlendingMode = bmAdditive
        Radius = 0.500000000000000000
      end
      object GLLines2: TGLLines
        Nodes = <
          item
          end
          item
            Z = 10.000000000000000000
          end>
        Options = []
      end
    end
    object GLCube4: TGLCube
      Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
      CubeSize = {0000803F0000A0400000A040}
      object GLArrowLine3: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {0000000000000000000000400000803F}
        BottomRadius = 0.100000001490116100
        Height = 5.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLCamera5: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        Direction.Coordinates = {00000000000000000000803F00000000}
      end
      object GLSphere3: TGLSphere
        Material.BlendingMode = bmAdditive
        Radius = 0.500000000000000000
      end
      object GLLines3: TGLLines
        Nodes = <
          item
          end
          item
            Z = 10.000000000000000000
          end>
        Options = []
      end
    end
    object GLCube5: TGLCube
      Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
      CubeSize = {0000803F0000A0400000A040}
      object GLArrowLine5: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {0000000000000000000000400000803F}
        BottomRadius = 0.100000001490116100
        Height = 5.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLCamera6: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        Direction.Coordinates = {00000000000000000000803F00000000}
      end
      object GLSphere4: TGLSphere
        Material.BlendingMode = bmAdditive
        Radius = 0.500000000000000000
      end
      object GLLines4: TGLLines
        Nodes = <
          item
          end
          item
            Z = 10.000000000000000000
          end>
        Options = []
      end
    end
    object GLCube6: TGLCube
      Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
      CubeSize = {0000803F0000A0400000A040}
      object GLArrowLine6: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {0000000000000000000000400000803F}
        BottomRadius = 0.100000001490116100
        Height = 5.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLCamera7: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        Direction.Coordinates = {00000000000000000000803F00000000}
      end
      object GLSphere5: TGLSphere
        Material.BlendingMode = bmAdditive
        Radius = 0.500000000000000000
      end
      object GLLines5: TGLLines
        Nodes = <
          item
          end
          item
            Z = 10.000000000000000000
          end>
        Options = []
      end
    end
    object GLCube7: TGLCube
      Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
      CubeSize = {0000803F0000A0400000A040}
      object GLArrowLine7: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {0000000000000000000000400000803F}
        BottomRadius = 0.100000001490116100
        Height = 5.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLCamera8: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        Direction.Coordinates = {00000000000000000000803F00000000}
      end
      object GLSphere6: TGLSphere
        Material.BlendingMode = bmAdditive
        Radius = 0.500000000000000000
      end
      object GLLines6: TGLLines
        Nodes = <
          item
          end
          item
            Z = 10.000000000000000000
          end>
        Options = []
      end
    end
    object GLCube8: TGLCube
      Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
      CubeSize = {0000803F0000A0400000A040}
      object GLArrowLine8: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {0000000000000000000000400000803F}
        BottomRadius = 0.100000001490116100
        Height = 5.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLCamera9: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        Direction.Coordinates = {00000000000000000000803F00000000}
      end
      object GLSphere7: TGLSphere
        Material.BlendingMode = bmAdditive
        Radius = 0.500000000000000000
      end
      object GLLines7: TGLLines
        Nodes = <
          item
          end
          item
            Z = 10.000000000000000000
          end>
        Options = []
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 150.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLCube2
      Position.Coordinates = {0000A0410000A0410000A0410000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 56
  end
  object TimerForFps: TTimer
    Interval = 500
    OnTimer = TimerForFpsTimer
    Left = 96
    Top = 56
  end
  object TimerForSpeed: TTimer
    OnTimer = TimerForSpeedTimer
    Left = 128
    Top = 56
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 176
    Top = 56
  end
end
