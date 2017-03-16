object MainForm: TMainForm
  Left = 436
  Top = 247
  Caption = 'MainForm'
  ClientHeight = 371
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSV: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 554
    Height = 371
    Camera = cam
    FieldOfView = 128.236984252929700000
    Align = alClient
    OnMouseDown = GLSVMouseDown
    OnMouseMove = GLSVMouseMove
    TabOrder = 0
  end
  object GLScene: TGLScene
    Left = 8
    Top = 16
    object SbBackground: TGLSkyBox
      CloudsPlaneOffset = 0.200000002980232200
      CloudsPlaneSize = 32.000000000000000000
      object Moons: TGLDummyCube
        Position.Coordinates = {0000A0C1000020410000A0C10000803F}
        CubeSize = 1.000000000000000000
        object sprMasser: TGLSprite
          Width = 10.000000000000000000
          Height = 10.000000000000000000
          Rotation = 0.000000000000000000
        end
        object sprSecunda: TGLSprite
          Position.Coordinates = {00000000000000000000A0400000803F}
          Width = 5.000000000000000000
          Height = 5.000000000000000000
          Rotation = 0.000000000000000000
        end
      end
      object sprSun: TGLSprite
        Position.Coordinates = {000040C000004040000020C10000803F}
        Width = 5.000000000000000000
        Height = 5.000000000000000000
        Rotation = 0.000000000000000000
      end
    end
    object SbClouds: TGLSkyBox
      Position.Coordinates = {0000A0C1000020410000A0C10000803F}
      CloudsPlaneOffset = 0.200000002980232200
      CloudsPlaneSize = 32.000000000000000000
    end
    object dc_cam: TGLDummyCube
      CubeSize = 1.000000000000000000
      object cam: TGLCamera
        DepthOfView = 500.000000000000000000
        FocalLength = 90.000000000000000000
        TargetObject = dc_cam
        Position.Coordinates = {0000204100000000000020410000803F}
      end
    end
  end
  object Cadencer: TGLCadencer
    Scene = GLScene
    OnProgress = CadencerProgress
    Left = 40
    Top = 16
  end
  object Timer: TGLAsyncTimer
    OnTimer = TimerTimer
    ThreadPriority = tpNormal
    Left = 72
    Top = 16
  end
  object MatLib: TGLMaterialLibrary
    Left = 104
    Top = 16
  end
  object CgBackground: TCgShader
    FragmentProgram.OnApply = CgBackgroundApplyFP
    FragmentProgram.OnUnApply = CgBackgroundUnApplyFP
    OnApplyFP = CgBackgroundApplyFP
    OnUnApplyFP = CgBackgroundUnApplyFP
    Left = 136
    Top = 16
  end
  object CgClouds: TCgShader
    VertexProgram.OnApply = CgCloudsApplyVP
    FragmentProgram.OnApply = CgCloudsApplyFP
    FragmentProgram.OnUnApply = CgCloudsUnApplyFP
    OnApplyVP = CgCloudsApplyVP
    OnApplyFP = CgCloudsApplyFP
    OnUnApplyFP = CgCloudsUnApplyFP
    Left = 168
    Top = 16
  end
  object CgMasser: TCgShader
    FragmentProgram.OnApply = CgMasserApplyFP
    FragmentProgram.OnUnApply = CgMasserUnApplyFP
    OnApplyFP = CgMasserApplyFP
    OnUnApplyFP = CgMasserUnApplyFP
    Left = 200
    Top = 16
  end
  object CgSecunda: TCgShader
    FragmentProgram.OnApply = CgSecundaApplyFP
    FragmentProgram.OnUnApply = CgSecundaUnApplyFP
    OnApplyFP = CgSecundaApplyFP
    OnUnApplyFP = CgSecundaUnApplyFP
    Left = 232
    Top = 16
  end
  object CgSun: TCgShader
    FragmentProgram.OnApply = CgSunApplyFP
    FragmentProgram.OnUnApply = CgSunUnApplyFP
    OnApplyFP = CgSunApplyFP
    OnUnApplyFP = CgSunUnApplyFP
    Left = 264
    Top = 16
  end
end
