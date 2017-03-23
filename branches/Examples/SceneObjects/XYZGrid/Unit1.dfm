object Form1: TForm1
  Left = 237
  Top = 172
  Caption = 'GLXYZGrid with Polyhedron '
  ClientHeight = 512
  ClientWidth = 866
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
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 866
    Height = 512
    Camera = Camera
    Buffer.BackgroundColor = 16492697
    Buffer.FaceCulling = False
    FieldOfView = 137.326278686523400000
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseUp = ViewerMouseUp
    TabOrder = 0
  end
  object Scene: TGLScene
    Left = 24
    Top = 16
    object AmbientLight: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000FA430000FA430000FA430000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
    object GLIcosahedron1: TGLIcosahedron
      Material.FrontProperties.Emission.Color = {295C4F3F8FC2353F1F856B3E0000803F}
      Scale.Coordinates = {0000C8420000C8420000C84200000000}
    end
    object GLXYZGrid1: TGLXYZGrid
      ShowAxes = True
      LineColor.Color = {6666663FC3F5683F48E17A3F0000803F}
      XSamplingScale.Min = -100.000000000000000000
      XSamplingScale.Max = 100.000000000000000000
      XSamplingScale.Step = 10.000000000000000000
      YSamplingScale.Min = -100.000000000000000000
      YSamplingScale.Max = 100.000000000000000000
      YSamplingScale.Step = 10.000000000000000000
      ZSamplingScale.Min = -100.000000000000000000
      ZSamplingScale.Max = 100.000000000000000000
      ZSamplingScale.Step = 10.000000000000000000
      Parts = [gpX, gpY, gpZ]
    end
    object CamH: TGLDummyCube
      Direction.Coordinates = {0100003FD7B35D3F0000000000000000}
      TurnAngle = -30.000000000000000000
      Up.Coordinates = {00000000000000000000803F00000000}
      Visible = False
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
      object CamV: TGLDummyCube
        Direction.Coordinates = {00000000EA46773FEE83843E00000000}
        PitchAngle = 15.000000000000000000
        Up.Coordinates = {00000000EE8384BEEA46773F00000000}
        Visible = False
        CubeSize = 1.000000000000000000
        VisibleAtRunTime = True
        object Camera: TGLCamera
          DepthOfView = 10000.000000000000000000
          FocalLength = 100.000000000000000000
          NearPlaneBias = 0.100000001490116100
          TargetObject = CamH
          Position.Coordinates = {000000000000FAC3000000000000803F}
          Direction.Coordinates = {000000000000803F0000000000000000}
          Up.Coordinates = {00000000000000000000803F00000000}
        end
      end
    end
  end
  object GLCadencer: TGLCadencer
    Scene = Scene
    OnProgress = GLCadencerProgress
    Left = 120
    Top = 16
  end
end
