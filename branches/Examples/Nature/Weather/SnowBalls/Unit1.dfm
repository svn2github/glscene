object Form1: TForm1
  Left = 189
  Top = 161
  Caption = 'Form1'
  ClientHeight = 659
  ClientWidth = 868
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 868
    Height = 659
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.DepthTest = False
    FieldOfView = 162.742950439453100000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 20.000000000000000000
      VisibleAtRunTime = True
      object GLParticles1: TGLParticles
        ObjectsSorting = osNone
        Scale.Coordinates = {00000040000000400000004000000000}
        CubeSize = 1.000000000000000000
        OnActivateParticle = GLParticles1ActivateParticle
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLParticles1
      Position.Coordinates = {0000704100000000000000000000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    Left = 8
    Top = 48
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 48
    Top = 8
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 48
    Top = 48
  end
end
