object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 558
  ClientWidth = 700
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DGLSceneViewer1: TDGLSceneViewer
    Left = 0
    Top = 0
    Width = 700
    Height = 558
    Camera = DGLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    Buffer.ShadeModel = smSmooth
    FieldOfView = 171.799575805664100000
    Align = alClient
    TabOrder = 0
  end
  object DGLScene1: TDGLScene
    VisibilityCulling = vcObjectBased
    Left = 456
    Top = 328
    object DGLCamera1: TDGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 20.000000000000000000
      NearPlaneBias = 0.009999999776482582
      TargetObject = DGLCube1
      CameraStyle = csInfinitePerspective
      KeepFOVMode = ckmVerticalFOV
      Position.Coordinates = {00803BC400803B4400803B440000803F}
      Direction.Coordinates = {FFFF7F3F000000000000008000000000}
    end
    object DGLCube1: TDGLCube
      MeshMode = mmStatic
      Pickable = False
    end
  end
  object DGLCadencer1: TDGLCadencer
    Scene = DGLScene1
    Enabled = False
    OnProgress = DGLCadencer1Progress
    Left = 456
    Top = 416
  end
end
