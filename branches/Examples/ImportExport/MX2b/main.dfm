object Form1: TForm1
  Left = 220
  Top = 207
  Caption = 'MX2 Demo'
  ClientHeight = 427
  ClientWidth = 711
  Color = clSilver
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Main_Panel1: TPanel
    Left = 0
    Top = 0
    Width = 515
    Height = 408
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object GLSceneViewer1: TGLSceneViewer
      Left = 0
      Top = 0
      Width = 515
      Height = 408
      Camera = GLCamera1
      VSync = vsmSync
      Buffer.BackgroundColor = 4210752
      FieldOfView = 152.456802368164100000
      Align = alClient
      OnMouseDown = GLSceneViewer1MouseDown
      OnMouseMove = GLSceneViewer1MouseMove
    end
  end
  object Tools_Panel2: TPanel
    Left = 515
    Top = 0
    Width = 196
    Height = 408
    Align = alRight
    BevelOuter = bvNone
    Color = 10724259
    TabOrder = 1
    object btnLoadMX2Actor1: TButton
      Left = 35
      Top = 15
      Width = 136
      Height = 25
      Caption = 'Load MX2 actor'
      TabOrder = 0
      OnClick = btnLoadMX2Actor1Click
    end
    object dbg1: TMemo
      Left = 5
      Top = 90
      Width = 186
      Height = 326
      TabOrder = 1
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 408
    Width = 711
    Height = 19
    Color = clSilver
    Panels = <
      item
        Width = 50
      end>
  end
  object OpenDialog1: TOpenDialog
    Filter = 'VRStudio MX2 (mx2b/mx2a)|*.mx2b;*.mx2a'
    InitialDir = '.\MX2'
    Left = 550
    Top = 45
  end
  object GLScene1: TGLScene
    Left = 5
    Top = 5
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLActor1: TGLActor
        Interval = 100
        MaterialLibrary = GLMaterialLibrary1
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00000000000000400000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00000000000000400000C8C20000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 200.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {00000000000000000000A0410000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    TexturePaths = '.'
    Left = 35
    Top = 5
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    OnProgress = GLCadencer1Progress
    Left = 75
    Top = 5
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 110
    Top = 5
  end
end
