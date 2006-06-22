object Form1: TForm1
  Left = 124
  Top = 110
  Width = 618
  Height = 336
  BorderWidth = 5
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 17
    Width = 297
    Height = 280
    Camera = GLCamera1
    Align = alLeft
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLSceneViewer2: TGLSceneViewer
    Left = 303
    Top = 17
    Width = 297
    Height = 280
    Camera = GLCamera2
    Align = alRight
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 297
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 304
      Top = 0
      Width = 297
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Label2'
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 32
    object DummyCube1: TGLDummyCube
      CubeSize = 10.000000000000000000
      object Mesh1: TGLMesh
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Mode = mmTriangles
        VertexMode = vmVNC
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00008C42000048420000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {000048420000F0410000A0410000803F}
    end
  end
  object Timer1: TTimer
    Interval = 5000
    OnTimer = Timer1Timer
    Left = 280
    Top = 40
  end
  object GLScene2: TGLScene
    Left = 560
    Top = 40
    object DummyCube2: TGLDummyCube
      CubeSize = 10.000000000000000000
      object Mesh2: TGLMesh
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Mode = mmTriangles
        VertexMode = vmVNC
      end
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00008C42000048420000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera2: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DummyCube2
      Position.Coordinates = {000048420000F0410000A0410000803F}
    end
  end
end
