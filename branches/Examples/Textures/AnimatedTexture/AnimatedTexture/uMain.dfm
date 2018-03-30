object Main: TMain
  Left = 238
  Top = 135
  BorderIcons = [biSystemMenu]
  Caption = 'OffSet Animation Demo'
  ClientHeight = 315
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    490
    315)
  PixelsPerInch = 96
  TextHeight = 13
  object SceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 490
    Height = 315
    Camera = Cam
    Buffer.BackgroundColor = clSilver
    Buffer.Lighting = False
    Buffer.DepthPrecision = dp32bits
    Buffer.ColorDepth = cd24bits
    FieldOfView = 144.774841308593800000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Button1: TButton
    Left = 411
    Top = 8
    Width = 81
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Scene: TGLScene
    Left = 8
    Top = 8
    object DummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Cube1: TGLCube
        Material.MaterialLibrary = tlp
        CubeSize = {000020410000204100002041}
      end
      object Cube2: TGLCube
        Material.MaterialLibrary = tlp
        CubeSize = {000020410000204100002041}
      end
    end
    object hsp: TGLHUDSprite
      Material.MaterialLibrary = tlp
      Position.Coordinates = {00008C4200008C42000000000000803F}
      Width = 128.000000000000000000
      Height = 128.000000000000000000
      Rotation = 0.000000000000000000
    end
    object Cam: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Cube1
      Position.Coordinates = {00000C4200007041000000000000803F}
      Left = 232
      Top = 152
    end
  end
  object Tick: TGLCadencer
    Scene = Scene
    OnProgress = TickProgress
    Left = 40
    Top = 8
  end
  object tlp: TGLMaterialLibrary
    Left = 72
    Top = 8
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 8
    Top = 40
  end
end
