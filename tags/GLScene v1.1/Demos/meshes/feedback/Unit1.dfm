object Form1: TForm1
  Left = 192
  Top = 107
  Width = 413
  Height = 328
  Caption = 'OpenGL Feedback into a GLFreeForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 397
    Height = 290
    Camera = GLCamera1
    Buffer.FaceCulling = False
    FieldOfView = 141.948791503906300000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Button1: TButton
    Left = 152
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Build Mesh'
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000803F00000040000040400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object GLFreeForm1: TGLFreeForm
    end
    object GLFeedback1: TGLFeedback
      MaxBufferSize = 1048576
      Active = False
      Mode = fm3DColorTexture
      Visible = False
      object MeshObject1: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object GLCube1: TGLCube
          Position.Coordinates = {CDCC0CBFCDCC0CBF000000000000803F}
        end
        object GLDodecahedron1: TGLDodecahedron
          Position.Coordinates = {CDCC0C3FCDCC0CBF000000000000803F}
        end
      end
      object MeshObject2: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object GLSphere1: TGLSphere
          Position.Coordinates = {00000000CDCC0C3F000000000000803F}
          Radius = 0.500000000000000000
        end
      end
    end
  end
end
