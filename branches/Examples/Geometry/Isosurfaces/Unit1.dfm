object Form1: TForm1
  Left = 601
  Top = 244
  Caption = 'Isosurfaces'
  ClientHeight = 441
  ClientWidth = 612
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 612
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 52
      Height = 13
      Caption = 'Scalar field'
    end
    object Label2: TLabel
      Left = 200
      Top = 12
      Width = 43
      Height = 13
      Caption = 'Iso value'
    end
    object CheckBox1: TCheckBox
      Left = 410
      Top = 12
      Width = 68
      Height = 17
      Caption = 'Fill/Lines'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object ComboBox1: TComboBox
      Left = 69
      Top = 8
      Width = 116
      Height = 22
      Style = csOwnerDrawFixed
      ItemIndex = 0
      TabOrder = 1
      Text = 'Sphere surface'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Sphere surface'
        'Minkowski space'
        'Klein Bottle'
        'Chmutov-surface-1'
        'Chmutov-surface-2'
        'Toroidal surface'
        'Double torus surface')
    end
    object Edit1: TEdit
      Left = 248
      Top = 8
      Width = 61
      Height = 21
      TabOrder = 2
      Text = '0.3'
    end
    object BitBtn1: TBitBtn
      Left = 320
      Top = 7
      Width = 81
      Height = 25
      Caption = 'Run MC'
      TabOrder = 3
      OnClick = BitBtn1Click
    end
    object ComboBox2: TComboBox
      Left = 484
      Top = 8
      Width = 116
      Height = 22
      Style = csOwnerDrawFixed
      ItemIndex = 0
      TabOrder = 4
      Text = 'fcBufferDefault'
      OnChange = ComboBox2Change
      Items.Strings = (
        'fcBufferDefault'
        'fcCull'
        'fcNoCull')
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 41
    Width = 612
    Height = 400
    Camera = GLCamera1
    Buffer.BackgroundColor = 10485760
    Buffer.ShadeModel = smSmooth
    FieldOfView = 50.000000000000000000
    Align = alClient
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 48
    object GLCamera1: TGLCamera
      DepthOfView = 70.000000000000000000
      FocalLength = 428.901397705078100000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000A0C000008040000040400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000C8420000A0410000A0400000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLDummyCube1: TGLDummyCube
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      PitchAngle = 90.000000000000000000
      ShowAxes = True
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      CubeSize = 1.000000000000000000
      object GLMesh1: TGLMesh
        Material.FrontProperties.Emission.Color = {C9C8483FC9C8C83EC9C8483E0000803F}
        Material.FaceCulling = fcNoCull
        Material.PolygonMode = pmLines
        Mode = mmTriangles
        VertexMode = vmVNC
      end
      object GLFreeForm1: TGLFreeForm
        Material.FrontProperties.Emission.Color = {DAD9593F8B8A0A3FEDEC6C3E0000803F}
        Material.BlendingMode = bmTransparency
      end
    end
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Isosurfaces - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 120
    Top = 48
  end
end
