object Form1: TForm1
  Left = 192
  Top = 107
  Width = 790
  Height = 399
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 416
    Height = 372
    Camera = GLCamera1
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 416
    Top = 0
    Width = 366
    Height = 372
    Align = alRight
    Constraints.MinHeight = 372
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 40
      Width = 40
      Height = 13
      Caption = 'DoApply'
    end
    object ShaderScript: TMemo
      Left = 8
      Top = 56
      Width = 349
      Height = 305
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        '// Renders with red flat shaded polygons and'
        '// a blue wireframe outline'
        ''
        'procedure DoApply;'
        'begin'
        '  glPushAttrib(GL_ALL_ATTRIB_BITS);'
        '  glEnable(GL_COLOR_MATERIAL);'
        '  glShadeModel(GL_FLAT);'
        '  glColor3f(1,0,0);'
        'end;'
        ''
        'function DoUnApply(Pass : Integer) : Boolean;'
        'begin'
        '  Result:=False;'
        '  case Pass of'
        ''
        '    1 : begin'
        '      glDisable(GL_LIGHTING);'
        '      glDisable(GL_COLOR_MATERIAL);'
        '      glDisable(GL_BLEND);'
        '      glDepthFunc(GL_LEQUAL);'
        '      glPolygonMode(GL_FRONT, GL_LINE);'
        '      glLineWidth(3);'
        '      glColor3f(0,0,1);'
        '      Result:=True;'
        '    end;'
        ''
        '    2 : glPopAttrib;'
        ''
        '  end;'
        'end;')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object Recompile: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Recompile'
      TabOrder = 1
      OnClick = RecompileClick
    end
    object Enabled: TCheckBox
      Left = 96
      Top = 8
      Width = 65
      Height = 17
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = EnabledClick
    end
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
        Position.Coordinates = {0000803F0000803F000000400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object GLCube1: TGLCube
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'Shader'
      Material.TextureEx = <>
    end
  end
  object dws2OpenGL1xUnit1: Tdws2OpenGL1xUnit
    Script = GLDelphiWebScriptII1
    Left = 182
    Top = 8
  end
  object GLUserShader1: TGLUserShader
    OnDoApply = GLUserShader1DoApply
    OnDoUnApply = GLUserShader1DoUnApply
    ShaderStyle = ssLowLevel
    Left = 72
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Shader'
        Material.TextureEx = <>
        Tag = 0
        Shader = GLUserShader1
      end>
    Left = 40
    Top = 8
  end
  object dws2VectorGeometryUnit1: Tdws2VectorGeometryUnit
    Script = GLDelphiWebScriptII1
    Left = 152
    Top = 8
  end
  object AsyncTimer1: TAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpHigher
    Left = 40
    Top = 40
  end
  object GLDelphiWebScriptII1: TGLDelphiWebScriptII
    Config.CompilerOptions = []
    Config.MaxDataSize = 0
    Config.Timeout = 0
    Left = 120
    Top = 8
  end
end
