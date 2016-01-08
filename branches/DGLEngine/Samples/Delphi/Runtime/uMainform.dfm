object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Runtime'
  ClientHeight = 407
  ClientWidth = 463
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DGLMaterialLibrary1: TDGLMaterialLibrary
    Materials = <
      item
        Name = 'CustomMaterial'
        Tag = 0
        BaseMaterial.Enabled = True
        BaseMaterial.Texture.Enabled = False
        BaseMaterial.Texture.TextureName = 'WallTexture'
        BaseMaterial.Texture.SamplerName = 'WallSampler'
        Multitexturing.Enabled = False
        Multitexturing.CombinerName = 'Combiner'
        Multitexturing.Texture0.Enabled = False
        Multitexturing.Texture1.Enabled = False
        Multitexturing.Texture2.Enabled = False
        Multitexturing.Texture3.Enabled = False
      end>
    ComponentsData = {
      0458434F4C02010203060B5444474C546578747572650200060B57616C6C5465
      787475726508021202000200050000000000000080FF3F050000000000000080
      FF3F050000000000000080FF3F050000000000000080FF3F0662433A5C557365
      72735C5075626C69635C446F63756D656E74735C456D62617263616465726F5C
      53747564696F5C31362E305C636F6D706F6E656E74735C676C7363656E655C53
      616D706C65735C6D656469615C6D75725F416D6269616E742E626D7008020208
      06125444474C5465787475726553616D706C65720200060B57616C6C53616D70
      6C65720802050201020102000200020002000000000000000000000000000000
      0000020002030906135444474C54657874757265436F6D62696E657202000608
      436F6D62696E6572081200000000}
    Left = 192
    Top = 136
  end
  object DGLShaderLibrary1: TDGLShaderLibrary
    Shaders = <
      item
        Name = 'LibShader'
        Tag = 0
        ShaderModel.Enabled = False
        ShaderModel.MaterialLibrary = DGLMaterialLibrary1
        ShaderModel.TessEvalShaderName = 'Shader1'
        ShaderModel.VertexShaderName = 'Shader'
      end>
    ComponentsData = {
      0458434F4C0201020206125444474C534C536861646572536372697074020006
      065368616465720C31010000696E207665633320506F736974696F6E3B0D0A69
      6E207665633420436F6C6F723B0D0A696E2076656333204E6F726D616C3B0D0A
      696E20766563332054616E67656E743B0D0A696E20766563332042696E6F726D
      616C3B0D0A696E207665633220546578436F6F7264303B0D0A696E2076656332
      20546578436F6F7264313B0D0A696E207665633220546578436F6F7264323B0D
      0A696E207665633220546578436F6F7264333B0D0A696E207665633220546578
      436F6F7264343B0D0A696E207665633220546578436F6F7264353B0D0A696E20
      7665633220546578436F6F7264363B0D0A696E207665633220546578436F6F72
      64373B0D0A696E207665633420437573746F6D303B0D0A696E20766563322043
      7573746F6D313B0D0A696E207665633220437573746F6D323B0D0A0D0A120000
      00000200020002000201020002000607536861646572310CE306000023766572
      73696F6E203333300D0A6C61796F757428747269616E676C65735F61646A6163
      656E63792920696E3B0D0A6C61796F7574286C696E655F73747269702C206D61
      785F7665727469636573203D203629206F75743B0D0A696E2076656334207632
      675F576F726C64506F735B5D3B202F2F2056657274657820706F736974696F6E
      20696E20766965772073706163650D0A756E69666F726D20766563342043616D
      657261506F736974696F6E3B0D0A0D0A2F2F2063616C63756C6174696E672066
      6163696E67206F66206120747269616E676C652072656C617469766520746F20
      6579650D0A666C6F617420666163696E6728766563342076302C207665633420
      76312C20766563342076322C2076656334206579655F706F73290D0A7B0D0A20
      20202076656333206530203D2076312E78797A202D2076302E78797A3B0D0A20
      20202076656333206531203D2076322E78797A202D2076302E78797A3B0D0A20
      2020207665633420703B0D0A20202020702E78797A203D2063726F7373286531
      2C206530293B0D0A20202020702E77203D202D646F742876302E78797A2C2070
      2E78797A293B0D0A2020202072657475726E202D646F7428702C206579655F70
      6F73293B0D0A7D0D0A0D0A2F2F206F7574707574206C696E6573206F6E207369
      6C686F756574746520656467657320627920636F6D706172696E672066616369
      6E67206F662061646A6163656E7420747269616E676C65730D0A766F6964206D
      61696E28290D0A7B0D0A20202020666C6F61742066203D20666163696E672876
      32675F576F726C64506F735B305D2C207632675F576F726C64506F735B325D2C
      207632675F576F726C64506F735B345D2C2043616D657261506F736974696F6E
      293B0D0A202020202F2F206F6E6C79206C6F6F6B2061742066726F6E74206661
      63696E6720747269616E676C65730D0A202020206966202866203E20302E3029
      0D0A202020207B0D0A2020202020202020666C6F617420663B0D0A2020202020
      2020202F2F2074657374206564676520300D0A202020202020202066203D2066
      6163696E67287632675F576F726C64506F735B305D2C207632675F576F726C64
      506F735B315D2C207632675F576F726C64506F735B325D2C2043616D65726150
      6F736974696F6E293B0D0A20202020202020206966202866203C3D2030290D0A
      20202020202020207B0D0A202020202020202020202020676C5F506F73697469
      6F6E203D20676C5F696E5B305D2E676C5F506F736974696F6E3B0D0A20202020
      2020202020202020456D697456657274657828293B0D0A202020202020202020
      202020676C5F506F736974696F6E203D20676C5F696E5B325D2E676C5F506F73
      6974696F6E3B0D0A202020202020202020202020456D69745665727465782829
      3B0D0A202020202020202020202020456E645072696D697469766528293B0D0A
      20202020202020207D0D0A0D0A20202020202020202F2F207465737420656467
      6520310D0A202020202020202066203D20666163696E67287632675F576F726C
      64506F735B325D2C207632675F576F726C64506F735B335D2C207632675F576F
      726C64506F735B345D2C2043616D657261506F736974696F6E293B0D0A202020
      20202020206966202866203C3D20302E30290D0A20202020202020207B0D0A20
      2020202020202020202020676C5F506F736974696F6E203D20676C5F696E5B32
      5D2E676C5F506F736974696F6E3B0D0A202020202020202020202020456D6974
      56657274657828293B0D0A202020202020202020202020676C5F506F73697469
      6F6E203D20676C5F696E5B345D2E676C5F506F736974696F6E3B0D0A20202020
      2020202020202020456D697456657274657828293B0D0A202020202020202020
      202020456E645072696D697469766528293B0D0A20202020202020207D0D0A0D
      0A20202020202020202F2F2074657374206564676520320D0A20202020202020
      2066203D20666163696E67287632675F576F726C64506F735B345D2C20763267
      5F576F726C64506F735B355D2C207632675F576F726C64506F735B305D2C2043
      616D657261506F736974696F6E293B0D0A20202020202020206966202866203C
      3D20302E30290D0A20202020202020207B0D0A20202020202020202020202067
      6C5F506F736974696F6E203D20676C5F696E5B345D2E676C5F506F736974696F
      6E3B0D0A202020202020202020202020456D697456657274657828293B0D0A20
      2020202020202020202020676C5F506F736974696F6E203D20676C5F696E5B30
      5D2E676C5F506F736974696F6E3B0D0A202020202020202020202020456D6974
      56657274657828293B0D0A202020202020202020202020456E645072696D6974
      69766528293B0D0A20202020202020207D0D0A202020207D0D0A7D0D0A0D0A12
      000000000202020002000201}
    Left = 200
    Top = 216
  end
end