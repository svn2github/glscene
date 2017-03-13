{

        GLZFightReferee Demo

        Jason Bell, with lots of help from Stuart Gooding
        August 10, 2003

        This is a demo shows how to use the GLAntiZFightShader to eliminate any
        ZBuffer fighting that is occuring.  This is when the ZBuffer can't
        determine which of two close surfaces to render and corruption results.
        To demonstrate functionality, the scene includes multiple transparent
        objects, as well as overlapping ones.

        At startup, the GLAntiZFightShaders are enabled and the dummy cubes
        that serve as parents for he cube and cylinder have depth sorting
        disabled.  This allows correct rendering.

        Disabling the shaders shows the ZBuffer fighting that they are
        concealing.

        Unchecking "Ignore Depth Buffer" while the referee is on shows the
        weirdness in object sorting that now occures when you allow GLScene to
        sort the objects.  Thus you need to pay more attention to sorting when
        using this object.

        When both boxes are unchecked you can see the ZBuffer fighting that even
        occures between different objects of this scene!

}
program AntiZFightShader;

uses
  Forms,
  GLAntiZFightShaderDemo in 'GLAntiZFightShaderDemo.pas' {Form1},
  GLAntiZFightShader in 'GLAntiZFightShader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
