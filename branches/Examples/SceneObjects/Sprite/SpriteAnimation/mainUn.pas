unit mainUn;
{Here are some more tips:
    - If you want a smoother transition between 2 frames, you could use blur.
      There are blur components but you could also make a somewhat simpler blur,
      render the previous frame as well but then almost transparent.
    - When using linear or mipmap filters for min/mag filter, you might see the colors
      used around the sprite. For example, if the background color is pink in your sprite
      images the sprites might have a pink edge around them. You can avoid this by using
      'nearest' for min/mag. If that's not nice enough (the edges might get too 'blocky'),
      you can also use images with a alpha channel inside. The bitmap, tga or whatever
      format has a fourth channel (rgb and a) which you can use to draw the transparency.
      Its more accurate and gives a nice effect, but your images will be bigger of course.
    - When performing the punch move in the demo, you'll see that the poor guy gets
      thinner when his fist goes forward. This is because the plane keeps the same
      width while the image requires more space. You could solve this by making the
      plane wider. You could make a 'width' property for each frame.

    }
interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  GLCadencer, GLTexture, ExtCtrls, GLWin32Viewer, GLScene,
  GLObjects,
  StdCtrls, GLMaterial, GLCoordinates, GLCrossPlatform,
  GLRenderContextInfo,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Panel1: TPanel;
    matLib: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCadencer1: TGLCadencer;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Button1Click(Sender: TObject);
  private
     
  public
     
  end;


      { A set of frames. Stuff like 'walk', 'stand' or 'die'.
        This animation is pretty simple, the interval between each frame is the same
        and you can't do much other stuff with it. But of course, you can add
        some more details. For example an array with the interval per frame instead
        of 1 value for the entire animation.
      }
      TAnimation        = record
          speed         : single;                                               // Interval between the frames
          frameCount    : integer;                                              // Amount of frames
          nextAnimation : integer;                                              // Index to the animation that follows on this one,
                                                                                // if it points to itself, we have a looped animation

          material      : integer;                                              // Index to the material library
      end; // TAnimation



      TAnimation_Data   = record
          animations    : array of TAnimation;
      end; // TAnimation_Data



      { Custom sprite class. I use a TGLplane but you could also use TGLsprite or
        TGLHUDSprite. Keep in mind that HUD sprites are always on top and use
        different coordinates (x,y are in pixels, not '3D-world' coordinates! }
      TMySprite         = class (TGLPlane)
        private
          anim_Index      : integer;                                            // Current animation, index for the animations array
          anim_Pos        : integer;                                            // Current frame
          anim_Delta      : single;                                             // Elapsed time since a frame got switched
        public
          { Animation }
          animation_Data  : TAnimation_Data;

          constructor create;
          procedure   setAnimation( const index : integer );
          procedure   render( var rci : TGLRenderContextInfo );
      end; // TMySprite



const
    ANIM_STAND    = 0;
    ANIM_PUNCH    = 1;

var
  Form1   : TForm1;

  animations  : TAnimation_Data;                                                // The frames/data for sprite1 and 2

  sprite1,
  sprite2     : TMySprite;

implementation

{$R *.dfm}



      procedure TForm1.FormCreate(Sender: TObject);

            procedure addTransparentTexture( const name, fname : string );
            begin
                  with matlib.AddTextureMaterial( name, fname ) do begin
                      material.Texture.TextureMode   := tmModulate;
                      material.Texture.TextureWrap   := twNone;
                      material.Texture.ImageAlpha    := tiaTopLeftPointColorTransparent;
                      material.BlendingMode          := bmTransparency;
                  end;
            end; // addTransparentTexture

      begin
            { Add sprite textures, we have a stand and punch animation }
            addTransparentTexture( 'stand', 'anim_Stand.bmp' );
            addTransparentTexture( 'punch', 'anim_Punch.bmp' );

            { Create a set of animations
            { Let both sprites share the same animation set.
              We do it hard-coded here, but you could write a nice unit
              that loads packages with sprite info of course. }
            setLength( animations.animations, 2 );
                { 1, stand animation, a looped animation }
                animations.animations[0].speed          := 0.1;
                animations.animations[0].frameCount     := 3;
                animations.animations[0].nextAnimation  := 0;                   // 0 is the index of the 'stand' animation
                animations.animations[0].material       := matLib.Materials.GetLibMaterialByName( 'stand' ).Index;
                { 2, punch animation, after the animation, switch back to stand }
                animations.animations[1].speed          := 0.2;
                animations.animations[1].frameCount     := 4;
                animations.animations[1].nextAnimation  := 0;                   // 0 is the index of the 'stand' animation
                animations.animations[1].material       := matLib.Materials.GetLibMaterialByName( 'punch' ).Index;

                
            { Create 2 sprites and place them somewhere }
            sprite1                 := TMySprite.create;
            sprite1.animation_Data  := animations;
            sprite1.Position.X      := -1;
            sprite1.Position.Z      := -5;
            sprite1.Width           := 1;
            sprite1.Height          := 2.5;

            sprite2 := TMySprite.create;
            sprite2.animation_Data  := animations;
            sprite2.Position.X      := +1;
            sprite2.Position.Z      := -5;
            sprite2.Width           := 2;
            sprite2.Height          := 5;
            { Let sprite 2 look the opposite way. The most effective way might be
              by reversing the texture coordinates but as we don't have access to
              them we'll do it by setting the scale.x on -1.
              !! Face culling must be off when rendering reversed sprites,
                 otherwise you won't see them! }
            sprite2.Scale.X         := -1;
            
      end; // formCreate



      procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
          var rci: TGLRenderContextInfo);
      begin
            { Render via a directOpenGL (so don't insert the planes
              into the GLScene object list. You don't have to use
              this approach but I prefer it as you have more control over
              the rendering process. But like I said, you could also insert
              the sprites simply in a TGLScene objects list and let him do the job.
              Bind your sprite to a MaterialLibrary via the material.libmaterialName
              property. You still need to use a 'beforeRender' event for each sprite
              as you need to modify the texture coordinates first, otherwise you can't
              share the same texture for multiple sprites with different frame positions. }
            sprite1.render( rci );
            sprite2.render( rci );
      end;



      procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
          newTime: Double);
      begin
            self.GLSceneViewer1.Refresh;
      end;



      procedure TForm1.Button1Click(Sender: TObject);
      begin
            { Punch animation }
            if sender = button1 then sprite1.setAnimation( ANIM_PUNCH ) else
                                     sprite2.setAnimation( ANIM_PUNCH );
      end; // button click




{ TMySprite }

      constructor TMySprite.create;
      begin
            inherited create( nil );

            { Reset animation stuff }
            anim_Pos   := 0;
            anim_Index := 0;
            anim_Delta := 0;
      end; // TMySprite Create



      procedure TMySprite.setAnimation(const index: integer);
      begin
            anim_Index := index;
            anim_Pos   := 0;
            anim_Delta := 0;
      end; // setAnimation



      procedure TMySprite.render(var rci: TGLRenderContextInfo);
      var fw : single;
      begin
            { Notice that if the overall framerate differs, the animation
              speed of the sprites will differ as well. Iy you want to solve
              this, take a look at the delta-time of a GLCadencer or something }
            with animation_Data.animations[ anim_Index ] do begin
                anim_Delta := anim_Delta + speed;
                if anim_Delta >=1 then begin
                    { Next frame }
                    inc( anim_Pos );
                    { If we're at the end of the current animation, switch to the next }
                    if anim_Pos = frameCount then
                        setAnimation( nextAnimation );
                    anim_Delta := 0;
                end;
            end;

            { Modify the material texcoords to our current frame }
            with animation_Data.animations[ anim_Index ] do begin
                fw := 1 / frameCount;
                
                form1.matLib.Materials[ material ].TextureScale.X  := fw;       // fit to the frame size
                form1.matLib.Materials[ material ].TextureOffset.X := fw * anim_Pos;// pick the current frame

                { Now render the plane with the modified material }
                form1.matLib.Materials[ material ].Apply( rci );
                inherited render( rci );
                form1.matLib.Materials[ material ].UnApply( rci )
            end;
      end; // render





end.
