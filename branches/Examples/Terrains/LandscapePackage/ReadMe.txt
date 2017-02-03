  This package contains four program demonstrating the use of the ahGLRandomHDS unit. 
  They are located into numbered folders and, for a faster learning curve, 
  you should study them in that order.

  The ahGLRandomHDS.pas unit is used by all three programs. 
  You MUST move it in a folder where Delphi will find it, 
  either in the GLScene Source folder (recommanded), or directly in the folder of the program.

01-LandscapeTutorial
********************
  This demo shows what are the minimal requirements to use a tGLFractalHDS. 
  It also shows how to set-up GLScene to get everything working quickly. 
  This demo does nothing else than to display a cyclic fractal landscape using 
  the built-in land-cover event handler (no external textures needed). 
  This demo could be used as a base for any program using this object.

02-FractalLandscape
*******************
  This program demonstrates almost all the properties of tGLFractalHDS. 
  You can play with them through an user-friendly interface. 
  This will allow you to get quickly an intuitive understanding of what 
  each property is controlling.

03-DuneFighter
***************
  This simple demo is based on the ActorTwoCams demo. 
  It simply shows how a cyclic tGLFractalHDS can be used in an actor-based game.

04-FractalArchipelago
*********************
  Here I demonstrate how tGLFractalHDS can be generated on the fly 
  by using a tGLFractalArchipelago component. 
  You should better get a good understanding of the tGLFractalHDS 
  before plungeing into this one.
