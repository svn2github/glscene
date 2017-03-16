This demo is heavily based on HMTest1.DPR (webmaster@...) and the central problem of texturing
the heightfield was done by Eric Grange not me.

The demo shows several things:

Reading an Arcinfo Grid Ascii dump for the heightfield
Runtime loading of texture and applying a real-world texture to a heightmap
The basic geometry of mapping program coordinates to real-world coordinates.

Usage. The are two modes.

Fly: uses the arrow keys to move about a fixed height about the landscape. UP,DOWN moves
forward,back in direction of heading. LEFT/RIGHT rotates camera. ALT-LEFT/RIGHT moves sideways

View: Click and hold down mouse button to rotate view of the image.

The dynamic viewing depends on fixing a bug in WIP4. An updated GLTexture.pas (WIP4 only - Eric will
have fixed in later releases) is included and the actual fix is in the comments of Mainform.pas,
procedure sizebitmap.

