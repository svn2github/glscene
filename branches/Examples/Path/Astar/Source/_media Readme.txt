landscaper : 256x256
painter 256x256

Pathfinder uses the names Tilesize (Data) and Image size 
The Image size / tilesize = Number of Tiles that are checked for a path

///////////////////////
Terrain demo 256x256 terrain.bmp Texture snow512.jpg [512x512]
256x256 height samples ...512x512 Texture Tilesize 32per 1
Generally the texture is 2,4,8 times the number of Elevation points

If the Elevation data is 256x256 and desired 'Rez' is 4 points, then there should be 64 tiles. 
Given a Texture 'Rez' of 4, then the Pathfinder Image size would be 1024.. with a Pathfinder tilesize of 16.
The Terrain data files would be 64x64..No need to be same as Elevation. 
(The Terrain data can be 'interpolated' to form the Image)

/////////////////////////////////
Archipelago : 512x512 heightmap
16 (4x4) 512x512 Texture images (Medium Rez)

PathFinder : 2048 Image, Tilesize 16 : 128 tiles (Terrain data 128x128)

////////////////////
FOREST  1024x1024 heightmap Texture 1024x1024..split into more... =4096 x4096 

PathFinder : 4096 Image, Tilesize 16 : 256 tiles (Terrain data 256x256)


//////////////////////
/H Racer
256 bmp for dem, 256 for all terrain type files



///Terrain demo
1024x1024 height samples, and with tiles of
   size 16 or less, this is a lot of tiles to prepare
snow512 our heightmap size is 256)

FOREST///////////

   cMapWidth : Integer = 1024;
   cMapHeight : Integer = 1024;
   cBaseSpeed : Single = 50;
//Tilesize 32
//Tilesper texture 32
//Trees 512x512
//dem: 1024x1024        low rez tex 1024x1024...SPLIT into more
////////////////////////////////////////

Archipelago GLScene demo v1.01 (http://glscene.org)

>>>>>> BEFORE starting the demo: <<<<<<

  "splitter.exe" from the data subdirectory will be run, 
  to cut the big .jpg into many .bmp (textures). 
  Uses 48MB of disk space for full-resolution textures
  (recommended if you have a board with 64+ MB of memory).
  You can re-run it later to generate higher/lower resolution
  textures.

---------------------------------------

This demo illustrates the terrain rendering as well as use
of several other components for custom rendering needs.
The terrain rendered is based on a 512x512 heightmap which
is dynamically tesselated (by a ROAM-like algorithm), and
a 4096x4096 texture map split into 16 1024x1024 tiles
(at medium resolution, that's 512x512 tiles, and at low
resolution, 256x256 tiles).

Graphics board memory requirements:
- 128+ MB: no issues, you can even comment out the texture
  compression request in the code for reduced loading times.
- 64 MB: no issues, as long as texture compression is on.
- 32 MB: use medium resolution option, may require to use
  low resolution to achieve decent performance.
- 16 MB or less : use low resolution.

When the program is running, you can access a mini-help via 'F1'.
Movements and camera orientation are controled by arrow keys and
the mouse respectively, camera altitude is controled with 
pageup/pagedown. Exit with the ESC key.

Approximative framerates at startup camera angle & position:
- Athlon XP 1800+ / GF4 Ti 4200: >210 FPS
- Athlon XP 1800+ / GF2 Pro: >140 FPS

Credits and kindly contributed resources by:

- Terrain elevation map and textures : Mattias Fagerlund
  (http://www.cambrianlabs.com/Mattias/), heightmap and sedimentation
  map created by World Machine from Stephen Schmitt
  (http://students.washington.edu/sschmitt/world/)
- Sailboat model and textures : Daniel Polli
  (Daniel@dansteph.com, http://virtualsailor.dansteph.com)
- Water and detail textures from Lemog's 3D Textures
  (http://www.3dtextures.fr.st/)
- Inno Setup was used for the installer
  (http://www.jrsoftware.org/isinfo.php)

Please, do not use/reuse the resources without prior consent 
of their respective owners, only the code is under MPL!

Changes to v1.01:
- added test to warn users that forgot to run "splitter.exe"
- texture filtering CLAMP_TO_EDGE to fix seams on ATI hardware
- reversed sailboat texture name (was mirrored horizontally)
- added warning for graphics boards memory requirements
- splitter.exe includes basic (low quality) resampling support

Eric Grange 
http://glscene.org