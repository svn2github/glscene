Archipelago GLScene demo v1.1 (http://glscene.org)


>>>>>> BEFORE starting the demo: <<<<<<

  run "splitter.exe" in the data subdirectory, to cut the 
  big .jpg into many .bmp (textures). Uses 48MB of disk space.

---------------------------------------

This demo illustrates the terrain rendering as well as use
of several other components for custom rendering needs.
The terrain rendered is based on a 512x512 heightmap which
is dynamically tesselated (by a ROAM-like algorithm), and
a 4096x4096 texture map split into 16 1024x1024 tiles.

Graphics board memory requirements:
- 128+ MB: no issues, you can even comment out the texture
  compression request in the code for reduced loading times
- 64 MB: no issues, as long as texture compression is on
- 32 MB: with the default textures, you'll end up with AGP
  texturing, base textures tiles should be downsampled to 512x512
- 16 MB or less : err... downsample the textures a lot... ;)

You can easily downsample all the textures with tools like IrfanView
and its "Batch" conversion functions.

When the program is running, you can access a mini-help via 'F1'.
Movements and camera orientation are controled by arrow keys and
the mouse respectively, camera altitude is controled with 
pageup/pagedown. Exit with the ESC key.

Approximative framerates at startup camera angle & position:
- Athlon XP 1800+ / GF4 Ti 4200: >230 FPS
- Athlon XP 1800+ / GF2 Pro: >140 FPS

Resources kindly contributed by:

- Terrain elevation map and textures : Mattias Fagerlund
  (http://www.cambrianlabs.com/Mattias/)
- Sailboat model and textures : Daniel Polli
  (Daniel@dansteph.com, http://virtualsailor.dansteph.com)
- Water and detail textures from Lemog's 3D Textures
  (http://www.3dtextures.fr.st/)

Please, do not use/reuse the resources without prior consent 
of their owners, only the code is under MPL!

Changes to v1.1:
- added test to warn users that forgot to run "splitter.exe"
- texture filtering CLAMP_TO_EDGE to fix seams on ATI hardware
- reversed sailboat texture name (was mirrored horizontally)
- added warning for graphics boards memory requirements

Eric Grange 
http://glscene.org