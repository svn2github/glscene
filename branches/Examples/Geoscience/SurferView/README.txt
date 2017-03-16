SurferView Demo
Opening and Viewing Surfer (http://www.goldensoftware.com/products/surfer/surfer.shtml) Grid Files.
Aaron Hochwimmer (hochwimmera@pbworld.com)

Built using GLScene 0.9rc2 (http://www.glscene.org) on Delphi 5 Pro

Main parsing functions are in "gridimportfn". 
Reads in any one of Surfers grid file format (Surfer7, GS Binary, GS Ascii) and 
parses the data into an array of floats.

Some gravity subsidence data is provided in the "data" directory. This comprises a csv file (gravity.csv) 
along with the grid (using Kriging) in each of the 3 grid formats.

The file formats are outlined in gridimportfn.pas.
This is based in part on the "karapiti" demo by Phil Scadden (p.scadden@gns.cri.nz)

Displays using a TGLHeightField.

NB: Once opening a grid you may need to use the mouse (left click and move) to correct the viewing angle. 