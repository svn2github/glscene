glData 0.3.3 (http://gldata.sourceforge.net)
A program for displaying and editing data points from different co-ordinate system.

Created by; Aaron Hochwimmer (aaron@graphic-edge.co.nz)
See About Box for contributions

Currently Supports:

Cartesian,
Cylindrical
Spherical
Prolate Spheroidal
Oblate Spheroidal
BiPolar Cylindrical

The sample data file (glass.csv) is cartesian data of a glass.

Surfer and ArcInfo Ascii Grids may be imported and viewed. A base map texture can be optionally applied.

A sample surfer data file and grid (demogrid.dat, demogrid.grd) is provided.
A sample arcinfo data file (kapiti.grd) is provided.

Usage:
======

1. Import data via the import module.
2. Choose the co-ordinate system transformation from the combo box. 
The "a" parameter is only used for prolate-spheroidal, oblate-spheroidal, and bipolar cylindrical
3. Click process to view the data in cartesian space (x,y,z). 

Mouse Navigation - ensure "Object Selection" is unchecked
================

Left mouse button = rotate 
Shift + Left mouse button = zoom in/out
Right mouse button = pan in XY
Shift + Right mouse button = pan in XZ

Mouse Selection - ensure "Object Selection" is checked
===============

Click on a point in the Scene Viewer and the record in the "Processed" tabsheet grid will be highlighted.