********************************************************************************************************
* Please mind that MX2 importer/Alias:Maya plug-in is free for non-commercial use only.                *
*                                                                                                      *
* Include in your project and documentation note that you use this mesh importer and a link to authors *
* addresses.                                                                                           *
*                                                                                                      *
* For commercial use ask for permision and price the owners at:                                        *
* rd@evrocom.net or stefan.bazelkov@gmail.com                                                          *
********************************************************************************************************

Export/Import capabilities:
-------------------------------------------------------------------------------------------------------------------
* Material properties (Ambient, Diffuse ....)
* Material textures (File name)
* Mesh Animation

You can use any aviable Alias:Maya mesh modifier like Cloth for example.

Installation:
GLScene MX2 mesh importer:
-------------------------------------------------------------------------------------------------------------------
1. copy GLFileMX2.pas into $GLSCENE_BASE_DIR\Source\FileFormats\ folder.
2. to use it add GLFileMX2 into your project Uses clause.
3. use it like any other mesh importer in GLScene.


Alias:Maya 6.0/7.0 plug-in and MEL script install
-------------------------------------------------------------------------------------------------------------------
1. copy Maya_plugins\mx2_export-6.0.mll (or mx2_export-7.0.mll, depending of what Maya version is installed) into
$MAYA_HOME_DIR\bin\plug-ins\ folder.
2. copy Maya_plugins\mx2ExportOptions.mel into $MAYA_HOME_DIR\scripts\others\ folder.
3. start Maya
4. go to  menu -> 'Window'/'Settings/Preferences'/'Plug-in Manager'.
5. find mx2_export-6.0 or mx2_export-7.0 and check 'loaded' and 'auto load' checkboxes.
6. close 'Plug-in Manager' and you are ready.
7. when export animation make sure to set 'Playback End Time' in Maya animation line.

MX2a is in ASCII format.
MX2b is binary format of the same file.

NOTE:
 Before export make sure to delete history of exported object or scene !!!!
 menu -> 'Edit'/'Delete All by type'/'History'

 All exported and textured objects have absolute texture path set !!!
 To use diferent texture path set GLMaterialLibrary.TexturePaths property.
