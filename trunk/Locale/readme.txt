The directory for GLScene's po files with translations of captions, text strings and hints 
from english to other languages.

This po files were created and could be edited and used in the next way: 
- Download dxgettext-1.2.2.exe from http://sourceforge.net/projects/dxgettext/ 
  and install GNU Gettext tools for Delphi, C++ and Lazarus. 
  For Windows there will be corresponding items inserted to Explorer's menu. 
- Extract captions, text strings and hints to be translated in your project's directory 
  from pas/dfm files to a template default.po file. Right click on appropriate folder opended in Explorer.
- Merge your project's templete default.po file with main translated samples.po file, 
  earlier extracted from GLScene\Samples directory (the same suitable for glscene.po updated by GLScene's developers).
- Download poEdit editor from http://www.poedit/net/ or http://sourceforge.net/projects/poedit/files/
  and make necessary traslations of your additional text strings. 
- Save your po file from poEdit and thus get compiled mo file. You will have one mo file for each language.
- Download gnugettext.pas unit from svn page:
  https://dxgettext.svn.sourceforge.net/svnroot/dxgettext/trunk/dxgettext-pachages/dxgettext/dxgettext/freepascal/
- Include gnugettext.pas into your project's uses clause, set paths to mo files in your program 
  and make language code selections.
- Choose a necessary language while running your application and get localised version of your software interface.

05.01.13
--------
GLS Team





