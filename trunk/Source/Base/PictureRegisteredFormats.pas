// PictureRegisteredFormats
{: Egg<p>

   Hacks into the VCL to access the list of TPicture registered TGraphic formats<p>

   <b>History : </b><font size=-1><ul>
      <li>24/02/05 - Egg - Creation
   </ul></font>
}
unit PictureRegisteredFormats;

interface

uses Classes, Graphics;

{$ifdef VER140} {$define PRF_HACK_PASSES} {$endif} // Delphi 6
{$ifdef VER150} {$define PRF_HACK_PASSES} {$endif} // Delphi 7

{$ifndef PRF_HACK_PASSES} Error: hack not tested for this Delphi version! {$endif}

{: Adds to the passed TStrings the list of registered formats.<p>
   Convention is "extension=description" for the string, the Objects hold
   the corresponding TGraphicClass (extensions do not include the '.'). }
procedure HackTPictureRegisteredFormats(destList : TStrings);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   PFileFormat = ^TFileFormat;
   TFileFormat = record
      GraphicClass: TGraphicClass;
      Extension: string;
      Description: string;
      DescResID: Integer;
   end;

// HackTPictureRegisteredFormats
//
procedure HackTPictureRegisteredFormats(destList : TStrings);
var
   pCallGetFileFormat, pGetFileFormats, pFileFormats : PChar;
   iCall : Integer;
   i : Integer;
   list : TList;
   fileFormat : PFileFormat;
begin
   pCallGetFileFormat:=@PChar(@TPicture.RegisterFileFormat)[16];
   iCall:=PInteger(pCallGetFileFormat)^;
   pGetFileFormats:=@pCallGetFileFormat[iCall+4];
   pFileFormats:=PChar(PInteger(@pGetFileFormats[2])^);
   list:=TList(PInteger(pFileFormats)^);
   if list<>nil then begin
      for i:=0 to list.Count-1 do begin
         fileFormat:=PFileFormat(list[i]);
         destList.AddObject(fileFormat.Extension+'='+fileFormat.Description, TObject(fileFormat.GraphicClass));
      end;
   end;
end;

end.
 