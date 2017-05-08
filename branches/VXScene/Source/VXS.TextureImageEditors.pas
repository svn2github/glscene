//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Standard texture image editors for standard texture image classes. 
}
unit VXS.TextureImageEditors;

interface

{$I VXScene.inc}

uses
  System.Classes, System.SysUtils,

  VXS.Texture, VXS.ProcTextures,
  VXS.CrossPlatform, VXS.Utils;

type

   // TVXTextureImageEditor
   //
   TVXTextureImageEditor = class(TObject)
		public
         { Public Properties }
			{ Request to edit a textureImage. 
				Returns True if changes have been made. 
				This method may be invoked from the IDE or at run-time. }
			class function Edit(aTexImage : TVXTextureImage) : Boolean; virtual; abstract;
   end;

   TVXTextureImageEditorClass = class of TVXTextureImageEditor;

   // TVXBlankTIE
   //
   TVXBlankTIE = class(TVXTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TVXTextureImage) : Boolean; override;
   end;

   // TVXPersistentTIE
   //
   TVXPersistentTIE = class(TVXTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TVXTextureImage) : Boolean; override;
   end;

   // TVXPicFileTIE
   //
   TVXPicFileTIE = class(TVXTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TVXTextureImage) : Boolean; override;
   end;

   // TVXProcTextureNoiseTIE
   //
   TVXProcTextureNoiseTIE = class(TVXTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TVXTextureImage) : Boolean; override;
   end;


// Invokes the editor for the given TVXTextureImage
function EditTextureImage(aTexImage : TVXTextureImage) : Boolean;
procedure RegisterTextureImageEditor(aTexImageClass : TVXTextureImageClass;
                                       texImageEditor : TVXTextureImageEditorClass);
procedure UnRegisterTextureImageEditor(texImageEditor : TVXTextureImageEditorClass);


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
   vTIEClass, vTIEEditor : TList;

// EditTextureImage
//
function EditTextureImage(aTexImage : TVXTextureImage) : Boolean;
var
   i : Integer;
   editor : TVXTextureImageEditorClass;
begin
   if Assigned(vTIEClass) then begin
      i:=vTIEClass.IndexOf(Pointer(aTexImage.ClassType));
      if i>=0 then begin
         editor:=TVXTextureImageEditorClass(vTIEEditor[i]);
         Result:=editor.Edit(aTexImage);
         Exit;
      end;
   end;
   InformationDlg(aTexImage.ClassName+': editing not supported.');
   Result:=False;
end;

// RegisterTextureImageEditor
//
procedure RegisterTextureImageEditor(aTexImageClass : TVXTextureImageClass;
                                       texImageEditor : TVXTextureImageEditorClass);
begin
   if not Assigned(vTIEClass) then begin
      vTIEClass:=TList.Create;
      vTIEEditor:=TList.Create;
   end;
   vTIEClass.Add(Pointer(aTexImageClass));
   vTIEEditor.Add(texImageEditor);
end;

// UnRegisterTextureImageEditor
//
procedure UnRegisterTextureImageEditor(texImageEditor : TVXTextureImageEditorClass);
var
   i : Integer;
begin
   if Assigned(vTIEClass) then begin
      i:=vTIEEditor.IndexOf(texImageEditor);
      if i>=0 then begin
         vTIEClass.Delete(i);
         vTIEEditor.Delete(i);
      end;
   end;
end;

// ------------------
// ------------------ TVXBlankTIE ------------------
// ------------------

// Edit
//
class function TVXBlankTIE.Edit(aTexImage : TVXTextureImage) : Boolean;
var
   p1, p2 : Integer;
   buf, part : String;
   texImage : TVXBlankImage;
begin
   texImage:=(aTexImage as TVXBlankImage);
   if texImage.Depth=0 then
     buf:=InputDlg('Blank Image', 'Enter size',
      Format('%d x %d', [texImage.Width, texImage.Height]))
   else
     buf:=InputDlg('Blank Image', 'Enter size',
      Format('%d x %d x %d', [texImage.Width, texImage.Height, texImage.Depth]));

   p1:=Pos('x', buf);
   if p1>0 then begin
      texImage.Width:=StrToIntDef(Trim(Copy(buf, 1, p1-1)), 256);
      part := Copy(buf, p1+1, MaxInt);
      p2:=Pos('x', part);
      if p2>0 then begin
        texImage.Height:=StrToIntDef(Trim(Copy(part, 1, p2-1)), 256);
        texImage.Depth:=StrToIntDef(Trim(Copy(part, p2+1, MaxInt)), 1)
      end
      else begin
        texImage.Height:=StrToIntDef(Trim(Copy(buf, p1+1, MaxInt)), 256);
        texImage.Depth:=0;
      end;
      Result:=True;
   end else begin
      InformationDlg('Invalid size');
      Result:=False;
   end;
end;

// ------------------
// ------------------ TVXPersistentTIE ------------------
// ------------------

// Edit
//
class function TVXPersistentTIE.Edit(aTexImage : TVXTextureImage) : Boolean;
var
   fName : String;
begin
   fName:='';
   Result:=OpenPictureDialog(fName);
   if Result then begin
   	aTexImage.LoadFromFile(fName);
		aTexImage.NotifyChange(aTexImage);
	end;
end;

// ------------------
// ------------------ TVXPicFileTIE ------------------
// ------------------

// Edit
//
class function TVXPicFileTIE.Edit(aTexImage : TVXTextureImage) : Boolean;
var
	newName : String;
   texImage : TVXPicFileImage;
begin
   { TODO : A better TVXPicFileImage.Edit is needed... }
   texImage:=(aTexImage as TVXPicFileImage);
	newName:=InputDlg('PicFile Image', 'Enter filename', texImage.PictureFileName);
	Result:=(texImage.PictureFileName<>newName);
	if Result then
		texImage.PictureFileName:=newName
end;

// Edit
//
class function TVXProcTextureNoiseTIE.Edit(aTexImage : TVXTextureImage) : Boolean;
var
   p : Integer;
   buf : String;
begin
   with aTexImage as TVXProcTextureNoise do begin
      buf:=InputDlg(TVXProcTextureNoise.FriendlyName, 'Enter size', Format('%d x %d', [Width, Height]));
      p:=Pos('x', buf);
      if p>0 then begin
         Width:=StrToIntDef(Trim(Copy(buf, 1, p-1)), 256);
         Height:=StrToIntDef(Trim(Copy(buf, p+1, MaxInt)), 256);
         buf:=InputDlg(TVXProcTextureNoise.FriendlyName, 'Minimum Cut', IntToStr(MinCut));
         MinCut := StrToIntDef(buf, 0);
         buf:=InputDlg(TVXProcTextureNoise.FriendlyName, 'Noise Sharpness', FloatToStr(NoiseSharpness));
         NoiseSharpness := VXS.Utils.StrToFloatDef(buf, 0.9);
         buf:=InputDlg(TVXProcTextureNoise.FriendlyName, 'Random Seed', IntToStr(NoiseRandSeed));
         NoiseRandSeed := StrToIntDef(buf, 0);
         RandSeed := NoiseRandSeed;
         buf := InputDlg(TVXProcTextureNoise.FriendlyName, 'Generate Seamless Texture (0,1)', IntToStr(Ord(Seamless)));
         Seamless := (buf<>'0');
         Result:=True;
         Invalidate;
      end else begin
         InformationDlg('Invalid size');
         Result:=False;
      end;
   end;
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterTextureImageEditor(TVXBlankImage, TVXBlankTIE);
	RegisterTextureImageEditor(TVXPersistentImage, TVXPersistentTIE);
	RegisterTextureImageEditor(TVXPicFileImage, TVXPicFileTIE);
  RegisterTextureImageEditor(TVXProcTextureNoise, TVXProcTextureNoiseTIE);

finalization

  UnRegisterTextureImageEditor(TVXBlankTIE);
	UnRegisterTextureImageEditor(TVXPersistentTIE);
	UnRegisterTextureImageEditor(TVXPicFileTIE);
  UnRegisterTextureImageEditor(TVXProcTextureNoiseTIE);

  FreeAndNil(vTIEClass);
  FreeAndNil(vTIEEditor);

end.

