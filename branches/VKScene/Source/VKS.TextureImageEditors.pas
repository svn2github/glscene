//
// This unit is part of the GLScene Project   
//
{: VKS.TextureImageEditors<p>

	Standard texture image editors for standard texture image classes.<p>

	<b>History : </b><font size=-1><ul>
      <li>10/11/12 - PW - Added CPPB compatibility: used dummy method instead
                          abstract class function Edit for GLS_CPPB
      <li>22/01/10 - Yar - Added to TVKBlankImage property editor ability to set the depth
      <li>03/07/04 - LR - Make change for Linux
      <li>24/07/03 - EG - Creation
   </ul></font>
}
unit VKS.TextureImageEditors;

interface

{$i GLScene.inc}

uses
  System.Classes, System.SysUtils,

  VKS.Texture, VKS.ProcTextures,
  VKS.CrossPlatform, VKS.Utils;

type

   // TVKTextureImageEditor
   //
   TVKTextureImageEditor = class(TObject)
		public
         { Public Properties }
			{: Request to edit a textureImage.<p>
				Returns True if changes have been made.<br>
				This method may be invoked from the IDE or at run-time. }
			class function Edit(aTexImage : TVKTextureImage) : Boolean; virtual;{$IFNDEF GLS_CPPB}abstract;{$ENDIF}
   end;

   TVKTextureImageEditorClass = class of TVKTextureImageEditor;

   // TVKBlankTIE
   //
   TVKBlankTIE = class(TVKTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TVKTextureImage) : Boolean; override;
   end;

   // TVKPersistentTIE
   //
   TVKPersistentTIE = class(TVKTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TVKTextureImage) : Boolean; override;
   end;

   // TVKPicFileTIE
   //
   TVKPicFileTIE = class(TVKTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TVKTextureImage) : Boolean; override;
   end;

   // TVKProcTextureNoiseTIE
   //
   TVKProcTextureNoiseTIE = class(TVKTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TVKTextureImage) : Boolean; override;
   end;


//: Invokes the editor for the given TVKTextureImage
function EditGLTextureImage(aTexImage : TVKTextureImage) : Boolean;
procedure RegisterGLTextureImageEditor(aTexImageClass : TVKTextureImageClass;
                                       texImageEditor : TVKTextureImageEditorClass);
procedure UnRegisterGLTextureImageEditor(texImageEditor : TVKTextureImageEditorClass);


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
   vTIEClass, vTIEEditor : TList;

  // Dummy method for CPP
  //
{$IFDEF GLS_CPPB}
class function TVKTextureImageEditor.Edit(ATexImage: TVKTextureImage): Boolean;
begin
  Result := True;
end;
{$ENDIF}

// EditGLTextureImage
//
function EditGLTextureImage(aTexImage : TVKTextureImage) : Boolean;
var
   i : Integer;
   editor : TVKTextureImageEditorClass;
begin
   if Assigned(vTIEClass) then begin
      i:=vTIEClass.IndexOf(Pointer(aTexImage.ClassType));
      if i>=0 then begin
         editor:=TVKTextureImageEditorClass(vTIEEditor[i]);
         Result:=editor.Edit(aTexImage);
         Exit;
      end;
   end;
   InformationDlg(aTexImage.ClassName+': editing not supported.');
   Result:=False;
end;

// RegisterGLTextureImageEditor
//
procedure RegisterGLTextureImageEditor(aTexImageClass : TVKTextureImageClass;
                                       texImageEditor : TVKTextureImageEditorClass);
begin
   if not Assigned(vTIEClass) then begin
      vTIEClass:=TList.Create;
      vTIEEditor:=TList.Create;
   end;
   vTIEClass.Add(Pointer(aTexImageClass));
   vTIEEditor.Add(texImageEditor);
end;

// UnRegisterGLTextureImageEditor
//
procedure UnRegisterGLTextureImageEditor(texImageEditor : TVKTextureImageEditorClass);
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
// ------------------ TVKBlankTIE ------------------
// ------------------

// Edit
//
class function TVKBlankTIE.Edit(aTexImage : TVKTextureImage) : Boolean;
var
   p1, p2 : Integer;
   buf, part : String;
   texImage : TVKBlankImage;
begin
   texImage:=(aTexImage as TVKBlankImage);
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
// ------------------ TVKPersistentTIE ------------------
// ------------------

// Edit
//
class function TVKPersistentTIE.Edit(aTexImage : TVKTextureImage) : Boolean;
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
// ------------------ TVKPicFileTIE ------------------
// ------------------

// Edit
//
class function TVKPicFileTIE.Edit(aTexImage : TVKTextureImage) : Boolean;
var
	newName : String;
   texImage : TVKPicFileImage;
begin
   { TODO : A better TVKPicFileImage.Edit is needed... }
   texImage:=(aTexImage as TVKPicFileImage);
	newName:=InputDlg('PicFile Image', 'Enter filename', texImage.PictureFileName);
	Result:=(texImage.PictureFileName<>newName);
	if Result then
		texImage.PictureFileName:=newName
end;

// Edit
//
class function TVKProcTextureNoiseTIE.Edit(aTexImage : TVKTextureImage) : Boolean;
var
   p : Integer;
   buf : String;
begin
   with aTexImage as TVKProcTextureNoise do begin
      buf:=InputDlg(TVKProcTextureNoise.FriendlyName, 'Enter size', Format('%d x %d', [Width, Height]));
      p:=Pos('x', buf);
      if p>0 then begin
         Width:=StrToIntDef(Trim(Copy(buf, 1, p-1)), 256);
         Height:=StrToIntDef(Trim(Copy(buf, p+1, MaxInt)), 256);
         buf:=InputDlg(TVKProcTextureNoise.FriendlyName, 'Minimum Cut', IntToStr(MinCut));
         MinCut := StrToIntDef(buf, 0);
         buf:=InputDlg(TVKProcTextureNoise.FriendlyName, 'Noise Sharpness', FloatToStr(NoiseSharpness));
         NoiseSharpness := VKS.Utils.StrToFloatDef(buf, 0.9);
         buf:=InputDlg(TVKProcTextureNoise.FriendlyName, 'Random Seed', IntToStr(NoiseRandSeed));
         NoiseRandSeed := StrToIntDef(buf, 0);
         RandSeed := NoiseRandSeed;
         buf := InputDlg(TVKProcTextureNoise.FriendlyName, 'Generate Seamless Texture (0,1)', IntToStr(Ord(Seamless)));
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

  RegisterGLTextureImageEditor(TVKBlankImage, TVKBlankTIE);
	RegisterGLTextureImageEditor(TVKPersistentImage, TVKPersistentTIE);
	RegisterGLTextureImageEditor(TVKPicFileImage, TVKPicFileTIE);
  RegisterGLTextureImageEditor(TVKProcTextureNoise, TVKProcTextureNoiseTIE);

finalization

  UnRegisterGLTextureImageEditor(TVKBlankTIE);
	UnRegisterGLTextureImageEditor(TVKPersistentTIE);
	UnRegisterGLTextureImageEditor(TVKPicFileTIE);
  UnRegisterGLTextureImageEditor(TVKProcTextureNoiseTIE);

  FreeAndNil(vTIEClass);
  FreeAndNil(vTIEEditor);

end.

