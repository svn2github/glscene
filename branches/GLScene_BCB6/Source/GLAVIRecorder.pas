// GLAVIRecorder
{: Component to make it easy to record GLScene frames into an AVI file<p>

	<b>History : </b><font size=-1><ul>
      <li>13/05/04 - EG - Added irmBitBlt mode (now the default mode)
      <li>05/01/04 - EG - Added Recording function and ability to record arbitrary bitmap,
                          Added OnPostProcessEvent
      <li>08/07/03 - NelC - Fixed access violation on exit (thx Solerman Kaplon)
                            and minor updates
      <li>11/12/01 - EG - Minor changes for compatibility with JEDI VfW.pas
      <li<02/03/01 - EG - Added TAVIImageRetrievalMode
      <li>24/02/01 - NelC - Creation and initial code
	</ul></font>
}
unit GLAVIRecorder;

interface

{$i GLScene.inc}
{$IFDEF LINUX}{$Message Error 'Unit not supported'}{$ENDIF LINUX}

uses Windows, Classes, Controls, Forms, Extctrls, Graphics, vfw, GLScene,
   GLWin32Viewer;

type
   TAVICompressor = (acDefault, acShowDialog);

   PAVIStream = ^IAVIStream;

   TAVISizeRestriction = ( srNoRestriction, srForceBlock2x2,
                           srForceBlock4x4, srForceBlock8x8);

   TAVIRecorderState = (rsNone, rsRecording);

   // TAVIImageRetrievalMode
   //
   {: Image retrieval mode for frame capture.<p>
      Following modes are supported:<p>
      <li>irmSnapShot : retrieve OpenGL framebuffer content using glReadPixels
      <li>irmRenderToBitmap : renders the whole scene to a bitmap, this is
         the slowest mode, but it won't be affected by driver-side specifics.
      <li>irmBitBlt : tranfers the framebuffer using the BitBlt function,
         usually the fastest solution
      </ul> }
   TAVIImageRetrievalMode = (irmSnapShot, irmRenderToBitmap, irmBitBlt);

   TAVIRecorderPostProcessEvent = procedure (Sender : TObject; frame : TBitmap) of object;

   // TAVIRecorder
   //
   {: Component to make it easy to record GLScene frames into an AVI file. }
   TAVIRecorder = class (TComponent)
     private
       { Private Declarations }
       AVIBitmap : TBitmap;
       AVIFrameIndex : integer;

       AVI_DPI : integer;

       asi   : TAVIStreamInfoA;

       pfile : IAVIFile;
       Stream, Stream_c : IAVIStream; // AVI stream and stream to be compressed

       BitmapInfo : PBitmapInfoHeader;
       BitmapBits : Pointer;
       BitmapSize : Dword;

       TempName : String; // so that we know the filename to delete case of user abort

       FAVIFilename : string;
       FFPS: byte;
       FWidth : integer;
       FHeight : integer;
       FSizeRestriction : TAVISizeRestriction;
       FImageRetrievalMode : TAVIImageRetrievalMode;
       RecorderState : TAVIRecorderState;
       FOnPostProcessEvent : TAVIRecorderPostProcessEvent;

       procedure SetHeight(const val : integer);
       procedure SetWidth(const val : integer);
       procedure SetSizeRestriction(const val : TAVISizeRestriction);

     protected
       { Protected Declarations }
       // Now, TAVIRecorder is tailored for GLScene. Maybe we should make a generic
       // TAVIRecorder, and then sub-class it to use a GLSceneViewer
       FGLSceneViewer : TGLSceneViewer;
       // FCompressor determines if the user is to choose a compressor via a dialog box, or
       // just use a default compressor without showing a dialog box.
       FCompressor : TAVICompressor;
       // some video compressor assumes input dimensions to be multiple of 2, 4 or 8.
       // Restricted() is for rounding off the width and height.
       // Currently I can't find a simple way to know which compressor imposes
       // what resiction, so the SizeRestiction property is there for the user to set.
       // The source code of VirtualDub (http://www.virtualdub.org/)
       // may give us some cues on this.
       // ( BTW, VirtualDub is an excellent freeware for editing your AVI. For
       //   converting AVI into MPG, try AVI2MPG1 - http://www.mnsi.net/~jschlic1 )
       function Restricted(s:integer):integer;

       procedure InternalAddAVIFrame;

     public
       { Public Declarations }
       constructor Create(AOwner : TComponent); override;
       destructor Destroy; override;

       function CreateAVIFile(DPI : integer = 0) : boolean;
       procedure AddAVIFrame; overload;
       procedure AddAVIFrame(bmp : TBitmap); overload;
       procedure CloseAVIFile(UserAbort : boolean = false);
       function Recording : Boolean;

     published
       { Published Declarations }
       property FPS : byte read FFPS write FFPS default 25;
       property GLSceneViewer : TGLSceneViewer read FGLSceneViewer write FGLSceneViewer;
       property Width : integer read FWidth write SetWidth;
       property Height : integer read FHeight write SetHeight;
       property Filename : String read FAVIFilename write FAVIFilename;
       property Compressor : TAVICompressor read FCompressor write FCompressor default acDefault;
       property SizeRestriction : TAVISizeRestriction read FSizeRestriction write SetSizeRestriction default srForceBlock8x8;
       property ImageRetrievalMode : TAVIImageRetrievalMode read FImageRetrievalMode write FImageRetrievalMode default irmBitBlt;

       property OnPostProcessEvent : TAVIRecorderPostProcessEvent read FOnPostProcessEvent write FOnPostProcessEvent;

     end;

procedure Register;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses SysUtils, Dialogs, Messages, GLGraphics;

procedure Register;
begin
	RegisterComponents('GLScene Utils', [TAVIRecorder]);
end;

// DIB support rountines for AVI output

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader);
var
  BM: Windows.TBitmap;
begin
  GetObject(Bitmap, SizeOf(BM), @BM);
  with BI do
  begin
    biSize := SizeOf(BI);
    biWidth := BM.bmWidth;
    biHeight := BM.bmHeight;
    biPlanes := 1;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed := 0;
    biClrImportant := 0;
    biCompression := BI_RGB;
    biBitCount := 24; // force 24 bits. Most video compressors would deal with 24-bit frames only.
    biSizeImage := (((biWidth * biBitCount) + 31) div 32) * 4 * biHeight;
  end;
end;

procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer; var ImageSize: DWORD);
var
  BI: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, BI);
  with BI do
    InfoHeaderSize:=SizeOf(TBitmapInfoHeader);
  ImageSize:=BI.biSizeImage;
end;

function InternalGetDIB(Bitmap: HBITMAP; var BitmapInfo; var Bits): Boolean;
var
  Focus: HWND;
  DC: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo));
  Focus := GetFocus;
  DC := GetDC(Focus);
  try
    Result := GetDIBits(DC, Bitmap, 0, TBitmapInfoHeader(BitmapInfo).biHeight, @Bits,
      TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0;
  finally
    ReleaseDC(Focus, DC);
  end;
end;

// ------------------
// ------------------ TAVIRecorder ------------------
// ------------------

constructor TAVIRecorder.Create(AOwner : TComponent);
begin
   inherited;
   FWidth:=320; // default values
   FHeight:=200;
   FFPS:=25;
   FCompressor:=acDefault;
   RecorderState:=rsNone;
   FSizeRestriction:=srForceBlock8x8;
   FImageRetrievalMode:=irmBitBlt;
end;

destructor TAVIRecorder.Destroy;
begin
     inherited;
end;

function TAVIRecorder.Restricted(s:integer):integer;
begin
  case FSizeRestriction of
    srForceBlock2x2: result:=(s div 2) * 2;
    srForceBlock4x4: result:=(s div 4) * 4;
    srForceBlock8x8: result:=(s div 8) * 8;
  else
    result:=s;
  end;
end;

procedure TAVIRecorder.SetHeight(const val : integer);
begin
  if RecorderState=rsRecording then exit;

  if val<>FHeight then
   if val>0 then FHeight:=Restricted(val);
end;

procedure TAVIRecorder.SetWidth(const val : integer);
begin
  if RecorderState=rsRecording then exit;

  if val<>FWidth then
   if val>0 then FWidth:=Restricted(val);
end;

procedure TAVIRecorder.SetSizeRestriction(const val : TAVISizeRestriction);
begin
  if val<>FSizeRestriction then
   begin
     FSizeRestriction:=val;
     FHeight:=Restricted(FHeight);
     FWidth:=Restricted(FWidth);
   end;
end;

// AddAVIFrame (from sceneviewer)
//
procedure TAVIRecorder.AddAVIFrame;
var
   bmp32 : TGLBitmap32;
   bmp : TBitmap;
begin
   if RecorderState<>rsRecording then
      raise Exception.create('Cannot add frame to AVI. AVI file not created.');

   Assert(FGLSceneViewer<>nil);

   case ImageRetrievalMode of
      irmSnapShot : begin
         bmp32:=GLSceneViewer.Buffer.CreateSnapShot;
         try
            bmp:=bmp32.Create32BitsBitmap;
            try
               AVIBitmap.Canvas.Draw(0, 0, bmp);
            finally
               bmp.Free;
            end;
         finally
            bmp32.Free;
         end;
      end;
      irmBitBlt : begin
         GLSceneViewer.Buffer.RenderingContext.Activate;
         try
            BitBlt(AVIBitmap.Canvas.Handle, 0, 0, AVIBitmap.Width, AVIBitmap.Height,
                   wglGetCurrentDC, 0, 0, SRCCOPY);
         finally
            GLSceneViewer.Buffer.RenderingContext.Deactivate;
         end;
      end;
      irmRenderToBitmap : begin
         GLSceneViewer.Buffer.RenderToBitmap(AVIBitmap, AVI_DPI);
      end;
   else
      Assert(False);
   end;

   InternalAddAVIFrame;
end;

// AddAVIFrame (from custom bitmap)
//
procedure TAVIRecorder.AddAVIFrame(bmp : TBitmap);
begin
   if RecorderState<>rsRecording then
      raise Exception.create('Cannot add frame to AVI. AVI file not created.');
   AVIBitmap.Canvas.Draw(0, 0, bmp);

   InternalAddAVIFrame;
end;

// InternalAddAVIFrame
//
procedure TAVIRecorder.InternalAddAVIFrame;
begin
   if Assigned(FOnPostProcessEvent) then
      FOnPostProcessEvent(Self, AVIBitmap);
   with AVIBitmap do begin
      InternalGetDIB( Handle, BitmapInfo^, BitmapBits^);
      if AVIStreamWrite(Stream_c, AVIFrameIndex, 1, BitmapBits, BitmapSize,
                        AVIIF_KEYFRAME, nil, nil)<>AVIERR_OK then
         raise Exception.Create('Add Frame Error');
      Inc(AVIFrameIndex);
   end;
end;

function TAVIRecorder.CreateAVIFile(DPI : integer = 0) : boolean;
var
   SaveDialog     : TSaveDialog;
   SaveAllowed    : Boolean;
   gaAVIOptions   : TAVICOMPRESSOPTIONS;
   galpAVIOptions : PAVICOMPRESSOPTIONS;
   BitmapInfoSize : Integer;
   AVIResult      : Cardinal;
   ResultString   : string;
begin
  SaveDialog := nil;
  Assert(FGLSceneViewer<>nil);

  try
    TempName := FAVIFilename;
    SaveAllowed := True;

    if TempName = '' then begin // if user didn't supply a filename, then ask for it
      SaveDialog := TSaveDialog.Create(Application);
      with SaveDialog do begin
        Options := [ofHideReadOnly, ofNoReadOnlyReturn];
        DefaultExt:='.avi';
        Filter := 'AVI Files (*.avi)|*.avi';
        SaveAllowed := Execute;
      end;
    end;

    if SaveAllowed then begin
      if TempName = '' then begin
        TempName := SaveDialog.FileName;
        if (FileExists(SaveDialog.FileName)) then
          SaveAllowed := MessageDlg(Format('Overwrite file %s?', [SaveDialog.FileName]),
                                    mtConfirmation, [mbYes, mbNo], 0) = mrYes;
      end;
    end;
  finally
    if SaveDialog<>nil then
      SaveDialog.Free;
  end;

  Result:=SaveAllowed;

  if not SaveAllowed then exit;

  AVIFileInit; // initialize the AVI lib.

  AVIBitmap:=TBitmap.Create;
  AVIFrameIndex:=0;

  RecorderState:=rsRecording;

  try
    AVIBitmap.PixelFormat := pf24Bit;
    AVIBitmap.Width := FWidth;
    AVIBitmap.Height := FHeight;

    // note: a filename with extension other then AVI give generate an error.
    if AVIFileOpen(pfile, PChar(TempName), OF_WRITE or OF_CREATE, nil)<>AVIERR_OK then
       raise Exception.Create('Cannot create AVI file. Disk full or file in use?');

    with AVIBitmap do begin
      InternalGetDIBSizes( Handle, BitmapInfoSize, BitmapSize);
      BitmapInfo:=AllocMem(BitmapInfoSize);
      BitmapBits:=AllocMem(BitmapSize);
      InternalGetDIB(Handle, BitmapInfo^, BitmapBits^);
    end;

    FillChar(asi,sizeof(asi),0);

    with asi do begin
      fccType   := streamtypeVIDEO; //  Now prepare the stream
      fccHandler:= 0;
      dwScale   := 1;         // dwRate / dwScale = frames/second
      dwRate    := FFPS;
      dwSuggestedBufferSize:=BitmapSize;
      rcFrame.Right  := BitmapInfo^.biWidth;
      rcFrame.Bottom := BitmapInfo^.biHeight;
    end;

    if AVIFileCreateStream(pfile, Stream, asi)<>AVIERR_OK then
       raise Exception.Create('Cannot create AVI stream.');

    with AVIBitmap do
      InternalGetDIB( Handle, BitmapInfo^, BitmapBits^);

    galpAVIOptions:=@gaAVIOptions;
    fillchar(gaAVIOptions, sizeof(gaAVIOptions), 0);

    if (FCompressor=acShowDialog) and
      // the following line will call a dialog box for the user to choose the compressor options
      AVISaveOptions( FGLSceneViewer.parent.Handle,
                      ICMF_CHOOSE_KEYFRAME or ICMF_CHOOSE_DATARATE, 1, Stream, galpAVIOptions ) then
    else begin
      with gaAVIOptions do begin // or, you may want to fill the compression options yourself
        fccType:=streamtypeVIDEO;
        fccHandler:=mmioFOURCC('M','S','V','C'); // User MS video 1 as default.
                                                 // I guess it is installed on every Win95 or later.
        dwQuality:=7500;     // compress quality 0-10,000
        dwFlags:=0;          // setting dwFlags to 0 would lead to some default settings
      end;
    end;

    AVIResult:=AVIMakeCompressedStream(Stream_c, Stream, galpAVIOptions, nil);

    if AVIResult <> AVIERR_OK then begin
      if AVIResult = AVIERR_NOCOMPRESSOR then
          ResultString:='No such compressor found'
      else ResultString:='';
      raise Exception.Create('Cannot make compressed stream. ' + ResultString);
    end;

    if AVIStreamSetFormat(Stream_c, 0, BitmapInfo, BitmapInfoSize) <> AVIERR_OK then
      raise Exception.Create('AVIStreamSetFormat Error'); // no error description found in MSDN.

    AVI_DPI:=DPI;

  except
    CloseAVIFile(true);
  end;

end;

procedure TAVIRecorder.CloseAVIFile(UserAbort:boolean=false);
// if UserAbort, CloseAVIFile will also delete the unfinished file.
begin
   if RecorderState<>rsRecording then
      raise Exception.create('Cannot close AVI file. AVI file not created.');

   AVIBitmap.Free;

   FreeMem(BitmapInfo);
   FreeMem(BitmapBits);

   AVIFileExit; // finalize the AVI lib.

   // release the interfaces explicitly (can't rely on automatic release)
   Stream:=nil;
   Stream_c:=nil;
   pfile:=nil;

   if UserAbort then
      DeleteFile(TempName);

   RecorderState:=rsNone;
end;

// Recording
//
function TAVIRecorder.Recording : Boolean;
begin
   Result:=(RecorderState=rsRecording);
end;

end.
