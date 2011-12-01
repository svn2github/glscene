{*
 * This unit is primarily based upon
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/media/UVideo.pas $
 *}

unit UVideoPlayback;

{*
 * based on 'An ffmpeg and SDL Tutorial' (http://www.dranger.com/ffmpeg/)
 *}

// uncomment if you want to see the debug stuff
{.$define DebugDisplay}
{.$define Info}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

// use BGR-format for accelerated colorspace conversion with swscale
{$IFDEF UseSWScale}
  {$DEFINE PIXEL_FMT_BGR}
{$ENDIF}

uses
  Classes,
  SysUtils,
  Math,
  SDL,
  avcodec,
  avformat,
  avutil,
  avio,
  rational,
  {$IFDEF UseSWScale}
  swscale,
  {$ENDIF}
  OpenGLAdapter,
  OpenGLTokens,
  GLContext,
  UMediaCore_FFmpeg,
  UConfig,
  UMusic,
  GLSLog
  ;

{$DEFINE PIXEL_FMT_BGR}

const
{$IFDEF PIXEL_FMT_BGR}
  PIXEL_FMT_OPENGL = GL_BGR;
  PIXEL_FMT_FFMPEG = PIX_FMT_BGR24;
  PIXEL_FMT_SIZE = 3;

  // looks strange on linux:
  //PIXEL_FMT_OPENGL = GL_RGBA;
  //PIXEL_FMT_FFMPEG = PIX_FMT_BGR32;
  //PIXEL_FMT_SIZE   = 4;
{$ELSE}
  // looks strange on linux:
  PIXEL_FMT_OPENGL = GL_RGB;
  PIXEL_FMT_FFMPEG = PIX_FMT_BGR24;
  PIXEL_FMT_SIZE = 3;
{$ENDIF}

  ReflectionH = 0.5; //reflection height (50%)

type
  IVideo_FFmpeg = interface (IVideo)
  ['{E640E130-C8C0-4399-AF02-67A3569313AB}']
    function Open(const FileName: UTF8string): boolean;
    procedure Close;
  end;

  { TVideo_FFmpeg }

  TVideo_FFmpeg = class( TInterfacedObject, IVideo_FFmpeg )
  private
    fInitialized: boolean;

    fOpened: boolean;     //**< stream successfully opened
    fPaused: boolean;     //**< stream paused
    fEOF: boolean;        //**< end-of-file state

    fLoop: boolean;       //**< looping enabled

    fStream: PAVStream;
    fStreamIndex: integer;
    fFormatContext: PAVFormatContext;
    fCodecContext: PAVCodecContext;
    fCodec: PAVCodec;

    fAVFrame: PAVFrame;
    fAVFrameRGB: PAVFrame;

    fFrameBuffer: PByte;  //**< stores a FFmpeg video frame
    //   fFrameTex:    GLuint; //**< OpenGL texture for FrameBuffer
    fFrameTexValid: boolean; //**< if true, fFrameTex contains the current frame
    fTexWidth, fTexHeight: cardinal;

    {$IFDEF UseSWScale}
    fSwScaleContext: PSwsContext;
    {$ENDIF}

    fAspect: real;        //**< width/height ratio

    fFrameDuration: extended; //**< duration of a video frame in seconds (= 1/fps)
    fFrameTime: extended; //**< video time position (absolute)
    fFrameLastTime: extended; //**< video Last time position (absolute)
    fLoopTime: extended;  //**< start time of the current loop

    procedure Reset();
    function DecodeFrame(): boolean;
    procedure SynchronizeTime(Frame: PAVFrame; var pts: double);

    procedure ShowDebugInfo();

  public
    constructor Create;
    destructor Destroy; override;

    function Open(const FileName: UTF8string): boolean; virtual;
    procedure Close; virtual;

    function GetHandle(): Pointer; virtual;

    procedure Play;
    procedure Pause;
    procedure Stop;

    procedure SetLoop(Enable: boolean);
    function GetLoop(): boolean;

    procedure SetPosition(Time: real);
    function GetPosition: real;

    procedure GetFrame(Time: extended);virtual;
    property Loop: boolean read GetLoop write SetLoop;
    property Position: real read GetPosition write SetPosition;
  end;

  { TVideo_FFmpeg_GL }

  TVideo_FFmpeg_GL = class(TVideo_FFmpeg)
    private
      FFrameTexture: TGLTextureHandle;
      fPboEnabled: boolean;
      FPBO: TGLUnpackPBOHandle;
    public
      constructor Create;
      destructor Destroy; override;
      function Open(const FileName: UTF8string): boolean; override;
      procedure Close; override;
      function GetHandle(): Pointer; override;
      procedure GetFrame(Time: extended); override;
  end;

  { TVideo_FFmpeg_SDL }

  TVideo_FFmpeg_SDL = class(TVideo_FFmpeg)
    private
      bmp             : PSDL_Overlay;
      screen          : PSDL_Surface;
      pScaleCtx       : PSwsContext;
    public
      function Open(const FileName: UTF8string): boolean; override;
      procedure GetFrame(Time: extended); override;
  end;

  TVideoPlayback_FFmpeg = class( TInterfacedObject, IVideoPlayback )
  private
    fInitialized: boolean;
    fVideoPlaybackList: TInterfaceList;
  public
    function GetName: String;

    function Init(): boolean;
    function Finalize: boolean;

    function Open(const FileName : UTF8String): IVideo;
    procedure Close();
  end;

implementation

uses
  GLUtils, GLTextureFormat;

var
  FFmpegCore: TMediaCore_FFmpeg;

// These are called whenever we allocate a frame buffer.
// We use this to store the global_pts in a frame at the time it is allocated.
function PtsGetBuffer(CodecCtx: PAVCodecContext; Frame: PAVFrame): integer; cdecl;
var
  pts: Pint64;
  VideoPktPts: Pint64;
begin
  Result := avcodec_default_get_buffer(CodecCtx, Frame);
  VideoPktPts := CodecCtx^.opaque;
  if (VideoPktPts <> nil) then
  begin
    // Note: we must copy the pts instead of passing a pointer, because the packet
    // (and with it the pts) might change before a frame is returned by av_decode_video.
    pts := av_malloc(sizeof(int64));
    pts^ := VideoPktPts^;
    Frame^.opaque := pts;
  end;
end;

procedure PtsReleaseBuffer(CodecCtx: PAVCodecContext; Frame: PAVFrame); cdecl;
begin
  if (Frame <> nil) then
    av_freep(@Frame^.opaque);
  avcodec_default_release_buffer(CodecCtx, Frame);
end;


{*------------------------------------------------------------------------------
 * TVideoPlayback_ffmpeg
 *------------------------------------------------------------------------------}

function  TVideoPlayback_FFmpeg.GetName: String;
begin
  result := 'FFmpeg_Video';
end;

function TVideoPlayback_FFmpeg.Init(): boolean;
begin
  Result := true;

  if (fInitialized) then
    Exit;
  fInitialized := true;

  FFmpegCore := TMediaCore_FFmpeg.GetInstance();

  av_register_all();
end;

function TVideoPlayback_FFmpeg.Finalize(): boolean;
begin
  Result := true;
end;

function TVideoPlayback_FFmpeg.Open(const FileName : UTF8String): IVideo;
var
  Video: IVideo_FFmpeg;
   i : Integer;
begin
  Video := TVideo_FFmpeg_GL.Create;


  if not Assigned(fVideoPlaybackList) then
     fVideoPlaybackList:= TInterfaceList.Create;

  if Video.Open(FileName) then
  begin
    Result := Video;
    fVideoPlaybackList.Add(Video);
  end
    else
      begin
        Result := nil;
        FreeandNil(Video);
      end;
end;

procedure TVideoPlayback_FFmpeg.Close();
var
   Video: IVideo_FFmpeg;
   I: Integer;
begin
    if Assigned(fVideoPlaybackList) then
    begin
      for i := 0 to fVideoPlaybackList.Count-1 do
           if Assigned(fVideoPlaybackList[i]) then begin
             Video := IVideo_FFmpeg(fVideoPlaybackList[i]);
             Video.Close;
             FreeandNil(Video);
             fVideoPlaybackList[i] := nil;
           end;
      FreeandNil(fVideoPlaybackList);
    end;
end;

{* TVideo_FFmpeg *}

constructor TVideo_FFmpeg.Create;
begin
  Reset();
end;

destructor TVideo_FFmpeg.Destroy;
begin
  Close();
end;

function TVideo_FFmpeg.Open(const FileName: UTF8string): boolean;
var
  errnum, LSize: integer;
  AudioStreamIndex: integer;
begin
  Result := False;
  Reset();

  // use custom 'ufile' protocol for UTF-8 support
  errnum := av_open_input_file(fFormatContext, PAnsiChar({'ufile:'+}FileName{.ToUTF8}),
    nil, 0, nil);
  if (errnum <> 0) then
  begin
    GLSLogger.LogError('Failed to open file "' +
      FileName + '" (' + FFmpegCore.GetErrorString(errnum) + ')');
    Exit;
  end;

  // update video info
  if (av_find_stream_info(fFormatContext) < 0) then
  begin
    GLSLogger.LogError('No stream info found' + ' TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  GLSLogger.LogInfo('VideoStreamIndex : ' + IntToStr(fStreamIndex) +
    ' TVideoPlayback_ffmpeg.Open');

  // find video stream
  FFmpegCore.FindStreamIDs(fFormatContext, fStreamIndex, AudioStreamIndex);
  if (fStreamIndex < 0) then
  begin
    GLSLogger.LogError('No video stream found' + ' TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  fStream := fFormatContext^.streams[fStreamIndex];
  fCodecContext := fStream^.codec;

  fCodec := avcodec_find_decoder(fCodecContext^.codec_id);
  if (fCodec = nil) then
  begin
    GLSLogger.LogError('No matching codec found' + ' TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // set debug options
  fCodecContext^.debug_mv := 0;
  fCodecContext^.debug := 0;

  // detect bug-workarounds automatically
  fCodecContext^.workaround_bugs := FF_BUG_AUTODETECT;
  // error resilience strategy (careful/compliant/agressive/very_aggressive)
  //fCodecContext^.error_resilience := FF_ER_CAREFUL; //FF_ER_COMPLIANT;
  // allow non spec compliant speedup tricks.
  //fCodecContext^.flags2 := fCodecContext^.flags2 or CODEC_FLAG2_FAST;

  // Note: avcodec_open() and avcodec_close() are not thread-safe and will
  // fail if called concurrently by different threads.
  FFmpegCore.LockAVCodec();
  try
    errnum := avcodec_open(fCodecContext, fCodec);
  finally
    FFmpegCore.UnlockAVCodec();
  end;
  if (errnum < 0) then
  begin
    GLSLogger.LogError('No matching codec found' + ' TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // register custom callbacks for pts-determination
  fCodecContext^.get_buffer := PtsGetBuffer;
  fCodecContext^.release_buffer := PtsReleaseBuffer;

  {$ifdef DebugDisplay}
  GLSLogger.LogDebug('Found a matching Codec: ' + fCodecContext^.Codec.Name +
    sLineBreak + sLineBreak + '  Width = ' + IntToStr(fCodecContext^.Width) +
    ', Height=' + IntToStr(fCodecContext^.Height) + sLineBreak +
    '  Aspect    : ' + IntToStr(fCodecContext^.sample_aspect_ratio.num) +
    '/' + IntToStr(fCodecContext^.sample_aspect_ratio.den) +
    sLineBreak + '  Framerate : ' + IntToStr(fCodecContext^.time_base.num) +
    '/' + IntToStr(fCodecContext^.time_base.den));
  {$endif}

  // allocate space for decoded frame and rgb frame
  fAVFrame := avcodec_alloc_frame();
  fAVFrameRGB := avcodec_alloc_frame();
  fFrameBuffer := av_malloc(avpicture_get_size(PIXEL_FMT_FFMPEG,
    fCodecContext^.Width, fCodecContext^.Height));

  if ((fAVFrame = nil) or (fAVFrameRGB = nil) or (fFrameBuffer = nil)) then
  begin
    GLSLogger.LogError('Failed to allocate buffers' + ' TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // TODO: pad data for OpenGL to GL_UNPACK_ALIGNMENT
  // (otherwise video will be distorted if width/height is not a multiple of the alignment)
  errnum := avpicture_fill(PAVPicture(fAVFrameRGB), fFrameBuffer,
    PIXEL_FMT_FFMPEG, fCodecContext^.Width, fCodecContext^.Height);
  if (errnum < 0) then
  begin
    GLSLogger.LogError('avpicture_fill failed: ' + FFmpegCore.GetErrorString(errnum) +
      ' TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // calculate some information for video display
  fAspect := av_q2d(fCodecContext^.sample_aspect_ratio);
  if (fAspect = 0) then
    fAspect := fCodecContext^.Width /
               fCodecContext^.Height
  else
    fAspect := fAspect * fCodecContext^.Width /
                         fCodecContext^.Height;

  fFrameDuration := 1 / av_q2d(fStream^.r_frame_rate);

  // hack to get reasonable framerate (for divx and others)
  if (fFrameDuration < 0.02) then // 0.02 <-> 50 fps
  begin
    fFrameDuration := av_q2d(fStream^.r_frame_rate);
    while (fFrameDuration > 50) do
      fFrameDuration := fFrameDuration / 10;
    fFrameDuration := 1 / fFrameDuration;
  end;

  GLSLogger.LogInfo('Framerate: ' + IntToStr(floor(1 / fFrameDuration)) + 'fps' +
    ' TVideoPlayback_ffmpeg.Open');

  {$IFDEF UseSWScale}
  // if available get a SWScale-context -> faster than the deprecated img_convert().
  // SWScale has accelerated support for PIX_FMT_RGB32/PIX_FMT_BGR24/PIX_FMT_BGR565/PIX_FMT_BGR555.
  // Note: PIX_FMT_RGB32 is a BGR- and not an RGB-format (maybe a bug)!!!
  // The BGR565-formats (GL_UNSIGNED_SHORT_5_6_5) is way too slow because of its
  // bad OpenGL support. The BGR formats have MMX(2) implementations but no speed-up
  // could be observed in comparison to the RGB versions.
  fSwScaleContext := sws_getContext(
      fCodecContext^.width, fCodecContext^.height,
      fCodecContext^.pix_fmt,
      fCodecContext^.width, fCodecContext^.height,
      PIXEL_FMT_FFMPEG,
      SWS_FAST_BILINEAR, nil, nil, nil);
  if (fSwScaleContext = nil) then
  begin
    GLSLogger.LogError('Failed to get swscale context ' + ' TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;
  {$ENDIF}

  fTexWidth   := Round(Power(2, Ceil(Log2(fCodecContext^.width))));
  fTexHeight  := Round(Power(2, Ceil(Log2(fCodecContext^.height))));

  fOpened := True;
  Result := True;
end;

procedure TVideo_FFmpeg.Reset();
begin
  // close previously opened video
  Close();

  fOpened := False;
  fPaused := False;
  fFrameDuration := 0;
  fFrameTime := 0;
  fStream := nil;
  fStreamIndex := -1;
  fFrameTexValid := False;

  fEOF := False;

  fLoop := False;
  fLoopTime := 0;
end;

procedure TVideo_FFmpeg.Close;
begin
  if (fFrameBuffer <> nil) then
    av_free(fFrameBuffer);
  if (fAVFrameRGB <> nil) then
    av_free(fAVFrameRGB);
  if (fAVFrame <> nil) then
    av_free(fAVFrame);

  fAVFrame := nil;
  fAVFrameRGB := nil;
  fFrameBuffer := nil;

  if (fCodecContext <> nil) then
  begin
    // avcodec_close() is not thread-safe
    FFmpegCore.LockAVCodec();
    try
      avcodec_close(fCodecContext);
    finally
      FFmpegCore.UnlockAVCodec();
    end;
  end;

  if (fFormatContext <> nil) then
    av_close_input_file(fFormatContext);

  fCodecContext := nil;
  fFormatContext := nil;

  fOpened := False;
end;

function TVideo_FFmpeg.GetHandle(): Pointer;
begin
  Result := nil;
end;

procedure TVideo_FFmpeg.SynchronizeTime(Frame: PAVFrame; var pts: double);
var
  FrameDelay: double;
begin
  if (pts <> 0) then
  begin
    // if we have pts, set video clock to it
    fFrameTime := pts;
  end
  else
  begin
    // if we aren't given a pts, set it to the clock
    pts := fFrameTime;
  end;

  // update the video clock
  FrameDelay := av_q2d(fCodecContext^.time_base);
  // if we are repeating a frame, adjust clock accordingly
  FrameDelay := FrameDelay + Frame^.repeat_pict * (FrameDelay * 0.5);
  fFrameTime := fFrameTime + FrameDelay;
end;

{**
 * Decode a new frame from the video stream.
 * The decoded frame is stored in fAVFrame. fFrameTime is updated to the new frame's
 * time.
 * @param pts will be updated to the presentation time of the decoded frame.
 * returns true if a frame could be decoded. False if an error or EOF occured.
 *}
function TVideo_FFmpeg.DecodeFrame(): boolean;
var
  FrameFinished: integer;
  VideoPktPts: int64;
  pbIOCtx: PByteIOContext;
  errnum: integer;
  AVPacket: TAVPacket;
  pts,delay: double;

begin
  Result := False;
  FrameFinished := 0;

  if fEOF then
    Exit;

  // read packets until we have a finished frame (or there are no more packets)
  while (FrameFinished = 0) do
  begin
    errnum := av_read_frame(fFormatContext, AVPacket);
    if (errnum < 0) then
    begin
      // failed to read a frame, check reason

      {$IF (LIBAVFORMAT_VERSION_MAJOR >= 52)}
      pbIOCtx := fFormatContext^.pb;
      {$ELSE}
      pbIOCtx := @fFormatContext^.pb;
      {$IFEND}

      // check for end-of-file (EOF is not an error)
      if (url_feof(pbIOCtx) <> 0) then
      begin
        fEOF := True;
        Exit;
      end;

      // check for errors
      if (url_ferror(pbIOCtx) <> 0) then
      begin
        GLSLogger.LogError('Video decoding file error' +
          ' TVideoPlayback_FFmpeg.DecodeFrame');
        Exit;
      end;

      // url_feof() does not detect an EOF for some mov-files (e.g. deluxe.mov)
      // so we have to do it this way.
      if ((fFormatContext^.file_size <> 0) and
        (pbIOCtx^.pos >= fFormatContext^.file_size)) then
      begin
        fEOF := True;
        Exit;
      end;

      // error occured, log and exit
      GLSLogger.LogError('Video decoding error' + ' TVideoPlayback_FFmpeg.DecodeFrame');
      Exit;
    end;

    // if we got a packet from the video stream, then decode it
    if (AVPacket.stream_index = fStreamIndex) then
    begin
      // save pts to be stored in pFrame in first call of PtsGetBuffer()
      VideoPktPts := AVPacket.pts;
      fCodecContext^.opaque := @VideoPktPts;

      // decode packet
      avcodec_decode_video(fCodecContext, fAVFrame,
        frameFinished, AVPacket.Data, AVPacket.size);

      // reset opaque data
      fCodecContext^.opaque := nil;

      // update pts
      if (AVPacket.dts <> AV_NOPTS_VALUE) then
      begin
        pts := AVPacket.dts;
      end
      else if ((fAVFrame^.opaque <> nil) and
        (Pint64(fAVFrame^.opaque)^ <> AV_NOPTS_VALUE)) then
      begin
        pts := Pint64(fAVFrame^.opaque)^;
      end
      else
      begin
        pts := 0;
      end;

      if fStream^.start_time <> AV_NOPTS_VALUE then
        pts := pts - fStream^.start_time;

      pts := pts * av_q2d(fStream^.time_base);

      // synchronize time on each complete frame
      if (frameFinished <> 0) then
        SynchronizeTime(fAVFrame, pts);
    end;

    // free the packet from av_read_frame
    av_free_packet(@AVPacket);
  end;

  Result := True;
end;

procedure TVideo_FFmpeg.GetFrame(Time: extended);
var
  errnum: integer;
  CurrentTime: extended;
  TimeDiff: extended;
  DropFrameCount: integer;
  i: integer;
  Success: boolean;
  delay: Extended;
const
  SKIP_FRAME_DIFF = 0.010; // start skipping if we are >= 10ms too late
begin
  if Time = -1 then
    Exit;

  if not fOpened then
    Exit;

  if fPaused then
    Exit;

  {*
   * Synchronization - begin
   *}

  // requested stream position (relative to the last loop's start)
  if (fLoop) then
    CurrentTime := Time - fLoopTime
  else
    CurrentTime := Time;

  // check if current texture still contains the active frame
  if (fFrameTexValid) then
  begin
    // time since the last frame was returned
    TimeDiff := CurrentTime - fFrameTime;

    {$IFDEF DebugDisplay}
    GLSLogger.LogDebug('Time:      ' + IntToStr(floor(Time * 1000)) + sLineBreak +
      'VideoTime: ' + IntToStr(floor(fFrameTime * 1000)) +
      sLineBreak + 'TimeBase:  ' + IntToStr(floor(fFrameDuration * 1000)) +
      sLineBreak + 'TimeDiff:  ' + IntToStr(floor(TimeDiff * 1000)));
    {$endif}

    // check if time has reached the next frame
    if (TimeDiff < fFrameDuration) then
    begin
     {$IFDEF DebugDisplay}
      GLSLogger.LogDebug( sLineBreak +'not getting new frame' + sLineBreak +
        'Time:      ' + IntToStr(floor(Time * 1000)) + sLineBreak +
        'VideoTime: ' + IntToStr(floor(fFrameTime * 1000)) + sLineBreak +
        'TimeBase:  ' + IntToStr(floor(fFrameDuration * 1000)) + sLineBreak +
        'TimeDiff:  ' + IntToStr(floor(TimeDiff * 1000)));
      {$endif}

      // we do not need a new frame now
      Exit;
    end;
  end;

  // fetch new frame (updates fFrameTime)
  Success := DecodeFrame();
  TimeDiff := CurrentTime - fFrameTime;

  // check if we have to skip frames
  // Either if we are one frame behind or if the skip threshold has been reached.
  // Do not skip if the difference is less than fFrameDuration as there is no next frame.
  // Note: We assume that fFrameDuration is the length of one frame.
  if (TimeDiff >= Max(fFrameDuration, SKIP_FRAME_DIFF)) then
  begin
    {$IFDEF DebugDisplay}
    GLSLogger.LogDebug('skipping frames' + sLineBreak +
      'TimeBase:  ' + IntToStr(floor(fFrameDuration * 1000)) + sLineBreak +
      'TimeDiff:  ' + IntToStr(floor(TimeDiff * 1000)));
    {$endif}

    // update video-time
    DropFrameCount := Trunc(TimeDiff / fFrameDuration);
    fFrameTime := fFrameTime + DropFrameCount * fFrameDuration;

    // skip frames
    for i := 1 to DropFrameCount do
      Success := DecodeFrame();
  end;

  // check if we got an EOF or error
  if (not Success) then
  begin
    if fLoop then
    begin
      // we have to loop, so rewind
      SetPosition(0);
      // record the start-time of the current loop, so we can
      // determine the position in the stream (fFrameTime-fLoopTime) later.
      fLoopTime := Time;
    end;
    Exit;
  end;

  {*
   * Synchronization - end
   *}

  // TODO: support for pan&scan
  //if (fAVFrame.pan_scan <> nil) then
  //begin
  //  Writeln(Format('PanScan: %d/%d', [fAVFrame.pan_scan.width, fAVFrame.pan_scan.height]));
  //end;

 (*TimeDiff>=0
  *Разница между кадрами может быть отрицательна в случае если
  *в ролике присутсвет пауза.
  *При чтении текущего пакета задается время до следующего
  *если разница в текущем времени и времени будущего кадра велика
  *то вывести кадр в текстуру лучше из предедущего кадра.
  *Например:
    TimeDiff<0
    CurrentTime 0,56750730403996  текущее время
    FrameTime 1,93526860193527   время до следующего кадра
    TimeDiff -1,36776129789531   разница при вычете

    TimeDiff>=0
    CurrentTime 0,53506399338236
    fFrameTime 0,53386720053387
    TimeDiff 0,0011967928485
  *)
  if TimeDiff>0 then
  begin
    // otherwise we convert the pixeldata from YUV to RGB
    {$IFDEF UseSWScale}
    errnum := sws_scale(fSwScaleContext, @fAVFrame.Data, @fAVFrame.linesize,
      0, fCodecContext^.Height, @fAVFrameRGB.Data, @fAVFrameRGB.linesize);
    {$ELSE}
    // img_convert from lib/ffmpeg/avcodec.pas is actually deprecated.
    // If ./configure does not find SWScale then this gives the error
    // that the identifier img_convert is not known or similar.
    // I think this should be removed, but am not sure whether there should
    // be some other replacement or a warning, Therefore, I leave it for now.
    // April 2009, mischi
    errnum := img_convert(PAVPicture(fAVFrameRGB), PIXEL_FMT_FFMPEG,
      PAVPicture(fAVFrame), fCodecContext^.pix_fmt,
      fCodecContext^.Width, fCodecContext^.Height);
    {$ENDIF}

    if (errnum < 0) then
    begin
      GLSLogger.LogError('Image conversion failed' + ' TVideoPlayback_ffmpeg.GetFrame');
      Exit;
    end;
  end;

  // TODO: data is not padded, so we will need to tell OpenGL.
  //   Or should we add padding with avpicture_fill? (check which one is faster)
  //glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

end;

procedure TVideo_FFmpeg.ShowDebugInfo();
begin
  {$IFDEF Info}
  if (fFrameTime + fFrameDuration < 0) then
  begin
    GL.Color4f(0.7, 1, 0.3, 1);
    SetFontStyle(1);
    SetFontItalic(False);
    SetFontSize(27);
    SetFontPos(300, 0);
    GL.Print('Delay due to negative VideoGap');
    GL.Color4f(1, 1, 1, 1);
  end;
  {$ENDIF}

  {$IFDEF DebugFrames}
  GL.Color4f(0, 0, 0, 0.2);
  GL.Begin_(GL_QUADS);
  GL.Vertex2f(0, 0);
  GL.Vertex2f(0, 70);
  GL.Vertex2f(250, 70);
  GL.Vertex2f(250, 0);
  GL.End_;

  GL.Color4f(1, 1, 1, 1);
  SetFontStyle(1);
  SetFontItalic(False);
  SetFontSize(27);
  SetFontPos(5, 0);
  GL.Print('delaying frame');
  SetFontPos(5, 20);
  GL.Print('fetching frame');
  SetFontPos(5, 40);
  GL.Print('dropping frame');
  {$ENDIF}
end;

procedure TVideo_FFmpeg.Play;
begin
    fPaused := false;
end;

procedure TVideo_FFmpeg.Pause;
begin
  fPaused := not fPaused;
end;

procedure TVideo_FFmpeg.Stop;
begin
  fPaused := true;
  SetPosition(0);
end;

procedure TVideo_FFmpeg.SetLoop(Enable: boolean);
begin
  fLoop := Enable;
  fLoopTime := 0;
end;

function TVideo_FFmpeg.GetLoop(): boolean;
begin
  Result := fLoop;
end;


{**
 * Sets the stream's position.
 * The stream is set to the first keyframe with timestamp <= Time.
 * Note that fFrameTime is set to Time no matter if the actual position seeked to is
 * at Time or the time of a preceding keyframe. fFrameTime will be updated to the
 * actual frame time when GetFrame() is called the next time.
 * @param Time new position in seconds
 *}
procedure TVideo_FFmpeg.SetPosition(Time: real);
var
  SeekFlags: integer;
begin
  if not fOpened then
    Exit;

  if (Time < 0) then
    Time := 0;

  // TODO: handle fLoop-times
  //Time := Time mod VideoDuration;

  // Do not use the AVSEEK_FLAG_ANY here. It will seek to any frame, even
  // non keyframes (P-/B-frames). It will produce corrupted video frames as
  // FFmpeg does not use the information of the preceding I-frame.
  // The picture might be gray or green until the next keyframe occurs.
  // Instead seek the first keyframe smaller than the requested time
  // (AVSEEK_FLAG_BACKWARD). As this can be some seconds earlier than the
  // requested time, let the sync in GetFrame() do its job.
  SeekFlags := AVSEEK_FLAG_BACKWARD;

  fFrameTime := Time;
  fEOF := False;
  fFrameTexValid := False;

  if (av_seek_frame(fFormatContext, fStreamIndex,
    Round(Time / av_q2d(fStream^.time_base)), SeekFlags) < 0) then
  begin
    GLSLogger.LogError('av_seek_frame() failed' + ' TVideoPlayback_ffmpeg.SetPosition');
    Exit;
  end;

  avcodec_flush_buffers(fCodecContext);
end;

function TVideo_FFmpeg.GetPosition: real;
begin
  Result := fFrameTime;
end;

{ TVideo_FFmpeg_GL }

constructor TVideo_FFmpeg_GL.Create;
begin
  inherited;
  FFrameTexture := TGLTextureHandle.Create;
  FPBO := TGLUnpackPBOHandle.Create;
end;

destructor TVideo_FFmpeg_GL.Destroy;
begin
  FFrameTexture.Destroy;
  FPBO.Destroy;
  inherited Destroy;
end;

function TVideo_FFmpeg_GL.Open(const FileName: UTF8string): boolean;
var
  LSize: integer;
begin
  fPboEnabled := true{PboSupported};

  Result:=inherited Open(FileName);

  fPboEnabled := fPboEnabled and FPBO.IsSupported;
  if fPboEnabled then
  begin
    GL.ClearError;
    FPBO.AllocateHandle;
    LSize := fCodecContext^.Width * fCodecContext^.Height * PIXEL_FMT_SIZE;
    FPBO.BindBufferData(nil, LSize, GL_STREAM_DRAW);
    FPBO.UnBind;
    GL.CheckError;
  end;

  // we retrieve a texture just once with glTexImage2D and update it with glTexSubImage2D later.
  // Benefits: glTexSubImage2D is faster and supports non-power-of-two widths/height.
  FFrameTexture.AllocateHandle;
  with CurrentGLContext.GLStates do
  begin
    TextureBinding[ActiveTexture, ttTexture2D] := FFrameTexture.Handle;
  end;
  with GL do
  begin
    TexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, fTexWidth, fTexHeight, 0,
      PIXEL_FMT_OPENGL, GL_UNSIGNED_BYTE, nil);
    TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end;
  FFrameTexture.NotifyDataUpdated;
end;

procedure TVideo_FFmpeg_GL.Close;
begin
  inherited Close;
  if (FPBO<>nil) then
  FPBO.DestroyHandle;
end;

function TVideo_FFmpeg_GL.GetHandle(): Pointer;
begin
  Result := PGLuint(FFrameTexture.Handle);
end;

procedure TVideo_FFmpeg_GL.GetFrame(Time: extended);
var
  LSize: integer;
  BufferPtr: PGLvoid;
begin
  inherited GetFrame(Time);

  if not fPboEnabled then
    with CurrentGLContext.GLStates do
    begin
      TextureBinding[ActiveTexture, ttTexture2D] := FFrameTexture.Handle;
      GL.TexSubImage2D(GL_TEXTURE_2D, 0, 0, 0,
        fCodecContext^.Width, fCodecContext^.Height,
        PIXEL_FMT_OPENGL, GL_UNSIGNED_BYTE, fAVFrameRGB^.Data[0]);
    end
  else // fPboEnabled
    with GL do
    begin
      ClearError;
      LSize := fCodecContext^.Height * fCodecContext^.Width * PIXEL_FMT_SIZE;
      FPBO.BindBufferData(nil, LSize, GL_STREAM_DRAW);

      bufferPtr := FPBO.MapBuffer(GL_WRITE_ONLY);
      if Assigned(bufferPtr) then
      begin
        Move(fAVFrameRGB^.Data[0]^, bufferPtr^,
          fCodecContext^.Height * fCodecContext^.Width * PIXEL_FMT_SIZE);

        // release pointer to mapping buffer
        FPBO.UnmapBuffer;
      end;

      with CurrentGLContext.GLStates do
        TextureBinding[ActiveTexture, ttTexture2D] := FFrameTexture.Handle;

      TexSubImage2D(GL_TEXTURE_2D, 0, 0, 0,
        fCodecContext^.Width, fCodecContext^.Height,
        PIXEL_FMT_OPENGL, GL_UNSIGNED_BYTE, nil);

      FPBO.UnBind;

      CheckError;
    end;

  // reset to default
  GL.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  if (not fFrameTexValid) then
  fFrameTexValid := True;
end;

{ TVideo_FFmpeg_SDL }

function TVideo_FFmpeg_SDL.Open(const FileName: UTF8string): boolean;
var
  dst_pix_fmt : TAVPixelFormat;
begin
  Result:=inherited Open(FileName);

  screen := SDL_SetVideoMode(640, 480, 24, 0);
  if not assigned(screen)  then
  begin
    raise exception.Create ('SDL: could not set video mode - exiting\n');
    exit();
  end;
  // Allocate a place to put our YUV image on that screen
  bmp := SDL_CreateYUVOverlay(fCodecContext^.Width,
        			fCodecContext^.Height,
        			SDL_YV12_OVERLAY,
        			screen);

  dst_pix_fmt := PIX_FMT_YUV420P;
  pScaleCtx := sws_getContext ( fCodecContext^.width, fCodecContext^.height, fCodecContext^.pix_fmt,
                                fCodecContext^.width, fCodecContext^.height, dst_pix_fmt, SWS_BICUBIC, nil, nil, nil  );
end;

procedure TVideo_FFmpeg_SDL.GetFrame(Time: extended);
var
  pict  : TAVPicture;
  rect         : TSDL_Rect;
  w, h, x, y   : integer;
begin
  inherited GetFrame(Time);

  if bmp<>nil then
  begin

    SDL_LockYUVOverlay(bmp);

    { point pict at the queue }

    inc(bmp.pixels); inc(bmp.pixels);
    pict.data[1] := sysutils.PByteArray(bmp.pixels^);
    dec(bmp.pixels);
    pict.data[2] := sysutils.PByteArray(bmp.pixels^);
    dec(bmp.pixels);
    pict.data[0] := sysutils.PByteArray(bmp.pixels^);

    inc(bmp.pitches); inc(bmp.pitches);
    pict.linesize[1] := integer(bmp.pitches^);
    dec(bmp.pitches);
    pict.linesize[2] := integer(bmp.pitches^);
    dec(bmp.pitches);
    pict.linesize[0] := integer(bmp.pitches^);

    sws_scale ( pScaleCtx, @fAVFrame.data, @fAVFrame.linesize,
                0, fCodecContext^.height,
                @pict.data, @pict.linesize );

     { now we inform our display thread that we have a pic ready }
    SDL_UnlockYUVOverlay(bmp);

    h := screen^.h;
    w := (round(h * fAspect)) and -3;
    if(w > screen^.w)then
    begin
      w := screen^.w;
      h := (round(w / fAspect)) and -3;
    end;
    x := (screen^.w - w) div 2;
    y := (screen^.h - h) div 2;

    rect.x := x;
    rect.y := y;
    rect.w := w;
    rect.h := h;
    SDL_DisplayYUVOverlay(bmp, @rect);
  end;
      if (not fFrameTexValid) then
    fFrameTexValid := True;
end;

initialization
  MediaManager.Add(TVideoPlayback_FFmpeg.Create);
end.

