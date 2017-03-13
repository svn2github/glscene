unit glAviPlane;

interface
uses
    Winapi.Windows, System.Classes,

     
    GLScene,
    GLVectorGeometry,
    GLBaseClasses,
    OpenGL1x,
    GLTexture,
    GLRenderContextInfo,
    OpenGLTokens,
    GLSVfw;

type
    TQual = 1..4;
    TRenderMode = (rmTriStripArray,rmTriStrip);
    TAVIUpdateEvent= procedure (sender:TObject;FrameIndex:integer) of object;
    TGLAviPlane = class(TGLSceneObject)
    private
        fFilename: string;
        fQuality: TQual;
        fRenderMode: TRenderMode;
        IntQuality: integer;
        currdelta: single;
        fFrameindex, fFirstFrame, fLastFrame: integer;
        fUpdateRate: integer;
        pAvi: IAVIFile;
        pAvis: IAVIStream;
        AviInfo: TAVISTREAMINFO; //AviInfo: TAVISTREAMINFOA;
        // pavisound:IAVIStream;
        pFrame: IGetFrame;
        pBmi: PBitmapInfoHeader;
        pColors: pRGBTriple;
        fAviOpened: boolean;
        CurrTime: Double;
        CurrFrameCount: integer;
         FileOrPosChanged:boolean;

        StripArray: array of TAffineVector;
        ColorArray: array of TAffineVector;

        fwidth, fheight: single;

        fCurrentFrameRate, fUserFrameRate: integer;
        fTargetFrameRate: single;
        fAutoFrameRate: boolean;
        fOnUpdate:TAVIUpdateEvent;

        function OpenAvi: boolean;
        procedure FillColorArray;
        procedure BuildStripArray;
    protected
        procedure SetFilename(val: string);
        procedure SetQuality(val: TQual);
        procedure SetRendermode(val: TRendermode);
        procedure SetFrameIndex(val:integer);
        procedure SetAutoFrameRate(val:boolean);
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure buildlist(var rci: TGLRenderContextInfo); override;
        procedure StructureChanged; override;

        procedure DoProgress(const progressTime: TProgressTimes); override;

        procedure CloseAvi;

        function GetFrame(Framenum: integer): boolean;

        property UserFrameRate: integer read fUserFrameRate write fUserFrameRate;
        property FrameIndex: longint read fFrameindex write SetFrameIndex;
        property FirstFrame: longint read fFirstFrame write fFirstFrame;
        property LastFrame: longint read fLastFrame write fLastFrame;

        property Filename: string read fFilename write SetFilename;

        property AviOpened: boolean read fAviOpened;

        property Quality: TQual read fQuality write SetQuality;
        property Rendermode: TRendermode read fRenderMode write SetRendermode;
        property width: single read fWidth;
        property height: single read fHeight;
        property CurrentFrameRate: integer read fCurrentFrameRate; //current number of frames shown per second
        property TargetFrameRate: single read fTargetFrameRate; //Actual avi file frame rate
        property AutoFrameRate: boolean read fAutoFrameRate write SetAutoFrameRate; //Ignores UserFrameRate and uses TargetFrameRate instead

        property OnUpdate:TAVIUpdateEvent read fOnUpdate write fOnUpdate;
    end;

implementation

procedure TGLAviPlane.SetAutoFrameRate(val:boolean);
begin
    fAutoFrameRate:=val;
    FileOrPosChanged:=true;
end;

procedure TGLAviPlane.SetFrameIndex(val:integer);
begin
    fFrameIndex:=val;
    FileOrPosChanged:=true;
end;

procedure TGLAviPlane.SetRendermode(val: TRendermode);
begin
    fRendermode := val;
    StructureChanged;
end;

procedure TGLAviPlane.SetQuality(val: TQual);
begin
    fQuality := val;
    case fQuality of
        1:
            IntQuality := 8;
        2:
            IntQuality := 4;
        3:
            IntQuality := 2;
        4:
            IntQuality := 1;
    end;
    Structurechanged;
end;

procedure TGLAviPlane.SetFilename(val: string);
begin
    fFilename := val;
    openavi;
end;

constructor TGLAviPlane.Create(AOwner: TComponent);
begin
    inherited;
    ObjectStyle := ObjectStyle + [osDirectDraw];
    fQuality := 4;
    IntQuality := 1;
    fFrameIndex := 0;
    fFirstFrame := 0;
    fLastFrame := 0;
    fUpdateRate := 10;
    pBmi := nil;
    pColors := nil;
    fCurrentFrameRate := 0;
    CurrTime := 0;
    CurrFrameCount := 0;
    fTargetFrameRate := 0;
    fAutoFrameRate := true;
end;

destructor TGLAviPlane.Destroy;
begin
    if faviopened then
         closeavi;
    SetLength(StripArray, 0);
    SetLength(ColorArray, 0);
    inherited;
end;

procedure TGLAviPlane.buildlist(var rci: TGLRenderContextInfo);
var
    w, h: integer;
    x, y: integer;
    r: integer;
    rgb: PRGBTriple;
begin
    if not assigned(pbmi) then
        exit;

    r := IntQuality;
    w := pbmi^.biWidth div r;
    h := pbmi^.biHeight div r;

    glpushmatrix;
    gltranslatef(-w / (120 / r), -h / (120 / r), 0);
    fwidth := w / (60 / r);
    fheight := h / (60 / r);
    case rendermode of
        rmTriStrip:
            begin
                glFrontFace(GL_CW);
                r := IntQuality;
                w := pbmi^.biWidth;
                h := pbmi^.biHeight;
                rgb := pColors;
                inc(rgb, w);
                for y := 1 to (h div r) - 1 do
                begin
                    glbegin(GL_TRIANGLE_STRIP);
                    for x := (w div r) - 1 downto 0 do
                    begin
                        glcolor3f(rgb^.rgbtred / 255, rgb^.rgbtgreen / 255, rgb^.rgbtblue / 255);
                        glvertex3f(x / (60 / r), y / (60 / r), 0);
                        glvertex3f(x / (60 / r), (y - 1) / (60 / r), 0);
                        inc(rgb, r);
                    end;
                    if r > 1 then
                        inc(rgb, w * (r - 1));
                    glend;
                end;
                glFrontFace(GL_CCW);
            end;
        rmTriStripArray:
            begin
                if length(colorArray) <> length(StripArray) then
                    exit;
                glEnableClientState(GL_VERTEX_ARRAY);
                glEnableClientState(GL_COLOR_ARRAY);
                glVertexPointer(3, GL_FLOAT, 0, @StripArray[0]);
                glColorPointer(3, GL_FLOAT, 0, @ColorArray[0]);
                glLockArraysEXT(0, w * h);

                for y := 0 to h - 1 do
                    glDrawArrays(GL_TRIANGLE_STRIP, (y * (w)), w);
                glUnlockArraysEXT;
                glDisableClientState(GL_COLOR_ARRAY);
                glDisableClientState(GL_VERTEX_ARRAY);
            end;
    end;
    glpopmatrix;
end;

procedure TGLAviPlane.StructureChanged;
begin
    inherited;
    SetLength(StripArray, 0);
    SetLength(ColorArray, 0);
    case rendermode of
        rmTriStripArray:
            begin
                BuildStripArray;
                FillColorArray;
            end;
    end;
end;

procedure TGLAviPlane.DoProgress(const progressTime: TProgressTimes);
var
    temp: single;
begin
    inherited;

    if fAutoFrameRate then
        temp := fTargetFrameRate
    else
        temp := fUserFrameRate;

    if (temp = 0) or (not fAviOpened) then
        exit;

    CurrDelta := CurrDelta + ProgressTime.DeltaTime;

    if (CurrDelta >= 1 / temp) or FileOrPosChanged then
    begin
        CurrDelta := CurrDelta - (1 / temp);
        getframe(fFrameIndex);
        FillColorArray;
        inc(fFrameIndex);
        if fFrameIndex > fLastFrame then
            //    do event ?
            fFrameIndex := fFirstFrame;

        CurrFrameCount := CurrFrameCount + 1;
        if (ProgressTime.newTime >= CurrTime + 1) or FileOrPosChanged then
        begin
            if  FileOrPosChanged then
            begin
                CurrTime:=ProgressTime.newTime;
                CurrDelta:=0;
                FileOrPosChanged:=false;
            end
            else
                CurrTime := CurrTime + 1;
            fCurrentFrameRate := CurrFrameCount;
            CurrFrameCount := 0;
            if assigned(fOnUpdate) then
                fOnUpdate(self,fFrameIndex);
        end;
    end;
end;

function TGLAviPlane.GetFrame(Framenum: integer): boolean;
var
    tempbmi: PBitmapInfoHeader;
begin
    if not fAviOpened then
    begin
        result := false;
        exit;
    end;
    try
        tempbmi := AVIStreamGetFrame(pFrame, Framenum);

        result := assigned(tempbmi);
        if result then
        begin
            pbmi := tempbmi;
            pcolors := PRGBtriple(DWORD(pbmi) + pbmi^.bisize);

        end;
    except
        result := false;
    end;
end;

function TGLAviPlane.OpenAvi: boolean;
var
    m: integer;
begin
    if fAviOpened then
        closeavi;
    fFirstFrame := 0;
    fFrameIndex := 0;
    flastFrame := 0;

    result := AVIFileOpen(pavi, PChar(fFilename), OF_READ, nil) = AVIERR_OK;
    if not result then
        exit;

    result := AVIFILEGetStream(pavi, pavis, streamtypeVIDEO, 0) = AVIERR_OK;
    if not result then
        exit;

    result := AVIStreamInfo(pavis, AviInfo, sizeof(TAVISTREAMINFOA)) = AVIERR_OK;
    if not result then
        exit;

    if AviInfo.dwRate > 1000 then
    begin
        if AviInfo.dwRate > 100000 then
            m := 10000
        else
            if AviInfo.dwRate > 10000 then
            m := 1000
        else
            m := 100;
    end
    else
        m := 1;

    fTargetFrameRate := AviInfo.dwRate / m;

    fFirstFrame := AVIStreamStart(pavis);
    fLastFrame := AVIStreamENd(pavis);

    pFrame := AVIStreamGetFrameOpen(pavis, nil);
    result := assigned(pFrame);

    AVIStreamBeginStreaming(pavis, fFirstFrame, fLastFrame, 1000);

    if not result then
        closeAvi
    else
    begin
        fAviOpened := true;
        GetFrame(fFirstFrame);
        StructureChanged;
        currdelta:=0;
        CurrFrameCount:=0;
         FileOrPosChanged:=true;
    end;

end;

procedure TGLAviPlane.CloseAvi;
begin
    AVIStreamEndStreaming(pavis);
    AVIStreamGetFrameClose(pFrame);

    AVIStreamrelease(pavis);
    // avifilerelease(pavi);

    if assigned(pbmi) then
    begin
        pbmi := nil;
        pcolors := nil;
    end;
    fFirstFrame := 0;
    fFrameIndex := 0;
    flastFrame := 0;
    fAviOpened := false;
end;

procedure TGLAviPlane.BuildStripArray;
var
    r: integer;
    temp, w, h: integer;
    x, y: integer;
    i: integer;
begin
    if not assigned(pbmi) then
        exit;
    r := IntQuality;
    w := (pbmi^.biWidth div r) - 2;
    h := pbmi^.biHeight div r;
    if (w + 2) * h <> length(stripArray) then
        setlength(StripArray, (w + 2) * h);
    x := 0;
    for y := 0 to h - 1 do
    begin
        StripArray[(y * (w + 2))] := affinevectormake(0, (y) / (60 / r), 0);
        i := 0;
        for temp := 1 to w do
        begin
            x := temp;
            i := 1 - i;
            if i = 0 then
                StripArray[(y * (w + 2)) + x] := affinevectormake((x - 1) / (60 / r), y / (60 / r), 0)
            else
                StripArray[(y * (w + 2)) + x] := affinevectormake((x - 1) / (60 / r), (y + 1) / (60 / r), 0);
        end;
        StripArray[(y * (w + 2)) + x] := affinevectormake((x - 2) / (60 / r), (y + 1) / (60 / r), 0);
    end;
end;

procedure TGLAviPlane.FillColorArray;
var
    x, y, i: integer;
    rgb: PRGBTriple;
    r, rw, w, h: integer;
begin
    if not assigned(pbmi) then
        exit;
    r := IntQuality;
    rw := pbmi^.biWidth;
    w := (rw div r) - 2;
    h := pbmi^.biHeight div r;

    rgb := pcolors;
    inc(rgb, rw);
    if (w + 2) * h <> length(colorArray) then
        setlength(colorArray, (w + 2) * h);
    for y := 0 to h - 2 do
    begin
        i := 1;
        for x := w + 1 downto 0 do
        begin
            if rendermode = rmTriStripArray then
            begin
                i := 1 - i;
                if i = 1 then
                    inc(rgb, (rw * (r - 1)));
            end;
            colorArray[(y * (w + 2)) + x].X := rgb^.rgbtRed / 256;
            colorArray[(y * (w + 2)) + x].Y := rgb^.rgbtGreen / 256;
            colorArray[(y * (w + 2)) + x].Z := rgb^.rgbtBlue / 256;
            if rendermode = rmTriStripArray then
            begin
                if i = 1 then
                    dec(rgb, (rw * (r - 1)));
            end;
            inc(rgb, r);
        end;
        if r > 1 then
            inc(rgb, rw * (r - 1));
    end;

end;
initialization
    avifileinit;
finalization
    avifileExit;

end.

