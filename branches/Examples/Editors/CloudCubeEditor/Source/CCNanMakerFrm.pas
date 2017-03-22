unit CCNanMakerFrm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,//Launch browser
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.Imaging.Jpeg,
  GLVectorGeometry,  //MaxInteger
  CCEditorFrm;

type
  TNanMakerForm = class(TForm)
    Panel1: TPanel;
    SaveBtn: TSpeedButton;
    ExitBtn: TSpeedButton;
    HelpBtn: TSpeedButton;
    TrackBar1: TTrackBar;
    SizeRG: TRadioGroup;
    Image1: TImage;
    StopBtn: TSpeedButton;
    SaveDialog1: TSaveDialog;
    ClearBtn: TSpeedButton;
    CARG: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExitBtnClick(Sender: TObject);
    procedure SizeRGClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);

    procedure SaveBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);

    procedure ClearBtnClick(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure CARGClick(Sender: TObject);
procedure CAMaker;

procedure CAMaker2;
procedure CAMaker3;
procedure DLACentroid(Intensity: Integer);
procedure DLADilation;
  private
    { Private declarations }
    Running:Boolean;
    CenterX, CenterY :Integer;
  public
    { Public declarations }
  end;

var
  NanMakerForm: TNanMakerForm;

//==========================================================================
implementation
//==========================================================================

{$R *.DFM}


procedure TNanMakerForm.FormCreate(Sender: TObject);
begin
  Running:=False;
   CenterX:=64;
   CenterY:=64;
end;
//Start with a Black screen.. to make the clouds on
procedure TNanMakerForm.FormShow(Sender: TObject);
begin
  with Image1 do begin
    Canvas.Brush.Color := clBlack;// clRed;
    Canvas.Brush.Style := bsSolid;//bsDiagCross;
    //Canvas.Ellipse(0, 0, Image1.Width, Image1.Height);
    Canvas.FillRect(Rect(0,0,Image1.Width, Image1.Height));
    Picture.Bitmap.PixelFormat:=pf24bit;    
  end;
end;
procedure TNanMakerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Running:=False;
end;
procedure TNanMakerForm.ExitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TNanMakerForm.SizeRGClick(Sender: TObject);
begin
  Case SizeRG.ItemIndex of
  0: Begin Image1.Width:=64; Image1.Height:=64;
    Image1.Picture.Bitmap.Width:=64; Image1.Picture.Bitmap.Height:=64;
  End;
  1: Begin Image1.Width:=128; Image1.Height:=128;
    Image1.Picture.Bitmap.Width:=128; Image1.Picture.Bitmap.Height:=128;
  End;
  2: Begin Image1.Width:=256; Image1.Height:=256;
    Image1.Picture.Bitmap.Width:=256; Image1.Picture.Bitmap.Height:=256;
  End;
  end;
   CenterX:=Image1.Width div 2;
   CenterY:=Image1.Height div 2;
   ClearBtnClick(Self);
end;

procedure TNanMakerForm.ClearBtnClick(Sender: TObject);
begin
  with Image1 do begin
    Canvas.Brush.Color := clBlack;// clRed;
    Canvas.Brush.Style := bsSolid;//bsDiagCross;
    //Canvas.Ellipse(0, 0, Image1.Width, Image1.Height);
    Canvas.FillRect(Rect(0,0,Image1.Width, Image1.Height));
    Picture.Bitmap.PixelFormat:=pf24bit;
  end;
end;


procedure TNanMakerForm.StopBtnClick(Sender: TObject);
begin
  Running:=False;
end;


procedure TNanMakerForm.SaveBtnClick(Sender: TObject);
begin
//  Image1       is ONLY 24 bit
  SaveDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp';
  //'CA Cloud image (*.bmp)|*.bmp';
  SaveDialog1.InitialDir:=ImagePath;
  //ExtractFilePath(Application.Exename);
  SaveDialog1.DefaultExt:='bmp';
  SaveDialog1.Filename:='*.bmp' ;
  If SaveDialog1.Execute then
  Image1.picture.bitmap.SaveToFile(SaveDialog1.FileName);
end;

procedure TNanMakerForm.HelpBtnClick(Sender: TObject);
begin
ShellExecute(
Application.Handle, // handle to parent window
'open',             // pointer to string that specifies operation to perform
PChar(ExtractFilePath(Application.Exename)+'MFClouds.htm'),// pointer to filename or folder name string
'',// pointer to string that specifies executable-file parameters
                                  //+'help'
 PChar(ExtractFilePath(Application.Exename)),// pointer to string that specifies default directory
SW_SHOWNORMAL);
end;




procedure TNanMakerForm.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   CenterX:=X;
   CenterY:=Y;
end;

(*************************************************************)
procedure TNanMakerForm.CARGClick(Sender: TObject);
begin
  Running:=False;
  Application.ProcessMessages;
  Case CARG.Itemindex of
  0:CAMaker;
  1:DLACentroid(TrackBar1.Position);
  2:DLADilation;
  3:CAMaker2;
  4:CAMaker3;
  end;
end;

(*************************************************************)
procedure TNanMakerForm.CAMaker;
var //TbyterMax,
Tbyter,Tbyter2:Byte;
//Centroid,
x,y,StepSize2,StepSize:Integer;
//Xs,Ys,StepSizeS,TbyterMaxS:String;
begin
  If Running then Exit;
  Running:=True;
      CenterX :=(Image1.Width )div 2;
      CenterY :=(Image1.Height)div 2;
   StepSize2:= Image1.Width div 2;
    {with Image1.Canvas do begin
    for X := 0 to Image1.Width do
      for Y := 0 to Image1.Height do
      begin
      Centroid := random(10);
      If Centroid >2 then
      Pixels[X , Y]:= RGB(1,1,1);
      end;
    end;}     {TrackBar1.Position}
    StepSize:=2;
//    TbyterMax:=0;
  Repeat
  with Image1.Canvas do
  begin
    for X := CenterX - Random(StepSize) to CenterX + Random(StepSize) do
      for Y := CenterY - Random(StepSize) to CenterY + Random(StepSize) do
      begin
        Tbyter:=GetBValue(Pixels[X , Y]);
       if ((Tbyter>0)and(Tbyter <255)) then
       begin
       inc(Tbyter,random(TrackBar1.Position)) ;
       Pixels[X , Y]:=RGB(Tbyter,Tbyter,Tbyter);
//       TbyterMax:=Tbyter;
       end else
       if (Tbyter = 0) then Pixels[X , Y]:= RGB(1,1,1)
       else
       if (Tbyter = 255) then
       begin   Tbyter2:=random(Tbyter);
         Pixels[X , Y]:= RGB(Tbyter2,Tbyter2,Tbyter2);//(1,1,1);
       end;
      end;
//      CenterX := Random(CenterX)+Random(CenterX)-Random(CenterX);
//      CenterY := Random(CenterY)+Random(CenterY)-Random(CenterY);
    inc(StepSize,random(TrackBar1.Position));
    If StepSize > StepSize2 then StepSize:=2;
  End;
  (*    str(CenterX, Xs); str(CenterY, Ys); {str(HighestHigh,HighS);}
        str(StepSize, StepSizeS);
        str(TbyterMax, TbyterMaxS);
      CAMakerForm.Caption := 'X: ' + Xs + ', Y: ' + Ys +
        ',  StepSize: ' + StepSizeS+
        ',  TbyterMax: ' + TbyterMaxS;*)

      Application.ProcessMessages;
  Until Running = False;
end;
{Grow at center of screen... make a box and throw at center}
(*************************************************************)
procedure TNanMakerForm.CAMaker3;
var //TbyterMax,Tbyter2,
Tbyter:Byte;
//Centroid,
x,y,//StepSize2,
Dist2,Radius2,StepSize:Integer;
//Xs,Ys,StepSizeS,TbyterMaxS:String;
Begin;
  If Running then Exit;
  Running:=True;

//Make some Circles at random loacations...
  Repeat
  with Image1.Canvas do
  begin
    Radius2 := trunc(sqr(TrackBar1.Position));
    Radius2 :=Radius2 *2;
    StepSize:=TrackBar1.Position*2;
    CenterX:=Random(Image1.Width);
    CenterY:=Random(Image1.Height);    
    for X := CenterX - (StepSize) to CenterX + (StepSize) do
    begin
      for Y := CenterY - (StepSize) to CenterY + (StepSize) do
      begin
          Dist2 := trunc(sqr(CenterX-x)+sqr(CenterY-y));
          if Dist2>=Radius2 then Continue;
        Tbyter:=GetBValue(Pixels[X , Y]);
       if ((Tbyter>0)and(Tbyter <246)) then
       begin
       inc(Tbyter,random(TrackBar1.Position)) ;
//       if (Tbyter > 255) then Pixels[X , Y]:= RGB(255,255,255)else
       Pixels[X , Y]:=RGB(Tbyter,Tbyter,Tbyter);
       end else
       if (Tbyter = 0) then Pixels[X , Y]:= RGB(1,1,1);

       end;
      Application.ProcessMessages;
     end;
  end;
  Until Running = False;
End;
(*************************************************************)
procedure TNanMakerForm.CAMaker2;
var //Tbyter2, TbyterMax
Tbyter:Byte;
  gx, gy, iRadius,Radius2,Dist2 : integer;
//Centroid,
CenterX2,CenterY2,
//x,y,
StepSize2,StepSize:Integer;
//Xs,Ys,StepSizeS,TbyterMaxS,CentroidS:String;
begin
  If Running then Exit;
  Running:=True;
  StepSize2:= Image1.Width div 2;
  StepSize:=2;
//  TbyterMax:=0;
//  Centroid :=0;
      CenterX2 :=CenterX;
      CenterY2 :=CenterY;
  Repeat
  with Image1.Canvas do
  begin
//    inc(Centroid);
    Radius2 := trunc(sqr(TrackBar1.Position));
    iRadius := trunc(TrackBar1.Position);
//    for CenterX2 := 0 to Image1.Width do
//    for CenterY2 := 0 to Image1.Height do
      inc(CenterX2,random(TrackBar1.Position)) ;
      inc(CenterY2,random(TrackBar1.Position)) ;
      If ( (CenterX2>Image1.Width) or (CenterY2>Image1.Height)) then
      begin
      CenterX2 :=random(Image1.Width );
      CenterY2 :=random(Image1.Height);
      end;
    begin
      for gx := CenterX2-iRadius to CenterX2+iRadius do
      begin
        if gx<0 then continue;
        if gx>=Image1.Width then continue;

        for gy := CenterY2-iRadius to CenterY2+iRadius do
        begin
          if gy<0 then continue;
          if gy>=Image1.Height then continue;
          Dist2 := trunc(sqr(CenterX2-gx)+sqr(CenterY2-gy));
          if Dist2>=Radius2 then Continue;
          Tbyter:=GetBValue(Pixels[gX , gY]);
          if ((Tbyter>0)and(Tbyter <246)) then
          begin
            inc(Tbyter,random(TrackBar1.Position)) ;
            Pixels[gX , gY]:=RGB(Tbyter,Tbyter,Tbyter);
//            TbyterMax:=Tbyter;
          end else
          if (Tbyter = 0) then Pixels[gX , gY]:= RGB(1,1,1)
          {else
          if (Tbyter = 255) then
          begin   Tbyter2:=random(Tbyter);
            Pixels[gX , gY]:= RGB(Tbyter2,Tbyter2,Tbyter2);//(1,1,1);
          end};
        end;
      end;
    end;
    inc(StepSize,random(TrackBar1.Position));
    If StepSize > StepSize2 then StepSize:=2;
  End;
   {   str(CenterX2, Xs); str(CenterY2, Ys);
    str(Centroid,CentroidS);
    str(StepSize, StepSizeS);
    str(TbyterMax, TbyterMaxS);
    CAMakerForm.Caption := 'X: ' + Xs + ', Y: ' + Ys +
        ',  StepSize: ' + StepSizeS+
        ',  TbyterMax: ' + TbyterMaxS+
        ',  #: ' + CentroidS; }
    Application.ProcessMessages;
  Until Running = False;
end;
(*************************************************************)


procedure TNanMakerForm.DLACentroid(Intensity: Integer);
  {(Freq,F_Dim: Extended;Points,Intensity:Integer)}
var
  FBackGroundColor,TempColor: TColor;
  Contact: Boolean;
  Spacer,TempColori,
  X, Y, HighestHigh, LowestLow, RightSide, LeftSide, Stepcounter,
    ICount, CenterX, CenterY, Centroid, maxcolx, maxrowy: Integer;
  HighS, Xs, Ys: string;
begin
  If Running then Exit;
  Running:=True;
  repeat  
  with Image1.Canvas do begin
{    Brush.Color:=FBackGroundColor;
    Brush.Style:=bsSolid;
    FillRect(Rect(0,0,FYImageX,FYImageY));
    TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color:= TempColor;
    MainForm.Show;}
    {Set up bottom line glue}
    maxcolx := (10);
    maxrowy := (10);
    CenterX := (Image1.Width div 2);
    CenterY := (Image1.Height div 2);
{    HighestHigh:=(CenterY+5);
    LowestLow:=(CenterY-5);}
    HighestHigh := (CenterY - 5);
    LowestLow := (CenterY + 5);
    RightSide := (CenterX + 5);
    LeftSide := (CenterX - 5);
{Set up multi color line}
TempColori:=clwhite;//Random(255) mod 255;
FBackGroundColor:=ClBlack;
    for X := CenterX - 2 to CenterX + 2 do begin
      for Y := CenterY - 2 to CenterY + 2 do begin
        TempColor := RGB(TempColori,TempColori,TempColori);
//        ((Random(255)),(Random(255)),(Random(255)));
        //  Colors[1, (Random(255) mod 255)],
        //  Colors[2, (Random(255) mod 255)]);
        Pixels[X, Y] := TempColor;
      end;
    end;
    Randomize;
 //   bRotateImage := False; {in the Drawing...}
 //   bRotatingImage := True;
    X := CenterX;
    Y := HighestHigh;
    str(X, Xs); str(Y, Ys); str(HighestHigh, HighS);
    NanMakerForm.Caption := 'FX: ' + Xs + ', FY: ' + Ys +
      ',  High: ' + HighS;
    Application.ProcessMessages;
    repeat
      for Spacer := 1 to 100 do begin
        ICount := 0;
        Centroid := random(4);
        if Centroid = 0 then {center + (width * random + or -)}
        begin X := LeftSide;
 {    Y:=(CenterY+ ( (random(maxrowy div 2)) *(random(2) -random(2)) )  );}
          Y := (CenterY + ((random(maxrowy div 2)) - (random(maxrowy
            div 2))));

        end else
          if Centroid = 1 then {center + (width * random + or -)}
          begin X := RightSide;
{     Y:=(CenterY+ ( (random(maxrowy div 2)) *(random(2) -random(2))));}
            Y := (CenterY + ((random(maxrowy div 2)) - (random(maxrowy
              div 2))));

          end else
            if Centroid = 2 then {center + (width * random + or -)}
            begin
{     X:=(CenterX + ( (random(maxcolx div 2)) *(random(2) -random(2))));}
              X := (CenterX + ((random(maxcolx div 2)) -
                (random(maxcolx div 2))));
              Y := HighestHigh;
            end else
              if Centroid = 3 then {center + (width * random + or -)}
              begin
                X := (CenterX + ((random(maxcolx div 2)) -
                  (random(maxcolx div 2))));
                Y := LowestLow;
              end else ;{center + (width * random + or -)}
               { showmessage('oops 1');}
        Stepcounter := 0;
        Contact := True;
        while ((Contact) and (Stepcounter < 2000)) do
          begin {Drop bombers}
          inc(Stepcounter);
          case Centroid of {Keep in bounds.. always dropping}
            0: begin {Left}
                X := X + (Random(3) - 1);
                Y := Y + (Random(3) - 1); {  +(Random(2)-Random(2));}
              end;
            1: begin {Right}
                X := X + (Random(3) - 1); {  -(Random(2));}
                Y := Y + (Random(3) - 1); {  +(Random(2)-Random(2));}
              end;
            2: begin {Top}
                X := X + (Random(3) - 1);
                Y := Y + (Random(3) - 1); {+(Random(2));}
              end;
            3: begin {Bottom}
                X := X + (Random(3) - 1);
                Y := Y + (Random(3) - 1); {  -(Random(2));}
              end;
          else ;{showmessage('oops');}
          end; {of case}

          if (X < (LeftSide - 4)) then X := LeftSide;
          if (X > (RightSide + 4)) then X := RightSide;
          if (Y < (HighestHigh - 4)) then Y := HighestHigh;
          if (Y > (LowestLow + 4)) then Y := LowestLow;
{    HighestHigh:=(CenterY-5);
    LowestLow:=(CenterY+5);
    RightSide:=(CenterX+5);
    LeftSide:=(CenterX-5);}
          if (FBackGroundColor <> Pixels[X - 1, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then
            begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X - 1, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end;
            end; { else}
          if (FBackGroundColor <> Pixels[X - 1, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount = Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X - 1, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X + 1, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X + 1, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X + 1, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X + 1, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X - 1, Y]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X - 1, Y];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X + 1, Y]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X + 1, Y];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
{str(X,Xs);str(Y,Ys);  str(StepCounter,HighS);
MainForm.HiddenFX.Caption:='FX: '+Xs+', FY: '+Ys+',  Count: '+HighS;
Application.ProcessMessages;}
        end; {end of while}
      end; {of spacer}
      str(X, Xs); str(Y, Ys); {str(HighestHigh,HighS);}
        str(StepCounter, HighS);
      NanMakerForm.Caption := 'FX: ' + Xs + ', FY: ' + Ys +
        ',  High: ' + HighS;
      Application.ProcessMessages;
    until (//(Running = False) or
       (HighestHigh < 7) or (LowestLow > Image1.Height)
      or (RightSide > Image1.Width) or (LeftSide < 7));
  end;
      Application.ProcessMessages;
    until (Running = False)
end;

(*************************************************************)
procedure TNanMakerForm.DLADilation;
function SpawnBitmap(Width,Height :Integer) : TBitmap;
begin
   Result:=TBitmap.Create;
   //Result.PixelFormat:=pf32bit;
   Result.Width:=Width;
   Result.Height:=Height;
  with Result do begin
    Canvas.Brush.Color := clBlack;// clRed;
    Canvas.Brush.Style := bsSolid;//bsDiagCross;
    Canvas.FillRect(Rect(0,0,Result.Width, Result.Height));
    Result.PixelFormat:=pf24bit;
  end;
end;
var
StepCounter,
   x, y, sx, sy : Integer;
   bmp//, bmAlpha
   : TBitmap;
   HighS:String;
//   maxNeighbour,
   Temp    : Integer;
begin
  If Running then Exit;
  Running:=True;
  StepCounter:=0;
  // make fully transparent all pixels in the color for all pixels in the alpha map
   // that are adjacent to a fully transparent one
  //   bmAlpha:=IMAlpha.Picture.Bitmap;
  repeat
  begin
    sx := (Image1.Width);
    sy := (Image1.Height );
   bmp:=SpawnBitmap(sx,sy);
   bmp.assign(Image1.picture.bitmap);
//   for y:=0 to sy-1 do
   begin
 //     for x:=0 to sx-1 do with Image1.Canvas do
with Image1.Canvas do      begin
{         if Pixels[x, y]<clWhite then
         begin                     //MaxIntValue   MaxInteger
            maxNeighbour:=MaxInteger(MaxInteger(Pixels[x, y-1], Pixels[x, y+1]),
                                     MaxInteger(Pixels[x-1, y], Pixels[x+1, y]));
            bmp.Canvas.Pixels[x, y]:=MaxInteger(maxNeighbour, Pixels[x, y]);
         end;// else//  bmp.Canvas.Pixels[x, y]:=clWhite;}
      x :=random(Image1.Width );
      y :=random(Image1.Height);
         if Pixels[x, y]> clBlack then
         begin
           bmp.Canvas.Pixels[x, y]:=Pixels[x, y];//clWhite;
           Temp:=GetRValue(Pixels[x, y-1])+1;If Temp>255 then Temp:=255;
           bmp.Canvas.Pixels[x, y-1]:=RGB(Temp,Temp,Temp);
           Temp:=GetRValue(Pixels[x, y+1])+1;If Temp>255 then Temp:=255;
           bmp.Canvas.Pixels[x, y+1]:=RGB(Temp,Temp,Temp);
           Temp:=GetRValue(Pixels[x+1, y])+1;If Temp>255 then Temp:=255;
           bmp.Canvas.Pixels[x+1, y]:=RGB(Temp,Temp,Temp);
           Temp:=GetRValue(Pixels[x-1, y])+1;If Temp>255 then Temp:=255;
           bmp.Canvas.Pixels[x-1, y]:=RGB(Temp,Temp,Temp);
           {
           Pixels[x-1, y]
           Pixels[x-1, y-1]
           Pixels[x-1, y+1]
           Pixels[x+1, y]
           Pixels[x+1, y-1]
           Pixels[x+1, y+1]}
         end;
      end;
   end;
   Image1.Picture.Bitmap:=bmp;
   bmp.Free;
  // TextureChanged;
      inc(StepCounter);
      str(StepCounter, HighS);
      NanMakerForm.Caption := HighS;
      Application.ProcessMessages;
    end;
    until (Running = False);
end;

(*************************************************************)

(*************************************************************)
(*************************************************************)








end.
