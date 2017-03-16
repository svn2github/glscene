unit nULifeAlien;

interface
uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ColorGrd, Mask, 
  Buttons, Menus, ComCtrls, ExtCtrls;



procedure DirectedDo(FileTodo: string);
procedure FredkinDo(FileTodo: string);
procedure InvasionDo(FileTodo: string);
procedure kinDo(FileTodo: string);
procedure DirectedDoColor(FileTodo: string);
procedure FredkinDoColor(FileTodo: string);
procedure InvasionDoColor(FileTodo: string);
procedure kinDoColor(FileTodo: string);

procedure BriansBrainDo(FileTodo: string);
procedure StepChildDo(FileTodo: string);
procedure BriansBrainDoColor(FileTodo: string);
procedure StepChildDoColor(FileTodo: string);
procedure SchellingDo(FileTodo: string);
procedure SchellingDoColor(FileTodo: string);
procedure SporadicDoColor(FileTodo: string);
procedure SporadicDo(FileTodo: string);
procedure HodgePodgeDo(FileTodo: string);
procedure HodgePodgeDoColor(FileTodo: string);



implementation
uses nUGlobal, {UEditorForm,UColorsForm,} ncLife;
(********************************************************************)
(********************************************************************)

procedure FredkinDo(FileTodo: string); {80}
var
  fil: file of Byte {Array7848};
  NewRect: TRect;
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, RanLife,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array7848;}
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
  SetLength(TV7848, 78, 48);


  LifeForm.WindowState := wsNormal; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 391; {391 781}
  LifeForm.Image1.Height := 241; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 391; {640}
  Bitmap.Height := 241; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    RanLife := random(10);
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    if (RanLife < 2) then begin
      Image[20, 20] := 1;
    end else if (RanLife < 4) then begin
      Image[40, 20] := 1;
      Image[40, 21] := 1;
      Image[40, 22] := 1;
      Image[40, 23] := 1;
    end
    else if (RanLife < 8) then begin
      Image[60, 20] := 1;
      Image[60, 21] := 1;
      Image[61, 20] := 1;
    end
    else begin
      for I := 0 to 77 do
        for J := 0 to 47 do begin
          Image[I, J] := Trunc(Random(2));
        end;
    end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do
  begin
    for J := 1 to 46 do
      if (TV7848[I, J] = 1) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
  end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 46 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I + 47, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 77; for J := 1 to 46 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 76 do begin
   {J-1} C := Aa[I, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; for I := 1 to 76 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 47];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 77; begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 47];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 0; begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 47];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I, J + 47];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 77; begin
 {J-1} C := Aa[I, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 77 do
      begin
        for J := 0 to 47 do
          if (Aa[I, J] <> TV7848[I, J]) then
          begin
            if TV7848[I, J] = 1 then
              LifeForm.Image1.Canvas.Brush.Color := Full
                {CurrentColor}
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then
            begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
            if (bSaveThatThing = true) then
            begin
              bSaveThatThing:=False;
    LifeForm.SaveDialog1.filter := 'Life files|*.bmp';
    LifeForm.SaveDialog1.DefaultExt := 'bmp';
    LifeForm.SaveDialog1.filename := 'NewLife.bmp';
    if LifeForm.SaveDialog1.execute then begin
      if (not (LifeForm.SaveDialog1.filename = '')) then begin
  {SAVe the bitmap}
        LifeForm.Image1.Picture.SaveToFile(LifeForm.SaveDialog1.filename)
      end; end
            end;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);


  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure Fredkin}
(********************************************************************)

(********************************************************************)
(********************************************************************)

procedure FredkinDoColor(FileTodo: string);
var
  fil: file of Byte {Array7848};
  NewRect: TRect;
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, RanLife,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array7848;}
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
  SetLength(TV, 78, 48);

  LifeForm.WindowState := wsMaximized; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 781; {391 781}
  LifeForm.Image1.Height := 481; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {640}
  Bitmap.Height := 481; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    RanLife := random(10);
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    if (RanLife < 2) then begin
      Image[20, 20] := 1;
    end else if (RanLife < 4) then begin
      Image[40, 20] := 1;
      Image[40, 21] := 1;
      Image[40, 22] := 1;
      Image[40, 23] := 1;
    end
    else if (RanLife < 8) then begin
      Image[60, 20] := 1;
      Image[60, 21] := 1;
      Image[61, 20] := 1;
    end
    else begin
      for I := 0 to 77 do
        for J := 0 to 47 do begin
          Image[I, J] := Trunc(Random(2));
        end;
    end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do
  begin
    for J := 1 to 46 do
      if (TV7848[I, J] = 1) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) + 10),
          ((J * 10) + 10));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) + 10),
          ((J * 10) + 10));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
  end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 46 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I + 47, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 77; for J := 1 to 46 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 76 do begin
   {J-1} C := Aa[I, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; for I := 1 to 76 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 47];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 77; begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 47];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 0; begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 47];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I, J + 47];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 77; begin
 {J-1} C := Aa[I, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 77 do
      begin
        for J := 0 to 47 do
          if (Aa[I, J] <> TV7848[I, J]) then
          begin
            if TV7848[I, J] = 1 then
              LifeForm.Image1.Canvas.Brush.Color := Full
                {CurrentColor}
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);



  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure Fredkin Color}
(********************************************************************)

(********************************************************************)

procedure DirectedDoColor(FileTodo: string);
var
  fil: file of Byte {Array7848};
  NewRect: TRect;
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, RanLife,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array7848;}
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
  SetLength(TV7848, 78, 48);

  LifeForm.WindowState := wsMaximized; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 781; {391 781}
  LifeForm.Image1.Height := 481; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {640}
  Bitmap.Height := 481; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    RanLife := random(10);
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    if (RanLife < 2) then begin
      Image[20, 20] := 1;
    end else if (RanLife < 4) then begin
      Image[40, 20] := 1;
      Image[40, 21] := 1;
      Image[40, 22] := 1;
      Image[40, 23] := 1;
    end
    else if (RanLife < 8) then begin
      Image[60, 20] := 1;
      Image[60, 21] := 1;
      Image[61, 20] := 1;
    end
    else begin
      for I := 0 to 77 do
        for J := 0 to 47 do begin
          Image[I, J] := Trunc(Random(2));
        end;
    end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do
  begin
    for J := 1 to 46 do
      if (TV7848[I, J] = 1) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) + 10),
          ((J * 10) + 10));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) + 10),
          ((J * 10) + 10));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
  end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
   {J-1} C := Aa[I - 1, J - 1];
   {J} C := C + Aa[I - 1, J + 1] + Aa[I + 1, J + 1];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 1, J - 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 46 do begin
   {J-1} C := Aa[I + 47, J - 1];
   {J} C := C + Aa[I + 47, J + 1] + Aa[I + 1, J + 1];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 1, J - 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 77; for J := 1 to 46 do begin
   {J-1} C := Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J + 1] + Aa[I - 77, J + 1];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 76 do begin
   {J-1} C := Aa[I + 1, J + 47];
   {J} C := C + Aa[I - 1, J + 1] + Aa[I + 1, J + 1];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 47];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; for I := 1 to 76 do begin
   {J-1} C := Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J - 47] + Aa[I + 1, J - 47];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 77; begin
   {J-1} C := Aa[I - 1, J - 1];
   {J} C := C + Aa[I - 1, J - 47] + Aa[I - 77, J - 47];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 77, J - 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 0; begin
   {J-1} C := Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 77, J - 47] + Aa[I + 1, J - 47];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J - 1];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I + 77, J + 47];
   {J} C := C + Aa[I + 77, J + 1] + Aa[I + 1, J + 1];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I + 1, J + 47];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 77; begin
 {J-1} C := Aa[I - 1, J + 47];
   {J} C := C + Aa[I - 1, J + 1] + Aa[I - 77, J + 1];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I - 77, J + 47];
          if (C = 1) or (C = 3) then begin TV7848[I, J] := 1; end
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 77 do
      begin
        for J := 0 to 47 do
          if (Aa[I, J] <> TV7848[I, J]) then
          begin
            if TV7848[I, J] = 1 then
              LifeForm.Image1.Canvas.Brush.Color := Full
                {CurrentColor}
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);


  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure Directed Color}
(********************************************************************)

(********************************************************************)

procedure DirectedDo(FileTodo: string);
var
  fil: file of Byte {Array7848};
  NewRect: TRect;
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, RanLife,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array7848;}
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
  SetLength(TV7848, 78, 48);

  LifeForm.WindowState := wsNormal; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 391; {391 781}
  LifeForm.Image1.Height := 241; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 391; {640}
  Bitmap.Height := 241; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    RanLife := random(10);
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    if (RanLife < 1) then begin
      Image[20, 20] := 1;
    end else if (RanLife < 2) then begin
      Image[40, 20] := 1;
      Image[40, 21] := 1;
      Image[40, 22] := 1;
      Image[40, 23] := 1;
    end
    else if (RanLife < 3) then begin
      Image[60, 20] := 1;
      Image[60, 21] := 1;
      Image[61, 20] := 1;
    end
    else begin
      for I := 0 to 77 do
        for J := 0 to 47 do begin
          RanLife := random(10);
          if (RanLife < 2) then Image[I, J] := Trunc(Random(2));
        end;
    end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do
  begin
    for J := 1 to 46 do
      if (TV7848[I, J] = 1) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
  end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
          if (Aa[I - 1, J] = 1) then TV7848[I, J] := 1
          else if (Aa[I + 1, J] = 1) then TV7848[I, J] := 1
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 46 do begin
          if (Aa[I + 47, J] = 1) then TV7848[I, J] := 1
          else if (Aa[I + 1, J] = 1) then TV7848[I, J] := 1
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 77; for J := 1 to 46 do begin
          if (Aa[I - 1, J] = 1) then TV7848[I, J] := 1
          else if (Aa[I - 47, J] = 1) then TV7848[I, J] := 1
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 76 do begin
          if (Aa[I - 1, J] = 1) then TV7848[I, J] := 1
          else if (Aa[I + 1, J] = 1) then TV7848[I, J] := 1
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; for I := 1 to 76 do begin
          if (Aa[I - 1, J] = 1) then TV7848[I, J] := 1
          else if (Aa[I + 1, J] = 1) then TV7848[I, J] := 1
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 77; begin
          if (Aa[I - 1, J] = 1) then TV7848[I, J] := 1
          else if (Aa[I - 77, J] = 1) then TV7848[I, J] := 1
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 0; begin
          if (Aa[I + 77, J] = 1) then TV7848[I, J] := 1
          else if (Aa[I + 1, J] = 1) then TV7848[I, J] := 1
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
          if (Aa[I + 77, J] = 1) then TV7848[I, J] := 1
          else if (Aa[I + 1, J] = 1) then TV7848[I, J] := 1
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 77; begin
          if (Aa[I - 1, J] = 1) then TV7848[I, J] := 1
          else if (Aa[I - 77, J] = 1) then TV7848[I, J] := 1
          else TV7848[I, J] := 0;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Direct Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 77 do
      begin
        for J := 0 to 47 do
          if (Aa[I, J] <> TV7848[I, J]) then
          begin
            if TV7848[I, J] = 1 then
              LifeForm.Image1.Canvas.Brush.Color := Full
                {CurrentColor}
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure Directed}
(********************************************************************)


(********************************************************************)
(********************************************************************)
 {Cellular Invasion starts at top left and}
{If (random > 0.5) then boogers}

procedure InvasionDo(FileTodo: string);
var
  fil: file of Byte {Array7848};
  NewRect: TRect;
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, RanLife, XI, YI,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array7848;}
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
  SetLength(TV7848, 78, 48);

  LifeForm.WindowState := wsNormal; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 391; {391 781}
  LifeForm.Image1.Height := 241; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 391; {640}
  Bitmap.Height := 241; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;

  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    Image[1, 1] := 1;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);

{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do
  begin
    for J := 1 to 46 do
      if (TV7848[I, J] = 1) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
  end;
  if bColorLife then begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] <> 1 then TV7848[I, J] := 2; end; end;
  end;
  Cycle := 0;
  repeat {2}  begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
      TV7848[1, 1] := 1;
{AA := TV7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
{    IF (Aa[I , J] = 1) THEN TV7848[I,J] := 1 ELSE TV7848[I,J] := 0;}
          if (Aa[I, J] = 1) then begin
            XI := Random(3) - 1;
            YI := Random(3) - 1;
            TV7848[I + XI, J + YI] := 1;
            if bColorLife then begin
              XI := Random(3) + 2;
              YI := Random(3) + 2;
              if ((I < 70) and (J < 40)) then
                TV7848[I + XI, J + YI] := 2;
            end;
          end else begin
            if bColorLife then begin
              XI := Random(4) + 2;
              YI := Random(4) + 2;
              if ((I < 70) and (J < 40)) then
                TV7848[I + XI, J + YI] := 2;
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Direct Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 77 do begin
        for J := 0 to 47 do
{    IF (Aa[I, J] <> TV7848[I, J]) THEN}
        begin
          if TV7848[I, J] = 1 then
            LifeForm.Image1.Canvas.Brush.Color := Full {CurrentColor}
          else if TV7848[I, J] = 2 then
            LifeForm.Image1.Canvas.Brush.Color := RGB(Random(255),
              Random(255), Random(255))
          else LifeForm.Image1.Canvas.Brush.Color := Desolate;
          NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
            * 5) + 5));
          LifeForm.Image1.Canvas.FillRect(NewRect);
          if (bSlowDown = true) then begin
            Application.ProcessMessages;
            Sleep(NapTime); {MsgWaitForMultipleObjects}
          end;
        end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          Application.ProcessMessages;
{      bIamDone:=True};
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure Invasion}
(********************************************************************)
(********************************************************************)
{Cellular Invasion starts at top left and}
{If (random > 0.5) then boogers}

procedure InvasionDoColor(FileTodo: string);
var
  fil: file of Byte {Array15696};
  NewRect: TRect;
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, RanLife, XI, YI,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array15696;}
  SetLength(Aa, 156, 96);
  SetLength(Image, 156, 96);
  SetLength(TV15696, 156, 96);

  LifeForm.WindowState := wsMaximized; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 781; {391 781}
  LifeForm.Image1.Height := 481; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {640}
  Bitmap.Height := 481; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;

  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := 0; end; end;
    Image[1, 1] := 1;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );   }
      for I := 0 to 155 do for J := 0 to 95 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);

{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 155 do begin for J := 0 to 95 do
    begin TV15696[I, J] := 0; end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 154 do begin for J := 1 to 94 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 154 do
  begin
    for J := 1 to 94 do
      if (TV15696[I, J] = 1) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
  end;
  if bColorLife then begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin if Image[I, J] <> 1 then TV15696[I, J] := 2; end; end;
  end;
  Cycle := 0;
  repeat {2}  begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
      TV15696[1, 1] := 1;
{AA := TV15696;}
      for I := 0 to 155 do for J := 0 to 95 do
        AA[I, J] := TV15696[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 154 do begin
        for J := 1 to 94 do begin
{    IF (Aa[I , J] = 1) THEN TV7848[I,J] := 1 ELSE TV7848[I,J] := 0;}
          if (Aa[I, J] = 1) then begin
            XI := Random(3) - 1;
            YI := Random(3) - 1;
            TV15696[I + XI, J + YI] := 1;
            if bColorLife then begin
              XI := Random(3) + 2;
              YI := Random(3) + 2;
              TV15696[I + XI, J + YI] := 2; end;
          end else begin
            if bColorLife then begin
              XI := Random(4) + 2;
              YI := Random(4) + 2;
              TV15696[I + XI, J + YI] := 2; end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Direct Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 155 do begin
        for J := 0 to 95 do
{    IF (Aa[I, J] <> TV7848[I, J]) THEN}
        begin
          if TV15696[I, J] = 1 then
            LifeForm.Image1.Canvas.Brush.Color := Full {CurrentColor}
          else if TV15696[I, J] = 2 then
            LifeForm.Image1.Canvas.Brush.Color := RGB(Random(255),
              Random(255), Random(255))
          else LifeForm.Image1.Canvas.Brush.Color := Desolate;
          NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
            * 5) + 5));
          LifeForm.Image1.Canvas.FillRect(NewRect);
          if (bSlowDown = true) then begin
            Application.ProcessMessages;
            Sleep(NapTime); {MsgWaitForMultipleObjects}
          end;
        end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          Application.ProcessMessages;
{      bIamDone:=True};
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure Invasion}
(********************************************************************)

(********************************************************************)
(********************************************************************)

procedure kinDoColor(FileTodo: string);
var
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  NapTime, ErrorCode, RanLife,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  fil: file of Byte {Array15696};
  fil78: file of Byte {Array7848};
  NewRect: TRect;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array15696;Image78 : Array7848;}
  SetLength(Aa, 156, 96);
  SetLength(Image, 156, 96);
  SetLength(Image78, 78, 48);
  SetLength(TV15696, 156, 96);


  LifeForm.WindowState := wsMaximized; {wsNormal}
  LifeForm.Image1.Width := 781; {391}
  LifeForm.Image1.Height := 481; {241}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {391}
  Bitmap.Height := 481; {241}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  for I := 0 to 155 do begin for J := 0 to 95 do
    begin TV15696[I, J] := 0; end; end;
  for I := 0 to 155 do begin for J := 0 to 95 do
    begin Image[I, J] := 0; end; end;

  if FileTodo = 'Random' then
  begin
    Randomize;
    RanLife := random(10);
    if (RanLife < 2) then begin
      Image[20, 20] := 1;
    end else if (RanLife < 4) then begin
      Image[40, 40] := 1;
      Image[40, 41] := 1;
      Image[40, 42] := 1;
      Image[40, 43] := 1;
    end
    else if (RanLife < 8) then begin
      Image[80, 20] := 1;
      Image[80, 21] := 1;
      Image[81, 20] := 1;
    end
    else begin
      for I := 0 to 155 do
        for J := 0 to 95 do begin
          Image[I, J] := Trunc(Random(2));
        end;
    end;

  end else begin
    Name := ExtractFileExt(FileTodo); Name := UpperCase(Name);
    if (Name = '.LMF') then begin
      for I := 0 to 77 do begin for J := 0 to 47 do
        begin Image[I, J] := 0; end; end;
      AssignFile(fil78, FileTodo);
{$I-}Reset(fil78); {$I+}
      i := IOresult;
      if i <> 0 then EXIT {ReportError( i )} else
      begin
{         read( fil78, Image78 );}
        for I := 0 to 77 do for J := 0 to 47 do
          begin
            read(fil, ImageByte);
            Image78[I, J] := ImageByte;
          end;
        CloseFile(fil78);
      end;
      for I := 0 to 77 do begin for J := 0 to 47 do begin
          TV15696[I + 39, J + 24] := Image78[I, J]; end; end;
      for I := 0 to 155 do begin for J := 0 to 95 do begin
          Image[I, J] := TV15696[I, J]; end; end;
    end else if (Name = '.LRF') then begin
      for I := 0 to 155 do begin
        for J := 0 to 95 do begin
          Image[I, J] := 0; end; end;
      AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
      i := IOresult;
      if i <> 0 then EXIT {ReportError( i )} else
      begin
{         read( fil, Image );}
        for I := 0 to 155 do for J := 0 to 95 do
          begin
            read(fil, ImageByte);
            Image[I, J] := ImageByte;
          end;
        CloseFile(fil);
      end;
    end else begin
      Randomize;
      for I := 0 to 155 do begin
        for J := 0 to 95 do begin
          Image[I, J] := Trunc(Random(2));
        end; end;
      MessageDlg('Mistake getting Pixel life', mtInformation, [mbOk],
        0);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 155 do begin for J := 0 to 95 do
    begin TV15696[I, J] := 0; end; end;

{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 154 do begin for J := 1 to 94 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := TV15696[I, J]; end; end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
  end;

  for I := 1 to 154 do begin
    for J := 1 to 94 do begin
      if (TV15696[I, J] = 1) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
    end; end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV15696;}
      for I := 0 to 155 do for J := 0 to 95 do
        AA[I, J] := TV15696[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 154 do begin
        for J := 1 to 94 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if C < 1 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Desolate;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            LifeForm.Image1.Canvas.Brush.Color := Growing;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if ((C = 3) or (C = 1)) then begin TV15696[I, J] := 1;
            LifeForm.Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Overpopulated;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 94 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I + 95, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if C < 1 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Desolate;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            LifeForm.Image1.Canvas.Brush.Color := Growing;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if ((C = 3) or (C = 1)) then begin TV15696[I, J] := 1;
            LifeForm.Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Overpopulated;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 155; for J := 1 to 94 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if C < 1 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Desolate;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            LifeForm.Image1.Canvas.Brush.Color := Growing;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if ((C = 3) or (C = 1)) then begin TV15696[I, J] := 1;
            LifeForm.Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Overpopulated;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 154 do begin
   {J-1} C := Aa[I, J + 95];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if C < 1 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Desolate;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            LifeForm.Image1.Canvas.Brush.Color := Growing;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if ((C = 3) or (C = 1)) then begin TV15696[I, J] := 1;
            LifeForm.Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Overpopulated;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;


{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; for I := 1 to 154 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 95];
          if C < 1 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Desolate;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            LifeForm.Image1.Canvas.Brush.Color := Growing;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if ((C = 3) or (C = 1)) then begin TV15696[I, J] := 1;
            LifeForm.Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Overpopulated;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;


{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; I := 155; begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 47];
          if C < 1 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Desolate;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            LifeForm.Image1.Canvas.Brush.Color := Growing;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if ((C = 3) or (C = 1)) then begin TV15696[I, J] := 1;
            LifeForm.Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Overpopulated;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
  {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; I := 0; begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I + 155, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 95];
          if C < 1 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Desolate;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            LifeForm.Image1.Canvas.Brush.Color := Growing;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if ((C = 3) or (C = 1)) then begin TV15696[I, J] := 1;
            LifeForm.Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Overpopulated;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
  {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I, J + 95];
   {J} C := C + Aa[I + 155, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I, J + 1];
          if C < 1 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Desolate;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            LifeForm.Image1.Canvas.Brush.Color := Growing;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if ((C = 3) or (C = 1)) then begin TV15696[I, J] := 1;
            LifeForm.Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Overpopulated;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 155; begin
 {J-1} C := Aa[I, J + 95];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I, J + 1];
          if C < 1 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Desolate;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            LifeForm.Image1.Canvas.Brush.Color := Growing;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if ((C = 3) or (C = 1)) then begin TV15696[I, J] := 1;
            LifeForm.Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            LifeForm.Image1.Canvas.Brush.Color := Overpopulated;
              {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure kinColor }
(********************************************************************)

(********************************************************************)

procedure kinDo(FileTodo: string);
var
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  NapTime, ErrorCode, RanLife,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  fil: file of Byte {Array15696};
  fil78: file of Byte {Array7848};
  NewRect: TRect;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array15696;Image78 : Array7848;}
  SetLength(Aa, 156, 96);
  SetLength(Image, 156, 96);
  SetLength(Image78, 78, 48);
  SetLength(TV15696, 156, 96);

  LifeForm.WindowState := wsMaximized; {wsNormal}
  LifeForm.Image1.Width := 781; {391}
  LifeForm.Image1.Height := 481; {241}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {391}
  Bitmap.Height := 481; {241}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  for I := 0 to 155 do begin for J := 0 to 95 do
    begin TV15696[I, J] := 0; end; end;
  for I := 0 to 155 do begin for J := 0 to 95 do
    begin Image[I, J] := 0; end; end;

  if FileTodo = 'Random' then
  begin
    Randomize;
    RanLife := random(10);
    if (RanLife < 2) then begin
      Image[20, 20] := 1;
    end else if (RanLife < 4) then begin
      Image[40, 40] := 1;
      Image[40, 41] := 1;
      Image[40, 42] := 1;
      Image[40, 43] := 1;
    end
    else if (RanLife < 8) then begin
      Image[80, 20] := 1;
      Image[80, 21] := 1;
      Image[81, 20] := 1;
    end
    else begin
      for I := 0 to 155 do
        for J := 0 to 95 do begin
          Image[I, J] := Trunc(Random(2));
        end;
    end;

  end else begin
    Name := ExtractFileExt(FileTodo); Name := UpperCase(Name);
    if (Name = '.LMF') then begin
      for I := 0 to 77 do begin for J := 0 to 47 do
        begin Image[I, J] := 0; end; end;
      AssignFile(fil78, FileTodo);
{$I-}Reset(fil78); {$I+}
      i := IOresult;
      if i <> 0 then EXIT {ReportError( i )} else
      begin
{         read( fil78, Image78 );}
        for I := 0 to 77 do for J := 0 to 47 do
          begin
            read(fil, ImageByte);
            Image78[I, J] := ImageByte;
          end;
        CloseFile(fil78);
      end;
      for I := 0 to 77 do begin for J := 0 to 47 do begin
          TV15696[I + 39, J + 24] := Image78[I, J]; end; end;
      for I := 0 to 155 do begin for J := 0 to 95 do begin
          Image[I, J] := TV15696[I, J]; end; end;
    end else if (Name = '.LRF') then begin
      for I := 0 to 155 do begin
        for J := 0 to 95 do begin
          Image[I, J] := 0; end; end;
      AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
      i := IOresult;
      if i <> 0 then EXIT {ReportError( i )} else
      begin
{         read( fil, Image );}
        for I := 0 to 155 do for J := 0 to 95 do
          begin
            read(fil, ImageByte);
            Image[I, J] := ImageByte;
          end;
        CloseFile(fil);
      end;
    end else begin
      Randomize;
      for I := 0 to 155 do begin
        for J := 0 to 95 do begin
          Image[I, J] := Trunc(Random(2));
        end; end;
      MessageDlg('Mistake getting Pixel life', mtInformation, [mbOk],
        0);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 155 do begin for J := 0 to 95 do
    begin TV15696[I, J] := 0; end; end;

{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 154 do begin for J := 1 to 94 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := TV15696[I, J]; end; end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
  end;

  for I := 1 to 154 do begin
    for J := 1 to 94 do begin
      if (TV15696[I, J] = 1) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
    end; end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV15696;}
      for I := 0 to 155 do for J := 0 to 95 do
        AA[I, J] := TV15696[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 154 do begin
        for J := 1 to 94 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV15696[I, J] := 1; end
          else TV15696[I, J] := 0;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 94 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I + 95, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV15696[I, J] := 1; end
          else TV15696[I, J] := 0;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 155; for J := 1 to 94 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV15696[I, J] := 1; end
          else TV15696[I, J] := 0;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 154 do begin
   {J-1} C := Aa[I, J + 95];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV15696[I, J] := 1; end
          else TV15696[I, J] := 0;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; for I := 1 to 154 do begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 95];
          if (C = 1) or (C = 3) then begin TV15696[I, J] := 1; end
          else TV15696[I, J] := 0;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; I := 155; begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 47];
          if (C = 1) or (C = 3) then begin TV15696[I, J] := 1; end
          else TV15696[I, J] := 0;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; I := 0; begin
   {J-1} C := Aa[I, J - 1];
   {J} C := C + Aa[I + 155, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I, J - 95];
          if (C = 1) or (C = 3) then begin TV15696[I, J] := 1; end
          else TV15696[I, J] := 0;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I, J + 95];
   {J} C := C + Aa[I + 155, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV15696[I, J] := 1; end
          else TV15696[I, J] := 0;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 155; begin
 {J-1} C := Aa[I, J + 95];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I, J + 1];
          if (C = 1) or (C = 3) then begin TV15696[I, J] := 1; end
          else TV15696[I, J] := 0;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 155 do
      begin
        for J := 0 to 95 do
          if (Aa[I, J] <> TV15696[I, J]) then
          begin
            if TV15696[I, J] = 1 then
              LifeForm.Image1.Canvas.Brush.Color := Full
                {CurrentColor}
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure kin do }
(********************************************************************)
(********************************************************************)




(********************************************************************)
{This page uses rules very similar to the Seeds rules.
As with Seeds: If a square is on, it turns off.
If a square is off,
it turns on if exactly two neighboring squares are on.
But there is one small twist:
When a square turns off,
it can't turn on in the very next iteration.
Squares in this in-between state are colored red.}
{Normal does SS, Color does Full S}
(********************************************************************)

procedure BriansBrainDo(FileTodo: string);
var
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  NapTime, ErrorCode,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, K, L, M, Hamming, X, Y, Cycle, TVC:
      integer;
  fil: file of Byte {Array7848};
  NewRect: TRect;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array7848;}
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
{SetLength(Image78,78,48);}
  SetLength(TV15696, 78, 48);

  LifeForm.WindowState := wsNormal; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 391; {391 781}
  LifeForm.Image1.Height := 241; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 391; {640}
  Bitmap.Height := 241; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    K := Random(67) + 5; L := Random(37) + 5; M := Random(10);
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    for I := K to K + M do
    begin
      for J := L to L + M do
      begin
        Image[I, J] := 1; {Random(30)-21;}
      end;
    end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;

{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] > 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] > 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do
  begin
    for J := 1 to 46 do begin
      if (TV7848[I, J] = 1) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
    end;
  end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
          if Aa[I, J] = 10 then TV7848[I, J] := 0 else
            if Aa[I, J] = 1 then TV7848[I, J] := 10 else
            begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
              if ((C = 2) or (C = 20) or (C = 11)) then
                TV7848[I, J] := 1;
            end;

          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 46 do begin
          if Aa[I, J] = 10 then TV7848[I, J] := 0 else
            if Aa[I, J] = 1 then TV7848[I, J] := 10 else
            begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 47, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
              if ((C = 2) or (C = 20) or (C = 11)) then
                TV7848[I, J] := 1;
            end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 77; for J := 1 to 46 do begin
          if Aa[I, J] = 10 then TV7848[I, J] := 0 else
            if Aa[I, J] = 1 then TV7848[I, J] := 10 else
            begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J +
     1];
              if ((C = 2) or (C = 20) or (C = 11)) then
                TV7848[I, J] := 1;
            end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 76 do begin
          if Aa[I, J] = 10 then TV7848[I, J] := 0 else
            if Aa[I, J] = 1 then TV7848[I, J] := 10 else
            begin
   {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
              if ((C = 2) or (C = 20) or (C = 11)) then
                TV7848[I, J] := 1;
            end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; for I := 1 to 76 do begin
          if Aa[I, J] = 10 then TV7848[I, J] := 0 else
            if Aa[I, J] = 1 then TV7848[I, J] := 10 else
            begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
              if ((C = 2) or (C = 20) or (C = 11)) then
                TV7848[I, J] := 1;
            end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 77; begin
          if Aa[I, J] = 10 then TV7848[I, J] := 0 else
            if Aa[I, J] = 1 then TV7848[I, J] := 10 else
            begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I - 77, J -
     47];
              if ((C = 2) or (C = 20) or (C = 11)) then
                TV7848[I, J] := 1;
            end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 0; begin
          if Aa[I, J] = 10 then TV7848[I, J] := 0 else
            if Aa[I, J] = 1 then TV7848[I, J] := 10 else
            begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
              if ((C = 2) or (C = 20) or (C = 11)) then
                TV7848[I, J] := 1;
            end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
          if Aa[I, J] = 10 then TV7848[I, J] := 0 else
            if Aa[I, J] = 1 then TV7848[I, J] := 10 else
            begin
 {J-1} C := Aa[I + 77, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
              if ((C = 2) or (C = 20) or (C = 11)) then
                TV7848[I, J] := 1;
            end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 77;
        if Aa[I, J] = 10 then TV7848[I, J] := 0 else
          if Aa[I, J] = 1 then TV7848[I, J] := 10 else
          begin
 {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I - 77, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J + 1];
            if ((C = 2) or (C = 20) or (C = 11)) then
              TV7848[I, J] := 1;
          end;
        if not (Aa[I, J] = TV7848[I, J]) then
          HAMMING := (HAMMING + 1);
        if TV7848[I, J] = 1 then CellCount := CellCount + 1;

 {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
{Hs := concat('Hamming # ',Hs);
Cs := concat('Cells   : ',Cs);
 s := concat('Cycle # ',s);   }

      LifeForm.Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 77 do
      begin
        for J := 0 to 47 do
          if (Aa[I, J] <> TV7848[I, J]) then
          begin
            if TV7848[I, J] = 1 then
              LifeForm.Image1.Canvas.Brush.Color := Full
                {CurrentColor}
            else if TV7848[I, J] = 10 then
              LifeForm.Image1.Canvas.Brush.Color := OverPopulated
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure BriansBrainDo}
(********************************************************************)

procedure BriansBrainDoColor(FileTodo: string);
var
  fil: file of Byte {Array15696};
  NewRect: TRect;
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, RanLife, XI, YI,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, K, L, M, Hamming, X, Y, Cycle, TVC:
      integer;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array15696;}
  SetLength(Aa, 156, 96);
  SetLength(Image, 156, 96);
  SetLength(Image78, 78, 48);
  SetLength(TV15696, 156, 96);

  LifeForm.WindowState := wsMaximized; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 781; {391 781}
  LifeForm.Image1.Height := 481; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {640}
  Bitmap.Height := 481; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1;
  LastCellCount := 1;
  OldHamming := 1;
  OldCellCount := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;

  if FileTodo = 'Random' then
  begin
    Randomize;
    K := Random(145) + 5; L := Random(85) + 5; M := Random(10);
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := 0; end; end;
    for I := K to K + M do begin for J := L to L + M do begin
        Image[I, J] := 1; end; end;
{ FOR I := 1 TO 154 DO begin   FOR J := 1 TO 94 DO
    begin    Image[I,J] := Random(30) -20;    end;   end;}
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image ); }
      for I := 0 to 155 do for J := 0 to 95 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 155 do
  begin for J := 0 to 95 do
    begin TV15696[I, J] := 0;
    end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 154 do begin for J := 1 to 94 do
      begin if Image[I, J] > 0 then TV15696[I, J] := 1; end; end;
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := TV15696[I, J]; end; end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin if Image[I, J] > 0 then
      TV15696[I, J] := Image[I, J]; end; end;
  end;

  for I := 1 to 154 do begin
    for J := 1 to 94 do begin
      if ((TV15696[I, J] = 1)) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
    end; end;
  Cycle := 0;
  repeat {2}  begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV15696; }
      for I := 0 to 155 do for J := 0 to 95 do
        AA[I, J] := TV15696[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 154 do begin
        for J := 1 to 94 do begin
          if Aa[I, J] = 10 then TV15696[I, J] := 0 else
            if Aa[I, J] = 1 then TV15696[I, J] := 10 else
            begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
              if ((C = 2) or (C = 20) or (C = 11)) then
                TV15696[I, J] := 1;
            end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Direct Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 155 do begin
        for J := 0 to 95 do
          if (Aa[I, J] <> TV15696[I, J]) then
          begin
            if ((TV15696[I, J] = 1)) then
              LifeForm.Image1.Canvas.Brush.Color := Full else
              if ((TV15696[I, J] = 10)) then
                LifeForm.Image1.Canvas.Brush.Color := OverPopulated
              else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          Application.ProcessMessages;
{      bIamDone:=True};
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure BriansBrainDo}
(********************************************************************)
(********************************************************************)

(********************************************************************)
(********************************************************************)

procedure StepChildDo(FileTodo: string);
var
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  NapTime, ErrorCode,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, K, L, M, Hamming, X, Y, Cycle, TVC:
      integer;
  fil: file of Byte {Array7848};
  NewRect: TRect;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array7848;}
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
  SetLength(TV7848, 78, 48);

  LifeForm.WindowState := wsNormal; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 391; {391 781}
  LifeForm.Image1.Height := 241; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 391; {640}
  Bitmap.Height := 241; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
{  K:=Random(67)+5;  L:=Random(37)+5;  M:=2;}{Random(5);}
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
{  FOR I := K TO K+M DO begin   FOR J := L TO L+M DO
    begin    Image[I,J] := 1;  end;   end;}
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin Image[I, J] := Random(30) - 20; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;

{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] > 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] > 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do
  begin
    for J := 1 to 46 do begin
      if (TV7848[I, J] = 1) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
    end;
  end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
          if Aa[I, J] = 1 then TV7848[I, J] := 0;
          begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
            if ((C = 2)) then TV7848[I, J] := 1;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 46 do begin
          if Aa[I, J] = 1 then TV7848[I, J] := 0;
          begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 47, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];

            if ((C = 2)) then TV7848[I, J] := 1;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 77; for J := 1 to 46 do begin
          if Aa[I, J] = 1 then TV7848[I, J] := 0;
          begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J +
     1];
            if ((C = 2)) then TV7848[I, J] := 1;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 76 do begin
          if Aa[I, J] = 1 then TV7848[I, J] := 0;
          begin
   {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
            if ((C = 2)) then TV7848[I, J] := 1;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; for I := 1 to 76 do begin
          if Aa[I, J] = 1 then TV7848[I, J] := 0;
          begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
            if ((C = 2)) then TV7848[I, J] := 1;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 77; begin
          if Aa[I, J] = 1 then TV7848[I, J] := 0;
          begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I - 77, J -
     47];
            if ((C = 2)) then TV7848[I, J] := 1;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 0; begin
          if Aa[I, J] = 1 then TV7848[I, J] := 0;
          begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
            if ((C = 2)) then TV7848[I, J] := 1;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
          if Aa[I, J] = 1 then TV7848[I, J] := 0;
          begin
 {J-1} C := Aa[I + 77, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
            if ((C = 2)) then TV7848[I, J] := 1;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 77;
        if Aa[I, J] = 1 then TV7848[I, J] := 0;
        begin
 {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I - 77, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J + 1];
          if ((C = 2)) then TV7848[I, J] := 1;
        end;
        if not (Aa[I, J] = TV7848[I, J]) then
          HAMMING := (HAMMING + 1);
        if TV7848[I, J] = 1 then CellCount := CellCount + 1;

 {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
{Hs := concat('Hamming # ',Hs);
Cs := concat('Cells   : ',Cs);
 s := concat('Cycle # ',s);   }

      LifeForm.Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 77 do
      begin
        for J := 0 to 47 do
          if (Aa[I, J] <> TV7848[I, J]) then
          begin
            if TV7848[I, J] = 1 then
              LifeForm.Image1.Canvas.Brush.Color := Full
                {CurrentColor}
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure BriansBrainDo}
(********************************************************************)

procedure StepChildDoColor(FileTodo: string);
var
  fil: file of Byte {Array15696};
  NewRect: TRect;
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, {RanLife,XI,YI,}
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, C, I, J, {K,L,M,} Hamming, Cycle: integer;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array15696;}
  SetLength(Aa, 156, 96);
  SetLength(Image, 156, 96);
{SetLength(Image78,78,48);}
  SetLength(TV15696, 156, 96);

  LifeForm.WindowState := wsMaximized; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 781; {391 781}
  LifeForm.Image1.Height := 481; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {640}
  Bitmap.Height := 481; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1;
  LastCellCount := 1;
  OldHamming := 1;
  OldCellCount := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;

  if FileTodo = 'Random' then
  begin
    Randomize;
{  K:=Random(145)+5;  L:=Random(85)+5;  M:=Random(5)+1;}
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := 0; end; end;
{  FOR I := K TO K+M DO   begin   FOR J := L TO L+M DO    begin
    Image[I,J] := 1;   end;   end;}
    for I := 1 to 154 do begin for J := 1 to 94 do
      begin Image[I, J] := Random(30) - 10; end; end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image ); }
      for I := 0 to 155 do for J := 0 to 95 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 155 do
  begin for J := 0 to 95 do
    begin TV15696[I, J] := 0;
    end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 154 do begin for J := 1 to 94 do
      begin if Image[I, J] > 0 then TV15696[I, J] := 1; end; end;
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := TV15696[I, J]; end; end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin if Image[I, J] > 0 then TV15696[I, J] := 1; end; end;
  end;

  for I := 1 to 154 do begin
    for J := 1 to 94 do begin
      if ((TV15696[I, J] = 1)) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
    end; end;
  Cycle := 0;
  repeat {2}  begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV15696;}
      for I := 0 to 155 do for J := 0 to 95 do
        AA[I, J] := TV15696[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 154 do begin
        for J := 1 to 94 do begin
          if Aa[I, J] = 1 then TV15696[I, J] := 0;
          begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
            if ((C = 2)) then TV15696[I, J] := 1;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
      I := 0; for J := 1 to 94 do begin
        if Aa[I, J] = 1 then TV15696[I, J] := 0;
   {J-1} C := Aa[I + 155, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 95, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 155, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
        if ((C = 2)) then TV15696[I, J] := 1;
        if not (Aa[I, J] = TV15696[I, J]) then
          HAMMING := (HAMMING + 1);
        if TV15696[I, J] = 1 then CellCount := CellCount + 1;
      end;
      I := 155; for J := 1 to 94 do begin
        if Aa[I, J] = 1 then TV15696[I, J] := 0;
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 155, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 155, J +
     1];
        if ((C = 2)) then TV15696[I, J] := 1;
        if not (Aa[I, J] = TV15696[I, J]) then
          HAMMING := (HAMMING + 1);
        if TV15696[I, J] = 1 then CellCount := CellCount + 1;
      end;
      J := 0; for I := 1 to 154 do begin
        if Aa[I, J] = 1 then TV15696[I, J] := 0;
   {J-1} C := Aa[I - 1, J + 95] + Aa[I, J + 95] + Aa[I + 1, J + 95];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
        if ((C = 2)) then TV15696[I, J] := 1;
        if not (Aa[I, J] = TV15696[I, J]) then
          HAMMING := (HAMMING + 1);
        if TV15696[I, J] = 1 then CellCount := CellCount + 1;
      end;
      J := 95; for I := 1 to 154 do begin
        if Aa[I, J] = 1 then TV15696[I, J] := 0;
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 95] + Aa[I, J - 95] + Aa[I + 1, J -
     95];
        if ((C = 2)) then TV15696[I, J] := 1;
        if not (Aa[I, J] = TV15696[I, J]) then
          HAMMING := (HAMMING + 1);
        if TV15696[I, J] = 1 then CellCount := CellCount + 1;
      end;
      J := 95; I := 155; begin
        if Aa[I, J] = 1 then TV15696[I, J] := 0;
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 155, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 95] + Aa[I, J - 47] + Aa[I - 155, J -
     95];
        if ((C = 2)) then TV15696[I, J] := 1;
        if not (Aa[I, J] = TV15696[I, J]) then
          HAMMING := (HAMMING + 1);
        if TV15696[I, J] = 1 then CellCount := CellCount + 1;
      end; {*************************************************}
      J := 95; I := 0; begin
        if Aa[I, J] = 1 then TV15696[I, J] := 0;
   {J-1} C := Aa[I + 155, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 155, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 155, J - 95] + Aa[I, J - 95] + Aa[I + 1, J -
     95];
        if ((C = 2)) then TV15696[I, J] := 1;
        if not (Aa[I, J] = TV15696[I, J]) then
          HAMMING := (HAMMING + 1);
        if TV15696[I, J] = 1 then CellCount := CellCount + 1;
      end; {*************************************************}
      J := 0; I := 0; begin
        if Aa[I, J] = 1 then TV15696[I, J] := 0;
 {J-1} C := Aa[I + 155, J + 95] + Aa[I, J + 95] + Aa[I + 1, J + 95];
   {J} C := C + Aa[I + 155, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I + 155, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
        if ((C = 2)) then TV15696[I, J] := 1;
        if not (Aa[I, J] = TV15696[I, J]) then
          HAMMING := (HAMMING + 1);
        if TV15696[I, J] = 1 then CellCount := CellCount + 1;
      end; {*************************************************}
      J := 0; I := 155; begin
        if Aa[I, J] = 1 then TV15696[I, J] := 0;
 {J-1} C := Aa[I - 1, J + 95] + Aa[I, J + 95] + Aa[I - 155, J + 95];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 155, J + 1];
        if ((C = 2)) then TV15696[I, J] := 1;
        if not (Aa[I, J] = TV15696[I, J]) then
          HAMMING := (HAMMING + 1);
        if TV15696[I, J] = 1 then CellCount := CellCount + 1;
      end; {*************************************************}

{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Direct Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 155 do begin
        for J := 0 to 95 do
          if (Aa[I, J] <> TV15696[I, J]) then
          begin
            if ((TV15696[I, J] = 1)) then
              LifeForm.Image1.Canvas.Brush.Color := Full
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          Application.ProcessMessages;
{      bIamDone:=True};
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure StepChild}
(********************************************************************)
(********************************************************************)
(********************************************************************)
(********************************************************************)


(********************************************************************)
(********************************************************************)
(********************************************************************)
(********************************************************************)

procedure SporadicDo(FileTodo: string);
var
  fil: file of Byte {Array15696};
  NewRect: TRect;
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, {RanLife,XI,}
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, C, I, J, Hamming, Cycle: integer;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array15696;}
  SetLength(Aa, 156, 96);
  SetLength(Image, 156, 96);
{SetLength(Image78,78,48);}
  SetLength(TV15696, 156, 96);

  LifeForm.WindowState := wsMaximized; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 781; {391 781}
  LifeForm.Image1.Height := 481; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {640}
  Bitmap.Height := 481; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;

  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 155 do
    begin
      for J := 0 to 95 do
      begin
        Image[I, J] := Random(3);
      end;
    end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 155 do for J := 0 to 95 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 155 do
  begin for J := 0 to 95 do
    begin TV15696[I, J] := 0;
    end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 154 do begin for J := 1 to 94 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := TV15696[I, J]; end; end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
  end;

  for I := 1 to 154 do begin
    for J := 1 to 94 do begin
      if ((TV15696[I, J] = 1)) then begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect); end else
      begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
    end; end;
  Cycle := 0;
  repeat {2}  begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV15696;}
      for I := 0 to 155 do for J := 0 to 95 do
        AA[I, J] := TV15696[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 154 do begin
        for J := 1 to 94 do begin

   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV15696[I, J] := 0; end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C < 5 then begin TV15696[I, J] := 1; end
          else if (C > 4) then begin TV15696[I, J] := 0; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Direct Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 155 do begin
        for J := 0 to 95 do
          if (Aa[I, J] <> TV15696[I, J]) then
          begin
            if ((TV15696[I, J] = 1)) then
              LifeForm.Image1.Canvas.Brush.Color := Full
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          Application.ProcessMessages;
{      bIamDone:=True};
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure BriansBrainDo}
(********************************************************************)

procedure SporadicDoColor(FileTodo: string);
var
  fil: file of Byte {Array15696};
  NewRect: TRect;
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, {RanLife,XI,}
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, C, I, J, Hamming, Cycle: integer;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array15696;}
  SetLength(Aa, 156, 96);
  SetLength(Image, 156, 96);
{SetLength(Image78,78,48);}
  SetLength(TV15696, 156, 96);

  LifeForm.WindowState := wsMaximized; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 781; {391 781}
  LifeForm.Image1.Height := 481; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {640}
  Bitmap.Height := 481; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;

  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 155 do
    begin
      for J := 0 to 95 do
      begin
        Image[I, J] := Random(11) - 9;
      end;
    end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 155 do for J := 0 to 95 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 155 do
  begin for J := 0 to 95 do
    begin TV15696[I, J] := 0;
    end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 154 do begin for J := 1 to 94 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := TV15696[I, J]; end; end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin if Image[I, J] > 0 then TV15696[I, J] := 1; end; end;
  end;

  for I := 1 to 154 do begin
    for J := 1 to 94 do begin
      if ((TV15696[I, J] = 1)) then begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect); end else
      begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
    end; end;
  Cycle := 0;
  repeat {2}  begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV15696;}
      for I := 0 to 155 do for J := 0 to 95 do
        AA[I, J] := TV15696[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 154 do begin
        for J := 1 to 94 do begin

   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C > 5 then begin TV15696[I, J] := 0; end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
{    ELSE IF C =7 THEN    begin	TV15696[I, J]:= 1;    end}
          else if (C = 3) then begin TV15696[I, J] := 1; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Direct Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 155 do begin
        for J := 0 to 95 do
          if (Aa[I, J] <> TV15696[I, J]) then
          begin
            if ((TV15696[I, J] = 1)) then
              LifeForm.Image1.Canvas.Brush.Color := Full
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          Application.ProcessMessages;
{      bIamDone:=True};
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end;
(********************************************************************)
(********************************************************************)


(********************************************************************)
(********************************************************************)

procedure HodgePodgeDo(FileTodo: string);
var
  Name, Cs, Hs, s: string;

  NapTime, ErrorCode,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  fil: file of Byte {Array7848};
  NewRect: TRect;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array7848;}
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
  SetLength(TV7848, 78, 48);

  LifeForm.WindowState := wsNormal; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 391; {391 781}
  LifeForm.Image1.Height := 241; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 391; {640}
  Bitmap.Height := 241; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 77 do
      for J := 0 to 47 do begin
        Image[I, J] := Trunc(Random(2));
      end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;

{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do
  begin
    for J := 1 to 46 do
      if (TV7848[I, J] = 1) then
      begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end else begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;

  end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV 7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 4 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 1; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 46 do begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 47, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
          if C <= 4 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 1; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 77; for J := 1 to 46 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J +
     1];
          if C <= 4 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 1; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 76 do begin
   {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 4 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 1; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; for I := 1 to 76 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
          if C <= 4 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 1; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 77; begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I - 77, J -
     47];
          if C <= 4 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 1; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 0; begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
          if C <= 4 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 1; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I + 77, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 4 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 1; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 77; begin
 {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I - 77, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J + 1];
          if C <= 4 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 1; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
{Hs := concat('Hamming # ',Hs);
Cs := concat('Cells   : ',Cs);
 s := concat('Cycle # ',s);   }

      LifeForm.Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 77 do
      begin
        for J := 0 to 47 do
          if (Aa[I, J] <> TV7848[I, J]) then
          begin
            if TV7848[I, J] = 1 then
              LifeForm.Image1.Canvas.Brush.Color := Full
                {CurrentColor}
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end; { of procedure GAIA M}
(********************************************************************)

procedure HodgePodgeDoColor(FileTodo: string);
var
  fil: file of Byte {Array15696};
  NewRect: TRect;
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, {RanLife,XI,}
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, C, I, J, Hamming, Cycle: integer;
  ImageByte: Byte;
  Bitmap: TBitmap;
begin
{Aa ,Image : Array15696;}
  SetLength(Aa, 156, 96);
  SetLength(Image, 156, 96);
{SetLength(Image78,78,48);}
  SetLength(TV15696, 156, 96);

  LifeForm.WindowState := wsMaximized; {wsNormal wsMaximized}
  LifeForm.Image1.Width := 781; {391 781}
  LifeForm.Image1.Height := 481; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {640}
  Bitmap.Height := 481; {480}
  LifeForm.Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(LifeForm.NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;

  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 155 do
    begin
      for J := 0 to 95 do
      begin
        Image[I, J] := Random(11) - 9;
      end;
    end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 155 do for J := 0 to 95 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  LifeForm.POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 155 do
  begin for J := 0 to 95 do
    begin TV15696[I, J] := 0;
    end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 154 do begin for J := 1 to 94 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := TV15696[I, J]; end; end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin if Image[I, J] > 0 then TV15696[I, J] := 1; end; end;
  end;

  for I := 1 to 154 do begin
    for J := 1 to 94 do begin
      if ((TV15696[I, J] = 1)) then begin
        LifeForm.Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect); end else
      begin
        LifeForm.Image1.Canvas.Brush.Color := Desolate;
          { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        LifeForm.Image1.Canvas.FillRect(NewRect);
      end;
    end; end;
  Cycle := 0;
  repeat {2}  begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV15696;}
      for I := 0 to 155 do for J := 0 to 95 do
        AA[I, J] := TV15696[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 154 do begin
        for J := 1 to 94 do begin

   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];

          if C = 2 then begin TV15696[I, J] := 0; end else
            if C = 4 then begin TV15696[I, J] := 0; end else
              if C = 6 then begin TV15696[I, J] := 0; end
              else if C = 7 then begin TV15696[I, J] := 1; end
              else if C = 5 then begin TV15696[I, J] := 1; end
              else if (C = 3) then begin TV15696[I, J] := 1; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;

{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      LifeForm.Panel2.Caption := 'Direct Life:  ' + 'Hamming:' + Hs +
        ' Cells:' + Cs;
      LifeForm.NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 155 do begin
        for J := 0 to 95 do
          if (Aa[I, J] <> TV15696[I, J]) then
          begin
            if ((TV15696[I, J] = 1)) then
              LifeForm.Image1.Canvas.Brush.Color := Full
            else LifeForm.Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            LifeForm.Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          Application.ProcessMessages;
{      bIamDone:=True};
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

  LifeForm.POGeniBtn.Caption := 'Genesis';
  LifeForm.POLeviBtn.Enabled := True;
  LifeForm.POGeniBtn.Enabled := True;
end;
(********************************************************************)
(********************************************************************)



(********************************************************************)
(********************************************************************)

procedure SchellingDo(FileTodo: string);
begin

end;
(********************************************************************)
{procedure TLifeForm.SchellingDo(FileTodo:String);
Random starting amount decides density,
as the cells 'move' not die}
{    procedure TLifeForm.SchellingDoColor(FileTodo:String);
The rule this ALife model operates on is that for every colored cell,
if greater than 33% of the adjacent cells are of a different color,
the cell moves to another randomly selected cell.}

procedure SchellingDoColor(FileTodo: string);
begin

end;
(********************************************************************)
(********************************************************************)


end.
