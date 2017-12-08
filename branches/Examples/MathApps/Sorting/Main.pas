// *****************************************************************//
// //
// Sorting Algorithms Demo Application                            //
// Copyright© BrandsPatch LLC                                     //
// http://www.explainth.at                                        //
// //
// All Rights Reserved                                            //
// //
// Permission is granted to use, modify and redistribute          //
// the code in this Delphi unit on the condition that this        //
// notice is retained unchanged.                                  //
// //
// BrandsPatch declines all responsibility for any losses,        //
// direct or indirect, that may arise  as a result of using       //
// this code.                                                     //
// //
// *****************************************************************//
unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  //
  Sorts;

type
  TMaster = class(TForm)
    pgcDemo: TPageControl;
    tsReadMe: TTabSheet;
    redInfo: TRichEdit;
    tsDemo: TTabSheet;
    rgSorts: TRadioGroup;
    pnlDemo: TPanel;
    memoRaw: TMemo;
    splitSort: TSplitter;
    pnlSorted: TPanel;
    memoSorted: TMemo;
    stTime: TStaticText;
    pnlCtrl: TPanel;
    cbCount: TComboBox;
    btnSort: TButton;
    procedure FormCreate(Sender: TObject);
    procedure DoSortSelect(Sender: TObject);
    procedure DoSort(Sender: TObject);
    procedure RePopulate(Sender: TObject);
  private
    FSort: TSortProc;
    FVals: TSortArray;
    GVals: TSortArray;
  public
  end;

var
  Master: TMaster;

implementation

{$R *.dfm}
{$R WindowsXP.RES}

procedure TMaster.FormCreate(Sender: TObject);
var
  AFile: String;
begin
  FSort := BubbleSort;
  RePopulate(nil);
  AFile := Format('%sReadMe.RTF', [ExtractFilePath(ParamStr(0))]);
  if FileExists(AFile) then
    redInfo.Lines.LoadFromFile(AFile);
end;

procedure TMaster.DoSortSelect(Sender: TObject);
begin
  case rgSorts.ItemIndex of
    0:  FSort := BubbleSort;
    1:  FSort := SelSort;
    2:  FSort := InsertSort;
    3:  FSort := HeapSort;
    4:  FSort := MergeSort;
    5:  FSort := QuickSort;
    6:  FSort := ShellSort;
  end;
  memoSorted.Clear;
  stTime.Caption := 'Not Sorted';
end;

type
  TSigns = array [0 .. 1] of Integer;

const
  Signs: TSigns = (-1, 1);

procedure TMaster.RePopulate;
var
  i, j, ACount: Integer;
  AFill: String;
begin
  FillChar(FVals, sizeof(FVals), 0);
  with cbCount do
    val(Items[ItemIndex], ACount, i);

  SetLength(AFill, 0);
  j := 0;
  Randomize;
  for i := 0 to ACount - 1 do
  begin
    FVals[i] := Signs[j] * Random(100);
    AFill := Format('%s%-8.0f', [AFill, FVals[i]]);
    // For demonstration purposes integers are more readable
    j := 1 - j;
  end;

  Move(FVals, GVals, sizeof(FVals));
  // safe copy of original sort data

  with memoRaw do
  begin
    Clear;
    Lines.Text := AFill;
  end;
  stTime.Caption := 'Not Sorted';
end;

procedure TMaster.DoSort(Sender: TObject);
var
  i, ACount, AStart: Integer;
  AFill: String;
begin
  with cbCount do
    val(Items[ItemIndex], ACount, i);
  stTime.Caption := 'Sorting...';

  rgSorts.Enabled := False;
  pnlCtrl.Enabled := False;
  try
    AStart := GetTickCount;
    FSort(FVals, ACount);
    AStart := GetTickCount - AStart;
    // get the time estimate immediately AFTERWARDS - the screen update is slow!
    SetLength(AFill, 0);
    for i := 0 to ACount - 1 do
      AFill := Format('%s%-8.0f', [AFill, FVals[i]]);
    with memoSorted do
    begin
      Clear;
      Lines.Text := AFill;
    end;
  finally
    rgSorts.Enabled := True;
    pnlCtrl.Enabled := True;
    stTime.Caption := Format('%dms', [AStart]);
    Move(GVals, FVals, sizeof(FVals));
    // ready for another sort
  end;
end;

end.
