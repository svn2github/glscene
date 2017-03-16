{-------------------------------------------------------------------------------
 Unit Name: frmProcess
 Author:    HochwimmerA
 Purpose:
 $Id: frmProcess.pas,v 1.8 2003/10/05 18:27:26 hochwimmera Exp $
-------------------------------------------------------------------------------}
unit frmProcess;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls,
   
  GLGeometryCoordinates,
  //glData
  geFloatEdit;

type
  TformProcess = class(TForm)
    pnlTop: TPanel;
    lblValue: TLabel;
    cbDataMapping: TComboBox;
    memProcessReport: TMemo;
    pnlBottom: TPanel;
    bCancel: TBitBtn;
    bOK: TBitBtn;
    geParameterA: TGEFloatEdit;
    pbProcess: TProgressBar;
    bProcess: TBitBtn;
    procedure bProcessClick(Sender: TObject);
    procedure cbDataMappingChange(Sender: TObject);
  private
     
  public
     
  end;

implementation

uses
  frmMain;

{$R *.dfm}

// ----- TformProcess.bProcessClick --------------------------------------------
{** process the original data to the processed data}
procedure TformProcess.bProcessClick(Sender: TObject);

var
  u,v,w,val,x,y,z:double;
  uu,uv,uw,ux,uy,uz,mag : double;
  dleft,dright,dBack,dForward,dBottom,dtop:double;
  bIsNull,bNullPosition,bNullVelocity:boolean;
  ierr,iScalar: integer;

begin
(*
  dLeft := 1e30;
  dRight := -1e30;
  dback := 1e30;
  dForward := -1e30;

  dTop := -1e30;
  dBottom := 1e30;
  iScalar := 0;

  with formMain do
  begin
    bProcessing := true;
///    kbmData1.DisableControls;
///    kbmData2.DisableControls;
///    kbmData2.EmptyTable;
    pbProcess.Position := 0;
    pbProcess.Min := 0;
    pbProcess.Max := kbmData1.RecordCount;

    with kbmData1 do
    begin
      First;
      while (not eof) do
      begin
        u := FieldByName('u').AsFloat;
        v := FieldByName('v').AsFloat;
        w := FieldByName('w').AsFloat;
        uu := FieldByName('Uu').AsFloat;
        uv := FieldByName('Uv').AsFloat;
        uw := FieldByName('Uw').AsFloat;
        bIsNull := FieldByName('Value').IsNull;
        bNullPosition :=  FieldByName('u').IsNull or
                          FieldByName('v').IsNull or
                          FieldByName('w').IsNull;
        bNullVelocity :=  FieldByName('Uu').IsNull or
                          FieldByName('Uv').IsNull or
                          FieldByName('Uw').IsNull;
        val := FieldByName('Value').AsFloat;

// convert the position
        if not bNullVelocity then
        begin
          case cbDataMapping.ItemIndex of
            0: begin
              ux := uu;
              uy := uv;
              uz := uw;
              ierr := 0;
            end;
            1: Cylindrical_Cartesian(uu,uv,uw,ux,uy,uz,ierr);
            2: Spherical_Cartesian(uu,uv,uw,ux,uy,uz,ierr);
            3: ProlateSpheroidal_Cartesian(uu,uv,uw,geParameterA.Value,
              ux,uy,uz,ierr);
            4: OblateSpheroidal_Cartesian(uu,uv,uw,geParameterA.Value,
              ux,uy,uz,ierr);
            5: BiPolarCylindrical_Cartesian(uu,uv,uw,geParameterA.Value,
              ux,uy,uz,ierr);
          end;
          mag := sqrt(ux*ux+uy*uy+uz*uz);
          if (mag >0.0) then
          begin
            ux := ux/mag;
            uy := uy/mag;
            uz := uz/mag;
          end
        end;

        if not bNullPosition then
        begin
          case cbDataMapping.ItemIndex of
            0: begin
              x := u;
              y := v;
              z := w;
              ierr := 0;
            end;
            1: Cylindrical_Cartesian(u,v,w,x,y,z,ierr);
            2: Spherical_Cartesian(u,v,w,x,y,z,ierr);
            3: ProlateSpheroidal_Cartesian(u,v,w,geParameterA.Value,x,y,z,ierr);
            4: OblateSpheroidal_Cartesian(u,v,w,geParameterA.Value,x,y,z,ierr);
            5: BiPolarCylindrical_Cartesian(u,v,w,geParameterA.Value,x,y,z,ierr);
          end;
        end;

// only add the point if the conversion was ok
      if ierr=0 then
      begin
        kbmData2.Append;
// only add positions that aren't null
        if bNullPosition then
        begin
          kbmData2.FieldByName('x').Clear;
          kbmData2.FieldByName('y').Clear;
          kbmData2.FieldByName('z').Clear;
        end else
        begin
          kbmData2.FieldByName('x').AsFloat := x;
          kbmData2.FieldByName('y').AsFloat := y;
          kbmData2.FieldByName('z').AsFloat := z;
        end;

        if bIsNull then
          kbmData2.FieldByName('Value').Clear
        else begin
          Inc(iScalar);
          kbmData2.FieldByName('Value').AsFloat := val;
          if X < dLeft then
            dLeft := X;
          if X > dRight then
            dRight := X;
          if Y < dBack then
            dBack := Y;
          if Y > dForward then
            dForward := Y;
          if Z < dBottom then
            dbottom := Z;
          if Z > dTop then
            dtop := z;
        end;

        if bNullVelocity then
        begin
          kbmData2.FieldByName('Magnitude').Clear;
          kbmData2.FieldByName('ux').Clear;
          kbmData2.FieldByName('uy').Clear;
          kbmData2.FieldByName('uz').Clear;
        end else
        begin
          kbmData2.FieldByName('Magnitude').AsFloat := mag;
          kbmData2.FieldByName('ux').AsFloat := ux;
          kbmData2.FieldByName('uy').AsFloat := uy;
          kbmData2.FieldByName('uz').AsFloat := uz;
        end;

        kbmData2.Post;
      end;
      Next;
      pbProcess.Position := pbProcess.Position + 1;
    end;
  end;

  kbmData1.EnableControls;
  kbmData2.EnableControls;

// write report...
  with memProcessReport.Lines do
  begin
    Clear;
    Add('Transformation performed: ' + DateTimeToStr(Now));
    Add('Mapping: '+cbDataMapping.Text);
    Add(' ');
    Add(IntToStr(kbmData1.RecordCount) + ' original data points');
    Add(' ');

    if (iScalar > 0) then
    begin
      Add(IntToStr(iScalar) + ' processed data points');
      Add('Scaler Point Limits');
      Add('Minimum X Point:' +#9+ FloatToStrF(dLeft,ffGeneral,4,4));
      Add('Maximum X Point:' +#9+ FloatToStrF(dRight,ffGeneral,4,4));
      Add('Minimum Y Point:' +#9+ FloatToStrF(dBottom,ffGeneral,4,4));
      Add('Maximum Y Point:' +#9+ FloatToStrF(dTop,ffGeneral,4,4));
      Add('Minimum Z Point:' +#9+ FloatToStrF(dBack,ffGeneral,4,4));
      Add('Maximum Z Point:' +#9+ FloatToStrF(dForward,ffGeneral,4,4));
    end else
      Add('No scalar data values processed.');

  end;

    bProcessing := false;
    bOK.Enabled := kbmData2.RecordCount>0;
    kbmData2.Edit;
  end;
*)
end;
// ----- TformProcess.cbDataMappingChange --------------------------------------
procedure TformProcess.cbDataMappingChange(Sender: TObject);
begin
  case cbDataMapping.ItemIndex of
    0: geParameterA.Enabled := false; // cartesian
    1: geParameterA.Enabled := false; // cylindrical
    2: geParameterA.Enabled := false; // spherical
    3: geParameterA.Enabled := true;  // prolate spheroidal
    4: geParameterA.Enabled := true;  // oblate spheroidal
    5: geParameterA.Enabled := true;  // bi-polar cylindrical
  end;
end;
// =============================================================================
end.















