unit aclass5;
{ S A M P L E 5  used in the About Box
  The complete definition of the TUserInfo
  object. This object can be used by any unit
  that includes this unit in its uses clause.}

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry;
{ Use the Registry unit to fetch User/Company Name }

type
  ERegInfoNotFound = class(Exception);

  TUserInfo = class(TObject)
    Reg: TRegistry;
  private
    function GetUserName: string;
    function GetCompanyName: string;
  public
    property UserName: string read GetUserName;
    property CompanyName: string read GetCompanyName;
  end;

const
  Win95RegInfo = 'SOFTWARE\Microsoft\Windows\CurrentVersion\';

implementation

{ Functional definition of TUserInfo code starts here }

function TUserInfo.GetUserName: string;
{var
  fileHandle: THandle;
  fileBuffer: Array [0..29] of Char;}
begin
{ Fetch registered user name from Win95 Registry }
  Reg := TRegistry.Create;
  with Reg do begin
    RootKey := HKEY_LOCAL_MACHINE;
    if KeyExists(Win95RegInfo) then begin
      OpenKey(Win95RegInfo, False);
      Result := ReadString('RegisteredOwner');
    end
    else
      raise ERegInfoNotFound.Create('Could not locate ' +
        'registered user name');
    Free;
  end; { with }
end;

function TUserInfo.GetCompanyName: string;
{var
  fileHandle: THandle;
  fileBuffer: Array [0..29] of Char;}
begin
{ Fetch registered company name from Win95 Registry }
  Reg := TRegistry.Create;
  with Reg do begin
    RootKey := HKEY_LOCAL_MACHINE;
    if KeyExists(Win95RegInfo) then begin
      OpenKey(Win95RegInfo, False);
      Result := ReadString('RegisteredOrganization');
    end
    else
      raise ERegInfoNotFound.Create('Could not locate ' +
        'registered company name');
    Free;
  end; { with }
end;

end.
