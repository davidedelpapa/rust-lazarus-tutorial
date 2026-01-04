unit rustlib;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  PPerson = ^TPerson;

  TPerson = record
    Name: pchar;
    office: pchar;
    phone: pchar;
    age: longint;
  end;

function verify_person(p: TPerson): TPerson; cdecl; external 'librustlaz';
function free_cstring(s: pchar): longint; cdecl; external 'librustlaz';

function NewCString(const S: string): pchar;

implementation

function NewCString(const S: string): pchar;
var
  Len: SizeInt;
begin
  Len := Length(S) + 1; // include null terminator
  Result := StrAlloc(Len);
  StrPLCopy(Result, S, Len);
end;

end.
