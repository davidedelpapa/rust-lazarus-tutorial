unit jsonmplib;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  PMpBuffer = ^TMpBuffer;

  TMpBuffer = record
    Data: pbyte;
    len: longint;
  end;

function json_to_msgpack(jsonStr: pchar): TMpBuffer; cdecl; external 'libjsonmplib';

function msgpack_to_json(Data: pbyte; len: longint): pchar; cdecl; external 'libjsonmplib';

procedure free_mpbuffer(buf: TMpBuffer); cdecl; external 'libjsonmplib';

procedure free_cstring(str: pchar); cdecl; external 'libjsonmplib';

function json_to_msgpack64(jsonStr: PChar): PChar; cdecl; external 'libjsonmplib';

function msgpack64_to_json(b64Str: PChar): PChar; cdecl; external 'libjsonmplib';

implementation

end.
