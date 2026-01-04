unit RustLib;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPoint = record
    x: LongInt;
    y: LongInt;
  end;

function move_point(p: TPoint; dx: LongInt; dy: LongInt): TPoint; cdecl; external 'librustlaz';

implementation

end.

