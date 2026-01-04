unit RustLib;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function add_numbers(a: LongInt; b: LongInt): LongInt; cdecl; external 'rustlaz';

implementation

end.

