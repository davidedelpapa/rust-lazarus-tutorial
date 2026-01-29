unit hexdump;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function BinToHexDump(const bytes: TBytes): string;
function HexDumpToBin(const s: string): TBytes;

implementation

function BinToHexDump(const bytes: TBytes): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to High(bytes) do
  begin
    Result += IntToHex(bytes[i], 2);
    if i < High(bytes) then
      Result += ' ';
  end;
end;

function HexDumpToBin(const s: string): TBytes;
var
  parts: TStringList;
  i: integer;
  Value: integer;
begin
  parts := TStringList.Create;
  try
    parts.Delimiter := ' ';
    parts.StrictDelimiter := True;
    parts.DelimitedText := StringReplace(s, #13#10, ' ', [rfReplaceAll]);

    SetLength(Result, parts.Count);

    for i := 0 to parts.Count - 1 do
    begin
      if TryStrToInt('$' + parts[i], Value) then
        Result[i] := byte(Value)
      else
        raise Exception.Create('Invalid hex byte: "' + parts[i] + '"');
    end;
  finally
    parts.Free;
  end;
end;

end.
