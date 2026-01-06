program jsonmpdemo;

{$mode ObjFPC}{$H+}

uses
  jsonmplib, Classes, SysUtils;

var
  buf: TMpBuffer;
  jsonStr: PChar;
  msgpackStream: TMemoryStream;

begin
  // Convert JSON to MessagePack
  buf := json_to_msgpack(PChar('{"x":1,"y":2}'));

  if (buf.data <> nil) and (buf.len > 0) then
  begin
    msgpackStream := TMemoryStream.Create;
    try
      // Write MessagePack bytes to stream/file
      msgpackStream.WriteBuffer(buf.data^, buf.len);
      msgpackStream.SaveToFile('out.msgpack');
    finally
      msgpackStream.Free;
    end;
    free_mpbuffer(buf);   // Rust-allocated memory must be freed
  end
  else
    Writeln('ERROR: json_to_msgpack returned empty buffer.');

  // Now read from file back into a NEW stream
  msgpackStream := TMemoryStream.Create;
  try
    msgpackStream.LoadFromFile('out.msgpack');
    msgpackStream.Position := 0;

    // Convert MessagePack -> JSON
    jsonStr := msgpack_to_json(msgpackStream.Memory, msgpackStream.Size);

    if jsonStr <> nil then
    begin
      Writeln('Decoded JSON: ', jsonStr);
      free_cstring(jsonStr); // Rust-allocated memory must be freed
    end
    else
      Writeln('ERROR: msgpack_to_json returned NULL.');
  finally
    msgpackStream.Free;
  end;
end.

