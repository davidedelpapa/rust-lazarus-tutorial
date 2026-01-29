unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  jsonmplib, hexdump;

type

  TDataFormat = (dfJSON, dfMsgPack);

  { TForm1 }

  TForm1 = class(TForm)
    btnConvert: TButton;
    btnReverse: TButton;
    CheckBase64: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    procedure btnConvertClick(Sender: TObject);
    procedure btnReverseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FLeftFormat: TDataFormat;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLeftFormat := dfJSON;  // left starts as JSON
  Label1.Caption := 'JSON';
  Label2.Caption := 'MessagePack';
end;

procedure TForm1.btnReverseClick(Sender: TObject);
var
  tmp: string;
begin
  // swap captions
  tmp := Label1.Caption;
  Label1.Caption := Label2.Caption;
  Label2.Caption := tmp;

  // swap memos visually
  tmp := Memo1.Text;
  Memo1.Text := Memo2.Text;
  Memo2.Text := tmp;

  // toggle the state
  if FLeftFormat = dfJSON then
    FLeftFormat := dfMsgPack
  else
    FLeftFormat := dfJSON;
end;

procedure TForm1.btnConvertClick(Sender: TObject);
var
  inp: string;
  outp: pchar;
  buf: TMpBuffer;
  bytes: TBytes;
  json: string;
begin
  inp := Memo1.Text;

  if FLeftFormat = dfJSON then
  begin
    // JSON -> MsgPack
    if CheckBase64.Checked then
    begin
      // JSON -> Base64 MsgPack
      outp := json_to_msgpack64(PChar(inp));
      try
        if outp <> nil then
          Memo2.Text := string(outp);
      finally
        free_cstring(outp);
      end;
    end
    else
    begin
      // JSON -> raw msgpack, shown as hex dump
      buf := json_to_msgpack(PChar(inp));
      try
        if (buf.Data <> nil) and (buf.len > 0) then
        begin
          // Build byte array
          SetLength(bytes, buf.len);
          Move(buf.Data^, bytes[0], buf.len);
          // Convert to hex
          Memo2.Text := BinToHexDump(bytes);
        end;
      finally
        free_mpbuffer(buf);
      end;
    end;
  end
  else
  begin
    // MsgPack -> JSON
    if CheckBase64.Checked then
    begin
      // Memo1 contains Base64
      outp := msgpack64_to_json(PChar(inp));
      try
        if outp <> nil then
          Memo2.Text := outp;
      finally
        free_cstring(outp);
      end;
    end
    else
    begin
      // Memo1 contains hex-dump MessagePack
      try
        bytes := HexDumpToBin(inp);
      except
        on E: Exception do
        begin
          ShowMessage('Invalid hex dump: ' + E.Message);
          Exit;
        end;
      end;

      if Length(bytes) = 0 then
      begin
        Memo2.Text := '(empty or invalid hex dump)';
        Exit;
      end;

      // Pass raw bytes to Rust
      outp := msgpack_to_json(@bytes[0], Length(bytes));
      try
        if outp <> nil then
          Memo2.Text := outp;
      finally
        free_cstring(outp);
      end;
    end;
  end;
end;


end.
