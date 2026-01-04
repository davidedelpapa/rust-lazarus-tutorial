unit TestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, RustLib;

type

  { TForm1 }

  TForm1 = class(TForm)
    FirstNumber: TEdit;
    SecondNumber: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ResultLabel: TLabel;
    procedure FirstNumberEditingDone(Sender: TObject);
    procedure SecondNumberEditingDone(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FirstNumberEditingDone(Sender: TObject);
begin
  ResultLabel.Caption:= IntToStr(add_numbers(StrToInt(FirstNumber.Text), StrToInt(SecondNumber.Text)));
end;

procedure TForm1.SecondNumberEditingDone(Sender: TObject);
begin
  ResultLabel.Caption:= IntToStr(add_numbers(StrToInt(FirstNumber.Text), StrToInt(SecondNumber.Text)));
end;

end.

