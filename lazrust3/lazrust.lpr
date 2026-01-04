program lazrust;

uses RustLib;

var
  P: TPoint;
  P2: TPoint;

begin
  P.x := 10;
  P.y := 20;

  P2 := move_point(P, 3, -2);

  Writeln('Original: (', P.x, ', ', P.y, ')');
  Writeln('Moved:    (', P2.x, ', ', P2.y, ')');

end.

