program lazrust;

uses rustlib, SysUtils;

var
  P, P2: TPerson;

begin

  P.name := NewCString('John Doe');
  P.office := NewCString('IT');
  P.phone := NewCString('555-0101');
  P.age := 28;

  P2 := verify_person(P);

  Writeln('Rust returned:');
  Writeln('  name:   ', P2.name);
  Writeln('  office: ', P2.office);
  Writeln('  phone:  ', P2.phone);
  Writeln('  age:    ', P2.age);

  // Free Rust-allocated strings
  free_cstring(P2.name);
  free_cstring(P2.office);
  free_cstring(P2.phone);

  // Free Pascal-allocated strings
  StrDispose(P.name);
  StrDispose(P.office);
  StrDispose(P.phone);

end.

