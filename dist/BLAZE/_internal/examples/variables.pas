{ variables.pas - BLAZE Example 2: Variables and Data Types }
program Variables;

var
  age       : Integer;
  height    : Real;
  initial   : Char;
  name      : String;
  isStudent : Boolean;

begin
  { Assign values }
  age       := 20;
  height    := 1.75;
  initial   := 'A';
  name      := 'Alice';
  isStudent := True;

  { Display them }
  WriteLn('Name:       ', name);
  WriteLn('Initial:    ', initial);
  WriteLn('Age:        ', age);
  WriteLn('Height:     ', height:0:2, ' m');
  WriteLn('Is student: ', isStudent);
end.
