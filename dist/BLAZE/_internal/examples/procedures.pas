{ procedures.pas - BLAZE Example 5: Procedures and Functions }
program Procedures;

{ Procedure: no return value }
procedure Greet(name : String);
begin
  WriteLn('Hello, ', name, '!');
end;

{ Function: returns a value }
function Square(n : Integer) : Integer;
begin
  Square := n * n;
end;

{ Function with VAR parameter (pass-by-reference) }
procedure Swap(var a, b : Integer);
var
  tmp : Integer;
begin
  tmp := a;
  a   := b;
  b   := tmp;
end;

{ Recursive function }
function Factorial(n : Integer) : Integer;
begin
  if n <= 1 then
    Factorial := 1
  else
    Factorial := n * Factorial(n - 1);
end;

var
  x, y : Integer;

begin
  Greet('World');

  WriteLn('Square of 7 = ', Square(7));

  x := 10;
  y := 20;
  WriteLn('Before swap: x=', x, ' y=', y);
  Swap(x, y);
  WriteLn('After swap:  x=', x, ' y=', y);

  WriteLn('5! = ', Factorial(5));
end.
