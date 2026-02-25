{ arrays.pas - BLAZE Example 6: Arrays }
program Arrays;

const
  SIZE = 5;

type
  IntArray = array[1..SIZE] of Integer;

var
  nums   : IntArray;
  words  : array[1..3] of String;
  i, sum : Integer;
  avg    : Real;

procedure PrintArray(const a : IntArray; n : Integer);
var
  j : Integer;
begin
  Write('  [');
  for j := 1 to n do
  begin
    Write(a[j]);
    if j < n then Write(', ');
  end;
  WriteLn(']');
end;

begin
  { Fill array }
  for i := 1 to SIZE do
    nums[i] := i * i;   { squares: 1, 4, 9, 16, 25 }

  WriteLn('--- Integer array (squares) ---');
  PrintArray(nums, SIZE);

  { Sum and average }
  sum := 0;
  for i := 1 to SIZE do
    sum := sum + nums[i];
  avg := sum / SIZE;
  WriteLn('Sum = ', sum, '   Average = ', avg:0:2);

  { String array }
  WriteLn('--- String array ---');
  words[1] := 'Pascal';
  words[2] := 'is';
  words[3] := 'fun!';
  for i := 1 to 3 do
    Write(words[i], ' ');
  WriteLn;
end.
