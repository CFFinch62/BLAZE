{ loops.pas - BLAZE Example 3: Loop Structures }
program Loops;

var
  i, sum : Integer;

begin
  { FOR / TO loop }
  WriteLn('--- FOR loop (1 to 5) ---');
  for i := 1 to 5 do
    WriteLn('  i = ', i);

  { FOR / DOWNTO loop }
  WriteLn('--- FOR DOWNTO loop (5 to 1) ---');
  for i := 5 downto 1 do
    WriteLn('  i = ', i);

  { WHILE loop }
  WriteLn('--- WHILE loop (sum until > 10) ---');
  sum := 0;
  i   := 1;
  while sum <= 10 do
  begin
    sum := sum + i;
    Inc(i);
  end;
  WriteLn('  Final sum = ', sum);

  { REPEAT / UNTIL loop }
  WriteLn('--- REPEAT / UNTIL (countdown from 3) ---');
  i := 3;
  repeat
    WriteLn('  ', i);
    Dec(i);
  until i = 0;

  WriteLn('Done.');
end.
