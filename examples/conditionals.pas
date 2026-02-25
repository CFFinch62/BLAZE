{ conditionals.pas - BLAZE Example 4: Conditionals }
program Conditionals;

var
  score : Integer;
  grade : Char;

begin
  score := 85;

  { IF / THEN / ELSE }
  WriteLn('--- IF / THEN / ELSE ---');
  if score >= 90 then
    WriteLn('Grade: A')
  else if score >= 80 then
    WriteLn('Grade: B')
  else if score >= 70 then
    WriteLn('Grade: C')
  else
    WriteLn('Grade: F');

  { CASE / OF }
  WriteLn('--- CASE / OF ---');
  if score >= 90 then grade := 'A'
  else if score >= 80 then grade := 'B'
  else if score >= 70 then grade := 'C'
  else grade := 'F';

  case grade of
    'A': WriteLn('Excellent!');
    'B': WriteLn('Good job!');
    'C': WriteLn('Satisfactory.');
    'F': WriteLn('Needs improvement.');
  else
    WriteLn('Unknown grade.');
  end;

  WriteLn('Score was: ', score);
end.
