{ records.pas - BLAZE Example 7: Records }
program Records;

type
  TStudent = record
    name  : String;
    age   : Integer;
    grade : Real;
  end;

procedure PrintStudent(const s : TStudent);
begin
  WriteLn('Name:  ', s.name);
  WriteLn('Age:   ', s.age);
  WriteLn('Grade: ', s.grade:0:1);
end;

var
  s1, s2 : TStudent;

begin
  { Direct field assignment }
  s1.name  := 'Alice';
  s1.age   := 20;
  s1.grade := 92.5;

  { WITH statement for cleaner access }
  with s2 do
  begin
    name  := 'Bob';
    age   := 21;
    grade := 87.0;
  end;

  WriteLn('--- Student 1 ---');
  PrintStudent(s1);

  WriteLn('--- Student 2 ---');
  PrintStudent(s2);

  { Compare grades }
  if s1.grade > s2.grade then
    WriteLn(s1.name, ' has the higher grade.')
  else
    WriteLn(s2.name, ' has the higher grade.');
end.
