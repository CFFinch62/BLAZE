{ fileio.pas - BLAZE Example 8: File I/O }
program FileIO;

const
  FILENAME = 'test_output.txt';

var
  f    : Text;
  line : String;
  i    : Integer;

begin
  { ── Write to a text file ─────────────────────────────────── }
  WriteLn('Writing to "', FILENAME, '"...');
  Assign(f, FILENAME);
  Rewrite(f);              { Open for writing (creates/overwrites) }

  WriteLn(f, 'Hello from BLAZE!');
  for i := 1 to 5 do
    WriteLn(f, 'Line ', i, ': value = ', i * 10);

  Close(f);
  WriteLn('Write complete.');

  { ── Read back from the text file ────────────────────────── }
  WriteLn;
  WriteLn('Reading "', FILENAME, '" back:');
  Assign(f, FILENAME);
  Reset(f);                { Open for reading }

  while not EOF(f) do
  begin
    ReadLn(f, line);
    WriteLn('  > ', line);
  end;

  Close(f);
  WriteLn('Read complete.');
end.
