unit RStrings;

interface

uses
  Classes, SysUtils;

procedure SaveStrings(const FileName: string; Strings: TStrings);
procedure LoadFileList(const FileMask: string; Strings: TStrings);

implementation

procedure SaveStrings(const FileName: string; Strings: TStrings);
var
  F: Text;
  i: Integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    for i := 0 to Strings.Count - 1 do
      WriteLn(F, Strings[i]);
  finally
    CloseFile(F);
  end;
end;

procedure LoadFileList(const FileMask: string; Strings: TStrings);
var
  F: Integer;
  SR: TSearchRec;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    F := FindFirst(FileMask, faAnyFile and not faDirectory, SR);
    try
      while F = 0 do
      begin
        Strings.Add(SR.Name);
        F := FindNext(SR);
      end;
    finally
      FindClose(SR);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

end.
