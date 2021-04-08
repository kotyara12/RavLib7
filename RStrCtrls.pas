unit RStrCtrls;

interface

uses
  SysUtils, StdCtrls, StrUtils, Dialogs;

function SearchEdit(EditControl: TCustomEdit; const SearchString: string;
  Options: TFindOptions; FindFirst: Boolean = False): Boolean;

implementation

function SearchEdit(EditControl: TCustomEdit; const SearchString: string;
  Options: TFindOptions; FindFirst: Boolean = False): Boolean;
var
  Buffer, P: PChar;
  Size: Word;
  SearchOptions: TStringSearchOptions;
begin
  Result := False;
  if (Length(SearchString) = 0) then Exit;
  Size := EditControl.GetTextLen;
  if (Size = 0) then Exit;
  Buffer := StrAlloc(Size + 1);
  try
    SearchOptions := [];
    if frDown in Options then
      Include(SearchOptions, soDown);
    if frMatchCase in Options then
      Include(SearchOptions, soMatchCase);
    if frWholeWord in Options then
      Include(SearchOptions, soWholeWord);
    EditControl.GetTextBuf(Buffer, Size + 1);
    if FindFirst then
      P := SearchBuf(Buffer, Size, 0, EditControl.SelLength,
             SearchString, SearchOptions)
    else
      P := SearchBuf(Buffer, Size, EditControl.SelStart, EditControl.SelLength,
             SearchString, SearchOptions);
    if P <> nil then
    begin
      EditControl.SelStart := P - Buffer;
      EditControl.SelLength := Length(SearchString);
      Result := True;
    end;
  finally
    StrDispose(Buffer);
  end;
end;

end.
 