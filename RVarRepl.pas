unit RVarRepl;

interface

const
  chTag  = '@';
  chFmt  = ':';
  chSFm  = ';';
  chStd  = '%';

  sEmptyText   = '';

  sIntFormat   = '%d';
  sStrFormat   = '%s';
  sFloatFormat = '%10.2n';
  sDateFormat  = 'yyyy-mm-dd';
  sDateTimeFormat = 'dd.mm.yyyy hh:nn:ss';

function ReplaceFmtTags(const SrcStr, TagName, TagFormat: string; TagValues: array of const;
  const ShowEmptyText: Boolean = True): string;
function ReplaceFmtDateTags(const SrcStr, TagName, DateFormat, StrFormat: string;
  const TagValue: TDateTime; const ShowEmptyText: Boolean = True): string;

implementation

uses
  SysUtils;

function ReplaceFmtTags(const SrcStr, TagName, TagFormat: string;
  TagValues: array of const; const ShowEmptyText: Boolean = True): string;
var
  i: Integer;
  sTag, sRepl, sTagName, sTagFormat, vTagName: string;
  bTag: Boolean;
begin
  Result := EmptyStr;
  vTagName := EmptyStr;
  bTag := False;
  for i := 1 to Length(TagName) do
    if TagName[i] <> chTag then
      vTagName := vTagName + TagName[i];
  for i := 1 to Length(SrcStr) do
    if SrcStr[i] = chTag then
    begin
      if bTag then
      begin
        bTag := False;
        if sTag = EmptyStr then
          sRepl := chTag
        else begin
          sRepl := chTag + sTag + chTag;
          if Pos(chFmt, sTag) > 0 then
          begin
            sTagName := Trim(Copy(sTag, 1, Pos(chFmt, sTag) - 1));
            sTagFormat := Trim(Copy(sTag, Pos(chFmt, sTag) + 1,
              Length(sTag) - Pos(chFmt, sTag)));
            if sTagFormat = '' then sTagFormat := TagFormat;
          end
          else begin
            sTagName := sTag;
            sTagFormat := TagFormat;
          end;
          if AnsiUpperCase(sTagName) = AnsiUpperCase(vTagName) then
            sRepl := Format(sTagFormat, TagValues);
          if (sRepl = EmptyStr) and ShowEmptyText then
            sRepl := sEmptyText;
        end;
        Result := Result + sRepl;
      end
      else begin
        bTag := True;
        sTag := EmptyStr;
      end;
    end
    else begin
      if bTag
      then sTag := sTag + SrcStr[i]
      else Result := Result + SrcStr[i];
    end;
end;

function ReplaceFmtDateTags(const SrcStr, TagName, DateFormat, StrFormat: string;
  const TagValue: TDateTime; const ShowEmptyText: Boolean = True): string;
var
  i: Integer;
  sTag, sRepl, sTagName, sTagFormat, vTagName: string;
  bTag: Boolean;
  sDateFormat, sStrFormat: string;
begin
  Result := EmptyStr;
  vTagName := EmptyStr;
  bTag := False;
  for i := 1 to Length(TagName) do
    if TagName[i] <> chTag then
      vTagName := vTagName + TagName[i];
  for i := 1 to Length(SrcStr) do
    if SrcStr[i] = chTag then
    begin
      if bTag then
      begin
        bTag := False;
        if sTag = EmptyStr then
          sRepl := chTag
        else begin
          sRepl := chTag + sTag + chTag;
          if Pos(chFmt, sTag) > 0 then
          begin
            sTagName := Trim(Copy(sTag, 1, Pos(chFmt, sTag) - 1));
            sTagFormat := Trim(Copy(sTag, Pos(chFmt, sTag) + 1,
              Length(sTag) - Pos(chFmt, sTag)));
            if sTagFormat = '' then sTagFormat := DateFormat + chSFm + StrFormat;
          end
          else begin
            sTagName := sTag;
            if StrFormat <> EmptyStr
            then sTagFormat := DateFormat + chSFm + StrFormat
            else sTagFormat := DateFormat;
          end;
          if AnsiUpperCase(sTagName) = AnsiUpperCase(vTagName) then
          begin
            sDateFormat := DateFormat;
            sStrFormat := StrFormat;
            if Pos(chSFm, sTagFormat) > 0 then
            begin
              sDateFormat := Trim(Copy(sTagFormat, 1, Pos(chSFm, sTagFormat) - 1));
              sStrFormat := Trim(Copy(sTagFormat, Pos(chSFm, sTagFormat) + 1,
                Length(sTagFormat) - Pos(chSFm, sTagFormat)));
              if sStrFormat = '' then sStrFormat := StrFormat;
            end
            else sDateFormat := sTagFormat;
            if sDateFormat = '' then sDateFormat := DateFormat;
            if (TagValue = 0) and ShowEmptyText
            then sRepl := sEmptyText
            else sRepl := Format(sStrFormat, [FormatDateTime(sDateFormat, TagValue)]);
          end;
        end;
        Result := Result + sRepl;
      end
      else begin
        bTag := True;
        sTag := EmptyStr;
      end;
    end
    else begin
      if bTag
      then sTag := sTag + SrcStr[i]
      else Result := Result + SrcStr[i];
    end;
end;

end.
