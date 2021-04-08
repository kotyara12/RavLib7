unit RScanFile;

interface

uses
  ComCtrls;

type
  TFileVIData = ^RFileVIData;
  RFileVIData = record
    FileName: string;
    FileDate: TDateTime;
    FileSize: Int64;
    FileDescription: string;
    FileComment: string;
    FileVersion: string;
    InternalName: string;
    ProductName: string;
    ProductVersion: string;
    Author: string;
    Copyright: string;
    Language: Integer;
    Crc32: Integer;
  end;

  RFileSVIData = packed record
    FileName: string[128];
    FilePath: string[255];
    FileDate: TDateTime;
    FileSize: Int64;
    FileDescription: string[255];
    FileComment: string[255];
    FileVersion: string[24];
	  InternalName: string[64];
    ProductName: string[255];
    ProductVersion: string[24];
    Author: string[255];
    Copyright: string[255];
    Language: Integer;
    Crc32: Integer;
  end;

function  GetFileVIData(const FileName: string): RFileVIData;
function  ConvertVItoSVI(const VID: RFileVIData): RFileSVIData;
procedure ScanDirectoryVIData(LV: TListView; const DirName, Mask: string; const SubDirs: Boolean);

implementation

uses
  RxVerInf, RxStrUtils, RCrc32, RFileProcs, RDialogs, RVclUtils,
  SysUtils, Windows;

const
  AnyFilesMask = '*.*';

function GetFileVIData(const FileName: string): RFileVIData;
var
  VersionData: TVersionInfo;
  P: Pointer;
begin
  Result.FileName := EmptyStr;
  if wFileExists(FileName) then
  begin
    Result.FileName := FileName;
    Result.FileDate := FileTimeToDateTime(wFileGetLastWriteTime(FileName));
    Result.FileSize := wFileGetSize(FileName);
    Result.Crc32 := FileCRC32(FileName);
    VersionData := TVersionInfo.Create(FileName);
    try
      Result.FileDescription := VersionData.FileDescription;
      Result.FileComment := VersionData.Comments;
      Result.FileVersion := VersionData.FileVersion;
      Result.InternalName := VersionData.InternalName;
      Result.ProductName := VersionData.ProductName;
      Result.ProductVersion := VersionData.ProductVersion;
      Result.Author := VersionData.CompanyName;
      Result.Copyright := VersionData.LegalCopyright;
      P := VersionData.Translation;
      if P <> nil then Result.Language := LoWord(Longint(P^));
    finally
      VersionData.Free;
    end;
  end;
end;

function ConvertVItoSVI(const VID: RFileVIData): RFileSVIData;
begin
  Result.FileName := ExtractFileName(VID.FileName);
  Result.FilePath := ExtractFilePath(VID.FileName);
  Result.FileDate := VID.FileDate;
  Result.FileSize := VID.FileSize;
  Result.FileDescription := VID.FileDescription;
  Result.FileComment := VID.FileComment;
  Result.FileVersion := VID.FileVersion;
  Result.InternalName := VID.InternalName;
  Result.ProductName := VID.ProductName;
  Result.ProductVersion := VID.ProductVersion;
  Result.Author := VID.Author;
  Result.Copyright := VID.Copyright;
  Result.Language := VID.Language;
  Result.Crc32 := VID.Crc32;
end;

procedure ScanDirectoryVIData(LV: TListView; const DirName, Mask: string; const SubDirs: Boolean);

  procedure IntScan(const ScanDir: string);
  var
    i, F: Integer;
    SR: TSearchRec;
    FD: RFileVIData;
    FT: TFileVIData;
  begin
    F := FindFirst(IncludeTrailingBackslash(ScanDir) + AnyFilesMask, faAnyFile, SR);
    try
      while F = 0 do
      begin
        if ((SR.Attr and faDirectory) = 0) then
        begin
          for i := 1 to WordCount(Mask, chDivChars) do
            if IsWild(SR.Name, Trim(ExtractWord(i, Mask, chDivChars)), True) then
            begin
              FD := GetFileVIData(IncludeTrailingBackslash(ScanDir) + SR.Name);
              if FD.FileName <> EmptyStr then
              begin
                with LV.Items.Add do
                begin
                  New(FT);
                  FT^ := FD;
                  Data := FT;
                  Caption := ExtractFileName(FD.FileName);
                  Checked := True;
                  if FD.InternalName <> EmptyStr
                  then Subitems.Add(FD.InternalName)
                  else Subitems.Add(FD.FileDescription);
                  Subitems.Add(DateTimeToStr(FD.FileDate));
                  Subitems.Add(FD.FileVersion);
                  Subitems.Add(Format('%8.8x', [FD.Crc32]));
                  Subitems.Add(FD.FileName);
                end;
              end;
              Break;
            end;
        end
        else if SubDirs and (Pos('.', SR.Name) <> 1)
             then IntScan(IncludeTrailingBackslash(ScanDir) + SR.Name);
        F := FindNext(SR);
      end;
    finally
      SysUtils.FindClose(SR);
    end;
  end;

begin
  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;
    IntScan(DirName);
  finally
    LV.Items.EndUpdate;
  end;
end;

end.
