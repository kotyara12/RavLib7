unit RMediaInfoDLL;

interface

uses
  Classes;

{ == Данные о формате аудиопотока ============================================== }
function MI_GetVideoFormat(const FileName: string): string;
function MI_GetAudioFormat(const FileName: string; const ShortFormat: Boolean): string;
{ == Данные о продолжительности потока (мс) ==================================== }
function MI_DurationToStr(const Duration: Integer; const ForcedHour: Boolean): string;
function MI_FileSizeToStr(const Size: Int64): string;
function MI_GetGeneralDuration(const FileName: string): Integer;
function MI_GetAudioDuration(const FileName: string): Integer;
function MI_GetAudioDurationAuto(const FileName: string): Integer;
{ == Данные о названии трека =================================================== }
function MI_GetTrackName(const FileName: string; const TrackNumDef: Integer): string;

implementation

uses
  SysUtils, StrUtils, RxStrUtils, RStrUtils, RFileUtils, RDialogs, MediaInfoDll;

{ == Данные о формате видеопотока ============================================== }
function  MI_GetVideoFormat(const FileName: string): string;
var
  Handle: Cardinal;
  sFmtName, sFmtVersion, sFmtCodec, sFmtDescr: string;
  sRate, sSizeF, sSizeA, sSizeX, sSizeY: string;
begin
  Result := '';
  if GetFileSize(FileName) > 0 then
  begin
    Handle := MediaInfo_New();
    try
      MediaInfo_Open(Handle, StringToOleStr(FileName));
      // Извлекаем данные о формате видеопотока
      sFmtDescr := '';
      MediaInfo_Option(0, 'Inform', 'Video;%Format%');
      sFmtName := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      MediaInfo_Option(0, 'Inform', 'Video;%Format_Version%');
      sFmtVersion := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      // Переименовываем "узнаваемые" форматы...
      if AnsiStartsText('MPEG', sFmtName) then
      begin
        if AnsiEndsText('Visual', sFmtName) then
          sFmtDescr := Trim(ReplaceStr(sFmtName, ' Visual', 'V'))
        else begin
          sFmtDescr := Trim(ReplaceStr(sFmtName, 'Video', ''));
          if AnsiStartsText('Version', sFmtVersion) then
          begin
            sFmtVersion := Trim(ReplaceStr(sFmtVersion, 'Version', ''));
            if sFmtVersion <> '' then
              sFmtDescr := sFmtDescr + '-' + sFmtVersion + 'V';
          end
          else begin
            if sFmtVersion <> '' then
              sFmtDescr := sFmtDescr + ' ' + sFmtVersion
            else
              sFmtDescr := sFmtDescr + 'V';
          end;
        end;
      end;
      if sFmtDescr = '' then
      begin
        sFmtDescr := sFmtName;
        if AnsiStartsText('Version', sFmtVersion) then
        begin
          sFmtVersion := Trim(ReplaceStr(sFmtVersion, 'Version', ''));
          if sFmtVersion <> '' then
            sFmtDescr := sFmtDescr + '-' + sFmtVersion;
        end
        else begin
          if sFmtVersion <> '' then
            sFmtDescr := sFmtDescr + ' ' + sFmtVersion;
        end;
      end;
      Result := sFmtDescr;
      // Извекаем данные о кодеке
      MediaInfo_Option(0, 'Inform', 'Video;%CodecID/Hint%');
      sFmtCodec := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      if sFmtCodec = '' then
      begin
        MediaInfo_Option(0, 'Inform', 'Video;%CodecID%');
        sFmtCodec := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      end;
      if sFmtCodec <> '' then
        Result := Result + ' (' + sFmtCodec + ')';
      // Извекаем данные о частоте дискретизации
      MediaInfo_Option(0, 'Inform', 'Video;%BitRate/String%');
      sRate := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      if sRate <> '' then
        Result := Result + ', ' + sRate;
      // Извекаем данные о размере кадра
      sSizeF := '';
      MediaInfo_Option(0, 'Inform', 'Video;%Width%');
      sSizeX := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      MediaInfo_Option(0, 'Inform', 'Video;%Height%');
      sSizeY := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      MediaInfo_Option(0, 'Inform', 'Video;%DisplayAspectRatio/String%');
      sSizeA := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      if (sSizeX <> '') and (sSizeY <> '') then
        sSizeF := sSizeX + '*' + sSizeY;
      if sSizeA <> '' then
        sSizeF := sSizeF + #32 + '(' + sSizeA + ')';
      if sSizeF <> '' then
        Result := Result + ', ' + sSizeF;
      // Извекаем данные о частоте кадров
      MediaInfo_Option(0, 'Inform', 'Video;%FrameRate/String%');
      sRate := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      sRate := Trim(ReplaceStr(sRate, '.000', '.0'));
      if sRate <> '' then
        Result := Result + ', ' + sRate;
    finally
      MediaInfo_Close(Handle);
    end;
  end;
end;

{ == Данные о формате аудиопотока ============================================== }
function MI_GetAudioFormat(const FileName: string; const ShortFormat: Boolean): string;
var
  Handle: Cardinal;
  bGetBitrate: Boolean;
  sFmtName, sFmtVersion, sFmtProfile, sFmtDescr: string;
  sBrMode, sBrValue, sBrMin, sBrMax: string;
  sChannels, sRate: string;

  function GetProfileName(const sProfile: string): string;
  begin
    Result := sProfile;
    if AnsiStartsText('Layer', sProfile) then
    begin
      Result := Trim(ReplaceStr(sProfile, 'Layer', ''));
      if Result <> '' then
        Result := 'L' + Result;
    end;
  end;

  function GetFullMpegName(sName, sVersion, sLayer: string): string;
  var
    sIntVersion, sIntLayer: string;
  begin
    Result := '';
    // Преобразовываем версию формата
    if AnsiStartsText('Version', sVersion) then
    begin
      Result := Trim(ReplaceStr(sName, 'MPEG Audio', 'MPEG'));
      sIntVersion := Trim(ReplaceStr(sVersion, 'Version', ''));
      if sIntVersion <> '' then
        Result := Result + '-' + sIntVersion + 'A';
    end
    else begin
      Result := sName;
      if sVersion <> '' then
        Result := Result + ' ' + sVersion;
    end;
    // Преобразовываем профиль (уровнь) формата
    sIntLayer := GetProfileName(sLayer);
    if sIntLayer <> '' then
      Result := Result + ' ' + sIntLayer;
  end;

begin
  Result := '';
  if GetFileSize(FileName) > 0 then
  begin
    Handle := MediaInfo_New();
    try
      MediaInfo_Open(Handle, StringToOleStr(FileName));
      // Извлекаем данные о формате аудиопотока
      bGetBitrate := True;
      sFmtDescr := '';
      MediaInfo_Option(0, 'Inform', 'Audio;%Format%');
      sFmtName := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      MediaInfo_Option(0, 'Inform', 'Audio;%Format_Version%');
      sFmtVersion := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      MediaInfo_Option(0, 'Inform', 'Audio;%Format_Profile%');
      sFmtProfile := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      // Переименовываем "узнаваемые" форматы...
      if ShortFormat then
      begin
        // Краткий вариант
        if SameText('MPEG Audio', sFmtName) then
        begin
          bGetBitrate := True;
          sFmtProfile := GetProfileName(sFmtProfile);
          if SameText('L3', sFmtProfile)
          then sFmtDescr := 'MP3'
          else sFmtDescr := GetFullMpegName(sFmtName, sFmtVersion, sFmtProfile);
        end;
        if SameText('Monkey''s Audio', sFmtName) then
        begin
          sFmtDescr := 'APE';
          bGetBitrate := False;
        end;
        if SameText('FLAC', sFmtName) then
        begin
          sFmtDescr := 'FLAC';
          bGetBitrate := False;
        end;
        if SameText('Vorbis', sFmtName) then
        begin
          sFmtDescr := 'OGG';
          bGetBitrate := True;
        end;
      end
      else begin
        // Полный вариант
        if SameText('MPEG Audio', sFmtName) then
        begin
          bGetBitrate := True;
          sFmtDescr := GetFullMpegName(sFmtName, sFmtVersion, sFmtProfile);
        end;
      end;
      // Остальные форматы
      if sFmtDescr = '' then
      begin
        sFmtDescr := sFmtName;
        if AnsiStartsText('Version', sFmtVersion) then
        begin
          sFmtVersion := Trim(ReplaceStr(sFmtVersion, 'Version', ''));
          if sFmtVersion <> '' then
            sFmtDescr := sFmtDescr + '-' + sFmtVersion;
        end
        else begin
          if sFmtVersion <> '' then
            sFmtDescr := sFmtDescr + ' ' + sFmtVersion;
        end;
        sFmtProfile := GetProfileName(sFmtProfile);
        if sFmtProfile <> '' then
          sFmtDescr := sFmtDescr + ' ' + sFmtProfile;
      end;
      Result := sFmtDescr;
      // Извекаем данные о битрейте
      if bGetBitrate then
      begin
        MediaInfo_Option(0, 'Inform', 'Audio;%BitRate_Mode%');
        sBrMode := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
        if SameText(sBrMode, 'CBR') then
        begin
          MediaInfo_Option(0, 'Inform', 'Audio;%BitRate/String%');
          sBrValue := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
        end
        else begin
          MediaInfo_Option(0, 'Inform', 'Audio;%BitRate_Minimum/String%');
          sBrMin := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
          MediaInfo_Option(0, 'Inform', 'Audio;%BitRate_Maximum/String%');
          sBrMax := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
          if (sBrMin <> '') and (sBrMax <> '') then
            sBrValue := sBrMin + '~' + sBrMax
          else begin
            MediaInfo_Option(0, 'Inform', 'Audio;%BitRate_Nominal/String%');
            sBrValue := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
            if sBrValue = '' then
            begin
              MediaInfo_Option(0, 'Inform', 'Audio;%BitRate/String%');
              sBrValue := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
            end;
            if sBrValue <> '' then
              sBrValue := '~' + sBrValue;
          end;
        end;
        if (sBrMode <> '') and (sBrValue <> '') then
          sBrValue := sBrValue + #32 + sBrMode;
        if sBrMode <> '' then
          Result := Result + ', ' + sBrValue;
      end;
      // Извекаем данные о режиме (моно/стерео)
      MediaInfo_Option(0, 'Inform', 'Audio;%Format_Settings_Mode%');
      sChannels := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      if sChannels = '' then
      begin
        MediaInfo_Option(0, 'Inform', 'Audio;%Channel(s)%');
        sChannels := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
        if sChannels = '1' then
          sChannels := 'Mono'
        else begin
          if sChannels = '2' then
            sChannels := 'Stereo'
          else
            sChannels := sChannels + ' ch';
        end;
      end;
      if sChannels <> '' then
        Result := Result + ', ' + sChannels;
      // Извекаем данные о частоте дискретизации
      MediaInfo_Option(0, 'Inform', 'Audio;%SamplingRate/String%');
      sRate := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      if sRate <> '' then
        Result := Result + ', ' + sRate;
    finally
      MediaInfo_Close(Handle);
    end;
  end;
end;

{ == Данные о продолжительности потока (мс) ==================================== }
function MI_DurationToStr(const Duration: Integer; const ForcedHour: Boolean): string;
var
  iDuration, iHour, iMin, iSec: Integer;
begin
  Result := EmptyStr;
  iDuration := Duration;
  if iDuration > 0 then
    iDuration := Round(Duration / 1000);
  if iDuration > 0 then
  begin
    iHour := iDuration div (3600);
    iMin := (iDuration - iHour * 3600) div 60;
    iSec := (iDuration - iHour * 3600 - iMin * 60);
    if (iHour > 0) or ForcedHour
    then Result := Format('%.2d:%.2d:%.2d', [iHour, iMin, iSec])
    else Result := Format('%.2d:%.2d', [iMin, iSec]);
  end;
end;

function MI_FileSizeToStr(const Size: Int64): string;
const
  iGb = 1024 * 1024 * 1024;
  iMb = 1024 * 1024;
  iKb = 1024;
begin
  if Size >= iGb
  then Result := Format('%.3n Gb', [Size / iGb])
  else begin
    if Size >= iMb
    then Result := Format('%.3n Mb', [Size / iMb])
    else begin
      if Size >= iKb
      then Result := Format('%.3n Kb', [Size / iKb])
      else Result := Format('%d', [Size]);
    end;
  end;
end;

function MI_GetGeneralDuration(const FileName: string): Integer;
var
  Handle: Cardinal;
begin
  Result := 0;
  if GetFileSize(FileName) > 0 then
  begin
    Handle := MediaInfo_New();
    try
      MediaInfo_Open(Handle, StringToOleStr(FileName));
      MediaInfo_Option(0, 'Inform', 'General;%Duration%');
      Result := StrToIntDef(WideCharToString(MediaInfo_Inform(Handle, 0)), 0);
    finally
      MediaInfo_Close(Handle);
    end;
  end;
end;

function MI_GetAudioDuration(const FileName: string): Integer;
var
  Handle: Cardinal;
begin
  Result := 0;
  if GetFileSize(FileName) > 0 then
  begin
    Handle := MediaInfo_New();
    try
      MediaInfo_Open(Handle, StringToOleStr(FileName));
      MediaInfo_Option(0, 'Inform', 'Audio;%Duration%');
      Result := StrToIntDef(WideCharToString(MediaInfo_Inform(Handle, 0)), 0);
    finally
      MediaInfo_Close(Handle);
    end;
  end;
end;

function MI_GetAudioDurationAuto(const FileName: string): Integer;
begin
  Result := 0;
  try
    Result := MI_GetGeneralDuration(FileName);
  except
  end;
  if Result < 1000 then
    Result := MI_GetAudioDuration(FileName);
end;

{ == Данные о названии трека =================================================== }
function MI_GetTrackName(const FileName: string; const TrackNumDef: Integer): string;
var
  Handle: Cardinal;
  sComposer: string;
  iTrackNum: Integer;
  iDuration: Integer;
begin
  Result := '';
  if GetFileSize(FileName) > 0 then
  begin
    Handle := MediaInfo_New();
    try
      // Считываем название трека
      MediaInfo_Open(Handle, StringToOleStr(FileName));
      MediaInfo_Option(0, 'Inform', 'General;%Title%');
      Result := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      if Result = '' then
      begin
        MediaInfo_Option(0, 'Inform', 'General;%Track%');
        Result := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
      end;
      // Остальные данные
      if Result <> '' then
      begin
        // Считываем имя исполнителя
        MediaInfo_Option(0, 'Inform', 'General;%Composer%');
        sComposer := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
        if sComposer = '' then
        begin
          MediaInfo_Option(0, 'Inform', 'General;%Original/Performer%');
          sComposer := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
        end;
        if sComposer = '' then
        begin
          MediaInfo_Option(0, 'Inform', 'General;%Performer%');
          sComposer := Trim(WideCharToString(MediaInfo_Inform(Handle, 0)));
        end;
        if (sComposer <> '') and not AnsiContainsText(ExtractFilePath(FileName), sComposer) then
          Result := sComposer + ' - ' + Result;
        // Считываем номер трека
        MediaInfo_Option(0, 'Inform', 'General;%Track/Position%');
        iTrackNum := StrToIntDef(WideCharToString(MediaInfo_Inform(Handle, 0)), TrackNumDef);
        if (iTrackNum > 0)
        and not AnsiStartsText(Format('%.1d', [iTrackNum]), Result)
        and not AnsiStartsText(Format('%.2d', [iTrackNum]), Result)
        and not AnsiStartsText(Format('%.3d', [iTrackNum]), Result) then
          Result := Format('%.2d. ', [iTrackNum]) + Result;
        // Считываем продолжительность
        MediaInfo_Option(0, 'Inform', 'General;%Duration%');
        iDuration := StrToIntDef(WideCharToString(MediaInfo_Inform(Handle, 0)), 0);
        if iDuration > 0 then
          Result := Result + ' [' + MI_DurationToStr(iDuration, False) + ']';
      end;
    finally
      MediaInfo_Close(Handle);
    end;
  end;
end;

end.
