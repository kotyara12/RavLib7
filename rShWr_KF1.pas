unit rShWr_KF1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, Mask, ToolEdit, RVclUtils;

type
  tRegMode = (rmTrialRuns, rmTrialDays, rmFull);

  tKeyState = (ksBadAppId, ksBadCompId, ksBadUserId, ksBadKey, ksFull, ksTrial, ksExpired);

  rKeyData = record
    sAppId: string[38];
    sHardId: string[8];
    sRegName: string[32];
    sRegCode: string[20];
    fMode: tRegMode;
    iCount: Integer;
    wLR_Day: Word;
    wLR_Month: Word;
    wLR_Year: Word;
  end;

  TFormRegister = class(TForm)
    btnCancel: TBitBtn;
    btnCopy: TButton;
    lblAppId: TLabel;
    edAppId: TEdit;
    lblCompId: TLabel;
    edCompId: TEdit;
    lblRegName: TLabel;
    edRegName: TEdit;
    lblRegData: TLabel;
    lblRegFile: TLabel;
    edRegFile: TFilenameEdit;
    btnReg: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure ButtonsChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  public
    { Public declarations }
  end;

function  rShareware_InitProtector(const fAppId: string): Boolean;
function  rShareware_IsTrial: Boolean;
function  rShareware_RegName: string;
function  rShareware_GenCode(const fKey: rKeyData): string;
function  rShareware_ReadFileKey(const sFileName: string; var fKey: rKeyData): Boolean;
procedure rShareware_SaveFileKey(const sFileName: string; const fKey: rKeyData);

{$IFDEF KEYGEN}
const
  sRegMode: array [tRegMode] of string = ('По запускам', 'По времени', 'Без ограничений');

  cRegMode: array [tRegMode] of TColor = (clOrangeTone, clRedTone, clGreenTone);
  cKeyMode: array [TRegMode] of TColor = (clMaroon, clPurple, clBlack);
{$ENDIF}

implementation

uses
  StrUtils, Registry, RClipbrd, LcUnit,
  RCryptApi, RWmiUtils, RDialogs, RCrc32, RWait;

{$R *.dfm}

resourcestring
  rsUserTrial            = 'dhdNYXahCko=';
  rsMsgKeyInit           = 'hJsrM1KbU97c3uMEJOy6Qbn7iv2tCUuI';
  rsErrReadFileKey       = 'keSQQqaPTfER/nor39fLdXclIdb+j9cSSg3XDphK91SNsMr805RVsw==';
  rsErrSaveFileKey       = '0Cx5A+1LXUMt1ImemrTf8sZNw4nbo/z4W9JFWqa34VK9luP5saRMgg==';
  rsErrReadRegKey        = 'keSQQqaPTfE/p717hZgesmSzBL6NBEYnjV6v44FTl1nJWdv08vGGUg==';
  rsErrSaveRegKey        = '0Cx5A+1LXUO/PzIG2+Tc0O9chDfWpYFAw3J4u1bFPFRQvj6CEnTarQ==';
  rsErrOpenKey           = 'bBb3FUQt2Yjhthp9BIuvmkODBl9nHf333eDe84Unw8EZSnzhKfbyqA==';
  rsErrBadKeyFile        = 'UvFlY1OqGto0oPyh/ldjV5YuOHMB2PNQJotCqKAQl2QZq9Rh5OHIdw==';
  rsErrKeyNotFound       = '4EQ7e8eNpnXOJKEi0jSqniFBYqX348NeLekgDybUqILbVXksXQbZHoSSRbNCerpYeVJfHCery4elheo/XZSB7g==';
  rsErrBadAppId          = '4EQ7e8eNpnXOJKEi0jSqniFBYqX348Nekn+5EM2HNzbEgUHnxjNjVW+FcQIYYcOJnAKk6wxJxWU5NrdZ5ugzRrog5oWKSZfqMk4rNc6kza11h1ofcaLhtA==';
  rsErrBadCompId         = '4EQ7e8eNpnXOJKEi0jSqniFBYqX348Nekn+5EM2HNzbEgUHnxjNjVW+FcQIYYcOJnAKk6wxJxWU5NrdZ5ugzRrog5oWKSZfqbtlvnVpmGWT6ZuLnJzuyIA==';
  rsErrBadUserId         = '4EQ7e8eNpnXOJKEi0jSqniFBYqX348Nekn+5EM2HNzbEgUHnxjNjVW+FcQIYYcOJ3zdDxCa7pwhTufb1BhPjUvlODKr1HK2iqOrCoaLJuskEQ/u2SjKkpLys/qHnttUZ';
  rsErrBadKey            = '4EQ7e8eNpnXOJKEi0jSqniFBYqX348Neif/C0oY60pvL2VzuzWSJbh9eqaNh9DzDZ5ywouQqVisYTj095jG8SQ==';
  rsMsgBadTrialDate      = '0/EDfzRTh8avjL/cIAK+aGNPx4PCLLXMEYHusX+kBu9tKg2cA4N7tQJeZXq6TfzABgmNNFDFk7TT3Ce9cZIPc1DSrhlrswlEpLTiEEKcwI8G2LKtjdVV2svUnb7Ux562IDjiJyws1czZJZeLmbUrEpqbfwSjD9JhgT9qMnya7vI=';
  rsMsgBadTrialRuns      = '0/EDfzRTh8avjL/cIAK+aGNPx4PCLLXMEYHusX+kBu9tKg2cA4N7tUOz717vtQolYvdWtgwC62/CNDZJAAKGoChCWHLCsYxTE9Hl/fg6H40FGcClcvAH/4y9QS6mD9ktSFBkV3Eww/fgXgQV1glVnl0axVf28t04j8A3Mg2G4pc=';
  rsErrBadTrialExpired   = '0/EDfzRTh8avjL/cIAK+aGNPx4PCLLXMEYHusX+kBu8ewakZOCbIv6rPiVo2LXT6Sx0reLs1TTd68wai5SOiwqJv9QCjk/aCbF2C21W49HKtNjJxNnGkHOWrugvFLWLbgJOlAonAGThGpmkm7jObAOFZ4OWVV9mP';
  rsMsgRegistred         = 'CO6yLUr4fRUDJbQrOB9BIQAjPtvrUT3DxokgkefcEAg=';

const
  pBaseClass             = '{BD1D15A6-1D62-4141-AFEF-FCA970FBD23B}';
  sExtKeyFile            = 'RUolYF8jQRY=';
  pKeyKeyFile            = '/OgESCUVedul7fOTdXcpUSamg1KQTX/FnnYS3GxuGmm1DIQLOvfqpg==';
  pRegCode               = 'h1oQUD5tLZRcre1veSVAcUzJyey5HFJC5b49kzSr36Lx8ibZl5IjhA==';
  sBadKeyCode            = 'Tibh4YLHRJqUJSWWn4zrNo2wqrOKuAbG';
  mLastRun               = 'RQLy/t7f01EuSqf5at23JYbEYIUg9X8C1gBeso5oPjU=';
  sRegKey                = 'P0VS0VToSukZvSOxy38m9H7II7x/VdN6';

var
  fAppKey: rKeyData;

function _estr(const sMsg: string): string;
begin
  Result := DecryptPwd_Text_RsaShaRc2B64(sMsg, pBaseClass);
end;

procedure _rSW_EncodeKey(sBuf: TStringList; const fKey: rKeyData);
begin
  sBuf.Clear;
  sBuf.Add(EncryptPwd_Text_RsaShaRc4Hex(fKey.sAppId, _estr(pKeyKeyFile)));
  sBuf.Add(EncryptPwd_Text_RsaShaRc4Hex(fKey.sHardId, _estr(pKeyKeyFile)));
  sBuf.Add(EncryptPwd_Text_RsaShaRc4Hex(fKey.sRegName, _estr(pKeyKeyFile)));
  sBuf.Add(EncryptPwd_Text_RsaShaRc4Hex(fKey.sRegCode, _estr(pKeyKeyFile)));
  sBuf.Add(EncryptPwd_Text_RsaShaRc4Hex(Format(_estr(mLastRun), [Byte(fKey.fMode), fKey.iCount, fKey.wLR_Day, fKey.wLR_Month, fKey.wLR_Year]), _estr(pKeyKeyFile)));
end;

procedure _rSW_DecodeKey(sBuf: TStringList; var fKey: rKeyData);
var
  sLastRun: string;
begin
  if sBuf.Count <> 5 then
    raise Exception.Create(_estr(rsErrBadKeyFile));

  fKey.sAppId := DecryptPwd_Text_RsaShaRc4Hex(sBuf[0], _estr(pKeyKeyFile));
  fKey.sHardId := DecryptPwd_Text_RsaShaRc4Hex(sBuf[1], _estr(pKeyKeyFile));
  fKey.sRegName := DecryptPwd_Text_RsaShaRc4Hex(sBuf[2], _estr(pKeyKeyFile));
  fKey.sRegCode := DecryptPwd_Text_RsaShaRc4Hex(sBuf[3], _estr(pKeyKeyFile));
  sLastRun := DecryptPwd_Text_RsaShaRc4Hex(sBuf[4], _estr(pKeyKeyFile));
  fKey.fMode := tRegMode(StrToIntDef(Copy(sLastRun, 1, 1), 0));
  fKey.wLR_Year := StrToIntDef(Copy(sLastRun, 2, 4), 0);
  fKey.wLR_Day := StrToIntDef(Copy(sLastRun, 6, 2), 0);
  fKey.iCount := StrToIntDef(Copy(sLastRun, 8, 2), 0);
  fKey.wLR_Month := StrToIntDef(Copy(sLastRun, 10, 2), 0);
end;

function rShareware_ReadFileKey(const sFileName: string; var fKey: rKeyData): Boolean;
var
  sBuf: TStringList;
begin
  Result := False;
  if FileExists(sFileName) then
  begin
    try
      sBuf := TStringList.Create;
      try
        sBuf.LoadFromFile(sFileName);

        _rSW_DecodeKey(sBuf, fKey);

        Result := True;
      finally
        sBuf.Free;
      end;
    except
      on E: Exception do
        ErrorBox(Format(_estr(rsErrReadFileKey), [E.Message]));
    end;
  end;
end;

procedure rShareware_SaveFileKey(const sFileName: string; const fKey: rKeyData);
var
  sBuf: TStringList;
begin
  try
    sBuf := TStringList.Create;
    try
      _rSW_EncodeKey(sBuf, fKey);
      sBuf.SaveToFile(sFileName);
    finally
      sBuf.Free;
    end;
  except
    on E: Exception do
      ErrorBox(Format(_estr(rsErrSaveFileKey), [E.Message]));
  end;
end;

function rShareware_ReadRegKey(var fKey: rKeyData): Boolean;
var
  sBuf: TStringList;
  Reg: TRegistry;
begin
  Result := False;
  try
    Reg := TRegistry.Create(KEY_ALL_ACCESS);
    try
      if Reg.OpenKey(Format(_estr(sRegKey), [fKey.sAppId]), False) then
      begin
        sBuf := TStringList.Create;
        try
          if Reg.ValueExists('0') then sBuf.Add(Reg.ReadString('0'));
          if Reg.ValueExists('1') then sBuf.Add(Reg.ReadString('1'));
          if Reg.ValueExists('2') then sBuf.Add(Reg.ReadString('2'));
          if Reg.ValueExists('3') then sBuf.Add(Reg.ReadString('3'));
          if Reg.ValueExists('4') then sBuf.Add(Reg.ReadString('4'));

          _rSW_DecodeKey(sBuf, fKey);

          Result := True;
        finally
          sBuf.Free;
        end;
      end;
    finally
      Reg.Free;
    end;
  except
    on E: Exception do
      ErrorBox(Format(_estr(rsErrReadRegKey), [E.Message]));
  end;
end;

procedure rShareware_SaveRegKey(const fKey: rKeyData);
var
  sBuf: TStringList;
  Reg: TRegistry;
  i, iCount: Integer;
begin
  try
    sBuf := TStringList.Create;
    try
      _rSW_EncodeKey(sBuf, fKey);

      Reg := TRegistry.Create(KEY_ALL_ACCESS);
      try
        if Reg.OpenKey(Format(_estr(sRegKey), [fKey.sAppId]), True) then
        begin
          iCount := sBuf.Count - 1;
          for i := 0 to iCount do
            Reg.WriteString(IntToStr(i), sBuf[i]);
        end
        else raise Exception.CreateFmt(_estr(rsErrOpenKey), [Format(_estr(sRegKey), [fKey.sAppId])]);
      finally
        Reg.Free;
      end;
    finally
      sBuf.Free;
    end;
  except
    on E: Exception do
      ErrorBox(Format(_estr(rsErrSaveRegKey), [E.Message]));
  end;
end;

function rShareware_GenCode(const fKey: rKeyData): string;
var
  sIntData: string;
begin
  try
    sIntData := fKey.sAppId + IntToStr(Integer(fKey.fMode)) + fKey.sRegName;
    if fKey.fMode = rmFull then
      sIntData := sIntData + fKey.sHardId;

    Result := Format('%.8x', [StrCRC32(sIntData)]) + '-' +
              Format('%.8x', [StrCRC32(EncryptPwd_Text_RsaShaRc2B64(sIntData, _estr(pRegCode)))]);
  except
    Result := _estr(sBadKeyCode);
  end;
end;

function _rSW_GetHardwareId: string;
var
  i, iCount: Integer;
  sDriveId, sBaseId, sBiosId, sFullId: string;
begin
  try
    sBiosId := Trim(WmiGetOne_GetFirst('Win32_BIOS', 'Version'));
  except
    sBiosId := '0CFCBD38108A';
  end;
  try
    sBaseId := Trim(WmiGetOne_GetFirst('Win32_BaseBoard', 'SerialNumber'));
  except
    sBaseId := '48EE9DC19F2A82DC2AD';
  end;
  try
    sDriveId := Trim(WmiGetOne_GetFirst('Win32_DiskDrive', 'SerialNumber'));
  except
    sDriveId := 'EF0236DB81B04FF2B931170C70720BC8';
  end;

  sFullId := AnsiUpperCase(sDriveId + sBiosId + sBaseId);
  iCount := Length(sFullId);
  for i := iCount downto 1 do
    if not (sFullId[i] in ['0'..'9', 'A'..'Z']) then
      Delete(sFullId, i, 1);

  Result := Format('%.8x', [StrCRC32(sFullId)]);
end;

procedure _rSW_InitKey(const fAppId: string; var fKey: rKeyData);
var
  Y, M, D: Word;
begin
  FillChar(fKey, SizeOf(rKeyData), #0);

  DecodeDate(Date, Y, M, D);

  fKey.sAppId := fAppId;
  fKey.sHardId := _rSW_GetHardwareId;
  fKey.fMode := rmTrialRuns;
  fKey.sRegName := _estr(rsUserTrial);
  fKey.iCount := 0;
  fKey.wLR_Day := D;
  fKey.wLR_Month := M;
  fKey.wLR_Year := Y;
  fKey.sRegCode := rShareware_GenCode(fKey);
end;

function _rSW_IsTrialKey(const fKey: rKeyData): Boolean;
begin
  Result := fKey.fMode <> rmFull;
end;

function _rSW_IsGoodKey(const fInitKey, fChckKey: rKeyData; const bCheckName, bCheckHard, bShowErrors: Boolean): TKeyState;
begin
  if not AnsiSameText(fInitKey.sAppId, fChckKey.sAppId) then
  begin
    Result := ksBadAppId;
    if bShowErrors then ErrorBox(_estr(rsErrBadAppId));
    Exit;
  end;
  if bCheckHard and (fChckKey.fMode = rmFull)
  and not AnsiSameText(fInitKey.sHardId, fChckKey.sHardId) then
  begin
    Result := ksBadCompId;
    if bShowErrors then ErrorBox(_estr(rsErrBadCompId));
    Exit;
  end;
  if bCheckName and (fChckKey.fMode = rmFull) 
  and not AnsiSameText(fInitKey.sRegName, fChckKey.sRegName) then
  begin
    Result := ksBadUserId;
    if bShowErrors then ErrorBox(_estr(rsErrBadUserId));
    Exit;
  end;
  if bCheckHard and (fChckKey.fMode = rmFull)
  and not AnsiSameText(fChckKey.sRegCode, rShareware_GenCode(fChckKey)) then
  begin
    Result := ksBadKey;
    if bShowErrors then ErrorBox(_estr(rsErrBadKey));
    Exit;
  end;

  if fChckKey.fMode = rmFull then
  begin
    Result := ksFull;
  end
  else begin
    if fChckKey.iCount > 0
    then Result := ksTrial
    else Result := ksExpired;
  end;
end;

procedure _rSW_KeyFixExecute(var fKey: rKeyData; const bSaveChanges: Boolean);
var
  Y, M, D: Word;
  P: Integer;
begin
  if (fKey.fMode <> rmFull) and (fKey.iCount > -1) then
  begin
    if fKey.fMode = rmTrialDays then
    begin
      DecodeDate(Date, Y, M, D);
      if (fKey.wLR_Day = 0) or (fKey.wLR_Month = 0) or (fKey.wLR_Year = 0) then
      begin
        fKey.wLR_Day := D;
        fKey.wLR_Month := M;
        fKey.wLR_Year := Y;
      end;

      if (fKey.wLR_Day <> D) or (fKey.wLR_Month <> M) or (fKey.wLR_Year <> Y) then
      begin
        P := Abs((Y * 10000 + M * 100 + D) - (fKey.wLR_Year * 10000 + fKey.wLR_Month * 100 + fKey.wLR_Day));
        fKey.iCount := fKey.iCount - P;

        fKey.wLR_Day := D;
        fKey.wLR_Month := M;
        fKey.wLR_Year := Y;
      end;
    end
    else begin
      fKey.iCount := fKey.iCount - 1;
    end;
  end;

  if bSaveChanges then
    rShareware_SaveRegKey(fKey);
end;

procedure _rSW_StoreKeyData(var fKey: rKeyData; const sFileName: string; const bFixRun: Boolean);
begin
  if bFixRun then
    _rSW_KeyFixExecute(fKey, False);
  rShareware_SaveRegKey(fKey);

  if _rSW_IsTrialKey(fKey) then
  begin
    try
      DeleteFile(PAnsiChar(sFileName));
    except
    end;
  end
  else InfoBox(_estr(rsMsgRegistred));
end;

function _rSW_StoreKeyFile(var fKey: rKeyData; const sFileName: string; const bFixRun: Boolean): Boolean;
begin
  Result := False;
  if rShareware_ReadFileKey(sFileName, fKey) then
  begin
    _rSW_StoreKeyData(fKey, sFileName, bFixRun);
    Result := True;
  end;
end;

function _rSW_EnterRegFile(const fKeyInit: rKeyData; var fKeyData: rKeyData): Boolean;
begin
  Result := False;
  with TFormRegister.Create(Application) do
  begin
    try
      StartWait;
      try
        edAppId.Text := fKeyInit.sAppId;
        edCompId.Text := fKeyInit.sHardId;
      finally
        StopWait;
      end;
      if ShowModal = mrOk then
      begin
        StartWait;
        try
          Result := _rSW_StoreKeyFile(fKeyData, edRegFile.FileName, True);
        finally
          StopWait;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

(*
function _rSW_ExpiredTrialPeriod(const fKey: rKeyData): Boolean;
begin
  Result := True;
  if (fKey.fMode <> rmFull) and (fKey.iCount < 1) then
  begin
    Result := False;
    ErrorBox(_estr(rsErrBadTrialExpired))
  end;
end;
*)

function _rSW_WarningTrialPeriod(const fKey: rKeyData): Boolean;
begin
  Result := False;
  if fKey.fMode <> rmFull then
  begin
    if fKey.iCount > 0 then
    begin
      case fKey.fMode of
        rmTrialDays:
          Result := WarningBoxNY(Format(_estr(rsMsgBadTrialDate), [fKey.iCount])) = ID_YES;
        rmTrialRuns:
          Result := WarningBoxNY(Format(_estr(rsMsgBadTrialRuns), [fKey.iCount])) = ID_YES;
      end;
    end
    else Result := True;
  end;
end;

function rShareware_InitProtector(const fAppId: string): Boolean;
var
  fDefKey: rKeyData;
  bFileKey: Boolean;
  sKeyFile: string;
begin
  ShowWaitMsg(_estr(rsMsgKeyInit));
  try
    // Инициализируем переменные
    _rSW_InitKey(fAppId, fDefKey);
    Move(fDefKey, fAppKey, SizeOf(fDefKey));

    // Генерируем имя ключа "по умолчанию"
    sKeyFile := ChangeFileExt(ParamStr(0), _estr(sExtKeyFile));

    // Пытаемся прочитать ключ из реестра (который д.б. записан туда ранее)
    bFileKey := not rShareware_ReadRegKey(fAppKey);
    if bFileKey then
    begin
      // Если в реестре ничего нет - ищем триальный ключ на диске
      if rShareware_ReadFileKey(sKeyFile, fAppKey) then
      begin
        if _rSW_IsGoodKey(fDefKey, fAppKey, False, True, False) in [ksTrial] then
          _rSW_StoreKeyData(fAppKey, sKeyFile, False);
      end;
    end;

    // Проверяем корректность загруженного ключа
    case _rSW_IsGoodKey(fDefKey, fAppKey, False, bFileKey, True) of
      ksFull:
      begin
        Result := True;

        // Фиксируем запуск
        _rSW_KeyFixExecute(fAppKey, True);
      end;
      ksTrial:
      begin
        Result := True;

        // Фиксируем запуск
        _rSW_KeyFixExecute(fAppKey, True);

        // Напоминание о регистрации, если согласились - регистрация
        if _rSW_WarningTrialPeriod(fAppKey) then
        begin
          if _rSW_EnterRegFile(fDefKey, fAppKey) then
            Result := True;
        end;
      end;
      else begin
        Result := _rSW_EnterRegFile(fDefKey, fAppKey);
      end;
    end;
  finally
    CloseWaitMsg;
  end;
end;

function rShareware_IsTrial: Boolean;
begin
  Result := _rSW_IsTrialKey(fAppKey);
end;

function rShareware_RegName: string;
begin
  Result := fAppKey.sRegName;
end;

{TFormRegister}

procedure TFormRegister.FormCreate(Sender: TObject);
begin
  Font.Name := Screen.MenuFont.Name;
  TranslateComponent(Self);

  ButtonsChange(nil);
end;

procedure TFormRegister.ButtonsChange(Sender: TObject);
begin
  btnCopy.Enabled := edRegName.Text <> '';
  btnReg.Enabled := (edRegName.Text <> '') and FileExists(edRegFile.FileName);
end;

procedure TFormRegister.btnCopyClick(Sender: TObject);
begin
  (* 2016-02-28 ----------------------------------------------------------------
  PutStringIntoClipBoard(Format('AppId: "%s"'#13'HwdCode: "%s"'#13'RegName: "%s"',
    [edAppId.Text, edCompId.Text, edRegName.Text]));
  InfoBox(Format('AppId: "%s"'#13'HwdCode: "%s"'#13'RegName: "%s"',
    [edAppId.Text, edCompId.Text, edRegName.Text]));
  -- 2016-02-28 ---------------------------------------------------------------- *)
  PutStringIntoClipBoard(Format('AppName: "%s"'#13'HwdCode: "%s"'#13'RegName: "%s"',
    [ChangeFileExt(ExtractFileName(ParamStr(0)), ''), edCompId.Text, edRegName.Text]));
  InfoBox(Format('AppName: "%s"'#13'HwdCode: "%s"'#13'RegName: "%s"',
    [ChangeFileExt(ExtractFileName(ParamStr(0)), ''), edCompId.Text, edRegName.Text]));
end;

procedure TFormRegister.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  fDefKey, fKey: rKeyData;
begin
  CanClose := ModalResult <> mrOk;
  if not CanClose then
  begin
    fDefKey.sAppId := edAppId.Text;
    fDefKey.sHardId := edCompId.Text;
    fDefKey.sRegName := edRegName.Text;

    rShareware_ReadFileKey(edRegFile.FileName, fKey);

    case _rSW_IsGoodKey(fDefKey, fKey, True, True, True) of
      ksFull:
      begin
        CanClose := True;
      end;
      ksTrial, ksExpired:
      begin
        CanClose := False;
        ErrorBox(_estr(rsErrBadKey));
      end;
      else begin
        CanClose := False;
      end;
    end;
  end;
end;

end.































