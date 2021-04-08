unit RSelfUpdate;

interface

uses
  Types;

function rSU_SelfUpdate(const sUrlSelfUpdate: string; const bSilentMode: Boolean; const LngId: DWord): Boolean;

const
  rSU_TempMask              = '*.~*';
  rSU_TempCmd               = 'restart.cmd';

implementation

uses
  Windows, Classes, SysUtils, StrUtils, Forms, IniFiles, RxVerInf, RxStrUtils,
  RVclUtils, RDialogs, RWait, RMsgRu, RMsgTypes, RCRC32,
  RFileProcs, RHttpUtils;

resourcestring
  sMsgCheckSelfUpdates      = 'Поиск доступных обновлений...';
  sErrCheckSelfUpdates      = 'Ошибка поиска обновлений'#13#13'%s';
  sQryCheckSelfUpdates      = 'Найдена новая версия программы: %s'#13 +
                              'Текущая версия программы %s'#13#13 +
                              'Выполнить установку обновления сейчас (программа будет перезапущена)?';

  sQryCheckSelfUpdatesEx    = 'Найдена новая версия программы: %s'#13 +
                              'Текущая версия программы %s'#13#13 +
                              'Внесенные изменения:'#13 +
                              '-------------------------------------------------'#13 +
                              '%s' +
                              '-------------------------------------------------'#13#13 +
                              'Выполнить установку обновления сейчас (программа будет перезапущена)?';

  sMsgLatestVersion         = 'Обновлений не найдено!';
  sMsgSelfUpdate            = 'Загрузка и установка обновления "%s"...';
  sErrSelfUpdate            = 'Ошибка установки обновления "%s"'#13#13'%s';
  SErrFileBadCrc32          = 'Ошибка загрузки файла обновления: неверная контрольная сумма скачанного файла!';
  sMsgSelfUpdateOk          = 'Установка обновления "%s" успешно выполнена.'#13#13'Для завершения установки необходимо перезапустить программу!';

const
  fnUpdDir                  = 'UpdDescr';
  fnUpdDescSmp              = 'UpdDescr_%s.txt';
  fnUpdDescLng              = 'UpdDescr_%d_%s.txt';

function rSU_SelfUpdate(const sUrlSelfUpdate: string; const bSilentMode: Boolean; const LngId: DWord): Boolean;
var
  VerInfo: TVersionInfo;
  sCfgFile, sExeFile, sDscFile, sDscUrl, sUpdFile, sSfxFile, sUpdPath: string;
  sVerExe, sVerId, sVerDescr, sVerFileUrl, sVerOriginal, sVerCrc32: string;
  sQueryText: string;
  slDscVers: TStringList;
  fTr: TRTransaction;
  fOp: TROperation;
  iExitCode: Integer;
  Ini: TIniFile;
  fCmd: TextFile;

  function ExtractBuils(const sVer: string): Integer;
  begin
    Result := StrToIntDef(Trim(ExtractWord(4, sVer, ['.', ',', '-', '_'])), 0);
  end;

begin
  Result := False;

  sDscFile := '';
  slDscVers := TStringList.Create;
  try
    // Генерируем имя файла конфигурации
    sExeFile := ParamStr(0);
    sCfgFile := ExtractFilePath(sExeFile) + rHttp_ExtractFileName(sUrlSelfUpdate);
    try
      StartWait;
      ShowWaitMsg(sMsgCheckSelfUpdates);
      try
        try
          // Удаляем временные файлы, если они еще остались
          try
            DeleteMask(ExtractFilePath(sExeFile) + rSU_TempMask, [], ezOk, [], 1, 0, nil, nil, nil);
          except
          end;

          // Определяем текущую версию программы
          VerInfo := AppVerInfo;
          try
            sVerExe := VerInfo.FileVersion;
          finally
            VerInfo.Free;
          end;

          // Скачиваем файл конфигурации
          rHttp_DownloadFile(sUrlSelfUpdate, sCfgFile, rHttp_IsSSL(sUrlSelfUpdate));

          // Читаем данные из файла конфигурации
          Ini := TIniFile.Create(sCfgFile);
          try
            sVerId := Ini.ReadString(ExtractFileName(sExeFile), 'VerId', sVerExe);
            sVerDescr := Ini.ReadString(ExtractFileName(sExeFile), 'Description', '');
            sVerFileUrl := Ini.ReadString(ExtractFileName(sExeFile), 'DownloadUrl', '');
            sVerOriginal := Ini.ReadString(ExtractFileName(sExeFile), 'OriginalFileName', '');
            sVerCrc32 := Ini.ReadString(ExtractFileName(sExeFile), 'ChecksumCRC32', '');
          finally
            Ini.Free;
          end;
        except
          on E: Exception do
          begin
            if not bSilentMode then
              ErrorBox(Format(sErrCheckSelfUpdates, [E.Message]));
            Exit;
          end;
        end;
      finally
        CloseWaitMsg;
        StopWait;
      end;

      // Сравниваем версии
      if (ExtractBuils(sVerExe) < ExtractBuils(sVerId)) and (sVerFileUrl <> '') then
      begin
        {*** 2016-12-06, ticket 6255, version 9.1.3.125 ************************ }

        // Пробуем загрузить описание версии
        try
          // Формируем имена файлов
          if LngId = 0 then
          begin
            sDscUrl := rHttp_ExtractFilePath(sUrlSelfUpdate) + fnUpdDir + '/'
              + Format(fnUpdDescSmp, [LngId, sVerId]);
            sDscFile := ExtractFilePath(sExeFile) + fnUpdDir + PathDelim
              + Format(fnUpdDescSmp, [LngId, sVerId]);
          end
          else begin
            sDscUrl := rHttp_ExtractFilePath(sUrlSelfUpdate) + fnUpdDir + '/'
              + Format(fnUpdDescLng, [LngId, sVerId]);
            sDscFile := ExtractFilePath(sExeFile) + fnUpdDir + PathDelim
              + Format(fnUpdDescLng, [LngId, sVerId]);
          end;

          // Создаем каталог с описаниями
          wForceDirectories(ExtractFilePath(sExeFile) + fnUpdDir);

          // Пробуем скачать файл описания версии
          rHttp_DownloadFile(sDscUrl, sDscFile, rHttp_IsSSL(sDscUrl));

          // Загружаем описание, если что-то скачалось
          if wFileExists(sDscFile) then
            slDscVers.LoadFromFile(sDscFile);
        except
          slDscVers.Clear;
        end;

        // Спрашиваем пользователя
        if slDscVers.Text = ''
        then sQueryText := Format(sQryCheckSelfUpdates, [sVerDescr, sVerExe])
        else sQueryText := Format(sQryCheckSelfUpdatesEx, [sVerDescr, sVerExe, slDscVers.Text]);

        {*** 2016-12-06, ticket 6255, version 9.1.3.125 ************************ }

        if QueryBoxStdNY(sQueryText) = ID_YES then
        begin
          // Начинаем обновление
          StartWait;
          ShowWaitMsg(Format(sMsgSelfUpdate, [sVerDescr]));
          try
            // Генерируем имена файлов обновления
            sUpdPath := ExtractFilePath(sExeFile) + 'SelfUpdate\';
            sUpdFile := sUpdPath + rHttp_ExtractFileName(sVerFileUrl);
            sSfxFile := sUpdPath + sVerOriginal;
            try
              try
                // Удаляем временный каталог, если он был ("на всякий случай")
                if wDirectoryExists(ExcludeTrailingPathDelimiter(sUpdPath)) then
                  DeleteMask(ExcludeTrailingPathDelimiter(sUpdPath),
                    [rfSrcSubDirs], ezOk, [], 3, 1, nil, nil, nil);

                // Создаем временный каталог для обновления
                ForceDirsEx(sUpdPath, False, nil, nil);

                // Скачиваем файл
                if wFileExists(sUpdFile) then
                begin
                  try
                    DeleteFile(sUpdFile);
                  except
                  end;
                end;
                rHttp_DownloadFile(sVerFileUrl, sUpdFile, rHttp_IsSSL(sVerFileUrl));

                // Проверим, скачался ли файл
                if not wFileExists(sUpdFile) then
                  raise Exception.CreateFmt(SErrFileNotFound, [sUpdFile]);

                // Проверяем контрольную сумму файла
                if (sVerCrc32 <> '') and not SameText(FileCRC32_STR(sUpdFile), sVerCrc32) then
                  raise Exception.Create(SErrFileBadCrc32);

                // Переименовываем скачанный файл
                fTr := RenameFileEx(sUpdFile, sSfxFile, nil, nil);
                if fTr.State <> msOk then raise Exception.Create(FormatTransaction(fTr));

                // На всякий еще раз проверим, есть ли такой файл
                if not wFileExists(sSfxFile) then
                  raise Exception.CreateFmt(SErrFileNotFound, [sSfxFile]);

                // Распаковываем архив (выполняем его)
                fTr := ExecuteFileEx(sSfxFile, ExtractFilePath(sSfxFile), SW_SHOWNORMAL, True, True, 60, iExitCode, '', '', nil, nil);
                if fTr.State <> msOk then raise Exception.Create(FormatTransaction(fTr));

                // Удаляем сам архив из временной папки
                iExitCode := 1;
                while wFileExists(sSfxFile) do
                begin
                  fTr := DeleteFileEx(sSfxFile, [], nil, nil);
                  if fTr.State <> msOk then
                  begin
                    Inc(iExitCode);
                    if iExitCode > 100 then
                      raise Exception.Create(FormatTransaction(fTr));
                    Delay(100);
                  end;
                end;

                // Проверим, появились ли файлы во временной папке
                if wDirectoryIsEmpty(sUpdPath) then
                  raise Exception.CreateFmt(SErrFileNotFound, [sUpdPath + MaskAll]);

                // Переписываем файлы из временной папки в основную
                fOp := CopyFilesMask(sUpdPath, ExtractFilePath(sExeFile), MaskAll, False,
                  [rfForceDirs, rfSrcSubDirs, rfOverWrite, rfCopyLocked, rfCheckCopy,
                   rfCopyFileDate, rfCopyFileAttr, rfCompareDate, rfCompareSize, rfCompareCrc32],
                  ezError, [esErrorStop, esErrorShow], nil, 1024, 100, 1, 1, nil, nil, nil, nil, nil);
                if fOp.State <> msOk then raise Exception.Create(FormatOperation(fOp));

                // Выводим сообщение и перезапускаем программу
                CloseWaitMsg;
                CautionBox(Format(sMsgSelfUpdateOk, [sVerDescr]));

                // Создаем cmd-файл для отложенного перезапуска
                AssignFile(fCmd, ExtractFilePath(sExeFile) + rSU_TempCmd);
                Rewrite(fCmd);
                try
                  WriteLn(fCmd, '@echo off');
                  WriteLn(fCmd, 'echo restart "' + ExtractFileName(sExeFile) + '"...');
                  WriteLn(fCmd, '');
                  WriteLn(fCmd, ':scan_process');
                  WriteLn(fCmd, 'ping 1.0.0.0 -n 1 -w 500 > nul');
                  WriteLn(fCmd, 'call tasklist > restart.lst');
                  WriteLn(fCmd, 'find /i "' + ExtractFileName(sExeFile) + '" restart.lst > nul');
                  WriteLn(fCmd, 'if %errorlevel%==0 goto scan_process');
                  WriteLn(fCmd, '');
                  WriteLn(fCmd, 'del restart.lst /q');
                  WriteLn(fCmd, 'start ' + ExtractFileName(sExeFile));
                finally
                  CloseFile(fCmd);
                end;
                ExecuteFileEx(rSU_TempCmd, ExtractFilePath(sExeFile), SW_HIDE, False, False, 0, iExitCode, '', '', nil, nil);

                // Завершаем работу
                Application.Terminate;
                // ExecuteFileEx(sExeFile, ExtractFilePath(sExeFile), SW_SHOWNORMAL, False, False, 0, iExitCode, '', '', nil, nil);

                // Ставим "сигнал", что все ОК
                Result := True;
              except
                on E: Exception do
                  ErrorBox(Format(sErrSelfUpdate, [sVerDescr, E.Message]));
              end;
            finally
              // Удаляем временный каталог
              if wDirectoryExists(ExcludeTrailingPathDelimiter(sUpdPath)) then
                DeleteMask(ExcludeTrailingPathDelimiter(sUpdPath),
                  [rfSrcSubDirs], ezOk, [], 3, 1, nil, nil, nil);
            end;
          finally
            CloseWaitMsg;
            StopWait;
          end;
        end;
      end
      else begin
        if not bSilentMode then
          InfoBox(sMsgLatestVersion);
      end;
    finally
      // Удаляем скаченные файлы
      if FileExists(sCfgFile) then
        DeleteFileEx(sCfgFile, [rfDstDirAuto], nil, nil);
      if (sDscFile <> '') and FileExists(sDscFile) then
        DeleteFileEx(sDscFile, [rfDstDirAuto], nil, nil);
    end;
  finally
    slDscVers.Free;
  end;
end;

end.
