unit AdminUnit;

interface

uses
  SysUtils, Classes, ImgList, Controls, ActnList, DB, ADODB, Windows, TmplMdiDb,
  RDbData;

type
  TAdminData = class(TDataModule)
    AdminImageList: TImageList;
    sr_levels: TADOTable;
    sr_operations: TADOTable;
    sr_levelsid: TIntegerField;
    sr_levelsname: TStringField;
    sr_levelsnotes: TStringField;
    sr_levelshidden: TBooleanField;
    sr_levelsfont_style: TIntegerField;
    sr_levelsfont_color: TIntegerField;
    sr_levelscell_color: TIntegerField;
    sr_operationsid: TIntegerField;
    sr_operationsid_levels: TIntegerField;
    sr_operationsname_levels: TStringField;
    sr_operationsnotes_levels: TStringField;
    sr_operationshidden: TBooleanField;
    sr_operationsname: TStringField;
    sr_operationsnotes: TStringField;
    sr_operationsfont_style: TIntegerField;
    sr_operationsfont_color: TIntegerField;
    sr_operationscell_color: TIntegerField;
  private
  public
    function InitAdminData: Boolean;
    procedure SysLogSave;
    procedure SysLogClear;
  end;

var
  AdminData: TAdminData;

implementation

{$R *.dfm}

uses
  Dialogs, Forms, RVclUtils, RMsgRu, RDialogs, RRssConst, RExHandlers, RFileExport,
  RDbConst, RDbUtils, RDbLog, RMdiIntf, BaseDbUnit, OprList, AdminVars;

resourcestring
  SLoadSrLevels     = 'Загрузка справочника уровней безопасности операций...';
  SLoadSrOpers      = 'Загрузка справочника операций...';

  EInitAdminData    = 'Ошибка инициализации модуля TAdminData!';

{ == Инициализация модуля ====================================================== }
function TAdminData.InitAdminData: Boolean;
begin
  Result := True;
  try
    Result := BaseData.OpenReference(SR_LEVELS, SLoadSrLevels, tagEditSrLevels);
    Application.ProcessMessages;
    if Result then Result := BaseData.OpenReference(SR_OPERATIONS, SLoadSrOpers, tagViewSrOperations);
    Application.ProcessMessages;
  except
    on E: Exception do
      HandleExcept(E, Self, EInitAdminData);
  end;
end;

{ == Работа с журналом аудита ================================================== }
procedure TAdminData.SysLogSave;
const
  sqlLoad = 'SELECT dateoper, id_operations, id_users, su_users.name, ' +
            'sr_workplases.name_s, info, host, netuser ' +
            'FROM vss_syslog, su_users, sr_workplases ' +
            'WHERE (vss_syslog.id_users = su_users.id) AND ' +
            '(vss_syslog.id_workplases = sr_workplases.id) AND (dateoper<%s) ' +
            'ORDER BY dateoper';
var
  Qry: TAdoQuery;
  ExpFileName: string;
  ExpCount: Integer;
begin
  Qry := nil;
  try
    try
      StartWait;
      ShowInStatusBar(SMsgLoadDataWait);
      try
        Qry := OpenDynamicQuery(BaseData.acDb, Format(sqlLoad,
          [DateToSqlStr(BaseData.DbParameters.DateFormat, Date)]));
      finally
        ShowInStatusBar(SMsgExportDataWait);
        StopWait;
      end;
      if DataSetIsOpen(Qry) then
      begin
        ExpCount := ExportDataSetToCsv(Qry, ExpFileName);
        if ExpCount > -1
        then AddToDbLog(tagViewSysLog, Format(SLogSaveSysLog, [Qry.RecordCount, ExpFileName]));
      end;
    except
      on E: Exception do
        HandleSqlExcept(E, Self, Format(sqlLoad,
          [DateToSqlStr(BaseData.DbParameters.DateFormat, Date)]), SErrSysLogSave);
    end;
  finally
    FreeDynamicQuery(Qry);
  end;
end;

procedure TAdminData.SysLogClear;
const
  sqlCount  = 'SELECT Count(*) AS cnt FROM ss_syslog WHERE dateoper<%s';
  sqlDelete = 'DELETE FROM ss_syslog WHERE dateoper<%s';
var
  Qry: TAdoQuery;
  Count: Integer;
  CurrSql: string;
  QryRes: Integer;
begin
  QryRes := QueryBoxStdYNC(SQuerySaveSysLog);
  if QryRes = ID_YES then
    SysLogSave;
  if QryRes <> ID_CANCEL then
  begin
    try
      CurrSql := EmptyStr;
      StartWait;
      ShowInStatusBar(SMsgClearSysLog);
      try
        Qry := nil;
        CurrSql := Format(sqlCount, [DateToSqlStr(BaseData.DbParameters.DateFormat, Date)]);
        try
          Qry := OpenDynamicQuery(BaseData.acDb, CurrSql);
          if DataSetIsNotEmpty(Qry)
          then Count := Qry.FieldByName(fnCOUNT).AsInteger
          else Count := 0;
        finally
          FreeDynamicQuery(Qry);
        end;
        CurrSql := Format(sqlDelete, [DateToSqlStr(BaseData.DbParameters.DateFormat, Date)]);
        if Count > 0 then begin
          if ExecDynamicQuery(BaseData.acDb, CurrSql) then
          begin
            AddToDbLog(tagClearSysLog, Format(SLogClearSysLog, [Count, DateToStr(Date)]));
            InfoBox(SMsgOperationComplete);
          end;
        end
        else ErrorBox(SErrDataSetIsEmpty);
      finally
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;
    except
      on E: Exception do
        HandleSqlExcept(E, Self, CurrSql, SErrClearSysLog);
    end;
  end;
end;

end.
