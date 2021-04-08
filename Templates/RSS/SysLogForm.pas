unit SysLogForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplQuery, Menus, RDbStatus, RDbCustom, RDbGridTuner, RDbFind,
  RDbOrder, RDbFilter, DB, ADODB, DBActns, ActnList, ImgList, RDbPanel,
  Grids, DBGrids, RDbColorGrid, ExtCtrls, ComCtrls, ToolWin, StdCtrls,
  RDbText, RDbEditor, RDbCustomSearch, RDbSearch, RDbUpdater, Buttons, Tabs;

type
  TFormSysLog = class(TQueryTemplate)
    RDbFilter_dateoper: TRDFDateItem;
    RDbFilter_id_operations: TRDFListLinkItem;
    RDbFilter_id_users: TRDFListLinkItem;
    IDRDbTextLabel: TLabel;
    dtDateOper: TRDbText;
    NAMERDbTextLabel: TLabel;
    deOperName: TRDbText;
    NOTESRDbTextLabel: TLabel;
    dtInfo255: TRDbText;
    deOperId: TRDbText;
    ID_USERSRDbTextLabel: TLabel;
    dtUserId: TRDbText;
    deUserNotes: TRDbText;
    dtUserLogin: TRDbText;
    deUserName: TRDbText;
    RDbFilter_id_levels: TRDFListLinkItem;
    ss_syslog: TADOQuery;
    ss_syslogdateoper: TDateTimeField;
    ss_syslogid_operations: TIntegerField;
    ss_syslogid_levels: TIntegerField;
    ss_syslogid_users: TIntegerField;
    ss_syslogid_workplases: TIntegerField;
    ss_sysloginfo255: TStringField;
    ss_sysloghost: TStringField;
    ss_syslognetuser: TStringField;
    ss_syslogname_operations: TStringField;
    ss_syslognotes_operations: TStringField;
    ss_syslogname_levels: TStringField;
    ss_syslognotes_levels: TStringField;
    ss_syslogfont_style: TIntegerField;
    ss_syslogfont_color: TIntegerField;
    ss_syslogcell_color: TIntegerField;
    ss_syslogname_s_wp: TStringField;
    ss_sysloglogin: TStringField;
    ss_syslogfullname: TStringField;
    ss_sysloguser_notes: TStringField;
    NAME_S_WPRDbTextLabel: TLabel;
    dtWpNameS: TRDbText;
    NAME_OPERATIONSRDbTextLabel: TLabel;
    dtHost: TRDbText;
    dtNetuser: TRDbText;
    deWpNameF: TRDbText;
    ss_syslogname_wp: TStringField;
    RDbFilter_id_workplases: TRDFListLinkItem;
    RDbFilter_host: TRDFStringItem;
    RDbFilter_netuser: TRDFStringItem;
    itemSysLogSave: TMenuItem;
    itemSysLogClear: TMenuItem;
    itemSysLogSaveO: TMenuItem;
    itemSysLogClearO: TMenuItem;
    itemSysLogSaveP: TMenuItem;
    itemSysLogClearP: TMenuItem;
    SysLogClear: TAction;
    SysLogSave: TAction;
    ss_sysloginfo: TMemoField;
    RDbFilter_info: TRDFTextItem;
    procedure RDbEditorGetEditRights(Sender: TObject;
      const Mode: TEditMode; var Enable: Boolean);
    procedure RDbEditorGetEditorClass(Sender: TObject;
      var EditorClass: TFormClass);
    procedure SysLogSaveUpdate(Sender: TObject);
    procedure SysLogSaveExecute(Sender: TObject);
    procedure SysLogClearUpdate(Sender: TObject);
    procedure SysLogClearExecute(Sender: TObject);
    procedure ss_syslogCalcFields(DataSet: TDataSet);
  protected
    function LoadDataForm: Boolean; override;
  public
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, BaseDbUnit, AdminUnit, OprList, SysLogProp, RDialogs;

const
  SqlSysLog   = 'SELECT * FROM vss_syslog';

function TFormSysLog.LoadDataForm: Boolean;
begin
  Result := BaseData.OpenVariableQuery(ss_syslog, RDbFilter, RDbOrder,
    SqlSysLog, EmptyStr, tagViewSysLog);
end;

procedure TFormSysLog.ss_syslogCalcFields(DataSet: TDataSet);
begin
  ss_sysloginfo255.AsString := Copy(ss_sysloginfo.AsString, 1, 255);
end;

procedure TFormSysLog.RDbEditorGetEditRights(Sender: TObject;
  const Mode: TEditMode; var Enable: Boolean);
begin
  Enable := False;
end;

procedure TFormSysLog.RDbEditorGetEditorClass(Sender: TObject;
  var EditorClass: TFormClass);
begin
  EditorClass := TFormSysLogProp;
end;

procedure TFormSysLog.SysLogSaveUpdate(Sender: TObject);
begin
  SysLogSave.Enabled := IsNotWait and orViewSysLog and BaseData.acDb.Connected;
end;

procedure TFormSysLog.SysLogSaveExecute(Sender: TObject);
begin
  AdminData.SysLogSave;
end;

procedure TFormSysLog.SysLogClearUpdate(Sender: TObject);
begin
  SysLogClear.Enabled := IsNotWait and orClearSysLog and BaseData.acDb.Connected;
end;

procedure TFormSysLog.SysLogClearExecute(Sender: TObject);
begin
  AdminData.SysLogClear;
end;

end.
