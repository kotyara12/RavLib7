unit ReportsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, Grids, DBGrids, DB, RDbData,
  RDbEditor, ADODB, frxClass, frxDesgn, ActnList, Menus, frxExportTXT,
  frxExportMail, frxExportImage, frxExportXLS, frxExportRTF, frxExportHTML, frxDBSet,
  frxGZip, ImgList, frxDCtrl, frxGradient, frxChBox, frxCross, frxRich,
  frxBarcode, frxExportODF, frxExportXML, frxExportPDF, frxExportText,
  frxChart, frxADOComponents, RDbCustom, RDbGridTuner, RDbOrder;

type
  TFormReports = class(TDialogTemplate)
    lblDBGrid: TLabel;
    DBGrid: TDBGrid;
    ss_reports: TADOQuery;
    ss_reportsid: TIntegerField;
    ss_reportsreport: TBlobField;
    ss_reportsname: TStringField;
    ss_reportsnotes: TStringField;
    frxReport: TfrxReport;
    frxDesigner: TfrxDesigner;
    ActionList: TActionList;
    PopupMenu: TPopupMenu;
    itemReportNew: TMenuItem;
    frxHTMLExport: TfrxHTMLExport;
    frxRTFExport: TfrxRTFExport;
    frxXLSExport: TfrxXLSExport;
    frxJPEGExport: TfrxJPEGExport;
    frxMailExport: TfrxMailExport;
    frxTXTExport: TfrxTXTExport;
    frxSimpleTextExport: TfrxSimpleTextExport;
    frxPDFExport: TfrxPDFExport;
    frxXMLExport: TfrxXMLExport;
    frxODSExport: TfrxODSExport;
    frxODTExport: TfrxODTExport;
    frxGZipCompressor: TfrxGZipCompressor;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ss_reportsform: TStringField;
    ImageList: TImageList;
    frxBarCodeObject: TfrxBarCodeObject;
    frxRichObject: TfrxRichObject;
    frxCrossObject: TfrxCrossObject;
    frxCheckBoxObject: TfrxCheckBoxObject;
    frxGradientObject: TfrxGradientObject;
    frxDialogControls: TfrxDialogControls;
    frxADOComponents: TfrxADOComponents;
    frxChartObject: TfrxChartObject;
    RDbEditor: TRDbExportEditor;
    RDbGridTuner: TRDbGridTuner;
    ReportNew: TAction;
    ReportClone: TAction;
    ReportEdit: TAction;
    ReportDesign: TAction;
    ReportImport: TAction;
    ReportExport: TAction;
    ReportDelete: TAction;
    ReportTuneGrid: TAction;
    ReportRefresh: TAction;
    itemReportClone: TMenuItem;
    itemReportEdit: TMenuItem;
    itemReportDelete: TMenuItem;
    N1: TMenuItem;
    itemReportDesign: TMenuItem;
    N2: TMenuItem;
    itemReportImport: TMenuItem;
    itemReportExport: TMenuItem;
    N3: TMenuItem;
    itemReportTuneGrid: TMenuItem;
    N4: TMenuItem;
    itemReportRefresh: TMenuItem;
    btnReportNew: TBitBtn;
    btnReportClone: TBitBtn;
    btnReportEdit: TBitBtn;
    btnReportDelete: TBitBtn;
    btnReportDesign: TBitBtn;
    ReportPrint: TAction;
    itemReportPrint: TMenuItem;
    btnReportImport: TBitBtn;
    btnReportExport: TBitBtn;
    btnReportRefresh: TBitBtn;
    btnReportPrint: TBitBtn;
    RDbOrder: TRDbOrder;
    ReportOrder: TAction;
    itemReportOrder: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ReportGetValue(const VarName: String; var Value: Variant);
    procedure RDbEditorGetNewKey(Sender: TObject; var Value: Integer);
    procedure RDbEditorFreeNewKey(Sender: TObject; var Value: Integer);
    procedure RDbEditorGetChangeLogMsg(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Text: String);
    procedure RDbEditorSaveToLog(Sender: TObject; const EditTag: Integer;
      const Text: String);
    procedure RDbEditorCreateNewRecord(Sender: TObject;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure RDbEditorGetEditorClass(Sender: TObject;
      var EditorClass: TFormClass);
    procedure RDbEditorGetEditRights(Sender: TObject;
      const Mode: TEditMode; var Enable: Boolean);
    procedure RDbEditorBeforeShowEditor(Sender: TObject; Editor: TForm;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    function DesignerLoadReport(Report: TfrxReport): Boolean;
    function DesignerSaveReport(Report: TfrxReport;
      SaveAs: Boolean): Boolean;
    procedure ReportNewUpdate(Sender: TObject);
    procedure ReportNewExecute(Sender: TObject);
    procedure ReportCloneUpdate(Sender: TObject);
    procedure ReportCloneExecute(Sender: TObject);
    procedure ReportEditUpdate(Sender: TObject);
    procedure ReportEditExecute(Sender: TObject);
    procedure ReportDesignUpdate(Sender: TObject);
    procedure ReportDesignExecute(Sender: TObject);
    procedure ReportDeleteUpdate(Sender: TObject);
    procedure ReportDeleteExecute(Sender: TObject);
    procedure ReportImportUpdate(Sender: TObject);
    procedure ReportImportExecute(Sender: TObject);
    procedure ReportExportUpdate(Sender: TObject);
    procedure ReportExportExecute(Sender: TObject);
    procedure ReportTuneGridUpdate(Sender: TObject);
    procedure ReportTuneGridExecute(Sender: TObject);
    procedure ReportRefreshUpdate(Sender: TObject);
    procedure ReportRefreshExecute(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure ReportPrintUpdate(Sender: TObject);
    procedure ReportPrintExecute(Sender: TObject);
    procedure RDbEditorGetDeleteLogMsg(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Text: String);
    procedure RDbEditorGetInsertLogMsg(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Text: String);
    procedure frxDesignerShow(Sender: TObject);
    procedure ReportOrderUpdate(Sender: TObject);
    procedure ReportOrderExecute(Sender: TObject);
  private
    fFormId: string;
    fFormTag: Integer;
    fCanEditList: Boolean;
    fVars: TStringList;
    function LoadReport(Report: TfrxReport): Boolean;
    function SaveReport(Report: TfrxReport; const Logged: Boolean): Boolean;
    function OpenReports: Boolean;
    procedure CloseReports;
  end;

type
  TDataSets = array of TDataSet;

procedure CreateListDataSet(Form: TForm; var DSL: TDataSets);

procedure OpenReportsListCustom(Db: TAdoConnection; Form: TForm; DSL: TDataSets;
  const FormId: string; const FormTag: Integer;
  const FilterTree, FilterUser, Variables, DefaultPath: string; const EditEnable: Boolean);
procedure OpenReportsList(Db: TAdoConnection; Form: TForm; DSL: TDataSets;
  const FilterTree, FilterUser, Variables, DefaultPath: string; const EditEnable: Boolean);

implementation

{$R *.dfm}

uses
  RVclUtils, RDbConst, RDbUtils, RDbOpenDS, RDbState, RDbGetId, RDialogs, RMsgRu,
  {$IFDEF RSS} RDbLog, RRssConst, {$ENDIF}
  BaseDbUnit,
  RExHandlers, RptNameForm;

resourcestring
  SMsgReportsOpen         = 'Загрузка списка отчетов...';
  SMsgReportsClose        = 'Завершение генератора отчетов...';
  SMsgReportsLoad         = 'Загрузка отчета...';
  SMsgReportsSave         = 'Сохранение отчета...';
  SMsgReportsShow         = 'Предварительный просмотр отчета...';
  SMsgReportsDesign       = 'Запуск дизайнера отчетов...';

  SfVarsFilterTree        = 'FilterTree';
  SfVarsFilterUser        = 'FilterUser';

const
  sqlReportForm           = 'SELECT * FROM ss_reports WHERE form=''%s''';

{ == Подключаем все доступные наборы данных ==================================== }

procedure CreateListDataSet(Form: TForm; var DSL: TDataSets);
var
  i: Integer;

  function IsNewDataSet(DS: TDataSet): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := Low(DSL) to High(DSL) do
    begin
      if DSL[i] = DS then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

  procedure PutDataSet(DS: TDataSet);
  begin
    if Assigned(DS) and IsNewDataSet(DS) then
    begin
      SetLength(DSL, Length(DSL) + 1);
      DSL[High(DSL)] := DS;
    end;
  end;

begin
  for i := 0 to Form.ComponentCount - 1 do
  begin
    if Form.Components[i] is TDataSource then
      PutDataSet(TDataSource(Form.Components[i]).DataSet);
    if Form.Components[i] is TDataSet then
      PutDataSet(TDataSet(Form.Components[i]));
  end;
end;

procedure OpenReportsListCustom(Db: TAdoConnection; Form: TForm; DSL: TDataSets;
  const FormId: string; const FormTag: Integer;
  const FilterTree, FilterUser, Variables, DefaultPath: string; const EditEnable: Boolean);
var
  i: Integer;
  frxDataSet: TfrxDbDataSet;
begin
  with TFormReports.Create(Form) do
  begin
    try
      StartWait;
      ShowInStatusBar(SMsgPrepareOperation);
      try
        fFormId := FormId;
        fFormTag := FormTag;
        fCanEditList := EditEnable;
        ss_reports.Connection := Db;

        for i := Low(DSL) to High(DSL) do
        begin
          frxDataSet := TfrxDbDataSet.Create(frxReport.Owner);
          frxDataSet.DataSource := nil;
          frxDataSet.DataSet := DSL[i];
          frxDataSet.UserName := DSL[i].Name;
          frxReport.DataSets.Add(frxDataSet);
        end;

        if Variables <> EmptyStr then
          fVars.Text := Variables;
        fVars.Add(SfVarsFilterTree + fVars.NameValueSeparator + FilterTree);
        fVars.Add(SfVarsFilterUser + fVars.NameValueSeparator + FilterUser);

        frxPDFExport.DefaultPath := DefaultPath;
        frxHTMLExport.DefaultPath := DefaultPath;
        frxRTFExport.DefaultPath := DefaultPath;
        frxXLSExport.DefaultPath := DefaultPath;
        frxJPEGExport.DefaultPath := DefaultPath;
        frxMailExport.DefaultPath := DefaultPath;
        frxTXTExport.DefaultPath := DefaultPath;
        frxSimpleTextExport.DefaultPath := DefaultPath;
        frxXMLExport.DefaultPath := DefaultPath;
        frxODSExport.DefaultPath := DefaultPath;
        frxODTExport.DefaultPath := DefaultPath;
      finally
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;

      if OpenReports then
      begin
        try
          if (ShowModal = mrOk) then
          begin
            if RDbEditor.RecordCanOpened(True) and not ss_reportsREPORT.IsNull then
              ReportPrintExecute(nil);
          end;
        finally
          CloseReports;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure OpenReportsList(Db: TAdoConnection; Form: TForm; DSL: TDataSets;
  const FilterTree, FilterUser, Variables, DefaultPath: string; const EditEnable: Boolean);
begin
  OpenReportsListCustom(Db, Form, DSL, Form.Name, Form.Tag,
    FilterTree, FilterUser, Variables, DefaultPath, EditEnable);
end;

{ == Обработка событий формы =================================================== }

procedure TFormReports.FormCreate(Sender: TObject);
begin
  inherited;

  fVars := TStringList.Create;
end;

procedure TFormReports.FormDestroy(Sender: TObject);
begin
  fVars.Free;

  inherited;
end;

{ == Загрузка отчета из базы данных в FastReport =============================== }

function TFormReports.OpenReports: Boolean;
begin
  StartWait;
  ShowInStatusBar(SMsgReportsOpen);
  try
    try
      {$IFDEF RSS}
      AddToDbLog(fFormTag, Format(SLogReportOpen, [fFormId]));
      {$ENDIF}

      RDbEditor.LogEnable := fFormTag > 0;
      RDbEditor.CheckTags := fFormTag > 0;

      if not RDbGridTuner.Active then
        RDbGridTuner.Open;

      if not RDbOrder.Active then
        RDbOrder.Open;

      Result := OpenDS_VariableQuery(ss_reports.Connection,
        ss_reports, nil, RDbOrder, Format(sqlReportForm, [fFormId]), '', '', '',
        True, fnID, {$IFDEF RSS} tagEditReports {$ELSE} 0 {$ENDIF});
    except
      on E: Exception do
      begin
        Result := False;
        HandleExcept(E, ss_reports, Format(SErrOpenDataSet, [ss_reports.Name]));
      end;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormReports.CloseReports;
begin
  StartWait;
  ShowInStatusBar(SMsgReportsClose);
  try
    {$IFDEF RSS}
    AddToDbLog(fFormTag, Format(SLogReportClose, [fFormId]));
    {$ENDIF}

    if RDbGridTuner.Active then
      RDbGridTuner.Close;

    if RDbOrder.Active then
      RDbOrder.Close;

    ss_reports.Close;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

function TFormReports.LoadReport(Report: TfrxReport): Boolean;
var
  msBuff: TStream;
begin
  StartWait;
  ShowInStatusBar(SMsgReportsLoad);
  try
    Result := True;
    if not ss_reportsREPORT.IsNull then
    begin
      msBuff := TMemoryStream.Create;
      try
        try
          ss_reportsREPORT.SaveToStream(msBuff);
          msBuff.Position := 0;
          Report.LoadFromStream(msBuff);
          Report.ReportOptions.Name := ss_reportsname.AsString;
          Report.Tag := ss_reportsid.AsInteger;

          frxPDFExport.Subject := ss_reportsname.AsString;
          frxPDFExport.FileName := ss_reportsname.AsString;
          frxHTMLExport.FileName := ss_reportsname.AsString;
          frxRTFExport.FileName := ss_reportsname.AsString;
          frxXLSExport.FileName := ss_reportsname.AsString;
          frxJPEGExport.FileName := ss_reportsname.AsString;
          frxMailExport.FileName := ss_reportsname.AsString;
          frxTXTExport.FileName := ss_reportsname.AsString;
          frxSimpleTextExport.FileName := ss_reportsname.AsString;
          frxXMLExport.FileName := ss_reportsname.AsString;
          frxODSExport.FileName := ss_reportsname.AsString;
          frxODTExport.FileName := ss_reportsname.AsString;
        except
          on E: Exception do
          begin
            Result := False;
            HandleExcept(E, Report, Format(SErrLoadReport, [ss_reportsname.AsString]));
          end;
        end;
      finally
        msBuff.Free;
      end;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

function TFormReports.SaveReport(Report: TfrxReport; const Logged: Boolean): Boolean;
var
  msBuff: TStream;
begin
  StartWait;
  ShowInStatusBar(SMsgReportsSave);
  try
    Result := True;
    msBuff := TMemoryStream.Create;
    try
      try
        ss_reports.Edit;
        Report.SaveToStream(msBuff);
        msBuff.Position := 0;
        ss_reportsREPORT.LoadFromStream(msBuff);
        ss_reports.Post;

        if Logged then
        begin
          {$IFDEF RSS}
          AddToDbLog(tagEditReports, Format(SLogReportEdit, [fFormId,
            ss_reportsid.AsInteger, Trim(ss_reportsname.AsString),
            Trim(ss_reportsnotes.AsString)]));
          {$ENDIF}
        end;
      except
        on E: Exception do
        begin
          Result := False;
          HandleExcept(E, Report, Format(SErrSaveReport, [ss_reportsname.AsString]));
        end;
      end;
    finally
      msBuff.Free;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Получение значения переменной из списка переменных ======================== }

procedure TFormReports.ReportGetValue(const VarName: String; var Value: Variant);
var
  i: Integer;
begin
  for i := 0 to fVars.Count - 1 do
    if AnsiSameText(VarName, fVars.Names[i]) then
    begin
      Value := fVars.Values[VarName];
      Break;
    end;
end;

{ == Обработка событий редактора =============================================== }

procedure TFormReports.RDbEditorGetNewKey(Sender: TObject; var Value: Integer);
begin
  Value := BaseData.GetNewId(RDbEditor.GetObjectName(etView), RDbEditor.KeyFieldName);
end;

procedure TFormReports.RDbEditorFreeNewKey(Sender: TObject; var Value: Integer);
begin
  BaseData.FreeId(RDbEditor.GetObjectName(etView), Value);
end;

procedure TFormReports.RDbEditorGetInsertLogMsg(Sender: TObject; OldData,
  NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
  var Text: String);
begin
  {$IFDEF RSS}
  Text := Format(SLogReportInsert, [fFormId, ss_reportsid.AsInteger, Trim(ss_reportsname.AsString), Trim(ss_reportsnotes.AsString)]);
  {$ENDIF}
end;

procedure TFormReports.RDbEditorGetChangeLogMsg(Sender: TObject; OldData,
  NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
  var Text: String);
begin
  {$IFDEF RSS}
  Text := Format(SLogReportEdit, [fFormId, ss_reportsid.AsInteger, Trim(ss_reportsname.AsString), Trim(ss_reportsnotes.AsString)]);
  {$ENDIF}
end;

procedure TFormReports.RDbEditorGetDeleteLogMsg(Sender: TObject; OldData,
  NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
  var Text: String);
begin
  {$IFDEF RSS}
  Text := Format(SLogReportDelete, [fFormId, ss_reportsid.AsInteger, Trim(ss_reportsname.AsString), Trim(ss_reportsnotes.AsString)])
  {$ENDIF}
end;

procedure TFormReports.RDbEditorSaveToLog(Sender: TObject;
  const EditTag: Integer; const Text: String);
begin
  {$IFDEF RSS}
  AddToDbLog(EditTag, Text);
  {$ENDIF}
end;

procedure TFormReports.RDbEditorCreateNewRecord(Sender: TObject;
  const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean);
begin
  ss_reportsFORM.AsString := fFormId;
end;

procedure TFormReports.RDbEditorGetEditRights(Sender: TObject;
  const Mode: TEditMode; var Enable: Boolean);
begin
  Enable := fCanEditList;
end;

procedure TFormReports.RDbEditorGetEditorClass(Sender: TObject;
  var EditorClass: TFormClass);
begin
  EditorClass := TFormRptName;
end;

procedure TFormReports.RDbEditorBeforeShowEditor(Sender: TObject;
  Editor: TForm; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
  inherited;

  if Assigned(Editor) then
  begin
    with TRDbEditor(Sender) do
    begin
      Editor.Caption := GetEditorCaption(GetObjectDesc(etView), DataSet);
      TFormRptName(Editor).DataSource.DataSet := DataSet;
    end;
  end;
end;

{ == Обработка событий дизайнера =============================================== }

function TFormReports.DesignerLoadReport(Report: TfrxReport): Boolean;
begin
  Result := False;
  if OpenDialog.Execute then
  begin
    StartWait;
    ShowInStatusBar(SMsgLoadDataFile);
    try
      Report.FileName := OpenDialog.FileName;
      Result := Report.LoadFromFile(OpenDialog.FileName);
      if Result then
      begin
        // 2015-11-29: сразу же сохраняем его в базу данных
        SaveReport(Report, False);

        {$IFDEF RSS}
        AddToDbLog(tagEditReports, Format(SLogReportImport, [fFormId, ss_reportsid.AsInteger,
          Trim(ss_reportsname.AsString), Trim(ss_reportsnotes.AsString)]));
        {$ENDIF}
      end;
    finally
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

function TFormReports.DesignerSaveReport(Report: TfrxReport;
  SaveAs: Boolean): Boolean;
begin
  Result := False;
  if SaveAs then
  begin
    if SaveDialog.Execute then
    begin
      StartWait;
      ShowInStatusBar(SMsgSaveDataFile);
      try
        Report.FileName := SaveDialog.FileName;
        Report.SaveToFile(Report.FileName);

        {$IFDEF RSS}
        AddToDbLog(tagEditReports, Format(SLogReportExport, [fFormId, ss_reportsid.AsInteger,
          Trim(ss_reportsname.AsString), Trim(ss_reportsnotes.AsString)]));
        {$ENDIF}
      finally
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;
    end;
  end
  else Result := SaveReport(Report, True);
end;

{ == Actions =================================================================== }

procedure TFormReports.ReportNewUpdate(Sender: TObject);
begin
  ReportNew.Enabled := IsNotWait and RDbEditor.RecordCanInserted(True);
end;

procedure TFormReports.ReportNewExecute(Sender: TObject);
begin
  if RDbEditor.InsertRecord(0) then
    ReportDesignExecute(Sender);
end;

procedure TFormReports.ReportCloneUpdate(Sender: TObject);
begin
  ReportClone.Enabled := IsNotWait and RDbEditor.RecordCanCopyed(True);
end;

procedure TFormReports.ReportCloneExecute(Sender: TObject);
begin
  if RDbEditor.CopyRecord(0) then
    ReportDesignExecute(Sender);
end;

procedure TFormReports.ReportEditUpdate(Sender: TObject);
begin
  ReportEdit.Enabled := IsNotWait and RDbEditor.RecordCanOpened(True);
end;

procedure TFormReports.ReportEditExecute(Sender: TObject);
begin
  RDbEditor.EditRecord(RDbEditor.RecordCanEdited(True));
end;

procedure TFormReports.ReportDesignUpdate(Sender: TObject);
begin
  ReportDesign.Enabled := IsNotWait and RDbEditor.RecordCanEdited(True);
end;

procedure TFormReports.ReportDesignExecute(Sender: TObject);
begin
  if LoadReport(frxReport) then
  begin
    {$IFDEF RSS}
    AddToDbLog(tagEditReports, Format(SLogReportDesign, [fFormId, ss_reportsid.AsInteger,
      Trim(ss_reportsname.AsString), Trim(ss_reportsnotes.AsString)]));
    {$ENDIF}

    StartWait;
    ShowInStatusBar(SMsgReportsDesign);
    try
      frxReport.DesignReport(True, False);
    finally
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

procedure TFormReports.frxDesignerShow(Sender: TObject);
begin
  ExitWait;
end;

procedure TFormReports.ReportPrintUpdate(Sender: TObject);
begin
  ReportPrint.Enabled := IsNotWait and RDbEditor.RecordCanOpened(True) and not ss_reportsREPORT.IsNull;
end;

procedure TFormReports.ReportPrintExecute(Sender: TObject);
begin
  if LoadReport(frxReport) then
  begin
    {$IFDEF RSS}
    AddToDbLog(fFormTag, Format(SLogReportShow, [fFormId, ss_reportsid.AsInteger,
      Trim(ss_reportsname.AsString), Trim(ss_reportsnotes.AsString)]));
    {$ENDIF}

    frxReport.ShowReport;
  end;
end;

procedure TFormReports.DBGridDblClick(Sender: TObject);
begin
  if RDbEditor.RecordCanEdited(True) then
    ReportDesignExecute(nil)
  else begin
    if RDbEditor.RecordCanOpened(True) and not ss_reportsREPORT.IsNull then
      ReportPrintExecute(nil);
  end
end;

procedure TFormReports.ReportDeleteUpdate(Sender: TObject);
begin
  ReportDelete.Enabled := IsNotWait and RDbEditor.RecordCanDeleted(True);
end;

procedure TFormReports.ReportDeleteExecute(Sender: TObject);
begin
  RDbEditor.DeleteRecord(True);
end;

procedure TFormReports.ReportImportUpdate(Sender: TObject);
begin
  ReportImport.Enabled := IsNotWait and RDbEditor.RecordCanEdited(True);
end;

procedure TFormReports.ReportImportExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    StartWait;
    ShowInStatusBar(SMsgLoadDataFile);
    try
      frxReport.FileName := OpenDialog.FileName;
      frxReport.LoadFromFile(OpenDialog.FileName, True);
      SaveReport(frxReport, False);

      {$IFDEF RSS}
      AddToDbLog(tagEditReports, Format(SLogReportImport, [fFormId, ss_reportsid.AsInteger,
        Trim(ss_reportsname.AsString), Trim(ss_reportsnotes.AsString)]));
      {$ENDIF}
    finally
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

procedure TFormReports.ReportExportUpdate(Sender: TObject);
begin
  ReportExport.Enabled := IsNotWait and RDbEditor.RecordCanEdited(True) and not ss_reportsREPORT.IsNull;
end;

procedure TFormReports.ReportExportExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    StartWait;
    ShowInStatusBar(SMsgSaveDataFile);
    try
      LoadReport(frxReport);
      frxReport.FileName := SaveDialog.FileName;
      frxReport.SaveToFile(SaveDialog.FileName);

      {$IFDEF RSS}
      AddToDbLog(tagEditReports, Format(SLogReportExport, [fFormId, ss_reportsid.AsInteger,
        Trim(ss_reportsname.AsString), Trim(ss_reportsnotes.AsString)]));
      {$ENDIF}
    finally
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

procedure TFormReports.ReportTuneGridUpdate(Sender: TObject);
begin
  ReportTuneGrid.Enabled := IsNotWait and RDbGridTuner.Active;
end;

procedure TFormReports.ReportTuneGridExecute(Sender: TObject);
begin
  RDbGridTuner.ShowDialog;
end;

procedure TFormReports.ReportOrderUpdate(Sender: TObject);
begin
  ReportOrder.Enabled := IsNotWait and RDbOrder.Active;
end;

procedure TFormReports.ReportOrderExecute(Sender: TObject);
begin
  if RDbOrder.ShowDialog then
    OpenReports;
end;

procedure TFormReports.ReportRefreshUpdate(Sender: TObject);
begin
  ReportRefresh.Enabled := IsNotWait;
end;

procedure TFormReports.ReportRefreshExecute(Sender: TObject);
begin
  OpenReports;
end;

end.

