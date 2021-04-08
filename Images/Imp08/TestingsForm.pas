unit TestingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplQuery, RDbUpdater, RDbFind, DB, RDbEditor, Menus, RDbStatus,
  RDbCustom, RDbGridTuner, RDbCustomSearch, RDbSearch, RDbOrder, RDbFilter,
  DBActns, ActnList, StdCtrls, Buttons, Tabs, RDbPanel, Grids, DBGrids,
  RDbColorGrid, ExtCtrls, ComCtrls, ToolWin, ADODB;

type
  TFormTestings = class(TQueryTemplate)
    testings: TADOQuery;
    testingsid: TIntegerField;
    testingsfullname: TWideStringField;
    testingsbirthday: TDateTimeField;
    testingsphone: TWideStringField;
    testingsemail: TWideStringField;
    testingsmessenger: TWideStringField;
    testingsspecialty: TWideStringField;
    testingsfeatures: TMemoField;
    testingstest_date_1: TDateTimeField;
    testingstest_date_2: TDateTimeField;
    testingstesting_cost: TBCDField;
    testingsnotes: TWideStringField;
    RDbFilter_id: TRDFIntegerItem;
    RDbFilter_fullname: TRDFStringItem;
    RDbFilter_birthday: TRDFDateItem;
    RDbFilter_phone: TRDFStringItem;
    RDbFilter_email: TRDFStringItem;
    RDbFilter_messenger: TRDFStringItem;
    RDbFilter_specialty: TRDFStringItem;
    RDbFilter_features: TRDFTextItem;
    RDbFilter_test_date_1: TRDFDateItem;
    RDbFilter_test_date_2: TRDFDateItem;
    RDbFilter_testing_cost: TRDFFloatItem;
    RDbFilter_notes: TRDFStringItem;
    RDbUpdater_fullname: TRDUStringItem;
    RDbUpdater_birthday: TRDUDateTimeItem;
    RDbUpdater_phone: TRDUStringItem;
    RDbUpdater_email: TRDUStringItem;
    RDbUpdater_messenger: TRDUStringItem;
    RDbUpdater_specialty: TRDUStringItem;
    RDbUpdater_features: TRDUStringItem;
    RDbUpdater_test_date_1: TRDUDateTimeItem;
    RDbUpdater_test_date_2: TRDUDateTimeItem;
    RDbUpdater_testing_cost: TRDUFloatItem;
    RDbUpdater_notes: TRDUStringItem;
    procedure RDbEditorCreateNewRecord(Sender: TObject;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure RDbEditorGetEditRights(Sender: TObject;
      const Mode: TEditMode; var Enable: Boolean);
    procedure RDbEditorGetEditorClass(Sender: TObject;
      var EditorClass: TFormClass);
  private
  public
    function LoadDataForm: Boolean; override;
  end;

implementation

{$R *.dfm}

uses
  BaseDbUnit, TestingsProp;

{ TFormTestings }

function TFormTestings.LoadDataForm: Boolean;
begin
  Result := BaseData.OpenVariableQuery(testings, RDbFilter, RDbOrder, 'SELECT * FROM testings', EmptyStr, 0);

  testingsbirthday.DisplayFormat := BaseData.fDateFmt;
  testingsbirthday.EditMask := BaseData.fDateMask;
  testingstest_date_1.DisplayFormat := BaseData.fDateFmt;
  testingstest_date_1.EditMask := BaseData.fDateMask;
  testingstest_date_2.DisplayFormat := BaseData.fDateFmt;
  testingstest_date_2.EditMask := BaseData.fDateMask;
end;

procedure TFormTestings.RDbEditorCreateNewRecord(Sender: TObject;
  const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean);
begin
  inherited;

  testingstest_date_1.AsDateTime := Date();
end;

procedure TFormTestings.RDbEditorGetEditRights(Sender: TObject;
  const Mode: TEditMode; var Enable: Boolean);
begin
  Enable := True;
end;

procedure TFormTestings.RDbEditorGetEditorClass(Sender: TObject;
  var EditorClass: TFormClass);
begin
  EditorClass := TFormTestingsProp;
end;

end.
