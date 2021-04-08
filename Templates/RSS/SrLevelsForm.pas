unit SrLevelsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDb, RDbStatus, RDbCustom, RDbGridTuner, RDbFind, DB, Menus,
  ActnList, ImgList, Grids, DBGrids, RDbColorGrid, RDbPanel, ExtCtrls,
  ComCtrls, ToolWin, StdCtrls, RDbText, DBActns, RDbEditor,
  RDbCustomSearch, RDbSearch, RDbUpdater, Buttons, Tabs;

type
  TFormSrLevels = class(TDbTemplate)
    lblId: TLabel;
    dtId: TRDbText;
    dtName: TRDbText;
    deNotes: TRDbText;
    lblName: TLabel;
    lblNotes: TLabel;
    procedure RDbEditorGetEditRights(Sender: TObject;
      const Mode: TEditMode; var Enable: Boolean);
    procedure RDbEditorGetEditorClass(Sender: TObject;
      var EditorClass: TFormClass);
  public
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RDialogs, RDbUtils, RDbData, RMsgRu,
  BaseDbUnit, AdminUnit, OprList, SrLevelsProp;

procedure TFormSrLevels.RDbEditorGetEditRights(Sender: TObject;
  const Mode: TEditMode; var Enable: Boolean);
begin
  Enable := (Mode = etEdit) and orEditSrLevels;
end;

procedure TFormSrLevels.RDbEditorGetEditorClass(Sender: TObject;
  var EditorClass: TFormClass);
begin
  EditorClass := TFormSrLevelsProp;
end;

end.
