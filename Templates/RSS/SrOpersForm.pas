unit SrOpersForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDb, Menus, RDbStatus, RDbCustom, RDbGridTuner, RDbFind, DB,
  DBActns, ActnList, ImgList, Grids, DBGrids, RDbColorGrid, RDbPanel,
  ExtCtrls, ComCtrls, ToolWin, StdCtrls, RDbText, RDbEditor,
  RDbCustomSearch, RDbSearch, RDbUpdater, Buttons, Tabs;

type
  TFormSrOpers = class(TDbTemplate)
    lblId: TLabel;
    dtId: TRDbText;
    lblName: TLabel;
    dtName: TRDbText;
    dtNotes: TRDbText;
    lblNotes: TLabel;
    lblHidden: TLabel;
    dtHidden: TRDbText;
    dtNameLevels: TRDbText;
    lblNotesLevels: TLabel;
    dtNotesLevels: TRDbText;
    lblNameLevels: TLabel;
    procedure RDbEditorGetEditRights(Sender: TObject;
      const Mode: TEditMode; var Enable: Boolean);
    procedure RDbEditorGetEditorClass(Sender: TObject;
      var EditorClass: TFormClass);
  public
  end;

implementation

{$R *.dfm}

uses
  AdminUnit, SrOpersProp;

procedure TFormSrOpers.RDbEditorGetEditRights(Sender: TObject;
  const Mode: TEditMode; var Enable: Boolean);
begin
  Enable := False;
end;

procedure TFormSrOpers.RDbEditorGetEditorClass(Sender: TObject;
  var EditorClass: TFormClass);
begin
  EditorClass := TFormSrOpersProp;
end;

end.
