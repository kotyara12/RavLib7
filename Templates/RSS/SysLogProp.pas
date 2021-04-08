unit SysLogProp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, DB, StdCtrls, Buttons, ExtCtrls, DBCtrls, Mask;

type
  TFormSysLogProp = class(TDbDialogTemplate)
    lblDatetime: TLabel;
    deDatetime: TDBEdit;
    lblInfo: TLabel;
    deInfo: TDBMemo;
    OperationsGroupBox: TGroupBox;
    lblIdOpertions: TLabel;
    lblNameOperations: TLabel;
    deIdOpertions: TDBEdit;
    deNameOperations: TDBEdit;
    lblNotesOperations: TLabel;
    deNotesOperations: TDBEdit;
    GroupsGroupBox: TGroupBox;
    lblIdLevels: TLabel;
    lblNameLevels: TLabel;
    deIdLevels: TDBEdit;
    deNameLevels: TDBEdit;
    lblNotesLevels: TLabel;
    deNotesLevels: TDBEdit;
    UserGroupBox: TGroupBox;
    lblLogin: TLabel;
    deLogin: TDBEdit;
    lblFullname: TLabel;
    deFullname: TDBEdit;
    lblHost: TLabel;
    deHost: TDBEdit;
    lblNetUser: TLabel;
    deNetUser: TDBEdit;
    deNameWorkplases: TDBEdit;
    lblNameWorkplases: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  BaseDbUnit, AdminUnit, SysLogForm;
  
end.
