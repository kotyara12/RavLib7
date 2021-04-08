{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Rav Soft Database Additional Components         }
{                                                       }
{       Copyright (c) 2006-2012 Razzhivin Alexandr      }
{                                                       }
{*******************************************************}

unit RDbCustom;

interface

uses
  Classes, SysUtils, Graphics, Db, DbGrids, StdCtrls, Messages;

type
  ERavDbError = class(Exception);

  { TRDbCustom - базовый класс для компонентов RDb###### }

  TRDbCustom = class(TComponent)
  private
    fActive: Boolean;
    fAutoActive: Boolean;
    fStreamedActive: Boolean;
    fStoreInIniFile: Boolean;
    fIniFileName: string;
    fOwnerName: string;
    fOnActivate: TNotifyEvent;
    fOnDeactivate: TNotifyEvent;
    procedure SetActive(const aValue: Boolean);
    procedure SetStoreInIniFile(const aValue: Boolean);
  protected
    procedure Loaded; override;
    procedure DbLinkInit; virtual; abstract;
    procedure DbLinkDone; virtual; abstract;
    procedure InternalInit; virtual; abstract;
    procedure InternalDone; virtual; abstract;
    procedure InternalReset; virtual; abstract;
    procedure CheckDbLink; virtual; abstract;
    function  IsStoreOptions: Boolean; virtual;
    procedure CheckOptions; virtual; abstract;
    procedure DoActivate; virtual;
    procedure DoDeactivate; virtual;
    function  GetIniFileName: string;
    function  GetIniSectionTag: string;
    function  GetIniSection: string; virtual; abstract;
    procedure LoadData; virtual; abstract;
    procedure SaveData; virtual; abstract;
    function  ShowDialog: Boolean; virtual; abstract;
    property  OwnerName: string read fOwnerName;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  CheckActive: Boolean;
    procedure BeforeDestruction; override;
    procedure Open;
    procedure Close;
  published
    property Active: Boolean read fActive write SetActive default False;
    property AutoActivate: Boolean read fAutoActive write fAutoActive default True;
    property IniFileName: string read fIniFileName write fIniFileName;
    property StoreInIniFile: Boolean read fStoreInIniFile write SetStoreInIniFile default False;
    property OnActivate: TNotifyEvent read fOnActivate write fOnActivate;
    property OnDeactivate: TNotifyEvent read fOnDeactivate write fOnDeactivate;
  end;

  { TRDbCustomDS - базовый класс со ссылкой на TDataSet }

  TRDbCustomDS = class(TRDbCustom)
  private
    fDataSet: TDataSet;
    procedure SetDataSet(aValue: TDataSet);
  protected
    procedure DbLinkInit; override;
    procedure DbLinkDone; override;
    procedure CheckDbLink; override;
    procedure CheckOptions; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property DataSet: TDataSet read fDataSet write SetDataSet;
  end;

  { TRDbCustomDG - базовый класс со ссылкой на TDbGrid }

  TRDbCustomDG = class(TRDbCustom)
  private
    fDbGrid: TDbGrid;
    procedure SetDbGrid(aValue: TDbGrid);
  protected
    procedure DbLinkInit; override;
    procedure DbLinkDone; override;
    procedure CheckDbLink; override;
    procedure CheckOptions; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property DbGrid: TDbGrid read fDbGrid write SetDbGrid;
  end;

const
  iniRestore          = 'LoadOnActivate';
  iniRebuild          = 'DialogOnActivate';

  iniMode             = '.%.3d';

  DelimChar           = ';';

  PanelHeight = 32;

{ == Загрузка списка полей в список строк ====================================== }
procedure LoadKeyFieldsToCmbBox(CmbBox: TComboBox; DataSet: TDataSet;
  const DefSelection: string;
  const LoadLookupFields, LoadCalcFields, LoadHiddenFields: Boolean);

implementation

uses
  Windows, Forms, Dialogs, RDialogs, RSysUtils;

resourcestring
  EDataSetNotDefine   = 'Для компонента "%s.%s" не определено свойство "DataSet"!';
  EDbGridNotDefine    = 'Для компонента "%s.%s" не определено свойство "DbGrid"!';
  SWarningNotActive   = 'Компонент "%s.%s" не активирован!';
  SWarningNotStored   = 'Внимание! Не установлено свойство "StoreInIniFile"'#13 +
                        'Некоторые опции не будут активированы!';

{ == Utilites ================================================================== }
procedure LoadKeyFieldsToCmbBox(CmbBox: TComboBox; DataSet: TDataSet;
  const DefSelection: string;
  const LoadLookupFields, LoadCalcFields, LoadHiddenFields: Boolean);
var
  i, N: Integer;
begin
  if Assigned(DataSet) and Assigned(CmbBox) then
  begin
    CmbBox.Items.BeginUpdate;
    try
      CmbBox.Items.Clear;
      for i := 0 to DataSet.FieldCount - 1 do
        if ((DataSet.Fields.Fields[i].DataType <> ftBlob)
          and ((DataSet.Fields[i].FieldKind = fkData)
          or ((DataSet.Fields[i].FieldKind = fkLookup) and LoadLookupFields)
          or ((DataSet.Fields[i].FieldKind = fkCalculated) and LoadCalcFields)))
        and (LoadHiddenFields or (DataSet.Fields[i].Visible)) then
        begin
          N := CmbBox.Items.AddObject(DataSet.Fields[i].DisplayName, DataSet.Fields.Fields[i]);
          if SameText(DataSet.Fields.Fields[i].FieldName, DefSelection) then CmbBox.ItemIndex := N;
        end;
    finally
      CmbBox.Items.EndUpdate;
    end;
  end;
end;

{ == TRDbCustom ================================================================ }
constructor TRDbCustom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fActive := False;
  fAutoActive := True;
  DbLinkInit;
  fStreamedActive := False;
  fIniFileName := EmptyStr;
  fOwnerName := '';
  if Assigned(Owner) then
  begin
    if (Owner is TFrame) and Assigned(Owner.Owner)
    then fOwnerName := Owner.Owner.ClassName + '.' + Owner.ClassName
    else fOwnerName := Owner.ClassName;
  end;
  fStoreInIniFile := False;
  InternalInit;
  fOnActivate := nil;
  fOnDeactivate := nil;
end;

destructor TRDbCustom.Destroy;
begin
  InternalDone;
  DbLinkDone;
  fOnActivate := nil;
  fOnDeactivate := nil;
  inherited Destroy;
end;

procedure TRDbCustom.BeforeDestruction;
begin
  Close;
  inherited;
end;

procedure TRDbCustom.Loaded;
begin
  inherited Loaded;
  if fStreamedActive then
    SetActive(True);
end;

procedure TRDbCustom.SetActive(const aValue: Boolean);
begin
  if aValue and (csReading in ComponentState) then
    fStreamedActive := aValue
  else begin
    if fActive <> aValue then
    begin
      if aValue then DoActivate else DoDeactivate;
      fStreamedActive := False;
    end;
  end;
end;

procedure TRDbCustom.Open;
begin
  SetActive(True);
end;

procedure TRDbCustom.Close;
begin
  SetActive(False);
end;

procedure TRDbCustom.DoActivate;
begin
  CheckDbLink;
  InternalReset;
  if Assigned(Owner) then
  begin
    if (Owner is TFrame) and Assigned(Owner.Owner)
    then fOwnerName := Owner.Owner.ClassName + '.' + Owner.ClassName
    else fOwnerName := Owner.ClassName;
  end;
  if not (csDesigning in ComponentState) then
    LoadData;
  try
    if Assigned(fOnActivate) then
      fOnActivate(Self);
  finally
    fActive := True;
  end;
end;

procedure TRDbCustom.DoDeactivate;
begin
  if not (csDesigning in ComponentState) then
    SaveData;
  try
    if Assigned(fOnDeactivate) then
      fOnDeactivate(Self);
  finally
    fActive := False;
  end;
end;

function TRDbCustom.CheckActive: Boolean;
begin
  if not fActive and fAutoActive then
    SetActive(True);
  Result := fActive;
  if not Result then
    raise ERavDbError.CreateFmt(SWarningNotActive, [ClassName, Name]);
end;

function TRDbCustom.IsStoreOptions: Boolean;
begin
  Result := False;
end;

procedure TRDbCustom.SetStoreInIniFile(const aValue: Boolean);
begin
  if fStoreInIniFile <> aValue then
  begin
    if aValue then
      CheckDbLink;
    fStoreInIniFile := aValue;
    if not ((csLoading in ComponentState) or (csDestroying in ComponentState))
    and (csDesigning in ComponentState) then CheckOptions;
  end;
end;

function TRDbCustom.GetIniFileName: string;
begin
  if fIniFileName <> EmptyStr
  then Result := fIniFileName
  else Result := GetModuleIniFile;
end;

function TRDbCustom.GetIniSectionTag: string;
begin
  if Tag = 0
  then Result := EmptyStr
  else Result := Format(iniMode, [Tag]);
end;

{ == TRDbCustomDS ============================================================== }
procedure TRDbCustomDS.DbLinkInit;
begin
  fDataSet := nil;
end;

procedure TRDbCustomDS.DbLinkDone;
begin
  fDataSet := nil;
end;

procedure TRDbCustomDS.CheckDbLink;
begin
  if not (csLoading in ComponentState) and (fDataSet = nil) then
    raise ERavDbError.CreateFmt(EDataSetNotDefine, [ClassName, Name]);
end;

procedure TRDbCustomDS.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(fDataSet) and (AComponent = fDataSet) then
    DataSet := nil;
end;

procedure TRDbCustomDS.SetDataSet(aValue: TDataSet);
begin
  if fDataSet <> aValue then
  begin
    if not Assigned(aValue) then
      SetActive(False);
    fDataSet := aValue;
    if Assigned(aValue) then
      aValue.FreeNotification(Self)
    else begin
      if not ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
        fStoreInIniFile := False;
    end;
  end;
end;

procedure TRDbCustomDS.CheckOptions;
begin
  if not fStoreInIniFile or not Assigned(fDataSet) then
  begin
    if IsStoreOptions then Application.MessageBox(PChar(SWarningNotStored),
      PChar(ClassName + '.' + Name), MB_OK + MB_ICONINFORMATION);
  end;
end;

{
function TRDbCustomDS.CheckDbState: Boolean;
begin
  Result := Assigned(fDataSet)
    and not ((csDestroying in fDataSet.ComponentState)
    or (csFixups in fDataSet.ComponentState)
    or (csFreeNotification in fDataSet.ComponentState));
end;
}

{ == TRDbCustomDG ============================================================== }
procedure TRDbCustomDG.DbLinkInit;
begin
  fDbGrid := nil;
end;

procedure TRDbCustomDG.DbLinkDone;
begin
  fDbGrid := nil;
end;

procedure TRDbCustomDG.CheckDbLink;
begin
  if not (csLoading in ComponentState) then
  begin
    if not Assigned(fDbGrid) then
      raise ERavDbError.CreateFmt(EDbGridNotDefine, [ClassName, Name]);
    if not Assigned(fDbGrid.DataSource) or not Assigned(fDbGrid.DataSource.DataSet) then
      raise ERavDbError.CreateFmt(EDataSetNotDefine, [ClassName, Name]);
  end;
end;

procedure TRDbCustomDG.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (DbGrid <> nil) and (AComponent = DbGrid) then
    DbGrid := nil;
end;

procedure TRDbCustomDG.SetDbGrid(aValue: TDbGrid);
begin
  if fDbGrid <> aValue then
  begin
    if not Assigned(aValue) then
      SetActive(False);
    fDbGrid := aValue;
    if Assigned(aValue) then
      aValue.FreeNotification(Self)
    else begin
      if not ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
        fStoreInIniFile := False;
    end;
  end;
end;

procedure TRDbCustomDG.CheckOptions;
begin
  if not fStoreInIniFile or not Assigned(fDbGrid) then
  begin
    if IsStoreOptions then Application.MessageBox(PChar(SWarningNotStored),
      PChar(ClassName + '.' + Name), MB_OK + MB_ICONINFORMATION);
  end;
end;

{
function TRDbCustomDG.CheckDbState: Boolean;
begin
  Result := Assigned(fDbGrid)
    and not ((csDestroying in fDbGrid.ComponentState)
    or (csFixups in fDbGrid.ComponentState)
    or (csFreeNotification in fDbGrid.ComponentState));
end;
}

end.
