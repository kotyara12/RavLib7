unit TmplStorage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplBase;

type
  TStorageTemplate = class(TBaseTemplate)
  private
    FStored: Boolean;
  protected
    procedure InitFormVariables; override;
    procedure StartForm; override;
    procedure DoneForm; override;
    procedure LoadFormPlacement;
    procedure SaveFormPlacement;
  public
    procedure LoadFormControls; virtual;
    procedure SaveFormControls; virtual;
    property  FormStorageEnabled: Boolean read FStored write FStored;
  end;

implementation

{$R *.dfm}

uses
  RFrmStorage, RExHandlers, RMsgRu;

{ == Инициализация формы ======================================================= }
procedure TStorageTemplate.InitFormVariables;
begin
  inherited;
  FStored := True;
end;

{ == Отображение формы ========================================================= }
procedure TStorageTemplate.StartForm;
begin
  try
    inherited;
  finally
    LoadFormPlacement;
  end;
end;

{ == Завершение работы с формой ================================================ }
procedure TStorageTemplate.DoneForm;
begin
  try
    SaveFormPlacement;
  finally
    inherited;
  end;
end;

{ == Сохранение и восстановление размеров формы ================================ }
procedure TStorageTemplate.LoadFormPlacement;
begin
  try
    if FStored then
    begin
      LoadFormPosition(Self, True, True);
      LoadFormControls;
    end;
  except
    on E: Exception do
      HandleExcept(E, Self, SErrLoadFormPlacement, 0, 0, esReg);
  end;
end;

procedure TStorageTemplate.LoadFormControls;
begin
end;

procedure TStorageTemplate.SaveFormPlacement;
begin
  try
    if FStored then
    begin
      SaveFormPosition(Self);
      SaveFormControls;
    end;
  except
    on E: Exception do
      HandleExcept(E, Self, SErrSaveFormPlacement, 0, 0, esReg);
  end;
end;

procedure TStorageTemplate.SaveFormControls;
begin
end;

end.
