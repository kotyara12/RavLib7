unit TmplBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Dialogs,
  {$IFDEF STYLES} RMessages, {$ENDIF} Forms;

type
  TBaseTemplate = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    {$IFDEF STYLES}
    procedure SetStyleMsgHandler(var Msg: TMessage); message WM_SETFORMSTYLE;
    {$ENDIF}
  protected
    FStarted: Boolean;
    procedure InitFormVariables; virtual;
    procedure InitForm; virtual;
    procedure StartForm; virtual;
    procedure DoneForm; virtual;
    procedure FreeForm; virtual;
  public
    {$IFDEF STYLES}
    procedure SetStyle; virtual;
    {$ENDIF}
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  RDialogs, RVclUtils;

{ == Инициализация формы ======================================================= }
procedure TBaseTemplate.InitFormVariables;
begin
  FStarted := False;
end;

procedure TBaseTemplate.InitForm;
begin
end;

{$IFDEF STYLES}
{ == Обработчик сообщения Windows о необходимости установить стиль окна ======== }
procedure TBaseTemplate.SetStyleMsgHandler(var Msg: TMessage);
begin
  if Msg.WParam = ID_SETFORMSTYLE then
  begin
    Msg.Result := 1;
    SetStyle;
  end;
end;

{ == Установка стиля формы ===================================================== }
procedure TBaseTemplate.SetStyle;
begin
  Ctl3D := ApplicationStyle.DataForm.Ctl3D;
  Color := ApplicationStyle.DataForm.FormColor;
  FontDataToFont(ApplicationStyle.DataForm.FormFont, Font);
end;
{$ENDIF}

{ == Первый запуск формы ======================================================= }
procedure TBaseTemplate.StartForm;
begin
end;

{ == Завершение работы с формой ================================================ }
procedure TBaseTemplate.DoneForm;
begin
end;

procedure TBaseTemplate.FreeForm;
begin
end;

{ == Стандартные методы TForm ================================================== }
procedure TBaseTemplate.FormCreate(Sender: TObject);
begin
  StartWait;
  try
    try
      InitFormVariables;
    finally
      {$IFDEF STYLES}
      try
        SetStyle;
      finally
        InitForm;
      end;
      {$ELSE}
      Font.Name := Screen.MenuFont.Name;
      InitForm;
      {$ENDIF}
    end;
  finally
    StopWait;
  end;
end;

procedure TBaseTemplate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StartWait;
  try
    if FormStyle = fsMdiChild then
      Action := caFree;
    DoneForm;
  finally
    StopWait;
  end;
end;

procedure TBaseTemplate.FormDestroy(Sender: TObject);
begin
  StartWait;
  try
    FreeForm;
  finally
    StopWait;
  end;
end;

procedure TBaseTemplate.FormShow(Sender: TObject);
begin
  if not FStarted then
  begin
    FStarted := True;
    StartWait;
    try
      StartForm;
    finally
      StopWait;
    end;
  end;
end;

end.
