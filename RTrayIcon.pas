unit RTrayIcon;

interface

uses
  Messages, Forms, Graphics;

const
  tiAdd        = 1;
  tiDelete     = 2;
  tiModify     = 3;

procedure ShowTrayIcon(AForm: TForm; AIcon: TIcon;
  const AIconMode: Integer; const AHintText: string = '');
procedure ShowTrayIcon_MainForm(AForm: TForm;
  const AIconMode: Integer; const AHintText: string = '');

implementation

uses
  Windows, ShellApi, SysUtils, RMessages, RDialogs;

resourcestring
  ssICON_IconFormNotFound      = '»конка формы не найдена!';

procedure ShowTrayIcon(AForm: TForm; AIcon: TIcon; const AIconMode: Integer; const AHintText: string = '');
var
  Nim: TNotifyIconData;
begin
  with Nim do
    begin
      cbSize := SizeOf(TNotifyIconData);
      Wnd := AForm.Handle;
      uID := 0;
      uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
      if AIcon <> nil
      then hIcon := AIcon.Handle
      else hIcon := 0;
      uCallbackMessage := WM_TRAYICON;
      if AHintText = EmptyStr
      then StrPLCopy(szTip, AForm.Caption, SizeOf(szTip) - 1)
      else StrPLCopy(szTip, AHintText, SizeOf(szTip) - 1);
    end;
  case AIconMode of
    tiAdd:    Shell_NotifyIcon(NIM_ADD, @Nim);
    tiDelete: Shell_NotifyIcon(NIM_DELETE, @Nim);
    tiModify: Shell_NotifyIcon(NIM_MODIFY, @Nim);
  end;
end;

procedure ShowTrayIcon_MainForm(AForm: TForm; const AIconMode: Integer; const AHintText: string = '');
begin
  if AForm.Icon <> nil then
    ShowTrayIcon(AForm, AForm.Icon, AIconMode, AHintText)
  else
    ErrorBox(ssICON_IconFormNotFound);
end;

end.
