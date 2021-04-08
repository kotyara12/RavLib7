library RStyles;

uses
  SysUtils,
  Classes,
  Windows,
  Forms,
  RDialogs in '..\..\RDialogs.pas',
  RFonts in '..\..\RFonts.pas',
  RMessages in '..\..\RMessages.pas',
  RAppStyles in '..\..\RAppStyles.pas',
  TmplBase in '..\..\Templates\TmplBase.pas' {BaseTemplate},
  TmplDialog in '..\..\Templates\TmplDialog.pas' {DialogTemplate},
  RAppStylesEd in '..\..\RAppStylesEd.pas' {FormStyles};

{$R *.res}

resourcestring
  EDllError        = 'Ошибка в библиотеке RStyles.Dll!'#13#13'%s'#13'Класс: %s';

var
  DllHandle: THandle;

procedure DllLoadStyles(const GlobalIniFile: PChar; const Style: PApplicationStyle);
begin
  try
    LoadStyles(GlobalIniFile, Style);
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

procedure DllSaveStyles(const GlobalIniFile: PChar; const Style: PApplicationStyle);
begin
  try
    SaveStyles(GlobalIniFile, Style);
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

procedure DllSetStyles(const MainWnd: THandle);
begin
  try
    SetStyles(MainWnd);
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

function DllEditStyles(const AppHandle, MainWnd: THandle; const Style: PApplicationStyle): Boolean;
begin
  Result := False;
  DllHandle := Application.Handle;
  Application.Handle := AppHandle;
  try
    try
      ApplicationStyle := Style;
      Result := EditStyles(MainWnd);
    except
      on E: Exception do
        ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
    end;
  finally
    Application.Handle := DllHandle;
  end;
end;

function DllEditStylesSave(const AppHandle, MainWnd: THandle;
  const Style: PApplicationStyle; const GlobalIniFile: PChar): Boolean;
begin
  Result := DllEditStyles(AppHandle, MainWnd, Style);
  if Result then DllSaveStyles(GlobalIniFile, Style);
  DllSetStyles(MainWnd);
end;

exports
  DllLoadStyles name 'DllLoadStyles',
  DllSaveStyles name 'DllSaveStyles',
  DllSetStyles name 'DllSetStyles',
  DllEditStyles name 'DllEditStyles',
  DllEditStylesSave name 'DllEditStylesSave';

begin
end.
