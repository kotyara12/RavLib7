unit AboutForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls;

type
  TFormAbout = class(TDialogTemplate)
    ProgramIcon: TImage;
    InternalName: TLabel;
    ProductName: TLabel;
    VersionInfo: TLabel;
    Bevel: TBevel;
    AutorName: TLabel;
    MailLabel: TLabel;
    MailToLabel: TLabel;
    OsLabel: TLabel;
    PoproshaykaLabel: TLabel;
    SiteLable: TLabel;
    HttpLabel: TLabel;
    procedure MailToLabelClick(Sender: TObject);
    procedure HttpLabelClick(Sender: TObject);
  protected
    procedure InitForm; override;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
  end;

{ == Вызов окна "О программе" ================================================== }
procedure ShowAbout;

implementation

{$R *.dfm}

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  ShellApi, RWinVer, RxVerInf;

resourcestring
  SVersions  = 'Версия продукта: %s, версия файла: %s';
  SAuthor    = 'Автор программы: %s';
  SCopyright = 'Copyright: %s';
  SOs        = 'ОС: Microsoft Windows %20:s [%14:d.%15:d.%18:d]';

{ == Вызов окна "О программе" ================================================== }
procedure ShowAbout;
begin
  with TFormAbout.Create(Application) do
  begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;

{ == Инициализация формы ======================================================= }
{$IFDEF STYLES}
procedure TFormAbout.SetStyle;
begin
  inherited;
  FontDataToFontNoStyled(ApplicationStyle.DataForm.FormFont, ProductName.Font);
  FontDataToFontNoStyled(ApplicationStyle.DataForm.FormFont, InternalName.Font);
  FontDataToFontNoStyled(ApplicationStyle.DataForm.FormFont, MailToLabel.Font);
  FontDataToFontNoStyled(ApplicationStyle.DataForm.FormFont, HttpLabel.Font);
end;
{$ENDIF}

procedure TFormAbout.InitForm;
var
  VerInfo: TVersionInfo;
begin
  inherited;
  VerInfo := AppVerInfo;
  try
    ProgramIcon.Picture.Assign(Application.Icon);
    ProductName.Caption := VerInfo.ProductName;
    ProductName.Font.Size := 14;
    if SameText(VerInfo.ProductName, VerInfo.InternalName)
    then InternalName.Caption := ''
    else InternalName.Caption := VerInfo.InternalName;
    VersionInfo.Caption := Format(SVersions, [VerInfo.ProductVersion, VerInfo.FileVersion]);
    AutorName.Caption := Format(SAuthor, [VerInfo.CompanyName]);
    OsLabel.Caption := GetWindowsVersion(GetWindowsVersionData, SOs);
  finally
    VerInfo.Free;
  end;
end;

{ == Переход по ссылкам ======================================================== }
procedure TFormAbout.MailToLabelClick(Sender: TObject);
begin
  ShellExecuteW(0, nil,
    StringToOleStr('mailto:kotyara12@yandex.ru?subject=' + ProductName.Caption + '. ' + VersionInfo.Caption),
    nil, nil, 1);
end;

procedure TFormAbout.HttpLabelClick(Sender: TObject);
begin
  ShellExecuteW(0, nil,
    StringToOleStr('https://kotyara12.ru'),
    nil, nil, 1);
end;

end.
