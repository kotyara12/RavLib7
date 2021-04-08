unit AboutForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls;

type
  TFormAbout = class(TDialogTemplate)
    ProgramIcon: TImage;
    ProgramName: TLabel;
    ProductName: TLabel;
    VersionInfo: TLabel;
    Bevel: TBevel;
    AutorName: TLabel;
    MailLabel: TLabel;
    MailToLabel: TLabel;
    OsLabel: TLabel;
    PoproshaykaLabel: TLabel;
    StateLabel: TLabel;
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
  ShellApi, RSysUtils, RWinVer, RxVerInf;

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
  FontDataToFontNoStyled(ApplicationStyle.DataForm.FormFont, ProgramName.Font);
  FontDataToFontNoStyled(ApplicationStyle.DataForm.FormFont, ProductName.Font);
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
    ProgramName.Caption := VerInfo.InternalName;
    ProgramName.Font.Size := 14;
    ProgramName.AutoSize := True;
    if SameText(VerInfo.ProductName, VerInfo.InternalName)
    then ProductName.Caption := ''
    else ProductName.Caption := VerInfo.ProductName;
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
  CreateEMail('mailto:kotyara12@yandex.ru?subject=' + ProgramName.Caption + '. ' + VersionInfo.Caption);
end;

procedure TFormAbout.HttpLabelClick(Sender: TObject);
begin
  OpenUrl('http://ravsoft2004.narod.ru');
end;

end.
