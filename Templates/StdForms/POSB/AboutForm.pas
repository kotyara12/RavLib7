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
    TrademarksLabel: TLabel;
    OsLabel: TLabel;
    DelphiVersion: TLabel;
    CommentsLabel: TLabel;
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
  SVersions   = 'Версия продукта: %s, версия файла: %s';
  SAuthor     = 'Автор программы: %s';
  STrademarks = '%s';
  SOs         = 'ОС: Microsoft Windows %20:s [%14:d.%15:d.%18:d]';

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
procedure TFormAbout.InitForm;
var
  VerInfo: TVersionInfo;
begin
  inherited;
  VerInfo := AppVerInfo;
  try
    ProgramIcon.Picture.Assign(Application.Icon);
    ProductName.Caption := VerInfo.ProductName;
    ProductName.Font.Name := Font.Name;
    ProductName.Font.Size := Font.Size + 1;
    ProgramName.Caption := VerInfo.InternalName; // Application.Title;
    ProgramName.Font.Name := Font.Name;
    ProgramName.Font.Size := Font.Size + 1;
    VersionInfo.Caption := Format(SVersions, [VerInfo.ProductVersion, VerInfo.FileVersion]);
    AutorName.Caption := Format(SAuthor, [VerInfo.CompanyName]);
    TrademarksLabel.Caption := Format(STrademarks, [VerInfo.LegalTrademarks]);
    CommentsLabel.Caption := VerInfo.Comments;
    OsLabel.Caption := GetWindowsVersion(GetWindowsVersionData, SOs);
  finally
    VerInfo.Free;
  end;
end;

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TFormAbout.SetStyle;
begin
  inherited;
  FontDataToFontNoStyled(ApplicationStyle.DataForm.FormFont, ProgramName.Font);
  FontDataToFontNoStyled(ApplicationStyle.DataForm.FormFont, ProductName.Font);
end;
{$ENDIF}

end.
