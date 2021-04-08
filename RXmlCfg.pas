unit RXmlCfg;

interface

function RXmlCfg_GetFileName: string;
function RXmlCfg_GetDocName: WideString;
function RXmlCfg_GetDocNamespace: WideString;

implementation

uses
  SysUtils, RSysUtils;

const
  xmlCfgFileExt   = '.xcf';
  xmlCfgDocName   = 'ConfigurationFile';

function RXmlCfg_GetFileName: string;
begin
  Result := GetModuleVarFile(xmlCfgFileExt);
end;

function RXmlCfg_GetDocName: WideString;
begin
  Result := WideString(xmlCfgDocName);
end;

function RXmlCfg_GetDocNamespace: WideString;
begin
  Result := WideString(ExtractFileName(GetModuleVarFile('')));
end;

end.
