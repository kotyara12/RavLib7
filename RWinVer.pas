{===============================================================================}
{ (c) 2010-2013 by Razzivin Alexandr                                            }
{ kotyara12@yandex.ru                                                           }
{ 22.02.2013                                                                    }
{ This code based on MSDN library samles:                                       }
{ http://msdn.microsoft.com/en-us/library/windows/desktop/ms724429(v=vs.85).aspx}
{===============================================================================}

unit RWinVer;

// Switch off "unsafe" warnings for this unit
{$IFDEF WARNDIRS}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
{$ENDIF}

interface

uses
  SysUtils, Classes, Windows;

(*******************************************************************************)

type
  TWinVersionSM = (smUndefined, smServerR2, smMediaCenter, smStarter, smTabletPC);

  TWinVersionData = record
    bWin32HaveExInfo: Boolean;
    bWin32HaveProductInfo: Boolean;
    dwPlatformId: DWord;
    dwMajorVersion: DWord;
    dwMinorVersion: DWord;
    dwBuildNumber: DWord;
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wProductType: Byte;
    wSuiteMask: Word;
    sCSDVersion: string[128];
    tSysMetrics: TWinVersionSM;
    dwProductInfo: DWord;
    wProcessorArchitecture: Word;
    dwLanguageId: DWord;
  end;

(*******************************************************************************)

type
  // ANSI versions of the Win API OSVERSIONINFOEX structure and pointers
  _OSVERSIONINFOEXA = packed record
    dwOSVersionInfoSize: DWORD;               // size of structure
    dwMajorVersion: DWORD;                    // major OS version number
    dwMinorVersion: DWORD;                    // minor OS version number
    dwBuildNumber: DWORD;                     // OS build number
    dwPlatformId: DWORD;                      // OS platform identifier
    szCSDVersion: array[0..127] of AnsiChar;  // service pack or extra info
    wServicePackMajor: WORD;                  // service pack major version no.
    wServicePackMinor: WORD;                  // service pack minor version no.
    wSuiteMask: WORD;                         // bitmask that stores OS suite(s)
    wProductType: Byte;                       // additional info about system
    wReserved: Byte;                          // reserved for future use
  end;
  OSVERSIONINFOEXA = _OSVERSIONINFOEXA;
  TOSVersionInfoExA = _OSVERSIONINFOEXA;
  POSVersionInfoExA = ^TOSVersionInfoExA;

  // Default version of the Win API OSVERSIONINFOEX structure.
  // UNICODE is defined when the Unicode API is used, so we use this to decide
  // which structure to use as default.
  _OSVERSIONINFOEX = _OSVERSIONINFOEXA;
  OSVERSIONINFOEX = OSVERSIONINFOEXA;
  TOSVersionInfoEx = TOSVersionInfoExA;
  POSVersionInfoEx = POSVersionInfoExA;

const
  // These Windows-defined constants are required for use with the
  // GetVersionExA call
  // _OSVERSIONINFOEX.wProductType:
  VER_NT_WORKSTATION                          = $00000001; // The operating system is Windows 8, Windows 7, Windows Vista, Windows XP Professional, Windows XP Home Edition, or Windows 2000 Professional.
  VER_NT_DOMAIN_CONTROLLER                    = $00000002; // The system is a domain controller and the operating system is Windows Server 2012 , Windows Server 2008 R2, Windows Server 2008, Windows Server 2003, or Windows 2000 Server.
  VER_NT_SERVER                               = $00000003; // The operating system is Windows Server 2012, Windows Server 2008 R2, Windows Server 2008, Windows Server 2003, or Windows 2000 Server.
                                                           // Note that a server that is also a domain controller is reported as VER_NT_DOMAIN_CONTROLLER, not VER_NT_SERVER.

  // These Windows-defined constants are required for use with the
  // GetVersionExA call
  // _OSVERSIONINFOEX.wSuiteMask: Mask representing NT product suites
  VER_SUITE_SMALLBUSINESS                     = $00000001; // Microsoft Small Business Server was once installed on the system, but may have been upgraded to another version of Windows. Refer to the Remarks section for more information about this bit flag.
  VER_SUITE_ENTERPRISE                        = $00000002; // Windows Server 2008 Enterprise, Windows Server 2003, Enterprise Edition, or Windows 2000 Advanced Server is installed. Refer to the Remarks section for more information about this bit flag.
  VER_SUITE_BACKOFFICE                        = $00000004; // Microsoft BackOffice components are installed.
  VER_SUITE_COMMUNICATIONS                    = $00000008;
  VER_SUITE_TERMINAL                          = $00000010; // Terminal Services is installed. This value is always set.
                                                           // If VER_SUITE_TERMINAL is set but VER_SUITE_SINGLEUSERTS is not set, the system is running in application server mode.
  VER_SUITE_SMALLBUSINESS_RESTRICTED          = $00000020; // Microsoft Small Business Server is installed with the restrictive client license in force. Refer to the Remarks section for more information about this bit flag.
  VER_SUITE_EMBEDDEDNT                        = $00000040; // Windows XP Embedded is installed.
  VER_SUITE_DATACENTER                        = $00000080; // Windows Server 2008 Datacenter, Windows Server 2003, Datacenter Edition, or Windows 2000 Datacenter Server is installed.
  VER_SUITE_SINGLEUSERTS                      = $00000100; // Remote Desktop is supported, but only one interactive session is supported. This value is set unless the system is running in application server mode.
  VER_SUITE_PERSONAL                          = $00000200; // Windows Vista Home Premium, Windows Vista Home Basic, or Windows XP Home Edition is installed.
  VER_SUITE_SERVERAPPLIANCE                   = $00000400; // Windows Server 2003, Web Edition is installed.
  VER_SUITE_BLADE                             = VER_SUITE_SERVERAPPLIANCE;
  VER_SUITE_EMBEDDED_RESTRICTED               = $00000800;
  VER_SUITE_SECURITY_APPLIANCE                = $00001000;
  VER_SUITE_STORAGE_SERVER                    = $00002000; // Windows Storage Server 2003 R2 or Windows Storage Server 2003is installed.
  VER_SUITE_COMPUTE_SERVER                    = $00004000; // Windows Server 2003, Compute Cluster Edition is installed.
  VER_SUITE_WH_SERVER                         = $00008000; // Windows Home Server is installed.

  // These Windows-defined constants are required for use with the
  // GetProductInfo API call (Vista and later)
  // ** Additional definitions were obtained from
  //    http://msdn.microsoft.com/en-us/library/ms724358
  PRODUCT_BUSINESS                            = $00000006; // Business
  PRODUCT_BUSINESS_N                          = $00000010; // Business N
  PRODUCT_CLUSTER_SERVER                      = $00000012; // HPC Edition
  PRODUCT_CLUSTER_SERVER_V                    = $00000040; // Server Hyper Core V
  PRODUCT_CORE                                = $00000065; // Windows 8
  PRODUCT_CORE_N                              = $00000062; // Windows 8 N
  PRODUCT_CORE_COUNTRYSPECIFIC                = $00000063; // Windows 8 China
  PRODUCT_CORE_SINGLELANGUAGE                 = $00000064; // Windows 8 Single Language
  PRODUCT_DATACENTER_EVALUATION_SERVER        = $00000050; // Server Datacenter (evaluation installation)
  PRODUCT_DATACENTER_SERVER                   = $00000008; // Server Datacenter (full installation)
  PRODUCT_DATACENTER_SERVER_CORE              = $0000000C; // Server Datacenter (core installation)
  PRODUCT_DATACENTER_SERVER_CORE_V            = $00000027; // Server Datacenter without Hyper-V (core installation)
  PRODUCT_DATACENTER_SERVER_V                 = $00000025; // Server Datacenter without Hyper-V (full installation)
  PRODUCT_ENTERPRISE                          = $00000004; // Enterprise
  PRODUCT_ENTERPRISE_E                        = $00000046; // Not supported
  PRODUCT_ENTERPRISE_N_EVALUATION             = $00000054; // Enterprise N (evaluation installation)
  PRODUCT_ENTERPRISE_N                        = $0000001B; // Enterprise N
  PRODUCT_ENTERPRISE_EVALUATION               = $00000048; // Server Enterprise (evaluation installation)
  PRODUCT_ENTERPRISE_SERVER                   = $0000000A; // Server Enterprise (full installation)
  PRODUCT_ENTERPRISE_SERVER_CORE              = $0000000E; // Server Enterprise (core installation)
  PRODUCT_ENTERPRISE_SERVER_CORE_V            = $00000029; // Server Enterprise without Hyper-V (core installation)
  PRODUCT_ENTERPRISE_SERVER_IA64              = $0000000F; // Server Enterprise for Itanium-based Systems
  PRODUCT_ENTERPRISE_SERVER_V                 = $00000026; // Server Enterprise without Hyper-V (full installation)
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT       = $0000003B; // Windows Essential Server Solution Management
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL       = $0000003C; // Windows Essential Server Solution Additional
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC    = $0000003D; // Windows Essential Server Solution Management SVC
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC    = $0000003E; // Windows Essential Server Solution Additional SVC
  PRODUCT_HOME_BASIC                          = $00000002; // Home Basic
  PRODUCT_HOME_BASIC_E                        = $00000043; // Not supported
  PRODUCT_HOME_BASIC_N                        = $00000005; // Home Basic N
  PRODUCT_HOME_PREMIUM                        = $00000003; // Home Premium
  PRODUCT_HOME_PREMIUM_E                      = $00000044; // Not supported
  PRODUCT_HOME_PREMIUM_N                      = $0000001A; // Home Premium N
  PRODUCT_HOME_PREMIUM_SERVER                 = $00000022; // Windows Home Server 2011
  PRODUCT_HOME_SERVER                         = $00000013; // Windows Storage Server 2008 R2 Essentials
  PRODUCT_HYPERV                              = $0000002A; // Microsoft Hyper-V Server
  PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT    = $0000001E; // Windows Essential Business Server Management Server
  PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING     = $00000020; // Windows Essential Business Server Messaging Server
  PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY      = $0000001F; // Windows Essential Business Server Security Server
  PRODUCT_MULTIPOINT_STANDARD_SERVER          = $0000004C; // Windows MultiPoint Server Standard (full installation)
  PRODUCT_MULTIPOINT_PREMIUM_SERVER           = $0000004D; // Windows MultiPoint Server Premium (full installation)
  PRODUCT_PROFESSIONAL                        = $00000030; // Professional
  PRODUCT_PROFESSIONAL_E                      = $00000045; // Not supported
  PRODUCT_PROFESSIONAL_N                      = $00000031; // Professional N
  PRODUCT_PROFESSIONAL_WMC                    = $00000067; // Professional with Media Center
  PRODUCT_SB_SOLUTION_SERVER_EM               = $00000036; // Server For SB Solutions EM
  PRODUCT_SERVER_FOR_SB_SOLUTIONS             = $00000033; // Server For SB Solutions
  PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM          = $00000037; // Server For SB Solutions EM
  PRODUCT_SERVER_FOR_SMALLBUSINESS            = $00000018; // Windows Server 2008 for Windows Essential Server Solutions
  PRODUCT_SERVER_FOR_SMALLBUSINESS_V          = $00000023; // Windows Server 2008 without Hyper-V for Windows Essential Server Solutions
  PRODUCT_SERVER_FOUNDATION                   = $00000021; // Server Foundation
  PRODUCT_SB_SOLUTION_SERVER                  = $00000032; // Windows Small Business Server 2011 Essentials
  PRODUCT_SMALLBUSINESS_SERVER                = $00000009; // Windows Small Business Server
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM        = $00000019; // Small Business Server Premium
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE   = $0000003F; // Small Business Server Premium (core installation)
  PRODUCT_SOLUTION_EMBEDDEDSERVER             = $00000038; // Windows MultiPoint Server
  PRODUCT_STANDARD_EVALUATION_SERVER          = $0000004F; // Server Standard (evaluation installation)
  PRODUCT_STANDARD_SERVER                     = $00000007; // Server Standard
  PRODUCT_STANDARD_SERVER_CORE                = $0000000D; // Server Standard (core installation)
  PRODUCT_STANDARD_SERVER_V                   = $00000024; // Server Standard without Hyper-V
  PRODUCT_STANDARD_SERVER_CORE_V              = $00000028; // Server Standard without Hyper-V (core installation)
  PRODUCT_STANDARD_SERVER_SOLUTIONS           = $00000034; // Server Solutions Premium
  PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE      = $00000035; // Server Solutions Premium (core installation)
  PRODUCT_STARTER                             = $0000000B; // Starter
  PRODUCT_STARTER_E                           = $00000042; // Not supported
  PRODUCT_STARTER_N                           = $0000002F; // Starter N
  PRODUCT_STORAGE_ENTERPRISE_SERVER           = $00000017; // Storage Server Enterprise
  PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE      = $0000002E; // Storage Server Enterprise (core installation)
  PRODUCT_STORAGE_EXPRESS_SERVER              = $00000014; // Storage Server Express
  PRODUCT_STORAGE_EXPRESS_SERVER_CORE         = $0000002B; // Storage Server Express (core installation)
  PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER  = $00000060; // Storage Server Standard (evaluation installation)
  PRODUCT_STORAGE_STANDARD_SERVER             = $00000015; // Storage Server Standard
  PRODUCT_STORAGE_STANDARD_SERVER_CORE        = $0000002C; // Storage Server Standard (core installation)
  PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER = $0000005F; // Storage Server Workgroup (evaluation installation)
  PRODUCT_STORAGE_WORKGROUP_SERVER            = $00000016; // Storage Server Workgroup
  PRODUCT_STORAGE_WORKGROUP_SERVER_CORE       = $0000002D; // Storage Server Workgroup (core installation)
  PRODUCT_UNDEFINED                           = $00000000; // An unknown product
  PRODUCT_ULTIMATE                            = $00000001; // Ultimate
  PRODUCT_ULTIMATE_E                          = $00000047; // Not supported
  PRODUCT_ULTIMATE_N                          = $0000001C; // Ultimate N
  PRODUCT_WEB_SERVER                          = $00000011; // Web Server (full installation)
  PRODUCT_WEB_SERVER_CORE                     = $0000001D; // Web Server (core installation)
  PRODUCT_UNLICENSED                          = $ABCDABCD; // Unlicensed

  // These constants are required when examining the
  // TSystemInfo.wProcessorArchitecture member.
  // Only constants marked * are defined in the MS 2008 SDK
  PROCESSOR_ARCHITECTURE_UNKNOWN              = $FFFF; // Unknown architecture.
  PROCESSOR_ARCHITECTURE_INTEL                = 0; // x86 *
  PROCESSOR_ARCHITECTURE_MIPS                 = 1; // MIPS architecture
  PROCESSOR_ARCHITECTURE_ALPHA                = 2; // Alpha architecture
  PROCESSOR_ARCHITECTURE_PPC                  = 3; // PPC architecture
  PROCESSOR_ARCHITECTURE_SHX                  = 4; // SHX architecture
  PROCESSOR_ARCHITECTURE_ARM                  = 5; // ARM  architecture
  PROCESSOR_ARCHITECTURE_IA64                 = 6; // Intel Itanium Processor Family *
  PROCESSOR_ARCHITECTURE_ALPHA64              = 7; // Alpha64 architecture
  PROCESSOR_ARCHITECTURE_MSIL                 = 8; // MSIL architecture
  PROCESSOR_ARCHITECTURE_AMD64                = 9; // x64 (AMD or Intel) *
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64        = 10; // IA32 on Win64 architecture

  // These constants are provided in case the obsolete
  // TSystemInfo.dwProcessorType needs to be used.
  // Constants marked Windows CE are only used on Windows mobile and are only
  // provided here for completeness.
  // Only constants marked * are defined in MS SDK 6.1
  PROCESSOR_INTEL_386                         = 386; // Intel i386 processor *
  PROCESSOR_INTEL_486                         = 486; // Intel i486 processor *
  PROCESSOR_INTEL_PENTIUM                     = 586; // Intel Pentium processor *
  PROCESSOR_INTEL_IA64                        = 2200; // Intel IA64 processor *
  PROCESSOR_AMD_X8664                         = 8664; // AMD X86 64 processor *
  PROCESSOR_MIPS_R4000                        = 4000; // MIPS R4000, R4101, R3910 processor
  PROCESSOR_ALPHA_21064                       = 21064; // Alpha 210 64 processor
  PROCESSOR_PPC_601                           = 601; // PPC 601 processor
  PROCESSOR_PPC_603                           = 603; // PPC 603 processor
  PROCESSOR_PPC_604                           = 604; // PPC 604 processor
  PROCESSOR_PPC_620                           = 620; // PPC 620 processor
  PROCESSOR_HITACHI_SH3                       = 10003; // Hitachi SH3 processor (Windows CE)
  PROCESSOR_HITACHI_SH3E                      = 10004; // Hitachi SH3E processor (Windows CE)
  PROCESSOR_HITACHI_SH4                       = 10005; // Hitachi SH4 processor (Windows CE)
  PROCESSOR_MOTOROLA_821                      = 821; // Motorola 821 processor (Windows CE)
  PROCESSOR_SHx_SH3                           = 103; // SHx SH3 processor (Windows CE)
  PROCESSOR_SHx_SH4                           = 104; // SHx SH4 processor (Windows CE)
  PROCESSOR_STRONGARM                         = 2577; // StrongARM processor (Windows CE)
  PROCESSOR_ARM720                            = 1824; // ARM 720 processor (Windows CE)
  PROCESSOR_ARM820                            = 2080; // ARM 820 processor (Windows CE)
  PROCESSOR_ARM920                            = 2336; // ARM 920 processor (Windows CE)
  PROCESSOR_ARM_7TDMI                         = 70001; // ARM 7TDMI processor (Windows CE)
  PROCESSOR_OPTIL                             = $494F; // MSIL processor

  // These constants are required for use with GetSystemMetrics to detect
  // certain editions. GetSystemMetrics returns non-zero when passed these flags
  // if the associated edition is present.
  // Obtained from http://msdn.microsoft.com/en-us/library/ms724385
  SM_TABLETPC                                 = 86; // Detects XP Tablet Edition
  SM_MEDIACENTER                              = 87; // Detects XP Media Center Edition
  SM_STARTER                                  = 88; // Detects XP Starter Edition
  SM_SERVERR2                                 = 89; // Detects Windows Server 2003 R2
  SM_REMOTESESSION                            = $1000; // Detects a remote terminal server session

const
  sKernelDll                                  = 'kernel32.dll';

const
  sFmtWindowsVersionAbbr                      = 'Windows %1:s %13:s %9:s';
  sFmtWindowsVersionType                      = 'Microsoft Windows %4:s';
  sFmtWindowsVersionName                      = 'Microsoft Windows %0:s %8:s';
  sFmtWindowsVersionNameLng                   = 'Microsoft Windows %0:s %13:s %8:s';
  sFmtWindowsVersionNameExt                   = 'Microsoft Windows %0:s %13:s %8:s [%14:d.%15:d.%18:d]';
  sFmtWindowsVersionProdType                  = '%2:s';
  sFmtWindowsVersionNumber                    = '%14:d.%15:d';
  sFmtWindowsVersionNumberExt                 = '%14:d.%15:d.%18:d';

{*******************************************************************************}

{ Get Language Name }

function GetLangAbbrLCID(const LocaleLCID: DWord): string;
function GetLangNameLCID(const LocaleLCID: DWord): string;

(*******************************************************************************)

{ Get system data for decode version of Windows }

function GetWindowsVersionData: TWinVersionData;

{*******************************************************************************
Get version of Windows (string)
In FmtTemplate maybe used:
%0:s  - Product Name & Edition (full)  etc: Server 2008 R2, Enterprise edition
%1:s  - Product Name & Edition (short) etc: Srv 2008R2 EE
%2:s  - Product Type (full)            etc: Server (Domain Controller)
%3:s  - Product Type (short)           etc: Srv (DC)
%4:s  - Product Family (full)          etc: Server 2008
%5:s  - Product Family (short)         etc: Srv 2008
%6:s  - Product Name (full)            etc: Server 2008 R2
%7:s  - Product Name (short)           etc: Srv 2008R2
%8:s  - Service Pack (full)            etc: Service Pack 1
%9:s  - Service Pack (short)           etc: SP1
%10:s - Architecture (full)            etc: 64-bit
%11:s - Architecture (short)           etc: x64
%12:s - Language (full)                etc: –усский
%13:s - Language (short)               etc: RUS
%14:d - Major Version                  etc: 6
%15:d - Minor Version                  etc: 1
%16:d - ServicePack Major              etc: 0
%17:d - ServicePack Minor              etc: 0
%18:d - Build                          etc: 3790
%19:d - Language Id                    etc: 1049
%20:s - Complete (full):  Server 2008, Enterprise edition Service Pack 1, RUS
%21:s - Complete (short): Srv 2008 EE SP1 RUS
*******************************************************************************}

function  GetWindowsVersion(const VersionData: TWinVersionData; const FmtTemplate: string): string;
procedure GetWindowsDescription(const VerData: TWinVersionData; TxtInfo: TStrings);

implementation

uses
  Registry, Nb30;

resourcestring
  sBadRegType =  'Unsupported registry type';

type
  // Dymanically loaded kernel32.dll funtions
  TGetSystemLCID = function : UINT; stdcall;
  TGetSystemInfo = procedure(var lpSystemInfo: TSystemInfo); stdcall;
  TGetProductInfo = function (dwOSMajorVersion: DWord; dwOSMinorVersion: DWord;
    dwSpMajorVersion: DWord; dwSpMinorVersion: DWord;
    out dwReturnedProductType: DWord): Boolean; stdcall;

(*******************************************************************************)

// Flag required when opening registry with specified access flags
const
  KEY_WOW64_64KEY = $0100;  // registry access flag not defined in all Delphis

// Creates a read only TRegistry instance. On versions of Delphi that don't
// support passing access flags to TRegistry constructor, registry is opened
// normally for read/write access.
function RegCreate: TRegistry;
begin
  Result := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
  (*
  Result := TRegistry.Create;
  *)
end;

// Uses registry object to open a key as read only. On versions of Delphi that
// can't open keys as read only the key is opened normally.
function RegOpenKeyReadOnly(const Reg: TRegistry; const Key: string): Boolean;
begin
  //! Fix for problem with OpenKeyReadOnly on 64 bit Windows
  //! requires Reg has (KEY_READ or KEY_WOW64_64KEY) access flags
  Result := Reg.OpenKey(Key, False);
  (*
  // Can't fix Win 64 problem since this version of Delphi does not support
  // customisation of registry access flags.
  Result := Reg.OpenKeyReadOnly(Key);
  // Result := Reg.OpenKey(Key, False);
  *)
end;

// Gets a string value from the given registry sub-key and value within the
// given root key (hive).
function GetRegistryString(const RootKey: HKEY; const SubKey, Name: string): string;
var
  Reg: TRegistry;          // registry access object
  ValueInfo: TRegDataInfo; // info about registry value
begin
  Result := '';
  // Open registry at required root key
  Reg := RegCreate;
  try
    Reg.RootKey := RootKey;
    // Open registry key and check value exists
    if RegOpenKeyReadOnly(Reg, SubKey)
      and Reg.ValueExists(Name) then
    begin
      // Check if registry value is string or integer
      Reg.GetDataInfo(Name, ValueInfo);
      case ValueInfo.RegData of
        rdString, rdExpandString:
          // string value: just return it
          Result := Reg.ReadString(Name);
        rdInteger:
          // integer value: convert to string
          Result := IntToStr(Reg.ReadInteger(Name));
        else
          // unsupported value: raise exception
          raise Exception.Create(sBadRegType);
      end;
    end;
  finally
    // Close registry
    Reg.CloseKey;
    Reg.Free;
  end;
end;

// Gets code describing product type from registry.
// Used to get product type for NT4 SP5 and earlier.
function ProductTypeFromReg: Byte;
var
  EditionCode: string;
begin
  Result := 0;
  EditionCode := GetRegistryString(HKEY_LOCAL_MACHINE,
    'SYSTEM\CurrentControlSet\Control\ProductOptions',
    'ProductType');
  if CompareText(EditionCode, 'WINNT') = 0 then
    Result := VER_NT_WORKSTATION
  else if CompareText(EditionCode, 'LANMANNT') = 0 then
    Result := VER_NT_DOMAIN_CONTROLLER
  else if CompareText(EditionCode, 'SERVERNT') = 0 then
    Result := VER_NT_SERVER;
end;

// System is reporting NT4 SP6 we have SP 6a if particular registry key exists
function IsNT4SP6a: Boolean;
var
  Reg: TRegistry;
begin
  Reg := RegCreate;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Result := Reg.KeyExists(
      'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009');
  finally
    Reg.Free;
  end;
end;

(*******************************************************************************)

{ Get Language Name }

function GetLangAbbrLCID(const LocaleLCID: DWord): string;
var
  LangAbbr: array [0..2] of Char;
begin
  try
    GetLocaleInfo(LocaleLCID, LOCALE_SABBREVLANGNAME, LangAbbr, SizeOf(LangAbbr));
    Result := LangAbbr;
  except
    Result := EmptyStr;
  end;
end;

function GetLangNameLCID(const LocaleLCID: DWord): string;
var
  LangName: array [0..127] of Char;
begin
  try
    GetLocaleInfo(LocaleLCID, LOCALE_SLANGUAGE, LangName, SizeOf(LangName));
    Result := LangName;
  except
    Result := EmptyStr;
  end;
end;

(*******************************************************************************)

function GetWindowsVersionData: TWinVersionData;
var
  pfGetSystemLCID: TGetSystemLCID;
  pfGetSystemInfo: TGetSystemInfo;
  pfGetProductInfo: TGetProductInfo;

  OSVI: TOSVersionInfoEx;
  POSVI: POSVersionInfo;
  SI: TSystemInfo;

  pdwProductInfo: DWord;

begin
  // FillChar(Result, SizeOf(Result), 0);
  with Result do
  begin
    (* Retrieve TOSVersionInfo *)

    FillChar(OSVI, SizeOf(OSVI), 0);
    // Get pointer to structure of non-extended type (GetVersionEx
    // requires a non-extended structure and we need this pointer to get
    // it to accept our extended structure!!)
    {$TYPEDADDRESS OFF}
    POSVI := @OSVI;
    {$TYPEDADDRESS ON}
    // Try to get exended TOsVersionInfoEx information
    OSVI.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
    if GetVersionEx(POSVI^) then
    begin
      bWin32HaveExInfo := True;
      dwPlatformId := OSVI.dwPlatformId;
      dwMajorVersion := OSVI.dwMajorVersion;
      dwMinorVersion := OSVI.dwMinorVersion;
      wServicePackMajor := OSVI.wServicePackMajor;
      wServicePackMinor := OSVI.wServicePackMinor;
      dwBuildNumber := OSVI.dwBuildNumber;
      wProductType := OSVI.wProductType;
      wSuiteMask := OSVI.wSuiteMask;
      sCSDVersion := Trim(OSVI.szCSDVersion);
    end
    else begin
      // If error, retrieve TOsVersionInfo information
      OSVI.dwOSVersionInfoSize := SizeOf(TOsVersionInfo);
      if GetVersionEx(POSVI^) then
      begin
        dwPlatformId := OSVI.dwPlatformId;
        dwMajorVersion := OSVI.dwMajorVersion;
        dwMinorVersion := OSVI.dwMinorVersion;
        wServicePackMajor := 0;
        wServicePackMinor := 0;
        dwBuildNumber := OSVI.dwBuildNumber;
        wProductType := ProductTypeFromReg;
        wSuiteMask := 0;
        sCSDVersion := Trim(OSVI.szCSDVersion);
      end
      else raise Exception.Create(SysErrorMessage(GetLastError));
    end;

    (* Convert sCSDVersion for Windows NT4 *)

    if (dwPlatformId = VER_PLATFORM_WIN32_NT)
      and (dwMajorVersion = 4) and (dwMinorVersion = 0)
      and (CompareText(Result.sCSDVersion, 'Service Pack 6') = 0)
      and IsNT4SP6a then
        Result.sCSDVersion := Result.sCSDVersion + 'a';

    (* Convert sCSDVersion & dwBuildNumber for Windows 9x *)

    if dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
    begin
      dwBuildNumber := dwBuildNumber and $FFFF;
      if sCSDVersion <> '' then
      begin
        if (dwMajorVersion = 4) and (dwMinorVersion = 0)
          and (sCSDVersion[1] in ['B', 'b', 'C', 'c']) then
            sCSDVersion := 'OSR2';
        if (dwMajorVersion = 4) and (dwMinorVersion = 10)
          and (sCSDVersion[1] in ['A', 'a']) then
            sCSDVersion := 'SE';
      end;
    end;

    (* Retrieve ProcessorArchitecture *)

    FillChar(SI, SizeOf(SI), 0);
    // Try to get exended GetNativeSystemInfo information
    pfGetSystemInfo := GetProcAddress(GetModuleHandle(sKernelDll),
      'GetNativeSystemInfo');
    if Assigned(pfGetSystemInfo) then
      pfGetSystemInfo(SI)
    else begin
      GetSystemInfo(SI);
    end;
    wProcessorArchitecture := SI.wProcessorArchitecture;

    (* Retrieve LanguageId *)

    dwLanguageId := 0;
    pfGetSystemLCID := GetProcAddress(GetModuleHandle(sKernelDll),
      'GetSystemDefaultUILanguage');
    if Assigned(pfGetSystemLCID) then
      dwLanguageId := Word(pfGetSystemLCID);

    (* Check System Metrics *)

    tSysMetrics := smUndefined;
    if (wProductType <> VER_NT_WORKSTATION)
      and (dwMajorVersion = 5) and (dwMinorVersion = 2)
      and (GetSystemMetrics(SM_SERVERR2) <> 0) then
        tSysMetrics := smServerR2;
    if (wProductType = VER_NT_WORKSTATION)
      and (dwMajorVersion = 5) and (dwMinorVersion = 1)
      and (GetSystemMetrics(SM_MEDIACENTER) <> 0) then
        tSysMetrics := smMediaCenter;
    if (wProductType = VER_NT_WORKSTATION)
      and (dwMajorVersion = 5) and (dwMinorVersion = 1)
      and (GetSystemMetrics(SM_STARTER) <> 0) then
        tSysMetrics := smStarter;
    if (wProductType = VER_NT_WORKSTATION)
      and (dwMajorVersion = 5) and (dwMinorVersion = 1)
      and (GetSystemMetrics(SM_TABLETPC) <> 0) then
        tSysMetrics := smTabletPC;

    (* Retrieve ProductInfo (only for dwMajorVersion >= 6) *)

    if dwMajorVersion >=6 then
    begin
      pfGetProductInfo := GetProcAddress(GetModuleHandle(sKernelDll),
        'GetProductInfo');
      if Assigned(pfGetProductInfo) then
      begin
        bWin32HaveProductInfo := pfGetProductInfo(
          dwMajorVersion, dwMinorVersion,
          wServicePackMajor, wServicePackMinor,
          pdwProductInfo);
        if Result.bWin32HaveProductInfo
        then dwProductInfo := pdwProductInfo
        else dwProductInfo := PRODUCT_UNDEFINED;
      end;
    end;
  end;
end;

(*******************************************************************************)

procedure AppendToString(var SrcStr: string; const AddStr: string);
begin
  if AddStr <> '' then
  begin
    if AddStr[1] in [',', ' ']
    then SrcStr := SrcStr + AddStr
    else SrcStr := SrcStr + ' ' + AddStr;
  end;
end;

{*******************************************************************************
Get version of Windows (string)
In FmtTemplate maybe used:
%0:s  - Product Name & Edition (full)  etc: Server 2008 R2, Enterprise edition
%1:s  - Product Name & Edition (short) etc: Srv 2008R2 EE
%2:s  - Product Type (full)            etc: Server (Domain Controller)
%3:s  - Product Type (short)           etc: Srv (DC)
%4:s  - Product Family (full)          etc: Server 2008
%5:s  - Product Family (short)         etc: Srv 2008
%6:s  - Product Name (full)            etc: Server 2008 R2
%7:s  - Product Name (short)           etc: Srv 2008R2
%8:s  - Service Pack (full)            etc: Service Pack 1
%9:s  - Service Pack (short)           etc: SP1
%10:s - Architecture (full)            etc: 64-bit
%11:s - Architecture (short)           etc: x64
%12:s - Language (full)                etc: –усский
%13:s - Language (short)               etc: RUS
%14:d - Major Version                  etc: 6
%15:d - Minor Version                  etc: 1
%16:d - ServicePack Major              etc: 0
%17:d - ServicePack Minor              etc: 0
%18:d - Build                          etc: 3790
%19:d - Language Id                    etc: 1049
%20:s - Complete (full):  Server 2008, Enterprise edition Service Pack 1, RUS
%21:s - Complete (short): Srv 2008 EE SP1 RUS
*******************************************************************************}

function GetWindowsVersion(const VersionData: TWinVersionData; const FmtTemplate: string): string;
var
  sFamilyN, sFamilyS: string;
  sProductN, sProductS: string;
  sVersionN, sVersionS: string;
  sEditionN, sEditionS: string;
  sServiceN, sServiceS: string;
  sArchitectureN, sArchitectureS: string;
  sLanguageN, sLanguageS: string;

  procedure DetectEdition(const VersionData: TWinVersionData);
  begin
    { Detect Editions on dwProductInfo }

    with VersionData do
    begin
      if bWin32HaveProductInfo then
      begin
        if dwProductInfo = PRODUCT_UNLICENSED then
        begin
          AppendToString(sEditionN, 'Unlicensed');
          AppendToString(sEditionS, 'Unlicensed');
        end;
        { Workstation Editions }
        if dwProductInfo = PRODUCT_CORE then
        begin
          AppendToString(sEditionN, '');
          AppendToString(sEditionS, '');
        end;
        if dwProductInfo = PRODUCT_CORE_N then
        begin
          AppendToString(sEditionN, 'N');
          AppendToString(sEditionS, 'N');
        end;
        if dwProductInfo = PRODUCT_CORE_COUNTRYSPECIFIC then
        begin
          AppendToString(sEditionN, 'China');
          AppendToString(sEditionS, 'China');
        end;
        if dwProductInfo = PRODUCT_CORE_SINGLELANGUAGE then
        begin
          AppendToString(sEditionN, 'Single Language');
          AppendToString(sEditionS, 'SLang');
        end;
        if dwProductInfo = PRODUCT_STARTER then
        begin
          AppendToString(sEditionN, 'Starter');
          AppendToString(sEditionS, 'Starter');
        end;
        if dwProductInfo = PRODUCT_STARTER_E then
        begin
          AppendToString(sEditionN, 'Starter E');
          AppendToString(sEditionS, 'StarterE');
        end;
        if dwProductInfo = PRODUCT_STARTER_N then
        begin
          AppendToString(sEditionN, 'Starter N');
          AppendToString(sEditionS, 'StarterN');
        end;
        if dwProductInfo = PRODUCT_HOME_BASIC then
        begin
          AppendToString(sEditionN, 'Home Basic');
          AppendToString(sEditionS, 'HomeBsc');
        end;
        if dwProductInfo = PRODUCT_HOME_BASIC_E then
        begin
          AppendToString(sEditionN, 'Home Basic E');
          AppendToString(sEditionS, 'HomeBscE');
        end;
        if dwProductInfo = PRODUCT_HOME_BASIC_N then
        begin
          AppendToString(sEditionN, 'Home Basic N');
          AppendToString(sEditionS, 'HomeBscN');
        end;
        if dwProductInfo = PRODUCT_HOME_PREMIUM then
        begin
          AppendToString(sEditionN, 'Home Premium');
          AppendToString(sEditionS, 'HomePrem');
        end;
        if dwProductInfo = PRODUCT_HOME_PREMIUM_E then
        begin
          AppendToString(sEditionN, 'Home Premium E');
          AppendToString(sEditionS, 'HomePremE');
        end;
        if dwProductInfo = PRODUCT_HOME_PREMIUM_N then
        begin
          AppendToString(sEditionN, 'Home Premium N');
          AppendToString(sEditionS, 'HomePremN');
        end;
        if dwProductInfo = PRODUCT_PROFESSIONAL then
        begin
          AppendToString(sEditionN, 'Professional');
          AppendToString(sEditionS, 'Pro');
        end;
        if dwProductInfo = PRODUCT_PROFESSIONAL_E then
        begin
          AppendToString(sEditionN, 'Professional E');
          AppendToString(sEditionS, 'ProE');
        end;
        if dwProductInfo = PRODUCT_PROFESSIONAL_N then
        begin
          AppendToString(sEditionN, 'Professional N');
          AppendToString(sEditionS, 'ProN');
        end;
        if dwProductInfo = PRODUCT_PROFESSIONAL_WMC then
        begin
          AppendToString(sEditionN, 'Professional with Media Center');
          AppendToString(sEditionS, 'ProMC');
        end;
        if dwProductInfo = PRODUCT_ULTIMATE then
        begin
          AppendToString(sEditionN, 'Ultimate');
          AppendToString(sEditionS, 'Ult');
        end;
        if dwProductInfo = PRODUCT_ULTIMATE_E then
        begin
          AppendToString(sEditionN, 'Ultimate E');
          AppendToString(sEditionS, 'UltE');
        end;
        if dwProductInfo = PRODUCT_ULTIMATE_N then
        begin
          AppendToString(sEditionN, 'Ultimate N');
          AppendToString(sEditionS, 'UltN');
        end;
        if dwProductInfo = PRODUCT_ENTERPRISE then
        begin
          AppendToString(sEditionN, 'Enterprise');
          AppendToString(sEditionS, 'Ent');
        end;
        if dwProductInfo = PRODUCT_ENTERPRISE_E then
        begin
          AppendToString(sEditionN, 'Enterprise E');
          AppendToString(sEditionS, 'EntE');
        end;
        if dwProductInfo = PRODUCT_ENTERPRISE_N then
        begin
          AppendToString(sEditionN, 'Enterprise N');
          AppendToString(sEditionS, 'EntN');
        end;
        if dwProductInfo = PRODUCT_ENTERPRISE_N_EVALUATION then
        begin
          AppendToString(sEditionN, 'Enterprise N (evaluation installation)');
          AppendToString(sEditionS, 'EntN(eval)');
        end;

        { Server Editions }
        if dwProductInfo = PRODUCT_STANDARD_SERVER then
        begin
          AppendToString(sEditionN, 'Standard Edition');
          AppendToString(sEditionS, 'Std');
        end;
        if dwProductInfo = PRODUCT_STANDARD_SERVER_CORE then
        begin
          AppendToString(sEditionN, 'Standard Edition (core installation)');
          AppendToString(sEditionS, 'Std(core)');
        end;
        if dwProductInfo = PRODUCT_STANDARD_SERVER_V then
        begin
          AppendToString(sEditionN, 'Standard Edition without Hyper-V');
          AppendToString(sEditionS, 'StdWoHV');
        end;
        if dwProductInfo = PRODUCT_STANDARD_SERVER_CORE_V then
        begin
          AppendToString(sEditionN, 'Standard Edition without Hyper-V (core installation)');
          AppendToString(sEditionS, 'StdWoHV(core)');
        end;
        if dwProductInfo = PRODUCT_STANDARD_EVALUATION_SERVER then
        begin
          AppendToString(sEditionN, 'Standard Edition (evaluation installation)');
          AppendToString(sEditionS, 'Std(eval)');
        end;
        if dwProductInfo = PRODUCT_STANDARD_SERVER_SOLUTIONS then
        begin
          AppendToString(sEditionN, 'Solutions Premium Edition');
          AppendToString(sEditionS, 'SolPrem');
        end;
        if dwProductInfo = PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE then
        begin
          AppendToString(sEditionN, 'Solutions Premium Edition (core installation)');
          AppendToString(sEditionS, 'SolPrem(core)');
        end;
        if dwProductInfo = PRODUCT_BUSINESS then
        begin
          AppendToString(sEditionN, 'Business Edition');
          AppendToString(sEditionS, 'Bsn');
        end;
        if dwProductInfo = PRODUCT_BUSINESS_N then
        begin
          AppendToString(sEditionN, 'Business N Edition');
          AppendToString(sEditionS, 'Bsn N');
        end;
        if dwProductInfo = PRODUCT_CLUSTER_SERVER then
        begin
          AppendToString(sEditionN, 'Cluster Server Edition');
          AppendToString(sEditionS, 'Cluster');
        end;
        if dwProductInfo = PRODUCT_CLUSTER_SERVER_V then
        begin
          AppendToString(sEditionN, 'Cluster Server Edition without Hyper-V');
          AppendToString(sEditionS, 'ClusterWoHV');
        end;
        if dwProductInfo = PRODUCT_DATACENTER_EVALUATION_SERVER then
        begin
          AppendToString(sEditionN, 'Datacenter Edition (evaluation installation)');
          AppendToString(sEditionS, 'Data(eval)');
        end;
        if dwProductInfo = PRODUCT_DATACENTER_SERVER then
        begin
          AppendToString(sEditionN, 'Datacenter Edition');
          AppendToString(sEditionS, 'Data');
        end;
        if dwProductInfo = PRODUCT_DATACENTER_SERVER_CORE then
        begin
          AppendToString(sEditionN, 'Datacenter Edition (core installation)');
          AppendToString(sEditionS, 'Data(core)');
        end;
        if dwProductInfo = PRODUCT_DATACENTER_SERVER_CORE_V then
        begin
          AppendToString(sEditionN, 'Datacenter Edition without Hyper-V (core installation)');
          AppendToString(sEditionS, 'DataWoHV(core)');
        end;
        if dwProductInfo = PRODUCT_DATACENTER_SERVER_V then
        begin
          AppendToString(sEditionN, 'Datacenter Edition without Hyper-V');
          AppendToString(sEditionS, 'DataWoHV');
        end;
        if dwProductInfo = PRODUCT_ENTERPRISE_SERVER then
        begin
          AppendToString(sEditionN, 'Enterprise Edition');
          AppendToString(sEditionS, 'Ent');
        end;
        if dwProductInfo = PRODUCT_ENTERPRISE_EVALUATION then
        begin
          AppendToString(sEditionN, 'Enterprise Edition (evaluation installation)');
          AppendToString(sEditionS, 'Ent(eval)');
        end;
        if dwProductInfo = PRODUCT_ENTERPRISE_SERVER_CORE then
        begin
          AppendToString(sEditionN, 'Enterprise Edition (core installation)');
          AppendToString(sEditionS, 'Ent(core)');
        end;
        if dwProductInfo = PRODUCT_ENTERPRISE_SERVER_V then
        begin
          AppendToString(sEditionN, 'Enterprise Edition without Hyper-V');
          AppendToString(sEditionS, 'EntWoHV');
        end;
        if dwProductInfo = PRODUCT_ENTERPRISE_SERVER_CORE_V then
        begin
          AppendToString(sEditionN, 'Enterprise Edition without Hyper-V (core installation)');
          AppendToString(sEditionS, 'EntWoHV(core)');
        end;
        if dwProductInfo = PRODUCT_ENTERPRISE_SERVER_IA64 then
        begin
          AppendToString(sEditionN, 'Enterprise Edition for Itanium-based Systems');
          AppendToString(sEditionS, 'EntIA64');
        end;
        if dwProductInfo = PRODUCT_WEB_SERVER then
        begin
          AppendToString(sEditionN, 'Web Server Edition');
          AppendToString(sEditionS, 'Web');
        end;
        if dwProductInfo = PRODUCT_WEB_SERVER_CORE then
        begin
          AppendToString(sEditionN, 'Web Server Edition (core installation)');
          AppendToString(sEditionS, 'Web(core)');
        end;
        if dwProductInfo = PRODUCT_SERVER_FOR_SB_SOLUTIONS then
        begin
          AppendToString(sEditionN, 'For SB Solutions');
          AppendToString(sEditionS, 'SBSol');
        end;
        if dwProductInfo = PRODUCT_SB_SOLUTION_SERVER_EM then
        begin
          AppendToString(sEditionN, 'For SB Solutions EM');
          AppendToString(sEditionS, 'SBSolEM');
        end;
        if dwProductInfo = PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM then
        begin
          AppendToString(sEditionN, 'For SB Solutions EM');
          AppendToString(sEditionS, 'SBSolEM');
        end;
        if dwProductInfo = PRODUCT_SERVER_FOR_SMALLBUSINESS then
        begin
          AppendToString(sEditionN, 'for Windows Essential Server Solutions');
          AppendToString(sEditionS, 'EssSol');
        end;
        if dwProductInfo = PRODUCT_SERVER_FOR_SMALLBUSINESS_V then
        begin
          AppendToString(sEditionN, 'without Hyper-V for Windows Essential Server Solutions');
          AppendToString(sEditionS, 'EssSolWoHV');
        end;
        if dwProductInfo = PRODUCT_SMALLBUSINESS_SERVER then
        begin
          AppendToString(sEditionN, 'Small Business Server');
          AppendToString(sEditionS, 'SBServer');
        end;
        if dwProductInfo = PRODUCT_SMALLBUSINESS_SERVER_PREMIUM then
        begin
          AppendToString(sEditionN, 'Small Business Server Premium');
          AppendToString(sEditionS, 'SBServerPrem');
        end;
        if dwProductInfo = PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE then
        begin
          AppendToString(sEditionN, 'Small Business Server Premium (core installation)');
          AppendToString(sEditionS, 'SBServerPrem(core)');
        end;
        if dwProductInfo = PRODUCT_STORAGE_ENTERPRISE_SERVER then
        begin
          AppendToString(sEditionN, 'Storage Server Enterprise Edition');
          AppendToString(sEditionS, 'StorServer Ent');
        end;
        if dwProductInfo = PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE then
        begin
          AppendToString(sEditionN, 'Storage Server Enterprise Edition (core installation)');
          AppendToString(sEditionS, 'StorServer Ent(core)');
        end;
        if dwProductInfo = PRODUCT_STORAGE_EXPRESS_SERVER then
        begin
          AppendToString(sEditionN, 'Storage Server Express');
          AppendToString(sEditionS, 'StorServer Expr');
        end;
        if dwProductInfo = PRODUCT_STORAGE_EXPRESS_SERVER_CORE then
        begin
          AppendToString(sEditionN, 'Storage Server Express (core installation)');
          AppendToString(sEditionS, 'StorServer Expr(core)');
        end;
        if dwProductInfo = PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER then
        begin
          AppendToString(sEditionN, 'Storage Server Standard (evaluation installation)');
          AppendToString(sEditionS, 'StorServer Std(eval)');
        end;
        if dwProductInfo = PRODUCT_STORAGE_STANDARD_SERVER then
        begin
          AppendToString(sEditionN, 'Storage Server Standard');
          AppendToString(sEditionS, 'StorServer Std');
        end;
        if dwProductInfo = PRODUCT_STORAGE_STANDARD_SERVER_CORE then
        begin
          AppendToString(sEditionN, 'Storage Server Standard (core installation)');
          AppendToString(sEditionS, 'StorServer Std(core)');
        end;
        if dwProductInfo = PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER then
        begin
          AppendToString(sEditionN, 'Storage Server Workgroup (evaluation installation)');
          AppendToString(sEditionS, 'StorServer WrkGrp(eval)');
        end;
        if dwProductInfo = PRODUCT_STORAGE_WORKGROUP_SERVER then
        begin
          AppendToString(sEditionN, 'Storage Server Workgroup');
          AppendToString(sEditionS, 'StorServer WrkGrp');
        end;
        if dwProductInfo = PRODUCT_STORAGE_WORKGROUP_SERVER_CORE then
        begin
          AppendToString(sEditionN, 'Storage Server Workgroup (core installation)');
          AppendToString(sEditionS, 'StorServer WrkGrp(core)');
        end;

        { Other (renamed) }
        if dwProductInfo = PRODUCT_HOME_SERVER then
        begin
          sEditionN := 'Home Server 2011';
          sEditionS := 'Home Server 2011';
        end;
        if dwProductInfo = PRODUCT_HOME_PREMIUM_SERVER then
        begin
          sEditionN := 'Storage Server 2008 R2 Essentials';
          sEditionS := 'StorServer 2008 R2 Ess';
        end;
        if dwProductInfo = PRODUCT_HYPERV then
        begin
          sEditionN := 'Hyper-V Server';
          sEditionS := 'HV Server';
        end;
        if dwProductInfo = PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT then
        begin
          sEditionN := 'Essential Server Solution Management';
          sEditionS := 'Ess Server SM';
        end;
        if dwProductInfo = PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL then
        begin
          sEditionN := 'Essential Server Solution Additional';
          sEditionS := 'Ess Server SA';
        end;
        if dwProductInfo = PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC then
        begin
          sEditionN := 'Essential Server Solution Management SVC';
          sEditionS := 'Ess Server SM SVC';
        end;
        if dwProductInfo = PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC then
        begin
          sEditionN := 'Essential Server Solution Additional SVC';
          sEditionS := 'Ess Server SA SVC';
        end;
        if dwProductInfo = PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT then
        begin
          sEditionN := 'Essential Business Server Management Server';
          sEditionS := 'EssBsn Server Man';
        end;
        if dwProductInfo = PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING then
        begin
          sEditionN := 'Essential Business Server Messaging Server';
          sEditionS := 'EssBsn Server Msg';
        end;
        if dwProductInfo = PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY then
        begin
          sEditionN := 'Essential Business Server Security Server';
          sEditionS := 'EssBsn Server Sec';
        end;
        if dwProductInfo = PRODUCT_SOLUTION_EMBEDDEDSERVER then
        begin
          sEditionN := 'MultiPoint Server';
          sEditionS := 'MP Server';
        end;
        if dwProductInfo = PRODUCT_MULTIPOINT_STANDARD_SERVER then
        begin
          sEditionN := 'MultiPoint Server Standard';
          sEditionS := 'MP Server Std';
        end;
        if dwProductInfo = PRODUCT_MULTIPOINT_PREMIUM_SERVER then
        begin
          sEditionN := 'MultiPoint Server Premium';
          sEditionS := 'MP Server Prem';
        end;
        if dwProductInfo = PRODUCT_SERVER_FOUNDATION then
        begin
          sEditionN := 'Server Foundation';
          sEditionS := 'Server Fnd';
        end;
        if dwProductInfo = PRODUCT_SB_SOLUTION_SERVER then
        begin
          sEditionN := 'Small Business Server 2011 Essentials';
          sEditionS := 'SB Server 2011 Ess';
        end;
      end;

      { Processor Architecture (for 64-bit only) }

      if wProcessorArchitecture <> PROCESSOR_ARCHITECTURE_INTEL then
      begin
        AppendToString(sEditionN, 'x64');
        AppendToString(sEditionS, 'x64');
      end;
    end;
  end;

begin
  Result := '';

  with VersionData do
  begin
    (* Init variables *)

    sProductN := '';
    sProductS := '';
    sArchitectureN := '';
    sArchitectureS := '';
    sFamilyN  := Format('%d.%d', [dwMajorVersion, dwMinorVersion]);
    sFamilyS  := Format('%d.%d', [dwMajorVersion, dwMinorVersion]);
    sVersionN := Format('%d.%d.%d', [dwMajorVersion, dwMinorVersion, dwBuildNumber]);
    sVersionS := Format('%d.%d.%d', [dwMajorVersion, dwMinorVersion, dwBuildNumber]);
    sEditionN := sVersionN;
    sEditionS := sVersionS;
    sServiceN := sCSDVersion;
    sServiceS := Trim(StringReplace(
      StringReplace(sServiceN, 'Service Pack ', 'SP', [rfIgnoreCase, rfReplaceAll]),
      ' ', '', [rfReplaceAll]));

    (* Get System Language *)

    sLanguageN := GetLangNameLCID(dwLanguageId);
    sLanguageS := GetLangAbbrLCID(dwLanguageId);

    (* Get Product Type *)

    case wProductType of
      VER_NT_WORKSTATION:
      begin
        sProductN := 'Workstation';
        sProductS := 'Wst';
      end;
      VER_NT_DOMAIN_CONTROLLER:
      begin
        sProductN := 'Server (Domain Controller)';
        sProductS := 'Srv (DC)';
      end;
      VER_NT_SERVER:
      begin
        sProductN := 'Server';
        sProductS := 'Srv';
      end;
    end;

    (* Get Processor Architecture *)

    case wProcessorArchitecture of
      PROCESSOR_ARCHITECTURE_INTEL:
      begin
        sArchitectureN := '32-bit';
        sArchitectureS := 'x86';
      end;
     PROCESSOR_ARCHITECTURE_IA32_ON_WIN64,
     PROCESSOR_ARCHITECTURE_IA64:
      begin
        sArchitectureN := '64-bit Itanium';
        sArchitectureS := 'ia64';
      end;
     PROCESSOR_ARCHITECTURE_ALPHA64,
     PROCESSOR_ARCHITECTURE_AMD64:
      begin
        sArchitectureN := '64-bit';
        sArchitectureS := 'x64';
      end;
    end;

    (* Decode Windows Version *)

    case dwPlatformId of

      { Win32s platform }

      VER_PLATFORM_WIN32S:
      begin
        sFamilyN  := '32s';
        sFamilyS  := '32s';
        sVersionN := Format('%d.%d (Win32s)', [dwMajorVersion, dwMinorVersion]);
        sVersionS := Format('%d.%d (Win32s)', [dwMajorVersion, dwMinorVersion]);
        sEditionN := sVersionN;
        sEditionS := sVersionS;
      end;

      { Win9x platform }

      VER_PLATFORM_WIN32_WINDOWS:
      begin
        sFamilyN  := '32';
        sFamilyS  := '32';
        sVersionN := Format('%d.%d (Win32)', [dwMajorVersion, dwMinorVersion]);
        sVersionS := Format('%d.%d (Win32)', [dwMajorVersion, dwMinorVersion]);
        case dwMajorVersion of
          4: { Win9x family }
          begin
            sFamilyN  := '9x';
            sFamilyS  := '9x';
            sVersionN := Format('%d.%d (Win9x)', [dwMajorVersion, dwMinorVersion]);
            sVersionS := Format('%d.%d (Win9x)', [dwMajorVersion, dwMinorVersion]);
            case dwMinorVersion of
              0: { Windows 95 }
              begin
                sVersionN := '95';
                sVersionS := '95';
              end;
              10: { Windows 98 }
              begin
                sVersionN := '98';
                sVersionS := '98';
              end;
              90: { Windows ME }
              begin
                sVersionN := 'Millenium Edition';
                sVersionS := 'ME';
              end;
            end;
          end;
        end;
        sEditionN := sVersionN;
        sEditionS := sVersionS;
      end;

      { WinNT platform }

      VER_PLATFORM_WIN32_NT:
      begin
        if wProductType = VER_NT_WORKSTATION then
        begin
          sFamilyN  := Format('NT %d.%d Workstation', [dwMajorVersion, dwMinorVersion]);
          sFamilyS  := Format('NT%d.%d Wst', [dwMajorVersion, dwMinorVersion]);
        end
        else begin
          sFamilyN  := Format('NT %d.%d Server', [dwMajorVersion, dwMinorVersion]);
          sFamilyS  := Format('NT%d.%d Server', [dwMajorVersion, dwMinorVersion]);
        end;
        sVersionN := sFamilyN;
        sVersionS := sFamilyS;
        sEditionN := sVersionN;
        sEditionS := sVersionS;
        case dwMajorVersion of

          4: { Windows NT4 }

          begin
            case dwMinorVersion of
              0: { Windows NT4 }
              begin
                sFamilyN  := 'NT4';
                sFamilyS  := 'NT4';
                sVersionN := sFamilyN;
                sVersionS := sFamilyS;
                sEditionN := sVersionN;
                sEditionS := sVersionS;
                if wProductType = VER_NT_WORKSTATION then
                begin
                  AppendToString(sFamilyN, 'Workstation');
                  AppendToString(sFamilyS, 'Wst');
                  AppendToString(sVersionN, 'Workstation');
                  AppendToString(sVersionS, 'Wst');
                  AppendToString(sEditionN, 'Workstation');
                  AppendToString(sEditionS, 'Wst');
                end
                else begin
                  AppendToString(sFamilyN, 'Server');
                  AppendToString(sFamilyS, 'Server');
                  AppendToString(sVersionN, 'Server');
                  AppendToString(sVersionS, 'Server');
                  if wSuiteMask and VER_SUITE_ENTERPRISE > 0 then
                  begin
                    AppendToString(sEditionN, 'Enterprise Edition');
                    AppendToString(sEditionS, 'Ent Server');
                  end
                  else begin
                    AppendToString(sEditionN, 'Server');
                    AppendToString(sEditionS, 'Server');
                  end;
                end;
              end;
            end;
          end;

          5: { Windows 2000, Windows XP, Windows 2003 }

          begin
            case dwMinorVersion of

              0: { Windows 2000 }

              begin
                sFamilyN  := '2000';
                sFamilyS  := '2000';
                sVersionN := sFamilyN;
                sVersionS := sFamilyS;
                sEditionN := sVersionN;
                sEditionS := sVersionS;
                if wProductType = VER_NT_WORKSTATION then
                begin
                  AppendToString(sFamilyN, 'Professional');
                  AppendToString(sFamilyS, 'Pro');
                  AppendToString(sVersionN, 'Professional');
                  AppendToString(sVersionS, 'Pro');
                  AppendToString(sEditionN, 'Professional');
                  AppendToString(sEditionS, 'Pro');
                end
                else begin
                  AppendToString(sFamilyN, 'Server');
                  AppendToString(sFamilyS, 'Server');
                  AppendToString(sVersionN, 'Server');
                  AppendToString(sVersionS, 'Server');
                  if wSuiteMask and VER_SUITE_DATACENTER > 0 then
                  begin
                    AppendToString(sEditionN, 'Datacenter Server');
                    AppendToString(sEditionS, 'Data Server');
                  end
                  else if wSuiteMask and VER_SUITE_ENTERPRISE > 0 then
                  begin
                    AppendToString(sEditionN, 'Advanced Server');
                    AppendToString(sEditionS, 'Adv Server');
                  end
                  else begin
                    AppendToString(sEditionN, 'Server');
                    AppendToString(sEditionS, 'Server');
                  end;
                end;
              end;

              1: { Windows XP }

              begin
                sFamilyN  := 'XP';
                sFamilyS  := 'XP';
                sVersionN := sFamilyN;
                sVersionS := sFamilyS;
                sEditionN := sVersionN;
                sEditionS := sVersionS;
                if tSysMetrics = smStarter then
                begin
                  { Starter Edition }
                  AppendToString(sEditionN, 'Starter Edition');
                  AppendToString(sEditionS, 'Starter');
                end
                else if tSysMetrics = smMediaCenter then
                begin
                  { Media Center Edition }
                  AppendToString(sEditionN, 'Media Center Edition');
                  AppendToString(sEditionS, 'MC');
                end
                else if tSysMetrics = smTabletPC then
                begin
                  { Tablet PC Edition }
                  AppendToString(sEditionN, 'Tablet PC Edition');
                  AppendToString(sEditionS, 'TPC');
                end
                else if wSuiteMask and VER_SUITE_PERSONAL > 0 then
                begin
                  { Home Edition }
                  AppendToString(sEditionN, 'Home Edition');
                  AppendToString(sEditionS, 'Home');
                end
                else if wSuiteMask and VER_SUITE_EMBEDDEDNT > 0 then
                begin
                  { Embedded }
                  AppendToString(sEditionN, 'Embedded');
                  AppendToString(sEditionS, 'Emb');
                end
                else if wSuiteMask and VER_SUITE_EMBEDDED_RESTRICTED > 0 then
                begin
                  { Embedded }
                  AppendToString(sEditionN, 'Embedded Restricted');
                  AppendToString(sEditionS, 'EmbR');
                end
                else begin
                  { Professional }
                  AppendToString(sEditionN, 'Professional');
                  AppendToString(sEditionS, 'Pro');
                end;
              end;

              2: { Windows XP x64, Windows Server 2003 }

              begin
                if (wProductType = VER_NT_WORKSTATION)
                and (wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
                begin
                  { Windows XP Professional x64 }
                  sFamilyN  := 'XP';
                  sFamilyS  := 'XP';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  sEditionN := 'XP Professional x64 Edition';
                  sEditionS := 'XP Pro x64';
                end
                else begin
                  { Windows Server 2003 }
                  sFamilyN  := 'Server 2003';
                  sFamilyS  := 'Server 2003';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  { Windows Storage Server 2003 }
                  if wSuiteMask and VER_SUITE_STORAGE_SERVER > 0 then
                  begin
                    sVersionN := 'Storage Server 2003';
                    sVersionS := 'Stor Server 2003';
                  end;
                  { Windows Home Server }
                  if wSuiteMask and VER_SUITE_WH_SERVER > 0 then
                  begin
                    sVersionN := 'Home Server';
                    sVersionS := 'Home Server';
                    sEditionN := sVersionN;
                    sEditionS := sVersionS;
                  end
                  { Windows Server 2003 Editions }
                  else begin
                    { Windows Server 2003 R2 }
                    if tSysMetrics = smServerR2 then
                    begin
                      AppendToString(sVersionN, 'R2');
                      AppendToString(sVersionS, 'R2');
                    end;
                    sEditionN := sVersionN;
                    sEditionS := sVersionS;
                    { Editions for PROCESSOR_ARCHITECTURE_IA64 }
                    if wProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA64 then
                    begin
                      if wSuiteMask and VER_SUITE_DATACENTER > 0 then
                      begin
                        AppendToString(sEditionN, ', Datacenter Edition for Itanium-based Systems');
                        AppendToString(sEditionS, 'Data ia64');
                      end
                      else if wSuiteMask and VER_SUITE_ENTERPRISE > 0 then
                      begin
                        AppendToString(sEditionN, ', Enterprise Edition for Itanium-based Systems');
                        AppendToString(sEditionS, 'Ent ia64');
                      end
                      else begin
                        AppendToString(sEditionN, ', Edition for Itanium-based Systems');
                        AppendToString(sEditionS, 'ia64');
                      end;
                    end
                    { Editions for PROCESSOR_ARCHITECTURE_AMD64 }
                    else if wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64 then
                    begin
                      if wSuiteMask and VER_SUITE_DATACENTER > 0 then
                      begin
                        AppendToString(sEditionN, ', Datacenter x64 Edition');
                        AppendToString(sEditionS, 'Data x64');
                      end
                      else if wSuiteMask and VER_SUITE_ENTERPRISE > 0 then
                      begin
                        AppendToString(sEditionN, ', Enterprise x64 Edition');
                        AppendToString(sEditionS, 'Ent x64');
                      end
                      else begin
                        AppendToString(sEditionN, ', Standard x64 Edition');
                        AppendToString(sEditionS, 'x64');
                      end;
                    end
                    { Editions for PROCESSOR_ARCHITECTURE_INTEL }
                    else begin
                      if wSuiteMask and VER_SUITE_COMPUTE_SERVER > 0 then
                      begin
                        AppendToString(sEditionN, ', Compute Cluster Edition');
                        AppendToString(sEditionS, 'Cluster');
                      end
                      else if wSuiteMask and VER_SUITE_DATACENTER > 0 then
                      begin
                        AppendToString(sEditionN, ', Datacenter Edition');
                        AppendToString(sEditionS, 'Data');
                      end
                      else if wSuiteMask and VER_SUITE_ENTERPRISE > 0 then
                      begin
                        AppendToString(sEditionN, ', Enterprise Edition');
                        AppendToString(sEditionS, 'Ent');
                      end
                      else if wSuiteMask and VER_SUITE_BLADE > 0 then
                      begin
                        AppendToString(sEditionN, ', Web Edition');
                        AppendToString(sEditionS, 'Web');
                      end
                      else begin
                        AppendToString(sEditionN, ', Standard Edition');
                        AppendToString(sEditionS, 'Std');
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;

          6: { Windows Vista, Windows 7, Windows 8, Windows 2008, Windows 2012 }

          begin
            case dwMinorVersion of

              0: { Windows Vista, Windows 2008 }

              begin
                if wProductType = VER_NT_WORKSTATION then
                begin
                  { Windows Vista }
                  sFamilyN  := 'Vista';
                  sFamilyS  := 'Vista';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  sEditionN := sVersionN;
                  sEditionS := sVersionS;
                end
                else begin
                  { Windows 2008 }
                  sFamilyN  := 'Server 2008';
                  sFamilyS  := 'Server 2008';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  sEditionN := sVersionN;
                  sEditionS := sVersionS;
                end;
              end;

              1: { Windows 7, Windows 2008 R2 }

              begin
                if wProductType = VER_NT_WORKSTATION then
                begin
                  { Windows 7 }
                  sFamilyN  := '7';
                  sFamilyS  := '7';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  sEditionN := sVersionN;
                  sEditionS := sVersionS;
                end
                else begin
                  { Windows 2008 R2 }
                  sFamilyN  := 'Server 2008 R2';
                  sFamilyS  := 'Server 2008 R2';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  sEditionN := sVersionN;
                  sEditionS := sVersionS;
                end;
              end;

              2: { Windows 8, Windows 2012 }

              begin
                if wProductType = VER_NT_WORKSTATION then
                begin
                  { Windows 8 }
                  sFamilyN  := '8';
                  sFamilyS  := '8';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  sEditionN := sVersionN;
                  sEditionS := sVersionS;
                end
                else begin
                  { Windows 2012 }
                  sFamilyN  := 'Server 2012';
                  sFamilyS  := 'Server 2012';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  sEditionN := sVersionN;
                  sEditionS := sVersionS;
                end;
              end;

              3: { Windows 8.1, Windows Server 2012 R2 }

              begin
                if wProductType = VER_NT_WORKSTATION then
                begin
                  { Windows 8.1 }
                  sFamilyN  := '8.1';
                  sFamilyS  := '8.1';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  sEditionN := sVersionN;
                  sEditionS := sVersionS;
                end
                else begin
                  { Windows Server 2012 R2 }
                  sFamilyN  := 'Server 2012 R2';
                  sFamilyS  := 'Server 2012 R2';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  sEditionN := sVersionN;
                  sEditionS := sVersionS;
                end;
              end;
            end;

            DetectEdition(VersionData);
          end;

          10: { Windows 10, Windows Server 2016 }

          begin
            case dwMinorVersion of

              0: { Windows 10, Windows Server 2016 }

              begin
                if wProductType = VER_NT_WORKSTATION then
                begin
                  { Windows 10 }
                  sFamilyN  := '10';
                  sFamilyS  := '10';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  sEditionN := sVersionN;
                  sEditionS := sVersionS;
                end
                else begin
                  { Windows Server 2016 }
                  sFamilyN  := 'Server 2016';
                  sFamilyS  := 'Server 2016';
                  sVersionN := sFamilyN;
                  sVersionS := sFamilyS;
                  sEditionN := sVersionN;
                  sEditionS := sVersionS;
                end;
              end;
            end;

            DetectEdition(VersionData);
          end;
        end;
      end;
    end;

    (* Get Result *)

    Result := StringReplace(Format(FmtTemplate,
      [sEditionN, sEditionS, sProductN, sProductS,
       sFamilyN, sFamilyS, sVersionN, sVersionS,
       sServiceN, sServiceS, sArchitectureN, sArchitectureS,
       sLanguageN, sLanguageS, dwMajorVersion, dwMinorVersion,
       wServicePackMajor, wServicePackMinor, dwBuildNumber, dwLanguageId,
       sEditionN + ' ' + sLanguageS + ' ' + sServiceN,
       sEditionS + ' ' + sLanguageS + ' ' + sServiceS]),
      '  ', ' ', [rfReplaceAll]);
  end;
end;

procedure GetWindowsDescription(const VerData: TWinVersionData; TxtInfo: TStrings);
resourcestring
  sFmtDescriptionName                 = 'Microsoft Windows %0:s %8:s';
  sFmtDescriptionExtData              = 'јрхитектура: %11:s (%10:s); верси€ продукта: %14:d.%15:.2d.%18:.3d';
  sFmtDescriptionLang                 = 'язык GUI: %12:s (%13:s), id: %19:d (0x%19:x)';
begin
  if (VerData.dwPlatformId > 0) and (VerData.dwMajorVersion > 0) then
  begin
    TxtInfo.BeginUpdate;
    try
      TxtInfo.Clear;
      TxtInfo.Add(GetWindowsVersion(VerData, sFmtDescriptionName));
      TxtInfo.Add(GetWindowsVersion(VerData, sFmtDescriptionExtData));
      TxtInfo.Add(GetWindowsVersion(VerData, sFmtDescriptionLang));
    finally
      TxtInfo.EndUpdate;
    end;
  end;
end;


end.
