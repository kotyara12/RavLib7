unit ExpandCPUInfo;

interface

type
  TCPUInfo = packed record
    IDString : array [0..11] of Char;
    Stepping : Integer;
    Model    : Integer;
    Family   : Integer;
    FPU,
    VirtualModeExtensions,
    DebuggingExtensions,
    PageSizeExtensions,
    TimeStampCounter,
    K86ModelSpecificRegisters,
    MachineCheckException,
    CMPXCHG8B,
    APIC,
    MemoryTypeRangeRegisters,
    GlobalPagingExtension,
    ConditionalMoveInstruction,
    MMX     : Boolean;
    SYSCALLandSYSRET,
    FPConditionalMoveInstruction,
    AMD3DNow : Boolean;
    CPUName : string;
  end;

{информация об идентификации процессора}
function ExistCPUID:Boolean;
function CPUIDInfo(out Info: TCPUInfo):Boolean;

{инф-я о технологии процессора}
function ExistMMX:Boolean;
function Exist3DNow:Boolean;
function ExistKNI:Boolean;

{------------------------}
procedure EMMS;
procedure FEMMS;
procedure PREFETCH(p: Pointer); register;

implementation

uses
  Windows;

function ExistCPUID: Boolean;
var
  TempDetect: DWord;
begin
{asm
  pushfd
  pop eax
  mov ebx, eax
  xor eax, 00200000h
  push eax
  popfd
  pushfd
  pop ecx
  mov eax,0
  cmp ecx, ebx
  jz @NO_CPUID
  inc eax
  @NO_CPUID:
end;}
  asm
    pushf
    pushfd
    push eax
    push ebx
    push ecx
    push edx

    pushfd
    pop eax
    mov ebx,eax
    xor eax,$00200000
    push eax
    popfd
    pushfd
    pop eax
    push ebx
    popfd
    xor eax,ebx
    mov TempDetect,eax

    pop edx
    pop ecx
    pop ebx
    pop eax
    popfd
    popf
  end;
  Result := (TempDetect=$00200000);
end;

function CPUIDInfo(out Info: TCPUInfo):Boolean;

  function ExistExtendedCPUIDFunctions:Boolean;
  asm
    mov eax,080000000h
    db $0F,$A2
  end;

var
  name : array [0..47] of Char;
  p : Pointer;
begin
  if ExistCPUID then
  asm
    jmp @Start
    @BitLoop:
    mov al,dl
    and al,1
    mov [edi],al
    shr edx,1
    inc edi
    loop @BitLoop
    ret
    @Start:
    mov edi, Info
    mov eax,0
    db $0F,$A2
    mov [edi],ebx
    mov [edi+4],edx
    mov [edi+8],ecx
    mov eax,1
    db $0F,$A2
    mov ebx,eax
    and eax,0fh;
    mov [edi+12],eax;
    shr ebx,4
    mov eax,ebx
    and eax,0fh
    mov [edi+12+4],eax
    shr ebx,4
    mov eax,ebx
    and eax,0fh
    mov [edi+12+8],eax
    add edi,24
    mov ecx,6
    call @BitLoop
    shr edx,1
    mov ecx,3
    call @BitLoop
    shr edx,2
    mov ecx,2
    call @BitLoop
    shr edx,1
    mov ecx,1
    call @BitLoop
    shr edx,7
    mov ecx,1
    call @BitLoop
    mov p,edi
  end;
  if (Info.IDString = 'AuthenticAMD') and ExistExtendedCPUIDFunctions then
  begin
    asm
      mov edi,p
      mov eax,080000001h
      db $0F,$A2
      mov eax,edx
      shr eax,11
      and al,1
      mov [edi],al
      mov eax,edx
      shr eax,16
      and al,1
      mov [edi+1],al
      mov eax,edx
      and al,1
      mov [edi+2],al
      lea edi,name
      mov eax,0
      mov [edi],eax
      mov eax,080000000h
      db $0F,$A2
      cmp eax,080000004h
      jl @NoString
      mov eax,080000002h
      db $0F,$A2
      mov [edi],eax
      mov [edi+4],ebx
      mov [edi+8],ecx
      mov [edi+12],edx
      add edi,16
      mov eax,080000003h
      db $0F,$A2
      mov [edi],eax
      mov [edi+4],ebx
      mov [edi+8],ecx
      mov [edi+12],edx
      add edi,16
      mov eax,080000004h
      db $0F,$A2
      mov [edi],eax
      mov [edi+4],ebx
      mov [edi+8],ecx
      mov [edi+12],edx
      @NoString:
    end;
    Info.CPUName := Name;
  end
  else
    with info do
    begin
      SYSCALLandSYSRET := False;
      FPConditionalMoveInstruction := False;
      AMD3DNow := False;
      CPUName := '';
    end;
  Result := ExistCPUID;
end;

function ExistMMX:Boolean;
var
  Info :TCPUInfo;
begin
  if CPUIDInfo(Info)
  then Result := Info.MMX
  else Result := False;
end;

function Exist3DNow:Boolean;
var
  Info :TCPUInfo;
begin
  if CPUIDInfo(Info)
  then Result := Info.AMD3DNow
  else Result := False;
end;

function ExistKNI:Boolean;
begin
  Result := False;
end;

procedure EMMS;
asm
  db $0F,$77
end;

procedure FEMMS;
asm
  db $0F,$03
end;

procedure PREFETCH(p: Pointer); register;
asm
  // PREFETCH byte ptr [eax]
end;

end.
