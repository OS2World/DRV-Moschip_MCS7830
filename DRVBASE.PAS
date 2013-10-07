{$B-,D+,H-,I-,J+,P-,Q-,R-,S-,T-,V+,W-,X+,Z-}
{&AlignCode-,AlignData-,AlignRec-,Asm-,Cdecl-,Comments-,Delphi-,Frame-,G5+}
{&LocInfo+,Open32-,Optimise+,OrgName-,SmartLink+,Speed+,Use32+,ZD-}
{$M 32768}

UNIT DRVBASE;

INTERFACE

{******************************************}
{*                                        *}
{* DRIVER Helper Base Types               *}
{*                                        *}
{******************************************}
CONST
  CR          = CHR(13);
  LF          = CHR(10);

TYPE
  Offset  = SmallWord;
  Semaphore=LongInt;
  SemHandle=LongInt;
  Selector= SmallWord;
  PSelector= ^Selector;
  ApiRet  = Longint;
  ApiRet16 = Word;
  SHandle = Word;
  LHandle = Longint;
  HPointer = LHandle;
  FHLock  = Longint;
  ULong   = Longint;
  Long    = Longint;
  Bool    = LongBool;
  UShort  = SmallWord;
  PULong  = ^ULong;
  PByte   = ^Byte;
  PLong   = ^Long;
  PFn     = Pointer;
  PUShort = ^UShort;
  PPChar  = ^PChar;
  HFile    = SHandle;
  DrvRet=    ApiRet16;
  PIDCStruct=^IDCStruct;
  PointerP=  Word;
  PointerPCI=ULong;
  PPointer16=^Pointer16;
  Pointer16=RECORD
    CASE Boolean OF
    FALSE:(
      Off:Offset;
      Seg:Selector;);
    TRUE: (
      Ptr:ULong;);
  END;
  IDCStruct=RECORD
    ResP  :Pointer16;
    ResS  :Selector;
    PEntry:Pointer16;
    EntryS:Selector;
  END;
  PhysPage=RECORD
    Handle:ULong;
    Start :PointerP;
    Size  :ULong;
  END;
  PGlobalInfo=^GlobalInfo;
  GlobalInfo=RECORD
    Time      :ULong;
    Millisecs :ULong;
    Hours     :Byte;
    Minute    :Byte;
    Seconds   :Byte;
    HundredSec:Byte;
    TimeZone  :SmallWord;
    Interval  :SmallWord;
    Day       :Byte;
    Month     :Byte;
    Year      :SmallWord;
    Weekday   :Byte;
    MajorVersion:Byte;
    MinorVersion:Byte;
    Revision  :Char;
    CurrentSession:Byte;
    MaxNumSessions:Byte;
    HugeShift :Byte;
    ProtModeInd :Byte;
    LastProcess :SmallWord;
    DynVarFlag:Byte;
    MaxWait   :Byte;
    MinTimeSlice:SmallWord;
    MaxTimeSlice:SmallWord;
    BootDrive :Byte;
    TraceFlags:ARRAY[1..32] of Byte;
    MaxTextSessions:SmallWord;
    MaxPMSessions:SmallWord;
  END;

{******************************************}
{*                                        *}
{* I/O Functions                          *}
{*                                        *}
{******************************************}


  FUNCTION DevReadB(Port:SmallWord):Byte;
  FUNCTION DevReadW(Port:SmallWord):SmallWord;
  FUNCTION DevReadD(Port:SmallWord):ULong;

  PROCEDURE DevWriteB(Port:SmallWord;Value:Byte);
  PROCEDURE DevWriteW(Port:SmallWord;Value:SmallWord);
  PROCEDURE DevWriteD(Port:SmallWord;Value:ULong);

  FUNCTION DosGetPCIDevice(DevID,Vendor:UShort;VAR Addr:PointerPCI):ApiRet;

  FUNCTION DevReadPCID(Addr:PointerPCI;Reg:Byte):ULong;
  FUNCTION DevReadPCIW(Addr:PointerPCI;Reg:Byte):UShort;
  FUNCTION DevReadPCIB(Addr:PointerPCI;Reg:Byte):Byte;
  PROCEDURE DevWritePCID(Addr:PointerPCI;Reg:Byte;Value:ULong);
  PROCEDURE DevWritePCIW(Addr:PointerPCI;Reg:Byte;Value:UShort);
  PROCEDURE DevWritePCIB(Addr:PointerPCI;Reg,Value:Byte);

{******************************************}
{*                                        *}
{* DRIVER Helper Functions and AddressConv*}
{*                                        *}
{******************************************}

{debug}
  PROCEDURE Stop;
  PROCEDURE MarkFS(Value:UShort); {Val=$30,70,78,160,168}
  PROCEDURE ReleaseFS;

{Driver Management}
  FUNCTION DevBeep(Freq,Dur: Longint):ApiRet;
  FUNCTION GetCallInAddr(Func:SmallWord):Pointer16;
  FUNCTION DevGetGlobalInfo:PGlobalInfo;
  FUNCTION DevGetKrnlInfo(RegAX,RegCX:UShort):Pointer;
  FUNCTION DevSetOptions(DDName:PChar;IDCOpt:SmallWord;
                         IRQOpt:SmallWord;HookMask:ULong;
                         InfoPtr:Pointer16):ApiRet;
{Process Management}
  FUNCTION DevYield:ApiRet;
  FUNCTION DevTCYield:ApiRet;
  FUNCTION DevArmCtxHook(Hook:ULong;Params:ULong):ApiRet;
  FUNCTION DevSemRequest(Sem:Pointer16;Timeout:ULong):ApiRet;
  FUNCTION DevSemClear(Sem:Pointer16):ApiRet;
  PROCEDURE DevSleep(Time:ULong);
{Stack Management}
  FUNCTION LocToDat16(LocPtr:Pointer):Pointer16;
  FUNCTION LocToDat32(LocPtr:Pointer):Pointer;
{Address conversion}
  FUNCTION DevVirtToLin(Ptr16:Pointer16):Pointer;
  FUNCTION DevLinToGDTSelector(Sel:Selector;Addr:Pointer;Size:SmallWord):ApiRet;
  FUNCTION DevLinToVirt(Flat:Pointer):Pointer16;
{Memory Management}
  FUNCTION DevAllocGDTSelector(var Buffer:Selector;Count:SmallInt):ApiRet;
  FUNCTION DevGetMemPhys(Size:SmallWord;ISA:Boolean;var Pages:PhysPage):Pointer;
  FUNCTION DevGetMem(Size:ULong;Flags:ULong):Pointer;
  FUNCTION DevFreeMem(Mem:Pointer):ApiRet;
  FUNCTION DevMapPhys(Phys:PointerP;Size:ULong;AllocMem:Boolean):Pointer;
  FUNCTION DevMapGlobalToProc(Glob:Pointer;Size:ULong):Pointer;
  FUNCTION DevMapProcToGlobal(Proc:Pointer;Size:ULong):Pointer;
{Packet Management}
  FUNCTION DevVerifyAccess(Mem:Pointer16;Len:SmallWord;Flags:Byte):ApiRet;
  FUNCTION DevAllocReqPacket(NoWait:Boolean):Pointer16;
  PROCEDURE DevFreeReqPacket(Packet:Pointer16);
  PROCEDURE DevPushReqPacket(Queue:PULong;Request:Pointer16);
  FUNCTION DevPullReqPacket(Queue:PULong):Pointer16;
  FUNCTION DevPullParticular(Queue:PULong;Request:Pointer16):Pointer16;
  PROCEDURE DevDone(Request:Pointer16);
{Semaphore Management}
  FUNCTION DevOpenEventSem(Sem:SemHandle):ApiRet;
  FUNCTION DevCloseEventSem(Sem:SemHandle):ApiRet;
  FUNCTION DevPostEventSem(Sem:SemHandle):ApiRet;
{Timer Management}
  FUNCTION DevTickCount(Count:SmallWord):ApiRet;
  FUNCTION DevResetTimer:ApiRet;
{IRQ Management}
  FUNCTION DevSetIRQ(Shared:SmallWord):ApiRet;
  FUNCTION DevUnSetIRQ:ApiRet;
{IDC Management}
  FUNCTION DevAttachDD(DDName:PChar;IDCData:PIDCStruct):ApiRet;
  FUNCTION DevCallDD16(IDC:PIDCStruct;Data:Pointer16;Param:SmallWord):ApiRet;
  FUNCTION DevCallDD32(IDC:PIDCStruct;Data:Pointer;Param:SmallWord):ApiRet;

CONST
{Options}
  IDC_NO    = 0;      {no IDC}
  IDC_NONE  = 1;      {IDC, no Args}
  IDC_REGS  = 2;      {IDC, Args ES:BX}
  IDC_STACK = 3;      {IDC, Args Pointer by Stack}
  IRQ_NONE  = $FF;    {no IRQ, 0..F = IRQ-Number}
  CTX_HOOK  = 1;      {Alloc Context Hook, may be added to next}
  CTX_THTC  = 2;      {Alloc TimeCritical Context Thread, may be added to next}
  CTX_THFS  = 4;      {Alloc ForegroundServer Context Thread}

  IRQ_EXCLUSIVE = $0000;
  IRQ_SHARED    = $0100;

  VERIFY_READ   = 0;
  VERIFY_RW     = 1;

  MEM_FIXED     = $02;
  MEM_SWAP      = $04;

  DD_NOPARM     = $FFFF;

  SEM_FOREVER   = $FFFFFFFF;
  SEM_NOWAIT    = $0;

{******************************************}
{*                                        *}
{* DRIVER DosCalls                        *}
{*                                        *}
{******************************************}

  FUNCTION DevDevIOCtl(DrvName:PChar; Category,Func: ULong;
                       Params: Pointer;ParamLen: ULong;
                       Data: Pointer;DataLen: ULong): ApiRet;

{******************************************}
{*                                        *}
{* DRIVER Request Packet                  *}
{*                                        *}
{******************************************}
TYPE
  DrvRequest=RECORD
    Length:       Byte;
    Unit_:        Byte;
    Command:      Byte;
    Status:       SmallWord;
    MePtr16:      Pointer16;
    QueueLink:    Pointer16;
    CASE Byte OF
    [0]:( {INIT COMMAND}
      Data1:      Byte;
      pDevHelp:   Pointer16;
      pInitArg:   Pointer16;
      FirstUnit:  Byte;);
    [4]:( {READ,WRITE COMMAND}
      Media:      Byte;
      pData:      Pointer;
      Size:       SmallWord;
      StartSector:Word;);
    [16]:({IOCTL}
      Category:   Byte;
      Code:       Byte;
      pIOParams:  Pointer16;
      pIOData:    Pointer16;
      FileNr:     Byte;
      ParamLen:   SmallWord;
      DataLen:    SmallWord;);
  END;
  PDrvRequest=^DrvRequest;

CONST
  COMMAND_INIT    =$00;
  COMMAND_READ    =$04;
  COMMAND_OPEN    =$0D;
  COMMAND_CLOSE   =$0E;
  COMMAND_IOCTL   =$10;
  COMMAND_BDINIT  =$1B;
  COMMAND_SHUTDOWN=$1C;
  COMMAND_INITCMPL=$1F;

  STATUS_DONE     =$0100;
  STATUS_WAIT     =$0000;
  STATUS_ILLEGAL  =$8103;
  STATUS_GENFAIL  =$810C;

{******************************************}
{*                                        *}
{* C STRING STUFF                         *}
{*                                        *}
{* taken from Unit Strings VPC Runtime    *}
{* Copyright (C) 1995-2000 vpascal.com    *}
{*                                        *}
{******************************************}

procedure MemMove(Src,Dest: Pointer; Count: Longint);
procedure MemFill(Dest: Pointer; Count: Longint; Value: Byte);
function StrLen(Str: PChar): Word;
function StrEnd(Str: PChar): PChar;
function StrMove(Dest, Source: PChar; Count: Word): PChar;
function StrCopy(Dest, Source: PChar): PChar;
function StrECopy(Dest, Source: PChar): PChar;
function StrLCopy(Dest, Source: PChar; MaxLen: Word): PChar;
function StrCat(Dest, Source: PChar): PChar;
function StrLCat(Dest, Source: PChar; MaxLen: Word): PChar;
function StrComp(Str1, Str2: PChar): Integer;
function StrIComp(Str1, Str2: PChar): Integer;
function StrLComp(Str1, Str2: PChar; MaxLen: Word): Integer;
function StrLIComp(Str1, Str2: PChar; MaxLen: Word): Integer;
function StrScan(Str: PChar; Chr: Char): PChar;
function StrRScan(Str: PChar; Chr: Char): PChar;
function StrPos(Str1, Str2: PChar): PChar;
function StrUpper(Str: PChar): PChar;
function StrLower(Str: PChar): PChar;
procedure BytesToHex(ByteStr1,Str2:PChar;ByteLen:Byte);
procedure HexToBytes(CharStr1,Str2:PChar;ByteLen:Byte);

{******************************************}
{*                                        *}
{* INFO I/O Routines and Param Parser     *}
{*                                        *}
{*                                        *}
{******************************************}
PROCEDURE InfInit(Buffer:PChar;Size:ULong);
PROCEDURE InfSet(Log:Boolean);
PROCEDURE InfAdd(Str:PChar;Val:Pointer);  {Add an Infotext to Buffer}
PROCEDURE InfCopy(pBuffer:Pointer;var Len:SmallWord);

FUNCTION GetParam(Buffer:PChar;ParamStr:PChar;Val:PULong):Boolean;

IMPLEMENTATION

{******************************************}
{*                                        *}
{* DRIVER IO                              *}
{*                                        *}
{******************************************}

FUNCTION DevReadB(Port:SmallWord):Byte;
         assembler;
asm
              MOV     DX,Port
              IN      AL,DX
              MOVZX   EAX,AL
end;

FUNCTION DevReadW(Port:SmallWord):SmallWord;
         assembler;  {tested, works correct in 32Bit Seg}
asm
              MOV     DX,Port
              IN      AX,DX
              MOVZX   EAX,AX
end;

FUNCTION DevReadD(Port:SmallWord):ULong;
         assembler;  {tested, works correct in 32Bit Seg}
asm
              MOV     DX,Port
              IN      EAX,DX
end;

PROCEDURE DevWriteB(Port:SmallWord;Value:Byte);
          assembler;
asm
              MOV     DX,Port
              MOV     AL,Value
              OUT     DX,AL
end;

PROCEDURE DevWriteW(Port:SmallWord;Value:SmallWord);
          assembler;
asm
              MOV     DX,Port
              MOV     AX,Value
              OUT     DX,AX
end;

PROCEDURE DevWriteD(Port:SmallWord;Value:ULong);
          assembler;
asm
              MOV     DX,Port
              MOV     EAX,Value
              OUT     DX,EAX
end;

{******************************************}
{*                                        *}
{* PCI IO                                 *}
{*                                        *}
{******************************************}

FUNCTION DosGetPCIDevice(DevID,Vendor:UShort;VAR Addr:PointerPCI):ApiRet;
TYPE
  FINDPCIDEVPP=RECORD
    SFunction :Byte;
    DeviceID  :UShort;
    VendorID  :UShort;
    DevIndex  :Byte;
  END;
  FINDPCIDEVDAT=RECORD
    ReturnCode:Byte;
    BusNumber :Byte;
    FuncNumber:Byte;
  END;
VAR
  rc    :ApiRet;
  Parm  :FINDPCIDEVPP;
  Data  :FINDPCIDEVDAT;

BEGIN
  WITH Parm DO
  BEGIN
    SFunction:=1;
    DeviceID:=DevID;
    VendorID:=Vendor;
    DevIndex:=0;
  END;
  Data.ReturnCode:=$FF;
  rc:=DevDevIOCtl('OEMHLP$ ',$80,$0B,
                  LocToDat32(@Parm),SizeOf(Parm),
                  LocToDat32(@Data),SizeOf(Data));
  WITH Data DO
  BEGIN
    DosGetPCIDevice:=ReturnCode;
    Addr:=$80000000 + (BusNumber SHL $10) + (FuncNumber SHL 8);
  END;
END;

FUNCTION DevReadPCID(Addr:PointerPCI;Reg:Byte):ULong;
         assembler; {&USES ebx}{&FRAME+}
asm
              PUSHF
              CLI
              MOV     DX,$0CF8
              IN      EAX,DX
              MOV     EBX,EAX
              MOV     EAX,Addr
              MOV     AL,Reg
              AND     AL,$FC
              OUT     DX,EAX
              MOV     DX,$0CFC
              IN      EAX,DX
              MOV     ECX,EAX
              MOV     DX,$0CF8
              MOV     EAX,EBX
              OUT     DX,EAX
              MOV     EAX,ECX
              POPF
end;

FUNCTION DevReadPCIW(Addr:PointerPCI;Reg:Byte):UShort;
         assembler; {&USES ebx}{&FRAME+}
asm
              PUSHF
              CLI
              MOV     DX,$0CF8
              IN      EAX,DX
              MOV     EBX,EAX
              MOV     EAX,Addr
              MOV     AL,Reg
              AND     AL,$FE
              OUT     DX,EAX
              MOV     DX,$0CFC
              AND     AL,$02
              OR      DL,AL
              IN      AX,DX
              MOVZX   ECX,AX
              MOV     DX,$0CF8
              MOV     EAX,EBX
              OUT     DX,EAX
              MOV     EAX,ECX
              POPF
end;

FUNCTION DevReadPCIB(Addr:PointerPCI;Reg:Byte):Byte;
         assembler; {&USES ebx}{&FRAME+}
asm
              PUSHF
              CLI
              MOV     DX,$0CF8
              IN      EAX,DX
              MOV     EBX,EAX
              MOV     EAX,Addr
              MOV     AL,Reg
              OUT     DX,EAX
              MOV     DX,$0CFC
              AND     AL,$03
              OR      DL,AL
              IN      AL,DX
              MOVZX   ECX,AL
              MOV     DX,$0CF8
              MOV     EAX,EBX
              OUT     DX,EAX
              MOV     EAX,ECX
              POPF
end;

PROCEDURE DevWritePCID(Addr:PointerPCI;Reg:Byte;Value:ULong);
          assembler; {&USES ebx}{&FRAME+}
asm
              PUSHF
              CLI
              MOV     DX,$0CF8
              IN      EAX,DX
              MOV     EBX,EAX
              MOV     EAX,Addr
              MOV     AL,Reg
              AND     AL,$FC
              OUT     DX,EAX
              MOV     DX,$0CFC
              MOV     EAX,Value
              OUT     DX,EAX
              MOV     DX,$0CF8
              MOV     EAX,EBX
              OUT     DX,EAX
              POPF
end;

PROCEDURE DevWritePCIW(Addr:PointerPCI;Reg:Byte;Value:UShort);
          assembler; {&USES ebx}{&FRAME+}
asm
              PUSHF
              CLI
              MOV     DX,$0CF8
              IN      EAX,DX
              MOV     EBX,EAX
              MOV     EAX,Addr
              MOV     AL,Reg
              AND     AL,$FE
              OUT     DX,EAX
              MOV     DX,$0CFC
              AND     AL,$02
              OR      DL,AL
              MOV     AX,Value
              OUT     DX,AX
              MOV     DX,$0CF8
              MOV     EAX,EBX
              OUT     DX,EAX
              POPF
end;

PROCEDURE DevWritePCIB(Addr:PointerPCI;Reg,Value:Byte);
          assembler; {&USES ebx}{&FRAME+}
asm
              PUSHF
              CLI
              MOV     DX,$0CF8
              IN      EAX,DX
              MOV     EBX,EAX
              MOV     EAX,Addr
              MOV     AL,Reg
              OUT     DX,EAX
              MOV     DX,$0CFC
              AND     AL,$03
              OR      DL,AL
              MOV     AL,Value
              OUT     DX,AL
              MOV     DX,$0CF8
              MOV     EAX,EBX
              OUT     DX,EAX
              POPF
end;




{******************************************}
{*                                        *}
{* DRIVER AdressConversion and Helpers    *}
{*                                        *}
{******************************************}
PROCEDURE Stop; assembler; {&FRAME-}
asm
  INT 3
end;

PROCEDURE MarkFS(Value:UShort); assembler; {&FRAME-}
asm
  MOV AX,Value
  MOV FS,AX
end;

PROCEDURE ReleaseFS; assembler; {&FRAME-}
asm
  XOR AX,AX
  MOV FS,AX
end;

PROCEDURE DevHlp32; EXTERNAL;
PROCEDURE GetGDTSelInf32; EXTERNAL;
FUNCTION DevYield:ApiRet; EXTERNAL;
FUNCTION DevTCYield:ApiRet; EXTERNAL;

CONST
  CDH_SemRequest= $06;
  CDH_SemClear  = $07;
  CDH_GetDosVar = $24;
  CDH_VerfyAccess=$27;
  CDH_Beep      = $52;
  CDH_WMLock    = $55;
  CDH_WMAlloc   = $57;
  CDH_WMFree    = $58;
  CDH_WMProc2Glo= $59;
  CDH_WMGlob2Prc= $5A;
  CDH_SelToFlat = $5B;
  CDH_LinToGDTSel=$5C;
  CDH_WMSetMem  = $66;
  CDH_OpenEvent = $67;
  CDH_CloseEvent= $68;
  CDH_PostEvent = $69;
  CDH_GetCallIn = $0000FF;
  CDH_ArmCtxHook= $0001FF;
  CDH_Memory    = $0007FF;
  CDH_ReqQueue  = $0008FF;
  CDH_DevDone   = $0009FF;
  CDH_IRQ       = $000AFF;
  CDH_SetTimer  = $000BFF;
  CDH_CallDDD16 = $000CFF;
  CDH_CallDDD32 = $000DFF;
  CDH_AttachDD  = $000EFF;
  CDH_SetOptions= $000FFF;

FUNCTION LocToDat16(LocPtr:Pointer):Pointer16;
         assembler;
asm
              MOV     AX,SS
              SHL     EAX,$10
              MOV     ECX,LocPtr
              MOV     AX,CX
end;

FUNCTION LocToDat32(LocPtr:Pointer):Pointer; EXTERNAL;


FUNCTION DevVirtToLin(Ptr16:Pointer16):Pointer;
         assembler; {&USES esi}
asm
              MOV     EAX,Ptr16
              SHR     EAX,$10
              MOV     ESI,Ptr16
              AND     ESI,$0000FFFF
              MOV     DX,AX
              AND     DX,0FFF8h
              CMP     DX,0h
              JE      @@ERR
              TEST    AX,0004h
              JNZ     @@LDT
              VERR    AX
              JNZ     @@ERR
              PUSH    ECX
              CALL    GetGDTSelInf32
              MOV     EAX,ESI
              ADD     EAX,ECX
              POP     ECX
              JMP     @@OK
              @@LDT:
              MOV     EDX,CDH_SelToFlat
              CALL    DevHlp32
              JAE     @@OK
              @@ERR:
              XOR     EAX,EAX
              @@OK:
end;

FUNCTION DevGetGlobalInfo:PGlobalInfo;
         assembler; {&USES ebx}
asm
              MOV     AX,01
              XOR     EBX,EBX
              MOV     DX,CDH_GetDosVar
              CALL    DevHlp32
              JB      @@ERR
              PUSH    FS
              MOV     FS,AX
              MOV     AX,FS:[EBX]
              POP     FS
              CALL    GetGDTSelInf32;
              MOV     EAX,ECX
              JMP     @@OK
              @@ERR:
              XOR     EAX,EAX
              @@OK:
end;

FUNCTION DevGetKrnlInfo(RegAX,RegCX:UShort):Pointer;
         assembler; {&USES ebx}
asm
              MOV     AX,RegAX
              XOR     EBX,EBX
              MOV     CX,RegCX
              MOV     DX,CDH_GetDosVar
              CALL    DevHlp32
              JB      @@ERR
              CALL    GetGDTSelInf32;
              MOV     EAX,ECX
              ADD     EAX,EBX
              JMP     @@OK
              @@ERR:
              XOR     EAX,EAX
              @@OK:
end;

FUNCTION DevVerifyAccess(Mem:Pointer16;Len:SmallWord;Flags:Byte):ApiRet;
         assembler; {&USES edi}
asm
              MOV     EAX,Mem
              MOV     EDI,Mem
              SHR     EAX,$10
              MOVZX   EDI,DI
              MOVZX   ECX,Len
              MOV     DH,Flags
              MOV     DL,CDH_VerfyAccess
              CALL    DevHlp32
              JB      @@ERR
              XOR     EAX,EAX
              JMP     @@OK
              @@ERR:
              MOV     EAX,$0073
              @@OK:
end;

FUNCTION DevBeep(Freq,Dur: Longint):ApiRet;
         assembler; {&USES ebx}
asm
              MOV     EBX,Freq
              MOV     ECX,Dur
              MOV     EDX,CDH_Beep
              CALL    DevHlp32
              JB      @@ERR
              XOR     EAX,EAX
              @@ERR:
end;

FUNCTION DevGetMemPhys(Size:SmallWord;ISA:Boolean;var Pages:PhysPage):Pointer;
         assembler; {&USES ebx,esi,edi}
asm
              MOVZX   ESI,ISA
              CMP     ESI,0
              JA      @@ISA
              MOV     ESI,0
              MOV     EDI,0
              JMP     @@SIZE
              @@ISA:
              MOV     ESI,4
              MOV     EDI,1
              @@SIZE:
              MOVZX   ECX,Size
              MOV     EAX,ECX
              AND     EAX,$00000FFF
              JZ      @@GRAN
              AND     ECX,$FFFFF000
              ADD     ECX,$1000
              @@GRAN:
              MOV     EAX,$0A
              OR      EAX,EDI
              MOV     EDI,-1
              MOV     EDX,CDH_WMAlloc
              CALL    DevHlp32
              JB      @@ERR
              MOV     EBX,EAX
              MOV     EAX,$1A
              OR      EAX,ESI
              MOV     ESI,Pages
              MOV     EDI,ESI
              ADD     EDI,4
              MOV     EDX,CDH_WMLock
              CALL    DevHlp32
              JB      @@ERR
              MOV     EAX,EBX
              JMP     @@OK
              @@ERR:
              XOR     EAX,EAX
              @@OK:
end;

FUNCTION DevGetMem(Size:ULong;Flags:ULong):Pointer;
         assembler; {&USES ebx,edi}
asm
              MOV     ECX,Size
              MOV     EDI,-1
              MOV     EAX,Flags
              MOV     EDX,CDH_WMAlloc
              CALL    DevHlp32
              JB      @@ERR
              JMP     @@OK
              @@ERR:
              XOR     EAX,EAX
              @@OK:
end;

FUNCTION DevFreeMem(Mem:Pointer):ApiRet;
         assembler;
asm
              MOV     EAX,Mem
              MOV     EDX,CDH_WMFree
              CALL    DevHlp32
              JB      @@ERR
              XOR     EAX,EAX
              @@ERR:
end;

FUNCTION DevMapPhys(Phys:PointerP;Size:ULong;AllocMem:Boolean):Pointer;
         assembler; {&USES ebx,edi}
asm
              MOVZX   EBX,AllocMem
              CMP     EBX,0
              JE      @@MEM
              MOV     EAX,OFFSET Phys
              ADD     EAX,ESP
              PUSH    EAX
              CALL    LocToDat32
              MOV     ECX,Size
              MOV     EDI,EAX
              MOV     EAX,$10
              MOV     EDX,CDH_WMAlloc
              CALL    DevHlp32
              JNB     @@EXIT
              @@ERR:
              XOR     EAX,EAX
              JMP     @@EXIT
              @@MEM:
              MOV     EAX,Phys
              MOV     EBX,2
              MOV     ECX,Size
              MOV     EDX,CDH_Memory
              CALL    DevHlp32
              @@EXIT:
end;

FUNCTION DevMapGlobalToProc(Glob:Pointer;Size:ULong):Pointer;
         assembler; {&USES ebx}
asm
              MOV     EAX,1
              MOV     EBX,Glob
              MOV     ECX,Size
              MOV     EDX,CDH_WMGlob2Prc
              CALL    DevHlp32
              JNB     @@OK
              XOR     EAX,EAX
              @@OK:
end;

FUNCTION DevMapProcToGlobal(Proc:Pointer;Size:ULong):Pointer;
         assembler; {&USES ebx}
asm
              MOV     EAX,1
              MOV     EBX,Proc
              MOV     ECX,Size
              MOV     EDX,CDH_WMProc2Glo
              CALL    DevHlp32
              JNB     @@OK
              XOR     EAX,EAX
              @@OK:
end;

FUNCTION DevLinToGDTSelector(Sel:Selector;Addr:Pointer;Size:SmallWord):ApiRet;
         assembler; {&USES ebx}
asm
              MOVZX   EAX,Sel
              MOV     EBX,Addr
              MOVZX   ECX,Size
              MOV     EDX,CDH_LinToGDTSel
              CALL    DevHlp32
              JB      @@ERR
              XOR     EAX,EAX
              @@ERR:
end;

FUNCTION DevArmCtxHook(Hook:ULong;Params:ULong):ApiRet;
         assembler;  {&USES ebx,edi}
asm
              MOV     EAX,Params
              MOV     EBX,Hook
              MOV     ECX,$FFFFFFFF
              MOV     EDX,CDH_ArmCtxHook
              CALL    DevHlp32
end;

FUNCTION GetCallInAddr(Func:SmallWord):Pointer16;
         assembler;
asm
              MOV     CX,Func
              MOV     EDX,CDH_GetCallIn
              CALL    DevHlp32
end;

FUNCTION DevLinToVirt(Flat:Pointer):Pointer16;
         assembler; {&USES ebx}
asm
              MOV     EAX,Flat
              MOV     EBX,0
              MOV     EDX,CDH_Memory
              CALL    DevHlp32
end;

FUNCTION DevAllocGDTSelector(var Buffer:Selector;Count:SmallInt):ApiRet;
         assembler; {&USES ebx,edi}
asm
              MOV     EAX,Buffer
              MOV     EBX,1
              MOV     CX,Count
              MOV     EDX,CDH_Memory
              CALL    DevHlp32
end;

PROCEDURE DevPushReqPacket(Queue:PULong;Request:Pointer16);
          assembler; {&USES ebx,esi}
asm
              MOV     EAX,Queue
              MOV     EBX,Request
              MOV     ECX,00
              MOV     EDX,CDH_ReqQueue
              CALL    DevHlp32
end;

FUNCTION DevPullReqPacket(Queue:PULong):Pointer16;
          assembler; {&USES ebx,esi}
asm
              MOV     EAX,Queue
              MOV     ECX,01
              MOV     EDX,CDH_ReqQueue
              CALL    DevHlp32
end;

FUNCTION DevPullParticular(Queue:PULong;Request:Pointer16):Pointer16;
          assembler; {&USES ebx,esi}
asm
              MOV     EAX,Queue
              MOV     EBX,Request
              MOV     ECX,02
              MOV     EDX,CDH_ReqQueue
              CALL    DevHlp32
end;

PROCEDURE DevDone(Request:Pointer16);
          assembler; {&USES ebx}
asm
              MOV     EBX,Request
              MOV     EDX,CDH_DevDone
              CALL    DevHlp32
end;

FUNCTION DevSetIRQ(Shared:SmallWord):ApiRet;
         assembler;  {&USES ebx}
asm
              MOV     EAX,01
              MOV     CX,Shared
              MOV     EDX,CDH_IRQ
              CALL    DevHlp32
end;

FUNCTION DevUnsetIRQ:ApiRet;
         assembler;
asm
              MOV     EAX,00
              MOV     EDX,CDH_IRQ
              CALL    DevHlp32
end;

FUNCTION DevTickCount(Count:SmallWord):ApiRet;
         assembler;  {&USES ebx}
asm
              MOV     EAX,01
              MOVZX   EBX,Count
              MOV     EDX,CDH_SetTimer
              CALL    DevHlp32
end;

FUNCTION DevResetTimer:ApiRet;
         assembler;
asm
              MOV     EAX,00
              MOV     EDX,CDH_SetTimer
              CALL    DevHlp32
end;

FUNCTION DevAttachDD(DDName:PChar;IDCData:PIDCStruct):ApiRet;
         assembler;  {&USES ebx}
asm
              MOV     EAX,DDName
              MOV     ECX,[IDCData]
              MOV     EDX,CDH_AttachDD
              CALL    DevHlp32
end;

FUNCTION DevCallDD16(IDC:PIDCStruct;Data:Pointer16;Param:SmallWord):ApiRet;
         assembler;  {&USES ebx,esi,edi}
asm
              MOV     ESI,[IDC]
              MOV     EBX,[Data]
              MOV     ECX,[Data]
              SHR     ECX,010h
              MOV     DX,Param
              SHL     EDX,$10
              MOV     DX,CDH_CallDDD16
              CALL    DevHlp32
end;

FUNCTION DevCallDD32(IDC:PIDCStruct;Data:Pointer;Param:SmallWord):ApiRet;
         assembler;  {&USES ebx,esi,edi}
asm
              MOV     ESI,[IDC]
              MOV     EAX,Data
              MOV     DX,Param
              SHL     EDX,$10
              MOV     DX,CDH_CallDDD32
              CALL    DevHlp32
end;

FUNCTION DevSetOptions(DDName:PChar;IDCOpt:SmallWord;
                       IRQOpt:SmallWord;HookMask:ULong;
                       InfoPtr:Pointer16):ApiRet;
         assembler;  {&USES ebx,esi}
asm
              MOV     ESI,DDName
              MOV     AX,IRQOpt
              SHL     EAX,010h
              MOV     AX,IDCOpt
              MOV     EBX,InfoPtr
              MOV     ECX,HookMask
              MOV     EDX,CDH_SetOptions
              CALL    DevHlp32
end;

FUNCTION DevSemRequest(Sem:Pointer16;Timeout:ULong):ApiRet;
         assembler;  {&USES ebx,edi}
asm
              MOV     EAX,Sem
              MOV     BX,AX
              SHR     EAX,$10;
              MOV     EDI,Timeout
              MOV     CX,DI
              SHR     EDI,$10
              MOV     EDX,CDH_SemRequest
              CALL    DevHlp32
              JB      @@ERR
              XOR     EAX,EAX
              @@ERR:
end;

FUNCTION DevSemClear(Sem:Pointer16):ApiRet;
         assembler;  {&USES ebx}
asm
              MOV     EAX,Sem
              MOV     BX,AX
              SHR     EAX,$10;
              MOV     EDX,CDH_SemClear
              CALL    DevHlp32
              JB      @@ERR
              XOR     EAX,EAX
              @@ERR:
end;

PROCEDURE DevSleep(Time:ULong); EXTERNAL;

FUNCTION DevOpenEventSem(Sem:SemHandle):ApiRet;
         assembler;
asm
              MOV     EAX,Sem
              MOV     EDX,CDH_OpenEvent
              CALL    DevHlp32
              JB      @@ERR
              XOR     EAX,EAX
              @@ERR:
end;

FUNCTION DevCloseEventSem(Sem:SemHandle):ApiRet;
         assembler;
asm
              MOV     EAX,Sem
              MOV     EDX,CDH_CloseEvent
              CALL    DevHlp32
              JB      @@ERR
              XOR     EAX,EAX
              @@ERR:
end;

FUNCTION DevPostEventSem(Sem:SemHandle):ApiRet;
         assembler;
asm
              MOV     EAX,Sem
              MOV     EDX,CDH_PostEvent
              CALL    DevHlp32
              JB      @@ERR
              XOR     EAX,EAX
              @@ERR:
end;

FUNCTION DevAllocReqPacket(NoWait:Boolean):Pointer16;
         assembler;  {&USES ebx}
asm
              MOVZX   EBX,NoWait
              ADD     EBX,10
              MOV     EDX,CDH_Memory
              CALL    DevHlp32
end;

PROCEDURE DevFreeReqPacket(Packet:Pointer16);
          assembler;
asm
              MOV     EAX,Packet
              MOV     EBX,12
              MOV     EDX,CDH_Memory
              CALL    DevHlp32
end;

{******************************************}
{*                                        *}
{* DosCalls                               *}
{*                                        *}
{******************************************}

CONST
  CDH_DosDevIOCtl   = $0006FF;

FUNCTION DevDevIOCtl(DrvName:PChar; Category,Func: ULong;
                     Params: Pointer;ParamLen: ULong;
                     Data: Pointer;DataLen: ULong): ApiRet;
         assembler; {&USES ebx,esi,edi}
asm
              MOV     EAX,Data
              MOV     EBX,Params
              MOV     CX,WORD PTR ParamLen
              SHL     ECX,$10
              MOV     CX,WORD PTR DataLen
              MOV     SI,WORD PTR Category
              SHL     ESI,$10
              MOV     SI,WORD PTR Func
              MOV     EDI,DrvName
              MOV     DX,CDH_DosDevIOCtl
              CALL    DevHlp32
end;



{******************************************}
{*                                        *}
{* C STRING STUFF                         *}
{*                                        *}
{* taken from Unit Strings VPC Runtime    *}
{* Copyright (C) 1995-2000 vpascal.com    *}
{*                                        *}
{******************************************}

procedure MemMove(Src,Dest: Pointer; Count: Longint);
          assembler; {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                mov     esi,Src
                mov     edi,Dest
                mov     ecx,Count
                test    ecx,ecx
                jz      @@RET
                cmp     esi,edi
                jae     @@Forward

                // Move backwards
                std
                add     esi,ecx
                add     edi,ecx
                mov     eax,ecx
                and     ecx,11b
                dec     esi
                dec     edi
                rep     movsb
                mov     ecx,eax
                shr     ecx,2
                jz      @@RET
                sub     esi,3
                sub     edi,3
                rep     movsd
                jmp     @@RET

                // Move forward
              @@Forward:
                cld
                // Make sure data is well aligned
              @@Align:
                test    edi,3
                jz      @@Aligned
                movsb
                dec     ecx
                jz      @@RET
                jmp     @@Align

              @@Aligned:
                mov     eax,ecx
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     cl,al
                rep     movsb
              @@RET:
                cld
end;

// FillChar standard procedure
// procedure FillChar(var Dest; Count: LongInt, Value);

procedure MemFill(Dest: Pointer; Count: Longint; Value: Byte);
          assembler; {&USES eax,ecx,edi} {&FRAME-}
asm
                cld
                mov     al,Value                // Fill all bytes of the eax
                mov     ah,al                   // with Value byte
                mov     ecx,eax
                shl     eax,16
                mov     ax,cx
                mov     edi,Dest
                mov     ecx,Count
                push    ecx
                shr     ecx,2
                rep     stosd
                pop     ecx
                and     ecx,11b
                rep     stosb
end;




{ Returns the number of characters in Str, not counting the null        }
{ terminator.                                                           }

function StrLen(Str: PChar): Word; assembler; {$USES ebx} {$FRAME-}
asm

                mov     ebx,Str
                lea     eax,[ebx-1]
              @@Begin:
                inc     eax
                test    eax,3
                jnz     @@Odd
              @@More:
                mov     ecx,[eax]
                mov     edx,ecx          // Check if any of the next 4 bytes contain a 0
                add     eax,4            // This costs just 3 clocks for 4 bytes
                not     ecx
                sub     edx,$01010101
                and     ecx,$80808080
                and     ecx,edx
                jz      @@More           // Zero flag: No 0 bytes in the dword
                sub     eax,4
               @@Odd:
                cmp     [eax].Byte,0
                jne     @@Begin
                sub     eax,ebx
end;

{ Returns a pointer to the null character that terminates Str.          }

function StrEnd(Str: PChar): PChar; assembler; {&Uses ebx} {&Frame-}
asm
                mov     ebx,Str
                lea     eax,[ebx-1]      // eax = ptr to string-1
              @@Begin:
                inc     eax
                test    eax,3            // Check that eax is dword-aligned
                jnz     @@Odd
              @@More:
                mov     ecx,[eax]        // Get the next 4 bytes to check
                mov     edx,ecx          // Check if any of them are 0
                add     eax,4            // .. in just 3 clocks for 4 bytes
                not     ecx
                sub     edx,$01010101
                and     ecx,$80808080
                and     ecx,edx
                jz      @@More           // Zero flag: No 0 bytes in the dword
                sub     eax,4            // nz: a 0 was found
               @@Odd:
                cmp     [eax].Byte,0     // Search individual characters
                jne     @@Begin
end;

{ Copies exactly Count characters from Source to Dest and returns Dest. }
{ Source and Dest may overlap.                                          }

function StrMove(Dest, Source: PChar; Count: Word): PChar; assembler; {$USES esi,edi} {$FRAME-}
asm
                mov     esi,Source
                mov     edi,Dest
                mov     edx,edi
                mov     ecx,Count
                cmp     esi,edi
                jae     @@1
                std
                add     esi,ecx
                add     edi,ecx
                mov     eax,ecx
                and     ecx,11b
                shr     eax,2
                dec     esi
                dec     edi
                rep     movsb
                mov     ecx,eax
                sub     esi,3
                sub     edi,3
                rep     movsd
                cld
                jmp     @@2
              @@1:
                cld
                mov     eax,ecx
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     cl,al
                rep     movsb
              @@2:
                mov     eax,edx
end;

{ Copies Source to Dest and returns Dest.                               }

function StrCopy(Dest, Source: PChar): PChar; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Source
                mov     esi,edi
                xor     al,al
                or      ecx,-1
                repne   scasb
                not     ecx
                mov     dl,cl
                mov     edi,Dest
                mov     eax,edi
                shr     ecx,2
                and     dl,11b
                rep     movsd
                mov     cl,dl
                rep     movsb
end;

{ Copies Source to Dest and returns StrEnd(Dest).                       }

function StrECopy(Dest, Source: PChar): PChar; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Source
                mov     esi,edi
                xor     al,al
                or      ecx,-1
                repne   scasb
                not     ecx
                mov     al,cl
                mov     edi,Dest
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     cl,al
                rep     movsb
                lea     eax,[edi-1]
end;

{ Copies at most MaxLen characters from Source to Dest and returns Dest.}

function StrLCopy(Dest, Source: PChar; MaxLen: Word): PChar; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Source
                mov     esi,edi
                mov     ecx,MaxLen
                mov     eax,Dest
//                mov     Byte Ptr [eax],0
                jecxz   @@RET
                mov     edx,ecx
                xor     al,al
                repne   scasb
                sub     edx,ecx
                mov     ecx,edx
                mov     edi,Dest
                mov     eax,edi
                shr     ecx,2
                and     dl,11b
                rep     movsd
                mov     cl,dl
                rep     movsb
                mov     [edi].Byte,0
              @@RET:
end;

{ Appends a copy of Source to the end of Dest and returns Dest.         }

function StrCat(Dest, Source: PChar): PChar; assembler; {$USES None} {$FRAME+}
asm
                push    Dest
                Call    StrEnd
                push    eax
                push    Source
                Call    StrCopy
                mov     eax,Dest
end;

{ Appends at most MaxLen - StrLen(Dest) characters from Source to the   }
{ end of Dest, and returns Dest.                                        }

function StrLCat(Dest, Source: PChar; MaxLen: Word): PChar; assembler; {$USES None} {$FRAME+}
asm
                push    Dest
                Call    StrEnd
                mov     ecx,Dest
                add     ecx,MaxLen
                sub     ecx,eax
                jbe     @@1
                push    eax
                push    Source
                push    ecx
                Call    StrLCopy
              @@1:
                mov     eax,Dest
end;

{ Compares Str1 to Str2. The return value is less than 0 if Str1 < Str2,}
{ 0 if Str1 = Str2, or greater than 0 if Str1 > Str2.                   }

function StrComp(Str1, Str2: PChar): Integer; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Str2
                mov     esi,edi
                or      ecx,-1
                xor     eax,eax
                xor     edx,edx
                repne   scasb
                not     ecx
                mov     edi,esi
                mov     esi,Str1
                repe    cmpsb
                mov     al,[esi-1]
                mov     dl,[edi-1]
                sub     eax,edx
end;

{ Compares Str1 to Str2, without case sensitivity. The return value is  }
{ the same as StrComp.                                                  }

function StrIComp(Str1, Str2: PChar): Integer; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Str2
                mov     esi,edi
                or      ecx,-1
                xor     eax,eax
                xor     edx,edx
                repne   scasb
                not     ecx
                mov     edi,esi
                mov     esi,Str1
              @@1:
                repe    cmpsb
                je      @@4
                mov     al,[esi-1]
                cmp     al,'a'
                jb      @@2
                cmp     al,'z'
                ja      @@2
                sub     al,'a'-'A'
              @@2:
                mov     dl,[edi-1]
                cmp     dl,'a'
                jb      @@3
                cmp     dl,'z'
                ja      @@3
                sub     dl,'a'-'A'
              @@3:
                sub     eax,edx
                je      @@1
              @@4:
end;

{ Compares Str1 to Str2, for a maximum length of MaxLen characters. The }
{ return value is the same as StrComp.                                  }

function StrLComp(Str1, Str2: PChar; MaxLen: Word): Integer; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Str2
                mov     esi,edi
                mov     eax,MaxLen
                mov     ecx,eax
                jecxz   @@1
                mov     edx,eax
                xor     eax,eax
                repne   scasb
                sub     edx,ecx
                mov     ecx,edx
                mov     edi,esi
                mov     esi,Str1
                repe    cmpsb
                xor     edx,edx
                mov     al,[esi-1]
                mov     dl,[edi-1]
                sub     eax,edx
              @@1:
end;

{ Compares Str1 to Str2, for a maximum length of MaxLen characters,     }
{ without case sensitivity. The return value is the same as StrComp.    }

function StrLIComp(Str1, Str2: PChar; MaxLen: Word): Integer; assembler; {$USES esi,edi} {$FRAME-}
asm
                mov     edi,Str2
                mov     esi,edi
                mov     eax,MaxLen
                mov     ecx,eax
                jecxz   @@4
                cld
                mov     edx,eax
                xor     eax,eax
                repne   scasb
                sub     edx,ecx
                mov     ecx,edx
                mov     edi,esi
                mov     esi,Str1
                xor     edx,edx
              @@1:
                repe    cmpsb
                je      @@4
                mov     al,[esi-1]
                cmp     al,'a'
                jb      @@2
                cmp     al,'z'
                ja      @@2
                sub     al,'a'-'A'
              @@2:
                mov     dl,[edi-1]
                cmp     dl,'a'
                jb      @@3
                cmp     dl,'z'
                ja      @@3
                sub     dl,'a'-'A'
              @@3:
                sub     eax,edx
                je      @@1
              @@4:
end;

{ Returns a pointer to the first occurrence of Chr in Str. If Chr does  }
{ not occur in Str, StrScan returns NIL. The null terminator is         }
{ considered to be part of the string.                                  }

function StrScan(Str: PChar; Chr: Char): PChar; assembler; {&Uses None} {&Frame-}
asm
                mov     eax,Str
                movzx   edx,Chr
              @@Loop:
                cmp     dl,[eax]
                je      @@Ok
                cmp     dh,[eax]
                je      @@Err
                inc     eax
                jmp     @@Loop
              @@Err:
                xor     eax,eax
              @@Ok:
end;

{ Returns a pointer to the last occurrence of Chr in Str. If Chr does   }
{ not occur in Str, StrRScan returns NIL. The null terminator is        }
{ considered to be part of the string.                                  }

function StrRScan(Str: PChar; Chr: Char): PChar; assembler; {&Uses None} {&Frame-}
asm
                mov     eax,Str
                push    eax
                push    eax
                call    StrEnd
                pop     ecx               // The start of the string
                movzx   edx,Chr
             @@Loop:
                cmp     dl,[eax]
                je      @@Ok
                dec     eax
                cmp     eax,ecx
                jl      @@Nil
                jmp     @@Loop
             @@Nil:
                xor     eax,eax
             @@Ok:
end;

{ Returns a pointer to the first occurrence of Str2 in Str1. If Str2    }
{ does not occur in Str1, StrPos returns NIL.                           }

function StrPos(Str1, Str2: PChar): PChar; assembler; {$USES ebx,esi,edi} {$FRAME-}
asm
                cld
                xor     al,al
                mov     edi,Str2
                or      ecx,-1
                repne   scasb
                not     ecx
                dec     ecx
                je      @@2
                mov     edx,ecx
                mov     edi,Str1
                mov     ebx,edi
                or      ecx,-1
                repne   scasb
                not     ecx
                sub     ecx,edx
                jbe     @@2
                mov     edi,ebx
              @@1:
                mov     esi,Str2
                lodsb
                repne   scasb
                jne     @@2
                mov     eax,ecx
                mov     ebx,edi
                mov     ecx,edx
                dec     ecx
                repe    cmpsb
                mov     ecx,eax
                mov     edi,ebx
                jne     @@1
                lea     eax,[edi-1]
                jmp     @@3
              @@2:
                xor     eax,eax
              @@3:
end;

{ Converts Str to upper case and returns Str.                           }

function StrUpper(Str: PChar): PChar; assembler; {$USES esi} {$FRAME-}
asm
                cld
                mov     esi,Str
                mov     eax,esi
              @@1:
                mov     dl,[esi]
                test    dl,dl
                jz      @@2
                inc     esi
                cmp     dl,'a'
                jb      @@1
                cmp     dl,'z'
                ja      @@1
                sub     dl,'a'-'A'
                mov     [esi-1],dl
                jmp     @@1
              @@2:
end;

{ Converts Str to lower case and returns Str.                           }

function StrLower(Str: PChar): PChar; assembler; {$USES esi} {$FRAME-}
asm
                cld
                mov     esi,Str
                mov     eax,esi
              @@1:
                mov     dl,[esi]
                test    dl,dl
                jz      @@2
                inc     esi
                cmp     dl,'A'
                jb      @@1
                cmp     dl,'Z'
                ja      @@1
                add     dl,'a'-'A'
                mov     [esi-1],dl
                jmp     @@1
              @@2:
end;


{******************************************}
{*                                        *}
{* INFO I/O Routines and Param Parser     *}
{*                                        *}
{*                                        *}
{******************************************}

CONST
  InfASCII:ARRAY[0..15] OF Char = ('0','1','2','3','4','5','6','7','8'
                                   ,'9','A','B','C','D','E','F');

PROCEDURE BytesToHex(ByteStr1,Str2:PChar;ByteLen:Byte);
VAR
  Pos   :Byte;
  Dat   :Byte;
BEGIN
  FOR Pos:=0 TO ByteLen-1 DO
  BEGIN
    Dat:=Byte(ByteStr1^);
    Str2^:=InfASCII[(Dat DIV 16) AND $0F];
    Inc(Str2);
    Str2^:=InfASCII[(Dat MOD 16) AND $0F];
    Inc(Str2);
    Inc(ByteStr1);
  END;
  Str2^:=CHR(0);
END;

PROCEDURE HexToBytes(CharStr1,Str2:PChar;ByteLen:Byte);
VAR
  Pos   :Byte;
  Dat   :Byte;
BEGIN
  StrUpper(CharStr1);
  FOR Pos:=0 TO ByteLen-1 DO
  BEGIN
    Str2^:=CHR(0);
    Dat:=Ord(CharStr1^);
    IF (Dat>=65) AND (Dat<=70) THEN Str2^:=CHR((Dat-55) SHL 4);
    IF (Dat>=48) AND (Dat<=57) THEN Str2^:=CHR((Dat-48) SHL 4);
    Inc(CharStr1);
    Dat:=Ord(CharStr1^);
    IF (Dat>=65) AND (Dat<=70) THEN Str2^:=CHR((Dat-55) + Ord(Str2^));
    IF (Dat>=48) AND (Dat<=57) THEN Str2^:=CHR((Dat-48) + Ord(Str2^));
    Inc(CharStr1);
    Inc(Str2);
  END;
  Str2^:=CHR(0);
END;

VAR
  InfBuff       :PChar;
  InfBuffSize   :ULong;
  InfLog        :Boolean;

PROCEDURE InfInit(Buffer:PChar;Size:ULong);
BEGIN
  InfBuff:=Buffer;
  InfBuffSize:=Size;
  InfLog:=True;
END;

PROCEDURE InfSet(Log:Boolean);
BEGIN
  InfLog:=Log;
END;

PROCEDURE InfAdd(Str:PChar;Val:Pointer);
VAR
  NumberPtr:PChar;
  Number   :Cardinal;
BEGIN
  IF (InfBuff<>NIL) AND (InfBuffSize>0) AND InfLog THEN
  BEGIN
    StrLCat(InfBuff,Str,InfBuffSize);
    IF Val<>NIL THEN
    BEGIN
      NumberPtr:=StrRScan(InfBuff,'%');
      IF NumberPtr<>NIL THEN
      BEGIN
        Number:=Cardinal(Val^);
        REPEAT
          NumberPtr^:=InfASCII[(Number MOD 10) AND $0F];
          Number:=Number DIV 10;
          Dec(NumberPtr);
        UNTIL NumberPtr^<>'%';
      END ELSE
      BEGIN
        NumberPtr:=StrRScan(InfBuff,'$');
        IF NumberPtr<>NIL THEN
        BEGIN
          Number:=Cardinal(Val^);
          REPEAT
            NumberPtr^:=InfASCII[(Number MOD 16) AND $0F];
            Number:=Number DIV 16;
            Dec(NumberPtr);
          UNTIL NumberPtr^<>'$';
        END;
      END;
    END;
  END;
END;

PROCEDURE InfCopy(pBuffer:Pointer;var Len:SmallWord);
VAR
  MaxCopy:SmallWord;
BEGIN
  IF (InfBuff<>NIL) AND (InfBuffSize>0) AND InfLog THEN
  BEGIN
    MaxCopy:=StrLen(InfBuff);
    IF MaxCopy > Len THEN MaxCopy:=Len ELSE Len:=MaxCopy;
    StrLCopy(pBuffer,InfBuff,MaxCopy);
    InfBuff[0]:=Chr(0);
  END ELSE Len:=0;
END;

FUNCTION GetParam(Buffer:PChar;ParamStr:PChar;Val:PULong):Boolean;
VAR
  ParamPtr  :PChar;
  NumberPtr :PChar;
  CharPtr   :PChar;
  Number    :ULong;
  NChar     :Char;
BEGIN
  CharPtr:=StrScan(ParamStr,'%');
  IF CharPtr=NIL THEN CharPtr:=StrScan(ParamStr,'$');
  IF CharPtr<>NIL THEN
  BEGIN
    NChar:=CharPtr^;
    CharPtr^:=CHR(0);
  END;
  ParamPtr:=StrPos(Buffer,ParamStr);
  IF CharPtr<>NIL THEN CharPtr^:=NChar;
  IF ParamPtr<>NIL THEN
  BEGIN
    GetParam:=TRUE;
    IF Val<>NIL THEN
    BEGIN
      NumberPtr:=StrScan(ParamStr,'%');
      IF NumberPtr<>NIL THEN
      BEGIN
        Number:=0;
        CharPtr:=ParamPtr+(NumberPtr-ParamStr);
        REPEAT
          NChar:=CharPtr^;
          IF (Ord(NChar) >= 48) AND (Ord(NChar)<=57) THEN
                          Number:=Number*10+Ord(NChar)-48;
          Inc(NumberPtr);
          Inc(CharPtr);
        UNTIL (NumberPtr^<>'%') OR (CharPtr^=' ') OR (CharPtr^<CHR(48));
        Val^:=Number;
      END ELSE
      BEGIN
        NumberPtr:=StrScan(ParamStr,'$');
        IF NumberPtr<>NIL THEN
        BEGIN
          Number:=0;
          CharPtr:=ParamPtr+(NumberPtr-ParamStr);
          REPEAT
            NChar:=CharPtr^;
            IF (Ord(NChar) >= 48) AND (Ord(NChar)<=57) THEN
                            Number:=Number*16+Ord(NChar)-48;
            IF (Ord(NChar) >= 65) AND (Ord(NChar)<=70) THEN
                            Number:=Number*16+Ord(NChar)-55;
            Inc(NumberPtr);
            Inc(CharPtr);
          UNTIL (NumberPtr^<>'$') OR (CharPtr^=' ') OR (CharPtr^<CHR(48));
          Val^:=Number;
        END;
      END;
    END;
  END ELSE
    GetParam:=FALSE;
END;
END.
