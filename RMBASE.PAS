{$B-,D+,H-,I-,J+,P-,Q-,R-,S-,T-,V+,W-,X+,Z-}
{&AlignCode-,AlignData-,AlignRec-,Asm-,Cdecl-,Comments-,Delphi-,Frame+,G5+}
{&LocInfo+,Open32-,Optimise+,OrgName-,SmartLink+,Speed+,Use32+,ZD-}
{$M 32768}

UNIT RMBASE;

INTERFACE
USES DRVBASE;

CONST {Driver Type and Subtypes}
  DRT_UNDEFINED = 0;
    DRS_UNDEFINED = 0;
  DRT_OS2       = 3;
    DRS_CHAR      = 1;
    DRS_APP_HELPER= 3;
  DRT_NETWORK   = 4;
  DRT_VIDEO     = 5;
  DRT_AUDIO     = 6;
  DRT_SERVICE   = 7;

FUNCTION RMInit(DrvName,DrvDesc,Vendor:PChar;
                DayP,MonthP,YearP,DType,SType:ULong):ApiRet;
FUNCTION RMCancel:ApiRet;

CONST {Adapter Type}
  AS_BASE_RESERVED = 0;
    AS_SUB_OTHER = $80;   {any BaseType}
  AS_BASE_NETWORK = $02;
    AS_SUB_ETHERNET = $01;
  AS_BASE_MMEDIA = $04;
    AS_SUB_MM_VIDEO = $01;
    AS_SUB_MM_AUDIO = $02;
  AS_BASE_BRIDGE = $06;
  AS_BASE_INPUT = $09;

CONST {HostBus}
  AS_HOSTBUS_OTHER  = $00;
  AS_HOSTBUS_ISA    = $01;
  AS_HOSTBUS_PCI    = $04;
  AS_HOSTBUS_PLANAR = $08;

  AS_BUSWIDTH_8BIT  = $10;
  AS_BUSWIDTH_16BIT = $20;
  AS_BUSWIDTH_32BIT = $30;
  AS_BUSWIDTH_64BIT = $40;

FUNCTION RMCreateAdapter(AdName:PChar;ISA:Boolean;AType,SType,
                         HType,HWidth:ULong):ApiRet;
FUNCTION RMGetLastAdapterHandle:ULong;
FUNCTION RMDestroyAdapter(Handle:ULong):ApiRet;

CONST {IO and other Resources}
  RS_EXCLUSIVE   = 1;
  RS_MULTIPLEXED = 2;
  RS_SHARED      = 4;

  RS_PCI_INT_NONE= 0;
  RS_PCI_INT_A   = 1;
  RS_PCI_INT_B   = 2;
  RS_PCI_INT_C   = 3;
  RS_PCI_INT_D   = 4;

FUNCTION RMAddIO(BasePort,Count,Access:ULong;
                 ISA10:Boolean):ApiRet;
FUNCTION RMAddIRQ(IRQ,PCI,Access:ULong):ApiRet;
FUNCTION RMAddMEM(Base,Size,Access:ULong):ApiRet;

IMPLEMENTATION
CONST
  PAR_ENDS  = $FFFFFFFF;

{ FUNCTION CODES OF RESMGR$:
  get version   1
  create driver 2
  destroy drver 3
  create adapter4
  destroy adapter5
  create device 6
  destroy device7
  alloc resource8
  dealloc res   9
  create ldev   C
  destroy ldev  D
  modify res   1B

  FIND VIA SEQ: 8D 46 6 16 50 6A X 0E E8 after function name in RMCALLS.LIB}



TYPE
  RMHANDLE=ULong;
  IDCPARAM=RECORD
  CASE Boolean OF
  [TRUE]: ( {normal operation}
    Par1    :Pointer16;
    Par2    :Pointer16;
    Par3    :Pointer16;
    Par4    :Pointer16;
    Par5    :Pointer16;);
  [FALSE]:( {Modify Resources}
    Drv     :RMHANDLE;
    Adp     :RMHANDLE;
    Act     :UShort;
    Res     :RMHANDLE;
    Fill    :UShort;
    Ends    :ULong;);
  END;
  DRIVER=RECORD
    DrvrName    :Pointer16;
    DrvrDescript:Pointer16;
    VendorName  :Pointer16;
    MajorVer    :Byte;
    MinorVer    :Byte;
    Year        :UShort;
    Month       :Byte;
    Day         :Byte;
    DrvrFlags   :UShort;
    DrvrType    :UShort;
    DrvrSubType :UShort;
    DrvrCallback:Pointer16;
  END;
  ADAPTER=RECORD
    AdaptName   :Pointer16;
    AdaptFlags  :UShort;
    BaseType    :UShort;
    SubType     :UShort;
    InterfaceType:UShort;
    HostBusType :UShort;
    HostBusWidth:UShort;
    pAdjunct    :Pointer16;
    Reserved    :ULong;
  END;
  RESOURCE=RECORD
    RType       :ULong;
    CASE ULong OF
    [1]:( {IO Resource}
      BaseIOPort  :UShort;
      NumIOPorts  :UShort;
      IOFlags     :UShort;
      IOAddressLines:UShort;);
    [2]:( {IRQ Resource}
      IRQLevel    :UShort;
      PCIIrqPin   :UShort;
      IRQFlags    :UShort;
      IRQReserved :UShort;);
    [3]:( {MEM Resource}
      MEMBase     :ULong;
      MEMSize     :ULong;
      MEMFlags    :UShort;
      MEMReserved :UShort;);
  END;

VAR
  RMIDC     :IDCStruct;
  DrvHandle :RMHANDLE;
  AdpHandle :RMHANDLE;

FUNCTION RMInit(DrvName,DrvDesc,Vendor:PChar;
                DayP,MonthP,YearP,DType,SType:ULong):ApiRet;
VAR
  DrvStrc   :DRIVER;
  IdcParm   :IDCPARAM;
BEGIN
  {Load all Struct Fields}
  WITH DrvStrc DO
  BEGIN
    DrvrName     :=DevLinToVirt(DrvName);
    DrvrDescript :=DevLinToVirt(DrvDesc);
    VendorName   :=DevLinToVirt(Vendor);
    MajorVer     :=1;
    MinorVer     :=1;
    Year         :=YearP;
    Month        :=MonthP;
    Day          :=DayP;
    DrvrFlags    :=0;     {STATIC ONLY since Driver does not unload}
    DrvrType     :=DType;
    DrvrSubType  :=SType;
    DrvrCallback.Ptr :=0;
  END;
  WITH IDCParm DO
  BEGIN
    Par1:=LocToDat16(Addr(DrvStrc));
    Par2:=DevLinToVirt(Addr(DrvHandle));
    Par3.Ptr:=PAR_ENDS;
  END;
  IF (IDCParm.Par1.Ptr<>0) AND (DevAttachDD('RESMGR$ ',@RMIDC)=0) THEN
      RMInit:=DevCallDD16(@RMIDC,LocToDat16(Addr(IdcParm)),$F002)
  ELSE
      RMInit:=$0073; {Invalid Address}
END;

FUNCTION RMCancel:ApiRet;
VAR
  IdcParm   :IDCPARAM;
BEGIN
  IF (RMIDC.ResS<>0) AND (DrvHandle<>0) THEN
  BEGIN
    WITH IdcParm DO
    BEGIN
      Par1.Ptr:=DrvHandle;
      Par2.Ptr:=PAR_ENDS;
    END;
    RMCancel:=DevCallDD16(@RMIDC,LocToDat16(Addr(IdcParm)),$F003);
  END
  ELSE
    RMCancel:=$0073; {Invalid Address}
END;

FUNCTION RMCreateAdapter(AdName:PChar;ISA:Boolean;AType,SType,
                         HType,HWidth:ULong):ApiRet;
VAR
  AdpStrc   :ADAPTER;
  IdcParm   :IDCPARAM;
BEGIN
  {Load all Struct Fields}
  WITH AdpStrc DO
  BEGIN
    AdaptName    :=DevLinToVirt(AdName);
    BaseType     :=AType;
    SubType      :=SType;
    InterfaceType:=1;
    HostBusType  :=HType;
    HostBusWidth :=HWidth;
    pAdjunct.Ptr :=0;
    Reserved:=0;
    IF ISA THEN AdaptFlags:=1 ELSE AdaptFlags:=0;
  END;
  WITH IDCParm DO
  BEGIN
    Par1.Ptr:=DrvHandle;
    Par2:=DevLinToVirt(Addr(AdpHandle));
    Par3:=LocToDat16(Addr(AdpStrc));
    Par4.Ptr:=0;
    Par5.Ptr:=0;
  END;
  IF (IDCParm.Par1.Ptr<>0) AND (RMIDC.ResS<>0) AND (DrvHandle<>0) THEN
      RMCreateAdapter:=DevCallDD16(@RMIDC,LocToDat16(Addr(IdcParm)),$F004)
  ELSE
    RMCreateAdapter:=$0073; {Invalid Address}
END;

FUNCTION RMGetLastAdapterHandle:ULong;
BEGIN
  RMGetLastAdapterHandle:=AdpHandle;
END;

FUNCTION RMDestroyAdapter(Handle:ULong):ApiRet;
VAR
  IdcParm   :IDCPARAM;
BEGIN
  IF (RMIDC.ResS<>0) AND (DrvHandle<>0) THEN
  BEGIN
    WITH IdcParm DO
    BEGIN
      Par1.Ptr:=DrvHandle;
      Par2.Ptr:=Handle;
      Par3.Ptr:=PAR_ENDS;
    END;
    RMDestroyAdapter:=DevCallDD16(@RMIDC,LocToDat16(Addr(IdcParm)),$F005);
  END
  ELSE
    RMDestroyAdapter:=$0073; {Invalid Address}
END;

FUNCTION RMAddRC(Handle:RMHANDLE):ApiRet;
VAR
  IdcParm  :IDCPARAM;
BEGIN
  WITH IDCParm DO
  BEGIN
    Drv:=DrvHandle;
    Adp:=AdpHandle;
    Act:=0; {modify add}
    Res:=Handle;
    Fill:=0;
    Ends:=PAR_ENDS;
  END;
  IF (IDCParm.Par1.Ptr<>0) AND (RMIDC.ResS<>0) AND (DrvHandle<>0) THEN
      RMAddRC:=DevCallDD16(@RMIDC,LocToDat16(Addr(IdcParm)),$F01B)
  ELSE
    RMAddRC:=$0073; {Invalid Address}
END;


FUNCTION RMAddIO(BasePort,Count,Access:ULong;
                 ISA10:Boolean):ApiRet;
VAR
  rc       :ApiRet;
  IOStrc   :RESOURCE;
  IdcParm  :IDCPARAM;
  ResHandle:RMHANDLE;
BEGIN
  {Load all Struct Fields}
  WITH IOStrc DO
  BEGIN
    RType:=1;
    BaseIOPort   :=BasePort;
    NumIOPorts   :=Count;
    IOFlags      :=Access;
    IF ISA10 THEN IOAddressLines:=10 ELSE IOAddressLines:=16;
  END;
  WITH IDCParm DO
  BEGIN
    Par1.Ptr:=DrvHandle;
    Par2:=LocToDat16(Addr(ResHandle));
    Par3:=LocToDat16(Addr(IOStrc));
    Par4.Ptr:=PAR_ENDS;
  END;
  IF (IDCParm.Par1.Ptr<>0) AND (RMIDC.ResS<>0) AND (DrvHandle<>0) THEN
    BEGIN
      rc:=DevCallDD16(@RMIDC,LocToDat16(Addr(IdcParm)),$F008);
      RMAddIO:=rc;
      IF rc=0 THEN RMAddIO:=RMAddRC(ResHandle);
    END
  ELSE
    RMAddIO:=$0073; {Invalid Address}
END;

FUNCTION RMAddIRQ(IRQ,PCI,Access:ULong):ApiRet;
VAR
  rc       :ApiRet;
  IOStrc   :RESOURCE;
  IdcParm  :IDCPARAM;
  ResHandle:RMHANDLE;
BEGIN
  {Load all Struct Fields}
  WITH IOStrc DO
  BEGIN
    RType:=2;
    IRQLevel    :=IRQ;
    PCIIrqPin   :=PCI;
    IRQFlags    :=Access;
    IRQReserved:=0;
  END;
  WITH IDCParm DO
  BEGIN
    Par1.Ptr:=DrvHandle;
    Par2:=LocToDat16(Addr(ResHandle));
    Par3:=LocToDat16(Addr(IOStrc));
    Par4.Ptr:=PAR_ENDS;
  END;
  IF (IDCParm.Par1.Ptr<>0) AND (RMIDC.ResS<>0) AND (DrvHandle<>0) THEN
    BEGIN
      rc:=DevCallDD16(@RMIDC,LocToDat16(Addr(IdcParm)),$F008);
      RMAddIRQ:=rc;
      IF rc=0 THEN RMAddIRQ:=RMAddRC(ResHandle);
    END
  ELSE
    RMAddIRQ:=$0073; {Invalid Address}
END;

FUNCTION RMAddMEM(Base,Size,Access:ULong):ApiRet;
VAR
  rc       :ApiRet;
  IOStrc   :RESOURCE;
  IdcParm  :IDCPARAM;
  ResHandle:RMHANDLE;
BEGIN
  {Load all Struct Fields}
  WITH IOStrc DO
  BEGIN
    RType:=3;
    MEMBase     :=Base;
    MEMSize     :=Size;
    MEMFlags    :=Access;
    MEMReserved:=0;
  END;
  WITH IDCParm DO
  BEGIN
    Par1.Ptr:=DrvHandle;
    Par2:=LocToDat16(Addr(ResHandle));
    Par3:=LocToDat16(Addr(IOStrc));
    Par4.Ptr:=PAR_ENDS;
  END;
  IF (IDCParm.Par1.Ptr<>0) AND (RMIDC.ResS<>0) AND (DrvHandle<>0) THEN
    BEGIN
      rc:=DevCallDD16(@RMIDC,LocToDat16(Addr(IdcParm)),$F008);
      RMAddMEM:=rc;
      IF rc=0 THEN RMAddMEM:=RMAddRC(ResHandle);
    END
  ELSE
    RMAddMEM:=$0073; {Invalid Address}
END;

END.
