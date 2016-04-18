{$B-,D+,H-,I-,J+,P-,Q-,R-,S-,T-,V+,W-,X+,Z-}
{&AlignCode+,AlignData-,AlignRec-,Asm-,Cdecl-,Comments-,Delphi-,Frame+,G5+}
{&LocInfo+,Open32-,Optimise+,OrgName-,SmartLink+,Speed+,Use32+,ZD-}
{$M 32768}

UNIT USBBASE;

INTERFACE
USES DRVBASE;

TYPE
  PUSBDEVDESCR=^USBDEVDESCR;
  USBDEVDESCR=RECORD
    Length      : Byte;
    DescType    : Byte;
    bcdUSB      : UShort;
    DevClass    : Byte;
    DevSubClass : Byte;
    DevProtocol : Byte;
    MaxPacketSize: Byte;
    IDVendor    : UShort;
    IDProduct   : UShort;
    bcdDevice   : UShort;
    Manufacturer: Byte;
    ProductS    : Byte;
    SerialNr    : Byte;
    NumConfig   : Byte;
  END;
  PUSBCONFIGDESCR=^USBCONFIGDESCR;
  USBCONFIGDESCR=RECORD
    Length      : Byte;
    DescType    : Byte;
    TotalLength : UShort;
    NumInterfaces: Byte;
    ConfigValue : Byte;
    Configuration: Byte;
    Attributes  : Byte;
    MaxPower    : Byte;
  END;
  PUSBINTERDESCR=^USBINTERDESCR;
  USBINTERDESCR=RECORD
    Length      : Byte;
    DescType    : Byte;
    InterfNumber: Byte;
    AlternSett  : Byte;
    NumEndPoints: Byte;
    IntClass    : Byte;
    IntSubClass : Byte;
    IntProtocol : Byte;
    Int         : Byte;
  END;
  PUSBENDPDESCR=^USBENDPDESCR;
  USBENDPDESCR=RECORD
    Length      : Byte;
    DescType    : Byte;
    EndpAddress : Byte;
    Attributes  : Byte;
    MaxPktSize  : UShort;
    Interval    : Byte;
  END;
  USBADDR=RECORD
    CASE Boolean OF
    FALSE:(
      CtrlID    :Byte;
      DevAddress:Byte;);
    TRUE: (
      Addr      :UShort;);
  END;
  PUSBDEVINFO=^USBDEVINFO;
  USBDEVINFO=RECORD
    DevAddr     : USBADDR;
    ConfigVal   : Byte;
    InterfaceNr : Byte;
    LowSpeed    : Byte;
    PortNum     : Byte;
    HubIndex    : UShort;
    RMDevHandle : ULong;
    SetupPacket : ULong;
    Reserved    : ULong;
    Descriptor  : USBDEVDESCR;
    ConfigData  : Byte;   {Label, after that comes additional Data}
  END;
  PUSBDEV=^USBDEV;
  USBDEV=RECORD
    Active      : Boolean;
    InitComplete: Boolean;
    CtxFlags    : ULong;
    DevAddr     : USBADDR;
    Data        : PUSBDEVINFO;
    DeviceDescr : PUSBDEVDESCR;
    ConfigDescr : PUSBCONFIGDESCR;
    Inter0Descr : PUSBINTERDESCR;
    Endp_Descr  : ARRAY[1..15] OF PUSBENDPDESCR;
    LastEndp    : ULong;
    PipesMem    : Pointer;
    PipesMemSize: ULong;
    PipesGDT    : Selector;
    Link        : Pointer;
    ResetPending: Boolean;    {to be used in application}
    DeviceInitOK: Boolean;    {to be used in application}
  END;
  PUSBIRQINF=^USBIRQINF;
  USBIRQINF=RECORD
    pUsbDev     : PUSBDEV;
    DevAddr     : USBADDR;
    EndpointID  : Byte;
  END;
  PUSBIRQFNC=^USBIRQFNC;
  USBIRQFNC=FUNCTION(Info:PUSBIRQINF):Boolean;

TYPE
  CmdReq  = ApiRet;

CONST
  USBCMDNONE= $00;
  USBCMDATT = $45;
  USBCMDDET = $46;
  USBCMDIRQ = $44;

  USBDONTCARE=$FFFF;

FUNCTION USBInit:ApiRet;
FUNCTION USBLink(MyName:PChar):ApiRet;
FUNCTION USBCheckIDC(Request:PDrvRequest):CmdReq;
FUNCTION USBAttach(Request:PDrvRequest;Data:PUSBDEV;
                   Vendor,Product,Device:UShort):ApiRet;
FUNCTION USBDetachGetNr(Request:PDrvRequest):USBADDR;
PROCEDURE USBSetupPipes(Data:PUSBDEV;GDT:Selector);
FUNCTION USBIssueControlRequest(Data:PUSBDEV;ReqType,Req,ReqValue,
                                ReqIndex,Length:UShort):ApiRet;
FUNCTION USBGetDataAddr(Data:PUSBDEV;Endpoint:Byte):Pointer;
FUNCTION USBGetDataLastLen(Data:PUSBDEV;Endpoint:Byte):UShort;
FUNCTION USBGetDataStatus(Data:PUSBDEV;Endpoint:Byte):UShort;
FUNCTION USBClearStalledPipe(Data:PUSBDEV;Endpoint:Byte):ApiRet;
FUNCTION USBSetConfig(Data:PUSBDEV;Config,Interf:Byte):ApiRet;
FUNCTION USBSetInterface(Data:PUSBDEV;Interf:Byte):ApiRet;
FUNCTION USBGetString(Data:PUSBDEV;Buffer:PChar;StrIndex:Byte):ApiRet;
FUNCTION USBIssueDataRequest(Data:PUSBDEV;Endpoint:Byte;
                             Buffer:Pointer16;Length:UShort;
                             MaxWait:ULong;Func:USBIRQFNC):ApiRet;
FUNCTION USBIssueDataRequestP(Data:PUSBDEV;Endpoint:Byte;
                             Buffer:PointerP;Length:UShort;
                             MaxWait:ULong;Func:USBIRQFNC):ApiRet;
FUNCTION USBCancelRequest(Data:PUSBDEV;Endpoint:Byte):ApiRet;
FUNCTION USBHandleIRQ(Request:PDrvRequest):PUSBIRQINF;

IMPLEMENTATION
CONST
  CTRLBUFFSIZE = $64;
  CTRL_MAXWAIT = 20000;

TYPE
  SETUPPKT=RECORD
    RType       : Byte;
    Request     : Byte;
    RValue      : UShort;
    RIndex      : UShort;
    RLength     : UShort;
  END;
  PIOREQUEST=^IOREQUEST;
  IOREQUEST=RECORD
    DevAddr     : USBADDR;
    EndpointID  : Byte;
    Status      : Byte;
    Flags       : UShort;
    Buffer1     : Pointer16;
    Buffer1Len  : UShort;
    Buffer2     : Pointer16;
    Buffer2Len  : UShort;
    ServiceTime : UShort;
    MaxPktSize  : UShort;
    IRQEntry    : Pointer16;
    IRQDS       : Selector;
    IRQCategory : Byte;
    IRQData1DEV : Pointer;
    IRQData2FNC : USBIRQFNC;
    IRQData3ENDP: ULong;
    MaxErrCnt   : Byte;
    Chain       : ULong;
    AltInterface: Byte;
    ISOFlags    : Byte;
    ISOFrameLen : UShort;
    ISOBuffers  : UShort;
  END;
  PUSBPIPE=^USBPIPE;
  USBPIPE=RECORD
    Pending   :Boolean;
    LastStat  :UShort;
    LastLen   :UShort;
    Toggle    :UShort;
    USBStat   :UShort;
    DrvReq16  :Pointer16;
    DrvCan16  :Pointer16;
    IOReqBlk16:Pointer16;
    Sem16     :Pointer16;
    Sem       :Semaphore;
    DrvReq    :DrvRequest;
    DrvCan    :DrvRequest;
    IOReqBlk  :IOREQUEST;
    Wait      :ULong;
  END;
  PUSBPIPES=^USBPIPES;
  USBPIPES=RECORD
    BaseAddr  : Pointer;
    MaxIndex  : ULong;
    Setup16   : Pointer16;
    Buffer16  : Pointer16;
    Setup     : SETUPPKT;
    Buffer    : ARRAY[0..CTRLBUFFSIZE-1] OF Byte;
    Pipe      : ARRAY[0..15] OF USBPIPE;
    IRQInfo   : USBIRQINF;
  END;

VAR
  USBIDC  :IDCStruct; {Call USBD$}
  MYIDC   :IDCStruct; {Call Me}
  GenSem  :Semaphore; {General Sem for SetConfig an SetInterface}
  GenSem16:Pointer16; {Pointer to GenSem16}

FUNCTION USBInit:ApiRet;
BEGIN
  GenSem:=0;
  GenSem16:=DevLinToVirt(@GenSem);
  DevSemRequest(GenSem16,SEM_NOWAIT);
  USBInit:=DevAttachDD('USBD$   ',@USBIDC);
END;

FUNCTION USBLink(MyName:PChar):ApiRet;
VAR
  ReqPacket16 :Pointer16;
  ReqPacket   :PDrvRequest;
BEGIN
  IF (DevAttachDD(MyName,@MYIDC)=0) AND (USBIDC.PEntry.Ptr<>0) THEN
  BEGIN
    ReqPacket16:=DevAllocReqPacket(True);
    IF ReqPacket16.Ptr<>0 THEN
    BEGIN
      ReqPacket:=DevVirtToLin(ReqPacket16);
      WITH ReqPacket DO
      BEGIN
        MemFill(ReqPacket,SizeOf(DrvRequest),0);
        Length:=SizeOf(DrvRequest);
        Command:=$10;    {IOCtl}
        Category:=$91;   {USBD}
        Code:=$43;       {Link Driver}
        pIOParams:=DevLinToVirt(@MYIDC.PEntry);
        DevCallDD16(@USBIDC,ReqPacket16,DD_NOPARM);
        IF Status=STATUS_DONE THEN USBLink:=0 ELSE USBLink:=Status;
        DevFreeReqPacket(ReqPacket16);
      END;
    END ELSE USBLink:=$0073; {Invalid Address}
  END ELSE USBLink:=2; {not found}
END;

FUNCTION USBCheckIDC(Request:PDrvRequest):CmdReq;
BEGIN
  IF (Request<>NIL) AND (Request^.Length=0) THEN
  WITH Request^ DO
  BEGIN
    IF (Command=$10) AND (Category=$92) AND (Code>=$44) AND (Code<=$46)
    THEN
      USBCheckIDC:=Code
    ELSE
      USBCheckIDC:=USBCMDNONE;
  END ELSE USBCheckIDC:=USBCMDNONE;
END;

FUNCTION USBAttach(Request:PDrvRequest;Data:PUSBDEV;
                   Vendor,Product,Device:UShort):ApiRet;
VAR
  Service :PPointer16;
  NewDev  :PUSBDEVINFO;
  DevOK   :Boolean;
  NextDescr:PUSBENDPDESCR;
  Len     :Byte;
BEGIN
  USBAttach:=$0119; {rejected}
  IF (Request<>NIL) AND (Request^.Length=0) AND NOT Data^.Active THEN
  WITH Request^ DO
  BEGIN
    IF (Command=$10) AND (Category=$92) AND (Code=$45) THEN
    BEGIN
      Service:=DevVirtToLin(pIOParams);
      NewDev:=DevVirtToLin(Service^);
      IF NewDev<>NIL THEN
      WITH NewDev^.Descriptor DO
      BEGIN
        DevOK:=True;
        IF (Vendor<>USBDONTCARE) AND (IDVendor<>Vendor) THEN DevOK:=False;
        IF (Product<>USBDONTCARE) AND (IDProduct<>Product) THEN DevOK:=False;
        IF (Device<>USBDONTCARE) AND (bcdDevice<>Device) THEN DevOK:=False;
      END;
      IF DevOK AND (NewDev.ConfigVal=0) THEN
      BEGIN
        MemFill(Data,SizeOf(USBDEV),0);
        WITH Data^ DO
        BEGIN
          Active      :=True;
          DevAddr.Addr:=NewDev^.DevAddr.Addr;
          Data        :=NewDev;
          DeviceDescr :=@NewDev^.Descriptor;
          NextDescr   :=Pointer(DeviceDescr);
          Len:=NextDescr^.Length;
          WHILE Len<>0 DO
          BEGIN {get Pointers to Descriptors - only first config is used}
            NextDescr:=Pointer(ULong(NextDescr)+Len);
            Len:=NextDescr^.Length;
            CASE NextDescr^.DescType OF
              $02: {Configuration Descriptor}
                IF ConfigDescr=NIL THEN ConfigDescr:=Pointer(NextDescr);
              $04: {Interface Descriptor}
                IF Inter0Descr=NIL THEN Inter0Descr:=Pointer(NextDescr);
              $05: {Endpoint}
                BEGIN
                  Inc(LastEndp);
                  IF LastEndp<=Inter0Descr^.NumEndPoints
                    THEN Endp_Descr[LastEndp]:=NextDescr;
                END;
            END;
          END;
          IF Inter0Descr<>NIL THEN NewDev^.InterfaceNr:=Inter0Descr^.InterfNumber;
          PipesMemSize:=SizeOf(USBPIPES)-(15-LastEndp)*SizeOf(USBPIPE); {buffer Size needed}
          USBAttach:=0;
        END;
      END;
    END;
  END;
END;

FUNCTION USBDetachGetNr(Request:PDrvRequest):USBADDR;
VAR
  USBAddr :PUShort;
BEGIN
  IF (Request<>NIL) AND (Request^.Length=0) THEN
  WITH Request^ DO
  BEGIN
    IF (Command=$10) AND (Category=$92) AND (Code=$46) THEN
    BEGIN
      USBAddr:=DevVirtToLin(pIOParams);
      USBDetachGetNr.Addr:=USBAddr^;
    END ELSE USBDetachGetNr.Addr:=USBCMDNONE;
  END ELSE USBDetachGetNr.Addr:=USBCMDNONE;
END;

PROCEDURE USBSetupPipes(Data:PUSBDEV;GDT:Selector);
VAR
  n     :ULong;
BEGIN
  IF (Data^.PipesMem<>NIL) THEN
  WITH PUSBPIPES(Data^.PipesMem) DO
  BEGIN
    MemFill(Data^.PipesMem,Data^.PipesMemSize,0);
    BaseAddr:=Data^.PipesMem;
    MaxIndex:=Data^.LastEndp;
    DevLinToGDTSelector(GDT,BaseAddr,Data^.PipesMemSize);
    Setup16.Seg:=GDT;
    Buffer16.Seg:=GDT;
    Setup16.Off:=ULong(Addr(Setup))-ULong(BaseAddr);
    Buffer16.Off:=ULong(Addr(Buffer))-ULong(BaseAddr);
    FOR n:=0 TO MaxIndex DO
    WITH Pipe[n] DO {Init Pipe Values}
    BEGIN
      DrvReq16.Seg:=GDT;
      DrvCan16.Seg:=GDT;
      IOReqBlk16.Seg:=GDT;
      Sem16.Seg:=GDT;
      DrvReq16.Off:=ULong(Addr(DrvReq))-ULong(BaseAddr);
      DrvCan16.Off:=ULong(Addr(DrvCan))-ULong(BaseAddr);
      IOReqBlk16.Off:=ULong(Addr(IOReqBlk))-ULong(BaseAddr);
      Sem16.Off:=ULong(Addr(Sem))-ULong(BaseAddr);
      {Setup Semaphore}
      Sem:=0;
      DevSemRequest(Sem16,SEM_NOWAIT);
      WITH DrvReq DO
      BEGIN {Setup Request Packet}
        Command:=$10;
        Category:=$91;
        Code:=$41; {requestio}
        pIOParams:=IOReqBlk16;
      END;
      WITH DrvCan DO
      BEGIN {Setup Cancel Packet}
        Command:=$10;
        Category:=$91;
        Code:=$42; {cancelio}
        pIOParams:=IOReqBlk16;
      END;
      WITH IOReqBlk DO
      BEGIN {Setup Request Packet}
        DevAddr:=Data^.DevAddr;
        IRQEntry:=MYIDC.PEntry;
        IRQDS:=MYIDC.EntryS;
        IRQCategory:=$92;
        IRQData1DEV:=Data;     {set Pointer1 to USBDEV}
        IRQData3ENDP:=n;       {set Pointer3 to Endpoint Index}
        IF Data^.Endp_Descr[n] <> NIL THEN
            MaxPktSize:=(Data^.Endp_Descr[n].MaxPktSize AND $07FF);
        IF n=0 THEN
        BEGIN {Endpoint 0 - control transfers}
          IF Data^.Data.LowSpeed>0 THEN Flags:=$1004 ELSE Flags:=$0004;
          Buffer1:=Setup16;
          Buffer1Len:=SizeOf(SETUPPKT);
          Buffer2:=Buffer16;
          Buffer2Len:=CTRLBUFFSIZE;
          MaxErrCnt:=3;
        END ELSE
        BEGIN {endpoint 1-15 - various transfers}
          EndPointID:=(Data^.Endp_Descr[n]^.EndpAddress AND $0F);
          CASE (Data^.Endp_Descr[n]^.Attributes AND $03) OF
          01: {isochronous}
            Flags:=$8000;
          02: {bulk}
            Flags:=$2000;
          03: {interrupt}
            Flags:=$4000;
          END;
          IF Data^.Data.LowSpeed>0 THEN Flags:=(Flags OR $1000);
          IF (Data^.Endp_Descr[n]^.EndpAddress AND $80)>0
          THEN
            Flags:=Flags OR $01 {device to host}
          ELSE
            Flags:=Flags OR $02;{host to device}
          MaxErrCnt:=3;  {max err count}
          ServiceTime:=0;{default service time}
        END;
      END;
    END;
  END;
END;

FUNCTION USBIssueControlRequest(Data:PUSBDEV;ReqType,Req,ReqValue,
                                ReqIndex,Length:UShort):ApiRet;
BEGIN
  IF (Data^.PipesMem<>NIL) AND NOT PUSBPIPES(Data^.PipesMem).Pipe[0].Pending THEN
  BEGIN
    IF Length>CTRLBUFFSIZE THEN Length:=CTRLBUFFSIZE;
    WITH PUSBPIPES(Data^.PipesMem)^.Setup DO
    BEGIN {setup packet}
      RType   :=ReqType;
      Request :=Req;
      RValue  :=ReqValue;
      RIndex  :=ReqIndex;
      RLength :=Length;
    END;
    WITH PUSBPIPES(Data^.PipesMem)^.Pipe[0] DO
    BEGIN
      Pending:=True;
      Wait:=CTRL_MAXWAIT;
      LastLen:=0;
      LastStat:=0;
      DrvReq.Status:=0;
      {no toggle, cause control ist always started with DATA0}
      IOReqBlk.Buffer2Len:=Length; {necessary cause len has to be 0 on set config}
      DevSemRequest(Sem16,SEM_NOWAIT);{make shure Sem is owned}
      DevCallDD16(@USBIDC,DrvReq16,DD_NOPARM);
      IF DrvReq.Status=STATUS_DONE THEN
      BEGIN
        IF DevSemRequest(Sem16,CTRL_MAXWAIT)=0 THEN {wait 20s}
        BEGIN
          IF LastStat=$100 THEN
            USBIssueControlRequest:=0
          ELSE
            USBIssueControlRequest:=LastStat;
        END ELSE
        BEGIN
          {Cancel Request}
          InfAdd('Request cancelled'+CR+LF,NIL);
          DevCallDD16(@USBIDC,DrvCan16,DD_NOPARM);
          USBIssueControlRequest:=$0002; {not found}
          Pending:=False;
        END;
      END ELSE
      BEGIN
        USBIssueControlRequest:=DrvReq.Status;
        Pending:=False;
      END;
    END;
  END ELSE USBIssueControlRequest:=$0073;
END;

FUNCTION USBClearStalledPipe(Data:PUSBDEV;Endpoint:Byte):ApiRet;
TYPE
  CLRSTA=RECORD
    DevAddr     : USBADDR;
    EndpointID  : Byte;
    IRQEntry    : Pointer16;
    IRQDS       : Selector;
    IRQData1DEV : Pointer;
    IRQData2FNC : USBIRQFNC;
    IRQData3ENDP: ULong;
    IRQCategory : Byte;
    Packet      : Pointer16;
  END;
VAR
  ClrBlk  :CLRSTA;
  ClrReq  :PDrvRequest;
  ClrReq16:Pointer16;
BEGIN
  ClrReq16:=DevAllocReqPacket(False);
  ClrReq:=DevVirtToLin(ClrReq16);
  IF (ClrReq<>NIL) AND (Data<>NIL) AND (Data^.PipesMem<>NIL) THEN
  BEGIN
    WITH ClrBlk DO
    BEGIN
      DevAddr:=Data^.DevAddr;
      EndpointID:=Endpoint;
      IRQEntry  :=MYIDC.PEntry;
      IRQDS     :=MYIDC.EntryS;
      IRQData1DEV:=Data;     {set Pointer1 to USBDEV}
      IRQData2FNC:=NIL;
      IRQData3ENDP:=Endpoint;{set Pointer3 to Endpoint Index}
      IRQCategory:=$92;
      Packet   :=PUSBPIPES(Data^.PipesMem).Setup16;
    END;
    WITH ClrReq^ DO
    BEGIN
      MemFill(ClrReq,SizeOf(DrvRequest),0);
      Length:=SizeOf(DrvRequest);
      Command:=$10;    {IOCtl}
      Category:=$91;   {USBD}
      Code:=$4A;       {ClrStalled}
      pIOParams:=LocToDat16(@ClrBlk);
    END;
    DevCallDD16(@USBIDC,ClrReq16,DD_NOPARM);
    IF ClrReq.Status=STATUS_DONE THEN
      WITH PUSBPIPES(Data^.PipesMem)^.Pipe[Endpoint] DO
      BEGIN
      InfAdd('CLR stalled Pipe WAIT...',NIL);
      IF DevSemRequest(Sem16,20000)=0 THEN {wait 20s}
      BEGIN
        InfAdd('CLR ok'+CR+LF,NIL);
        PUSBPIPES(Data^.PipesMem)^.Pipe[Endpoint].Toggle:=0;
        USBClearStalledPipe:=0;
      END ELSE
      BEGIN
        InfAdd('CLR TIMEOUT'+CR+LF,NIL);
        USBClearStalledPipe:=$002;
      END;
    END ELSE USBClearStalledPipe:=ClrReq^.Status;
    DevFreeReqPacket(ClrReq16);
  END ELSE USBClearStalledPipe:=$0073;
END;

FUNCTION USBSetConfig(Data:PUSBDEV;Config,Interf:Byte):ApiRet;
TYPE
  SETCONF=RECORD
    DevAddr   :USBADDR;
    DSValue   :UShort;
    ConfValue :Byte;
    IRQSwitch :ULong;
    IRQCat    :Byte;
    Packet    :Pointer16;
  END;
VAR
  SetBlk  :SETCONF;
  SetReq  :PDrvRequest;
  SetReq16:Pointer16;
BEGIN
  SetReq16:=DevAllocReqPacket(False);
  SetReq:=DevVirtToLin(SetReq16);
  IF (SetReq<>NIL) AND (Data^.Data.ConfigVal=0) THEN
  BEGIN
    WITH SetBlk DO
    BEGIN
      DevAddr:=Data^.DevAddr;
      DSValue:=MYIDC.EntryS;
      ConfValue:=Config;
      IRQSwitch:=$00;  {keep zero to clear GenSem (only LOWORD is copied by USBD}
      IRQCat   :=$92;
      Packet   :=PUSBPIPES(Data^.PipesMem).Setup16;
    END;
    WITH SetReq^ DO
    BEGIN
      MemFill(SetReq,SizeOf(DrvRequest),0);
      Length:=SizeOf(DrvRequest);
      Command:=$10;    {IOCtl}
      Category:=$91;   {USBD}
      Code:=$48;       {SetConf}
      pIOParams:=LocToDat16(@SetBlk);
    END;
    InfAdd('Set Config'+CR+LF,NIL);
    DevCallDD16(@USBIDC,SetReq16,DD_NOPARM);
    DevSemRequest(GenSem16,SEM_NOWAIT); {make shure, sem is owned}
    IF SetReq^.Status=STATUS_DONE THEN
    BEGIN
      IF DevSemRequest(GenSem16,20000)=0
        THEN USBSetConfig:=USBSetInterface(Data,Interf)
        ELSE USBSetConfig:=2;
    END ELSE USBSetConfig:=SetReq^.Status;
    DevFreeReqPacket(SetReq16);
  END ELSE USBSetConfig:=0; {tell ok to caller if already configured}
END;

FUNCTION USBSetInterface(Data:PUSBDEV;Interf:Byte):ApiRet;
TYPE
  SETCONF=RECORD
    DevAddr   :USBADDR;
    DSValue   :UShort;
    InterValue:Byte;
    IRQSwitch :ULong;
    IRQCat    :Byte;
    Packet    :Pointer16;
  END;
VAR
  SetBlk  :SETCONF;
  SetReq  :PDrvRequest;
  SetReq16:Pointer16;
BEGIN
  SetReq16:=DevAllocReqPacket(False);
  SetReq:=DevVirtToLin(SetReq16);
  IF SetReq<>NIL THEN
  BEGIN
    WITH SetBlk DO
    BEGIN
      DevAddr:=Data^.DevAddr;
      DSValue:=MYIDC.EntryS;
      InterValue:=Interf;
      IRQSwitch:=$00;  {keep zero to clear GenSem (only LOWORD is copied by USBD}
      IRQCat   :=$92;
      Packet   :=PUSBPIPES(Data^.PipesMem).Setup16;
    END;
    WITH SetReq^ DO
    BEGIN
      MemFill(SetReq,SizeOf(DrvRequest),0);
      Length:=SizeOf(DrvRequest);
      Command:=$10;    {IOCtl}
      Category:=$91;   {USBD}
      Code:=$49;       {SetInterface}
      pIOParams:=LocToDat16(@SetBlk);
    END;
    InfAdd('Set Interface'+CR+LF,NIL);
    DevCallDD16(@USBIDC,SetReq16,DD_NOPARM);
    DevSemRequest(GenSem16,SEM_NOWAIT); {make shure, sem is owned}
    IF SetReq^.Status=STATUS_DONE THEN
    BEGIN
      IF DevSemRequest(GenSem16,20000)=0 THEN USBSetInterface:=0
                                         ELSE USBSetInterface:=2;
    END ELSE USBSetInterface:=SetReq^.Status;
    DevFreeReqPacket(SetReq16);
  END ELSE USBSetInterface:=$0073;
END;

FUNCTION USBGetDataAddr(Data:PUSBDEV;Endpoint:Byte):Pointer;
BEGIN
  IF (Data^.PipesMem<>NIL) AND (Endpoint<=Data^.LastEndp) THEN
  WITH PUSBPIPES(Data^.PipesMem) DO
  BEGIN
    IF Endpoint=0
      THEN USBGetDataAddr:=@Buffer
      ELSE
      BEGIN
        WITH Pipe[Endpoint].IOReqBlk DO
        BEGIN
          IF (Flags AND $0080)=0
            THEN USBGetDataAddr:=Pointer(Buffer1.Off+ULong(BaseAddr))
            ELSE USBGetDataAddr:=NIL;
        END;
      END;
  END ELSE USBGetDataAddr:=NIL;
END;

FUNCTION USBGetDataLastLen(Data:PUSBDEV;Endpoint:Byte):UShort;
BEGIN
  IF (Data^.PipesMem<>NIL) AND (Endpoint<=Data^.LastEndp) THEN
    USBGetDataLastLen:=PUSBPIPES(Data^.PipesMem).Pipe[Endpoint].LastLen
  ELSE USBGetDataLastLen:=0;
END;

FUNCTION USBGetDataStatus(Data:PUSBDEV;Endpoint:Byte):UShort;
BEGIN
  IF (Data^.PipesMem<>NIL) AND (Endpoint<=Data^.LastEndp) THEN
    USBGetDataStatus:=PUSBPIPES(Data^.PipesMem).Pipe[Endpoint].LastStat
  ELSE USBGetDataStatus:=$0002;
END;

FUNCTION USBGetString(Data:PUSBDEV;Buffer:PChar;StrIndex:Byte):ApiRet;
VAR
  pLangID :PUShort;
  pLen    :PByte;
  pUni    :PUShort;
  n       :ULong;
BEGIN
  {get Language}
  IF (Buffer<>NIL) AND (USBIssueControlRequest(Data,$80,$06,$0300,0,4)=0) THEN
  BEGIN
    pLangID:=Pointer(ULong(USBGetDataAddr(Data,0))+2);
    {get Length}
    IF USBIssueControlRequest(Data,$80,$06,$0300+StrIndex,pLangID^,1)=0 THEN
    BEGIN
      pLen:=USBGetDataAddr(Data,0);
      {get Length}
      IF USBIssueControlRequest(Data,$80,$06,$0300+StrIndex,pLangID^,pLen^)=0 THEN
      BEGIN
        pUni:=Pointer(ULong(USBGetDataAddr(Data,0))+2);
        {convert Unicode to ASCII}
        FOR n:=0 TO ((pLen^-2) DIV 2) DO
        BEGIN
          Buffer[n]:=CHR(pUni^);
          INC(pUni);
        END;
        Buffer[n]:=CHR(0);
        USBGetString:=0;
      END;
    END;
  END ELSE USBGetString:=$FF;
END;

FUNCTION USBIssueDataRequest(Data:PUSBDEV;Endpoint:Byte;
                             Buffer:Pointer16;Length:UShort;
                             MaxWait:ULong;Func:USBIRQFNC):ApiRet;
BEGIN
  IF (Data^.PipesMem<>NIL) AND
    NOT PUSBPIPES(Data^.PipesMem).Pipe[Endpoint].Pending THEN
  BEGIN
    WITH PUSBPIPES(Data^.PipesMem)^.Pipe[Endpoint] DO
    BEGIN
      Pending:=True;
      Wait:=MaxWait;
      LastLen:=0;
      LastStat:=0;
      DrvReq.Status:=0;
      WITH IOReqBlk DO
      BEGIN
        Flags:=(Flags AND NOT $0080);
        Flags:=(Flags AND NOT $0800) OR Toggle;
        Buffer1:=Buffer;
        Buffer1Len:=Length;
        IRQData2FNC:=Func;
 {       InfAdd('Start Request $$$$$$$$'+CR+LF,@EndPointID);}
      END;
      DevSemRequest(Sem16,SEM_NOWAIT);{make shure Sem is owned}
      DevCallDD16(@USBIDC,DrvReq16,DD_NOPARM);
      IF DrvReq.Status=STATUS_DONE THEN
      BEGIN
        IF (DevSemRequest(Sem16,Wait)=0) OR (Wait=SEM_NOWAIT) THEN {wait as specified}
        BEGIN
          IF (LastStat=$100) OR (Wait=SEM_NOWAIT) THEN
            USBIssueDataRequest:=0
          ELSE
            USBIssueDataRequest:=LastStat;
        END ELSE
        BEGIN
          {Cancel Request}
          InfAdd('Request cancelled'+CR+LF,NIL);
          USBIssueDataRequest:=$0002; {not found}
          Pending:=False;
        END;
      END ELSE
      BEGIN
        USBIssueDataRequest:=DrvReq.Status;
        Pending:=False;
      END;
    END;
  END ELSE USBIssueDataRequest:=$0073;
END;

FUNCTION USBIssueDataRequestP(Data:PUSBDEV;Endpoint:Byte;
                             Buffer:PointerP;Length:UShort;
                             MaxWait:ULong;Func:USBIRQFNC):ApiRet;
BEGIN
  IF (Data^.PipesMem<>NIL) AND
    NOT PUSBPIPES(Data^.PipesMem).Pipe[Endpoint].Pending THEN
  BEGIN
    WITH PUSBPIPES(Data^.PipesMem)^.Pipe[Endpoint] DO
    BEGIN
      Pending:=True;
      Wait:=MaxWait;
      LastLen:=0;
      LastStat:=0;
      DrvReq.Status:=0;
      WITH IOReqBlk DO
      BEGIN
        Flags:=(Flags OR $0080);
        Flags:=(Flags AND NOT $0800) OR Toggle;
        Buffer1:=Pointer16(Buffer);
        Buffer1Len:=Length;
        IRQData2FNC:=Func;
{        InfAdd('Start RequestPhys $$$$$$$$'+CR+LF,@EndPointID);}
      END;
      DevSemRequest(Sem16,SEM_NOWAIT);{make shure Sem is owned}
      DevCallDD16(@USBIDC,DrvReq16,DD_NOPARM);
      IF DrvReq.Status=STATUS_DONE THEN
      BEGIN
        IF (DevSemRequest(Sem16,Wait)=0) OR (Wait=SEM_NOWAIT) THEN {wait as specified}
        BEGIN
          IF (LastStat=$100) OR (Wait=SEM_NOWAIT) THEN
            USBIssueDataRequestP:=0
          ELSE
            USBIssueDataRequestP:=LastStat;
        END ELSE
        BEGIN
          {Cancel Request}
          InfAdd('Request cancelled'+CR+LF,NIL);
          USBIssueDataRequestP:=$0002; {not found}
          Pending:=False;
        END;
      END ELSE
      BEGIN
        USBIssueDataRequestP:=DrvReq.Status;
        Pending:=False;
      END;
    END;
  END ELSE USBIssueDataRequestP:=$0073;
END;


FUNCTION USBCancelRequest(Data:PUSBDEV;Endpoint:Byte):ApiRet;
VAR
  Count:  ULong;
BEGIN
  IF (Data^.PipesMem<>NIL) AND
    PUSBPIPES(Data^.PipesMem).Pipe[Endpoint].Pending THEN
  BEGIN
    WITH PUSBPIPES(Data^.PipesMem)^.Pipe[Endpoint] DO
    BEGIN
      DrvCan.Status:=0;
      DevCallDD16(@USBIDC,DrvCan16,DD_NOPARM);
      IF DrvCan.Status=STATUS_DONE THEN
      BEGIN
        InfAdd('Request cancelled'+CR+LF,NIL);
        DevSemClear(Sem16);  {release waiting thread}
        USBCancelRequest:=0; {ok}
      END ELSE
      BEGIN
        USBCancelRequest:=DrvCan.Status;
        InfAdd('ERROR while Request cancelled'+CR+LF,NIL);
        DevSemClear(Sem16);        {release thread}
      END;
      Count:=0;
      IF Pending THEN
      BEGIN
        DevSemRequest(Sem16,SEM_NOWAIT);
        InfAdd('Waiting to finish'+CR+LF,NIL);
        DevSemRequest(Sem16,Wait); {block this thread till finished}
      END;
    END;
  END ELSE USBCancelRequest:=$0073;
END;

FUNCTION USBHandleIRQ(Request:PDrvRequest):PUSBIRQINF;
VAR
  ResultPacket:PIOREQUEST;
  Device      :PUSBDEV;
  Endpoint    :UShort;
  Pipes       :PUSBPIPES;
  OK          :Boolean;
BEGIN
  ResultPacket:=DevVirtToLin(Request^.pIOParams);
  WITH ResultPacket DO
  BEGIN
    Device  :=IRQData1DEV;
    Endpoint:=IRQData3ENDP AND $FF;
    IF (Device<>NIL) AND (Endpoint<=15) AND (Endpoint>=0) THEN
    BEGIN
      Pipes:=Device^.PipesMem;
      WITH Pipes^.IRQInfo DO
      BEGIN
        pUSBDev:=Device;
        DevAddr:=Device^.DevAddr;
        EndpointID:=Endpoint;
      END;
      IF (Pipes<>NIL) THEN
      WITH Pipes.Pipe[Endpoint] DO
      BEGIN
        {InfAdd('Result is here $$$$$$$$'+CR+LF,@EndPointID);}
        LastStat:=Request^.Status;
        Toggle:=(NOT ResultPacket^.Flags AND $0800);
        USBStat:=(ResultPacket^.Status);
        Pending:=False;
        IF Endpoint=0 THEN
        BEGIN
          IF LastStat=$0100
            THEN LastLen:=ResultPacket.Buffer2Len
            ELSE LastLen:=0;
          DevSemClear(Sem16);
        END ELSE
        BEGIN
          {clear stalled pipe} // NEIN-> Crash beim ausstecken und kein zuverl„ssiger Neustart
          //IF (USBStat AND $40)>0 THEN USBClearStalledPipe(Device,Endpoint);
          IF LastStat=$0100
            THEN LastLen:=ResultPacket.Buffer1Len
            ELSE LastLen:=0;
          IF @IRQData2FNC<>NIL THEN
          BEGIN {start postprocessing}
            OK:=IRQData2FNC(@Pipes.IRQInfo);
            IF NOT OK THEN LastStat:=$FFFF;
          END;
          DevSemClear(Sem16);
        END;
        USBHandleIRQ:=@Pipes.IRQInfo
      END ELSE USBHandleIRQ:=NIL;
    END ELSE
    BEGIN
      DevSemClear(GenSem16);
      USBHandleIRQ:=NIL;
    END;
  END;
END;
END.
