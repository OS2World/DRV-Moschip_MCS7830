{$B-,D+,H-,I-,J+,P-,Q-,R-,S-,T-,V+,W-,X+,Z-}
{&AlignCode-,AlignData+,AlignRec-,Asm-,Cdecl-,Comments-,Delphi-,Frame+,G5+}
{&LocInfo+,Open32-,Optimise+,OrgName-,SmartLink-,Speed+,Use32+,ZD-}
{$M 32768}

unit DRVSTUB1;

INTERFACE

USES DRVBASE,RMBASE,MACBASE,USBBASE;

PROCEDURE Strategy (Request:PDrvRequest);
PROCEDURE IDCEntry (Data:Pointer;Data16:Pointer16);
PROCEDURE Context (Hook:ULong;Param:ULong);
FUNCTION Interrupt (IRQ:LongBool):LongBool;
FUNCTION MACEntry (Func:ULong;Param:Pointer;Data:PMACMODULE):MACREQRET;

IMPLEMENTATION
CONST
  DRV_HEAD  ='MCS7830$';
  HEADLINE  ='Moschip MCS7830 USB to ETHERNET Driver';
  FILENAME  ='MCS7830.OS2';
  AUTHOR    ='Holger Veith';
  DAT_YEAR  = 2011;
  DAT_MONTH = 03;
  DAT_DAY   = 21;
  VERSION   = 1;
  SUBVERSION= 3;

  // Shutdown-Lock eingefÅhrt
  // kein Senden an USB wenn ResetPending, danach aber auf jeden Fall ein ContextCall


  INFBUFFSIZE=1000;

  MACBUFFSIZE=1520;
  MACBIDLE  =0;
  MACBFILL  =1;
  MACBFULL  =2;
  MACBTRANS =3;

  SNDBUFFCNT=21;
  RCVBUFFCNT=9;
  RCUBUFFCNT=9;

  GSIDLE    =2;

  CTX_SNDS  =$00010000; {send frame start}
  CTX_RCVD  =$00200000; {loop recive done}
  CTX_RCUD  =$02000000; {usb recive done}
  CTX_RCUOR =$04000000; {usb recive out of ressource}
  CTX_INDC  =$10000000; {send indication complete}
  CTX_INDAC =$20000000; {send adapter check}
  CTX_INDIRQ=$40000000; {send IRQ indication}
  CTX_USBDET=$00000001; {usb adapter detached}
  CTX_USBATT=$00000002; {usb adapter attached}
  CTX_GETSTA=$00000010; {get adapter status}

TYPE
  PMACBUFFER=^MACBUFFER;
  MACBUFFER=RECORD
    FrLen   :ULong;
    Status  :ULong;
    ProtID  :SmallInt;
    Handle  :SmallInt;
    Buff    :ARRAY[0..MACBUFFSIZE] OF Byte;
  END;
  PMACADDITIONAL=^MACADDITIONAL;
  MACADDITIONAL=RECORD
    CtxRequest  :ULong;
    SndBNextFill:ULong;
    SndBFillCompl:ULong;
    SndBNextTrns:ULong;
    SndBuff     :ARRAY[0..SNDBUFFCNT] OF MACBUFFER;
    RcvBNextFill:ULong;
    RcvBFillCompl:ULong;
    RcvBNextTrns:ULong;
    RcvBuff     :ARRAY[0..RCVBUFFCNT] OF MACBUFFER;
    RcuBNextFill:ULong;
    RcuBFillCompl:ULong;
    RcuBNextTrns:ULong;
    RcuBuff     :ARRAY[0..RCUBUFFCNT] OF MACBUFFER;
    UsbLink     :PUSBDEV;
    RMUsbDevH   :ULong;
    MACAddrOvr  :Boolean;
    MACStatus   :ULong;
    IdleCnt     :ULong;
    SerialNr    :ARRAY[0..20] OF Char;
    LocalMAC    :ARRAY[0..6] OF Byte;
    UseLocalMAC :Boolean;
    RcStarted   :Boolean;
    TxStarted   :Boolean;
    NoRcCount   :ULong;
    IndUSBBuff  :Boolean;
    EtherMode   :Byte;
  END;

VAR
  InfBuff:ARRAY[0..INFBUFFSIZE] OF Char;   {Puffer fÅr Debug-Info}
  MACMasterTable:MACMASTER;
  USB           :ARRAY[0..2] OF USBDEV;
  USBGDT        :ARRAY[0..2] OF Selector;
  UsbCnt        :ULong;
  GlInfo        :PGlobalInfo;
  GetStatusBuff :ARRAY[0..10] OF UShort;   {may not be local cause Stack corruption on delayed answer}
  Dummy         :PhysPage;
  Shutdown      :Boolean;                  // true if Shutdown is running - no more actions allowed

FUNCTION USBRcFrame(IRQ:PUSBIRQINF):Boolean; FORWARD;
FUNCTION USBTxFrameDone(IRQ:PUSBIRQINF):Boolean; FORWARD;

{******************************************}
{*                                        *}
{* USB Device specific Routines           *}
{*                                        *}
{*                                        *}
{*                                        *}
{*                                        *}
{******************************************}

PROCEDURE ResetStats(MAC:PMACMODULE);
BEGIN
  WITH MAC^.MSS DO
  BEGIN
    {recive stats}
    MssFR    :=0;
    MssRFCRC :=0;
    MssFRByt :=0;
    MssRFLack:=0;
    MssFRMC  :=0;
    MssFRBC  :=0;
    MssRFHW  :=0;
    {send stats}
    MssFS    :=0;
    MssFSByt :=0;
    MssFSBC  :=0;
    MssSFTime:=0;
    MssSFHW  :=0;
    {set time}
    MssClearDT:=GlInfo^.Time;
  END;
END;

PROCEDURE SendFrame(MAC:PMACMODULE;USB:PUSBDEV);
VAR
  AData   :PMACADDITIONAL;
  FIndex  :ULong;
  Offset  :UShort;
  BuffP   :PointerP;
  rc      :ApiRet;
BEGIN
  IF (MAC<>NIL) AND (USB<>NIL) AND (MAC^.MoreData<>NIL) AND NOT USB.ResetPending THEN
  BEGIN
    AData:=MAC^.MoreData;
    WITH AData^DO
    BEGIN
      Inc(SndBNextTrns);
      IF SndBNextTrns>SNDBUFFCNT THEN SndBNextTrns:=0;
    END;
    WITH AData.SndBuff[AData.SndBNextTrns] DO
    BEGIN
      Offset:=ULong(@Buff)-ULong(MAC^.BaseAddr);
      BuffP:=MAC^.BaseAddrPhys;
      BuffP:=BuffP+Offset;
      {if Len=USB-Framesize add 1 0-Byte to mark End of Frame}
      IF (FrLen MOD USB^.Endp_Descr[2].MaxPktSize)=0 THEN
      BEGIN
        Buff[FrLen]:=0;
        INC(FrLen);
      END;
      rc:=USBIssueDataRequestP(USB,2,BuffP,FrLen,SEM_NOWAIT,USBTxFrameDone);
      IF rc<>0 THEN
      BEGIN
        MACTransmitConfirm(MAC,ProtID,Handle,MAC_XMITERR);
        FrLen :=0;
        ProtID:=0;
        Handle:=0;
        Status:=MACBIDLE;
      END;
    END;
  END;
END;

PROCEDURE SetupRcPipe(MAC:PMACMODULE;USB:PUSBDEV);
VAR
  AData   :PMACADDITIONAL;
  FIndex  :ULong;
  Offset  :UShort;
  BuffP   :PointerP;
  rc      :ApiRet;
BEGIN
  IF (MAC<>NIL) AND (USB<>NIL) AND (MAC^.MoreData<>NIL) THEN
  BEGIN
    AData:=MAC^.MoreData;
    IF NOT AData^.RcStarted THEN
    BEGIN
      WITH AData DO
      BEGIN
        FIndex:=RcuBNextFill;
        {Set pointer to next Buffer}
        Inc(RcuBNextFill);
        IF RcuBNextFill>RCUBUFFCNT THEN RcuBNextFill:=0;
      END;
      WITH AData^.RcuBuff[FIndex] DO
      BEGIN
        IF (Status=MACBIDLE) AND (FIndex<>AData^.RcuBNextTrns) THEN
        BEGIN
          AData^.RcStarted:=True;
          AData^.NoRcCount:=0;
          Status:=MACBFILL;
          FrLen :=0;
          ProtID:=0;
          Handle:=FIndex;
          {InfAdd('GetData1'+CR+LF,NIL);}
          Offset:=ULong(@Buff)-ULong(MAC^.BaseAddr);
          BuffP:=MAC^.BaseAddrPhys;
          BuffP:=BuffP+Offset;
          rc:=USBIssueDataRequestP(USB,1,BuffP,MACBUFFSIZE,SEM_NOWAIT,USBRcFrame);
          IF rc<>0 THEN
          BEGIN
            IF AData^.RcuBNextFill>=1
              THEN Dec(AData^.RcuBNextFill)
              ELSE AData^.RcuBNextFill:=RCUBUFFCNT;
            Status:=MACBIDLE; {on Error mark Buffer idle}
            InfAdd('RcuDenied: '+CR+LF,NIL);
            INC(MAC^.MSS.MssRFHW);
            AData^.RcStarted:=False;
          END;
        END ELSE
        BEGIN
          IF AData^.RcuBNextFill>=1
             THEN Dec(AData^.RcuBNextFill)
             ELSE AData^.RcuBNextFill:=RCUBUFFCNT;
          InfAdd('RcuFillOutRes:'+CR+LF,NIL);
          INC(MAC^.MSS.MssRFLack);
          INC(AData^.NoRcCount);
          IF AData^.NoRcCount < $1000 THEN
          BEGIN
            {request processing of recived data}
            AData^.CtxRequest:=AData^.CtxRequest OR (CTX_RCUOR + CTX_RCUD);
            DevArmCtxHook(CTX_THTC,CTX_RCUOR + CTX_RCUD);
          END;
        END;
      END;
    END;
  END;
END;

PROCEDURE GetMACStatus(MAC:PMACMODULE;Wait:Boolean);
VAR
  AData   :PMACADDITIONAL;
  USBData :PUSBDEV;
  rc      :ApiRet;
  n       :ULong;
  Count   :ULong;
BEGIN
  IF MAC<>NIL THEN
  BEGIN
    AData:=MAC^.MoreData;
    IF AData<>NIL THEN USBData:=AData^.USBLink ELSE USBData:=NIL;
    IF (USBData<>NIL) AND (AData<>NIL) THEN
    BEGIN
      {signal start of get status}
      MAC.MSS.MssStatus:=MAC.MSS.MssStatus OR $20; {diagnose started}
      MAC.MSS.MssDiagDT:=GlInfo^.Time;
      {read status from Endpoint 3}
      rc:=USBIssueDataRequest(USBData,3,DevLinToVirt(@GetStatusBuff),$10,20000,NIL);
      IF rc=0 THEN
      BEGIN
        AData^.MACStatus:=0;
        FOR n:=0 TO 7 DO
          AData^.MACStatus:=AData^.MACStatus OR GetStatusBuff[n];
      END ELSE
      BEGIN
        {keep old status value}
        InfAdd('ERROR reading Status!'+CR+LF,NIL);
      END;
      {set status bits in MSC}
      IF (AData^.MACStatus AND $0800)>0
        THEN MAC.MSC.MscLinkSpd:=100000000 {100 Mbit}
        ELSE MAC.MSC.MscLinkSpd:=10000000; {10 Mbit}
      IF ((AData^.MACStatus AND $2000)=0) OR
         ((AData^.MACStatus AND $0C00)>0)
        THEN MAC.MSS.MssStatus:=MAC.MSS.MssStatus OR $4 {Link ok}
        ELSE MAC.MSS.MssStatus:=MAC.MSS.MssStatus AND NOT $4;
      IF Wait AND ((MAC.MSS.MssStatus AND $4)=0) THEN {wait on network connect}
      BEGIN
        InfAdd('Wait on Network connect'+CR+LF,NIL);
        Count:=0;
        WHILE ((MAC.MSS.MssStatus AND $4)=0) AND (Count<=10) DO
        BEGIN
          DevSleep(1000); // wait 1 sec
          InfAdd('Status $$$$, Retry: ',@MAC.MSS.MssStatus);
          GetMacStatus(MAC,False);
          INC(Count);
        END;
        IF NOT((MAC.MSS.MssStatus AND $4)=0) THEN InfAdd('Link ok '+CR+LF,NIL);
      END;
      {signal end of diagnose}
      MAC.MSS.MssStatus:=MAC.MSS.MssStatus AND NOT $20; {diagnose end}
    END;
  END;
END;

FUNCTION SetMACRXPolicy (MAC:PMACMODULE):ApiRet;
VAR
  AData   :PMACADDITIONAL;
  USBData :PUSBDEV;
  pOption :PByte;
BEGIN
  IF MAC<>NIL THEN
  BEGIN
    AData:=MAC^.MoreData;
    USBData:=AData^.USBLink;
    IF (USBData<>NIL) AND (AData<>NIL) THEN
    BEGIN
      pOption:=USBGetDataAddr(USBData,0);
      IF pOption<>NIL THEN
      BEGIN
        pOption^:=$08+$04; {set TX on + sleep enable}
        IF (MAC^.MSS.MssFilter AND $4) > 0 THEN
          pOption^:=pOption^ OR $01; {promiscuous mode}
        IF ((MAC^.MSS.MssFilter AND $1) > 0) AND (MAC^.MCB.McbCnt>0) THEN
          pOption^:=pOption^ OR $02; {multicast mode}
        pOption^:=pOption^ OR AData^.EtherMode; {Speed&Duplex Mode}
        IF USBIssueControlRequest(USBData,$40,$0D,0,14,1)<>0 THEN
        BEGIN
        {retry}
          IF USBIssueControlRequest(USBData,$40,$0D,0,14,1)<>0 THEN
          BEGIN
            InfAdd('Error, in Communication while SetPolicy'+CR+LF,NIL);
            SetMACRXPolicy:=$FFFF;
          END ELSE SetMACRXPolicy:=0;
        END ELSE SetMACRXPolicy:=0;
      END ELSE
      BEGIN
        InfAdd('Error, NIL-Pointer while SetPolicy'+CR+LF,NIL);
        SetMACRXPolicy:=$FFFF;
      END;
    END ELSE
    BEGIN
      InfAdd('Error, no USB-Port specified in SetPolicy'+CR+LF,NIL);
      SetMACRXPolicy:=$FFFF;
    END;
  END ELSE
  BEGIN
    InfAdd('Error, no Adapter specified in SetPolicy'+CR+LF,NIL);
    SetMACRXPolicy:=$FFFF;
  END;
END;

FUNCTION RestartMAC (USBData:PUSBDEV):ApiRet;
VAR
  AData :PMACADDITIONAL;
  rc    :ApiRet;
  MACCnt:ULong;
  n     :UShort;
  pText1:PChar;
  pText2:PChar;
  Text1 :ARRAY[0..60] OF Char;
  Text2 :ARRAY[0..60] OF Char;
BEGIN
  AData:=NIL;
  IF USBData<>NIL THEN
  BEGIN
    WITH USBData DO
    BEGIN
      ResetPending:=True;
      InfAdd('Reset started'+CR+LF,NIL);
      pText1:=LocToDat32(@Text1);
      pText2:=LocToDat32(@Text2);
      { get Device Description Text}
      rc:=USBGetString(USBData,pText1,2);
      { get Device Serial Number}
      IF rc=0 THEN rc:=USBGetString(USBData,pText2,3);
      IF rc=0 THEN
      BEGIN
        StrLCat(pText1,' SN:',60);
        StrLCat(pText1,pText2,60);
        InfAdd(pText1,NIL);
        InfAdd(CR+LF,NIL);
      END ELSE
      BEGIN
        pText1:='USB To Ethernet Adapter';
        pText2:='';
      END;
      IF Link=NIL THEN
      BEGIN
        {if device is not linked, link to MAC-Structure}
        FOR MACCnt:=0 TO MACMasterTable.Count-1 DO
        IF MACMasterTable.GDT[MACCnt]<>0 THEN
        WITH MACMasterTable.Module[MACCnt]^ DO
        BEGIN
          AData:=MoreData;

          InfAdd('Module: % / ',LocToDat32(@MACCnt));
          InfAdd(pText2,NIL);
          InfAdd(' / ',NIL);
          InfAdd(@AData^.SerialNr,NIL);
          InfAdd(CR+LF,NIL);

          IF (StrLen(pText2)>0) AND (StrLen(@AData^.SerialNr)>0) THEN
          BEGIN
            { if both strings exit, test for equality}
            IF (StrPos(pText2,@AData^.SerialNr)<>NIL) AND (AData^.USBLink=NIL) THEN
            BEGIN
              { link them together}
              Link:=MACMasterTable.Module[MACCnt];
              AData^.USBLink:=USBData;
              InfAdd('Device linked specific'+CR+LF,NIL);
              MACCnt:=MACMasterTable.Count;
            END;
          END ELSE
            { if serial no is undefined, take the first empty entry}
            IF AData^.USBLink=NIL THEN
            BEGIN
              { link them together}
              Link:=MACMasterTable.Module[MACCnt];
              AData^.USBLink:=USBData;
              InfAdd('Device linked not specific'+CR+LF,NIL);
              MACCnt:=MACMasterTable.Count;
            END;
        END;   {else set AData to linked MAC}
      END ELSE AData:=PMACMODULE(Link)^.MoreData;
      rc:=$FFFF;
      {setup USB-Adapter}
      IF (Link<>NIL) AND (AData<>NIL) THEN
          rc:=USBSetConfig(USBData,
                           USBData^.ConfigDescr^.ConfigValue,
                           USBData^.Inter0Descr^.InterfNumber);
      IF rc=0 THEN
      BEGIN
        {inform protocols about reset}
        MACIndicateResetStatus(Link,True,0);
        WITH PMACMODULE(Link)^.MSS DO
          MssStatus:=MssStatus AND NOT $0F; {Hardware disabled}
        IF (AData^.RMUsbDevH=0) THEN
        BEGIN
          {add Ressource}
          RMCreateAdapter(pText1,FALSE,AS_BASE_NETWORK,AS_SUB_ETHERNET,
                          AS_HOSTBUS_OTHER,AS_BUSWIDTH_8BIT);
          AData^.RMUsbDevH:=RMGetLastAdapterHandle;
          InfAdd('Adapter added to RM'+CR+LF,NIL);
        END;
        {get MAC Address}
        rc:=USBIssueControlRequest(USBData,$C0,$0E,0,15,6);
        IF (rc=0) AND (NOT AData^.MACAddrOvr) THEN
        BEGIN
          pText1:=USBGetDataAddr(USBData,0);
          pText2:=@PMACMODULE(Link)^.MSC.MscPermStnAdr;
          MemMove(pText1,pText2,6);
          {set Current-Addr=perm Addr}
          pText2:=@PMACMODULE(Link)^.MSC.MscCurrStnAdr;
          MemMove(PText1,pText2,6);
          InfAdd('MAC-Address is:',NIL);
          BytesToHex(pText2,pText1,6);
          InfAdd(pText1,NIL);
          InfAdd(CR+LF,NIL);
        END ELSE IF (NOT AData^.MACAddrOvr) THEN
        BEGIN
          InfAdd('Error reading MAC-Address'+CR+LF,NIL);
          RestartMAC:=$FFFF;
        END;
        {set threshold difference}
        IF (rc=0) THEN
        BEGIN
          pText1:=USBGetDataAddr(USBData,0);
          pText1^:=CHR($01); {threshold = 128 words}
          rc:=USBIssueControlRequest(USBData,$40,$0D,0,23,1);
          IF rc<>0 THEN InfAdd('ERROR setting threshold value!'+CR+LF,NIL);
        END;
        {set wakeup on lan}
        IF (rc=0) THEN
        BEGIN
          rc:=USBIssueControlRequest(USBData,0,3,1,0,0);
          IF rc<>0 THEN InfAdd('ERROR setting wakeup feature!'+CR+LF,NIL);
        END;
        IF (rc=0) AND AData^.MACAddrOvr THEN
        BEGIN
          {set MAC-Addr}
          pText1:=USBGetDataAddr(USBData,0);
          pText2:=@PMACMODULE(Link)^.MSC.MscCurrStnAdr;
          MemMove(pText2,pText1,6);
          rc:=USBIssueControlRequest(USBData,$40,$0D,0,15,6);
          InfAdd('MAC-Address is set to:',NIL);
          BytesToHex(pText2,pText1,6);
          InfAdd(pText1,NIL);
          InfAdd(CR+LF,NIL);
          IF rc<>0 THEN
          BEGIN
            InfAdd('Error setting MAC-Address'+CR+LF,NIL);
            RestartMAC:=$FFFF;
          END;
        END;
        IF rc=0 THEN
        BEGIN
          {reset MAC statistics}
          ResetStats(Link);
          {get ethernet status}
          GetMACStatus(Link,True);
          {rest of MAC initialisation}
          IF NOT USBData^.DeviceInitOK THEN
          WITH AData^ DO
          BEGIN
            {reset buffer, but only if new attached, else recive-buffer is overwritten}
            IdleCnt:=0;
            SndBNextFill:=0;
            SndBFillCompl:=SNDBUFFCNT;
            SndBNextTrns:=SNDBUFFCNT;
            RcvBNextFill:=0;
            RcvBFillCompl:=RCVBUFFCNT;
            RcvBNextTrns:=RCVBUFFCNT;
            RcuBNextFill:=0;
            RcuBFillCompl:=RCUBUFFCNT;
            RcuBNextTrns:=RCUBUFFCNT;
            FOR n:=0 TO SNDBUFFCNT DO
              SndBuff[n].Status:=MACBIDLE;
            FOR n:=0 TO RCVBUFFCNT DO
              RcvBuff[n].Status:=MACBIDLE;
            FOR n:=0 TO RCUBUFFCNT DO
              RcuBuff[n].Status:=MACBIDLE;
          END;
          WITH PMACMODULE(Link) DO
          BEGIN
            {set rest of status bits}
            MSS.MssStatus:=MSS.MssStatus OR $3; {Hardware ok}
            MSS.MssStatus:=MSS.MssStatus OR $8; {open}
            rc:=MSS.MssStatus;
          END;
          RestartMAC:=SetMACRXPolicy (Link);
          {inform Protocol that we are ready}
          IF (rc AND $07)=$07
            THEN MACIndicateResetStatus(Link,False,MAC_SUCCESS)   {success}
            ELSE MACIndicateResetStatus(Link,False,MAC_NONETWORK);{network not connected}
          MACIndicationComplete(Link);
          USBData^.DeviceInitOK:=True;
          SetupRcPipe(Link,USBData);
        END;
      END ELSE
      BEGIN
        InfAdd('Error, no Adapter found'+CR+LF,NIL);
        RestartMAC:=$FFFF;
      END;
      ResetPending:=False;
      DevArmCtxHook(CTX_THTC,CTX_SNDS);  {Request Transmission, may be blocked during ResetPending}
    END;
  END ELSE
  BEGIN
    InfAdd('Error, no Adapter specified'+CR+LF,NIL);
    RestartMAC:=$FFFF;
  END;
END;

{******************************************}
{*                                        *}
{* DRIVER Init Routine                    *}
{*                                        *}
{* Input: Pointer to RequestPacket        *}
{* Output:STATUS_DONE or STATUS_GENFAIL   *}
{*                                        *}
{******************************************}

FUNCTION Init(Request:PDrvRequest):DrvRet;
VAR
  ParamText :PChar;
  n         :ULong;
  rc        :ApiRet;
  Key       :PMACKEYWORDENTRY;
  Val       :PMACPARAMENTRY;
  Count     :ULong;
BEGIN
  {basic setup}
  Shutdown:=False;
  Init:=STATUS_GENFAIL;
  InfInit(InfBuff,INFBUFFSIZE);
  rc:=DevSetOptions(DRV_HEAD,IDC_STACK,IRQ_NONE,CTX_THTC+CTX_THFS,DevLinToVirt(@InfBuff));
  IF rc=0 THEN
  BEGIN
    {parse config.sys params}
    ParamText:=DevVirtToLin(Request^.pInitArg);
    IF (ParamText<>NIL) AND (StrLen(ParamText)>0) THEN
    BEGIN
      IF GetParam(ParamText,'/V',NIL) OR GetParam(ParamText,'/W',NIL) THEN
      BEGIN
        n:=VERSION;
        InfAdd(HEADLINE+' Version %.',LocToDat32(@n));
        n:=SUBVERSION;
        InfAdd('%'+CR+LF,LocToDat32(@n));
      END;
      IF NOT GetParam(ParamText,'/W',NIL) THEN InfSet(False);
    END;
    rc:=RMInit(FILENAME,HEADLINE,AUTHOR,
               DAT_DAY,DAT_MONTH,DAT_YEAR,
               DRT_NETWORK,DRS_UNDEFINED);
    IF rc=0 THEN
    BEGIN
      rc:=MACInit(@MACMasterTable,DRV_HEAD,3,SizeOf(MACADDITIONAL));
      IF rc=0 THEN
      BEGIN
        FOR n:=0 TO MACMasterTable.Count-1 DO
        BEGIN
          IF MACMasterTable.GDT[n]<>0 THEN
          WITH MACMasterTable.Module[n] DO
          BEGIN
            CCT.CcMjrVer:=VERSION; {Module Version}
            CCT.CcMnrVer:=SUBVERSION;
            MSC.MscService:=MSC.MscService OR MAC_BROADCAST;
            MSC.MscService:=MSC.MscService OR MAC_MULTICAST;
            MSC.MscService:=MSC.MscService OR MAC_PROMISCUOUS;
            MSC.MscService:=MSC.MscService OR MAC_STNADDRSET;
            MSC.MscService:=MSC.MscService OR MAC_STATSCURRENT;
            MSC.MscService:=MSC.MscService OR MAC_DIAG;
            MSC.MscService:=MSC.MscService OR MAC_HWLOOPBACK;
            MSC.MscService:=MSC.MscService OR MAC_RESET;
            MSC.MscService:=MSC.MscService OR MAC_IRQREQ;
            MSC.MscMaxFrame:=1514;
            MSC.MscTBufCap:=SNDBUFFCNT*1514;
            MSC.MscTBlkSz:=1514;
            MSC.MscRBufCap:=RCUBUFFCNT*1514;
            MSC.MscRBlkSz:=1514;
            MSC.MscTxQDepth:=SNDBUFFCNT;
            IF (IniData<>NIL) AND (MoreData<>NIL) THEN
            BEGIN
              InfAdd('MODULE $:'+CR+LF+'Name: ',LocToDat32(@n));
              InfAdd(IniData^.ModuleName,NIL);
              InfAdd(CR+LF,NIL);
              Key:=Addr(IniData^.KeyWordEnt);
              WHILE Key <> NIL DO
              BEGIN
                Val:=Addr(Key^.ParamEnt);
                FOR Count:=1 TO Key^.ParamCount DO
                BEGIN
                  IF StrPos(Addr(Key^.Keyword),'ETHERMODE') <> NIL THEN
                  BEGIN
                    IF (Val^.ParamType=0) AND (Val^.Value>0) THEN
                    BEGIN
                      InfAdd('Ethermode set to ',NIL);
                      WITH PMACADDITIONAL(MoreData)^ DO
                      BEGIN
                        EtherMode:=Val^.Value;
                        IF EtherMode>4 THEN EtherMode:=0;
                        CASE EtherMode OF
                          0: InfAdd('AUTO'+CR+LF,NIL);
                          1: InfAdd('10 MBit, HalfDuplex'+CR+LF,NIL);
                          2: InfAdd('10 MBit, FullDuplex'+CR+LF,NIL);
                          3: InfAdd('100 MBit, HalfDuplex'+CR+LF,NIL);
                          4: InfAdd('100 MBit, FullDuplex'+CR+LF,NIL);
                        END;
                        IF EtherMode>0 THEN
                        BEGIN
                          DEC(EtherMode);
                          EtherMode:=((EtherMode SHL 5) OR $80);
                        END;
                      END;
                    END;
                  END;
                  IF StrPos(Addr(Key^.Keyword),'SERIALNR') <> NIL THEN
                  BEGIN
                    IF (Val^.ParamType=1) AND (StrLen(@Val^.Param)>0) THEN
                    BEGIN
                      InfAdd('Serial Number Request: ',NIL);
                      InfAdd(@Val^.Param,NIL);
                      InfAdd(CR+LF,NIL);
                      StrLCopy(@PMACADDITIONAL(MoreData)^.SerialNr,@Val^.Param,20);
                    END;
                  END;
                  IF StrPos(Addr(Key^.Keyword),'NODE') <> NIL THEN
                  BEGIN
                    IF (Val^.ParamType=1) AND (StrLen(@Val^.Param)>0) THEN
                    BEGIN
                      InfAdd('MAC-Address Request: ',NIL);
                      InfAdd(@Val^.Param,NIL);
                      InfAdd(CR+LF,NIL);
                      HexToBytes(@Val^.Param,@PMACADDITIONAL(MoreData)^.LocalMAC,6);
                      PMACADDITIONAL(MoreData)^.UseLocalMAC:=True;
                    END;
                  END;
                  Val:=Ptr(ULong(Val)+SizeOf(MACPARAMENTRY)+Val^.ParamLen-4);
                END;
                Key:=PMACKEYWORDENTRY(DevVirtToLin(Key^.NextEntry));
              END;
            END;
          END;
        END;
        IF rc=0 THEN rc:=MACRegister(@MACMasterTable);
        IF rc=0 THEN
        BEGIN
          rc:=USBInit;
          IF rc=0 THEN
          BEGIN
            rc:=DevAllocGDTSelector(USBGDT[0],3);
            IF rc=0 THEN Init:=STATUS_DONE;
          END ELSE InfAdd('Connection to USBD failed!'+CR+LF,NIL);
        END ELSE InfAdd('Register on Protman failed!'+CR+LF,NIL);
      END ELSE InfAdd('No MAC-Module found in PROTMAN.INI!'+CR+LF,NIL);
      IF rc<>0 THEN RMCancel;
    END ELSE InfAdd('Connection to Ressource Manager failed!'+CR+LF,NIL);
  END;
END;

{******************************************}
{*                                        *}
{* DRIVER Strategy Routine                *}
{*                                        *}
{* Input: Pointer to RequestPacket        *}
{* Output:Set Status to appropriate Values*}
{*                                        *}
{******************************************}

PROCEDURE Strategy (Request:PDrvRequest);
BEGIN
  CASE Request^.Command OF
    COMMAND_INIT:
      BEGIN
        Request^.Status:=Init(Request);
      END;
    COMMAND_READ:
      BEGIN
        InfCopy(Request^.pData,Request^.Size);
        Request^.Status:=STATUS_DONE;
      END;
    COMMAND_OPEN:
      Request^.Status:=STATUS_DONE;
    COMMAND_CLOSE:
      Request^.Status:=STATUS_DONE;
    COMMAND_INITCMPL:
      BEGIN
        USBLink(DRV_HEAD);
        GlInfo:=DevGetGlobalInfo;
        Request^.Status:=STATUS_DONE;
      END;
    COMMAND_SHUTDOWN:
      BEGIN
        Shutdown:=True;
        Request^.Status:=STATUS_DONE;
      END;
    ELSE
      Request^.Status:=STATUS_ILLEGAL;
  END;
END;

{******************************************}
{*                                        *}
{* DRIVER IDC Entry, called by USBD.SYS   *}
{*                                        *}
{* Input: Pointer to RequestPacket        *}
{* Output:Set Status to appropriate Values*}
{*                                        *}
{******************************************}

PROCEDURE IDCEntry (Data:Pointer;Data16:Pointer16);
VAR
  Cmd     :CmdReq;
  USBCnt  :ULong;
  USBDet  :USBADDR;
  rc      :ApiRet;
  PInfo   :PULong;
BEGIN
  IF NOT Shutdown THEN
  BEGIN
    Cmd:=USBCheckIDC(Data);
    CASE Cmd OF
      USBCMDATT:
      BEGIN
        PDrvRequest(Data)^.Status:=$0119; {default: rejected}
        FOR USBCnt:=0 TO 2 DO
        BEGIN
          IF NOT USB[USBCnt].Active THEN
          BEGIN
            rc:=USBAttach(Data,@USB[USBCnt],$9710,$7830,USBDONTCARE);
            IF (rc=0) AND (USB[USBCnt].LastEndp=3) THEN
            BEGIN
              USB[USBCnt].Endp_Descr[3].Attributes:=2;{force bulk transfer, cause OS2 doesn't like it as interrupt}
              InfAdd('USB-Device Nr $$ attached.'+CR+LF,LocToDat32(@USBCnt));
              USB[USBCnt].CtxFlags:=USB[USBCnt].CtxFlags OR CTX_USBATT;
              DevArmCtxHook(CTX_THFS,CTX_USBATT);{further Init within ContextHook}
              USBCnt:=3; {done}
              PDrvRequest(Data)^.Status:=$100; {ok}
            END;
          END;
        END;
      END;
      USBCMDDET:
      BEGIN
        USBDet:=USBDetachGetNr(Data);
        PDrvRequest(Data)^.Status:=$119; {rejected}
        FOR USBCnt:=0 TO 2 DO
        BEGIN
          IF USB[USBCnt].Active AND (USB[USBCnt].DevAddr.Addr=USBDet.Addr) THEN
          BEGIN
            InfAdd('USB-Device Nr $$ detached.'+CR+LF,LocToDat32(@USBCnt));
            USB[USBCnt].CtxFlags:=USB[USBCnt].CtxFlags OR CTX_USBDET;
            DevArmCtxHook(CTX_THFS,CTX_USBDET);{cleanup within ContextHook}
            PDrvRequest(Data)^.Status:=$100; {ok}
          END;
        END;
      END;
      USBCMDIRQ:
      BEGIN
        USBHandleIRQ(Data);
        PDrvRequest(Data)^.Status:=$100;
      END;
    END;
  END ELSE PDrvRequest(Data)^.Status:=$100; // signal done on shutdown - no actions allowed
END;


{******************************************}
{*                                        *}
{* USB Data Exchange Context Recive Frame *}
{*                                        *}
{* Input: IRQ Info Struct                 *}
{* Output:Release Calling Proces          *}
{*                                        *}
{******************************************}

FUNCTION USBRcFrame(IRQ:PUSBIRQINF):Boolean;
VAR
Device  :PUSBDEV;
MAC     :PMACMODULE;
AData   :PMACADDITIONAL;
BIndex  :ULong;
BEGIN
  Device:=IRQ^.pUsbDev;
  IF (Device<>NIL) AND (Device^.Link<>NIL) THEN
  BEGIN
    MAC:=PMACMODULE(Device^.Link);
    AData:=MAC^.MoreData;
    WITH AData^DO
    BEGIN
      IdleCnt:=0;  {do not call get Status cause bandwith is needed for the Frames}
      MAC.MSS.MssStatus:=MAC.MSS.MssStatus OR $7; {when Frame recived, Link must be ok}
      RcStarted:=False;
      BIndex:=RcuBFillCompl;
      Inc(BIndex);
      IF BIndex>RCUBUFFCNT THEN BIndex:=0;
    END;
    WITH AData^.RcuBuff[BIndex] DO
    BEGIN
      IF USBGetDataStatus(Device,IRQ^.EndpointID)=$0100 THEN
      BEGIN
        Status:=MACBFULL;
        FrLen:=USBGetDataLastLen(Device,IRQ^.EndpointID)-1;
        IF Buff[FrLen]=$20 THEN
        BEGIN
          {InfAdd('Rc done',NIL);}
          IF FrLen < MAC^.MinLenLkAh THEN FrLen:=MAC^.MinLenLkAh;
          AData^.RcuBFillCompl:=BIndex;
          AData^.CtxRequest:=AData^.CtxRequest OR CTX_RCUD;
          DevArmCtxHook(CTX_THTC,CTX_RCUD);
        END ELSE
        BEGIN
          FrLen :=0;
          ProtID:=0;
          Handle:=0;
          Status:=MACBIDLE;
          InfAdd('Rc illegal Frame done',NIL);
          INC(MAC^.MSS.MssRFCRC);
        END;
      END ELSE
      BEGIN
        FrLen :=0;
        ProtID:=0;
        Handle:=0;
        Status:=MACBIDLE;
        InfAdd('Rc Error!',NIL);
        INC(MAC^.MSS.MssRFHW);
      END;
    END;
    SetupRcPipe(MAC,Device);
  END;
  USBRcFrame:=True;
END;

{******************************************}
{*                                        *}
{* USB Data Exchange Context              *}
{*                                        *}
{* Input: IRQ Info Struct                 *}
{* Output:Release Calling Proces          *}
{*                                        *}
{******************************************}

FUNCTION USBTxFrameDone(IRQ:PUSBIRQINF):Boolean;
VAR
Device  :PUSBDEV;
MAC     :PMACMODULE;
AData   :PMACADDITIONAL;
BEGIN
  Device:=IRQ^.pUsbDev;
  IF (Device<>NIL) AND (Device^.Link<>NIL) THEN
  BEGIN
    MAC:=PMACMODULE(Device^.Link);
    AData:=MAC^.MoreData;
    WITH AData^.SndBuff[AData^.SndBNextTrns] DO
    BEGIN
      IF Status=MACBTRANS THEN
      BEGIN
        IF USBGetDataStatus(Device,IRQ^.EndpointID)=$0100 THEN
        BEGIN
          MACTransmitConfirm(MAC^.BaseAddr,ProtId,Handle,MAC_SUCCESS);
        END ELSE
        BEGIN
          MACTransmitConfirm(MAC^.BaseAddr,ProtId,Handle,MAC_XMITERR);
          InfAdd('Confirm Error done',NIL);
        END;
      END;
      FrLen :=0;
      ProtID:=0;
      Handle:=0;
      Status:=MACBIDLE;
      END;
    WITH AData^DO
    IF (SndBNextTrns<>SndBFillCompl)
      THEN SendFrame(MAC,Device)
      ELSE TxStarted:=False;
  END;
  USBTxFrameDone:=True;
END;

{******************************************}
{*                                        *}
{* DRIVER Context Hook Routine            *}
{*                                        *}
{* Input: Bit Mask from Calling ArmCtxHook*}
{* Output:none                            *}
{*                                        *}
{******************************************}

PROCEDURE Context (Hook:ULong;Param:ULong);
VAR
  n     :ULong;
  Buff  :ULong;
  AddrT :ULong;
  Buff16:Pointer16;
  AData :PMACADDITIONAL;
  Text  :ARRAY[0..160] OF Char;
BEGIN
  IF (Param AND $0000FFFF)>0 THEN
  BEGIN
    {Process USB Contexts}
    FOR n:=0 TO 2 DO
    WITH USB[n] DO
    IF Active THEN
    BEGIN
      IF (CtxFlags AND CTX_GETSTA)>0 THEN
      BEGIN {get device status}
        IF (USB[n].Link<>NIL) AND NOT (USB[n].ResetPending) THEN
        BEGIN
          AData:=PMACMODULE(USB[n].Link).MoreData;
          IF (AData<>NIL) AND (AData^.IdleCnt>=GSIDLE) THEN
          BEGIN
            {InfAdd('GetStatus called '+CR+LF,NIL);}
            GetMACStatus(USB[n].Link,False);
          END;
        END;
        CtxFlags:=CtxFlags AND NOT CTX_GETSTA;
      END;

      IF (CtxFlags AND CTX_USBDET)>0 THEN
      BEGIN {device detached}
        ResetPending:=True;
        {cancel all open requests}
        {USBCancelRequest(@USB[n],0);
        USBCancelRequest(@USB[n],1);
        USBCancelRequest(@USB[n],2);
        USBCancelRequest(@USB[n],3); }//disabled-does not work and may cause hang on shutdown
        IF USB[n].Link<>NIL THEN
        BEGIN
          AData:=PMACMODULE(USB[n].Link).MoreData;
          IF AData<>NIL THEN
          BEGIN
            {rc-Task not running}
            AData^.RcStarted:=False;
            {mark hardware as not functional}
            MACIndicateResetStatus(Link,True,0);
            PMACMODULE(USB[n].Link).MSS.MssStatus:=
              PMACMODULE(USB[n].Link).MSS.MssStatus AND NOT $07;
            MACIndicateResetStatus(Link,False,MAC_NONETWORK);
            AData^.CtxRequest:=AData^.CtxRequest OR CTX_INDAC;
            DevArmCtxHook(CTX_THTC,CTX_INDAC);
            {kill Link}
            USB[n].Link:=NIL;
            AData^.USBLink:=NIL;
          END;
          DevFreeMem(PipesMem);
          MemFill(@USB[n],SizeOf(USBDEV),0);
          InfAdd('Cleanup OK'+CR+LF,NIL);
          ResetPending:=False;
          {stop status timer}
          DEC(USBCnt);
          IF USBCnt=0 THEN DevResetTimer;
        END;
        CtxFlags:=CtxFlags AND NOT CTX_USBDET;
      END;

      IF (CtxFlags AND CTX_USBATT)>0 THEN
      BEGIN {device attached}
        PipesMem:=DevGetMemPhys(PipesMemSize,FALSE,Dummy);
        IF (PipesMem<>NIL) AND NOT InitComplete THEN
        BEGIN
          {reset all USB-Structures}
          USBSetupPipes(@USB[n],USBGDT[n]);
          USB[n].DeviceInitOK:=False;
          {init device}
          RestartMAC (@USB[n]);
          {setup status timer 10 sec}
          INC(USBCnt);
          IF USBCnt = 1 THEN DevTickCount(312);
          {if local MAC-Address is set, tell it to hardware}
          IF USB[n].Link <> NIL THEN
          BEGIN
            IF PMACMODULE(USB[n].Link) <> NIL THEN
            BEGIN
              AData:=PMACMODULE(USB[n].Link)^.MoreData;
              IF (AData <> NIL) AND (AData^.UseLocalMAC) THEN
              BEGIN
                AData^.MACAddrOvr:=True;
                MemMove(Addr(AData^.LocalMAC),
                  Addr(PMACMODULE(USB[n].Link)^.MSC.MscCurrStnAdr),
                  PMACMODULE(USB[n].Link)^.MSC.MscStnAdrSz);
                RestartMAC (@USB[n]);{sets MAC Address}
              END;
            END;
          END;
        END;
        CtxFlags:=CtxFlags AND NOT CTX_USBATT;
      END;
    END;
  END;
  IF (Param AND $FFFF0000)>0 THEN
  BEGIN
    {Process MAC Contexts}
    FOR n:=0 TO MACMasterTable.Count-1 DO
    IF MACMasterTable.GDT[n]<>0 THEN
    WITH MACMasterTable.Module[n]^ DO
    BEGIN
      AData:=MoreData;
      WITH AData^ DO
      BEGIN
       { InfAdd('Context run ',NIL); }
       { InfAdd('SndBNextFill: %%%'+CR+LF,@SndBNextFill);
        InfAdd('SndBFillCompl: %%%'+CR+LF,@SndBFillCompl);
        InfAdd('SndBNextTrns: %%%'+CR+LF,@SndBNextTrns);
        InfAdd('RcuBNextFill: %%%'+CR+LF,@RcuBNextFill);
        InfAdd('RcuBFillCompl: %%%'+CR+LF,@RcuBFillCompl);
        InfAdd('RcuBNextTrns: %%%'+CR+LF,@RcuBNextTrns);}

        IF ((CtxRequest AND CTX_RCUOR)>0) THEN
        BEGIN
          CtxRequest:=CtxRequest AND NOT CTX_RCUOR;
          InfAdd('Rcu Overrun Context called',NIL);
          SetupRcPipe(BaseAddr,USBLink);
        END;

        IF (CtxRequest AND CTX_SNDS)>0 THEN
        BEGIN
          {GetMACStatus(BaseAddr,False);}
          IF (MSS.MssStatus AND $7)=7 THEN
          BEGIN
            {InfAdd('SendFrame started',NIL);}
            SendFrame(BaseAddr,USBLink);
          END;
          CtxRequest:=CtxRequest AND NOT CTX_SNDS;
        END;

        IF ((CtxRequest AND CTX_INDAC)>0) AND (IndicationCnt=0) THEN
        BEGIN
          MACAdapterCheck(BaseAddr,$8800);
          InfAdd('Adapter check required!',NIL);
          CtxRequest:=CtxRequest OR CTX_INDC;
          DevArmCtxHook(CTX_THTC,CTX_INDC);
          CtxRequest:=CtxRequest AND NOT CTX_INDAC;
        END;

        IF ((CtxRequest AND CTX_INDIRQ)>0) AND (IndicationCnt=0) THEN
        BEGIN
          MACIndicateIRQ(BaseAddr);
          InfAdd('IRQ request!',NIL);
          CtxRequest:=CtxRequest OR CTX_INDC;
          DevArmCtxHook(CTX_THTC,CTX_INDC);
          CtxRequest:=CtxRequest AND NOT CTX_INDIRQ;
        END;

        IF (CtxRequest AND CTX_RCVD)>0 THEN
        BEGIN
          AData^.IndUSBBuff:=False;
          WHILE (RcvBFillCompl<>RcvBNextTrns) AND (IndicationCnt=0) DO
          BEGIN
            DevYield;
            INC (RcvBNextTrns);
            IF RcvBNextTrns>RCVBUFFCNT THEN RcvBNextTrns:=0;
            WITH RcvBuff[RcvBNextTrns] DO
            BEGIN
              AddrT:=MACTestFilter(BaseAddr,Addr(Buff));
              IF (Status=MACBFULL) AND (AddrT<>MAC_ADDR_NONE) THEN
              BEGIN
                Buff16.Seg:=MacMasterTable.GDT[n];
                Buff16.Off:=ULong(Addr(Buff[0]))-ULong(BaseAddr);
                MACReciveLookahead(BaseAddr,Buff16,FrLen);
                FrLen :=0;
                ProtID:=0;
                Handle:=0;
                Status:=MACBIDLE;
                CtxRequest:=CtxRequest OR CTX_INDC;
              END;
            END;
          END;
          IF (RcvBFillCompl=RcvBNextTrns) THEN CtxRequest:=CtxRequest AND NOT CTX_RCVD;
          {IF (CtxRequest AND CTX_INDC)>0 THEN DevArmCtxHook(CtxHook.Prim,CTX_INDC);}
        END;

        IF (CtxRequest AND CTX_RCUD)>0 THEN
        BEGIN
          AData^.IndUSBBuff:=True;
          WHILE (RcuBFillCompl<>RcuBNextTrns) AND (IndicationCnt=0) DO
          BEGIN
            {InfAdd('RCU Indication ',NIL);}
            DevYield;
            INC (RcuBNextTrns);
            IF RcuBNextTrns>RCUBUFFCNT THEN RcuBNextTrns:=0;
            WITH RcuBuff[RcuBNextTrns] DO
            BEGIN
              AddrT:=MACTestFilter(BaseAddr,Addr(Buff));
              IF (Status=MACBFULL) AND (AddrT<>MAC_ADDR_NONE) THEN
              BEGIN
                WITH PMACMODULE(BaseAddr)^.MSS DO
                BEGIN
                  INC(MssFR);
                  INC(MssFRByt,FrLen);
                  IF AddrT=MAC_ADDR_BROAD THEN INC(MssFRBC);
                  IF AddrT=MAC_ADDR_MULTI THEN INC(MssFRMC);
                END;
                Buff16.Seg:=MacMasterTable.GDT[n];
                Buff16.Off:=ULong(Addr(Buff[0]))-ULong(BaseAddr);
                MACReciveLookahead(BaseAddr,Buff16,FrLen);
                FrLen :=0;
                ProtID:=0;
                Handle:=0;
                Status:=MACBIDLE;
                CtxRequest:=CtxRequest OR CTX_INDC;
              END ELSE
              BEGIN
                {discard}
                FrLen :=0;
                ProtID:=0;
                Handle:=0;
                Status:=MACBIDLE;
              END;
            END;
          END;
          IF (RcuBFillCompl=RcuBNextTrns) THEN CtxRequest:=CtxRequest AND NOT CTX_RCUD;
          {IF (CtxRequest AND CTX_INDC)>0 THEN DevArmCtxHook(CtxHook.Prim,CTX_INDC);}
        END;

        IF (CtxRequest AND CTX_INDC)>0 THEN
        BEGIN
          {InfAdd('Indication complete ',NIL);}
          MACIndicationComplete(BaseAddr);
          CtxRequest:=CtxRequest AND NOT CTX_INDC;
        END;

      END;
    END;
  END;
  DevYield;
END;

{******************************************}
{*                                        *}
{* DRIVER Interrupt/Timer Routine         *}
{*                                        *}
{* Input: Bool IRQ or Timer Call          *}
{* Output:Bool:was my IRQ                 *}
{*                                        *}
{******************************************}

FUNCTION Interrupt (IRQ:LongBool):LongBool;
VAR
  n     :ULong;
  MAC   :PMACMODULE;
  AData :PMACADDITIONAL;
BEGIN
  IF NOT IRQ THEN
  BEGIN
    {Timer call}
    FOR n:=0 TO 2 DO
    WITH USB[n] DO
    IF Active THEN
    BEGIN
      MAC:=Link;
      IF (MAC<>NIL) AND (MAC^.MoreData<>NIL) THEN
      BEGIN
        AData:=MAC^.MoreData;
        WITH AData DO
        BEGIN
          IF IdleCnt>=GSIDLE THEN
          BEGIN
            CtxFlags:=CtxFlags OR CTX_GETSTA;
            DevArmCtxHook(CTX_THFS,CTX_GETSTA);
          END ELSE INC(IdleCnt);
        END;
      END;
    END;
  END;
  Interrupt:=FALSE;
END;

{******************************************}
{*                                        *}
{* MAC Worker Routines                    *}
{*                                        *}
{*                                        *}
{*                                        *}
{*                                        *}
{******************************************}

FUNCTION XmitChain (Param:PMACXMITCHAIN;Data:PMACMODULE):MACREQRET;
VAR
  AData   :PMACADDITIONAL;
  Buffer  :PMACBUFFER;
  USBData :PUSBDEV;
  Offset  :UShort;
  BuffP   :PointerP;
  FIndex  :ULong;
  DIndex  :ULong;
  XmitStat:ULong;
  AddrT   :ULong;
  rc      :MACREQRET;
  Count   :UShort;
BEGIN
  AData:=Data^.MoreData;
  XmitChain.PSize:=SizeOf(MACXMITCHAIN);
  IF (Data^.MSS.MssStatus AND $7)>0 THEN
  BEGIN
    XmitChain.RStat:=MAC_NONETWORK;
  END ELSE XmitChain.RStat:=MAC_HARDERR;
  IF (Data^.MSS.MssStatus AND $7)=7 THEN
  BEGIN
    IF AData<>NIL THEN
    BEGIN
      USBData:=AData^.USBLink;
      IF USBData<>NIL THEN
      BEGIN
        WITH AData^ DO
        BEGIN
          FIndex:=SndBNextFill;
          {Set pointer to next Buffer}
          Inc(SndBNextFill);
          IF SndBNextFill>SNDBUFFCNT THEN SndBNextFill:=0;
        END;
        WITH AData^.SndBuff[FIndex] DO
        BEGIN
          IF (Status=MACBIDLE) AND (FIndex<>AData^.SndBNextTrns) THEN
          BEGIN
            XmitStat:=MAC_QUEUED;
            Status:=MACBFILL;
            rc:=MACXmitChainPrep(Data,Param,Addr(Buff),MACBUFFSIZE,FrLen,XmitStat);
            IF FrLen>1514 THEN
            BEGIN
              rc.RStat:=MAC_INVALIDPAR;
              InfAdd('Xmit: Frame > 1514 Bytes!',NIL);
            END;
            IF rc.RStat<=MAC_QUEUED THEN Status:=MACBFULL ELSE Status:=MACBIDLE;
            XmitStat:=rc.RStat;
            AddrT:=MACTestFilter(Data,Addr(Buff));
            IF (Status=MACBFULL) AND (AddrT<>MAC_ADDR_NONE) THEN
            WITH AData^ DO
            BEGIN
              {Loopback - copy Data to RC-Buffer}
              DIndex:=RcvBNextFill;
              {Set pointer to next Buffer}
              Inc(RcvBNextFill);
              IF RcvBNextFill>RCVBUFFCNT THEN RcvBNextFill:=0;
              WITH RcvBuff[DIndex] DO
              BEGIN
                IF (Status=MACBIDLE) AND (DIndex<>RcvBNextTrns) THEN
                BEGIN {buffer idle}
                  MemMove(@SndBuff[FIndex].Buff,@Buff,SndBuff[FIndex].FrLen);
                  FrLen:=SndBuff[FIndex].FrLen;
                  Status:=MACBFULL;
                  CtxRequest:=CtxRequest OR CTX_RCVD;
                  DevArmCtxHook(CTX_THTC,CTX_RCVD);
                  RcvBFillCompl:=DIndex;
                END ELSE {buffer full or queue overrun}
                BEGIN
                  IF RcvBNextFill>=1 THEN Dec(RcvBNextFill) ELSE RcvBNextFill:=RCVBUFFCNT;
                  XmitStat:=MAC_OUTOFRES;
                  {request for buffer cleaning}
                  CtxRequest:=CtxRequest OR CTX_RCVD;
                  DevArmCtxHook(CTX_THTC,CTX_RCVD);
                  InfAdd('RcvDenied: %%%'+CR+LF,LocToDat32(@DIndex));
                END;
              END;
            END;
            IF (Status=MACBFULL) AND (XmitStat<=MAC_QUEUED) THEN
            BEGIN
              {prepare USB send procedure}
              Status:=MACBTRANS;
              ProtID:=Param^.ProtID;
              Handle:=Param^.ReqHandle;
              AData^.SndBFillCompl:=FIndex;
              WITH AData^ DO
              BEGIN
                IF NOT TxStarted THEN
                BEGIN
                  TxStarted:=True;
                  CtxRequest:=CtxRequest OR CTX_SNDS;
                  DevArmCtxHook(CTX_THTC,CTX_SNDS);
                  {InfAdd('Snd start OK: %%%'+CR+LF,LocToDat32(@Findex,SSOffset));}
                END {ELSE InfAdd('Snd running OK: %%%'+CR+LF,LocToDat32(@Findex,SSOffset))};
              END;
              INC(Data^.MSS.MssFS);
              INC(Data^.MSS.MssFSByt,USBGetDataLastLen(USBData,2));
              IF AddrT=MAC_ADDR_BROAD THEN INC(Data^.MSS.MssFSBC);
            END ELSE
            BEGIN
              IF AData^.SndBNextFill>=1
                THEN Dec(AData^.SndBNextFill)
                ELSE AData^.SndBNextFill:=SNDBUFFCNT;
              INC(Data^.MSS.MssSFHW);
              Status:=MACBIDLE;
              InfAdd('SndDenied: %%%'+CR+LF,LocToDat32(@FIndex));
            END;
          END ELSE
          BEGIN
            XmitStat:=MAC_OUTOFRES;
            INC(Data^.MSS.MssSFHW);
            IF AData^.SndBNextFill>=1
                THEN Dec(AData^.SndBNextFill)
                ELSE AData^.SndBNextFill:=SNDBUFFCNT;
            {InfAdd('SndFillOutRes: %%%'+CR+LF,LocToDat32(@FIndex,SSOffset));}
          END;
          XmitChain.RStat:=XmitStat;
        END;
      END ELSE XmitChain.RStat:=MAC_GENFAIL;
    END ELSE XmitChain.RStat:=MAC_GENFAIL;
  END ELSE
  IF AData<>NIL THEN
  BEGIN
    {adapter check indication when Adapter disconnected}
    IF (Data^.MSS.MssStatus AND $7)=0 THEN
    BEGIN
      AData^.CtxRequest:=AData^.CtxRequest OR CTX_INDAC;
      DevArmCtxHook(CTX_THTC,CTX_INDAC);
    END;
    INC(Data^.MSS.MssSFHW);
  END;
END;

{******************************************}
{*                                        *}
{* MAC Entry Routine                      *}
{*                                        *}
{* Input: FuncNumber,Stack Param,Module   *}
{* Output:Status,Param Length for RET     *}
{*                                        *}
{******************************************}

FUNCTION MACEntry (Func:ULong;Param:Pointer;Data:PMACMODULE):MACREQRET;
VAR
  AData   :PMACADDITIONAL;
  USBData :PUSBDEV;
  rc      :MACREQRET;
  Sem16   :Pointer16;
BEGIN
  CASE Func OF
  MAC_SYSREQ:
  BEGIN
    MACEntry:=MACBind(Param,Data);
  END;
  MAC_GENREQ:
    BEGIN
      AData:=Data^.MoreData;
      WITH PMACREQPARM(Param) DO
      BEGIN
        CASE Func OF
        MAC_GSETFILTER:
          BEGIN
            IF (Data^.MSS.MssStatus AND $07)>0 THEN
            BEGIN
              IF (Data^.MSS.MssStatus AND $07)=$03 THEN
                GetMACStatus(Data,True);
              IF (Data^.MSS.MssStatus AND $07)=$07 THEN
              BEGIN
                rc:=MACDefGenReq(Param,Data,MAC_SUCCESS);
                IF SetMACRXPolicy (Data)<>0 THEN
                BEGIN
                  rc.RStat:=MAC_HARDERR;
                  InfAdd('Failure setting Filter'+CR+LF,NIL);
                END;
              END ELSE InfAdd('Missing Network'+CR+LF,NIL);
            END ELSE
            BEGIN
              rc:=MACDefGenReq(Param,Data,MAC_HARDERR);
              InfAdd('Missing Hardware'+CR+LF,NIL);
            END;
            MACEntry:=rc;
          END;
        MAC_SETLOOKAH:
          BEGIN
            MACEntry:=MACDefGenReq(Param,Data,MAC_SUCCESS);
          END;
        MAC_GSETSTNA:
          BEGIN
            USBData:=AData^.USBLink;
            IF USBData<>NIL THEN
            BEGIN
              AData^.MACAddrOvr:=True;
              rc:=MACDefGenReq(Param,Data,MAC_SUCCESS);
              RestartMAC (USBData);{sets MAC Address}
            END ELSE rc:=MACDefGenReq(Param,Data,MAC_INVALIDFNC);
            MACEntry:=rc;
          END;
        MAC_GRESET:
          IF (AData^.USBLink<>NIL) AND NOT AData^.USBLink.ResetPending THEN
          BEGIN
            USBData:=AData^.USBLink;
            IF USBData<>NIL THEN
            BEGIN
              IF RestartMAC (USBData)=0 THEN
              BEGIN
                IF (Data^.MSS.MssStatus AND $07)=$07
                  THEN rc:=MACDefGenReq(Param,Data,MAC_SUCCESS)
                  ELSE rc:=MACDefGenReq(Param,Data,MAC_NONETWORK);
              END ELSE rc:=MACDefGenReq(Param,Data,MAC_NOHARDWARE);
            END ELSE rc:=MACDefGenReq(Param,Data,MAC_NOHARDWARE);
            MACEntry:=rc;
          END ELSE
          BEGIN
            MACEntry:=MACDefGenReq(Param,Data,MAC_INVALIDFNC);
            InfAdd('GENREQ: Reset is pending!',NIL);
          END;
        MAC_GDIAGNOSE:
          IF (AData^.USBLink<>NIL) AND NOT AData^.USBLink.ResetPending THEN
          BEGIN
            IF (Data^.MSS.MssStatus AND $07)>0 THEN
            BEGIN
              GetMACStatus(Data,True);
              rc:=MACDefGenReq(Param,Data,MAC_SUCCESS);
            END ELSE rc:=MACDefGenReq(Param,Data,MAC_HARDERR);
            MACEntry:=rc;
          END ELSE
          BEGIN
            MACEntry:=MACDefGenReq(Param,Data,MAC_INVALIDFNC);
            InfAdd('GENREQ: Reset is pending!',NIL);
          END;
        MAC_ADDMC,MAC_DELMC:
          BEGIN
            rc:=MACDefGenReq(Param,Data,MAC_SUCCESS);
            IF rc.RStat=MAC_SUCCESS THEN SetMACRXPolicy (Data);
            MACEntry:=rc;
          END;
        MAC_CLRSTATS:
          BEGIN
            ResetStats(Data);
            MACEntry:=MACDefGenReq(Param,Data,MAC_SUCCESS);
          END;
        MAC_IRQ:
          BEGIN
            IF AData<>NIL THEN
            BEGIN
              WITH AData^ DO
              BEGIN
                CtxRequest:=CtxRequest OR CTX_INDIRQ;
                DevArmCtxHook(CTX_THTC,CTX_INDIRQ);
              END;
              MACEntry:=MACDefGenReq(Param,Data,MAC_SUCCESS);
            END ELSE MACEntry:=MACDefGenReq(Param,Data,MAC_GENFAIL);
          END;
        ELSE
          BEGIN
            MACEntry:=MACDefGenReq(Param,Data,MAC_NOSUPPORT);
            InfAdd('GENREQ: unsupported Function call!'+CR+LF,NIL);
          END;
        END;
      END;
    END;
  MAC_XMITCHAIN:
    MACEntry:=XmitChain(Param,Data);
  MAC_XFERDATA:
    BEGIN
      AData:=Data^.MoreData;
      IF AData^.IndUSBBuff THEN
      WITH AData.RcuBuff[AData.RcuBNextTrns] DO
      BEGIN
        MACEntry:=MACTransferData(Param,Data,Addr(Buff[0]),FrLen);
      END ELSE
      WITH AData.RcvBuff[AData.RcvBNextTrns] DO
      BEGIN
        MACEntry:=MACTransferData(Param,Data,Addr(Buff[0]),FrLen);
      END;
    END;
  MAC_INDON:
    BEGIN
      MACEntry:=MACSetIndication(Data,True);
      IF Data^.IndicationCnt=0 THEN
      BEGIN
        AData:=Data^.MoreData;
        AData^.CtxRequest:=AData^.CtxRequest OR CTX_RCVD;
        DevArmCtxHook(CTX_THTC,CTX_RCVD);
      END;
    END;
  MAC_INDOFF:
    MACEntry:=MACSetIndication(Data,False);
  MAC_RCVRELEASE:
    {not supported}
    BEGIN
    MACEntry.PSize:=4;
    MACEntry.RStat:=MAC_NOSUPPORT;
    InfAdd('RCVRELEASE: unsupported Function call!'+CR+LF,NIL);
    END;
  END;
END;

END.

