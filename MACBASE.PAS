{$B-,D+,H-,I-,J+,P-,Q-,R-,S-,T-,V+,W-,X+,Z-}
{&AlignCode-,AlignData-,AlignRec-,Asm-,Cdecl-,Comments-,Delphi-,Frame+,G5+}
{&LocInfo+,Open32-,Optimise+,OrgName-,SmartLink+,Speed+,Use32+,ZD-}
{$M 32768}

UNIT MACBASE;

INTERFACE
USES DRVBASE;
CONST
  MAC_MAXMODULE = 3; {Max Number of Modules supported by Driver}

  MAC_ADDR_NONE = 0; {no Address match}
  MAC_ADDR_UNI  = 1; {unicast match}
  MAC_ADDR_BROAD= 2; {broadcast match}
  MAC_ADDR_PROM = 4; {promiscuous match}
  MAC_ADDR_MULTI= 8; {multicast match}

  {MAC CallIn Function Codes}
  MAC_SYSREQ    = 0;
  MAC_GENREQ    = 1;
  MAC_XMITCHAIN = 2;
  MAC_XFERDATA  = 3;
  MAC_RCVRELEASE= 4;
  MAC_INDON     = 5;
  MAC_INDOFF    = 6;

  {General Request Codes}
  MAC_GDIAGNOSE = 1;
  MAC_GERRLOG   = 2;
  MAC_GSETSTNA  = 3;
  MAC_GOPEN     = 4;
  MAC_GCLOSE    = 5;
  MAC_GRESET    = 6;
  MAC_GSETFILTER= 7;
  MAC_ADDMC     = 8;
  MAC_DELMC     = 9;
  MAC_UPDSTATS  =10;
  MAC_CLRSTATS  =11;
  MAC_IRQ       =12;
  MAC_SETFNCA   =13;
  MAC_SETLOOKAH =14;

  {Status Codes}
  MAC_SUCCESS   = $0000;
  MAC_QUEUED    = $0002;
  MAC_OUTOFRES  = $0006;
  MAC_INVALIDPAR= $0007;
  MAC_INVALIDFNC= $0008;
  MAC_NOSUPPORT = $0009;
  MAC_HARDERR   = $000A;
  MAC_XMITERR   = $000B;
  MAC_NOHARDWARE= $0024;
  MAC_NONETWORK = $002A;
  MAC_GENFAIL   = $00FF;

  {Support Codes (optional)}
  MAC_BROADCAST     =$0001;
  MAC_MULTICAST     =$0002;
  MAC_GROUPADDR     =$0004;
  MAC_PROMISCUOUS   =$0008;
  MAC_STNADDRSET    =$0010;
  MAC_STATSCURRENT  =$0020;
  MAC_DIAG          =$0040;
  MAC_HWLOOPBACK    =$0080;
  MAC_RESET         =$0400;
  MAC_OPENCLOSE     =$0800;
  MAC_IRQREQ        =$1000;
  MAC_SOURCEROUTE   =$2000;

  {Support Codes (set by BASE)}
  MAC_GDTADRESS_SUPP=$4000;
  MAC_MULTIDATAXFER =$8000;

  {REQUEST_RETURN_STRUCT}
TYPE
  MACREQRET=RECORD
    CASE Boolean OF
    FALSE:(
      PSize:UShort;
      RStat:UShort;);
    TRUE: (
      ApiRet:ULong;);
  END;

  {XMIT CHAIN PARAMS}
TYPE
  PMACXMITCHAIN=^MACXMITCHAIN;
  MACXMITCHAIN=RECORD
    MACDS     :Selector;
    TxBufDesc :Pointer16;
    ReqHandle :SmallWord;
    ProtID    :SmallWord;
  END;

  {GENERAL REQUEST PARAMS}
TYPE
  PMACREQPARM=^MACREQPARAM;
  MACREQPARAM=RECORD
    TargetDS  :Selector;
    Func      :SmallWord;
    Param2    :ULong;
    Param1    :SmallWord;
    ReqHandle :SmallWord;
    ProtDS    :SmallWord;
  END;
  {TRANSFER DATA PARAMS}
TYPE
  PMACTXDATA=^MACTXDATA;
  MACTXDATA=RECORD
    MACDS     :Selector;
    TDBufDesc :Pointer16;
    FrameOffset:SmallWord;
    PBytesCopied:Pointer16;
  END;

  {PROTMAN$ TYPES FOR INTERPRETING PROTOCOL.INI}
TYPE
  PMACPARAMENTRY   = ^MACPARAMENTRY;
  MACPARAMENTRY=RECORD
    ParamType   :UShort;
    ParamLen    :UShort;
    CASE UShort OF
    [0]:({Integer}
      Value     :ULong;);
    [1]:({String}
      Param     :Char;);
  END;
  PMACKEYWORDENTRY = ^MACKEYWORDENTRY;
  MACKEYWORDENTRY=RECORD
    NextEntry   :Pointer16;
    PrevEntry   :Pointer16;
    Keyword     :ARRAY[0..15] OF Char;
    ParamCount  :UShort;
    ParamEnt    :MACPARAMENTRY;
  END;
  PMACMODULECONFIG = ^MACMODULECONFIG;
  MACMODULECONFIG=RECORD
    NextModule  :Pointer16;
    PrevModule  :Pointer16;
    ModuleName  :ARRAY[0..15] OF Char;
    KeyWordEnt  :MACKEYWORDENTRY;
  END;
   {MAC STRUCTURES REL 2.01}
TYPE
  PMACCOMCHAR   =^MACCOMCHAR;
  MACCOMCHAR=RECORD
    CcSize      :UShort;
    CcLvlMjr    :Byte;
    CcLvlMnr    :Byte;
    CcSpLvl     :UShort;
    CcMjrVer    :Byte;
    CcMnrVer    :Byte;
    CcBindFnc   :ULong;
    CcName      :ARRAY[0..15] OF Char;
    CcUPLevel   :Byte;
    CcUIType    :Byte;
    CcLPLevel   :Byte;
    CcLIType    :Byte;
    CcModuleID  :UShort;
    CcDataSeg   :UShort;
    CcSysReq    :Pointer16;
    CcSCp       :Pointer16;
    CcSSp       :Pointer16;
    CcUDp       :Pointer16;
    CcLDp       :Pointer16;
    CcRsv1      :ULong;
    CcRsv2      :ULong;
  END;
  PMACSPECCHAR  =^MACSPECCHAR;
  MACSPECCHAR=RECORD
    MscSize     :UShort;
    MscType     :ARRAY[0..15] OF Char;
    MscStnAdrSz :UShort;
    MscPermStnAdr:ARRAY[0..15] OF Char;
    MscCurrStnAdr:ARRAY[0..15] OF Char;
    MscCurrFncAdr:ULong;
    MCastBuf    :Pointer16;
    MscLinkSpd  :ULong;
    MscService  :ULong;
    MscMaxFrame :UShort;
    MscTBufCap  :ULong;
    MscTBlkSz   :UShort;
    MscRBufCap  :ULong;
    MscRBlkSz   :UShort;
    MscVenCode  :ARRAY[0..2] OF Char;
    MscVenAdapter:Byte;
    MscVenAdaptDesc:Pointer16;
    MscInterrupt:UShort;
    MscTxQDepth :UShort;
    MscMaxNumDataBlks:UShort;
  END;
  PMACMCASTBUF  =^MACMCASTBUF;
  MACMCASTBUF=RECORD
    McbMax      :UShort;
    McbCnt      :UShort;
    mAddr       :ARRAY[0..15] OF Char;
  END;
  PMACSPECSTAT  =^MACSPECSTAT;
  MACSPECSTAT=RECORD
    MssSize     :UShort;
    MssDiagDT   :ULong;
    MssStatus   :ULong;
    MssFilter   :UShort;
    MssM8Sp     :Pointer16;
    MssClearDT  :ULong;
    MssFR       :Ulong;
    MssRFCRC    :ULong;
    MssFRByt    :ULong;
    MssRFLack   :ULong;
    MssFRMC     :ULong;
    MssFRBC     :ULong;
    MssRFErr    :ULong;
    MssRFMax    :ULong;
    MssRFMin    :ULong;
    MssFRMCByt  :ULong;
    MssFRBCByt  :ULong;
    MssRFHW     :ULong;
    MssFS       :ULong;
    MssFSByt    :ULong;
    MssFSMC     :ULong;
    MssFSBC     :ULong;
    MssFSBCByt  :ULong;
    MssFSMCByt  :ULong;
    MssSFTime   :ULong;
    MssSFHW     :ULong;
  END;
  PMACUPRDISP   =^MACUPRDISP;
  MACUPRDISP=RECORD
    MudCCp      :Pointer16;
    MudGReq     :Pointer16;
    MudXmitChain:Pointer16;
    MudXferData :Pointer16;
    MudRcvRelease:Pointer16;
    MudIndOn    :Pointer16;
    MudIndOff   :Pointer16;
  END;
  PPROTLWRDISP  =^PROTLWRDISP;
  PROTLWRDISP=RECORD
    PldCCp      :Pointer16;
    PldIFF      :ULong;
    PldReqConfirm :Pointer16;
    PldXmitConfirm:Pointer16;
    PldRcvLkAhead :Pointer16;
    PldIndComplete:Pointer16;
    PldRcvChain   :Pointer16;
    PldStatInd    :Pointer16;
  END;
  PMACMODULE    =^MACMODULE;
  MACMODULE=RECORD
    {Pointers}
    BaseAddr    :Pointer;         {Linear Base Address}
    BaseAddrPhys:PointerP;        {Physical Base Address}
    IniData     :PMACMODULECONFIG;
    MoreData    :Pointer;
    Protocol    :PMACCOMCHAR;
    {IDC-Entries}
    ReqConfirm  :IDCStruct;
    XmitConfirm :IDCStruct;
    RcvLkAhead  :IDCStruct;
    IndComplete :IDCStruct;
    RcvChain    :IDCStruct;
    StatInd     :IDCStruct;
    {other stuff}
    Protocol16  :Pointer16;
    PIndiRt     :Pointer16;
    IndiRt      :ULong;
    IndicationCnt:Ulong;
    MinLenLkAh  :ULong;
    SetLkAhEn   :Boolean;
    CCT         :MACCOMCHAR;
    MSC         :MACSPECCHAR;
    MCB         :MACMCASTBUF;
    MSS         :MACSPECSTAT;
    MUD         :MACUPRDISP;
    PLD         :PROTLWRDISP;
    MORE        :Byte;  {Label, after that comes additional Data}
  END;
  PMACMASTER    =^MACMASTER;
  MACMASTER=RECORD
    Module      :ARRAY[0..MAC_MAXMODULE-1] OF PMACMODULE;
    GDT         :ARRAY[0..MAC_MAXMODULE-1] OF Selector;
    Physical    :ARRAY[0..MAC_MAXMODULE-1] OF PhysPage;
    Count       :UShort;
  END;

FUNCTION MACInit(MacMaster:PMACMASTER;DrvName:PChar;
                 MaxModule,MoreCnt:ULong):ApiRet;
FUNCTION MACRegister(MacMaster:PMACMASTER):ApiRet;
FUNCTION MACBind(Param:Pointer;Data:PMACMODULE):MACREQRET;
FUNCTION MACXmitChainPrep(Data:PMACMODULE;Param:PMACXMITCHAIN;Buffer:Pointer;Size:Ulong;
                          VAR Len:ULong;Stat:SmallWord):MACREQRET;
FUNCTION MACTransmitConfirm(Data:PMACMODULE;ProtID,Handle,Status:SmallInt):ApiRet;
FUNCTION MACTestFilter(Data:PMACMODULE;Buffer:Pointer):ULong;
FUNCTION MACDefGenReq(Param:Pointer;Data:PMACMODULE;Stat:SmallWord):MACREQRET;
FUNCTION MACSetIndication(Data:PMACMODULE;SetOn:Boolean):MACREQRET;
PROCEDURE MACAdapterCheck(Data:PMACMODULE;CauseCode:ULong);
FUNCTION MACReciveLookahead(Data:PMACMODULE;Buffer:Pointer16;Len:ULong):ApiRet;
FUNCTION MACTransferData(Param:PMACTXDATA;Data:PMACMODULE;Buffer:Pointer;Len:ULong):MACREQRET;
FUNCTION MACIndicationComplete(Data:PMACMODULE):ApiRet;
PROCEDURE MACIndicateResetStatus(Data:PMACMODULE;Start:Boolean;EndStatus:UShort);
PROCEDURE MACIndicateIRQ(Data:PMACMODULE);

IMPLEMENTATION
CONST
  GETPMINFO       = 1;
  REGISTERMODULE  = 2;
  BINDANDSTART    = 3;
  GETPMLINKAGE    = 4;

TYPE
  REQBLK=RECORD
    Opcode    :UShort;
    Status    :UShort;
    Pointer1  :Pointer16;
    Pointer2  :Pointer16;
    Word1     :UShort;
  END;


FUNCTION MACInit(MacMaster:PMACMASTER;DrvName:PChar;
                 MaxModule,MoreCnt:ULong):ApiRet;
CONST
  MSN             = $FFFFFFFF; {MAC Stats None}
VAR
  rc    :ApiRet;
  Parm  :REQBLK;
  Config:PMACMODULECONFIG;
  Key   :PMACKEYWORDENTRY;
  Val   :PMACPARAMENTRY;
  Count :ULong;

BEGIN
  MacMaster^.Count:=0;
  IF MaxModule>MAC_MAXMODULE THEN MaxModule:=MAC_MAXMODULE;
  MemFill(LocToDat32(@Parm),SizeOf(Parm),0);
  Parm.Opcode:=GETPMINFO;
  rc:=DevDevIOCtl('PROTMAN$',$81,$58,
                  LocToDat32(@Parm),SizeOf(Parm),
                  NIL,0);
  IF (rc=0) AND (Parm.Status=0) THEN
  BEGIN
    Config:=PMACMODULECONFIG(DevVirtToLin(Parm.Pointer1));
    WHILE Config <> NIL DO
    BEGIN
      Key:=Addr(Config^.KeyWordEnt);
      WHILE Key <> NIL DO
      BEGIN
        Val:=Addr(Key^.ParamEnt);
        FOR Count:=1 TO Key^.ParamCount DO
        BEGIN
          IF StrPos(Addr(Key^.Keyword),'DRIVERNAME') <> NIL THEN
          BEGIN
            IF (Val^.ParamType=1) AND
               (StrPos(DrvName,Addr(Val^.Param))<>NIL) AND
               (MACMaster^.Count<MaxModule) THEN
            WITH MACMaster^ DO
            BEGIN
              Module[Count]:=DevGetMemPhys(SizeOf(MACMODULE)+MoreCnt,False,Physical[Count]);
              IF Module[Count] <> NIL THEN
              BEGIN
                rc:=DevAllocGDTSelector(GDT[Count],1);
                IF rc=0 THEN
                BEGIN
                  DevLinToGDTSelector(GDT[Count],Module[Count],SizeOf(MACMODULE)+MoreCnt);
                  {Clear Mem}
                  MemFill(Module[Count],SizeOf(MACMODULE)+MoreCnt,0);
                  WITH Module[Count]^ DO
                  {Init Pointers}
                  BEGIN
                    BaseAddr  :=Module[Count];
                    BaseAddrPhys:=Physical[Count].Start;
                    IniData   :=Config;
                    MoreData  :=Addr(MORE);
                    MinLenLkAh:=64;
                    SetLkAhEn :=True;
                    {Set Offsets of Pointers in Tables}
                    CCT.CcSCp.Off :=ULong(Addr(MSC))-ULong(BaseAddr);
                    CCT.CcSSp.Off :=ULong(Addr(MSS))-ULong(BaseAddr);
                    CCT.CcUDp.Off :=ULong(Addr(MUD))-ULong(BaseAddr);
                    CCT.CcLDp.Off :=ULong(Addr(PLD))-ULong(BaseAddr);
                    MUD.MudCCp.Off:=ULong(Addr(CCT))-ULong(BaseAddr);
                    MSC.MCastBuf.Off:=ULong(Addr(MCB))-ULong(BaseAddr);
                    MSC.MscVenAdaptDesc.Off:=ULong(Addr(CCT.CcName))-ULong(BaseAddr);
                    PIndiRt.Off   :=ULong(Addr(IndiRt))-ULong(BaseAddr);
                    PIndiRt.Seg   :=GDT[Count];
                  END;
                  WITH Module[Count]^.CCT DO
                  {Init Common Characterisics}
                  BEGIN
                    CcSize    :=SizeOf(MACCOMCHAR);
                    CcLvlMjr  :=$02;
                    CcLvlMnr  :=$00;
                    CcBindFnc :=$01; {upper binding}
                    CcUPLevel :=$01; {MAC Level}
                    CcUIType  :=$01; {MAC Type}
                    CcLIType  :=$01;
                    CcDataSeg :=GDT[Count]; {Selector to this Seg}
                    CcSysReq  :=GetCallInAddr(MAC_SYSREQ);
                    CcSCp.Seg :=CcDataSeg;
                    CcSSp.Seg :=CcDataSeg;
                    CcUDp.Seg :=CcDataSeg;
                    CcLDp.Seg :=CcDataSeg;
                    StrCopy(CCName,Config^.ModuleName);
                  END;
                  WITH Module[Count]^.MCB DO
                    McbMax:=1; {allow 1 Multicast Address}
                  WITH Module[Count]^.MSC DO
                  {Init specific Characteristics}
                  BEGIN
                    MCastBuf.Seg:=GDT[Count];
                    MscVenAdaptDesc.Seg:=MCastBuf.Seg;
                    MscSize:=SizeOf(MACSPECCHAR);
                    MscStnAdrSz:=6;
                    MscVenCode[0]:=Chr($FF);
                    MscVenCode[1]:=Chr($FF);
                    MscVenCode[2]:=Chr($FF);
                    MscService:=MAC_GDTADRESS_SUPP OR MAC_MULTIDATAXFER;
                    MscTxQDepth:=1;
                    MscMaxNumDataBlks:=10;
                    StrCopy(MscType,'DIX+802.3');
                  END;
                  WITH Module[Count]^.MSS DO
                  {Init specific Stats}
                  BEGIN
                    MssSize   :=SizeOf(MACSPECSTAT);
                    MssDiagDT :=MSN;
                    MssClearDT:=MSN;
                    MssFR     :=MSN;
                    MssRFCRC  :=MSN;
                    MssFRByt  :=MSN;
                    MssRFLack :=MSN;
                    MssFRMC   :=MSN;
                    MssFRBC   :=MSN;
                    MssRFErr  :=MSN;
                    MssRFMax  :=MSN;
                    MssRFMin  :=MSN;
                    MssFRMCByt:=MSN;
                    MssFRBCByt:=MSN;
                    MssRFHW   :=MSN;
                    MssFS     :=MSN;
                    MssFSByt  :=MSN;
                    MssFSMC   :=MSN;
                    MssFSBC   :=MSN;
                    MssFSBCByt:=MSN;
                    MssFSMCByt:=MSN;
                    MssSFTime :=MSN;
                    MssSFHW   :=MSN;
                  END;
                  WITH Module[Count]^.MUD DO
                  {Init upper dispatch Table}
                  BEGIN
                    MudCCp.Seg  :=Module[Count]^.CCT.CcDataSeg;
                    MudGReq     :=GetCallInAddr(MAC_GENREQ);
                    MudXmitChain:=GetCallInAddr(MAC_XMITCHAIN);
                    MudXferData :=GetCallInAddr(MAC_XFERDATA);
                    MudRcvRelease:=GetCallInAddr(MAC_RCVRELEASE);
                    MudIndOn    :=GetCallInAddr(MAC_INDON);
                    MudIndOff   :=GetCallInAddr(MAC_INDOFF);
                  END;
                  Inc(Count);
                END ELSE InfAdd('no GDT Selector available!'+CR+LF,NIL);
              END;
            END;
          END;
          Val:=Ptr(ULong(Val)+SizeOf(MACPARAMENTRY)+Val^.ParamLen-4);
        END;
        Key:=PMACKEYWORDENTRY(DevVirtToLin(Key^.NextEntry));
      END;
      Config:=PMACMODULECONFIG(DevVirtToLin(Config^.NextModule));
    END;
  END ELSE InfAdd('PROTMAN$ not loaded!'+CR+LF,NIL);
  IF MACMaster^.Count>0 THEN MACInit:=0 ELSE MACInit:=2;
END;

FUNCTION MACRegister(MacMaster:PMACMASTER):ApiRet;
VAR
  n     :ULong;
  rc    :ApiRet;
  Parm  :REQBLK;


BEGIN
  MACRegister:=0;
  FOR n:=0 TO MacMaster^.Count-1 DO
  BEGIN
    IF MacMaster^.GDT[n]<>0 THEN
    BEGIN
      MemFill(LocToDat32(@Parm),SizeOf(Parm),0);
      Parm.Opcode  :=REGISTERMODULE;
      Parm.Pointer1:=MacMaster^.Module[n].MUD.MudCCp;
      rc:=DevDevIOCtl('PROTMAN$',$81,$58,
                      LocToDat32(@Parm),SizeOf(Parm),
                      NIL,0);
      IF rc<>0 THEN MACRegister:=rc; {Fail}
    END;
  END;
END;

FUNCTION MACBind(Param:Pointer;Data:PMACMODULE):MACREQRET;
TYPE
  PBINDPARM=^BINDPARM;
  BINDPARM=RECORD
    TargetDS  :Selector;
    Func      :SmallWord; {only 2 = bind supported}
    Pad       :SmallWord;
    TabAddr   :Pointer16; {pointer to pointer to MAC CCT}
    CharTab   :Pointer16; {pointer to protocol CCT}
  END;
VAR
  Dest  :PPointer16;
  LDT   :PPROTLWRDISP;
BEGIN
  MACBind.PSize:=SizeOf(BINDPARM);
  WITH PBINDPARM(Param)^ DO
  BEGIN
    IF (Func=2) AND (Data^.Protocol=NIL) THEN
    BEGIN
      {give protocol pointer to MAC CCT}
      Dest:=DevVirtToLin(TabAddr);
      Dest^:=Data^.MUD.MudCCp;
      {get protocol CCT}
      Data^.Protocol16:=CharTab;
      Data^.Protocol:=DevVirtToLin(CharTab);
      {copy LDT entries}
      WITH Data^ DO
      BEGIN
        MSS.MssStatus:=MSS.MssStatus OR $10; {bound}
        LDT:=DevVirtToLin(Protocol.CcLDp);
        ReqConfirm.PEntry:=LDT^.PldReqConfirm;
        ReqConfirm.EntryS:=Protocol.CcDataSeg;
        XmitConfirm.PEntry:=LDT^.PldXmitConfirm;
        XmitConfirm.EntryS:=Protocol.CcDataSeg;
        RcvLkAhead.PEntry:=LDT^.PldRcvLkAhead;
        RcvLkAhead.EntryS:=Protocol.CcDataSeg;
        IndComplete.PEntry:=LDT^.PldIndComplete;
        IndComplete.EntryS:=Protocol.CcDataSeg;
        RcvChain.PEntry:=LDT^.PldRcvChain;
        RcvChain.EntryS:=Protocol.CcDataSeg;
        StatInd.PEntry:=LDT^.PldStatInd;
        StatInd.EntryS:=Protocol.CcDataSeg;
      END;
      MACBind.RStat:=MAC_SUCCESS;
    END ELSE MACBind.RStat:=MAC_INVALIDFNC;
  END;
END;

FUNCTION MACXmitChainPrep(Data:PMACMODULE;Param:PMACXMITCHAIN;Buffer:Pointer;Size:Ulong;
                          VAR Len:ULong;Stat:SmallWord):MACREQRET;
TYPE
  PMACTXDATACNT=^MACTXDATACNT;
  MACTXDATACNT=RECORD
    TxPtrType :Byte;
    TxResByte :Byte;
    TxDataLen :SmallWord;
    TxDataPtr :PointerP;
  END;
  PMACTXBUFDESC=^MACTXBUFDESC;
  MACTXBUFDESC=RECORD
    TxImmedLen:SmallWord;
    TxImmedPtr:Pointer16;
    TxDataCount:SmallWord;
    Next      :MACTXDATACNT;
  END;
VAR
TxBuffer  :PMACTXBUFDESC;
TxBuffNxt :PMACTXDATACNT;
TxSource  :Pointer;
TxDest    :Pointer;
Count     :ULong;
Status    :SmallWord;
BEGIN
  Status:=Stat;
  Len:=0;
  MACXmitChainPrep.PSize:=SizeOf(MACXMITCHAIN);
  IF (Data^.MSS.MssStatus AND $7)>0 THEN
  BEGIN
    TxBuffer:=PMACTXBUFDESC(DevVirtToLin(Param^.TxBufDesc));
    IF (TxBuffer<>NIL) AND (Buffer<>NIL) AND (Stat<=2) THEN
    WITH TxBuffer^ DO
    BEGIN
      {Clean Mem}
      MemFill(Buffer,Data^.MinLenLkAh,0);
      {Immediate Data}
      IF (TxImmedLen>0) AND (TxImmedPtr.Ptr>0) THEN
      BEGIN
        TxSource:=DevVirtToLin(TxImmedPtr);
        IF TxSource<>NIL THEN
        BEGIN
          TxDest:=Pointer(ULong(Buffer)+Len);
          MemMove(TxSource,TxDest,TxImmedLen);
          Len:=Len+TxImmedLen;
        END
        ELSE  Status:=MAC_INVALIDPAR;
      END;
      {regular Data}
      TxBuffNxt:=Addr(Next);
      FOR Count:=1 TO TxDataCount DO
      BEGIN
        IF (Len+TxBuffNxt^.TxDataLen)<=Size THEN
        WITH TxBuffNxt^ DO
        BEGIN
          IF TxPtrType=0 THEN
            TxSource:=DevMapPhys(TxDataPtr,TxDataLen,FALSE)
          ELSE
            TxSource:=DevVirtToLin(Pointer16(TxDataPtr));
          IF TxSource<>NIL THEN
          BEGIN
            TxDest:=Pointer(ULong(Buffer)+Len);
            MemMove(TxSource,TxDest,TxDataLen);
            Len:=Len+TxDataLen;
          END
          ELSE  Status:=MAC_INVALIDPAR;
        END
        ELSE
        BEGIN
          Status:=MAC_OUTOFRES;
          Count:=TxDataCount+1;
        END;
        INC(TxBuffNxt);
      END;
    END;
    MACXMitChainPrep.RStat:=Status;
    IF Len < Data^.MinLenLkAh THEN Len:=Data^.MinLenLkAh;
  END ELSE MACXmitChainPrep.RStat:=MAC_HARDERR;
END;

FUNCTION MACTransmitConfirm(Data:PMACMODULE;ProtID,Handle,Status:SmallInt):ApiRet;
TYPE
  MAC_XMITCONFIRMPARM=RECORD
    ProtDS      :SmallInt;
    RetStatus   :SmallInt;
    ReqHandle   :SmallInt;
    MACID       :SmallInt;
    ProtIDPar   :SmallInt;
  END;
VAR
  Param :MAC_XMITCONFIRMPARM;
BEGIN
  WITH Param DO
  BEGIN
    ProtIDPar:=ProtID;
    MACID:=Data^.CCT.CcModuleID;
    ReqHandle:=Handle;
    RetStatus:=Status;
    ProtDS:=Data^.XmitConfirm.EntryS;
  END;
  MACTransmitConfirm:=DevCallDD16(@Data^.XmitConfirm,
                                  LocToDat16(Addr(Param)),
                                  $F100+SizeOf(MAC_XMITCONFIRMPARM));
END;

FUNCTION MACTestFilter(Data:PMACMODULE;Buffer:Pointer):ULong;
VAR
  Brc      :ULong;
  Val1,Val2:PULong;
BEGIN
  MACTestFilter:=MAC_ADDR_NONE;
  Brc:=$FFFFFFFF;
  IF (Data^.MSS.MssFilter AND $2) > 0 THEN
  BEGIN
    {Test for Broadcast}
    Val1:=Buffer;
    Val2:=LocToDat32(@Brc);
    IF Val1^ = Val2^ THEN
    BEGIN
      Val1:=Pointer(ULong(Buffer)+2);
      IF Val1^ = Val2^ THEN MACTestFilter:=MAC_ADDR_BROAD;
    END;
  END;
  IF (Data^.MSS.MssFilter AND $1) > 0 THEN
  BEGIN
    {Test for my Address}
    Val1:=Buffer;
    Val2:=@Data^.MSC.MscCurrStnAdr[0];
    IF Val1^ = Val2^ THEN
    BEGIN
      Val1:=Pointer(ULong(Buffer)+2);
      Val2:=@Data^.MSC.MscCurrStnAdr[2];
      IF Val1^ = Val2^ THEN MACTestFilter:=MAC_ADDR_UNI;
    END;
    {Test for Multicast Address}
    IF Data^.MCB.McbCnt>0 THEN
    BEGIN
      Val1:=Buffer;
      Val2:=@Data^.MCB.mAddr[0];
      IF Val1^ = Val2^ THEN
      BEGIN
        Val1:=Pointer(ULong(Buffer)+2);
        Val2:=@Data^.MCB.mAddr[2];
        IF Val1^ = Val2^ THEN MACTestFilter:=MAC_ADDR_MULTI;
      END;
    END;
  END;
  IF (Data^.MSS.MssFilter AND $4) > 0 THEN
    MACTestFilter:=MAC_ADDR_PROM; {promiscuous mode}
END;

FUNCTION MACDefGenReq(Param:Pointer;Data:PMACMODULE;Stat:SmallWord):MACREQRET;
BEGIN
  MACDefGenReq.PSize:=SizeOf(MACREQPARAM);
  WITH PMACREQPARM(Param)^ DO
  BEGIN
    IF Stat<=2 THEN
    CASE Func OF
      MAC_GDIAGNOSE:
      {no action, status as specified by caller or HW_ERR}
        BEGIN
          IF (Data^.MSS.MssStatus AND $7)>0
          THEN MACDefGenReq.RStat:=Stat
          ELSE MACDefGenReq.RStat:=MAC_HARDERR;
        END;
      MAC_GOPEN,MAC_GRESET:
      {no action, status as specified by caller or HW_ERR}
        BEGIN
          IF (Data^.MSS.MssStatus AND $7)>0
            THEN MACDefGenReq.RStat:=Stat
            ELSE MACDefGenReq.RStat:=MAC_NOHARDWARE;
        END;
      MAC_ADDMC: {set Multicast Addr}
        WITH Data^.MCB DO
        BEGIN
          IF McbCnt < McbMax THEN
          BEGIN
            MACDefGenReq.RStat:=Stat;
            Inc(McbCnt);
            MemMove(DevVirtToLin(Pointer16(Param2)),
                    Addr(mAddr),
                    Data^.MSC.MscStnAdrSz);
          END ELSE MACDefGenReq.RStat:=MAC_OUTOFRES;
        END;
      MAC_DELMC: {delete Multicast Addr}
        WITH Data^.MCB DO
        BEGIN
          IF McbCnt>0 THEN
          BEGIN
            MACDefGenReq.RStat:=Stat;
            Dec(McbCnt);
            MemFill(Addr(mAddr),Data^.MSC.MscStnAdrSz,0);
          END ELSE MACDefGenReq.RStat:=MAC_INVALIDPAR;
        END;
      MAC_GERRLOG,MAC_GCLOSE,
      MAC_UPDSTATS,MAC_CLRSTATS,MAC_IRQ,MAC_SETFNCA:
      {no action, status as specified by caller}
        BEGIN
          MACDefGenReq.RStat:=Stat;
        END;
      MAC_GSETSTNA: {set station address}
        BEGIN
          {copy address}
          MemMove(DevVirtToLin(Pointer16(Param2)),
                  Addr(Data^.MSC.MscCurrStnAdr),
                  Data^.MSC.MscStnAdrSz);
          MACDefGenReq.RStat:=Stat;
        END;
      MAC_GSETFILTER: {set packet filter}
        IF (Param1 AND $FFF0)=0 THEN
        WITH Data^ DO
        BEGIN
          MSS.MssFilter:=Param1; {copy packet filter flags}
          IndicationCnt:=0;      {indications on}
          MACDefGenReq.RStat:=Stat;
        END ELSE MACDefGenReq.RStat:=MAC_INVALIDPAR;
      MAC_SETLOOKAH: {set lookahead size}
        IF Param1 <= Data^.MSC.MscMaxFrame THEN
        WITH Data^ DO
        BEGIN
          IF SetLkAhEn THEN
          BEGIN
            SetLkAhEn:=False;
            MinLenLkAh:=Param1;
          END ELSE IF Param1>MinLenLkAh THEN MinLenLkAh:=Param1;
          MACDefGenReq.RStat:=MAC_SUCCESS;
        END ELSE MACDefGenReq.RStat:=MAC_INVALIDPAR;
    END ELSE MACDefGenReq.RStat:=Stat;
  END;
END;

PROCEDURE MACAdapterCheck(Data:PMACMODULE;CauseCode:ULong);
TYPE
  MAC_ADAPCHECK=RECORD
    ProtDS    :SmallInt;
    Func      :SmallInt;
    Indicate  :Pointer16;
    Reason    :SmallInt;
    MACID     :SmallInt;
  END;
VAR
  Param:MAC_ADAPCHECK;
BEGIN
  WITH Param DO
  BEGIN
    MACID:=Data^.CCT.CcModuleID;
    Reason:=CauseCode;
    Indicate:=Data^.PIndiRt;
    Func:=2;
    ProtDS:=Data^.RcvLkAhead.EntryS;
  END;
  Data^.IndiRT:=$FFFFFFFF;
  MACSetIndication(Data,False);
  DevCallDD16(@Data^.StatInd,
              LocToDat16(Addr(Param)),
              $F100+SizeOf(MAC_ADAPCHECK));
  IF Data^.IndiRT=$FFFFFFFF THEN MACSetIndication(Data,True);
END;

FUNCTION MACSetIndication(Data:PMACMODULE;SetOn:Boolean):MACREQRET;
BEGIN
  WITH Data^ DO
  BEGIN
    IF SetOn THEN
    BEGIN
      IF IndicationCnt>0 THEN DEC(IndicationCnt);
    END ELSE
    BEGIN
      IF IndicationCnt<$FF THEN INC(IndicationCnt);
    END;
  END;
  MACSetIndication.ApiRet:=2; {OK-1 Word Param}
END;

FUNCTION MACReciveLookahead(Data:PMACMODULE;Buffer:Pointer16;Len:ULong):ApiRet;
TYPE
  MAC_RXLOOKAHEAD=RECORD
    ProtDS      :SmallInt;
    Indicate    :Pointer16;
    PBuffer     :Pointer16;
    BytesAvail  :SmallInt;
    FrameSize   :SmallInt;
    MACID       :SmallInt;
  END;
VAR
  Param :MAC_RXLOOKAHEAD;
BEGIN
  WITH Param DO
  BEGIN
    MACID:=Data^.CCT.CcModuleID;
    FrameSize:=Len;
    BytesAvail:=Len;
    PBuffer:=Buffer;
    Indicate:=Data^.PIndiRt;
    ProtDS:=Data^.RcvLkAhead.EntryS;
  END;
  Data^.IndiRT:=$FFFFFFFF;
  MACSetIndication(Data,False);
  MACReciveLookahead:=DevCallDD16(@Data^.RcvLkAhead,
                                  LocToDat16(Addr(Param)),
                                  $F100+SizeOf(MAC_RXLOOKAHEAD));
  IF Data^.IndiRT=$FFFFFFFF THEN MACSetIndication(Data,True);
END;

FUNCTION MACTransferData(Param:PMACTXDATA;Data:PMACMODULE;Buffer:Pointer;Len:ULong):MACREQRET;
TYPE
  PMACTXDATACNT=^MACTXDATACNT;
  MACTXDATACNT=RECORD
    TxPtrType :Byte;
    TxResByte :Byte;
    TxDataLen :SmallWord;
    TxDataPtr :PointerP;
  END;
  PMACTXBUFDESC=^MACTXBUFDESC;
  MACTXBUFDESC=RECORD
    TxDataCount:SmallWord;
    Next      :MACTXDATACNT;
  END;
VAR
TxBuffer  :PMACTXBUFDESC;
TxBuffNxt :PMACTXDATACNT;
TxSource  :Pointer;
TxDest    :Pointer;
Offset    :ULong;
Count     :ULong;
PartLen   :ULong;
Copied    :PSmallWord;
Status    :SmallWord;
BEGIN
  MACTransferData.PSize:=SizeOf(MACTXDATA);
  IF (Data^.MSS.MssStatus AND $7)=$7 THEN
  BEGIN
    TxBuffer:=PMACTXBUFDESC(DevVirtToLin(Param^.TDBufDesc));
    Copied:=DevVirtToLin(Param^.PBytesCopied);
    Copied^:=0;
    Offset:=Param^.FrameOffset;
    IF (TxBuffer<>NIL) AND (Buffer<>NIL) THEN
    WITH TxBuffer^ DO
    BEGIN
      TxBuffNxt:=Addr(Next);
      FOR Count:=1 TO TxDataCount DO
      WITH TxBuffNxt^ DO
      BEGIN
        IF TxPtrType=0 THEN
          TxDest:=DevMapPhys(TxDataPtr,TxDataLen,FALSE)
        ELSE
          TxDest:=DevVirtToLin(Pointer16(TxDataPtr));
        IF TxDest<>NIL THEN
        BEGIN
          TxSource:=Pointer(ULong(Buffer)+Offset);
          PartLen:=Len-Offset-Copied^;
          IF PartLen>0 THEN
          BEGIN
            IF PartLen>TxDataLen THEN PartLen:=TxDataLen;
            MemMove(TxSource,TxDest,PartLen);
            Copied^:=Copied^+PartLen;
          END;
        END;
        INC(TxBuffNxt);
      END;
    END;
    MACTransferData.RStat:=MAC_SUCCESS;
    IF Len < Data^.MinLenLkAh THEN Len:=Data^.MinLenLkAh;
  END ELSE
  BEGIN
    MACAdapterCheck(Data,$8000);
    MACTransferData.RStat:=MAC_INVALIDFNC;
  END;
END;


FUNCTION MACIndicationComplete(Data:PMACMODULE):ApiRet;
TYPE
  MAC_INDCOMPLETE=RECORD
    ProtDS      :SmallInt;
    MACID       :SmallInt;
  END;
VAR
  Param :MAC_INDCOMPLETE;
BEGIN
  WITH Param DO
  BEGIN
    MACID:=Data^.CCT.CcModuleID;
    ProtDS:=Data^.IndComplete.EntryS;
  END;
  MACIndicationComplete:=DevCallDD16(@Data^.IndComplete,
                                    LocToDat16(Addr(Param)),
                                    $F100+SizeOf(MAC_INDCOMPLETE));
END;

PROCEDURE MACIndicateResetStatus(Data:PMACMODULE;Start:Boolean;EndStatus:UShort);
TYPE
  MAC_RESSTAT=RECORD
    ProtDS    :SmallInt;
    Func      :SmallInt;
    Indicate  :Pointer16;
    Status    :SmallInt;
    MACID     :SmallInt;
  END;
VAR
  Param:MAC_RESSTAT;
BEGIN
  WITH Param DO
  BEGIN
    MACID:=Data^.CCT.CcModuleID;
    Indicate:=Data^.PIndiRt;
    ProtDS:=Data^.RcvLkAhead.EntryS;
    IF Start THEN
    BEGIN
      Status:=0;
      Func:=3;
    END ELSE BEGIN
      Status:=EndStatus;
      Func:=5;
    END;
  END;
  Data^.IndiRT:=$FFFFFFFF;
  MACSetIndication(Data,False);
  DevCallDD16(@Data^.StatInd,
              LocToDat16(Addr(Param)),
              $F100+SizeOf(MAC_RESSTAT));
  IF Data^.IndiRT=$FFFFFFFF THEN MACSetIndication(Data,True);
END;

PROCEDURE MACIndicateIRQ(Data:PMACMODULE);
TYPE
  MAC_IRQ=RECORD
    ProtDS    :SmallInt;
    Func      :SmallInt;
    Indicate  :Pointer16;
    Reserved  :SmallInt;
    MACID     :SmallInt;
  END;
VAR
  Param:MAC_IRQ;
BEGIN
  WITH Param DO
  BEGIN
    MACID:=Data^.CCT.CcModuleID;
    Indicate:=Data^.PIndiRt;
    ProtDS:=Data^.RcvLkAhead.EntryS;
    Reserved:=0;
    Func:=4;
  END;
  Data^.IndiRT:=$FFFFFFFF;
  MACSetIndication(Data,False);
  DevCallDD16(@Data^.StatInd,
              LocToDat16(Addr(Param)),
              $F100+SizeOf(MAC_IRQ));
  IF Data^.IndiRT=$FFFFFFFF THEN MACSetIndication(Data,True);
END;


END.
