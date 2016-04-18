  ;
  ; DRVSTUB.ASM
  ;
  ; Assembler Stub for writing character device drivers with Virtual Pascal
  ; Assemble with ALP
  ;
  ;

  MaxCmd  EQU     01Fh            ; maximum allowed command code

  stdin   EQU     0               ; standard device handles
  stdout  EQU     1
  stderr  EQU     2

  cr      EQU     0dh             ; ASCII carriage return
  lf      EQU     0ah             ; ASCII linefeed

          EXTRN   Dos16Write:FAR
          EXTRN   Dos32SetPriority:FAR
          EXTRN   Dos32Exit:FAR
          EXTRN   Dos32CreateThread:FAR
          EXTRN   DRVSTUB1@Strategy:FAR
          EXTRN   DRVSTUB1@IDCEntry:FAR
          EXTRN   DRVSTUB1@Context:FAR
          EXTRN   DRVSTUB1@Interrupt:FAR
          EXTRN   DRVSTUB1@MACEntry:FAR

.686P
.MMX
  DGROUP1  GROUP  _DATA
  _DATA   SEGMENT PARA PUBLIC USE32 '16DATA'

                                  ; device driver header...
  Header  DD      -1              ; link to next device driver
  H_Attr  DW      08980h          ; device attribute word
          DW      LOWWORD Strat   ; Strategy entry point
  IDC_Ent DW      0               ; IDC entry point
  H_Name  DB      'õõõõõõõõ'      ; logical device name
          DB      8 dup (0)       ; reserved
  H_Cap   DD      0013h           ; Capabilities Type 3

          ALIGN 16
  DevHlp  DD      ?               ; DevHlp entry point
  PRPCode DD      0h              ; 32-Bit Linear PrepGDT Entry Point
  GateR0  DF      0h              ; 48-Bit Pointer To CallGate PrepGDT
          DW      0h
  STRCode DF      0h              ; 48-Bit Linear Strategy Entry Point
          DW      0h
  IRQCode DF      0h              ; 48-Bit Linear IRQ Entry Point
          DW      0h
  IDCCode DF      0h              ; 48-Bit Linear IDC Entry Point
          DW      0h
  MACCode DF      0h              ; 48-Bit Linear MAC Entry Point
          DW   7777h              ; reserved

          ALIGN 16
  DataLin DD      0h              ; FLAT Pointer to start of DATA32
  DataLen DD      0h              ; Length of DATA32
PTKSSBase DD      0h              ; Pointer to TKSSBase KernelVar
   CtxReq DD      0h              ; Request Level for CtxHook
          DD      0h              ; Request Level for CtxThread
          DD      0h              ; secondary
   CtxSem DD      0h              ; Lock for inner CtxHook
          DD      0h              ; Lock for inner CtxThread
          DD      0h              ; secondary
CtxSemP16 DD      0h              ; Pointer 16 to CtxSem
          DD      0h              ; for inner CtxThread
          DD      0h              ; secondary
   CtxRun DD      0h              ; Inner CtxHook is running
          DD      0h              ; Inner CtxThread is running
          DD      0h              ; secondary
   CtxHan DD      0h              ; Handle CtxHook
          DD      0h              ; CtxThreadID
          DD      0h              ; secondary;
  CtxMask DD      0h              ; Hook Mask, set on INIT
          DD      0h              ; HookThread Ring3 EntyPoint
          DD      0h              ; secondary;
 GDT_Base DF      0h              ; Buffer for GDT-Register
          DW      0h              ; Lock for GDT_Base
IRQStruct DW      14              ; Number of bytes in structure, including itself.
IRQ_flags DW      0h              ; IRQ-Routine does not change IRQ-Stats.
IRQ_num   DW      10h             ; IRQ of interrupt handler that is being used
IRQ_SkCLI DW      011FFh          ; bytes of stack used when interrupts disabled.
IRQ_SkSTI DW      011FFh          ; bytes of stack used when interrupts enabled.
IRQ_SkEOI DW      128             ; bytes of stack used after EOI.
IRQ_Nest  DW      10              ; Maximum number of IRQ's pending

                                  ; Strategy routine dispatch table
          ALIGN 16                ; for request packet command code...
  Dispch  DW      LOWWORD Init            ;00 0  = initialize driver
          DW      LOWWORD CallStr         ;01 1  = media check
          DW      LOWWORD CallStr         ;02 2  = build BIOS parameter block
          DW      LOWWORD Error           ;03 3  = not used
          DW      LOWWORD DosIO           ;04 4  = read from device
          DW      LOWWORD CallStr         ;05 5  = nondestructive read
          DW      LOWWORD CallStr         ;06 6  = return input status
          DW      LOWWORD CallStr         ;07 7  = flush device input buffers
          DW      LOWWORD DosIO           ;08 8  = write to device
          DW      LOWWORD DosIO           ;09 9  = write with verify
          DW      LOWWORD CallStr         ;0A 10 = return output status
          DW      LOWWORD CallStr         ;0B 11 = flush output buffers
          DW      LOWWORD Error           ;0C 12 = not used
          DW      LOWWORD CallStr         ;0D 13 = device open
          DW      LOWWORD CallStr         ;0E 14 = device close
          DW      LOWWORD CallStr         ;0F 15 = removable media
          DW      LOWWORD IOCtl           ;10 16 = generic IOCTL
          DW      LOWWORD CallStr         ;11 17 = reset media
          DW      LOWWORD CallStr         ;12 18 = get logical drive
          DW      LOWWORD CallStr         ;13 19 = set logical drive
          DW      LOWWORD CallStr         ;14 20 = deinstall
          DW      LOWWORD CallStr         ;15 21 = not used
          DW      LOWWORD CallStr         ;16 22 = partitionable fixed disks
          DW      LOWWORD CallStr         ;17 23 = get fixed disk unit map
          DW      LOWWORD Error           ;18 24 = not used
          DW      LOWWORD Error           ;19 25 = not used
          DW      LOWWORD Error           ;1A 26 = not used
          DW      LOWWORD Init            ;1B 27 = basedev init
          DW      LOWWORD CallStr         ;1C 28 = shutdown
          DW      LOWWORD CallStr         ;1D 29 = get driver capabilities
          DW      LOWWORD Error           ;1E 30 = not used
          DW      LOWWORD CallStr         ;1F 31 = init complete
  DosCall DW      LOWWORD GetCallIn       ;00 0  = GetCallInAddr
          DW      LOWWORD ArmCtxHook      ;01 1  = ArmContextHook
          DW      LOWWORD DosNI           ;02 2  = Empty
          DW      LOWWORD DosNI           ;03 3  = Empty
          DW      LOWWORD DosNI           ;04 4  = Empty
          DW      LOWWORD DosNI           ;05 5  = Empty
          DW      LOWWORD DosDevIOCtl     ;06 6  = DosDevIOCtl
          DW      LOWWORD Memory          ;07 23 = Memory Routines
          DW      LOWWORD ReqQueue        ;08 24 = RequQueue
          DW      LOWWORD DevDone         ;09 25 = DevDone
          DW      LOWWORD SetIRQ          ;0A 26 = IRQ
          DW      LOWWORD SetTimer        ;0B 27 = Timer
          DW      LOWWORD CallDDD16       ;0C 28 = CallDDD16
          DW      LOWWORD CallDDD32       ;0D 29 = CallDDD32
          DW      LOWWORD AttachDD        ;0E 30 = AttachDD
          DW      LOWWORD SetOptions      ;0F 31 = SetOptions
  infoid  DW      0h                      ;message table
  cnt     DW      1h
  info    DD      0h                      ;Pointer to InfoBuffer
  wlen    DW      0h                      ;Buffer for DosWrite w.Errormessage
  abuff   DD      0h                      ;Buffer for RESRGM-Call Ring 3
LockCS    DD      ?                       ; save Buffer for Lock-Handle
LockDS    DD      ?
  errm    DB      cr,lf,lf
          DB      'Error in DevHlp while loading Device Driver'
          DB      cr,lf
  errm_len equ $-errm

  _DATA   ENDS

  _TEXT   SEGMENT PARA PUBLIC USE16 '16CODE'
          ASSUME  CS:_TEXT,DS:DGROUP1,ES:NOTHING

;################################################################
;
; 16-BIT HELPER ROUTINES
;
;################################################################

  ; Get Info about Segment.
  ; IN:   AX=Selector; GS=FLAT Data Segment
  ; OUT:  ECX=linear Base; EDX=Size
  ; USES: none
  ; If we have a GDT Selector, read it from Mem, else use DevHlp
          ALIGN 16
GetSelInf PROC    NEAR
          MOV     DX,AX             ; check for 0-pointer
          AND     DX,0FFF8h
          CMP     DX,0h
          JE      Err
          MOV     DX,SEG FLAT:CODE32
          VERR    DX                ; test, if we are in Ring 0
          JNZ     LDT
          TEST    AX,0004h          ; test if LDT/GDT
          JNZ     LDT
          PUSH    ESI               ; save ESI
          MOVZX   ESI,AX            ; get Selector in ESI
          MOV     EDX,OFFSET GDT_Base ; load GDT regs at GDT_Base
          MOV     CX,-1             ; acquire lock
    WAIT: XCHG    CX,[EDX+6]
          OR      CX,CX
          JNZ     WAIT
          SGDT    [EDX]
          AND     ESI,0000FFF8h     ; mask RPL (for security)
          ADD     ESI,[EDX+2]       ; build address of Selector in GDT
          MOV     WORD PTR [EDX+6],0; free lock
          MOV     EDX,GS:[ESI+2]    ; load lower Base in EDX
          AND     EDX,000FFFFFFh
          MOV     ECX,GS:[ESI+4]    ; load higher Base in ECX
          AND     ECX,0FF000000h
          ADD     ECX,EDX           ; Base is now in ECX
          MOVZX   EDX,AX
          LSL     EDX,EDX           ; Size is in EDX
          POP     ESI
          CLC                       ; signal no Error
          RET
          ALIGN 8
     LDT: MOV     DL,05Dh         ; function GetDescInfo
          CALL    [DevHlp]        ; Call DevHelper,Result: ES starts at ECX
          RET
          ALIGN 8
     Err: XOR     ECX,ECX
          XOR     EDX,EDX
          STC
GetSelInf ENDP

  ; This is to find the selector to a given FLAT address.
  ; Input:  EAX=FLAT Address, GS=FLAT Data Segment
  ; Output: ES:AX = VirtualAddress
  ; Uses:   none
  ; can convert only data on data or stack segments!
  ; If data cannot be localized, carry ist set and a null-pointer is returned
          ALIGN 16
LinToVirt PROC
          PUSH    EBX               ; save EBX
          CMP     EAX, 0h           ; NIL?
          JE      NotFound
          MOV     EBX,EAX           ; save EAX in EBX for stack-check
          CMP     EAX,[DataLin]     ; EAX < Start of DATA32?
          JL      Stack             ; if yes - test stack
          SUB     EAX,[DataLin]     ; get offset
          CMP     EAX,[DataLen]     ; offset > segment size?
          JGE     Stack             ; if yes - test stack
          CMP     EAX,00000FFFFh    ; offset <= 16 bit?
          JA      NotFound          ; cannot be found if not in first 64kB
          MOV     BX,SEG DATA32     ; get 16-bit selector of data32 in ES
          MOV     ES,BX
          POP     EBX               ; get back EBX
          CLC                       ; clear C
          RET
          ALIGN 8
   Stack: MOV     EAX,EBX           ; get clean copy of lin address
          MOV     EBX,PTKSSBase     ; get FLAT Pointer to TKSSBase
          CMP     EAX,GS:[EBX]      ; EAX < Start of STACK?
          JL      NotFound          ; if yes - sorry
          SUB     EAX,GS:[EBX]      ; get offset
          JS      NotFound          ; negative?
          CMP     EAX,00000FFFFh    ; offset <= 16 bit?
          JA      NotFound          ; cannot be found if not in first 64kB
          MOV     BX,SS             ; get 16-bit selector of Stack in ES
          MOV     ES,BX
          POP     EBX               ; get back EBX
          CLC                       ; clear C
          RET
          ALIGN 8
NotFound: POP     EBX               ; get back EBX
          XOR     EAX,EAX
          MOV     ES,AX
          STC
          RET
LinToVirt ENDP

;########################################################
;
;       DOSCALLS
;
;########################################################


  ; This is 16-bit part of dos function calls.
  ; IMPORTANT!
  ; If using DI to get params DI must be loeded from stack!!!!
  ;
          ALIGN 16
    DosNI PROC    NEAR
          MOV     EAX,0001h        ; invalid Function
          RET
    DosNI ENDP

          ALIGN 16
DosDevIOCtl PROC NEAR
          PUSH    FS
          ENTER   0h,0h
          CALL    LinToVirt        ; pointer to data returned
          PUSH    ES
          PUSH    AX
          PUSH    CX                ; data len
          MOV     EAX,EBX
          CALL    LinToVirt        ; pointer to param returned
          PUSH    ES
          PUSH    AX
          SHR     ECX,010h
          PUSH    CX                ; param len
          PUSH    SI                ; function
          SHR     ESI,010h
          PUSH    SI                ; category
          MOV     AX,070h           ; SAS segment
          MOV     FS,AX
          MOV     EAX,FS:[0]        ; check if 'SAS '
          SUB     EAX,020534153h
          JNZ     NEAR PTR Err      ; not SAS-Segment?
          MOV     SI,FS:[0Ah]       ; get offset to dd_data
          MOV     SI,FS:[SI]        ; get offset to first driver
          MOV     DI,SS:[EBP+6]     ; get back DI, over RET near and FS
          MOV     EBX,GS:[EDI]      ; get first 4 bytes of name
          MOV     ECX,GS:[EDI+4]    ; get last 4 bytes of name
          JMP     Compare
          ALIGN 8
    Loop: MOV     EAX,FS:[SI]       ; test next pointer
          CMP     EAX,0FFFFFFFFh
          JE      NEAR PTR Err      ; not found
          LFS     SI,FS:[SI]
 Compare: MOV     EAX,FS:[SI+0Ah]   ; compare first 4 bytes of name
          SUB     EAX,EBX
          JNZ     Loop
          MOV     EAX,FS:[SI+0Eh]   ; compare last 4 bytes of name
          SUB     EAX,ECX
          JNZ     Loop
          MOV     EDX,0Dh           ; alloc request packet with wait
          CALL    [DevHlp]
          JB      NEAR PTR Err
          MOV     DI,BX
          MOV     EAX,010001Ch      ; set Len and Command
          MOV     ES:[DI],EAX
          XOR     EAX,EAX
          MOV     ES:[DI+4],EAX     ; clear struct 3-13
          MOV     ES:[DI+8],EAX
          MOV     ES:[DI+12],EAX
          POP     AX
          MOV     ES:[DI+13],AL     ; category
          POP     AX
          MOV     ES:[DI+14],AL     ; function
          POP     AX
          MOV     ES:[DI+25],AX     ; param len
          POP     EAX
          MOV     ES:[DI+15],EAX    ; param addr
          POP     AX
          MOV     ES:[DI+27],AX     ; data len
          POP     EAX
          MOV     ES:[DI+19],EAX    ; data buffer
          MOV     EAX,FS:[SI+020h]  ; get dev caps
          AND     EAX,01            ; test if ioctl2
          JNZ     IoCtl2
          MOV     AL,019h
          MOV     ES:[DI],AL        ; correct size to 25 if ioctl1
  IoCtl2: PUSH    DS                ; save DS
          PUSH    CS                ; push CS on stack
          PUSH    OFFSET Return     ; push offset for return on stack
          MOV     AX,FS:[SI+014h]   ; get driver DS
          MOV     DS,AX
          MOV     AX,FS:[SI+012h]   ; get driver CS
          PUSH    AX
          MOV     AX,FS:[SI+6]      ; get strat entry
          PUSH    AX
          RETF
          ALIGN 8
  Return: POP     DS
          MOV     CX,ES:[BX+3]      ; get status
          CMP     CX,0100h          ; ok?
          JZ      Ok
          ADD     AX,CX
      Ok: MOV     DX,0Eh            ; free req packet
          CALL    [DevHlp]
          MOVZX   EAX,AX            ; result AX->EAX
          MOV     SP,BP
          POP BP
          POP     FS
          RET
          ALIGN 8
     Err: MOV     EAX,02h           ; set Error
          MOV     SP,BP
          POP BP
          POP     FS
          RET
DosDevIOCtl ENDP



;###########################################################
;
;         DEVICEHELPER AND DOSCALLS DISPATCHER
;
;###########################################################

  ; This is 16-bit part of device-helper function calls and
  ; doscalls. DosCalls are recongnized when DL=FFh
  ; DevHelp Functions are called through Register Params.
  ; These are directly driven by Wrappers in PASCAL.
  ; Only EAX comes from Stack, cause we need it to get
  ; 16-bit DS back.
          ALIGN 16
 DevHlp16 PROC    FAR             ; 16-bit part for calling devhlp
          PUSH    SEG _DATA       ; get selector for _DATA
          POP     DS
          CMP     DL,0FFh
          JE      CallDos
          CALL    [DevHlp]        ; call devhlp
          DB      066h            ; opcodes for RETF32
          DB      0CBh            ; (don't know how to tell alp)
          RET
          ALIGN 16
 CallDos: SHR     DX,08h
          AND     DX,0000Fh
          SHL     DX,01h
          PUSH    DI
          MOV     DI,DX
          CALL    WORD PTR [DI+DosCall] ; call appropriate
          POP     DI
          DB      066h            ; opcodes for RETF32
          DB      0CBh            ; (don't know how to tell alp)
          RET
 DevHlp16 ENDP

          ALIGN 16
GetCallIn PROC    NEAR
          MOV     AX,SEG _TEXT
          SHL     EAX,010h
          CMP     CX,0
          JA      CallIn1
          MOV     AX,OFFSET MACCall0
          RET
          ALIGN 8
 CallIn1: CMP     CX,1
          JA      CallIn2
          MOV     AX,OFFSET MACCall1
          RET
          ALIGN 8
 CallIn2: CMP     CX,2
          JA      CallIn3
          MOV     AX,OFFSET MACCall2
          RET
          ALIGN 8
 CallIn3: CMP     CX,3
          JA      CallIn4
          MOV     AX,OFFSET MACCall3
          RET
          ALIGN 8
 CallIn4: CMP     CX,4
          JA      CallIn5
          MOV     AX,OFFSET MACCall4
          RET
          ALIGN 8
 CallIn5: CMP     CX,5
          JA      CallIn6
          MOV     AX,OFFSET MACCall5
          RET
          ALIGN 8
 CallIn6: MOV     AX,OFFSET MACCall6
          RET
GetCallIn ENDP

          ALIGN 16
ArmCtxHook PROC NEAR
          AND     EBX,[CtxMask]  ; test if Hook available
          JNZ     Found
          MOV     AX,2            ; not available
          JMP     NEAR PTR Err
          ALIGN 8
   Found: MOV     EDX,EBX         ; copy Mask
          AND     EDX,01h         ; test if ContextHook
          JZ      NotCtxHk
          MOV     EDX,[CtxReq]    ; get already set requests
          NOT     EDX             ; mask already set requests
          AND     EDX,EAX
          JZ      Leave           ; if nothing more to do exit
          LOCK    OR [CtxReq],EAX
          CMP     [CtxRun],1      ; already running?
          JE      Leave
          MOV     EBX,[CtxHan]    ; get Handle
          MOV     DX,065h
          Call    [DevHlp]
          JB      Err
          JMP     Leave
          ALIGN 8
 NotCtxHk:MOV     EDX,EBX
          AND     EDX,02h         ; test if ContextThread 1
          JNZ     CT
          MOV     EDX,EBX
          AND     EDX,04h         ; test if ContextThread 2
          JNZ     CT
          MOV     AX,2
          JMP     Err
          ALIGN 8
      CT: SHL     DX,1
          MOV     DI,DX
          MOV     EDX,[CtxReq+DI] ; get already set requests
          NOT     EDX             ; mask already set requests
          AND     EDX,EAX
          JZ      Leave           ; if nothing more to do exit
          LOCK    OR [CtxReq+DI],EAX
          CMP     [CtxRun+DI],1      ; already running?
          JE      Leave
          MOV     EAX,[CtxSemP16+DI] ; get SemPointer
          MOVZX   EBX,AX
          SHR     EAX,010h
          MOV     DX,07h             ; Sem Clear
          Call    [DevHlp]
          JB      Err
    Leave:XOR     EAX,EAX
    Err:  MOVZX   EAX,AX
          RET
ArmCtxHook ENDP

          ALIGN 16
   Memory PROC    NEAR
          CMP     BX,10
          JAE     ReqPkt          ; RequestPacket Functions
          CMP     BX,1
          JA      PhysToLin
          CALL    LinToVirt       ; convert to Virt
          JB      Err
          CMP     BX,0
          JNZ     NoFlToVirt
          PUSH    ES              ; return converted
          PUSH    AX
          POP     EAX
          RET

          ALIGN 8
NoFlToVirt: MOVZX   EDI,AX        ; alloc GDT Selector
          MOV     DX,02Dh
          CALL    [DevHlp]
          JB      Err
          XOR     EAX,EAX         ; ret OK
          RET
          ALIGN 8
      Err:ADD     AX,BX           ; make nonzero return if not convert
          RET

          ALIGN 16
PhysToLin:MOVZX   EBX,AX
          SHR     EAX,010h
          PUSH    ES
          MOV     DX,0115h        ;PhysToVirt
          CALL    [DevHlp]
          JB      MemErr
          MOV     AX,ES
          POP     ES
          CALL    GetSelInf
          MOVZX   EAX,DI
          ADD     EAX,ECX
          RET
          ALIGN 8
   MemErr:POP     ES
          XOR     EAX,EAX
          RET

          ALIGN 16
   ReqPkt:CMP     BX,11
          JA      FreeReq
          SUB     BX,10           ; 10=wait, 11=nowait
          MOV     DH,BL
          MOV     DL,0Dh          ; Alloc RequestPacket
          PUSH    ES
          CALL    [DevHlp]
          JB      RqErr
          MOV     AX,ES
          POP     ES
          SHL     EAX,010h
          MOV     AX,BX
          RET
          ALIGN 8
   RqErr: POP     ES
          XOR     EAX,EAX
          RET
          ALIGN 16
  FreeReq:MOV     BX,AX
          SHR     EAX,010h
          PUSH    ES
          MOV     ES,AX
          MOV     DX,0Eh
          CALL    [DevHlp]
          POP     ES
          RET
   Memory ENDP

          ALIGN 16
 ReqQueue PROC    NEAR
          CALL    LinToVirt       ; get queue-pointer
          JB      NEAR PTR Err
          MOVZX   ESI,AX
          PUSH    FS
          PUSH    DS              ; copy DS to FS
          POP     FS
          PUSH    ES
          POP     DS              ; we need queue-seg in DS
          CMP     ECX,0           ; test if push
          JNE     Pull
          MOV     ECX,EBX         ; get packet seg to ES
          SHR     ECX,010h
          MOVZX   EBX,BX
          MOV     ES,CX
          MOV     DWORD PTR ES:[BX+09h],00h ; make shure, QueueLink is 0!
          MOV     DX,09h          ; push req packet
          CALL    FS:[DevHlp]
          POP     FS
          RET
          ALIGN 16
    Pull: CMP     ECX,1
          JNE     PullPart        ; test if ordinary pull
          MOV     DX,0Ah          ; pull request packet
          CALL    FS:[DevHlp]
          POP     FS
          JB      Err
          MOV     AX,ES
          SHL     EAX,010h
          MOV     AX,BX           ; ret pointer16
          RET
          ALIGN 16
PullPart: MOV     ECX,EBX         ; get packet seg to ES
          SHR     ECX,010h
          MOVZX   EBX,BX
          MOV     ES,CX
          MOV     DX,0Bh          ; push req packet
          CALL    FS:[DevHlp]
          POP     FS
          JB      Err
          MOV     AX,ES
          SHL     EAX,010h
          MOV     AX,BX           ; ret pointer16
          RET
          ALIGN 8
     Err: XOR     EAX,EAX
          RET
 ReqQueue ENDP

          ALIGN 16
  DevDone PROC    NEAR            ; signal done
          MOV     ECX,EBX
          SHR     ECX,010h
          MOV     ES,CX
          MOV     DX,01h
          CALL    [DevHlp]
          RET
  DevDone ENDP

          ALIGN 16
   SetIRQ PROC    NEAR            ; set/unset IRQ
          CMP     AX,0h
          JE      Unset
          MOV     AX,OFFSET Intr  ; store Code Offset
          MOV     DS:[IRQ_num],BX ; store IRQ number
          MOV     DX,CX           ; copy shared flag
          MOV     DL,01Bh
          CALL    [DevHlp]
          JB      Err
          CMP     DH,1h           ; if shared, don't register stack usage
          JE      OK
          MOV     BX,OFFSET IRQStruct
          MOV     DX,003Ah
          CALL    [DevHlp]        ; register stack usage
          JNB     OK
   Err:   MOVZX   EAX,AX
          RET
          ALIGN 8
   Unset: MOV     BX,DS:[IRQ_num] ; get IRQ
          MOV     DX,01Ch
          CALL    [DevHlp]
          JNB     OK
          MOV     EAX,01h
          RET
          ALIGN 8
      OK: XOR     EAX,EAX
          RET
   SetIRQ ENDP

          ALIGN 16
 SetTimer PROC    NEAR            ; set/unset timer hook
          CMP     AX,0h           ; check if unset request
          JE      Unset
          MOV     AX,OFFSET Timer
          MOV     DL,033h         ; TickCount
          CALL    [DevHlp]
          JNB     OK
          MOVZX   EAX,AX
          RET
          ALIGN 8
   Unset: MOV     AX,OFFSET Timer
          MOV     DL,01Eh         ; Reset Timer
          CALL    [DevHlp]
          JNB     OK
          MOVZX   EAX,AX
          RET
          ALIGN 8
      OK: XOR     EAX,EAX
          RET
 SetTimer ENDP

          ALIGN 16
 AttachDD PROC    NEAR
          CALL    LinToVirt       ; get pointer to string
          JB      Err
          MOV     BX,AX
          MOV     EAX,ECX
          CALL    LinToVirt       ; get pointer to structure
          JB      Err
          MOV     DI,AX
          PUSH    DS              ; change DS to DATA32
          PUSH    FS
          MOV     AX,DS
          MOV     FS,AX           ; change FS to _DATA
          MOV     AX,ES
          MOV     DS,AX
          MOV     DL,02Ah
          CALL    FS:[DevHlp]     ; call AttachDD via FS
          POP     FS
          POP     DS
          JB      Err
          XOR     EAX,EAX         ; OK
          RET
          ALIGN 8
     Err: MOV     EAX,0073h       ; illegal address
          RET
 AttachDD ENDP

          ALIGN 16
CallDDD32 PROC    NEAR
          CALL    LinToVirt       ; get 16:16 pointer
          MOV     CX,ES           ; copy to CX:BX
          MOV     BX,AX
          CALL    CallDDD16
          RET
CallDDD32 ENDP

          ALIGN 16
CallDDD16 PROC    NEAR
          MOV     ES,CX           ; get param seg
          PUSH    GS              ; save segs
          PUSH    FS
          PUSH    DS
          MOV     EAX,GS:[ESI+6]  ; IDC entry point
          CMP     EAX,0h          ; NIL?
          JZ      NEAR PTR Failure0
          PUSH    EAX             ; available as [BP+2]
          SHR     EDX,010h        ; get back param
          ENTER   0h,0h
          PUSH    ES              ; push params on stack
          PUSH    BX
          CMP     DX,0FFFFh
          JE      NoParam
          CMP     DH,0F0h
          JE      CallRES
          CMP     DH,0F1h
          JE      CallMAC
          PUSH    DX
 NoParam: MOV     AX,GS:[ESI+10]  ; DD data seg
          MOV     DS,AX
          DD      00029EFFh       ; CALL FAR SS:[BP+2]
          MOV     SP,BP
          POP BP                  ; clenup
          POP     EDX
          POP     DS
          POP     FS
          POP     GS
          MOVZX   EAX,AX
          RET
          ALIGN 8
 CallRES: XOR     DI,DI           ; call resource-manager which
   Loop1: MOV     EAX,ES:[BX+DI]  ; uses this part of stack so
          CMP     EAX,0FFFFFFFFh  ; we need to copy params
          JE      Start           ; search for PAR_ENDS identifier
          ADD     DI,4
          CMP     DI,14h
          JB      Loop1
   Start: SUB     DI,4            ; copy params
          MOV     EAX,ES:[BX+DI]
          PUSH    EAX
          JNZ     Start
          PUSH    CS              ; make call far
          CALL    RESMGR1
          MOV     SP,BP
          POP BP                  ; clenup
          POP     EDX
          POP     DS
          POP     FS
          POP     GS
          MOVZX   EAX,AX
          RET
 CallMAC: AND     DX,00FFh
          MOV     DI,DX
  LoopMC: SUB     DI,2
          MOV     AX,ES:[BX+DI]
          PUSH    AX
          JNZ     LoopMC
          MOV     AX,GS:[ESI+10]  ; DD data seg
          MOV     DS,AX
          DD      00029EFFh       ; CALL FAR SS:[BP+2]
          MOV     SP,BP
          POP BP                  ; clenup
          POP     EDX
          POP     DS
          POP     FS
          POP     GS
          MOVZX   EAX,AX
          RET
          ALIGN 8
Failure0: POP     DS
          POP     FS
          POP     GS
          MOV     EAX,0073h
          RET
CallDDD16 ENDP

          ALIGN 16
  RESMGR1 PROC    NEAR            ; call ressource-manager
          ENTER   4,0             ; does strange things on stack
          LEA     AX,[BP+6]       ; so exact stack struct is necessary
          PUSH    SS
          PUSH    AX
          AND     DX,0FFh         ; mask RM param identifier
          PUSH    DX
          PUSH    CS              ; make far call
          CALL    RESMGR2
          ADD     SP,6
          MOV     SP,BP
          POP BP
          RETF
  RESMGR1 ENDP

          ALIGN 16
  RESMGR2 PROC    NEAR
          ENTER   2,0
          MOV     AX,GS:[ESI+8]   ; check if we can call resrgm dirctly
          VERR    AX              ; ring 0?
          JZ      Ring0
          PUSH    WORD PTR[BP+0Ah]; copy params again...
          PUSH    WORD PTR[BP+8]
          PUSH    WORD PTR[BP+6]
          XOR     EAX,EAX         ; for resrgm kernel places callgate in realDS
          MOV     GS:[ESI],EAX    ; we clear real CS:OFFSET so we get a propriate
          MOV     EAX,GS:[ESI+2]  ; DS:0000 => CallGate:0000 pointer
          MOV     abuff,EAX       ; place to buffer
          CALL    [abuff]         ; call resrgm
  Return: ADD     SP,6
          MOV     SP,BP
          POP BP
          RETF                    ; return
          ALIGN 8
   Ring0: PUSH    WORD PTR[BP+6]  ; direct call Ring 0
          PUSH    CS              ; uses copied params, so here we need
          PUSH    OFFSET Return   ; function number only
          MOV     EAX,GS:[ESI+6]  ; place call on stack
          PUSH    EAX
          RETF                    ; call resrgm
  RESMGR2 ENDP

          ALIGN 16
SetOptions PROC   NEAR
          MOV     [info],EBX      ; set info pointer
          MOV     EDX,GS:[ESI]    ; copy name
          MOV     DWORD PTR DS:[H_Name],EDX
          MOV     EDX,GS:[ESI+4]
          MOV     DWORD PTR DS:[H_Name+4],EDX
          CMP     AX,00h         ; IDC entry?
          JE      NoIDC
          MOV     DX,DS:[H_Attr]  ; set IDC bit in header
          OR      DX,04000h
          MOV     DS:[H_Attr],DX
          CMP     AX,01h         ; is IDC without params?
          JNE     NoIDCNone
          MOV     DX,OFFSET IDC_None
          MOV     DS:[IDC_Ent],DX ; set entry point
          JMP     NoIDC
          ALIGN 8
NoIDCNone:CMP     AX,02h         ; is IDC with param in ES:BX?
          JNE     NoIDCReg
          MOV     DX,OFFSET IDC_Reg
          MOV     DS:[IDC_Ent],DX ; set entry point
          JMP     NoIDC
          ALIGN 8
NoIDCReg: MOV     DX,OFFSET IDC_Stk
          MOV     DS:[IDC_Ent],DX ; set entry point, params via stack
   NoIDC: SHR     EAX,010h
          CMP     AX,0Fh
          JA      NoIRQ
          MOV     DS:[IRQ_num],AX ; store IRQ number
          MOV     BX,OFFSET IRQStruct
          MOV     DX,003Ah
          CALL    [DevHlp]        ; register stack usage
          JB      Err
   NoIRQ: MOV     [CtxMask],ECX   ; save Hook Mask
          AND     ECX,01h         ; mask real ContextHook
          JZ      NoCtxHook
          MOV     AX,OFFSET Context ; get Entry Address
          MOVZX   EAX,AX
          MOV     EBX,0FFFFFFFFh
          MOV     DX,063h         ; allocate context hook
          CALL    [DevHlp]
          JB      Err
          MOV     [CtxHan],EAX
NoCtxHook:XOR     EAX,EAX
          RET
          ALIGN 8
     Err: MOV     EAX,0001h
          RET
SetOptions ENDP


;##############################################################
;
;         KERNEL INTERFACE
;
;##############################################################

  ; Strategy code routine is called by kernel.
  ; ES:BX = request packet address
  ; all Registers saved
          ALIGN 16
  Strat   PROC    FAR             ; Strategy entry point
          MOV     DI,ES:[BX+2]    ; get command code from packet
          AND     DI,0FFh
          CMP     DI,MaxCmd       ; supported by this driver?
          JLE     Strat1          ; jump if command code OK
          CALL    Error           ; bad command code
          JMP     Strat2
          ALIGN   8
  Strat1: ADD     DI,DI           ; branch to command code routine
          CALL    WORD PTR [DI+Dispch] ; call appropriate
  Strat2: RET                     ; back to OS/2 kernel
  Strat   ENDP

          ALIGN 16
  Context PROC    FAR             ; Context hook general
          PUSHAD
          PUSH    ES
          PUSH    GS
          PUSH    DS
          MOV     EDI,EAX           ; save EAX
          MOV     AX,SEG FLAT:_DATA ; for some functions to work
          MOV     GS,AX             ; we need FLAT Data Selector in GS
          MOV     AX,SEG _DATA      ; make shure DS is pointing to _DATA
          MOV     DS,AX
          XOR     EAX,EAX           ; set EAX to compare to CtxSem
          MOV     EBX,1             ; if CtxSem is 0 set it to 1
          LOCK    CMPXCHG [CtxSem],EBX ; enter with first processor only
          JNE     CFail             ; if hook is already running, leave
          LOCK    OR  [CtxRun],1    ; mark Hook running
          MOV     EAX,EDI           ; get ReqBits back
  Loop:   NOT     EAX
          LOCK    AND [CtxReq],EAX  ; mask Req Bits handled within this call
          LOCK    OR  [CtxRun],1    ; mark Hook running
          XOR     ESI,ESI           ; signal Context
          MOV     AX,SEG FLAT:DATA32 ; prepare AX to give Data-selector
          CALL    DS:[STRCode]      ; call Strat
          MOV     AX,SEG _DATA      ; make shure DS is pointing to _DATA
          MOV     DS,AX
          LOCK    AND [CtxRun],0    ; mark Hook not running
          MOV     EAX,[CtxReq]      ; look, if requests are left
          MOV     EDI,EAX           ; copy requests to PASCAL
          CMP     EAX,0
          JNZ     Loop
          MOV     [CtxSem],0
   CFail: POP     DS
          POP     GS
          POP     ES                ; restore
          POPAD
          RET                       ; back to OS/2 kernel
  Context ENDP

          ALIGN 16
  IDC_Stk PROC    FAR C  @@offset,@@selector
          PUSHFD
          PUSHAD
          PUSH    DS
          PUSH    ES
          PUSH    GS
          MOV     AX,SEG FLAT:_DATA; prep data segments
          MOV     GS,AX
          MOV     AX,SEG _DATA
          MOV     DS,AX
          MOV     BX,[@@offset]   ; obtain packet pointer
          MOV     AX,[@@selector]
          MOV     DI,AX           ; keep 16:16 pointer in EDI
          SHL     EDI,010h
          MOV     DI,BX
          CALL    GetSelInf
          JB      IFail
          MOVZX   EBX,BX
          ADD     ECX,EBX         ; build FLAT address
          MOV     ESI,ECX
          CALL    IDC_Main
          MOV     SS:[ESP+022h],EAX ;patch StackEAX to EAX
          SHR     EAX,010h        ; C-Style callers need MSB of Error Code in DX
          MOV     SS:[ESP+01Ah],EAX ;patch StackEDX to EAX (High Bytes]
   IFail: POP     GS              ; cleanup
          POP     ES
          POP     DS
          POPAD
          POPFD
          AND   EAX,0FFFFh        ; mask MSB
          RET
  IDC_Stk ENDP

          ALIGN 16
  IDC_Reg PROC    FAR
          PUSHFD
          PUSHAD                   ; save Regs
          PUSH    DS
          PUSH    ES
          PUSH    GS
          MOV     AX,SEG FLAT:_DATA; prep data segments
          MOV     GS,AX
          MOV     AX,SEG _DATA
          MOV     DS,AX
          MOV     AX,ES            ; get info about given segment in ES
          MOV     DI,AX            ; keep 16:16 pointer in EDI
          SHL     EDI,010h
          MOV     DI,BX
          CALL    GetSelInf
          JB      IFail
          MOVZX   EBX,BX
          ADD     ECX,EBX          ; build FLAT address
          MOV     ESI,ECX
          CALL    IDC_Main
          MOV     SS:[ESP+022h],EAX ;patch StackEAX to EAX
   IFail: POP     GS               ; cleanup
          POP     ES
          POP     DS
          POPAD
          POPFD
          AND   EAX,0FFFFh
          RET
  IDC_Reg ENDP

          ALIGN 16
 IDC_None PROC    FAR
          PUSHFD
          PUSHAD                   ; save Regs
          PUSH    DS
          PUSH    ES
          PUSH    GS
          MOV     AX,SEG FLAT:_DATA; prep data segments
          MOV     GS,AX
          MOV     AX,SEG _DATA
          MOV     DS,AX
          XOR     ESI,ESI          ; data is NULL pointer
          XOR     EDI,EDI
          CALL    IDC_Main
          MOV     SS:[ESP+022h],EAX ;patch StackEAX to EAX
   IFail: POP     GS               ; cleanup
          POP     ES
          POP     DS
          POPAD
          POPFD
          AND   EAX,0FFFFh
          RET
 IDC_None ENDP

          ALIGN 16
 IDC_Main PROC    NEAR
          MOV     AX,SEG FLAT:DATA32 ; prepare AX to give Data-selector
          CALL    DS:[IDCCode]    ; call Strat
          RET
 IDC_Main ENDP

          ALIGN 16
  Intr    PROC    FAR               ; driver Interrupt handler
          MOV     AX,SEG _DATA      ; make shure DS is pointing to _DATA
          MOV     DS,AX
          PUSH    DS
          MOV     AX,SEG FLAT:_DATA ; for some functions to work
          MOV     GS,AX             ; we need FLAT Data Selector in GS
          MOV     ESI,01h         ; set ESI = 'TRUE'
          CALL    DS:[IRQCode]    ; call Strat
          POP     DS              ; restore 16-bit DS
          CMP     EAX, 0h         ; get Result
          JZ      NotReady
          MOV     AX,DS:[IRQ_num]
          MOV     DX,031h
          CALL    [DevHlp]        ; send EOI
          CLC                     ; signal we owned interrupt
          RET                     ; return from interrupt
          ALIGN 8
NotReady: STC                     ; signal we didn't owned interrupt
          RET                     ; return
  Intr    ENDP

          ALIGN 16
  Timer   PROC    FAR             ; driver timer call handler
          PUSHFD
          PUSHAD                  ; save all
          PUSH    ES
          PUSH    GS
          PUSH    DS
          MOV     AX,SEG _DATA      ; make shure DS is pointing to _DATA
          MOV     DS,AX
          MOV     AX,SEG FLAT:_DATA ; for some functions to work
          MOV     GS,AX             ; we need FLAT Data Selector in GS
          XOR     ESI,ESI         ; set ESI = 'FALSE'
          CALL    DS:[IRQCode]    ; call Strat
          POP     DS
          POP     GS
          POP     ES              ; restore
          POPAD
          POPFD
          RET                     ; return from timer call
  Timer   ENDP

;##############################################################
;
;         MAC INTERFACE / PASCAL-STYLE CALL FUNCTIONS
;
;##############################################################

          ALIGN 16
 MACCall0 PROC    FAR
          MOV     EAX,0           ;store func number
          JMP     MACCall
 MACCall0 ENDP
          ALIGN 4
 MACCall1 PROC    FAR
          MOV     EAX,1           ;store func number
          JMP     MACCall
 MACCall1 ENDP
          ALIGN 4
 MACCall2 PROC    FAR
          MOV     EAX,2           ;store func number
          JMP     MACCall
 MACCall2 ENDP
          ALIGN 4
 MACCall3 PROC    FAR
          MOV     EAX,3           ;store func number
          JMP     MACCall
 MACCall3 ENDP
          ALIGN 4
 MACCall4 PROC    FAR
          MOV     EAX,4           ;store func number
          JMP     MACCall
 MACCall4 ENDP
          ALIGN 4
 MACCall5 PROC    FAR
          MOV     EAX,5           ;store func number
          JMP     MACCall
 MACCall5 ENDP
          ALIGN 4
 MACCall6 PROC    FAR
          MOV     EAX,6           ;store func number
          JMP     MACCall
 MACCall6 ENDP
          ALIGN 16
 MACCall  PROC    FAR
          SHL     EAX,010h
          MOV     AX,SP
          PUSHAD
          PUSH    ES
          PUSH    GS
          PUSH    DS
          PUSHFD
          MOV     EBX,EAX
          MOV     AX,SEG _DATA      ; make shure DS is pointing to _DATA
          MOV     DS,AX
          MOV     AX,SEG FLAT:_DATA ; for some functions to work
          MOV     GS,AX             ; we need FLAT Data Selector in GS
          MOV     ECX,PTKSSBase     ; get FLAT Pointer to TKSSBase
          MOVZX   ESI,BX          ; calc FLAT addr to params
          MOV     EDX,ESI
          ADD     EDX,GS:[ECX]
          ADD     EDX,4
          MOV     ES,GS:[EDX]     ; get MAC DS from stack
          MOV     ESI,ES:0        ; set ESI = FLAT MAC DS = PPMACMODULE
          CALL    DS:[MACCode]    ; call MAC
          POPFD
          MOV     SS:[ESP+022h],EAX ;patch StackEAX to EAX
          POP     DS
          POP     GS
          POP     ES              ; restore
          POPAD                   ; EAX is patched!
          CLD
          CMP     AX,0
          JA      Ret2
          SHR     EAX,010h
          RET
          ALIGN 4
    Ret2: CMP     AX,2
          JA      Ret4
          SHR     EAX,010h
          RET 2
          ALIGN 4
    Ret4: CMP     AX,4
          JA      Ret6
          SHR     EAX,010h
          RET 4
          ALIGN 4
    Ret6: CMP     AX,6
          JA      Ret8
          SHR     EAX,010h
          RET 6
          ALIGN 4
    Ret8: CMP     AX,8
          JA      RetA
          SHR     EAX,010h
          RET 8
          ALIGN 4
    RetA: CMP     AX,0Ah
          JA      RetC
          SHR     EAX,010h
          RET 0Ah
          ALIGN 4
    RetC: CMP     AX,0Ch
          JA      RetE
          SHR     EAX,010h
          RET 0Ch
          ALIGN 4
    RetE: SHR     EAX,010h
          RET 0Eh
 MACCall  ENDP

;###############################################################
;
;         Strategy Routines 16-bit
;
;###############################################################

  ; Command code routines are called by the Strategy routine
  ; via the Dispatch table with ES:BX pointing to the request
  ; header.
  ; Reserved Field is saved and replaced by virtual address of packet
  ; Status to be placed in request packet:
  ; 0100H if 'done' and no error
  ; 0000H if thread should block pending interrupt
  ; 81xxH if 'done' and error detected (xx=error code)
  ; Status has to be set in PASCAL

          ALIGN 16
  CallStr PROC    NEAR
          MOV     ECX,ES:[BX+05h] ; save Reserved
          PUSH    ECX
          MOV     CX,ES           ; build virtual address in ECX
          SHL     ECX,010h
          MOV     CX,BX
          MOV     ES:[BX+05h],ECX ; store to Reserved
          PUSH    BX              ; save BX
          MOV     AX,SEG FLAT:CODE32
          VERR    AX              ; test, if we are in Ring 0
          JZ      Ring0
          MOV     AX,ES           ; load ES selector in AX
          CALL    GetSelInf       ; ES starts at ECX
          JB      CFail           ; on Error cancel
          MOVZX   EBX,BX          ; mask MSW
          ADD     ECX,EBX         ; add to start
          MOV     ESI,ECX         ; store to ESI
          PUSH    ESI             ; store Address to Request to pascal
          CALL    DS:[GateR0]     ; call Strat via Gate
          POP     BX              ; cleanup
          POP     ECX
          MOV     ES:[BX+05h],ECX ; restore reserved
          RET
          ALIGN 16
   Ring0: MOV     AX,SEG FLAT:_DATA ; for some functions to work
          MOV     GS,AX             ; we need FLAT Data Selector in GS
          MOV     AX,ES           ; load ES selector in AX
          CALL    GetSelInf       ; ES starts at ECX
          JB      CFail           ; on Error cancel
          MOVZX   EBX,BX          ; mask MSW
          ADD     ECX,EBX         ; add to start
          MOV     ESI,ECX         ; store to ESI
          MOV     AX,SEG FLAT:DATA32 ; prepare AX to give Data-selector
          PUSH    ES              ; keep ES
          PUSH    DS              ; keep DS for comming back
          CALL    DS:[STRCode]    ; call Strat
          POP     DS              ; cleanup
          POP     ES
          POP     BX
          POP     ECX
          MOV     ES:[BX+05h],ECX ; restore reserved
          RET
          ALIGN 8
   CFail: POP     BX              ; stack cleanup
          POP     ECX
          MOV     ES:[BX+05h],ECX ; restore reserved
          JMP     Error           ; signal error
  CallStr ENDP

          ALIGN 8
  Error   PROC    NEAR            ; bad command code
          MOV     AX,8103h        ; error bit and 'done' status + "Unknown Command" code
          MOV     ES:[BX+3],AX    ; status into request packet
          RET
  Error   ENDP


  ; this is DosIO-Routine, preparing Read and Write Commands
  ; -saving DataPointer from RequestPacket on Stack
  ; -converting DataPointer from PhysMem to FLAT
  ; -Call PASCAL
  ; -cleanup afer PASCAL has run
  ; USES: ALL Registers except BX,ES are saved

          ALIGN 16
  DosIO   PROC    NEAR            ; convert given PhysAddr to FLAT
          MOV     AX,SEG FLAT:_DATA ; for some functions to work
          MOV     GS,AX           ; we need FLAT Data Selector in GS
          MOV     EAX,ES:[BX+0Eh] ; get physical Address
          MOV     CX,ES:[BX+012h] ; length
          PUSH    EAX             ; save
          PUSH    BX
          PUSH    ES
          MOVZX   EBX,AX          ; low part
          SHR     EAX,010h        ; high part
          MOV     DX,0115h        ; PhysToLin
          CALL    [DevHlp]        ; put Virtual in ES:DI
          JB      DFail           ; on Error cancel
          MOV     AX,ES
          MOVZX   EBX,DI
          POP     ES
          CALL    GetSelInf       ; now we have flat base address in ECX
          ADD     ECX,EBX         ; add offset to base
          POP     BX
          MOV     ES:[BX+0Eh],ECX ; store flat pointer
          CALL    CallStr         ; call PASCAL
          POP     EAX
          MOV     ES:[BX+0Eh],EAX ; restore Physical Address
          RET
          ALIGN 8
   DFail: POP     BX
          POP     ES
          POP     EAX
          JMP     Error
  DosIO   ENDP

  ; this is IOCtl-Routine, preparing IOCtl Commands
  ; -test access to given pointers
  ; -Call PASCAL
  ; USES: ALL Registers except BX,ES are saved

          ALIGN 16
  IOCtl   PROC    NEAR
          MOV     AX,SEG FLAT:_DATA ; for some functions to work
          MOV     GS,AX           ; we need FLAT Data Selector in GS
          MOV     EAX,ES:[BX+00Fh]; get address params
          MOV     EDX,EAX
          AND     EDX,0FFF8FFFFh  ; mask RPL&TI
          MOV     CX,ES:[BX+019h] ; length
          CMP     EDX,00h         ; null pointer?
          JE      NoParam
          CMP     CX,00h          ; length given?
          JNE     ParLenOK
          MOV     CX,01h          ; check 1 byte
ParLenOK: MOV     DX,0027h
          MOV     DI,AX
          SHR     EAX,010h
          CALL    [DevHlp]
          JB      NEAR PTR Error
 NoParam: MOV     EAX,ES:[BX+013h]; get address data
          MOV     EDX,EAX
          AND     EDX,0FFF8FFFFh  ; mask RPL&TI
          MOV     CX,ES:[BX+01Bh] ; length
          CMP     EDX,00h         ; null pointer?
          JE      NoData
          CMP     CX,00h          ; length given?
          JNE     DataLenOK
          MOV     CX,01h          ; check 1 byte
DataLenOK:MOV     DX,0027h
          MOV     DI,AX
          SHR     EAX,010h
          CALL    [DevHlp]
          JB      NEAR PTR Error
  NoData: CALL    CallStr         ; call PASCAL
          RET
  IOCtl   ENDP


  ; this is INIT-Routine, preparing for running PASCAL-Part
  ; -getting Pointer to Kernel DevHlp
  ; -setting up FLAT-Offsets to all 32-bit Code Entries
  ; -setting up ALIAS FLAT Segments for running FLAT-Code in Ring3
  ; -cleanup afer PASCAL has run
  ; USES: ALL Registers except BX,ES are saved

          ALIGN 16
  Init    PROC    NEAR            ; function 0 = initialize
          MOV     AX,ES:[BX+14]   ; get DevHlp entry point
          MOV     WORD PTR DevHlp,AX
          MOV     AX,ES:[BX+16]
          MOV     WORD PTR DevHlp+2,AX
          PUSH    BX              ; save BX cause DevHelp will destroy
          PUSH    ES              ; save ES cause DevHelp will destroy
          MOV     AX,09h          ; get TKSSBase Address
          MOV     CX,00h          ; use undocumented GetDosVar 9
          MOV     DX,024h         ; see Matthieu WILLM 32DRV170.ZIP
          CALL    [DevHlp]
          JB      NEAR  PTR Fail1
          MOV     ES,AX           ; Selector to DosTable
          MOVZX   EBX,BX          ; Offset to DosTabel1
          MOVZX   ECX,BYTE PTR ES:[EBX] ; get Length  (DWords)
          SHL     ECX,02h         ; convert to DWord
          ADD     EBX,ECX         ; add Offset
          INC     EBX             ; skip Length
          MOV     ECX,0Bh         ; OFFSET to TKSSBase (DWords)
          SHL     ECX,02h         ; convert to DWord
          ADD     EBX,ECX         ; add Offset
          INC     EBX             ; skip Length
          MOV     EAX,ES:[EBX]    ; now EAX points to TKSSBase Kernel Var
          MOV     [PTKSSBase],EAX ; store this Address
          MOV     AX,SEG CODE32   ; get linear start of CODE32
          MOV     DL,05Dh         ; function GetDescInfo
          CALL    [DevHlp]        ; call DevHelper;Result: CODE32 starts at ECX
          JB      NEAR PTR Fail1  ; cancel if C is set
          MOV     EBX,ECX         ; save startpoint
          MOV     EAX,ECX
          MOV     EDX,ECX
          ADD     EBX,OFFSET PrepGDT  ; add offset of PrepGDT
          MOV     DWORD PTR DS:[PRPCode],EBX ; store to memory
          MOV     EBX,ECX
          ADD     EBX,OFFSET CallS32  ; add offset of CallS32
          MOV     DWORD PTR DS:[STRCode],EBX ; store to memory
          MOV     WORD PTR DS:[STRCode+4],SEG FLAT:CODE32 ; store Ring0 Selector
          ADD     EAX,OFFSET CallI32  ; add offset of CallI32
          MOV     DWORD PTR DS:[IRQCode],EAX ; store to memory
          MOV     WORD PTR DS:[IRQCode+4],SEG FLAT:CODE32 ; store Ring0 Selector
          ADD     EDX,OFFSET CallD32  ; add offset of CallD32
          MOV     DWORD PTR DS:[IDCCode],EDX ; store to memory
          MOV     WORD PTR DS:[IDCCode+4],SEG FLAT:CODE32 ; store Ring0 Selector
          ADD     ECX,OFFSET CallM32  ; add offset of CallM32
          MOV     DWORD PTR DS:[MACCode],ECX ; store to memory
          MOV     WORD PTR DS:[MACCode+4],SEG FLAT:CODE32 ; store Ring0 Selector
          MOV     AX,WORD PTR [PRPCode+2] ;we need a call gate to ring 0
          MOV     BX,WORD PTR [PRPCode]   ;to prepare GDT
          MOV     CX,1h           ; one param
          MOV     DX,00006Ch      ; 32 bit gate, function DynamicAPI
          CALL    DS:[DevHlp]     ; Call DevHelp
          JB      NEAR PTR Fail1  ; Result: Gateselector Gate32 to PrepGDT
          MOV     WORD PTR DS:[GateR0+4],DI ;stores Gate32:00000000
          MOV     AX,SEG DATA32   ; we have to get a linear Adress for DATA32
          MOV     DL,05Dh         ; so we can recreate a selector for 16 bit calls
          CALL    [DevHlp]        ; get descriptor info
          JB      NEAR PTR Fail1  ; cancel if C is set
          MOV     [DataLin],ECX   ; store start
          MOV     [DataLen],EDX   ; store length
          MOV     AX,DS           ; fill CtxSemP16 Pointers
          SHL     EAX,010h
          MOV     AX,OFFSET CtxSem
          MOV     DI,OFFSET CtxSemP16
          MOV     [DI],EAX
          ADD     AX,4
          MOV     [DI+4],EAX
          ADD     AX,4
          MOV     [DI+8],EAX
          POP     ES              ; get ES back
          POP     BX              ; get BX back
          CALL    CallStr         ; call pascal
          PUSH    BX
          PUSH    ES
          CALL    PrintMSG        ; print message queue
          XOR     EAX,EAX         ; reset GS cause invalid after freeing ALIAS
          MOV     GS,AX
          MOV     AX,SEG CODE32   ; lock CODE32
          MOV     BX,00100h       ; long term
          MOV     DL,013h
          CALL    [DevHlp]
          MOV     WORD PTR [LockCS],AX     ; save for unlock on error
          MOV     WORD PTR [LockCS+2],BX
          MOV     AX,SEG DATA32   ; lock DATA32
          MOV     BX,00100h       ; long term
          MOV     DL,013h
          CALL    [DevHlp]
          MOV     WORD PTR [LockDS],AX     ; save for unlock on error
          MOV     WORD PTR [LockDS+2],BX
          POP     ES
          POP     BX
          MOV     AX,WORD PTR ES:[BX+3] ; read status from pascal
          SUB     AX,00100h         ; all ok?
          JNE     Fail2             ; if not, reserve no memory
          MOV     WORD PTR ES:[BX+14],OFFSET _TEXT:Init   ; reserve till begin of Init
          MOV     WORD PTR ES:[BX+16],OFFSET DGROUP1:errm ; reserve till beginn of errm
          RET
          ALIGN 8
   Fail1: POP     ES                      ;cleanup stack
          POP     BX
   Fail2: MOV     WORD PTR ES:[BX+14],0 ;no code needed
          MOV     WORD PTR ES:[BX+16],0 ;no data needed
          MOV     WORD PTR ES:[BX+3],0810Ch ;signal failure to kernel
          MOV     AX,WORD PTR [LockCS]
          MOV     BX,WORD PTR [LockCS+2]
          MOV     DX,014h
          CALL    [DevHlp]              ; unlock CODE32
          MOV     AX,WORD PTR [LockDS]
          MOV     BX,WORD PTR [LockDS+2]
          MOV     DX,014h
          CALL    [DevHlp]              ; unlock DATA32
          PUSH    stdout            ; display error message on standard output handle
          PUSH    DS                ; address of message
          MOV     EAX,OFFSET DGROUP1:errm
          PUSH    AX
          PUSH    errm_len          ; length of message
          PUSH    DS                ; receives bytes written
          MOV     EAX,OFFSET DGROUP1:wlen
          PUSH    AX
          CALL    Dos16Write        ; transfer to OS/2
          RET
  Init    ENDP

          ALIGN 16
PrintMSG  PROC    NEAR
          MOV     EBX,[info]
          AND     EBX,0FFF8FFFFh
          CMP     EBX,0h
          JZ      Done
          MOV     AX,SEG FLAT:CODE32
          VERR    AX              ; test, if we are in Ring 0
          JZ      Ring0
          PUSH    stdout
          PUSH    EBX
          XOR     EAX,EAX
          LGS     AX,[info]
          DEC     AX
   Begin: INC     AX              ; get length
          TEST    AX,3
          JNZ     Odd
    More: MOV     ECX,GS:[EAX]
          MOV     EDX,ECX
          ADD     AX,4
          NOT     ECX
          SUB     EDX,01010101h
          AND     ECX,80808080h
          AND     ECX,EDX
          JZ      More
          SUB     AX,4
     Odd: CMP     BYTE PTR GS:[EAX],0
          JNE     Begin
          SUB     AX,BX
          PUSH    AX
          PUSH    DS
          MOV     EAX,OFFSET DGROUP1:wlen
          PUSH    AX
          CALL    Dos16Write        ; transfer to OS/2
          RET
   Ring0: LEA     AX,[infoid]       ; set id
          MOV     [infoid],AX
          MOV     SI,OFFSET DGROUP1:infoid
          XOR     EBX,EBX
          MOV     EDX,03Dh
          CALL    [DevHlp]
    Done: RET
PrintMSG  ENDP


  _TEXT   ENDS

;################################################################
;
;         32-Bit
;
;################################################################

CODE32    SEGMENT DWORD USE32 PUBLIC 'CODE'
          .MODEL FLAT
          ;ASSUME CS:FLAT, DS:FLAT, ES:FLAT, SS:FLAT

;################################################################
;
;         Context Thread INIT CODE - MUST BE ON PAGE START
;
;################################################################

; this Code is maped to Ring3 to get Thread to Ring0

CtxThBase DD    00h             ; Base Address - Contains Linear Base
CtxThPrio DD    00h             ; +04 Entry SetPrio
CtxThEnd  DD    00h             ; +08 Entry Thread Ends
CtxThGate DF    00h             ; +12 Entry Gate Selector

CtxThSt1  PROC NEAR
          PUSH  04h             ; Identifier (Offset)
          PUSH  03h             ; PrioClass
          JMP   CtxThMain
CtxThSt1  ENDP

CtxThSt2  PROC NEAR
          PUSH  08h             ; Identifier
          PUSH  04h             ; PrioCass
          JMP   CtxThMain
CtxThSt2  ENDP

CtxThMain PROC NEAR
          DB    0bdh            ; MOV EBP, BaseAddr
CtxThM_A1 DD    00h             ; this is patched to linear Base Addr by CreateThread
          MOV   EAX,EBP         ; store Base Addr
          MOV   [EBP],EAX
          POP   EBX             ; get PrioClass
          PUSH  00h             ; this thread
          PUSH  01Fh            ; delta +31
          PUSH  EBX             ; Prio Class
          PUSH  02h             ; PRTYS_THREAD
          MOV   EAX,OFFSET CtxThPrio-CtxThBase
          ADD   EAX,EBP
          CALL  DWORD PTR [EAX] ; Call SetPriority
          ADD   ESP,16          ; Stack Cleanup
          MOV   EAX,OFFSET CtxThGate-CtxThBase
          ADD   EAX,EBP
          CALL  FWORD PTR [EAX] ; call Gate to Ring0
          PUSH  0               ; end thread
          MOV   EAX,OFFSET CtxThEnd-CtxThBase
          ADD   EAX,EBP
          CALL  DWORD PTR [EAX] ; Call DosExit
CtxThMain ENDP


;################################################################
;
;         CallGate Entry Point to Ring0
;
;################################################################
  ; here we are when CallGate completes
  ; when call ist strategy, the packet address ist found at SS:[ESI]
  ; if packet address is null, call is context thread
  ; Segment Registers are kept by CallGate

          ALIGN 4
  PrepGDT PROC FAR                ; here we are at ring 0
          MOV   AX,SEG FLAT:_DATA ; get flat data descriptor
          MOV   DS,AX             ; set DS FLAT
          MOV   ES,AX             ; set ES FLAT
          MOV   GS,AX             ; set GS FLAT
          AND   ESI,0000FFFFh
          MOV   EAX,SS:[ESI]      ; get Address to Request
          CMP   EAX,010h           ; test if Hook
          JB    NEAR PTR RunThread
          PUSH  EAX               ; save to later Check
          PUSH  EAX               ; store to PASCAL
          CALL  NEAR32 PTR DRVSTUB1@Strategy
          POP   EAX               ; get Request back
          CMP   WORD PTR [EAX+3],0100h ; check if Driver Init was OK
          JNE   Leave
          CALL  NEAR32 PTR InitContextThread
  Leave:  XOR   EAX,EAX           ; clear carry flag
          RET                     ; status is set in packet
  PrepGDT ENDP

;##############################################################
;
;         PASCAL Entry Points
;
;##############################################################

  ; here we call the PASCAL Strategy Routine
  ; Params are: LinPointer to RequestPacket - delivered in REG ESI
  ;             Base to Stack - delivered in REG EBP
  ; If ESI=0 then call is a context-hook call.
  ; In that case EDI contains given Param of EAX from activation

          ALIGN 4
 CallS32  PROC  FAR
          MOV   DS,AX             ; set DS FLAT
          MOV   ES,AX             ; set ES FLAT
          CMP   ESI,0h
          JZ    ContextH
          PUSH  ESI               ; store Address to Request to pascal
          CALL  NEAR32 PTR DRVSTUB1@Strategy
          XOR   EAX,EAX           ; clear carry flag
          RET                     ; status is set in packet
ContextH: MOV   EAX,01h
          PUSH  EAX               ; store 1 to pascal to indicate CtxHook
          PUSH  EDI               ; store Params from ArmCtxHook
          CALL  NEAR32 PTR DRVSTUB1@Context
          XOR   EAX,EAX           ; clear carry flag
          RET                     ; status is set in packet
 CallS32  ENDP

          ALIGN 4
 CallI32  PROC  FAR
          MOV   DS,AX             ; set DS FLAT
          MOV   ES,AX             ; set ES FLAT
          PUSH  ESI               ; store Bool IRQ Indicator
          CALL  NEAR32 PTR DRVSTUB1@Interrupt
          RET                     ; status is set in packet
 CallI32  ENDP

          ALIGN 4
 CallD32  PROC  FAR
          MOV   DS,AX             ; set DS FLAT
          MOV   ES,AX             ; set ES FLAT
          PUSH  ESI               ; store pointer to data
          PUSH  EDI               ; store 16:16 pointer to data
          CALL  NEAR32 PTR DRVSTUB1@IDCEntry
          RET                     ; status is set in packet
 CallD32  ENDP

 CallM32  PROC  FAR
          MOV   DS,AX             ; set DS FLAT
          MOV   ES,AX             ; set ES FLAT
          SHR   EBX,010h          ; get func number
          PUSH  EBX
          PUSH  EDX               ; store pointer to params
          PUSH  ESI               ; store pointer to data
          CALL  NEAR32 PTR DRVSTUB1@MACEntry
          RET                     ; status is set in packet
 CallM32  ENDP

;##################################################################
;
;         PASCAL FUNCTIONS
;
;##################################################################

          ALIGN 4
DRVBASE@DevHlp32 PROC  NEAR       ; 32-bit part for calling devhlp
          PUSH  DS                ; push flat DS on stack
          PUSH  ES                ; push flat ES on stack
          PUSH  EBP               ; save ebp
          PUSH  CS                  ; push flat CS on stack
          PUSH  OFFSET DevHlpRt ; push offset for return on stack
          JMP   FAR16 PTR DevHlp16  ; call 16-bit part
          ALIGN 4
DevHlpRt: POP   EBP                 ; get back EBP
          POP   ES                  ; get FLAT ES
          POP   DS                  ; get FLAT DS
          RET
DRVBASE@DevHlp32 ENDP

          ALIGN 4
DRVBASE@GetGDTSelInf32 PROC  NEAR
          PUSH    ESI
          MOVZX   ESI,AX            ; get Selector in ESI
          MOV     EDX,OFFSET GDT_Base ; load GDT regs at GDT_Base
          MOV     CX,-1             ; acquire lock
    WAIT: XCHG    CX,[EDX+6]
          OR      CX,CX
          JNZ     WAIT
          SGDT    [EDX]
          AND     ESI,0000FFF8h     ; mask RPL (for security)
          ADD     ESI,[EDX+2]       ; build address of Selector in GDT
          MOV     DWORD PTR [EDX+6],0 ;free Lock
          MOV     EDX,GS:[ESI+2]    ; load lower Base in EDX
          AND     EDX,000FFFFFFh
          MOV     ECX,GS:[ESI+4]    ; load higher Base in ECX
          AND     ECX,0FF000000h
          ADD     ECX,EDX           ; Base is now in ECX
          MOVZX   EDX,AX
          LSL     EDX,EDX           ; Size is in EDX
          POP     ESI
          RET
DRVBASE@GetGDTSelInf32 ENDP

          ALIGN 4
DRVBASE@DevYield  PROC  NEAR
          MOV     AX,CS
          AND     AX,03h            ; check if Ring 0
          JNZ     Leave             ; only valid at task time
          MOV     EAX,[PYield]
          JNZ     Yield
          PUSH    EBX               ; save BX
          XOR     EBX,EBX           ; fill MSW with zero
          MOV     AX,007h
          MOV     DX,024h
          CALL    DRVBASE@DevHlp32
          JB      Err
          CALL    DRVBASE@GetGDTSelInf32
          ADD     EBX,ECX
          MOV     [PYield],EBX        ; store flat adress to Yield-Flag
          POP     EBX
   Yield: MOV     EAX,[PYield]      ; test kernel yield flag
          MOV     DL,GS:[EAX]
          OR      DL,DL
          JZ      Leave             ; if zero then exit
          MOV     DX,02h            ; call Yield
          CALL    DRVBASE@DevHlp32
          XOR     EAX,EAX           ; signal success
          RET
          ALIGN 4
     Err: POP     EBX
   Leave: MOV     EAX,01            ; signal not done
          RET
DRVBASE@DevYield  ENDP

          ALIGN 4
DRVBASE@DevTCYield  PROC  NEAR
          MOV     AX,CS
          AND     AX,03h            ; check if Ring 0
          JNZ     Leave             ; only valid at task time
          MOV     EAX,[PTCYield]
          JNZ     Yield
          PUSH    EBX               ; save BX
          XOR     EBX,EBX           ; fill MSW with zero
          MOV     AX,008h
          MOV     DX,024h
          CALL    DRVBASE@DevHlp32
          JB      Err
          CALL    DRVBASE@GetGDTSelInf32
          ADD     EBX,ECX
          MOV     [PTCYield],EBX        ; store flat adress to Yield-Flag
          POP     EBX
   Yield: MOV     EAX,[PTCYield]      ; test kernel yield flag
          MOV     DL,GS:[EAX]
          OR      DL,DL
          JZ      Leave             ; if zero then exit
          MOV     DX,03h            ; call TCYield
          CALL    DRVBASE@DevHlp32
          XOR     EAX,EAX           ; signal success
          RET
          ALIGN 4
     Err: POP     EBX
   Leave: MOV     EAX,01            ; signal not done
          RET
DRVBASE@DevTCYield  ENDP

DRVBASE@LocToDat32 PROC NEAR
          MOV     EAX,[ESP+4]
          MOV     EDX,[PTKSSBase]    ; get PTKSSBase
          CMP     EAX,[EDX]
          JAE     Ready
          ADD     EAX,[EDX]
  Ready:  RET 4
DRVBASE@LocToDat32 ENDP

          ALIGN 4
DRVBASE@DevSleep PROC  NEAR
          PUSH    EBX
          PUSH    EDI
          MOV     EDI,[ESP+12]
          MOVZX   ECX,DI
          SHR     EDI,010h
          MOV     EAX,OFFSET $
          MOVZX   EBX,AX
          SHR     EAX,010h
          MOV     EDX,00104h
          CALL    NEAR32 PTR DRVBASE@DevHlp32
          POP     EDI
          POP     EBX
          RET 4
DRVBASE@DevSleep ENDP

;##################################################################
;
;         CONTEXT THREAD CREATION AND RUNNING LOOP
;
;##################################################################

; now call DOS32CREATETHREAD to add the Context Threads to Process1 (Kernel)

          ALIGN 4
InitContextThread PROC NEAR
          MOV   ESI,OFFSET CtxMask; get Mask
          MOV   EAX,[ESI]         ; test if any Threads to create
          AND   EAX,06h
          JZ    NEAR PTR No2ndTh
          MOV   EBX,OFFSET CtxThBase ; StartAddr of StartupCode to be mapped to Ring3
          MOV   EAX,01h           ; map writeable
          MOV   ECX,100h          ; Len
          MOV   EDX,05Ah          ; map to Ring3 FLAT Area
          CALL  NEAR32 PTR DRVBASE@DevHlp32
          JB    NEAR PTR No2ndTh  ;Error
          MOV   EDI,OFFSET CtxMask ; get BasePtr to Var to store Ring3 Entry Point
          MOV   ECX,OFFSET CtxThSt1-CtxThBase ; calc Address to first Entry
          ADD   ECX,EAX
          MOV   [EDI+04h],ECX     ; store first EntryPoint
          MOV   ECX,OFFSET CtxThSt2-CtxThBase ; calc Address to second
          ADD   ECX,EAX
          MOV   [EDI+08h],ECX     ; store second EntryPoint
          MOV   ECX,OFFSET CtxThM_A1-CtxThBase ; calc Address to BasePtr
          ADD   ECX,EAX
          MOV   [ECX],EAX         ; Patch Code to Set BaseAddr
          MOV   EDX,OFFSET Dos32SetPriority
          MOV   ECX,OFFSET CtxThPrio-CtxThBase ; calc Address to DosOpen
          ADD   ECX,EAX
          MOV   [ECX],EDX         ; Patch Code to Set DosSetPriority
          MOV   EDX,OFFSET Dos32Exit
          MOV   ECX,OFFSET CtxThEnd-CtxThBase ; calc Address to DosOpen
          ADD   ECX,EAX
          MOV   [ECX],EDX         ; Patch Code to Set DosExit
          MOV   EDX,OFFSET GateR0
          MOV   ECX,OFFSET CtxThGate-CtxThBase ; calc Address to Gate
          ADD   ECX,EAX
          MOV   DX,[EDX+4]
          MOV   [ECX+4],DX        ; Patch Code to CallGate
          MOV   EAX,[ESI]         ; get mask
          AND   EAX,02h           ; mask first ContextThread
          JZ    No1stTh
          MOV   ESI,04h           ; set Offset to Structures
          CALL  NEAR32 PTR CreateContextThread
  No1stTh:MOV   ESI,OFFSET CtxMask; get Mask
          MOV   EAX,[ESI]
          AND   EAX,04h           ; mask second ContextThread
          JZ    No2ndTh
          MOV   ESI,08h           ; set Offset to Structures
          CALL  NEAR32 PTR CreateContextThread
  No2ndTh:RET
InitContextThread ENDP

; ESI has Offset to Data Structures (04=1st / 08=2nd Thread)

          ALIGN 4
CreateContextThread PROC NEAR
          PUSH  EBP             ; necessary else Stack gets corrupted by DosCall
          MOV   EBP,ESP
          MOV   EDI,OFFSET CtxMask ; get Var to store Ring3 EntryPoint
          MOV   EAX,0100h       ; Initial Stacksize to CreateThread
          PUSH  EAX
          XOR   EAX,EAX         ; Flags=0
          PUSH  EAX
          PUSH  EAX             ; Args=0
          MOV   EAX,[EDI+ESI]   ; get Start Addr
          PUSH  EAX             ; Start Addr at Ring3 to CreateThread
          MOV   EAX,OFFSET CtxHan
          ADD   EAX,ESI         ; Address to store ThreadID
          PUSH  EAX             ;
          CALL  NEAR32 PTR Dos32CreateThread
          MOV   ESP,EBP
          POP   EBP
          RET
CreateContextThread ENDP

          ALIGN 4
RunThread PROC  NEAR
          MOV     ESI,EAX              ; save Hook Number
          MOV     EAX,OFFSET CtxHan
          MOV     DWORD PTR [EAX+ESI],0
          MOV     EBX,[EAX+4]          ; look if threads left
          OR      EBX,[EAX+8]
          JNZ     ThLeft
          MOV     EAX,[CtxThBase] ; get Map Base
          MOV     EDX,058h             ; free Map
          CALL    NEAR32 PTR DRVBASE@DevHlp32
   ThLeft:MOV     EBX,OFFSET CtxSemP16 ; load all Regs to Request Sem
          ADD     EBX,ESI
          MOV     EAX,[EBX]
          MOVZX   EBX,AX
          SHR     EAX,010h
          MOV     ECX,0FFFFh
          MOV     EDI,ECX
          MOV     EDX,006h
          PUSHAD                       ; save all
          CALL    NEAR32 PTR DRVBASE@DevHlp32
OuterLoop:POPAD                        ; get Regs back
          PUSHAD
          PUSH    ESI                   ; on SMP Kernel, ESI will be destroyed
          CALL    NEAR32 PTR DRVBASE@DevHlp32  ; Request Sem
          POP     ESI
          MOV     EDX,[CtxReq+ESI]
          LOCK    OR  [CtxRun+ESI],1    ; mark Hook running
          MOV     EAX,EDX               ; get active ReqBits
InnerLoop:NOT     EAX
          LOCK    AND [CtxReq+ESI],EAX  ; mask Req Bits handled within this call
          LOCK    OR  [CtxRun+ESI],1    ; mark Hook running
          PUSH    ESI                   ; save Offset
          SHR     ESI,1
          PUSH    ESI                   ; store Hook to pascal
          PUSH    EDX                   ; store Params from ArmCtxHook
          CALL    NEAR32 PTR DRVSTUB1@Context
          POP     ESI
          LOCK    AND [CtxRun+ESI],0    ; mark Hook not running
          MOV     EAX,[CtxReq+ESI]      ; look if requests are left
          CMP     EAX,0
          JNZ     NEAR PTR InnerLoop
          JMP     NEAR PTR OuterLoop
RunThread ENDP

CODE32    ENDS

DATA32    SEGMENT DWORD USE32 PUBLIC 'DATA'
PYield:   DD      0                 ; pointer to yield-flag
PTCYield: DD      0                 ; pointer to tcyield-flag
DATA32    ENDS


PUBLIC    DRVBASE@DevHlp32
PUBLIC    DRVBASE@GetGDTSelInf32
PUBLIC    DRVBASE@DevYield

END

