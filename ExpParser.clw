
  PROGRAM

OMIT('***')
 * Created with Clarion 11.1
 * User: owen
 * Date: 23/12/22
 * Time: 4:40 PM
 * 
 * History =========================================================================================
 * 2022-12-27 Owen  see GitHub
 * 2022-12-28 Carl  Add ProtoMapString with all parameters as seen in MAP. 
 *                  Queue add Token Position and Tokens to understand how it was parsed apart
 * 2022-12-30 Carl  add some TopSpeed C to decode RTL exports i=LONG Ui=ULONG C=CONST
 * 2024-08-05 Carl  Changes after testing with Skype post by Eric with 22 and 42 parms
 *                      Test 22:  EXECUTESQL@F11FL_SQLCLASSsbRuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPu @   <-- Has leading Name@F that needs "clean"
 *                      Test 42:  F11FL_SQLCLASSsbRuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPuPu
 *                      Types:    PAAllRlsbRsbOsbPsbBqg5MYQUE7MYGROUPFUcUiPCcUiBwBrBfBbBkBqBiBa
 *                  On Accept ExportString DO Clean routine to remove "Name@F" at start else trips up parsing. Was in above Test 22.
 *                  CleanExportStringRtn change StrPos() RegEx to look for '@ ?' or '@ #' on end using '$'
 *                  ProtoMapString add Copy Button.
 *                  Add ParameterNumber to Queue and Parser in Class so can display in LIST. Helps to know where Parm is in Prototype.
 *                  Add # Column to LIST to see new q.ParameterNumber
 *                  WINDOW - Move and Resize controls to work better with 20+ parms and longer Export String
 *                  Message() more readable in Font Segoe UI 11
 ***      !=========================================================================================

!Region Equates
MANGLECODE:PassByRef             EQUATE('R')
MANGLECODE:PassByRefOptional     EQUATE('P')
MANGLECODE:Optional              EQUATE('O')
MANGLECODE:Array                 EQUATE('A')    !Array after RPO. Can occur multiple times, once for each dim so AA=[,]
MANGLECODE:EntityPrefix          EQUATE('B')
MANGLECODE:UnsignedPrefix        EQUATE('U')
MANGLECODE:Complex               EQUATE('b')
MANGLECODE:StringEntity          EQUATE('s')

MANGLECODE:File                  EQUATE('Bf')
MANGLECODE:Blob                  EQUATE('Bb')
MANGLECODE:Key                   EQUATE('Bk')
MANGLECODE:Queue                 EQUATE('Bq')
MANGLECODE:Report                EQUATE('Br')
MANGLECODE:Window                EQUATE('Bw')
MANGLECODE:View                  EQUATE('Bi')
MANGLECODE:Application           EQUATE('Ba')
MANGLECODE:Function              EQUATE('F')
MANGLECODE:FunctionEnd           EQUATE('_')
MANGLECODE:Byte                  EQUATE('Uc')
MANGLECODE:Ushort                EQUATE('Us')
MANGLECODE:Ulong                 EQUATE('Ul')
MANGLECODE:Date                  EQUATE('bd')
MANGLECODE:Time                  EQUATE('bt')
MANGLECODE:Bfloat4               EQUATE('b4')
MANGLECODE:Bfloat8               EQUATE('b8')
MANGLECODE:String                EQUATE('sb')
MANGLECODE:Pstring               EQUATE('sp')
MANGLECODE:Cstring               EQUATE('sc')
MANGLECODE:Short                 EQUATE('s')
MANGLECODE:Long                  EQUATE('l')
MANGLECODE:Sreal                 EQUATE('f')
MANGLECODE:Real                  EQUATE('d')
MANGLECODE:Decimal               EQUATE('e')
MANGLECODE:Pdecimal              EQUATE('p')
MANGLECODE:CstringRaw            EQUATE('c')
MANGLECODE:GroupRaw              EQUATE('v')
MANGLECODE:Group                 EQUATE('g')
MANGLECODE:Any                   EQUATE('u')
MANGLECODE:Any2                  EQUATE('u')

TYPE:File					EQUATE('FILE')
TYPE:Blob					EQUATE('BLOB')
TYPE:Key                    EQUATE('KEY')
TYPE:Queue                  EQUATE('QUEUE')
TYPE:Report                 EQUATE('REPORT')
TYPE:Window                 EQUATE('WINDOW')
TYPE:View                   EQUATE('VIEW')
TYPE:Application            EQUATE('APPLICATION')
TYPE:Byte                   EQUATE('BYTE')
TYPE:Ushort                 EQUATE('USHORT')
TYPE:Ulong                  EQUATE('ULONG')
TYPE:Date                   EQUATE('DATE')
TYPE:Time                   EQUATE('TIME')
TYPE:Bfloat4                EQUATE('BFLOAT4')
TYPE:Bfloat8                EQUATE('BFLOAT8')
TYPE:String                 EQUATE('STRING')
TYPE:Pstring                EQUATE('PSTRING')
TYPE:Cstring                EQUATE('CSTRING')
TYPE:Short                  EQUATE('SHORT')
TYPE:Long                   EQUATE('LONG')
TYPE:Sreal                  EQUATE('SREAL')
TYPE:Real                   EQUATE('REAL')
TYPE:Decimal                EQUATE('DECIMAL')
TYPE:Pdecimal               EQUATE('PDECIMAL')
TYPE:CstringRaw             EQUATE('CSTRINGRAW')
TYPE:GroupRaw               EQUATE('GROUPRAW')
TYPE:Group                  EQUATE('GROUP')
TYPE:Any                    EQUATE('ANY')
TYPE:Function               EQUATE('FUNCTION')
TYPE:FunctionEnd            EQUATE('FUNCTIONEND')

MANGLECODE:TopSpeedC_INT    EQUATE('i')             !TopSpeed C Integer 32 bits set as Clarion LONG ... would be 16 bits (USHORT) in 16-bit builds that no longer exist
MANGLECODE:TopSpeedC_Uint   EQUATE('Ui')            !TopSpeed C Unsigned Integer 32-bits set as Clarion Long
MANGLECODE:TopSpeedC_CONST  EQUATE('C')             !TopSpeed C CONST ... always 'PC' or 'RC' i.e. CONST <*> or *Ref

TYPE:TopSpeedC_INT          EQUATE('LONG')   !or ('SIGNED')     !i
TYPE:TopSpeedC_Uint         EQUATE('ULONG')  !or ('UNSIGNED')   !Ui
TYPE:TopSpeedC_CONST        EQUATE('CONST')

!EndRegion Equates

!Region QUEUEs
TYPE_TypesQueue     QUEUE,TYPE
ParameterNumber         USHORT          !Squence Number 1,2,3,4,5,6
TokenStartPos           USHORT          !SUB(Input, StartPos
TokenLength             USHORT          !SUB(Input, StartPos, Length)
Tokens                  STRING(255)
ParameterType           STRING(255)
ParameterName           STRING(255)
IsOptionalYN            BOOL   ! O
IsReferenceYN           BOOL   ! R
ArrayDIMs               BYTE   ! A  has DIM() with this many ,,, e.g. DIM(5,12) is DIMs=2
IsRaw                   BOOL   ! Applies to Cstrings and Groups where RAW was on Prototype, not allowed in Clarion
IsConstYN               BOOL   ! CONST a 'C' in TopSpeed C Prototypes e.g. PCc is <CONST *CSTRING>
ProtoType               STRING(255)
                    END

TYPE_TokensQueue    QUEUE
ScanToken               STRING(2)
ReturnToken             STRING(10)
                    END
!EndRegion QUEUEs

    MAP
TestWindow          PROCEDURE()
DB                  PROCEDURE(STRING DebugMessage)  !Does OutputDebugString().. maybe rename Trace?
        MODULE('WinAPI')
           OutputDebugString(*cstring Msg),PASCAL,RAW,DLL(1),NAME('OutputDebugStringA')
        END
    END

DBParsing   SHORT(1)

ExpParser           CLASS,TYPE
ExpString               &STRING
ExpStringLength          LONG
CharacterIndex           LONG
CharacterIndexStart      LONG

Construct               PROCEDURE()
Destruct                PROCEDURE()
Destroy                 PROCEDURE()
Parse                   PROCEDURE(*STRING ExpString, *TYPE_TypesQueue q)
GetToken                PROCEDURE(),STRING
IsTokenPreamble         PROCEDURE(STRING Token),BOOL    !Token is R,P,O = *XXX <*XX> <XXX>
BuildPrototype          PROCEDURE(STRING ParameterType, STRING ParameterName, BOOL IsReferenceYN, BOOL IsOptionalYN, LONG ArrayDIMs, BOOL IsConstYN),STRING
                    END


    CODE
        TestWindow()
        
TestWindow          PROCEDURE()
AT_Pos                  LONG
ExportString            STRING(2048)
ProtoMapString          STRING(2048)            !Prototype of all parameters in Queue e.g. (<LONG P1>). Tested 42 and took 670 bytes
qParameters             QUEUE(TYPE_TypesQueue).
CBTest                  STRING(32000)           !Carl's test what's on the Clipboard 

Window WINDOW('EXP to MAP - Convert Mangled to Protype'),AT(,,400,202),GRAY,AUTO,SYSTEM,ICON(ICON:Thumbnail) |
            ,FONT('Segoe UI',9),RESIZE
        PROMPT('Export String:'),AT(2,18),USE(?ExportString:PROMPT)
        TEXT,AT(47,18,,10),FULL,USE(ExportString),FONT('Consolas'),SINGLE
        TEXT,AT(45,32,,32),FULL,USE(ProtoMapString),SKIP,TRN,VSCROLL,FONT('Consolas'),READONLY
        BUTTON,AT(45,3,11,11),USE(?ExpPasteBtn),SKIP,ICON(ICON:Paste),TIP('Paste clipboard into Expo' & |
                'rt String and Parse'),FLAT
        PROMPT('Only include the portion of the Export string after @F'),AT(97,5),USE(?ExportString:FYI)
        BUTTON('&Parse'),AT(4,31,35),USE(?Parse)
        BUTTON,AT(29,52,11,11),USE(?ProtoCopyBtn),SKIP,ICON(ICON:Copy),TIP('Copy the Prototype'),FLAT
        LIST,AT(2,71),FULL,USE(?LIST:Parameters),VSCROLL,FROM(qParameters) , |
 FORMAT('14R(4)|M~#~C(0)@N2@' &|
        '['                            &|
          '16R@N_6~ ,~@'               &|
          '14L(1)@S4@'                 &|
                     ']|~Position~'    &|
        '28L(2)|M~Tokens~'             &|
        '60L(2)|M~Type~'               &|
        '36L(2)|M~Name~'               &|
        '36C|M~<<Optional>~@N2~<<>~b@' &|
        '40C|M~*Reference~@N1~*~b@'    &|
        '20L(5)|M~Array~C(0)@N2b@'     &|
        '20C|M~Raw~@N3~RAW~b@'         &|
        '23C|M~Const~@N5~Const~b@'     &|
        '80L(2)~Prototype~'           )
        BUTTON('Cl&ose'),AT(295,3,30,12),USE(?Close),HIDE
        BUTTON('Re-Run'),AT(331,3,35,12),USE(?ReRun),TIP('Run another instance')
        TEXT,AT(400,1,,55),FULL,USE(CBTest),SKIP,HIDE,HVSCROLL,TIP('Filled from ClipBoard when Input' & |
                ' is TEST')
    END

parser                  ExpParser
    CODE
! ExportString='PAAlllsbRsbOsbPsb' !test:  (<*LONG[,] A2dim>,LONG SheetFEQ,BOOL Wrap=0,STRING S1,*STRING S2,<STRING S3>,<*STRING S4>)
! ExportString='BwBrBfBbBkBqBiBa'  !test:  (*WINDOW W1,*REPORT R1>,FILE F1,BLOB B1,KEY K1,QUEUE Q1,VIEW V1,APPLICATION A1)  
! ExportString='Bqg'              !test:  (QUEUE Q1,GROUP G1)
! ExportString='5MYQUE7MYGROUP'    !test:  (MyQue Q1,MyGroup G1)       Named Types
! ExportString='5MYQUE10MYGROUPABC'  !test:  (MyQue Q1,MyGroupABC G1)    Named Types 10
! ExportString='iPc'  !_WslHelp$GetHelpFile@FiPc   
! ExportString='PCc'  !_WslHelp$SetHelpFile@FPCc    SetHelpFileRTL(CONST *CSTRING HelpFileName)
! ExportString='FUcUiPCcUiPCcPCc'  !    e.g. _14TypeDescWriter__AddField@FUcUiPCcUiPCcPCc  (BYTE Parm_1,ULONG Parm_2,<CONST *CSTRING Parm_3>,ULONG Parm_4,<CONST *CSTRING Parm_5>,<CONST *CSTRING Parm_6>),RAW

        SYSTEM{PROP:FontName}='Segoe UI' ; SYSTEM{PROP:FontSize}=11     !Message() in Segoe UI 11
        SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY
        SYSTEM{PROP:PropVScroll}=1
        OPEN(Window)
        0{PROP:MinWidth}=0{PROP:Width} ; 0{PROP:MinHeight}=0{PROP:Height} * .60
        ExportString=COMMAND('INPUT')   !Was command line ExpParser.exe Input=FUcUiPCcUiPCcPCc
        IF ExportString THEN 
            DO CleanExportStringRtn
            IF ExportString THEN POST(EVENT:Accepted,?Parse).
        END 
        ACCEPT
            CASE ACCEPTED()
            OF ?ExpPasteBtn ; IF ~CLIPBOARD() THEN CYCLE. 
                              ExportString=CLIPBOARD()
                              DO CleanExportStringRtn
                              DISPLAY 
                              IF ExportString THEN POST(EVENT:Accepted,?Parse).
            OF ?ExportString
               DO CleanExportStringRtn
               DO BuildProtoMapStringRtn
               DISPLAY 

            OF ?Parse
                IF INLIST(UPPER(ExportString),'TEST','DTEST') THEN 
                   DO CBTestFromClipRtn ; CYCLE
                END            
                Parser.Parse(ExportString, qParameters)
                DO BuildProtoMapStringRtn
                DISPLAY()
                
            OF ?ProtoCopyBtn 
                SETCLIPBOARD(ProtoMapString)  
            OF ?Close
                POST(EVENT:CloseWindow)
            OF ?ReRun
                RUN(COMMAND('0'))
            END
        END
        CLOSE(Window)    

CleanExportStringRtn ROUTINE    !Remove Function Name before "@F" and any "@?" or "@123" on the end
    AT_Pos=INSTRING('@F',ExportString,1)   !Did they paste the Function@F 
    IF AT_Pos THEN ExportString=SUB(ExportString,AT_Pos+2,9999). !Start after @F
    !AT_Pos=INSTRING(' @?',ExportString,1)  !Did they paste EXP Line with @?
    AT_Pos=STRPOS(CLIP(ExportString),' @[?0-9 ]*$')              !Did they paste EXP Line with @? or @123 or '@ 123' or @ alone
    IF AT_Pos THEN ExportString=SUB(ExportString,1,AT_Pos-1).   !Cutoff @? @123
    ExportString=LEFT(ExportString)
    EXIT

BuildProtoMapStringRtn ROUTINE  !Format MAP Prototype from qParameters - probably should be in Class
    DATA
QX      LONG
IsRaw   BOOL
    CODE
    IsRaw = FALSE
    ProtoMapString='('
    LOOP QX=1 TO RECORDS(qParameters)
        GET(qParameters,QX)

        ProtoMapString=CLIP(ProtoMapString) & |
                    CHOOSE(QX=1,'',', ')    & |
                    qParameters.ProtoType
        IF qParameters.IsRaw = TRUE THEN IsRaw = TRUE.
    END
    ProtoMapString=CLIP(ProtoMapString) & ')' & CHOOSE(IsRaw = TRUE, ',RAW', '')
    ?ProtoMapString{PROP:Tip}='Label' & ProtoMapString

    EXIT
!------------------------------------------------------------
CBTestFromClipRtn ROUTINE   !1. Put EXP lines on Clipboard. 2. Type TEST and click Parse, can do DTEST for extra Debug
                            !Sets CBtest=Clipboard. That's a TEXT. Reads each line and Parses it. Quick and dirty test
    CBTest=CLIPBOARD()  !CBTest is a TEXT control off Window on the Right used to grab lines
    UNHIDE(?CBTest)
    DISPLAY
    DBPb4# = DBParsing
    DBParsing = CHOOSE(UPPER(ExportString[1])='D',1,0)    !Turn off parsing debug unless DTEST
    DB('CB TEST - TOP #{60}')
    LOOP L#=1 TO ?CBTest{PROP:LineCount}
         IF ~?CBTest{PROP:Line,L#} THEN CYCLE.
         ExportString=?CBTest{PROP:Line,L#}
         DB('CB TEST Input: ' & CLIP(ExportString) &' ={30}')
         DO CleanExportStringRtn
         IF ~ExportString THEN CYCLE.
         Parser.Parse(ExportString, qParameters)
         DO BuildProtoMapStringRtn
         DB('CB TEST Output: ' & CLIP(ProtoMapString) )
         IF DBParsing THEN DB('={60}')                                .
    END
    DB('CB TEST - End - Lines=' & L# &' #{60}')
    DBParsing = DBPb4#
    DISPLAY
    EXIT
ExpParser.Construct PROCEDURE()
    CODE
        
ExpParser.Destruct  PROCEDURE()
    CODE
        SELF.ExpString &= NULL
        
ExpParser.Destroy   PROCEDURE()
    CODE
        
ExpParser.Parse     PROCEDURE(*STRING ExpString, *TYPE_TypesQueue q)
Token                   STRING(255)

ParseState              LONG
                        ITEMIZE
PARSESTATE:ParameterInit     EQUATE
PARSESTATE:ParameterPreamble EQUATE     !ROP for R=*XXXX  P=<*XXX>  O=<OOO>
PARSESTATE:ParameterARRAY    EQUATE     !AAA for Array[,,] always with R or P
PARSESTATE:ParameterType     EQUATE
PARSESTATE:ParameterDone     EQUATE
                        END
ParameterCounter        LONG
ParameterStartPos       LONG
    CODE
        IF DBParsing THEN DB('Parse: Top ExpString=' & ExpString ).
        SELF.ExpString &= ExpString
        FREE(q)
        IF SELF.ExpString &= NULL THEN RETURN.
        
        SELF.ExpStringLength = LEN(CLIP(SELF.ExpString))
        SELF.CharacterIndex  = 0
        ParameterCounter     = 0
        ParseState           = PARSESTATE:ParameterInit     !Suggest DO SetStateForNextParameterRtn
        ParameterStartPos    = 1

        Token = SELF.GetToken()
        LOOP WHILE Token <> ''
            IF DBParsing THEN DB('Parse: LOOP Index=' & SELF.CharacterIndex &' ParseState='& ParseState &' Token=' & Token ).
            CASE ParseState
            OF PARSESTATE:ParameterInit
                CLEAR(q)                            ! What is really happening
                q.TokenStartPos          = SELF.CharacterIndexStart
                q.TokenLength            = 0
                q.Tokens                 = ''
                q.ParameterType          = 'unknown'
                q.ParameterName          = ''
                q.IsOptionalYN           = False
                q.IsReferenceYN          = False
                q.IsRaw                  = False
                q.ArrayDIMs              = 0
                q.Prototype              = ''  !Carl asks: should this be CLEAR(Q) so if new field is added its cleared ?
                                               !Owen:      Yes it should be a CLEAR(Q).  I did it this way to make it obvious when those flags are getting reset.
                IF SELF.IsTokenPreamble(Token)
                    ParseState = PARSESTATE:ParameterPreamble
                    CYCLE
                ELSE
                    ParseState = PARSESTATE:ParameterType
                    CYCLE
                END
                
            OF PARSESTATE:ParameterPreamble
                CASE Token
                OF MANGLECODE:PassByRef
                    q.IsReferenceYN = TRUE
                OF MANGLECODE:Optional
                    q.IsOptionalYN = TRUE
                OF MANGLECODE:PassByRefOptional
                    q.IsReferenceYN = TRUE
                    q.IsOptionalYN = TRUE
                END
                
                ParseState = PARSESTATE:ParameterARRAY      !Array[,,] may come after * or <*>, always by *

            OF PARSESTATE:ParameterARRAY        !AAA always after ROP
                CASE Token
                OF MANGLECODE:Array
                   q.ArrayDIMs += 1             !Add an ARRAY [,,,] DIM(,,,)
                   !Fall thru for next GetToken could be more 'A'
                OF MANGLECODE:TopSpeedC_CONST                   
                   q.IsConstYN = 1
                   !Fall thru for next GetToken could be 'A'
                ELSE
                   ParseState = PARSESTATE:ParameterType   !No more AAA so move to Param Types
                   CYCLE                                   !We have a Token that must be processed as a Param Type
                END

            OF PARSESTATE:ParameterType
                IF DBParsing THEN DB('Parse: PARSESTATE:ParameterType Token=' & Token ).
                CASE Token
                OF MANGLECODE:File;        q.ParameterType = TYPE:File
                OF MANGLECODE:Blob;        q.ParameterType = TYPE:Blob
                OF MANGLECODE:Key;         q.ParameterType = TYPE:Key
                OF MANGLECODE:Queue;       q.ParameterType = TYPE:Queue
                OF MANGLECODE:Report;      q.ParameterType = TYPE:Report
                OF MANGLECODE:Window;      q.ParameterType = TYPE:Window
                OF MANGLECODE:View;        q.ParameterType = TYPE:View
                OF MANGLECODE:Application; q.ParameterType = TYPE:Application
                OF MANGLECODE:Byte;        q.ParameterType = TYPE:Byte
                OF MANGLECODE:Ushort;      q.ParameterType = TYPE:Ushort
                OF MANGLECODE:Ulong;       q.ParameterType = TYPE:Ulong
                OF MANGLECODE:Date;        q.ParameterType = TYPE:Date
                OF MANGLECODE:Time;        q.ParameterType = TYPE:Time
                OF MANGLECODE:Bfloat4;     q.ParameterType = TYPE:Bfloat4
                OF MANGLECODE:Bfloat8;     q.ParameterType = TYPE:Bfloat8
                OF MANGLECODE:String;      q.ParameterType = TYPE:String
                OF MANGLECODE:Pstring;     q.ParameterType = TYPE:Pstring
                OF MANGLECODE:Cstring;     q.ParameterType = TYPE:Cstring
                OF MANGLECODE:Short;       q.ParameterType = TYPE:Short
                OF MANGLECODE:Long;        q.ParameterType = TYPE:Long
                OF MANGLECODE:Sreal;       q.ParameterType = TYPE:Sreal
                OF MANGLECODE:Real;        q.ParameterType = TYPE:Real
                OF MANGLECODE:Decimal;     q.ParameterType = TYPE:Decimal
                OF MANGLECODE:Pdecimal;    q.ParameterType = TYPE:Pdecimal
                OF MANGLECODE:CstringRaw
                    q.ParameterType = TYPE:Cstring
                    q.IsRaw         = True
                OF MANGLECODE:GroupRaw
                    q.ParameterType = TYPE:Group
                    q.IsRaw         = True
                OF MANGLECODE:Group;       q.ParameterType = TYPE:Group
                OF MANGLECODE:Any;         q.ParameterType = TYPE:Any
                OF MANGLECODE:Function;    q.ParameterType = TYPE:Function
                OF MANGLECODE:FunctionEnd; q.ParameterType = TYPE:FunctionEnd 
                
                OF MANGLECODE:TopSpeedC_INT  ; q.ParameterType = TYPE:TopSpeedC_INT
                OF MANGLECODE:TopSpeedC_Uint ; q.ParameterType = TYPE:TopSpeedC_Uint

                ELSE
                    q.ParameterType = Token     !Likely a Named Type e.g. Exp=5MYQUE is MapParm=MYQUE
                END
                IF DBParsing THEN DB('Parse: PARSESTATE:ParameterType Token=' & Token &' ParameterType=' & q.ParameterType).
                ParseState = PARSESTATE:ParameterDone
                CYCLE
                
            OF PARSESTATE:ParameterDone
                ParameterCounter += 1
                q.ParameterNumber = ParameterCounter
                q.ParameterName   = 'Parm_' & ParameterCounter 
                q.ProtoType       = SELF.BuildPrototype(q.ParameterType, q.ParameterName, q.IsReferenceYN, q.IsOptionalYN, q.ArrayDIMs, q.IsConstYN)
                !q.TokenStartPos  =  ! Set on initialisation of queue record
                q.TokenLength     = SELF.CharacterIndex + 1 - q.TokenStartPos
                q.Tokens          = SUB(SELF.ExpString, q.TokenStartPos, q.TokenLength)
                ADD(q)
                
                ParseState        = PARSESTATE:ParameterInit    !Suggest DO SetStateForNextParameterRtn so Q is cleared and ready to go
                ParameterStartPos = SELF.CharacterIndex + 1
            END
            
            Token = SELF.GetToken()
        END

!Suggest Q Init code should be in a ROUTINE that is DO instead of "ParseState = PARSESTATE:ParameterInit" in 2 placesss0 Q is ready to go
!!!SetStateForNextParameterRtn ROUTINE     !Because GetToken() takes tokens off the input want Q ready to go 
!!!     ParseState = PARSESTATE:ParameterInit
!!!     CLEAR(q)
!!!     q.Xxxx =     etc


ExpParser.GetToken  PROCEDURE()
ReturnValue             ANY
UserTypeLength          LONG

ParseState              LONG
                        ITEMIZE
PARSESTATE:InitialPass      EQUATE
PARSESTATE:DefinedMultiCharacter  EQUATE
PARSESTATE:UserTypePrefix   EQUATE
                        END

    CODE
        ReturnValue         = ''
        ParseState          = PARSESTATE:InitialPass
        SELF.CharacterIndexStart = SELF.CharacterIndex +1
        
        LOOP WHILE SELF.CharacterIndex < SELF.ExpStringLength
            IF DBParsing THEN DB('GetToken: LOOP Index=' & SELF.CharacterIndex &' ParseState='& ParseState &' ExpString[]=' & SELF.ExpString[ (SELF.CharacterIndex +1) ] ).
            CASE ParseState
            OF PARSESTATE:InitialPass
                CASE SELF.ExpString[ (SELF.CharacterIndex +1) ]
                    
                OF MANGLECODE:EntityPrefix
                OROF MANGLECODE:UnsignedPrefix
                OROF MANGLECODE:StringEntity
                OROF MANGLECODE:Complex
                    ReturnValue = SELF.ExpString[ (SELF.CharacterIndex +1) ]
                    ParseState = PARSESTATE:DefinedMultiCharacter
                    
                OF MANGLECODE:Function
                OROF MANGLECODE:FunctionEnd
                    ReturnValue = SELF.ExpString[ (SELF.CharacterIndex +1) ]
                    SELF.CharacterIndex += 1    ! Character Index needs to be pointing at start of next string when called again
                    BREAK
                   
                OF MANGLECODE:PassByRef
                OROF MANGLECODE:PassByRefOptional
                OROF MANGLECODE:Optional
                OROF MANGLECODE:Array
                OROF MANGLECODE:Long
                OROF MANGLECODE:Sreal
                OROF MANGLECODE:Real
                OROF MANGLECODE:Decimal
                OROF MANGLECODE:Pdecimal
                OROF MANGLECODE:CstringRaw
                OROF MANGLECODE:GroupRaw
                OROF MANGLECODE:Group
                OROF MANGLECODE:Any
                OROF MANGLECODE:Any2
                OROF MANGLECODE:TopSpeedC_INT
                OROF MANGLECODE:TopSpeedC_CONST
                    ReturnValue = SELF.ExpString[ (SELF.CharacterIndex +1) ]
                    SELF.CharacterIndex += 1    ! Character Index needs to be pointing at start of next string when called again
                    BREAK
                    
                OF '0' TO '9'
                    ! User defined type.
                    ! Keep current character position
                    ! But change state so correct handler gets character stream.
                    ParseState = PARSESTATE:UserTypePrefix
                    CYCLE
                END

            OF PARSESTATE:DefinedMultiCharacter
                CASE CLIP(ReturnValue)
                OF MANGLECODE:EntityPrefix
                    CASE SELF.ExpString[ (SELF.CharacterIndex +1) ]
                    OF SUB(MANGLECODE:File, 2, 1)
                    OROF SUB(MANGLECODE:Blob, 2, 1)
                    OROF SUB(MANGLECODE:Key, 2, 1)
                    OROF SUB(MANGLECODE:Queue, 2, 1)
                    OROF SUB(MANGLECODE:Report, 2, 1)
                    OROF SUB(MANGLECODE:Window, 2, 1)
                    OROF SUB(MANGLECODE:View, 2, 1)
                    OROF SUB(MANGLECODE:Application, 2, 1)
                        ReturnValue = CLIP(ReturnValue) & SELF.ExpString[ (SELF.CharacterIndex +1) ]
                        SELF.CharacterIndex += 1
                        BREAK
                    ELSE
                        MESSAGE('Unexpected character for Entity' & |
                                ' "'& CLIP(ReturnValue) & SELF.ExpString[ (SELF.CharacterIndex +1) ] &'" @ Position ' & SELF.CharacterIndex +1 & |
                                '||PARSESTATE:DefinedMultiCharacter','ExpParser.GetToken')
                        BREAK
                    END
                    
                OF MANGLECODE:UnsignedPrefix
                    CASE SELF.ExpString[ (SELF.CharacterIndex +1) ]
                    OF SUB(MANGLECODE:Byte, 2, 1)
                    OROF SUB(MANGLECODE:Ushort, 2, 1)
                    OROF SUB(MANGLECODE:Ulong, 2, 1)
                    OROF SUB(MANGLECODE:TopSpeedC_Uint, 2, 1)
                        ReturnValue = CLIP(ReturnValue) & SELF.ExpString[ (SELF.CharacterIndex +1) ]
                        SELF.CharacterIndex += 1
                        BREAK
                    ELSE
                        MESSAGE('Unexpected character for Unsigned' & |
                                ' "'& CLIP(ReturnValue) & SELF.ExpString[ (SELF.CharacterIndex +1) ] &'" @ Position ' & SELF.CharacterIndex +1 & |
                                '||MANGLECODE:UnsignedPrefix','ExpParser.GetToken')
                        BREAK
                    END
                    
                OF MANGLECODE:Complex
                    CASE SELF.ExpString[ (SELF.CharacterIndex +1) ]
                    OF SUB(MANGLECODE:Date, 2, 1)
                    OROF SUB(MANGLECODE:Time, 2, 1)
                    OROF SUB(MANGLECODE:Bfloat4, 2, 1)
                    OROF SUB(MANGLECODE:Bfloat8, 2, 1)
                        ReturnValue = CLIP(ReturnValue) & SELF.ExpString[ (SELF.CharacterIndex +1) ]
                        SELF.CharacterIndex += 1
                        BREAK
                    ELSE
                        MESSAGE('Unexpected character for Complex' & |
                                ' "'& CLIP(ReturnValue) & SELF.ExpString[ (SELF.CharacterIndex +1) ] &'" @ Position ' & SELF.CharacterIndex +1 & |
                                '||MANGLECODE:Complex','ExpParser.GetToken')
                        BREAK
                    END
                    
                OF MANGLECODE:StringEntity
                    CASE SELF.ExpString[ (SELF.CharacterIndex +1) ]
                    OROF SUB(MANGLECODE:String, 2, 1)
                    OROF SUB(MANGLECODE:Pstring, 2, 1)
                    OROF SUB(MANGLECODE:Cstring, 2, 1)
                        ReturnValue = CLIP(ReturnValue) & SELF.ExpString[ (SELF.CharacterIndex +1) ]
                        SELF.CharacterIndex += 1
                        BREAK
                    ELSE
                        ! The data type found is a short.  ReturnValue has already been assigned the value.
                        ! We need to keep the current character position for the next call.
                        ! So we do nothing except break out.
                        BREAK
                    END
                END
            OF PARSESTATE:UserTypePrefix
                CASE SELF.ExpString[ (SELF.CharacterIndex +1) ]
                OF '0' TO '9'
                ELSE
                    ! Our character indexes are zero based.
                    ! Clarion strings are one based arrays
                    ! We have read past the size parameter, so we need to keep the current character position for the next state
                    UserTypeLength = SELF.ExpString[ SELF.CharacterIndexStart : SELF.CharacterIndex ]
                    ReturnValue    = SELF.ExpString[ SELF.CharacterIndex +1 :  SELF.CharacterIndex + UserTypeLength]
                    SELF.CharacterIndex += UserTypeLength
                    BREAK
                END
                
            END
            
            SELF.CharacterIndex += 1    
        END
        IF DBParsing THEN DB('GetToken: Return Index=' & SELF.CharacterIndex &' ReturnValue='& ReturnValue ).
        RETURN(ReturnValue)
        
ExpParser.IsTokenPreamble   PROCEDURE(STRING Token)     !Token is R,P,O = *XXX <*XX> <XXX>
ReturnValue                     BOOL

    CODE
        ReturnValue = FALSE
        CASE Token
        OF MANGLECODE:PassByRef
        OROF MANGLECODE:PassByRefOptional
        OROF MANGLECODE:Optional
            ReturnValue = TRUE
        END
        RETURN(ReturnValue)

    OMIT('**END OB**')
ExpParser.BuildPrototype        PROCEDURE(STRING ParameterType, STRING ParameterName, BOOL IsReferenceYN, BOOL IsOptionalYN, LONG ArrayDIMs)
ReturnValue                         ANY
    CODE
        ReturnValue = CLIP(ParameterType)                        !Prototype=TYPE
        
        IF ArrayDIMs <> 0
            ReturnValue = CLIP(ReturnValue) & '[' & ALL(',',ArrayDIMs-1) & ']'    !Add Array=TYPE[]
        END
        ReturnValue = CLIP(ReturnValue) & ' ' & CLIP(ParameterName)               !Add Parm=TYPE[] Parm_#
        IF IsReferenceYN
           ReturnValue = '*' & ReturnValue                                        !Add *Ref=*TYPE[] Parm_#
        END
        IF IsOptionalYN
           ReturnValue = '<<' & CLIP(ReturnValue) &'>'                            !Add <Omit>=<TYPE[] Parm_#>
        END
        
        RETURN(CLIP(ReturnValue))
    !end of OMIT('**END OB**')

ExpParser.BuildPrototype        PROCEDURE(STRING ParameterType, STRING ParameterName, BOOL IsReferenceYN, BOOL IsOptionalYN, LONG ArrayDIMs, BOOL IsConstYN)
            !Parameter <CONST *REF Type [Array,,,] Label >
Optional    GROUP
AngelOpen      PSTRING('<<')    !Note _P_ PStrings so No Clip() needed
AngelClose     PSTRING('>')
            END
CONST_          PSTRING('CONST ')            
ReferenceBug    PSTRING('*')    !FYI typesetter slang for an Asterisk is a Bug. Further an Apprentice Jockey is called a Bug Boy because weight as Asterisk *121
ArrayBrackets   PSTRING(64)     !Set to [,,,]  There is no limit to the number of dimensions... so 60 seems 5X what anyone would use
    CODE 
    IF ~IsConstYN     THEN CLEAR(CONST_).
    IF ~IsOptionalYN  THEN CLEAR(Optional).
    IF ~IsReferenceYN THEN CLEAR(ReferenceBug).
    IF ArrayDIMs      THEN ArrayBrackets = '[' & ALL(',',ArrayDIMs-1) & ']'.

                                  !Parameter <*Type[Array] Label>
    RETURN Optional.AngelOpen       & |     !< 
              CONST_                & |     ! CONST
              ReferenceBug          & |     !       *
              CLIP(ParameterType)   & |     !        TYPE
              ArrayBrackets         & |     !            [,,,,,]
              ' '                   & |     !                   _             space between TYPE and LABEL
              CLIP(ParameterName)   & |     !                    LABEL        aka Parm Name
           Optional.AngelClose              !                         >
!----------------------------------------
DB   PROCEDURE(STRING xMessage)
Prfx EQUATE('ExpParser: ')
sz   CSTRING(SIZE(Prfx)+SIZE(xMessage)+3),AUTO
  CODE 
  sz = Prfx & CLIP(xMessage) & '<13,10>'
  OutputDebugString( sz )
  RETURN