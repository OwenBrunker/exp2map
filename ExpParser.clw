
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
!EndRegion Equates

!Region QUEUEs
TYPE_TypesQueue     QUEUE,TYPE
TokenStartPos           USHORT          !SUB(Input, StartPos
TokenLength             USHORT          !SUB(Input, StartPos, Length)
Tokens                  STRING(255)
ParameterType           STRING(255)
ParameterName           STRING(255)
IsOptionalYN            BOOL   ! O
IsReferenceYN           BOOL   ! R
IsOptionalReferenceYN   BOOL   ! P
IsRaw                   BOOL   ! Applies to cstrings, and groups
ArrayCount              BYTE   ! A
ProtoType               STRING(255)
                    END

TYPE_TokensQueue    QUEUE
ScanToken               STRING(2)
ReturnToken             STRING(10)
                    END
!EndRegion QUEUEs

    MAP
TestWindow          PROCEDURE()
    END



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
BuildPrototype          PROCEDURE(STRING ParameterType, STRING ParameterName, BOOL IsReferenceYN, BOOL IsOptionalYN, BOOL IsOptionalReferenceYN, LONG ArrayCount),STRING
                    END



  CODE
        TestWindow()
        
TestWindow          PROCEDURE()
AT_Pos                  LONG
ExportString            STRING(2048)
ProtoMapString             STRING(2048)            !Prototype of all parameters in Queue e.g. (<LONG P1>)
qParameters             QUEUE(TYPE_TypesQueue).


Window                  WINDOW('EXP to MAP'),AT(,,371,202),GRAY,SYSTEM,ICON(ICON:Thumbnail), |
                            FONT('Segoe UI',9)  ,RESIZE
                            PROMPT('Export String:'),AT(2,18),USE(?ExportString:PROMPT)
                            TEXT,AT(47,18,303,10),USE(ExportString),SINGLE,FONT('Consolas')
                            TEXT,AT(45,32,,12),FULL,USE(ProtoMapString),SINGLE,TRN,READONLY,SKIP,FONT('Consolas')
                            BUTTON,AT(355,18,10,10),USE(?ExpPasteBtn),SKIP,ICON(ICON:Paste), |
                                TIP('Paste clipboard into Export String')
                            PROMPT('Only include the portion of the export string after @F'),AT(46,5), |
                                USE(?PROMPT2)
                            BUTTON('&Parse'),AT(46,47,35),USE(?Parse)
        LIST,AT(2,71),FULL,USE(?LIST:Parameters),FROM(qParameters),VSCROLL, |
          FORMAT('[' &|
                   '16R@N_6~ ,~@'            &|
                   '14L(1)@S4@'              &|
                              ']|~Position~' &|
                 '28L(2)|M~Tokens~'          &|
                 '60L(2)|M~Type~'            &|
                 '36L(2)|M~Name~'            &|
                 '36C|M~<<Optional>~@N1b@'   &|
                 '40C|M~*Reference~@N1b@'    &|
                 '36C|M~<<*Ref Opt>~@N1b@'   &|
                 '20C|M~Raw~@N1b@'           &|
                 '20L(5)|M~Array~C(0)@N2b@'  &|
                 '80L(2)~Prototype~'       )
                            BUTTON('Cl&ose'),AT(93,47,35),USE(?Close)
                            BUTTON('Re-Run'),AT(330,47,35),USE(?ReRun),TIP('Run another instance')
                        END

parser                  ExpParser

    CODE
! ExportString='PAAlllsbRsbOsbPsb' !test:  (<*LONG[,] A2dim>,LONG SheetFEQ,BOOL Wrap=0,STRING S1,*STRING S2,<STRING S3>,<*STRING S4>)
! ExportString='BwBrBfBbBkBqBiBa'  !test:  (*WINDOW W1,*REPORT R1>,FILE F1,BLOB B1,KEY K1,QUEUE Q1,VIEW V1,APPLICATION A1)  
! ExportString='Bqg'              !test:  (QUEUE Q1,GROUP G1)
! ExportString='5MYQUE7MYGROUP'    !test:  (MyQue Q1,MyGroup G1)       Named Types

        SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY
        SYSTEM{PROP:PropVScroll}=1
        OPEN(Window)
        0{PROP:MinWidth}=0{PROP:Width} ; 0{PROP:MinHeight}=0{PROP:Height} * .60
        ACCEPT
            CASE ACCEPTED()
            OF ?ExpPasteBtn ; IF ~CLIPBOARD() THEN CYCLE. 
                              ExportString=CLIPBOARD() ; DISPLAY
                              POST(EVENT:Accepted,?ExportString)
                              POST(EVENT:Accepted,?Parse)
            OF ?ExportString
               AT_Pos=INSTRING('@F',ExportString,1)   !Did they paste the Function@F 
               IF AT_Pos THEN ExportString=SUB(ExportString,AT_Pos+2,9999). !Start after @F
               AT_Pos=INSTRING(' @?',ExportString,1)  !Did they paste EXP Line with @?
               IF AT_Pos THEN ExportString=SUB(ExportString,1,AT_Pos-1).    !Cutoff @?
               ExportString=LEFT(ExportString)
               DISPLAY 

            OF ?Parse
                Parser.Parse(ExportString, qParameters)
                DO BuildProtoMapStringRtn
                DISPLAY()
                
            OF ?Close
                POST(EVENT:CloseWindow)
            OF ?ReRun
                RUN(COMMAND('0'))
            END
        END
        CLOSE(Window)    

BuildProtoMapStringRtn ROUTINE  !Format MAP Prototype from qParameters - probably should be in Class
    DATA
QX LONG
    CODE
    ProtoMapString='('
    LOOP QX=1 TO RECORDS(qParameters)
        GET(qParameters,QX)
        ProtoMapString=CLIP(ProtoMapString) & |
                    CHOOSE(QX=1,'',',') & |
                    qParameters.ProtoType
    END
    ProtoMapString=CLIP(ProtoMapString) & ')' 
    ?ProtoMapString{PROP:Tip}='Label' & ProtoMapString
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
                q.IsOptionalReferenceYN  = False
                q.IsRaw                  = False
                q.ArrayCount             = 0
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
                    q.Tokens = CLIP(q.Tokens) & Token
                OF MANGLECODE:Optional
                    q.IsOptionalYN = TRUE
                    q.Tokens = CLIP(q.Tokens) & Token
                OF MANGLECODE:PassByRefOptional
                    q.IsOptionalReferenceYN = TRUE
                    q.Tokens = CLIP(q.Tokens) & Token
                END
                
                ParseState = PARSESTATE:ParameterARRAY  !Carl was: PARSESTATE:ParameterType

            OF PARSESTATE:ParameterARRAY        !AAA always after ROP
                CASE Token
                OF MANGLECODE:Array
                   q.ArrayCount += 1  !Increment ARRAY
                    q.Tokens     = CLIP(q.Tokens) & Token
                   !Fall thru for next GetToken could be more 'A' 
                ELSE
                   ParseState = PARSESTATE:ParameterType   !No more AAA so move to Param Types
                   CYCLE                                   !We have a Token that must be processed as a Param Type
                END

            OF PARSESTATE:ParameterType
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
                    q.Tokens        = CLIP(q.Tokens) & Token
                OF MANGLECODE:GroupRaw
                    q.ParameterType = TYPE:Group
                    q.IsRaw         = True
                    q.Tokens        = CLIP(q.Tokens) & Token
                OF MANGLECODE:Group;       q.ParameterType = TYPE:Group
                OF MANGLECODE:Any;         q.ParameterType = TYPE:Any
                OF MANGLECODE:Function;    q.ParameterType = TYPE:Function
                OF MANGLECODE:FunctionEnd; q.ParameterType = TYPE:FunctionEnd
                ELSE
                    q.ParameterType = Token     !Likely a Named Type e.g. Exp=5MYQUE is MapParm=MYQUE
                    q.Tokens        = CLIP(q.Tokens) & Token
                END
                ParseState = PARSESTATE:ParameterDone
                CYCLE
                
            OF PARSESTATE:ParameterDone
                ParameterCounter += 1
                q.ParameterName   = 'Parm_' & ParameterCounter 
                q.ProtoType       = SELF.BuildPrototype(q.ParameterType, q.ParameterName, q.IsReferenceYN, q.IsOptionalYN, q.IsOptionalReferenceYN, q.ArrayCount)
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

ExpParser.BuildPrototype        PROCEDURE(STRING ParameterType, STRING ParameterName, BOOL IsReferenceYN, BOOL IsOptionalYN, BOOL IsOptionalReferenceYN, LONG ArrayCount)
ReturnValue                         ANY
    CODE
        ReturnValue = CLIP(ParameterType)                        !Prototype=TYPE
        
        IF ArrayCount <> 0
            ReturnValue = CLIP(ReturnValue) & '[' & ALL(',',ArrayCount-1) & ']'   !Add Array=TYPE[]
        END
        ReturnValue = CLIP(ReturnValue) & ' ' & CLIP(ParameterName)               !Add Parm=TYPE[] Parm_#
        IF IsReferenceYN
           ReturnValue = '*' & ReturnValue                                        !Add *Ref=*TYPE[] Parm_#
        ELSIF IsOptionalYN
           ReturnValue = '<<' & CLIP(ReturnValue) &'>'                            !Add <Omit>=<TYPE[] Parm_#>
        ELSIF IsOptionalReferenceYN
           ReturnValue = '<<*' & CLIP(ReturnValue) &'>'                           !Add <*Omit>=<*TYPE[] Parm_#>
        END
        
        RETURN(CLIP(ReturnValue))
        