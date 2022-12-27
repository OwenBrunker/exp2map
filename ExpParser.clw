
  PROGRAM

OMIT('***')
 * Created with Clarion 11.1
 * User: owen
 * Date: 23/12/22
 * Time: 4:40 PM
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 ***

MANGLECODE:PassByRef  			 EQUATE('R')
MANGLECODE:PassByRefOptional     EQUATE('P')
MANGLECODE:Optional   			 EQUATE('O')
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


TYPE_TypesQueue     QUEUE,TYPE
ParameterType           STRING(255)
ParameterName           STRING(255)
IsOptionalYN            BOOL   ! O
IsReferenceYN           BOOL   ! R
IsOptionalReferenceYN   BOOL   ! P
                    END

TYPE_TokensQueue    QUEUE
ScanToken               STRING(2)
ReturnToken             STRING(10)
                    END


    MAP
TestWindow          PROCEDURE()
    END



ExpParser           CLASS,TYPE
ExpString               &STRING
ExpStringLength          LONG
CharacterIndex           LONG

Construct               PROCEDURE()
Destruct                PROCEDURE()
Destroy                 PROCEDURE()
Parse                   PROCEDURE(*STRING ExpString, *TYPE_TypesQueue q)
GetToken                PROCEDURE(),STRING
IsTokenPreamble         PROCEDURE(STRING Token),BOOL
                    END



  CODE
        TestWindow()
        
TestWindow          PROCEDURE()

ExportString            STRING(2048)

qParameters             QUEUE(TYPE_TypesQueue).


Window                  WINDOW('Caption'),AT(,,371,252),GRAY,FONT('Segoe UI',9)
                            PROMPT('Export String:'),AT(2,20),USE(?ExportString:PROMPT)
                            TEXT,AT(47,18,322,10),USE(ExportString),SINGLE
                            PROMPT('Only include the portion of the export string after @F'),AT(46,34,323), |
                                USE(?PROMPT2)
                            BUTTON('Parse'),AT(46,47,35),USE(?Parse)
                            LIST,AT(47,71,322,159),USE(?LIST:Parameters),FROM(qParameters), |
                                FORMAT('73L(2)|M~Type~87L(2)|M~Name~38L(2)|M~Is Optional~@N01@41' & |
                                'L(2)|M~Is Reference~@N01@20L(2)|M~Is Optional Reference~@N01@')
                            BUTTON('Close'),AT(46,234),USE(?Close)
                        END

parser                  ExpParser

    CODE
        OPEN(Window)
        ACCEPT
            CASE FIELD()
            OF 0
            OF ?Parse
                CASE EVENT()
                OF EVENT:Accepted
                    Parser.Parse(ExportString, qParameters)
                    DISPLAY()
                END
                
            OF ?Close
                CASE EVENT()
                OF EVENT:Accepted
                   POST(EVENT:CloseWindow) 
                END
            END
            
        END
        CLOSE(Window)    

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
PARSESTATE:ParameterPreamble EQUATE
PARSESTATE:ParameterType     EQUATE
PARSESTATE:ParameterDone     EQUATE
                        END
ParameterCounter        LONG
    CODE
        SELF.ExpString &= ExpString
        FREE(q)
        IF SELF.ExpString &= NULL THEN RETURN.
        
        SELF.ExpStringLength = LEN(CLIP(SELF.ExpString))
        SELF.CharacterIndex  = 0
        ParameterCounter     = 0
        ParseState           = PARSESTATE:ParameterInit

        Token = SELF.GetToken()
        LOOP WHILE Token <> ''

            CASE ParseState
            OF PARSESTATE:ParameterInit
                q.ParameterType          = ''
                q.ParameterName          = ''
                q.IsOptionalYN           = ''
                q.IsReferenceYN          = ''
                q.IsOptionalReferenceYN  = ''
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
                    q.IsOptionalReferenceYN = TRUE
                END
                
                ParseState = PARSESTATE:ParameterType
                
            OF PARSESTATE:ParameterType
                CASE Token
                OF MANGLECODE:File
                    q.ParameterType = TYPE:File
                OF MANGLECODE:Blob
                    q.ParameterType = TYPE:Blob
                OF MANGLECODE:Key
                    q.ParameterType = TYPE:Key
                OF MANGLECODE:Queue
                    q.ParameterType = TYPE:Queue
                OF MANGLECODE:Report
                    q.ParameterType = TYPE:Report
                OF MANGLECODE:Window
                    q.ParameterType = TYPE:Window
                OF MANGLECODE:View
                    q.ParameterType = TYPE:View
                OF MANGLECODE:Application
                    q.ParameterType = TYPE:Application
                OF MANGLECODE:Byte
                    q.ParameterType = TYPE:Byte
                OF MANGLECODE:Ushort
                    q.ParameterType = TYPE:Ushort
                OF MANGLECODE:Ulong
                    q.ParameterType = TYPE:Ulong
                OF MANGLECODE:Date
                    q.ParameterType = TYPE:Date
                OF MANGLECODE:Time
                    q.ParameterType = TYPE:Time
                OF MANGLECODE:Bfloat4
                    q.ParameterType = TYPE:Bfloat4
                OF MANGLECODE:Bfloat8
                    q.ParameterType = TYPE:Bfloat8
                OF MANGLECODE:String
                    q.ParameterType = TYPE:String
                OF MANGLECODE:Pstring
                    q.ParameterType = TYPE:Pstring
                OF MANGLECODE:Cstring
                    q.ParameterType = TYPE:Cstring
                OF MANGLECODE:Short
                    q.ParameterType = TYPE:Short
                OF MANGLECODE:Long
                    q.ParameterType = TYPE:Long
                OF MANGLECODE:Sreal
                    q.ParameterType = TYPE:Sreal
                OF MANGLECODE:Real
                    q.ParameterType = TYPE:Real
                OF MANGLECODE:Decimal
                    q.ParameterType = TYPE:Decimal
                OF MANGLECODE:Pdecimal
                    q.ParameterType = TYPE:Pdecimal
                OF MANGLECODE:CstringRaw
                    q.ParameterType = TYPE:CstringRaw
                OF MANGLECODE:GroupRaw
                    q.ParameterType = TYPE:GroupRaw
                OF MANGLECODE:Group
                    q.ParameterType = TYPE:Group
                OF MANGLECODE:Any
                    q.ParameterType = TYPE:Any
                OF MANGLECODE:Function
                    q.ParameterType = TYPE:Function
                OF MANGLECODE:FunctionEnd
                    q.ParameterType = TYPE:FunctionEnd
                ELSE
                    q.ParameterType = Token
                END
                ParseState = PARSESTATE:ParameterDone
                CYCLE
                
            OF PARSESTATE:ParameterDone
                q.ParameterName = 'ParameterExpression' & ParameterCounter
                ADD(q)
                ParameterCounter += 1
                ParseState        = PARSESTATE:ParameterInit
            END
            
            Token = SELF.GetToken()
        END


ExpParser.GetToken  PROCEDURE()
ReturnValue             ANY
CharacterIndexStart     LONG
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
        CharacterIndexStart = SELF.CharacterIndex
        
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
                        MESSAGE('Unexpected character for Entity')
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
                        MESSAGE('Unexpected character for Unsigned')
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
                        MESSAGE('Unexpected character for Complex')
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
                    UserTypeLength = SELF.ExpString[ (CharacterIndexStart +1) : SELF.CharacterIndex ]
                    ReturnValue    = SELF.ExpString[ SELF.CharacterIndex +1 :  SELF.CharacterIndex + UserTypeLength]
                    SELF.CharacterIndex += UserTypeLength
                    BREAK
                END
                
            END
            
            SELF.CharacterIndex += 1    
        END
        RETURN(ReturnValue)
        
ExpParser.IsTokenPreamble   PROCEDURE(STRING Token)
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
        