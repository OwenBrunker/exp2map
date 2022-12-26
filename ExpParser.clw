
  PROGRAM


MANGLECODE:PassByRef  			 		 EQUATE('R')
MANGLECODE:PassByRefOptional     EQUATE('P')
MANGLECODE:Optional   			 	   EQUATE('O')
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


TYPE_TypesQueue     QUEUE,TYPE
ParameterType           STRING(255)
ParameterName           STRING(255)
IsOptionalYN            BOOL   ! O
IsReferenceYN           BOOL   ! R
IsOptionalReferenceYN   BOOL   ! P
                    END


    MAP
    END



ExpParser           CLASS,TYPE
ExpString                &STRING
ExpStringLength          LONG
CharacterIndex           LONG

Construct               PROCEDURE()
Destruct                PROCEDURE()
Destroy                 PROCEDURE()
Parse                   PROCEDURE(*STRING ExpString, *TYPE_TypesQueue q)
GetToken                PROCEDURE(),STRING
                    END



  CODE

        


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

    CODE
        SELF.ExpString &= ExpString
        FREE(q)
        IF SELF.ExpString &= NULL THEN RETURN.
        
        SELF.ExpStringLength = LEN(CLIP(SELF.ExpString))
        SELF.CharacterIndex  = 0
        
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
                END
                
                ParseState = PARSESTATE:ParameterType
                
            OF PARSESTATE:ParameterType
                CASE Token
                END
                
            OF PARSESTATE:ParameterDone
                ADD(q)
                ParseState = PARSESTATE:ParameterInit
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
                    ReturnValue    = SELF.ExpString[ SELF.CharacterIndex +1 :  SELF.CharacterIndex + UserTypeLength +1]
                    SELF.CharacterIndex += UserTypeLength
                    BREAK
                END
                
            END
            
            SELF.CharacterIndex += 1    
        END
        RETURN(ReturnValue)
        
        
