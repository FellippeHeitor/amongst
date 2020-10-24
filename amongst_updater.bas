OPTION _EXPLICIT

$CONSOLE:ONLY
_DEST _CONSOLE

CONST true = -1, false = 0

DIM remoteFile$, localFile$
DIM newContents$

localFile$ = COMMAND$
IF _FILEEXISTS(localFile$) = false THEN
    PRINT "Incorrect usage."
    SYSTEM
END IF

IF INSTR(_OS$, "WIN") THEN
    remoteFile$ = "server_win.exe"
ELSEIF INSTR(_OS$, "MAC") THEN
    remoteFile$ = "server_mac"
ELSE
    remoteFile$ = "server_lnx"
END IF

IF _FILEEXISTS(remoteFile$) THEN
    OPEN remoteFile$ FOR BINARY AS #1
    newContents$ = SPACE$(LOF(1))
    GET #1, , newContents$
    CLOSE #1

    OPEN localFile$ FOR OUTPUT AS #1: CLOSE #1
    OPEN localFile$ FOR BINARY AS #1
    PUT #1, , newContents$
    CLOSE #1

    KILL remoteFile$
    SHELL _DONTWAIT localFile$
    PRINT "Update successful."
    SYSTEM
ELSE
    PRINT "Incorrect usage."
    SYSTEM
END IF

