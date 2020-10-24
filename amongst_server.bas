OPTION _EXPLICIT

DIM SHARED gameVersion AS INTEGER
'this is to be increased everytime the client
'becomes incompatible with previous versions
gameVersion = 2

$LET DEBUGGING = FALSE
$IF DEBUGGING = TRUE THEN
    $CONSOLE
$END IF

$CONSOLE:ONLY
_DEST _CONSOLE

CONST True = -1, False = 0

CONST id_SERVERFULL = 1
CONST id_PING = 2
CONST id_ID = 3
CONST id_NEWCOLOR = 4
CONST id_NEWNAME = 5
CONST id_COLOR = 6
CONST id_POS = 7
CONST id_NAME = 8
CONST id_CHAT = 9
CONST id_PLAYERONLINE = 10
CONST id_PLAYEROFFLINE = 11
CONST id_PONG = 12
CONST id_PLAYERQUIT = 13
CONST id_GAMEVERSION = 14
CONST id_SHOOT = 15
CONST id_SIZE = 16

TYPE object
    name AS STRING
    handle AS LONG
    x AS SINGLE
    y AS SINGLE
    state AS INTEGER
    color AS INTEGER
    basicInfoSent AS _BYTE
    broadcastOffline AS _BYTE
    ping AS SINGLE
    hasNewName AS _BYTE
    hasNewColor AS _BYTE
    hasNewPosition AS STRING
    hasNewMessage AS _BYTE
    hasNewSize AS _BYTE
    size AS INTEGER
END TYPE

DIM SHARED totalClients AS INTEGER
DIM SHARED playerStream(1 TO 10) AS STRING
DIM SHARED player(1 TO 10) AS object
DIM SHARED colors(1 TO 12) AS _UNSIGNED LONG
DIM i AS LONG, j AS LONG
DIM newClient AS LONG
DIM id AS INTEGER, value$
DIM packet$

DIM SHARED endSignal AS STRING
endSignal = CHR$(253) + CHR$(254) + CHR$(255)

CONST timeout = 30

DIM SHARED host AS LONG
PRINT "Starting server (ver. "; _TRIM$(STR$(gameVersion)); ")... ";
host = _OPENHOST("TCP/IP:51512")
IF host = 0 THEN
    PRINT "Cannot listen on port 51512"
    SYSTEM
END IF
PRINT "Listening on port 51512"

DO
    newClient = 0
    newClient = _OPENCONNECTION(host)
    IF newClient THEN
        IF totalClients < UBOUND(player) THEN
            totalClients = totalClients + 1
            FOR i = 1 TO UBOUND(player)
                IF player(i).state = False THEN
                    playerStream(i) = ""
                    player(i).color = 0
                    player(i).handle = newClient
                    player(i).state = True
                    player(i).broadcastOffline = False
                    player(i).size = 15
                    sendData player(i), id_GAMEVERSION, MKI$(gameVersion)
                    sendData player(i), id_ID, MKI$(i)

                    'send existing players' data:
                    FOR j = 1 TO UBOUND(player)
                        IF j = i THEN _CONTINUE
                        IF player(j).state = True THEN
                            sendData player(j), id_PLAYERONLINE, MKI$(i)

                            sendData player(i), id_PLAYERONLINE, MKI$(j)
                            sendData player(i), id_NAME, MKI$(j) + player(j).name
                            sendData player(i), id_COLOR, MKI$(j) + MKI$(player(j).color)
                            sendData player(i), id_POS, MKI$(j) + MKS$(player(j).x) + MKS$(player(j).y)
                            sendData player(i), id_SIZE, MKI$(j) + MKI$(player(j).size)
                        END IF
                    NEXT

                    player(i).ping = TIMER
                    EXIT FOR
                END IF
            NEXT
            PRINT "User at " + _CONNECTIONADDRESS$(newClient) + " connected as client #" + LTRIM$(STR$(i))
        ELSE
            packet$ = MKI$(id_SERVERFULL) + endSignal
            PUT #newClient, , packet$
            PRINT "Connection from " + _CONNECTIONADDRESS$(newClient) + " refused (server full)"
            CLOSE newClient
        END IF
    END IF

    FOR i = 1 TO UBOUND(player)
        IF player(i).state = False THEN
            IF player(i).broadcastOffline = False THEN
                player(i).broadcastOffline = True
                FOR j = 1 TO UBOUND(player)
                    IF j = i OR player(j).state = False THEN _CONTINUE
                    sendData player(j), id_PLAYEROFFLINE, MKI$(i)
                NEXT
            END IF
            _CONTINUE
        END IF

        player(i).hasNewName = False
        player(i).hasNewColor = False
        player(i).hasNewPosition = ""
        player(i).hasNewMessage = False
        player(i).hasNewSize = False

        IF TIMER - player(i).ping > timeout THEN
            'player inactive
            player(i).state = False
            CLOSE player(i).handle
            PRINT player(i).name + " lost connection."
            totalClients = totalClients - 1
            _CONTINUE
        END IF

        getData player(i), playerStream(i)

        DO WHILE parse(playerStream(i), id, value$)
            player(i).ping = TIMER
            SELECT CASE id
                CASE id_NAME
                    player(i).hasNewName = True
                    player(i).name = value$
                    DIM attempt AS INTEGER, checkAgain AS _BYTE, m$
                    m$ = ""
                    attempt = 0
                    DO
                        checkAgain = False
                        FOR j = 1 TO UBOUND(player)
                            IF j = i THEN _CONTINUE
                            IF attempt THEN m$ = STR$(attempt)
                            IF player(j).name = player(i).name + m$ THEN
                                attempt = attempt + 1
                                checkAgain = True
                                EXIT FOR
                            END IF
                        NEXT
                    LOOP WHILE checkAgain
                    IF attempt THEN
                        player(i).name = player(i).name + m$
                        sendData player(i), id_NEWNAME, player(i).name
                    END IF
                    PRINT "Client #" + LTRIM$(STR$(i)) + " has name " + player(i).name
                CASE id_COLOR 'received once per player
                    player(i).hasNewColor = True
                    DIM newcolor AS INTEGER, changed AS _BYTE
                    newcolor = CVI(value$)
                    changed = False
                    'check if this color is already in use, so another one can be assigned
                    FOR j = 1 TO UBOUND(player)
                        IF player(j).state = True AND player(j).color = newcolor THEN
                            newcolor = newcolor + 1
                            IF newcolor > UBOUND(colors) THEN newcolor = 1
                            changed = True
                            j = 0 'check again
                        END IF
                    NEXT
                    player(i).color = newcolor
                    IF changed THEN
                        sendData player(i), id_NEWCOLOR, MKI$(newcolor)
                    END IF
                CASE id_SHOOT
                    IF player(CVI(LEFT$(value$, 2))).size > 5 THEN
                        player(CVI(LEFT$(value$, 2))).size = player(CVI(LEFT$(value$, 2))).size - 2
                        FOR j = 1 TO UBOUND(player)
                            IF player(j).state = False THEN _CONTINUE
                            sendData player(j), id_SHOOT, MKI$(i) + LEFT$(value$, 2)
                            sendData player(j), id_SIZE, LEFT$(value$, 2) + MKI$(player(CVI(LEFT$(value$, 2))).size)
                        NEXT
                    END IF
                CASE id_POS
                    player(i).hasNewPosition = player(i).hasNewPosition + value$
                    player(i).x = CVS(LEFT$(value$, 4))
                    player(i).y = CVS(RIGHT$(value$, 4))
                CASE id_SIZE
                    player(i).hasNewSize = True
                    player(i).size = CVI(value$)
                CASE id_GAMEVERSION
                    'player is signaling it will disconnect due to wrong version
                    player(i).x = -1
                    player(i).y = -1
                CASE id_PLAYERQUIT
                    player(i).state = False
                    CLOSE player(i).handle
                    totalClients = totalClients - 1
                    PRINT "Client #" + LTRIM$(STR$(i)) + " quit";
                    IF player(i).x = -1 AND player(i).y = -1 THEN
                        PRINT " - wrong version."
                    ELSE
                        PRINT "."
                    END IF
                    EXIT DO
                CASE id_CHAT
                    DIM chatMessage$
                    player(i).hasNewMessage = True
                    chatMessage$ = value$
                CASE id_PING
                    sendData player(i), id_PONG, ""
            END SELECT
        LOOP

        IF player(i).state = False THEN
            _CONTINUE
        ELSE
            'send this player's data to everybody else
            FOR j = 1 TO UBOUND(player)
                IF j = i THEN _CONTINUE
                IF player(j).state = True THEN
                    IF player(i).hasNewName THEN sendData player(j), id_NAME, MKI$(i) + player(i).name
                    IF player(i).hasNewColor THEN sendData player(j), id_COLOR, MKI$(i) + MKI$(player(i).color)
                    IF LEN(player(i).hasNewPosition) THEN sendData player(j), id_POS, MKI$(i) + player(i).hasNewPosition
                    IF player(i).hasNewMessage THEN sendData player(j), id_CHAT, MKI$(i) + chatMessage$
                    IF player(i).hasNewSize THEN sendData player(j), id_SIZE, MKI$(i) + MKI$(player(i).size)
                END IF
            NEXT
        END IF
    NEXT

    _LIMIT 60
LOOP

SUB sendData (client AS object, id AS INTEGER, value$)
    DIM key$
    key$ = MKI$(id) + value$ + endSignal
    PUT #client.handle, , key$
END SUB

SUB getData (client AS object, buffer AS STRING)
    DIM incoming$
    GET #client.handle, , incoming$
    buffer = buffer + incoming$
END SUB

FUNCTION parse%% (buffer AS STRING, id AS INTEGER, value$)
    DIM endMarker AS LONG
    endMarker = INSTR(buffer, endSignal)
    IF endMarker THEN
        id = CVI(LEFT$(buffer, 2))
        value$ = MID$(buffer, 3, endMarker - 3)
        buffer = MID$(buffer, endMarker + LEN(endSignal))
        parse%% = True
    END IF
END FUNCTION


