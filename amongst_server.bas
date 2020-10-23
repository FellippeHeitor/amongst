OPTION _EXPLICIT

DIM SHARED gameVersion AS INTEGER
gameVersion = 1

$LET DEBUGGING = FALSE
$IF DEBUGGING = TRUE THEN
    $CONSOLE
$END IF

$CONSOLE:ONLY
_DEST _CONSOLE

CONST True = -1, False = 0

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
    hasNewPosition AS _BYTE
    hasNewMessage AS _BYTE
END TYPE

DIM SHARED totalClients AS INTEGER
DIM SHARED playerStream(1 TO 10) AS STRING
DIM SHARED player(1 TO 10) AS object
DIM SHARED colors(1 TO 15) AS _UNSIGNED LONG
DIM i AS LONG, j AS LONG
DIM newClient AS LONG
DIM key$, value$

DIM SHARED endSignal AS STRING
endSignal = "<" + CHR$(254) + ">"

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
    IF totalClients < UBOUND(player) THEN
        newClient = 0
        newClient = _OPENCONNECTION(host)
        IF newClient THEN
            totalClients = totalClients + 1
            FOR i = 1 TO UBOUND(player)
                IF player(i).state = False THEN
                    playerStream(i) = ""
                    player(i).color = 0
                    player(i).handle = newClient
                    player(i).state = True
                    player(i).broadcastOffline = False
                    sendData player(i), "ID", MKI$(i)

                    'send existing players' data:
                    FOR j = 1 TO UBOUND(player)
                        IF j = i THEN _CONTINUE
                        IF player(j).state = True THEN
                            sendData player(j), "PLAYERONLINE", MKI$(i)

                            sendData player(i), "PLAYERONLINE", MKI$(j)
                            sendData player(i), "PLAYERNAME", MKI$(j) + player(j).name
                            sendData player(i), "PLAYERCOLOR", MKI$(j) + MKI$(player(j).color)
                            sendData player(i), "PLAYERPOS", MKI$(j) + MKS$(player(j).x) + MKS$(player(j).y)
                        END IF
                    NEXT

                    player(i).ping = TIMER
                    EXIT FOR
                END IF
            NEXT
            PRINT "User at " + _CONNECTIONADDRESS$(newClient) + " connected as client #" + LTRIM$(STR$(i))
        END IF
    END IF

    FOR i = 1 TO UBOUND(player)
        IF player(i).state = False THEN
            IF player(i).broadcastOffline = False THEN
                player(i).broadcastOffline = True
                FOR j = 1 TO UBOUND(player)
                    IF j = i OR player(j).state = False THEN _CONTINUE
                    sendData player(j), "PLAYEROFFLINE", MKI$(i)
                NEXT
            END IF
            _CONTINUE
        END IF

        player(i).hasNewName = False
        player(i).hasNewColor = False
        player(i).hasNewPosition = False
        player(i).hasNewMessage = False

        IF TIMER - player(i).ping > timeout THEN
            'player inactive
            player(i).state = False
            CLOSE player(i).handle
            PRINT player(i).name + " lost connection."
            totalClients = totalClients - 1
            _CONTINUE
        END IF

        getData player(i), playerStream(i)

        DO WHILE parse(playerStream(i), key$, value$)
            player(i).ping = TIMER
            SELECT CASE key$
                CASE "NAME"
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
                        sendData player(i), "NEWNAME", player(i).name
                    END IF
                    PRINT "Client #" + LTRIM$(STR$(i)) + " has name " + player(i).name
                CASE "COLOR" 'received once per player
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
                        sendData player(i), "NEWCOLOR", MKI$(newcolor)
                    END IF
                CASE "PLAYERPOS"
                    player(i).hasNewPosition = True
                    player(i).x = CVI(LEFT$(value$, 2))
                    player(i).y = CVI(RIGHT$(value$, 2))
                CASE "PLAYERQUIT"
                    player(i).state = False
                    CLOSE player(i).handle
                    totalClients = totalClients - 1
                    PRINT "Client #" + LTRIM$(STR$(i)) + " quit."
                    EXIT DO
                CASE "CHAT"
                    DIM chatMessage$
                    player(i).hasNewMessage = True
                    chatMessage$ = value$
                CASE "PING"
                    sendData player(i), "PONG", ""
            END SELECT
        LOOP

        IF player(i).state = False THEN
            _CONTINUE
        ELSE
            'send this player's data to everybody else
            FOR j = 1 TO UBOUND(player)
                IF j = i THEN _CONTINUE
                IF player(j).state = True THEN
                    IF player(i).hasNewName THEN sendData player(j), "PLAYERNAME", MKI$(i) + player(i).name
                    IF player(i).hasNewColor THEN sendData player(j), "PLAYERCOLOR", MKI$(i) + MKI$(player(i).color)
                    IF player(i).hasNewPosition THEN sendData player(j), "PLAYERPOS", MKI$(i) + MKS$(player(i).x) + MKS$(player(i).y)
                    IF player(i).hasNewMessage THEN sendData player(j), "PLAYERCHAT", MKI$(i) + chatMessage$
                END IF
            NEXT
        END IF
    NEXT

    _LIMIT 60
LOOP

SUB sendData (client AS object, id$, value$)
    DIM key$
    key$ = id$ + ">" + value$ + endSignal
    PUT #client.handle, , key$
END SUB

SUB getData (client AS object, buffer AS STRING)
    DIM incoming$
    GET #client.handle, , incoming$
    buffer = buffer + incoming$
END SUB

FUNCTION parse%% (buffer AS STRING, key$, value$)
    DIM endMarker AS LONG
    endMarker = INSTR(buffer, endSignal)
    IF endMarker THEN
        key$ = LEFT$(buffer, endMarker - 1)
        buffer = MID$(buffer, endMarker + LEN(endSignal))
        endMarker = INSTR(key$, ">")
        value$ = MID$(key$, endMarker + 1)
        key$ = LEFT$(key$, endMarker - 1)
        parse%% = True
    END IF
END FUNCTION

