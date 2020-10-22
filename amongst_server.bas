OPTION _EXPLICIT

$LET DEBUGGING = FALSE
$IF DEBUGGING = TRUE THEN
    $CONSOLE
$END IF

$CONSOLE:ONLY
_DEST _CONSOLE

CONST True = -1, False = 0

TYPE object
    NAME AS STRING
    handle AS LONG
    x AS SINGLE
    y AS SINGLE
    state AS INTEGER
    COLOR AS INTEGER
    basicInfoSent AS _BYTE
    ping AS SINGLE
END TYPE

DIM SHARED totalClients AS INTEGER
DIM SHARED playerStream(1 TO 10) AS STRING
DIM SHARED player(1 TO 10) AS object
DIM SHARED colors(1 TO 15) AS _UNSIGNED LONG
DIM i AS LONG, j AS LONG
DIM newClient AS LONG
DIM KEY$, value$

DIM SHARED endSignal AS STRING
endSignal = "<" + CHR$(254) + ">"

CONST timeout = 10

DIM SHARED host AS LONG
PRINT "Starting server... ";
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
                    player(i).COLOR = 0
                    player(i).handle = newClient
                    player(i).state = True
                    sendData player(i), "ID", MKI$(i)
                    player(i).ping = TIMER
                    EXIT FOR
                END IF
            NEXT
            PRINT "User at " + _CONNECTIONADDRESS$(newClient) + " connected as client #" + _TRIM$(STR$(i))
        END IF
    END IF

    FOR i = 1 TO UBOUND(player)
        IF player(i).state = False THEN _CONTINUE
        IF TIMER - player(i).ping > timeout THEN
            'player inactive
            player(i).state = False
            CLOSE player(i).handle
            PRINT player(i).NAME + " lost connection."
            totalClients = totalClients - 1
            _CONTINUE
        END IF

        getData player(i), playerStream(i)

        DO WHILE parse(playerStream(i), KEY$, value$)
            player(i).ping = TIMER
            SELECT CASE KEY$
                CASE "NAME"
                    player(i).NAME = value$
                    PRINT "Client #" + _TRIM$(STR$(i)) + " has name " + value$
                CASE "COLOR" 'received once per player
                    DIM newcolor AS INTEGER, changed AS _BYTE
                    newcolor = CVI(value$)
                    changed = False
                    'check if this color is already in use, so a random one can be assigned
                    FOR j = 1 TO UBOUND(player)
                        IF player(j).state = True AND player(j).COLOR = newcolor THEN
                            newcolor = newcolor + 1
                            IF newcolor > UBOUND(colors) THEN newcolor = 1
                            changed = True
                            j = 0 'check again
                        END IF
                    NEXT
                    player(i).COLOR = newcolor
                    IF changed THEN
                        sendData player(i), "COLOR", MKI$(newcolor)
                    END IF
                CASE "POS"
                    player(i).x = CVI(LEFT$(value$, 2))
                    player(i).y = CVI(RIGHT$(value$, 2))
            END SELECT
        LOOP

        'send ping
        sendData player(i), "PING", ""

        'send all players' data
        FOR j = 1 TO UBOUND(player)
            IF j = i THEN _CONTINUE
            IF player(j).state = True THEN
                sendData player(i), "PLAYERCOLOR", MKI$(j) + MKI$(player(j).COLOR)
                sendData player(i), "PLAYERPOS", MKI$(j) + MKS$(player(j).x) + MKS$(player(j).y)
                sendData player(i), "PLAYERNAME", MKI$(j) + player(j).NAME
            ELSE
                sendData player(i), "PLAYEROFFLINE", MKI$(j)
            END IF
        NEXT
    NEXT
    _LIMIT 30
LOOP

SUB sendData (client AS object, id$, value$)
    DIM KEY$
    KEY$ = id$ + ">" + value$ + endSignal
    PUT #client.handle, , KEY$
END SUB

SUB getData (client AS object, buffer AS STRING)
    DIM incoming$
    GET #client.handle, , incoming$
    buffer = buffer + incoming$
END SUB

FUNCTION parse%% (buffer AS STRING, KEY$, value$)
    DIM endMarker AS LONG
    endMarker = INSTR(buffer, endSignal)
    IF endMarker THEN
        KEY$ = LEFT$(buffer, endMarker - 1)
        buffer = MID$(buffer, endMarker + LEN(endSignal))
        endMarker = INSTR(KEY$, ">")
        value$ = MID$(KEY$, endMarker + 1)
        KEY$ = LEFT$(KEY$, endMarker - 1)
        parse%% = True
    END IF
END FUNCTION

