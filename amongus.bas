OPTION _EXPLICIT

$LET DEBUGGING = TRUE
$IF DEBUGGING = TRUE THEN
    $CONSOLE
$END IF

CONST True = -1, False = 0
CONST mode_freeplay = 0
CONST mode_localhost = 1
CONST mode_localclient = 2
CONST mode_onlinehost = 3
CONST mode_onlineclient = 4

TYPE object
    handle AS LONG
    x AS SINGLE
    y AS SINGLE
    state AS INTEGER
    COLOR AS INTEGER
    colorSent AS _BYTE
    ping AS SINGLE
END TYPE

DIM SHARED mode AS INTEGER
DIM SHARED totalClients AS INTEGER
DIM SHARED playerStream(1 TO 9) AS STRING, incoming$
DIM SHARED serverStream AS STRING
DIM SHARED player(1 TO 10) AS object, me AS INTEGER
DIM SHARED colors(1 TO 15) AS _UNSIGNED LONG, r AS INTEGER, g AS INTEGER, b AS INTEGER
DIM idSet AS _BYTE
DIM i AS LONG, j AS LONG
DIM newClient AS LONG
DIM endMarker AS INTEGER, packet$, info$

vgaPalette:
DATA 0,0,170
DATA 0,170,0
DATA 0,170,170
DATA 170,0,0
DATA 170,0,170
DATA 170,85,0
DATA 170,170,170
DATA 85,85,85
DATA 85,85,255
DATA 85,255,85
DATA 85,255,255
DATA 255,85,85
DATA 255,85,255
DATA 255,255,85
DATA 255,255,255

RESTORE vgaPalette
FOR i = 1 TO 15
    READ r%, g%, b%
    colors(i) = _RGB32(r%, g%, b%)
NEXT

DO
    COLOR 15
    PRINT "-------------------------"
    PRINT "(1) Free play"
    PRINT "(2) Host game locally"
    PRINT "(3) Connect to local host"
    PRINT "(4) Host game online"
    PRINT "(5) Find game online"

    DIM choice AS STRING
    DO
        choice = INKEY$
        _LIMIT 30
    LOOP UNTIL choice >= "1" AND choice <= "5"

    DIM SHARED server AS LONG, host AS LONG
    DIM c AS INTEGER, attempt AS INTEGER
    SELECT CASE VAL(choice)
        CASE 1
            EXIT DO
        CASE 2
            COLOR 7
            PRINT "Attempting to become a local host... ";
            r = CSRLIN: c = POS(1)
            attempt = 0
            DO
                host = 0
                host = _OPENHOST("TCP/IP:51512")
                IF host THEN EXIT DO
                attempt = attempt + 1
                LOCATE r, c: PRINT USING "###%"; (attempt / 1000) * 100;
                _LIMIT 10
            LOOP WHILE attempt < 1000
            IF host THEN mode = mode_localhost: EXIT DO
            PRINT: COLOR 14: PRINT "/\ ";: COLOR 12
            PRINT "Failed to become a local host."
            CLOSE host
        CASE 3
            COLOR 7
            PRINT "Attempting to connect to local host... ";
            r = CSRLIN: c = POS(1)
            attempt = 0
            DO
                server = 0
                server = _OPENCLIENT("TCP/IP:51512:localhost")
                IF server THEN EXIT DO
                attempt = attempt + 1
                LOCATE r, c: PRINT USING "###%"; (attempt / 1000) * 100;
                _LIMIT 10
            LOOP WHILE attempt < 1000
            IF server THEN mode = mode_localclient: EXIT DO
            PRINT: COLOR 14: PRINT "/\ ";: COLOR 12
            PRINT "Failed to connect to local host."
            CLOSE server
        CASE 4
        CASE 5
    END SELECT
LOOP

SCREEN _NEWIMAGE(800, 600, 32)
_FONT 8
_PRINTMODE _KEEPBACKGROUND

DIM SHARED playerSpeed AS SINGLE
DIM SHARED map AS LONG
DIM SHARED camera AS object

map = _NEWIMAGE(_WIDTH * 3, _HEIGHT * 2, 32)
_DEST map
FOR i = 1 TO 50
    CircleFill RND * _WIDTH, RND * _HEIGHT, RND * 1000, _RGB32(RND * 255, RND * 255, RND * 255, RND * 150)
NEXT
_DEST 0

CONST keyUP = 18432
CONST keyDOWN = 20480
CONST keyLEFT = 19200
CONST keyRIGHT = 19712
CONST keyLSHIFT = 100304
CONST keyRSHIFT = 100303

CONST cameraWindow = 100

CONST minSpeed = 3
CONST maxSpeed = 5

playerSpeed = maxSpeed

IF mode > 1 THEN
    idSet = False
ELSE
    idSet = True
    me = 1
    player(me).x = _WIDTH / 2
    player(me).y = _HEIGHT / 2
    player(me).state = True
    player(me).COLOR = 1
END IF
DO
    SELECT CASE mode
        CASE mode_localhost
            'check for new connections (unless already at max)
            IF idSet = False THEN
                idSet = True
                me = 1
                player(me).x = _WIDTH / 2
                player(me).y = _HEIGHT / 2
                player(me).state = True
                player(me).COLOR = 1
            END IF

            IF totalClients < UBOUND(player) THEN
                newClient = 0
                newClient = _OPENCONNECTION(host)
                IF newClient THEN
                    totalClients = totalClients + 1
                    FOR i = 1 TO UBOUND(player)
                        IF player(i).state = False THEN
                            player(i).handle = newClient
                            player(i).state = True
                            packet$ = "ID>" + MKI$(i) + "<END>"
                            PUT #player(i).handle, , packet$
                            player(i).ping = TIMER
                            EXIT FOR
                        END IF
                    NEXT
                END IF
            END IF

            FOR i = 1 TO UBOUND(player)
                IF player(i).state = False OR i = me THEN _CONTINUE
                IF TIMER - player(i).ping > 3 THEN
                    'player inactive
                    player(i).state = False
                    CLOSE player(i).handle
                    _CONTINUE
                END IF

                GET #player(i).handle, , incoming$
                playerStream(i) = playerStream(i) + incoming$
                endMarker = INSTR(playerStream(i), "<END>")
                DO WHILE endMarker > 0
                    packet$ = LEFT$(playerStream(i), endMarker - 1)
                    playerStream(i) = MID$(playerStream(i), endMarker + 5)
                    endMarker = INSTR(packet$, ">")
                    info$ = MID$(packet$, endMarker + 1)
                    packet$ = LEFT$(packet$, endMarker - 1)

                    endMarker = INSTR(playerStream(i), "<END>")
                    player(i).ping = TIMER
                    SELECT CASE packet$
                        CASE "COLOR" 'received once per player
                            DIM newcolor AS INTEGER, changed AS _BYTE
                            newcolor = CVI(info$)
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
                                info$ = "COLOR>" + MKI$(newcolor) + "<END>"
                                PUT #player(i).handle, , info$
                            END IF
                        CASE "POS"
                            player(i).x = CVI(LEFT$(info$, 2))
                            player(i).y = CVI(RIGHT$(info$, 2))
                    END SELECT
                LOOP

                'send all players' data
                FOR j = 1 TO UBOUND(player)
                    IF j = i THEN _CONTINUE
                    info$ = "PLAYERCOLOR>" + MKI$(j) + MKI$(player(j).COLOR) + "<END>"
                    PUT #player(i).handle, , info$
                    info$ = "PLAYERPOS>" + MKI$(j) + MKS$(player(j).x) + MKS$(player(j).y) + "<END>"
                    PUT #player(i).handle, , info$
                NEXT
            NEXT

        CASE mode_localclient, mode_onlineclient
            IF idSet THEN
                IF player(me).colorSent = False THEN
                    player(me).colorSent = True
                    sendInfo "COLOR", MKI$(player(me).COLOR)
                END IF

                sendInfo "POS", MKI$(player(me).x) + MKI$(player(me).y)
            END IF

            GET #server, , incoming$
            serverStream = serverStream + incoming$
            endMarker = INSTR(serverStream, "<END>")
            DO WHILE endMarker > 0
                packet$ = LEFT$(serverStream, endMarker - 1)
                serverStream = MID$(serverStream, endMarker + 5)
                endMarker = INSTR(packet$, ">")
                info$ = MID$(packet$, endMarker + 1)
                packet$ = LEFT$(packet$, endMarker - 1)

                endMarker = INSTR(serverStream, "<END>")

                SELECT CASE packet$
                    CASE "ID" 'first info sent by server
                        idSet = True
                        me = CVI(info$)
                        player(me).x = _WIDTH / 2
                        player(me).y = _HEIGHT / 2
                        player(me).state = True
                        player(me).COLOR = 1
                    CASE "COLOR" 'server color changes must always be applied
                        player(me).COLOR = CVI(info$)
                    CASE "PLAYERCOLOR"
                        player(CVI(LEFT$(info$, 2))).COLOR = CVI(RIGHT$(info$, 2))
                    CASE "PLAYERPOS"
                        player(CVI(LEFT$(info$, 2))).state = True
                        player(CVI(LEFT$(info$, 2))).ping = TIMER
                        player(CVI(LEFT$(info$, 2))).x = CVS(MID$(info$, 3, 4))
                        player(CVI(LEFT$(info$, 2))).y = CVS(RIGHT$(info$, 4))
                END SELECT
            LOOP

            FOR i = 1 TO UBOUND(player)
                IF i = me THEN _CONTINUE
                IF TIMER - player(i).ping > 3 THEN
                    player(i).state = False
                END IF
            NEXT
    END SELECT

    IF playerSpeed < minSpeed THEN playerSpeed = minSpeed
    IF playerSpeed > maxSpeed THEN playerSpeed = maxSpeed

    IF idSet THEN
        IF _KEYDOWN(keyUP) THEN player(me).y = player(me).y - playerSpeed
        IF _KEYDOWN(keyDOWN) THEN player(me).y = player(me).y + playerSpeed
        IF _KEYDOWN(keyLEFT) THEN player(me).x = player(me).x - playerSpeed
        IF _KEYDOWN(keyRIGHT) THEN player(me).x = player(me).x + playerSpeed

        IF player(me).x < 0 THEN player(me).x = 0
        IF player(me).y < 0 THEN player(me).y = 0
        IF player(me).x > _WIDTH(map) THEN player(me).x = _WIDTH(map)
        IF player(me).y > _HEIGHT(map) THEN player(me).y = _HEIGHT(map)

        adjustCamera
    END IF

    CLS

    _DONTBLEND
    _PUTIMAGE (camera.x, camera.y), map
    _BLEND

    FOR i = 1 TO UBOUND(player)
        IF player(i).state = False OR player(i).COLOR = 0 THEN _CONTINUE
        CircleFill player(i).x + camera.x, player(i).y + camera.y, 15, _RGB32(0)
        CircleFill player(i).x + camera.x, player(i).y + camera.y, 10, colors(player(i).COLOR)
    NEXT
    'LINE (_WIDTH / 2 - cameraWindow, _HEIGHT / 2 - cameraWindow)-STEP(cameraWindow * 2, cameraWindow * 2), , B

    _DISPLAY
    _LIMIT 60
LOOP UNTIL _KEYHIT = 27
CLOSE
SYSTEM

SUB sendInfo (id$, info$)
    DIM packet$
    packet$ = id$ + ">" + info$ + "<END>"
    PUT #server, , packet$
END SUB

SUB adjustCamera
    IF player(me).x + camera.x > _WIDTH / 2 + cameraWindow THEN
        camera.x = _WIDTH / 2 - player(me).x + cameraWindow
    ELSEIF player(me).x + camera.x < _WIDTH / 2 - cameraWindow THEN
        camera.x = _WIDTH / 2 - player(me).x - cameraWindow
    END IF
    IF camera.x > 0 THEN camera.x = 0
    IF camera.x < -(_WIDTH(map) - _WIDTH) THEN camera.x = -(_WIDTH(map) - _WIDTH)

    IF player(me).y + camera.y > _HEIGHT / 2 + cameraWindow THEN
        camera.y = _HEIGHT / 2 - player(me).y + cameraWindow
    ELSEIF player(me).y + camera.y < _HEIGHT / 2 - cameraWindow THEN
        camera.y = _HEIGHT / 2 - player(me).y - cameraWindow
    END IF
    IF camera.y > 0 THEN camera.y = 0
    IF camera.y < -(_HEIGHT(map) - _HEIGHT) THEN camera.y = -(_HEIGHT(map) - _HEIGHT)
END SUB


SUB CircleFill (CX AS INTEGER, CY AS INTEGER, R AS INTEGER, C AS _UNSIGNED LONG)
    ' CX = center x coordinate
    ' CY = center y coordinate
    '  R = radius
    '  C = fill color
    DIM Radius AS INTEGER, RadiusError AS INTEGER
    DIM X AS INTEGER, Y AS INTEGER
    Radius = ABS(R)
    RadiusError = -Radius
    X = Radius
    Y = 0
    IF Radius = 0 THEN PSET (CX, CY), C: EXIT SUB
    LINE (CX - X, CY)-(CX + X, CY), C, BF
    WHILE X > Y
        RadiusError = RadiusError + Y * 2 + 1
        IF RadiusError >= 0 THEN
            IF X <> Y + 1 THEN
                LINE (CX - Y, CY - X)-(CX + Y, CY - X), C, BF
                LINE (CX - Y, CY + X)-(CX + Y, CY + X), C, BF
            END IF
            X = X - 1
            RadiusError = RadiusError - X * 2
        END IF
        Y = Y + 1
        LINE (CX - X, CY - Y)-(CX + X, CY - Y), C, BF
        LINE (CX - X, CY + Y)-(CX + X, CY + Y), C, BF
    WEND
END SUB

SUB db (text$)
    $IF DEBUGGING = TRUE THEN
        _ECHO text$
    $ELSE
        DIM dummy$
        dummy$ = text$
    $END IF
END SUB

