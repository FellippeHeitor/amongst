OPTION _EXPLICIT

DIM SHARED gameVersion AS INTEGER
'this is to be increased everytime the client
'becomes incompatible with previous versions
gameVersion = 2

$LET DEBUGGING = FALSE
$IF DEBUGGING = TRUE THEN
    $CONSOLE
$END IF

CONST True = -1, False = 0
CONST mode_freeplay = 0
CONST mode_onlineclient = 1

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

CONST timeout = 30

CONST windowWidth = 800
CONST windowHeight = 600

TYPE object
    name AS STRING
    handle AS LONG
    x AS SINGLE
    prevX AS SINGLE
    y AS SINGLE
    prevY AS SINGLE
    w AS INTEGER
    h AS INTEGER
    state AS INTEGER
    color AS INTEGER
    basicInfoSent AS _BYTE
    broadcastOffline AS _BYTE
    ping AS SINGLE
    id AS INTEGER
    text AS STRING
END TYPE

DIM SHARED mode AS INTEGER
DIM SHARED totalClients AS INTEGER
DIM SHARED serverStream AS STRING
DIM SHARED player(1 TO 10) AS object, me AS INTEGER
'DIM SHARED playerStream(1 TO 10) AS STRING
DIM SHARED colors(1 TO 12) AS _UNSIGNED LONG, r AS INTEGER, g AS INTEGER, b AS INTEGER
DIM SHARED warning(1 TO 30) AS object
DIM SHARED chat(1 TO 14) AS object, hasUnreadMessages AS _BYTE, chatOpen AS _BYTE
DIM idSet AS _BYTE
DIM shipMovement AS _BYTE
DIM i AS LONG
DIM serverPing AS SINGLE, currentPing AS SINGLE, waitingForPong AS _BYTE
DIM id AS INTEGER, value$
DIM choice AS STRING
DIM exitSign AS INTEGER

DIM SHARED ui(1 TO 1) AS object, focus AS INTEGER

DIM serverList(1 TO 3) AS STRING, chosenServer$
i = 0
i = i + 1: serverList(i) = "localhost Local host"
i = i + 1: serverList(i) = "spriggsyspriggs.ddns.net North America"
i = i + 1: serverList(i) = "alephc.xyz Australia"

DIM SHARED endSignal AS STRING
endSignal = CHR$(253) + CHR$(254) + CHR$(255)

playerColorPalette:
DATA 195,17,16
DATA 14,51,196
DATA 18,125,46
DATA 236,84,187
DATA 239,125,17
DATA 248,245,91
DATA 62,71,77
DATA 216,225,241
DATA 107,48,187
DATA 112,73,28
DATA 93,250,220
DATA 79,240,58

RESTORE playerColorPalette
FOR i = 1 TO UBOUND(colors)
    READ r%, g%, b%
    colors(i) = _RGB32(r%, g%, b%)
NEXT

DIM SHARED map AS LONG
DIM SHARED messageIcon AS LONG

map = _NEWIMAGE(windowWidth * 3, windowHeight * 2, 32)
_DEST map
RANDOMIZE 6
FOR i = 1 TO 50
    CircleFill RND * _WIDTH, RND * _HEIGHT, RND * 1000, _RGB32(RND * 255, RND * 255, RND * 255, RND * 150)
NEXT
_DEST 0

messageIcon = _NEWIMAGE(32, 32, 32)
_DEST messageIcon
LINE (0, 0)-(31, 31), _RGB32(0), BF
LINE (4, 4)-(27, 27), _RGB32(200), BF
FOR i = 8 TO 23 STEP 5
    LINE (5, i)-(25, i), _RGB32(0)
NEXT
ui(1).name = "messageicon"
ui(1).x = windowWidth - 50
ui(1).y = 10
ui(1).w = _WIDTH
ui(1).h = _HEIGHT
_DEST 0


DIM userName$, userColor%
IF _FILEEXISTS(COMMAND$) THEN
    OPEN COMMAND$ FOR BINARY AS #1
    GET #1, , i
    userName$ = SPACE$(i)
    GET #1, , userName$
    GET #1, , userColor%
    CLOSE #1
    choice = "1"
    GOTO clientTemp
ELSE
    INPUT "Name: ", userName$
    userName$ = LEFT$(userName$, 20)
    DO
        PRINT "Color (1-"; LTRIM$(STR$(UBOUND(colors))); "): ";
        INPUT "", userColor%
    LOOP WHILE userColor% < 1 OR userColor% > UBOUND(colors)
END IF

start:
DO
    COLOR 15
    PRINT "-------------------------"
    PRINT "(1) Free play"
    PRINT "(2) Connect to server"

    DO
        choice = INKEY$
        _LIMIT 30
    LOOP UNTIL choice >= "1" AND choice <= "3"

    DIM SHARED server AS object
    DIM c AS INTEGER, attempt AS INTEGER
    SELECT CASE VAL(choice)
        CASE 1
            EXIT DO
        CASE 2
            COLOR 7
            PRINT "Choose a server: "
            FOR i = 1 TO UBOUND(serverList)
                PRINT i; " " + MID$(serverList(i), INSTR(serverList(i), " ") + 1)
            NEXT
            DO
                choice = INKEY$
                _LIMIT 30
            LOOP UNTIL VAL(choice) >= 1 AND VAL(choice) <= UBOUND(serverList)
            clientTemp:
            chosenServer$ = LEFT$(serverList(VAL(choice)), INSTR(serverList(VAL(choice)), " ") - 1)

            PRINT "Attempting to connect to server... ";
            r = CSRLIN: c = POS(1)
            CONST maxAttempts = 100
            attempt = 0
            DO
                server.handle = 0
                server.handle = _OPENCLIENT("TCP/IP:51512:" + chosenServer$)
                IF server.handle THEN EXIT DO
                attempt = attempt + 1
                LOCATE r, c: PRINT USING "###%"; (attempt / maxAttempts) * 100;
                _LIMIT 30
            LOOP WHILE attempt < maxAttempts
            IF server.handle THEN
                mode = mode_onlineclient
                serverStream = ""
                EXIT DO
            END IF
            CLS
            COLOR 14: PRINT "/\ ";: COLOR 12
            PRINT "Failed to connect to server."
    END SELECT
LOOP

serverPing = TIMER
DO
    getData server, serverStream
    WHILE parse(serverStream, id, value$)
        SELECT CASE id
            CASE id_SERVERFULL
                CLS
                COLOR 14: PRINT "/\ ";: COLOR 12
                PRINT "Server full."
                CLOSE server.handle
                GOTO start
            CASE id_GAMEVERSION
                IF CVI(value$) <> gameVersion THEN
                    CLS
                    COLOR 14: PRINT "/\ ";: COLOR 12
                    PRINT "Server version incompatible."
                    sendData server, id_GAMEVERSION, ""
                    sendData server, id_PLAYERQUIT, ""
                    CLOSE server.handle
                    GOTO start
                ELSE
                    EXIT DO
                END IF
        END SELECT
    WEND
    IF TIMER - serverPing > 10 THEN
        CLS
        COLOR 14: PRINT "/\ ";: COLOR 12
        PRINT "No response from server."
        GOTO start
    END IF
    _LIMIT 30
LOOP

SCREEN _NEWIMAGE(windowWidth, windowHeight, 32)
_FONT 8
_PRINTMODE _KEEPBACKGROUND

DIM SHARED playerSpeed AS SINGLE
DIM SHARED camera AS object

RANDOMIZE TIMER

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
shipMovement = True

IF mode > 0 THEN
    idSet = False
ELSE
    idSet = True
    me = 1
    player(me).name = userName$
    player(me).x = _WIDTH / 2 + COS(RND * _PI) * (RND * 100)
    player(me).y = _HEIGHT / 2 + SIN(RND * _PI) * (RND * 100)
    player(me).state = True
    player(me).color = userColor%
END IF
DO
    SELECT CASE mode
        CASE mode_onlineclient
            IF waitingForPong = False THEN
                serverPing = TIMER
                sendData server, id_PING, ""
                waitingForPong = True
            END IF

            getData server, serverStream
            DO WHILE parse(serverStream, id, value$)
                SELECT EVERYCASE id
                    CASE id_ID 'first piece of data sent by server if not full
                        idSet = True
                        me = CVI(value$)
                        player(me).name = userName$
                        player(me).x = _WIDTH / 2 + COS(RND * _PI) * (RND * 100)
                        player(me).y = _HEIGHT / 2 + SIN(RND * _PI) * (RND * 100)
                        player(me).state = True
                        player(me).color = userColor%
                    CASE id_NEWCOLOR 'server color changes must always be applied
                        player(me).color = CVI(value$)
                    CASE id_NEWNAME 'server name changes must always be applied
                        player(me).name = value$
                    CASE id_COLOR
                        player(CVI(LEFT$(value$, 2))).color = CVI(RIGHT$(value$, 2))
                    CASE id_POS
                        'playerStream(CVI(LEFT$(value$, 2))) = playerStream(CVI(LEFT$(value$, 2))) + MID$(value$, 3)
                        player(CVI(LEFT$(value$, 2))).x = CVS(MID$(value$, LEN(value$) - 7, 4))
                        player(CVI(LEFT$(value$, 2))).y = CVS(RIGHT$(value$, 4))
                    CASE id_NAME
                        player(CVI(LEFT$(value$, 2))).name = MID$(value$, 3)
                    CASE id_CHAT
                        addMessageToChat CVI(LEFT$(value$, 2)), MID$(value$, 3)
                        hasUnreadMessages = True
                    CASE id_PLAYERONLINE
                        player(CVI(value$)).state = True
                    CASE id_PLAYEROFFLINE
                        IF player(CVI(value$)).state = True THEN
                            player(CVI(value$)).state = False
                            addWarning player(CVI(value$)).name + " left the game."
                        END IF
                    CASE id_PONG
                        waitingForPong = False
                        currentPing = TIMER - serverPing
                END SELECT
            LOOP

            IF idSet THEN
                IF player(me).basicInfoSent = False THEN
                    player(me).basicInfoSent = True
                    sendData server, id_COLOR, MKI$(player(me).color)
                    sendData server, id_NAME, player(me).name
                END IF

                IF player(me).x <> player(me).prevX OR player(me).y <> player(me).prevY THEN
                    player(me).prevX = player(me).x
                    player(me).prevY = player(me).y
                    sendData server, id_POS, MKS$(player(me).x) + MKS$(player(me).y)
                END IF
            END IF

            totalClients = 0
            FOR i = 1 TO UBOUND(player)
                IF player(i).state = True THEN totalClients = totalClients + 1
            NEXT
    END SELECT

    IF playerSpeed < minSpeed THEN playerSpeed = minSpeed
    IF playerSpeed > maxSpeed THEN playerSpeed = maxSpeed

    IF idSet THEN
        IF chatOpen = False THEN
            IF _KEYDOWN(keyUP) THEN player(me).y = player(me).y - playerSpeed
            IF _KEYDOWN(keyDOWN) THEN player(me).y = player(me).y + playerSpeed
            IF _KEYDOWN(keyLEFT) THEN player(me).x = player(me).x - playerSpeed
            IF _KEYDOWN(keyRIGHT) THEN player(me).x = player(me).x + playerSpeed

            IF player(me).x < 0 THEN player(me).x = 0
            IF player(me).y < 0 THEN player(me).y = 0
            IF player(me).x > _WIDTH(map) THEN player(me).x = _WIDTH(map)
            IF player(me).y > _HEIGHT(map) THEN player(me).y = _HEIGHT(map)
        END IF
        adjustCamera
    END IF

    CLS

    DIM shipFlotation AS SINGLE, shipFloatIntensity AS SINGLE
    IF shipMovement THEN
        shipFlotation = shipFlotation + .05
        IF shipFlotation > _PI(2) THEN shipFlotation = shipFlotation - _PI(2)
        shipFloatIntensity = 1.5
    END IF

    _DONTBLEND
    _PUTIMAGE (camera.x + COS(shipFlotation) * shipFloatIntensity, camera.y + SIN(shipFlotation) * shipFloatIntensity), map
    _BLEND

    IF (mode = mode_onlineclient) THEN
        DIM k AS LONG
        k = _KEYHIT

        IF k = 27 THEN
            IF chatOpen THEN
                chatOpen = False
            END IF
        END IF

        exitSign = _EXIT
        IF exitSign THEN
            sendData server, id_PLAYERQUIT, ""
            EXIT DO
        END IF

        COLOR _RGB32(255)

        DIM m$
        m$ = LTRIM$(STR$(currentPing))
        m$ = MID$(m$, INSTR(m$, ".") + 1)
        m$ = LEFT$(STRING$(3 - LEN(m$), "0") + m$, 3) + "ms"
        _PRINTSTRING (_WIDTH - 150 - _PRINTWIDTH(m$), 0), m$

        _FONT 16
        m$ = LTRIM$(STR$(totalClients)) + "/" + LTRIM$(STR$(UBOUND(player)))
        _PRINTSTRING ((_WIDTH - _PRINTWIDTH(m$)) / 2, _HEIGHT - _FONTHEIGHT), m$
        _FONT 8
    END IF

    DIM x AS SINGLE, y AS SINGLE
    FOR i = 1 TO UBOUND(player)
        IF player(i).state = False OR player(i).color = 0 THEN _CONTINUE
        'IF i <> me AND LEN(playerStream(i)) > 0 THEN
        '    'process player stream of coordinates
        '    player(i).x = CVS(MID$(playerStream(i), 1, 4))
        '    player(i).y = CVS(MID$(playerStream(i), 5, 4))
        '    playerStream(i) = MID$(playerStream(i), 9)
        'END IF
        x = player(i).x + camera.x + COS(shipFlotation) * shipFloatIntensity
        y = player(i).y + camera.y + SIN(shipFlotation) * shipFloatIntensity
        CircleFill x, y + 6, 15, _RGB32(0, 50)
        CircleFill x, y, 15, _RGB32(0)
        CircleFill x, y, 10, colors(player(i).color)
        COLOR _RGB32(0)
        _PRINTSTRING (1 + x - _PRINTWIDTH(player(i).name) / 2, 1 + y - 20), player(i).name
        COLOR _RGB32(255)
        _PRINTSTRING (x - _PRINTWIDTH(player(i).name) / 2, y - 20), player(i).name
    NEXT
    'LINE (_WIDTH / 2 - cameraWindow, _HEIGHT / 2 - cameraWindow)-STEP(cameraWindow * 2, cameraWindow * 2), , B

    'display warnings
    FOR i = 1 TO UBOUND(warning)
        IF warning(i).state = True THEN
            COLOR _RGB32(0)
            _PRINTSTRING (11, 1 + _HEIGHT / 2 + _FONTHEIGHT * i), warning(i).name
            COLOR _RGB32(238, 50, 22)
            _PRINTSTRING (10, _HEIGHT / 2 + _FONTHEIGHT * i), warning(i).name
            IF TIMER - warning(i).ping > 2.5 THEN warning(i).state = False
        END IF
    NEXT

    'display messagebox icon
    _PUTIMAGE (ui(1).x, ui(1).y), messageIcon
    IF hasUnreadMessages THEN
        CircleFill _WIDTH - 50, 10, 8, _RGB32(0)
        CircleFill _WIDTH - 50, 10, 5, _RGB32(205, 6, 0)
    END IF

    IF chatOpen THEN
        hasUnreadMessages = False
        LINE (50, 50)-(_WIDTH - 50, _HEIGHT - 50), _RGB32(0, 150), BF
        LINE (50, 50)-(_WIDTH - 50, _HEIGHT - 50), _RGB32(0), B
        _FONT 16
        COLOR _RGB32(0)
        FOR i = 1 TO UBOUND(chat)
            IF chat(i).state THEN
                y = 65 + _FONTHEIGHT * ((i - 1) * 2)
                LINE (55, y - 10)-(_WIDTH - 55, y + 18), _RGB32(255, 80), BF
                _FONT 8
                x = 60
                IF chat(i).id = me THEN x = _WIDTH - 60 - _PRINTWIDTH(chat(i).name)
                COLOR _RGB32(100)
                _PRINTSTRING (1 + x, 1 + y - 8), chat(i).name
                COLOR colors(chat(i).color)
                _PRINTSTRING (x, y - 8), chat(i).name
                _FONT 16
                x = 60
                IF chat(i).id = me THEN x = _WIDTH - 60 - _PRINTWIDTH(chat(i).text)
                COLOR _RGB32(100)
                _PRINTSTRING (1 + x, 1 + y), LEFT$(chat(i).text, (_WIDTH - 100) \ _FONTWIDTH)
                COLOR _RGB32(255)
                _PRINTSTRING (x, y), LEFT$(chat(i).text, (_WIDTH - 100) \ _FONTWIDTH)
            END IF
        NEXT

        DIM myMessage$, char$, tooFast AS _BYTE
        CONST messageSpeed = 1.5
        char$ = INKEY$
        SELECT CASE char$
            CASE CHR$(22) 'ctrl+v
                myMessage$ = myMessage$ + _CLIPBOARD$
            CASE " " TO "z"
                myMessage$ = myMessage$ + char$
            CASE CHR$(8)
                IF LEN(myMessage$) THEN
                    myMessage$ = LEFT$(myMessage$, LEN(myMessage$) - 1)
                END IF
            CASE CHR$(13)
                DIM lastSentChat AS SINGLE
                IF LEN(myMessage$) > 0 AND TIMER - lastSentChat > messageSpeed THEN
                    lastSentChat = TIMER
                    addMessageToChat me, myMessage$
                    sendData server, id_CHAT, myMessage$
                    myMessage$ = ""
                ELSEIF LEN(myMessage$) > 0 AND TIMER - lastSentChat < messageSpeed THEN
                    tooFast = True
                END IF
        END SELECT

        COLOR _RGB32(0)
        _PRINTSTRING (61, 61 + _FONTHEIGHT * (i * 2) - 24), "> " + myMessage$ + "_"
        COLOR _RGB32(255)
        _PRINTSTRING (60, 60 + _FONTHEIGHT * (i * 2) - 24), "> " + myMessage$ + "_"
        _FONT 8

        IF tooFast THEN
            DIM s AS INTEGER
            s = _CEIL(messageSpeed - (TIMER - lastSentChat))
            m$ = "(too fast - wait" + STR$(s) + " second" + LEFT$("s", ABS(s > 1)) + ")"
            y = _HEIGHT - 50 - _FONTHEIGHT
            COLOR _RGB32(0)
            _PRINTSTRING (61, 1 + y), m$
            COLOR _RGB32(200, 177, 44)
            _PRINTSTRING (60, y), m$
            IF TIMER - lastSentChat > messageSpeed THEN tooFast = False
        END IF
    ELSE
        char$ = INKEY$
        SELECT CASE char$
            CASE CHR$(13)
                chatOpen = True
        END SELECT
    END IF

    DIM mouseIsDown AS _BYTE, mouseDownOn AS INTEGER, mouseWheel AS INTEGER
    DIM mb1 AS _BYTE, mb2 AS _BYTE, mx AS INTEGER, my AS INTEGER
    mouseWheel = 0
    IF _MOUSEINPUT THEN
        mouseWheel = mouseWheel + _MOUSEWHEEL
        IF _MOUSEBUTTON(1) = mb1 AND _MOUSEBUTTON(2) = mb2 THEN
            DO WHILE _MOUSEINPUT
                mouseWheel = mouseWheel + _MOUSEWHEEL
                IF NOT (_MOUSEBUTTON(1) = mb1 AND _MOUSEBUTTON(2) = mb2) THEN EXIT DO
            LOOP
        END IF
        mb1 = _MOUSEBUTTON(1)
        mb2 = _MOUSEBUTTON(2)
        mx = _MOUSEX
        my = _MOUSEY
    END IF

    focus = 0
    FOR i = UBOUND(ui) TO 1 STEP -1
        IF mx > ui(i).x AND mx < ui(i).x + ui(i).w AND my > ui(i).y AND my < ui(i).y + ui(i).h THEN
            focus = i
            EXIT FOR
        END IF
    NEXT

    IF mb1 THEN
        mouseDownOn = focus
        mouseIsDown = True
    ELSE
        IF mouseIsDown THEN
            IF mouseDownOn THEN
                SELECT CASE ui(mouseDownOn).name
                    CASE "messageicon"
                        chatOpen = NOT chatOpen
                END SELECT
            END IF
        END IF
        mouseIsDown = False
    END IF

    _DISPLAY
    _LIMIT 60
LOOP
CLOSE
SYSTEM

SUB addMessageToChat (id AS INTEGER, text$)
    DIM i AS INTEGER
    FOR i = 2 TO UBOUND(chat)
        SWAP chat(i), chat(i - 1)
    NEXT
    chat(UBOUND(chat)).id = id
    chat(UBOUND(chat)).state = True
    chat(UBOUND(chat)).name = player(id).name
    chat(UBOUND(chat)).text = text$
    chat(UBOUND(chat)).color = player(id).color
END SUB

SUB addWarning (text$)
    DIM i AS INTEGER
    FOR i = 1 TO UBOUND(warning)
        IF warning(i).state = False THEN
            warning(i).state = True
            warning(i).name = text$
            warning(i).ping = TIMER
            EXIT FOR
        END IF
    NEXT
END SUB

SUB sendData (client AS object, id AS INTEGER, value$)
    DIM packet$
    packet$ = MKI$(id) + value$ + endSignal
    IF client.handle THEN PUT #client.handle, , packet$
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
