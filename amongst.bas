OPTION _EXPLICIT

DIM SHARED gameVersion AS INTEGER
gameVersion = 1

$LET DEBUGGING = FALSE
$IF DEBUGGING = TRUE THEN
    $CONSOLE
$END IF

CONST True = -1, False = 0
CONST mode_freeplay = 0
CONST mode_onlineclient = 1

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
DIM SHARED colors(1 TO 15) AS _UNSIGNED LONG, r AS INTEGER, g AS INTEGER, b AS INTEGER
DIM SHARED warning(1 TO 30) AS object
DIM SHARED chat(1 TO 14) AS object, hasUnreadMessages AS _BYTE, chatOpen AS _BYTE
DIM idSet AS _BYTE
DIM gameVersionChecked AS _BYTE
DIM i AS LONG
DIM serverPing AS SINGLE
DIM key$, value$
DIM choice AS STRING
DIM exitSign AS INTEGER

DIM SHARED ui(1 TO 1) AS object, focus AS INTEGER

DIM serverList(1 TO 3) AS STRING, chosenServer$
i = 0
i = i + 1: serverList(i) = "localhost Local host"
i = i + 1: serverList(i) = "spriggsyspriggs.ddns.net North America"
i = i + 1: serverList(i) = "alephc.xyz Australia"

DIM SHARED endSignal AS STRING
endSignal = "<" + CHR$(254) + ">"

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

CONST timeout = 30
DIM userName$, userColor%
IF _FILEEXISTS("amongus.dat") THEN
    OPEN "amongus.dat" FOR BINARY AS #1
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
        INPUT "Color (1-15): ", userColor%
    LOOP WHILE userColor% < 1 OR userColor% > 15
END IF

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
            IF server.handle THEN serverPing = TIMER: mode = mode_onlineclient: EXIT DO
            PRINT: COLOR 14: PRINT "/\ ";: COLOR 12
            PRINT "Failed to connect to server."
            CLOSE server.handle
    END SELECT
LOOP

SCREEN _NEWIMAGE(800, 600, 32)
_FONT 8
_PRINTMODE _KEEPBACKGROUND

DIM SHARED playerSpeed AS SINGLE
DIM SHARED map AS LONG
DIM SHARED camera AS object
DIM messageIcon AS LONG

map = _NEWIMAGE(_WIDTH * 3, _HEIGHT * 2, 32)
_DEST map
RANDOMIZE 6
FOR i = 1 TO 50
    CircleFill RND * _WIDTH, RND * _HEIGHT, RND * 1000, _RGB32(RND * 255, RND * 255, RND * 255, RND * 150)
NEXT
_DEST 0
RANDOMIZE TIMER

messageIcon = _NEWIMAGE(32, 32, 32)
_DEST messageIcon
LINE (0, 0)-(31, 31), _RGB32(0), BF
LINE (4, 4)-(27, 27), _RGB32(200), BF
FOR i = 8 TO 23 STEP 5
    LINE (5, i)-(25, i), _RGB32(0)
NEXT
ui(1).name = "messageicon"
ui(1).x = _WIDTH(0) - 50
ui(1).y = 10
ui(1).w = _WIDTH
ui(1).h = _HEIGHT
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
            IF idSet THEN
                IF player(me).basicInfoSent = False THEN
                    player(me).basicInfoSent = True
                    sendData server, "COLOR", MKI$(player(me).color)
                    sendData server, "NAME", player(me).name
                END IF

                IF player(me).x <> player(me).prevX OR player(me).y <> player(me).prevY THEN
                    player(me).prevX = player(me).x
                    player(me).prevY = player(me).y
                    sendData server, "PLAYERPOS", MKI$(player(me).x) + MKI$(player(me).y)
                END IF
            ELSE
                gameVersionChecked = False
            END IF

            sendData server, "PING", ""

            getData server, serverStream
            DO WHILE parse(serverStream, key$, value$)
                SELECT EVERYCASE key$
                    CASE "ID" 'first piece of data sent by server
                        idSet = True
                        me = CVI(value$)
                        player(me).name = userName$
                        player(me).x = _WIDTH / 2 + COS(RND * _PI) * (RND * 100)
                        player(me).y = _HEIGHT / 2 + SIN(RND * _PI) * (RND * 100)
                        player(me).state = True
                        player(me).color = userColor%
                    CASE "NEWCOLOR" 'server color changes must always be applied
                        player(me).color = CVI(value$)
                    CASE "NEWNAME" 'server name changes must always be applied
                        player(me).name = value$
                    CASE "PLAYERCOLOR"
                        player(CVI(LEFT$(value$, 2))).color = CVI(RIGHT$(value$, 2))
                    CASE "PLAYERPOS"
                        player(CVI(LEFT$(value$, 2))).x = CVS(MID$(value$, 3, 4))
                        player(CVI(LEFT$(value$, 2))).y = CVS(RIGHT$(value$, 4))
                    CASE "PLAYERNAME"
                        player(CVI(LEFT$(value$, 2))).name = MID$(value$, 3)
                    CASE "PLAYERCHAT"
                        addMessageToChat CVI(LEFT$(value$, 2)), MID$(value$, 3)
                        hasUnreadMessages = True
                    CASE "PLAYERONLINE"
                        player(CVI(value$)).state = True
                    CASE "PLAYEROFFLINE"
                        IF player(CVI(value$)).state = True THEN
                            player(CVI(value$)).state = False
                            addWarning player(CVI(value$)).name + " left the game."
                        END IF
                    CASE "PING"
                        serverPing = TIMER
                END SELECT
            LOOP

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
    shipFlotation = shipFlotation + .05
    IF shipFlotation > _PI(2) THEN shipFlotation = shipFlotation - _PI(2)
    shipFloatIntensity = 1.5

    _DONTBLEND
    _PUTIMAGE (camera.x + COS(shipFlotation) * shipFloatIntensity, camera.y + SIN(shipFlotation) * shipFloatIntensity), map
    _BLEND

    IF (mode = mode_onlineclient) THEN
        IF TIMER - serverPing > timeout THEN
            SCREEN 0
            PRINT: COLOR 14: PRINT "/\ ";: COLOR 12
            PRINT "Disconnected from host."
            END
        ELSE
            DIM k AS LONG
            k = _KEYHIT

            IF k = 27 THEN
                IF chatOpen THEN
                    chatOpen = False
                END IF
            END IF

            exitSign = _EXIT
            IF exitSign THEN
                sendData server, "PLAYERQUIT", MKI$(me)
                EXIT DO
            END IF

            COLOR _RGB32(255)

            DIM p AS SINGLE, m$
            p = TIMER - serverPing
            m$ = LTRIM$(STR$(p))
            m$ = MID$(m$, INSTR(m$, ".") + 1)
            m$ = LEFT$(STRING$(3 - LEN(m$), "0") + m$, 3) + "ms"
            _PRINTSTRING (_WIDTH - 150 - _PRINTWIDTH(m$), 0), m$

            _FONT 16
            m$ = LTRIM$(STR$(totalClients)) + "/" + LTRIM$(STR$(UBOUND(player)))
            _PRINTSTRING ((_WIDTH - _PRINTWIDTH(m$)) / 2, _HEIGHT - _FONTHEIGHT), m$
            _FONT 8


            'DIM pcount AS INTEGER, p AS SINGLE, m$, lastPingUpdate AS SINGLE
            'IF TIMER - lastPingUpdate >= 1 THEN
            '    p = p / pcount
            '    pcount = 0
            '    lastPingUpdate = TIMER
            '    m$ = LTRIM$(STR$(p))
            '    m$ = MID$(m$, INSTR(m$, ".") + 1)
            '    m$ = LEFT$(STRING$(3 - LEN(m$), "0") + m$, 3) + "ms"
            'ELSE
            '    pcount = pcount + 1
            '    p = p + (TIMER - serverPing)
            'END IF
            '_PRINTSTRING (_WIDTH - 50 - _PRINTWIDTH(m$), 0), m$
        END IF
    END IF

    DIM x AS SINGLE, y AS SINGLE
    FOR i = 1 TO UBOUND(player)
        IF player(i).state = False OR player(i).color = 0 THEN _CONTINUE
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
        LINE (50, 50)-(_WIDTH - 50, _HEIGHT - 50), _RGB32(0, 80), BF
        LINE (50, 50)-(_WIDTH - 50, _HEIGHT - 50), _RGB32(0), B
        _FONT 16
        COLOR _RGB32(0)
        FOR i = 1 TO UBOUND(chat)
            IF chat(i).id THEN
                y = 65 + _FONTHEIGHT * ((i - 1) * 2)
                LINE (55, y - 10)-(_WIDTH - 55, y + 18), _RGB32(255, 100), BF
                _FONT 8
                x = 60
                IF chat(i).id = me THEN x = _WIDTH - 60 - _PRINTWIDTH(player(chat(i).id).name)
                COLOR _RGB32(100)
                _PRINTSTRING (1 + x, 1 + y - 8), player(chat(i).id).name
                COLOR colors(player(chat(i).id).color)
                _PRINTSTRING (x, y - 8), player(chat(i).id).name
                _FONT 16
                x = 60
                IF chat(i).id = me THEN x = _WIDTH - 60 - _PRINTWIDTH(chat(i).text)
                COLOR _RGB32(100)
                _PRINTSTRING (1 + x, 1 + y), LEFT$(chat(i).text, (_WIDTH - 100) \ _FONTWIDTH)
                COLOR _RGB32(255)
                _PRINTSTRING (x, y), LEFT$(chat(i).text, (_WIDTH - 100) \ _FONTWIDTH)
            END IF
        NEXT

        DIM myMessage$, char$
        char$ = INKEY$
        SELECT CASE char$
            CASE " " TO "z"
                myMessage$ = myMessage$ + char$
            CASE CHR$(8)
                IF LEN(myMessage$) THEN
                    myMessage$ = LEFT$(myMessage$, LEN(myMessage$) - 1)
                END IF
            CASE CHR$(13)
                DIM lastSentChat AS SINGLE
                IF LEN(myMessage$) > 0 AND TIMER - lastSentChat > 1.5 THEN
                    lastSentChat = TIMER
                    addMessageToChat me, myMessage$
                    sendData server, "CHAT", myMessage$
                    myMessage$ = ""
                END IF
        END SELECT

        COLOR _RGB32(0)
        _PRINTSTRING (61, 61 + _FONTHEIGHT * (i * 2) - 24), "> " + myMessage$ + "_"
        COLOR _RGB32(255)
        _PRINTSTRING (60, 60 + _FONTHEIGHT * (i * 2) - 24), "> " + myMessage$ + "_"
        _FONT 8
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
    chat(UBOUND(chat)).text = text$
    chat(UBOUND(chat)).id = id
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

SUB sendData (client AS object, id$, value$)
    DIM key$
    key$ = id$ + ">" + value$ + endSignal
    IF client.handle THEN PUT #client.handle, , key$
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
