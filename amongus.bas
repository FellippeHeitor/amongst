OPTION _EXPLICIT
RANDOMIZE TIMER
SCREEN _NEWIMAGE(1000, 800, 32)

TYPE object
    x AS SINGLE
    y AS SINGLE
END TYPE

DIM SHARED player AS object
DIM SHARED camera AS object
DIM SHARED map AS LONG
DIM playerSpeed AS SINGLE
DIM i AS LONG

map = _NEWIMAGE(_WIDTH * 3, _HEIGHT * 2, 32)
_DEST map
FOR i = 1 TO 50
    CircleFill _WIDTH * RND, _HEIGHT * RND, 1000 * RND, _RGB32(255 * RND, 255 * RND, 255 * RND, 255 * RND)
NEXT
_DEST 0

player.x = _WIDTH / 2
player.y = _HEIGHT / 2

playerSpeed = 5

CONST keyUP = 18432
CONST keyDOWN = 20480
CONST keyLEFT = 19200
CONST keyRIGHT = 19712

DO
    IF _KEYDOWN(keyUP) THEN player.y = player.y - playerSpeed
    IF _KEYDOWN(keyDOWN) THEN player.y = player.y + playerSpeed
    IF _KEYDOWN(keyLEFT) THEN player.x = player.x - playerSpeed
    IF _KEYDOWN(keyRIGHT) THEN player.x = player.x + playerSpeed

    IF player.x < 0 THEN player.x = 0
    IF player.x > _WIDTH(map) THEN player.x = _WIDTH(map)
    IF player.y < 0 THEN player.y = 0
    IF player.y > _HEIGHT(map) THEN player.y = _HEIGHT(map)

    adjustCamera
    CLS
    _PUTIMAGE (camera.x, camera.y), map

    CircleFill player.x + camera.x, player.y + camera.y, 20, _RGB32(255)
    _DISPLAY
    _LIMIT 60
LOOP

SUB adjustCamera
    IF player.x + camera.x > _WIDTH / 2 OR player.x + camera.x < _WIDTH / 2 THEN
        camera.x = _WIDTH / 2 - player.x
    END IF
    IF camera.x > 0 THEN camera.x = 0
    IF camera.x < -(_WIDTH(map) - _WIDTH) THEN camera.x = -(_WIDTH(map) - _WIDTH)

    IF player.y + camera.y > _HEIGHT / 2 OR player.y + camera.y < _HEIGHT / 2 THEN
        camera.y = _HEIGHT / 2 - player.y
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

