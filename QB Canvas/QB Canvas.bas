' Developed and maintained by Skyquest


$EXEICON:'.\myIcon.ico'
_TITLE "QB CANVAS"
SCREEN _NEWIMAGE(740, 532, 32)

OPTION _EXPLICIT ' forces variables to be declared
OPTION _EXPLICITARRAY ' forces arrays to be declared
OPTION BASE 1


' start of DIMS
':------------------------------------------------------------------------------------------
' declares strings
DIM tool$

'declares integers

DIM isDrawing%, isButtonPressed%, isFullscreenOn%, quit%, isButtonActive%, isDrawFinished%, isTemp0Saved% ' booleans
DIM mi, mb, mX%, mY%, lastX%, lastY%, startX%, startY%, size% ' holds variables related to mouse

'declares colors
DIM currentColor~&

'declares images
DIM loadimg&

DIM fimage&, cimage&
fimage& = _NEWIMAGE(_WIDTH, _HEIGHT)
cimage& = _NEWIMAGE(_WIDTH, _HEIGHT)
':------------------------------------------------------------------------------------------
'end of DIMS


' start of setups
':------------------------------------------------------------------------------------------
tool$ = "Pencil"
currentColor~& = _RGB32(0, 0, 0) ' setups the current color being used

_DEST cimage&
LINE (114, 10)-(625, 521), _RGB32(255, 255, 255), BF ' background
_DEST 0

CALL gui
CALL PaintTimeStartup("temp0.bmp", cimage&)
CALL PaintTimeStartup("temp1.bmp", cimage&)
'CALL PaintTimeSave("temp0.bmp")
'CALL PaintTimeSave("temp1.bmp")
':------------------------------------------------------------------------------------------
'end of setups


'start of the program
':------------------------------------------------------------------------------------------
CLS
DO
    ' Gets the latest mouse information
    DO WHILE _MOUSEINPUT
    LOOP
    mi = _MOUSEINPUT
    mb = _MOUSEBUTTON(1)
    mX% = _MOUSEX
    mY% = _MOUSEY

    IF mb THEN
        IF mX% > 0 AND mX% < 740 AND mY% > 0 AND mY% < 532 THEN ' mouse within the canvas
            IF isDrawing% = 0 THEN

                _PUTIMAGE (0, 0), cimage&, fimage&
                _FREEIMAGE cimage&
                cimage& = _NEWIMAGE(_WIDTH, _HEIGHT)
                lastX% = mX%
                lastY% = mY%
                startX% = lastX%
                startY% = lastY%
                isDrawing% = 1
                isButtonActive% = 0
                ' canUndo% = 0

            ELSE
                IF isTemp0Saved% = 1 THEN ' checks if temp1 is saved, if true then: saves the previous image: saves the image late than the temp 1
                    CALL PaintTimeSave("temp0.bmp")
                    isTemp0Saved% = 0
                END IF

                IF mX% > 95 AND mX% < 725 THEN
                    isButtonActive% = 1
                    IF tool$ = "Pencil" THEN
                        _DEST cimage&
                        LINE (lastX%, lastY%)-(mX%, mY%), currentColor~&
                        lastX% = mX%
                        lastY% = mY%
                        _DEST 0
                    ELSEIF tool$ = "Line" THEN
                        CALL PrepDrawDest(cimage&)
                        LINE (startX%, startY%)-(mX%, mY%), currentColor~&
                        _DEST 0
                    ELSEIF tool$ = "Circle" THEN
                        CALL PrepDrawDest(cimage&)
                        IF startX% > mX% THEN ' to left
                            CIRCLE (startX% - ((startX% - mX%) / 2), startY%), (startX% - mX%) / 2, currentColor~&
                        ELSEIF startX% < mX% THEN ' to right
                            CIRCLE (startX% + ((mX% - startX%) / 2), startY%), (mX% - startX%) / 2, currentColor~&
                        END IF
                        _DEST 0
                    ELSEIF tool$ = "Triangle" THEN
                        CALL PrepDrawDest(cimage&)
                        LINE (startX%, startY%)-(mX%, startY%), currentColor~& ' bottom
                        IF startX% > mX% THEN ' to left
                            LINE (startX%, startY%)-(mX% + ((startX% - mX%) / 2), startY% - (startY% - mY%)), currentColor~& ' right 1
                            LINE (startX% - ((startX% - mX%) / 2), startY% - (startY% - mY%))-(mX%, startY%), currentColor~& ' left 1
                        ELSEIF startX% < mX% THEN ' to right
                            LINE (startX%, startY%)-(mX% + ((startX% - mX%) / 2), startY% + (mY% - startY%)), currentColor~& ' right 2
                            LINE (startX% - ((startX% - mX%) / 2), startY% + (mY% - startY%))-(mX%, startY%), currentColor~& ' left 2
                        END IF
                        _DEST 0
                    ELSEIF tool$ = "Rectangle" THEN
                        CALL PrepDrawDest(cimage&)
                        LINE (startX%, startY%)-(mX%, mY%), currentColor~&, B
                        _DEST 0
                    ELSEIF tool$ = "FCircle" THEN
                        CALL PrepDrawDest(cimage&)
                        IF startX% > mX% THEN ' to left
                            CIRCLE (startX% - ((startX% - mX%) / 2), startY%), (startX% - mX%) / 2, currentColor~&
                            DIM rl!
                            FOR rl! = 0 TO ((startX% - mX%) / 2) STEP .3
                                CIRCLE (startX% - ((startX% - mX%) / 2), startY%), rl!, currentColor~&
                            NEXT rl!
                        ELSEIF startX% < mX% THEN ' to right
                            CIRCLE (startX% + ((mX% - startX%) / 2), startY%), (mX% - startX%) / 2, currentColor~&
                            DIM rr!
                            FOR rr! = 0 TO ((mX% - startX%) / 2) STEP .3
                                CIRCLE (startX% + ((mX% - startX%) / 2), startY%), rr!, currentColor~&
                            NEXT rr!
                        END IF
                        _DEST 0
                    ELSEIF tool$ = "FTriangle" THEN
                        CALL PrepDrawDest(cimage&)

                        DIM ftcY!, FTC, FTCU
                        FTC = 0
                        ftcY! = 0
                        FTCU = 0
                        LINE (startX%, startY%)-(mX%, startY%), currentColor~& ' bottom
                        IF startX% > mX% THEN ' to left
                            IF startY% > mY% THEN ' up
                                FTCU = startY% - (startX% - mX%)
                                FOR FTC = (startY%) TO (FTCU + 1) STEP -1
                                    ftcY! = ftcY! + 1
                                    LINE (startX% - (ftcY! / 2), FTC)-(mX% + (ftcY! / 2), FTC), currentColor~&
                                NEXT FTC
                            ELSEIF startY% < mY% THEN ' down
                                FTCU = startY% + (startX% - mX%)
                                FOR FTC = (startY%) TO (FTCU - 1)
                                    ftcY! = ftcY! + 1
                                    LINE (startX% - (ftcY! / 2), FTC)-(mX% + (ftcY! / 2), FTC), currentColor~&
                                NEXT FTC
                            END IF
                        ELSEIF startX% < mX% THEN ' to right
                            IF startY% > mY% THEN ' up
                                FTCU = startY% + (startX% - mX%)
                                FOR FTC = (startY%) TO (FTCU + 1) STEP -1
                                    ftcY! = ftcY! + 1
                                    LINE (startX% + (ftcY! / 2), FTC)-(mX% - (ftcY! / 2), FTC), currentColor~&
                                NEXT FTC
                            ELSEIF startY% < mY% THEN ' down
                                FTCU = startY% - (startX% - mX%)
                                FOR FTC = (startY%) TO (FTCU - 1)
                                    ftcY! = ftcY! + 1
                                    LINE (startX% + (ftcY! / 2), FTC)-(mX% - (ftcY! / 2), FTC), currentColor~&
                                NEXT FTC
                            END IF
                        END IF

                        _DEST 0
                    ELSEIF tool$ = "FRectangle" THEN
                        CALL PrepDrawDest(cimage&)
                        LINE (startX%, startY%)-(mX%, mY%), currentColor~&, BF
                        _DEST 0
                    END IF
                END IF

            END IF

            IF isButtonActive% = 0 THEN ' allows buttons to be pressed
                _DEST cimage&
                IF mX% > 10 AND mX% < 90 THEN ' all the buttons on left
                    IF mY% > 10 AND mY% < 35 THEN
                        isButtonPressed% = 1
                        LINE (10, 10)-(90, 35), _RGB32(0, 162, 232), B
                        IF _FILEEXISTS("image.bmp") THEN
                            loadimg& = _LOADIMAGE("image.bmp")
                            _PUTIMAGE (114, 10), loadimg&
                        END IF
                    ELSEIF mY% > 40 AND mY% < 65 THEN
                        isButtonPressed% = 1
                        LINE (10, 40)-(90, 65), _RGB32(0, 162, 232), B
                        CALL PaintTimeSave("image.bmp")
                    ELSEIF mY% > 70 AND mY% < 100 THEN
                        isButtonPressed% = 1
                        LINE (10, 70)-(90, 100), _RGB32(0, 162, 232), B
                        tool$ = "Pencil"
                    ELSEIF mY% > 110 AND mY% < 130 THEN
                        isButtonPressed% = 1
                        LINE (10, 110)-(90, 130), _RGB32(0, 162, 232), B
                        tool$ = "Line"
                    ELSEIF mY% > 135 AND mY% < 160 THEN
                        isButtonPressed% = 1
                        LINE (10, 135)-(90, 160), _RGB32(0, 162, 232), B
                        tool$ = "Circle"
                    ELSEIF mY% > 165 AND mY% < 195 THEN
                        isButtonPressed% = 1
                        LINE (10, 165)-(90, 195), _RGB32(0, 162, 232), B
                        tool$ = "Triangle"
                    ELSEIF mY% > 200 AND mY% < 225 THEN
                        isButtonPressed% = 1
                        LINE (10, 200)-(90, 225), _RGB32(0, 162, 232), B
                        tool$ = "Rectangle"
                    ELSEIF mY% > 230 AND mY% < 260 THEN
                        isButtonPressed% = 1
                        LINE (10, 230)-(90, 260), _RGB32(0, 162, 232), B
                        tool$ = "FCircle"
                    ELSEIF mY% > 265 AND mY% < 290 THEN
                        isButtonPressed% = 1
                        LINE (10, 265)-(90, 290), _RGB32(0, 162, 232), B
                        tool$ = "FTriangle"
                    ELSEIF mY% > 295 AND mY% < 325 THEN
                        isButtonPressed% = 1
                        LINE (10, 295)-(105, 325), _RGB32(0, 162, 232), B
                        tool$ = "FRectangle"
                    ELSEIF mY% > 330 AND mY% < 355 THEN
                        isButtonPressed% = 1
                        LINE (10, 330)-(90, 355), _RGB32(0, 162, 232), B
                        size% = 0
                    ELSEIF mY% > 360 AND mY% < 385 THEN
                        isButtonPressed% = 1
                        LINE (10, 360)-(90, 385), _RGB32(0, 162, 232), B
                        size% = 5
                    ELSEIF mY% > 390 AND mY% < 420 THEN
                        isButtonPressed% = 1
                        LINE (10, 390)-(90, 420), _RGB32(0, 162, 232), B
                        size% = 10
                    ELSEIF mY% > 425 AND mY% < 450 THEN
                        isButtonPressed% = 1
                        LINE (10, 425)-(90, 450), _RGB32(0, 162, 232), B
                        tool$ = "Eraser"
                    ELSEIF mY% > 455 AND mY% < 480 THEN
                        isButtonPressed% = 1
                        LINE (10, 455)-(90, 480), _RGB32(0, 162, 232), B
                        IF _FILEEXISTS("temp0.bmp") THEN
                            loadimg& = _LOADIMAGE("temp0.bmp")
                            _PUTIMAGE (114, 10), loadimg&
                        END IF
                    END IF

                ELSEIF mX% > 635 AND mX% < 730 THEN

                    IF mY% > 60 AND mY% < 80 THEN
                        isButtonPressed% = 1
                        LINE (635, 60)-(730, 80), _RGB32(0, 162, 232), B
                        IF isFullscreenOn% = 0 THEN
                            isFullscreenOn% = 1
                            _FULLSCREEN _SQUAREPIXELS , _SMOOTH
                            SLEEP 1
                        ELSE
                            isFullscreenOn% = 0
                            _FULLSCREEN _OFF
                        END IF
                    ELSEIF mY% > 120 AND mY% < 145 THEN
                        isButtonPressed% = 1
                        LINE (635, 120)-(730, 145), _RGB32(0, 162, 232), B
                        quit% = 1
                    ELSEIF mY% > 90 AND mY% < 115 THEN
                        isButtonPressed% = 1
                        LINE (635, 90)-(730, 115), _RGB32(0, 162, 232), B
                        SHELL "start website.url"

                    ELSEIF mY% > ((12 / 35) * 532) AND mY% < ((13 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(255, 255, 255)
                    ELSEIF mY% > ((13 / 35) * 532) AND mY% < ((14 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(255, 255, 0)
                    ELSEIF mY% > ((14 / 35) * 532) AND mY% < ((15 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(255, 165, 0)
                    ELSEIF mY% > ((15 / 35) * 532) AND mY% < ((16 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(255, 0, 0)
                    ELSEIF mY% > ((16 / 35) * 532) AND mY% < ((17 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(144, 238, 144)
                    ELSEIF mY% > ((17 / 35) * 532) AND mY% < ((18 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(0, 100, 0)
                    ELSEIF mY% > ((18 / 35) * 532) AND mY% < ((19 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(173, 216, 230)

                    ELSEIF mY% > ((20 / 35) * 532) AND mY% < ((21 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(0, 0, 139)
                    ELSEIF mY% > ((21 / 35) * 532) AND mY% < ((22 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(255, 192, 203)
                    ELSEIF mY% > ((22 / 35) * 532) AND mY% < ((23 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(128, 0, 128)
                    ELSEIF mY% > ((23 / 35) * 532) AND mY% < ((24 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(211, 211, 211)
                    ELSEIF mY% > ((24 / 35) * 532) AND mY% < ((25 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(64, 64, 64)
                    ELSEIF mY% > ((25 / 35) * 532) AND mY% < ((26 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(165, 42, 42)
                    ELSEIF mY% > ((26 / 35) * 532) AND mY% < ((27 / 35) * 532) THEN
                        isButtonPressed% = 1
                        LINE (635, 165)-(730, 430), _RGB32(0, 162, 232), B
                        currentColor~& = _RGB32(0, 0, 0)
                    END IF
                END IF
                _DEST 0

            END IF
        END IF
    ELSE
        IF isDrawing% = 1 THEN
            CALL PaintTimeSave("temp1.bmp")
            isDrawFinished% = 1
        END IF
        isButtonPressed% = 0
        isDrawing% = 0
    END IF

    IF isDrawFinished% = 1 THEN
        isDrawFinished% = 0
        isTemp0Saved% = 1
    END IF

    CLS

    _DEST cimage&

    IF isButtonPressed% = 0 THEN
        CALL gui
    END IF
    COLOR currentColor~&, _RGB32(195, 195, 195)
    LOCATE 2, 81: PRINT tool$
    COLOR _RGB32(0, 0, 0), _RGB32(195, 195, 195)
    LOCATE 1, 80: PRINT size%
    LOCATE 3, 80: PRINT mX% - 112
    LOCATE 3, 84: PRINT mY% - 10
    COLOR _RGB32(255, 255, 255), _RGB32(0, 0, 0)

    _DEST 0

    _PUTIMAGE (0, 0), fimage&
    _PUTIMAGE (0, 0), cimage&
    _DISPLAY
    _LIMIT 30

LOOP UNTIL _KEYDOWN(27) OR quit% = 1

IF loadimg& <> 0 THEN
    _FREEIMAGE loadimg&
END IF
_FREEIMAGE fimage&
_FREEIMAGE cimage&

END
':------------------------------------------------------------------------------------------
' end of the program


'start if subroutines and functions
':------------------------------------------------------------------------------------------
SUB PrepDrawDest (cimage&)
    _FREEIMAGE cimage&
    cimage& = _NEWIMAGE(_WIDTH, _HEIGHT)
    _DEST cimage&
END SUB

SUB gui ()
    LINE (113, 0)-(626, 9), _RGB32(0, 0, 0), BF ' upper bar
    LINE (113, 522)-(626, 532), _RGB32(0, 0, 0), BF 'lower bar

    LINE (0, 0)-(112, 532), _RGB32(195, 195, 195), BF 'left
    LINE (630, 0)-(740, 532), _RGB32(195, 195, 195), BF 'right

    LINE (110, 0)-(113, 532), _RGB32(0, 0, 0), BF ' left bar
    LINE (625, 0)-(629, 532), _RGB32(0, 0, 0), BF 'right bar

    CALL guiLeft
    CALL guiRight
END SUB

SUB guiLeft
    COLOR _RGB32(0, 0, 0), _RGB32(255, 255, 255)

    LINE (10, 10)-(90, 35), _RGB32(255, 255, 255), BF
    LINE (10, 10)-(90, 35), _RGB32(0, 0, 0), B
    LOCATE 2, 3: PRINT "Load"

    LINE (10, 40)-(90, 65), _RGB32(255, 255, 255), BF
    LINE (10, 40)-(90, 65), _RGB32(0, 0, 0), B
    LOCATE 4, 3: PRINT "Save"

    LINE (10, 70)-(90, 100), _RGB32(255, 255, 255), BF
    LINE (10, 70)-(90, 100), _RGB32(0, 0, 0), B
    LOCATE 6, 3: PRINT "Pencil"

    LINE (10, 110)-(90, 130), _RGB32(255, 255, 255), BF
    LINE (10, 110)-(90, 130), _RGB32(0, 0, 0), B
    LOCATE 8, 3: PRINT "Line"

    LINE (10, 135)-(90, 160), _RGB32(255, 255, 255), BF
    LINE (10, 135)-(90, 160), _RGB32(0, 0, 0), B
    LOCATE 10, 3: PRINT "Circle"

    LINE (10, 165)-(90, 195), _RGB32(255, 255, 255), BF
    LINE (10, 165)-(90, 195), _RGB32(0, 0, 0), B
    LOCATE 12, 3: PRINT "Triangle"

    LINE (10, 200)-(90, 225), _RGB32(255, 255, 255), BF
    LINE (10, 200)-(90, 225), _RGB32(0, 0, 0), B
    LOCATE 14, 3: PRINT "Rectangle"

    LINE (10, 230)-(90, 260), _RGB32(255, 255, 255), BF
    LINE (10, 230)-(90, 260), _RGB32(0, 0, 0), B
    LOCATE 16, 3: PRINT "FCircle"

    LINE (10, 265)-(90, 290), _RGB32(255, 255, 255), BF
    LINE (10, 265)-(90, 290), _RGB32(0, 0, 0), B
    LOCATE 18, 3: PRINT "FTriangle"

    LINE (10, 295)-(105, 325), _RGB32(255, 255, 255), BF
    LINE (10, 295)-(105, 325), _RGB32(0, 0, 0), B
    LOCATE 20, 3: PRINT "FRectangle "

    LINE (10, 330)-(90, 355), _RGB32(255, 255, 255), BF
    LINE (10, 330)-(90, 355), _RGB32(0, 0, 0), B
    LOCATE 22, 3: PRINT "Small"

    LINE (10, 360)-(90, 385), _RGB32(255, 255, 255), BF
    LINE (10, 360)-(90, 385), _RGB32(0, 0, 0), B
    LOCATE 24, 3: PRINT "Medium"

    LINE (10, 390)-(90, 420), _RGB32(255, 255, 255), BF
    LINE (10, 390)-(90, 420), _RGB32(0, 0, 0), B
    LOCATE 26, 3: PRINT "Large"

    LINE (10, 425)-(90, 450), _RGB32(255, 255, 255), BF
    LINE (10, 425)-(90, 450), _RGB32(0, 0, 0), B
    LOCATE 28, 3: PRINT "eraser"

    LINE (10, 455)-(90, 480), _RGB32(255, 255, 255), BF
    LINE (10, 455)-(90, 480), _RGB32(0, 0, 0), B
    LOCATE 30, 3: PRINT "Undo"

    COLOR _RGB32(255, 255, 255), _RGB32(0, 0, 0)
END SUB

SUB guiRight

    COLOR _RGB32(0, 0, 0), _RGB32(255, 255, 255)

    LINE (635, 60)-(730, 80), _RGB32(255, 255, 255), BF
    LINE (635, 60)-(730, 80), _RGB32(0, 0, 0), B
    LOCATE 5, 81: PRINT "Fullscreen"

    LINE (635, 90)-(730, 115), _RGB32(255, 255, 255), BF
    LINE (635, 90)-(730, 115), _RGB32(0, 0, 0), B
    LOCATE 7, 81: PRINT "Browser"

    LINE (635, 120)-(730, 145), _RGB32(255, 255, 255), BF
    LINE (635, 120)-(730, 145), _RGB32(0, 0, 0), B
    LOCATE 9, 81: PRINT "Quit"

    LINE (635, 165)-(730, 430), _RGB32(255, 255, 255), BF
    LINE (635, 165)-(730, 430), _RGB32(0, 0, 0), B

    LOCATE 12, 81: PRINT "White"
    LOCATE 13, 81: PRINT "Yellow"
    LOCATE 14, 81: PRINT "Orange"
    LOCATE 15, 81: PRINT "Red"
    LOCATE 16, 81: PRINT "Light Green"
    LOCATE 17, 81: PRINT "Dark Green"
    LOCATE 18, 81: PRINT "Light Blue"

    LOCATE 20, 81: PRINT "Dark Blue"
    LOCATE 21, 81: PRINT "Pink"
    LOCATE 22, 81: PRINT "Purple"
    LOCATE 23, 81: PRINT "Light Grey"
    LOCATE 24, 81: PRINT "Dark Grey"
    LOCATE 25, 81: PRINT "Brown"
    LOCATE 26, 81: PRINT "Black"

    COLOR _RGB32(255, 255, 255), _RGB32(0, 0, 0)

END SUB

'Saveimage v2.3d https://qb64phoenix.com/forum/showthread.php?tid=1253&pid=12103
'HANDLES THE SAVING OF IMAGES
SUB PaintTimeSave (filename$)
    DIM S
    S = _SOURCE
    CALL SaveBMP(filename$, S, 114, 10, 625, 521)
END SUB

SUB PaintTimeStartup (filename$, image&)
    CALL SaveBMP(filename$, image&, 114, 10, 625, 521)
END SUB

SUB SaveBMP (filename$, image&, x1%, y1%, x2%, y2%)
    'Super special STEVE-Approved BMP Export routine for use with any QB64 graphic mode.
    IF x2% = _WIDTH(image&) THEN x2% = x2% - 1
    IF y2% = _HEIGHT(image&) THEN y2% = y2% - 1

    TYPE BMPFormat
        ID AS STRING * 2
        Size AS LONG
        Blank AS LONG
        Offset AS LONG
        Hsize AS LONG
        PWidth AS LONG
        PDepth AS LONG
        Planes AS INTEGER
        BPP AS INTEGER
        Compression AS LONG
        ImageBytes AS LONG
        Xres AS LONG
        Yres AS LONG
        NumColors AS LONG
        SigColors AS LONG
    END TYPE

    ' handles the errors with option _explicit
    DIM imagehandle%, s&, OffsetBITS&, ZeroPAD$, ImageSize&, zp&, c&, cv&, b$, w&, f, t1$

    DIM BMP AS BMPFormat
    DIM x AS LONG, y AS LONG
    DIM temp AS STRING

    DIM n AS _MEM, o AS _OFFSET, m AS _MEM
    m = _MEMIMAGE(image&)

    IF x1% > x2% THEN SWAP x1%, x2%
    IF y1% > y2% THEN SWAP y1%, y2%
    IF x2% = _WIDTH(imagehandle%) THEN x2% = _WIDTH(imagehandle%) - 1 'troubleshoot in case user does a common mistake for 0-width instead of 0 - (width-1) for fullscreen
    IF y2% = _HEIGHT(imagehandle%) THEN y2% = _HEIGHT(imagehandle%) - 1 'troubleshoot in case user does a common mistake for 0-width instead of 0 - (width-1) for fullscreen

    s& = _SOURCE
    _SOURCE image&

    BMP.PWidth = (x2% - x1%) + 1
    BMP.PDepth = (y2% - y1%) + 1
    BMP.ID = "BM"
    BMP.Blank = 0
    BMP.Hsize = 40
    BMP.Planes = 1
    BMP.Compression = 0
    BMP.Xres = 0
    BMP.Yres = 0

    BMP.SigColors = 0

    SELECT CASE _PIXELSIZE(image&)
        CASE 1
            temp = SPACE$(x2% - x1% + 1)
            OffsetBITS& = 54 + 1024 'add palette in 256 color modes
            BMP.BPP = 8
            IF BMP.PWidth MOD 4 THEN ZeroPAD$ = SPACE$(4 - (BMP.PWidth MOD 4))
            ImageSize& = (BMP.PWidth + LEN(ZeroPAD$)) * BMP.PDepth
            BMP.ImageBytes = ImageSize&
            BMP.NumColors = 256
            BMP.Size = ImageSize& + OffsetBITS&
            BMP.Offset = OffsetBITS&
        CASE 4
            temp = SPACE$(3)
            OffsetBITS& = 54 'no palette in 24/32 bit
            BMP.BPP = 24
            IF ((BMP.PWidth * 3) MOD 4) THEN ZeroPAD$ = SPACE$(4 - ((BMP.PWidth * 3) MOD 4))
            ImageSize& = (BMP.PWidth + LEN(ZeroPAD$)) * BMP.PDepth
            BMP.ImageBytes = ImageSize&
            BMP.NumColors = 0
            BMP.Size = ImageSize& * 3 + OffsetBITS&
            BMP.Offset = OffsetBITS&
    END SELECT

    n = _MEMNEW(BMP.Size)
    _MEMPUT n, n.OFFSET, BMP
    o = n.OFFSET + 54
    zp& = LEN(ZeroPAD$)
    $CHECKING:OFF

    IF BMP.BPP = 8 THEN 'Store the Palette for 256 color mode
        FOR c& = 0 TO 255 ' read BGR color settings from JPG image + 1 byte spacer(CHR$(0))
            cv& = _PALETTECOLOR(c&, image&) ' color attribute to read.
            b$ = CHR$(_BLUE32(cv&)) + CHR$(_GREEN32(cv&)) + CHR$(_RED32(cv&)) + CHR$(0) 'spacer byte
            _MEMPUT n, o, b$
            o = o + 4
        NEXT
        y = y2% + 1
        w& = _WIDTH(image&)
        x = x2% - x1% + 1
        DO
            y = y - 1
            _MEMGET m, m.OFFSET + (w& * y + x1%), temp
            _MEMPUT n, o, temp
            o = o + x
            _MEMPUT n, o, ZeroPAD$
            o = o + zp&
        LOOP UNTIL y = y1%
    ELSE
        y = y2% + 1
        w& = _WIDTH(image&)
        DO
            y = y - 1: x = x1% - 1
            DO
                x = x + 1
                _MEMGET m, m.OFFSET + (w& * y + x) * 4, temp
                _MEMPUT n, o, temp
                o = o + 3
            LOOP UNTIL x = x2%
            _MEMPUT n, o, ZeroPAD$
            o = o + zp&
        LOOP UNTIL y = y1%
    END IF
    $CHECKING:ON
    f = FREEFILE
    OPEN filename$ FOR BINARY AS #f ' the writing part
    t1$ = SPACE$(BMP.Size)
    _MEMGET n, n.OFFSET, t1$
    PUT #f, , t1$
    CLOSE #f
    _SOURCE s&
    _MEMFREE m
    _MEMFREE n
END SUB
':------------------------------------------------------------------------------------------
' end of subroutines and functions



