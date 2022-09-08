*******************************************************************
*   Meraki Systems
*   August 17, 2022
*   This Is To Genreate T24 GL Movements Flat File For Third Party Application.
*
*******************************************************************
    $PACKAGE BOK.ERP.GL.EXT
    SUBROUTINE BOK.ERP.GL.EXT.SELECT

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DAS.COMMON
    $INSERT I_DAS.RE.STAT.LINE.PL.WORK
    $INSERT I_DAS.RE.STAT.LINE.AL.WORK
    $INSERT I_BOK.ERP.GL.EXT

*   Clear Table Before Generating New Records
    DEL.CMD.LIVE = "CLEAR-FILE F.ERP.GL"
    EXECUTE DEL.CMD.LIVE

*   Report Head
    ARGS = "BKTB"

*   Table Suffix ~ Live Is Usually Empty Suffix
    TABLE.SUFFIX = ""

*   Selects All Asset & Liability Work Table Records 
    AL.LIST  = dasReStatLineAlWorkHavingReportName
    CALL DAS("RE.STAT.LINE.AL.WORK", AL.LIST, ARGS, TABLE.SUFFIX)

*   Selects All Profit & Loss Work Table Records 
    PL.LIST  = dasReStatLinePlWorkHavingReportName
    CALL DAS("RE.STAT.LINE.PL.WORK", PL.LIST, ARGS, TABLE.SUFFIX)

*   Merging Dynamic Arrays List
    SELECTION.LIST = AL.LIST:@FM:PL.LIST
    CALL BATCH.BUILD.LIST("", SELECTION.LIST)

    RETURN
END