*******************************************************************
*   Meraki Systems
*   August 17, 2022
*   This Is To Genreate T24 GL Movements Flat File For Third Party.
*
*******************************************************************
    SUBROUTINE BOK.ERP.GL.EXT.SELECT

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DAS.COMMON
    $INSERT I_DAS.RE.STAT.LINE.PL.WORK
    $INSERT I_DAS.RE.STAT.LINE.AL.WORK
    $INSERT I_BOK.ERP.GL.EXT

*   Clear Table Before Generating New Records
    DEL.CMD = "CLEAR-FILE F.ERP.GL"
    EXECUTE DEL.CMD

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
    SELECTION.LIST = AL.LIST:FM:PL.LIST
    *SELECTION.LIST = "BKTB.0050.RW0010002*AC.1.TR.RWF.10000........1....RW0010002*DEBIT*"
    *SELECTION.LIST<-1> = "BKTB.0100.RW0010062*AC.1.TR.EUR.10000........1....RW0010062*DEBIT*"
    *SELECTION.LIST<-1> = "BKTB.0050.RW0010002*AC.1.TR.RWF.10199........403....RW0010002*DEBIT*"
    *SELECTION.LIST<-1> = "BKTB.0110.RW0010018*AC.1.TR.GBP.10011........403....RW0010018*DEBIT*"
*   -----------------------------------
    CALL BATCH.BUILD.LIST("", SELECTION.LIST)

    RETURN
END