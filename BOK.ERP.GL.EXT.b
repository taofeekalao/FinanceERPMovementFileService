*******************************************************************
*   Meraki Systems
*   August 17, 2022
*   This Is To Genreate T24 GL Movements Flat File For Third Party Application.
*
*******************************************************************
*	Meraki Systems
*	December 3, 2022
*	Modification To Include Extraction Of Balances From Keys
*	EXT.RESERVED.20 Is Used In This Case For CCY Balance
*	EXT.RESERVED.19 Is Used In This Case For LCY Balance
*******************************************************************
*	Meraki Systems
*	December 15, 2022
*	Modification To Change From Opening Balances To Closing Balances
*	Added Calculation Of Difference / Imbalances To Balance Reporting
*******************************************************************
    $PACKAGE BOK.ERP.GL.EXT
    SUBROUTINE BOK.ERP.GL.EXT(WORK.ID)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CONSOLIDATE.ASST.LIAB
    $INSERT I_F.CONSOLIDATE.PRFT.LOSS
    $INSERT I_F.DATES
    $INSERT I_F.CURRENCY
    $INSERT I_F.BOK.ERP.GL.PARAMETER
    $INSERT I_F.BOK.GEN.PARAMETER
    $INSERT I_F.RE.STAT.REP.LINE
    $INSERT I_F.ERP.GL
    $INSERT I_BATCH.FILES
    $INSERT I_BOK.ERP.GL.EXT

    DFF.DATA = ""
    PREV.ID = ""
    RE.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
    FM.DATE = RE.DATE[1,4] : "/" : RE.DATE[5,2] : "/" : RE.DATE[7,2]
    LPERIOD = RE.DATE[5,2]

    * Suspending Checks For Movement To Add All Balances
    *NEW.KEY.FLAG = 0

    BATCH.NAME = "BKGL" : RE.DATE[2] : RE.DATE[5,2] : RE.DATE[1,4]

    RSLC.ID = WORK.ID['*',1,1]
    RE.KEY = WORK.ID['*',2,1]
    RE.TYPE = WORK.ID['*',3,1]

    IF RE.KEY[1,2] EQ "RE" THEN
        RETURN
    END

    LINE.ID = FIELD(RSLC.ID, ".", 1, 2)

    IF PREV.ID NE LINE.ID THEN
        READ R.LINE FROM F.STAT.REP.LINE, LINE.ID ELSE R.LINE = ""
        PREV.ID = LINE.ID
    END
    LINE.DESC = R.LINE<RE.SRL.DESC,2,1>
    LINE.TYPE = R.LINE<RE.SRL.TYPE>

    *   PROCESS.RSLC
    *****************
    CO.CODE = FIELD(RSLC.ID, ".", 3, 1)

    IF (RE.KEY[1,2] <> 'PL') THEN
        RE.CCY = FIELD(RE.KEY, ".", 4, 1)
        GOSUB PROCESS.CAL
    END ELSE
        RE.CCY = RE.TYPE
        GOSUB PROCESS.CPL
    END

    * Suspending Checks For Movement To Add All Balances
    *IF NEW.KEY.FLAG EQ 1 THEN
    *    RETURN
    *END
!   ---------------------------------------------------------------------
    IF (DFF.DATA<2>) OR (DFF.DATA<3>) THEN
        GOSUB PROCESS.DFF
    END

    RETURN

PROCESS.CAL:
************
    READ R.CAL FROM F.CAL, RE.KEY ELSE R.CAL = ''
    LOCATE RE.TYPE IN R.CAL<RE.ASL.TYPE,1> SETTING APS ELSE
        RETURN
    END

    LCY.BALANCE = 0
    CCY.BALANCE = 0

    LCY.DB.MVMT = 0
    CCY.DB.MVMT = 0

    LCY.CR.MVMT = 0
    CCY.CR.MVMT = 0

    IF R.CAL<RE.ASL.DATE.LAST.UPDATE> EQ RE.DATE THEN
        IF RE.CCY EQ LCCY THEN
            LCY.DB.MVMT += R.CAL<RE.ASL.DEBIT.MOVEMENT, APS>
            LCY.CR.MVMT += R.CAL<RE.ASL.CREDIT.MOVEMENT, APS>
            LCY.BALANCE += R.CAL<RE.ASL.BALANCE, APS> + R.CAL<RE.ASL.CREDIT.MOVEMENT, APS> + R.CAL<RE.ASL.DEBIT.MOVEMENT, APS>
        END ELSE
            LCY.DB.MVMT += R.CAL<RE.ASL.LOCAL.DEBIT.MVE, APS>
            LCY.CR.MVMT += R.CAL<RE.ASL.LOCAL.CREDT.MVE, APS>
            LCY.BALANCE += R.CAL<RE.ASL.LOCAL.BALANCE, APS> + R.CAL<RE.ASL.LOCAL.CREDT.MVE, APS> + R.CAL<RE.ASL.LOCAL.DEBIT.MVE, APS>
        END
        CCY.DB.MVMT += R.CAL<RE.ASL.DEBIT.MOVEMENT, APS>
        CCY.CR.MVMT += R.CAL<RE.ASL.CREDIT.MOVEMENT, APS>
        CCY.BALANCE += R.CAL<RE.ASL.BALANCE, APS> + R.CAL<RE.ASL.CREDIT.MOVEMENT, APS> + R.CAL<RE.ASL.DEBIT.MOVEMENT, APS>
    END

    * Suspending Checks For Movement To Add All Balances
    *IF NOT(LCY.DB.MVMT + CCY.DB.MVMT) THEN
    *    IF NOT(LCY.CR.MVMT + CCY.CR.MVMT) THEN
    *        NEW.KEY.FLAG = 1
    *        RETURN
    *    END
    *END

    R.DATA = ""
    R.DATA<EXT.STATUS.CODE> = LINE.DESC          ;*R.RSLC<RE.SLC.DESC,2,1>
    R.DATA<EXT.CURRENCY> = RE.CCY
    R.DATA<EXT.COMPANY> = "1200"

    COMP.CODE = CO.CODE
    GOSUB GET.ERP.CODE
    R.DATA<EXT.BRANCH> = ERP.CODE
    R.DATA<EXT.ACCOUNT> = LINE.DESC             ;*R.RSLC<RE.SLC.DESC,2,1>

    R.DATA<EXT.SECTOR.ACTV> = FIELD(RE.KEY, ".", 6, 1)
    R.DATA<EXT.INST.SECTOR> = FIELD(RE.KEY, ".", 11, 1)
    R.DATA<EXT.COST.CENTER> = "2105"

    GRP.ID = CHANGE(R.DATA, @FM, "*")
    CALL F.READU(FN.ERP.GL.TAB, GRP.ID, R.ERP.GL.REC, F.ERP.GL.TAB, ERP.GL.ERR, "")
    IF (R.ERP.GL.REC) THEN
        R.DATA<EXT.CCY.DR.AMT> = R.ERP.GL.REC<EXT.CCY.DR.AMT> + CCY.DB.MVMT * (-1)
        R.DATA<EXT.LCY.DR.AMT> = R.ERP.GL.REC<EXT.LCY.DR.AMT> + LCY.DB.MVMT * (-1)

        R.DATA<EXT.CCY.CR.AMT> = R.ERP.GL.REC<EXT.CCY.CR.AMT> + CCY.CR.MVMT
        R.DATA<EXT.LCY.CR.AMT> = R.ERP.GL.REC<EXT.LCY.CR.AMT> + LCY.CR.MVMT

        R.DATA<EXT.RESERVED.20> = R.ERP.GL.REC<EXT.RESERVED.20> + CCY.BALANCE
        R.DATA<EXT.RESERVED.19> = R.ERP.GL.REC<EXT.RESERVED.19> + LCY.BALANCE
    END ELSE
        R.DATA<EXT.CCY.DR.AMT> += CCY.DB.MVMT * (-1)
        R.DATA<EXT.LCY.DR.AMT> += LCY.DB.MVMT * (-1)

        R.DATA<EXT.CCY.CR.AMT> += CCY.CR.MVMT
        R.DATA<EXT.LCY.CR.AMT> += LCY.CR.MVMT

        R.DATA<EXT.RESERVED.20> += CCY.BALANCE
        R.DATA<EXT.RESERVED.19> += LCY.BALANCE
    END

    *   -----------------------------------------
    *   Processing Difference For Current CAL Key
    *********************************************
    DFF.DATA<1> = ""
    DIFF.ID = CO.CODE : "*" : RE.CCY
    CALL F.READU(FN.ERP.GL.TAB, DIFF.ID, R.ERP.GL.REC, F.ERP.GL.TAB, ERP.GL.ERR, "")
    IF (R.ERP.GL.REC) THEN
        DFF.DATA<1> = DIFF.ID
        DFF.DATA<2> = R.ERP.GL.REC<EXT.LCY.DR.AMT> + LCY.DB.MVMT
        DFF.DATA<3> = R.ERP.GL.REC<EXT.LCY.CR.AMT> + LCY.CR.MVMT
        DFF.DATA<4> = R.ERP.GL.REC<EXT.CCY.DR.AMT> + CCY.DB.MVMT
        DFF.DATA<5> = R.ERP.GL.REC<EXT.CCY.CR.AMT> + CCY.CR.MVMT

        *** Adding difference calculation in balance reporting
        DFF.DATA<6> = R.ERP.GL.REC<EXT.RESERVED.20> + CCY.BALANCE        ;   *   CCY Reporting
        DFF.DATA<7> = R.ERP.GL.REC<EXT.RESERVED.19> + LCY.BALANCE        ;   *   LCY Reporting
        ***
    END ELSE
        DFF.DATA<1> = DIFF.ID
        DFF.DATA<2> = LCY.DB.MVMT
        DFF.DATA<3> = LCY.CR.MVMT
        DFF.DATA<4> = CCY.DB.MVMT
        DFF.DATA<5> = CCY.CR.MVMT

        *** Adding difference calculation in balance reporting
        DFF.DATA<6> = CCY.BALANCE        ;   *   CCY Reporting
        DFF.DATA<7> = LCY.BALANCE        ;   *   LCY Reporting
        ***
    END
    *   End Difference Processing

    GOSUB WRITE.DATA
    RETURN

PROCESS.CPL:
************
    READ R.CPL FROM F.CPL, RE.KEY ELSE R.CPL = ''
    LOCATE RE.CCY IN R.CPL<RE.PTL.CURRENCY,1> SETTING CPS ELSE
        RETURN
    END

    LCY.BALANCE = 0
    CCY.BALANCE = 0

    LCY.DB.MVMT = 0
    CCY.DB.MVMT = 0

    LCY.CR.MVMT = 0
    CCY.CR.MVMT = 0

    IF R.CPL<RE.PTL.DATE.LAST.UPDATE> EQ RE.DATE THEN
        LCY.DB.MVMT += R.CPL<RE.PTL.DEBIT.MOVEMENT, CPS>
        LCY.CR.MVMT += R.CPL<RE.PTL.CREDIT.MOVEMENT, CPS>
        LCY.BALANCE += R.CPL<RE.PTL.BALANCE, CPS> + R.CPL<RE.PTL.BALANCE.YTD, CPS> + R.CPL<RE.PTL.DEBIT.MOVEMENT, CPS> + R.CPL<RE.PTL.CREDIT.MOVEMENT, CPS>

        IF RE.CCY EQ LCCY THEN
            CCY.DB.MVMT += LCY.DB.MVMT
            CCY.CR.MVMT += LCY.CR.MVMT
            CCY.BALANCE += LCY.BALANCE
        END ELSE
            CCY.DB.MVMT += R.CPL<RE.PTL.CCY.DEBIT.MVE, CPS>
            CCY.CR.MVMT += R.CPL<RE.PTL.CCY.CREDT.MVE, CPS>
            CCY.BALANCE += R.CPL<RE.PTL.CCY.BALANCE, CPS> + R.CPL<RE.PTL.CCY.BALANCE.YTD, CPS> + R.CPL<RE.PTL.CCY.DEBIT.MVE, CPS> + R.CPL<RE.PTL.CCY.CREDT.MVE, CPS>
        END
    END

    * Suspending Checks For Movement To Add All Balances
    *IF NOT(LCY.DB.MVMT + CCY.DB.MVMT) THEN
    *    IF NOT(LCY.CR.MVMT + CCY.CR.MVMT) THEN
    *        NEW.KEY.FLAG = 1
    *        RETURN
    *    END
    *END

    R.DATA = ""
    R.DATA<EXT.STATUS.CODE> = LINE.DESC         ;*R.RSLC<RE.SLC.DESC,2,1>
    R.DATA<EXT.CURRENCY> = RE.CCY
    R.DATA<EXT.COMPANY> = "1200"

    COMP.CODE = CO.CODE
    GOSUB GET.ERP.CODE
    R.DATA<EXT.BRANCH> = ERP.CODE

    R.DATA<EXT.ACCOUNT> = LINE.DESC             ;*R.RSLC<RE.SLC.DESC,2,1>
    R.DATA<EXT.SECTOR.ACTV> = FIELD(RE.KEY, ".", 4, 1)
    R.DATA<EXT.COST.CENTER> = "2105"

    GRP.ID = CHANGE(R.DATA, @FM, "*")
    CALL F.READU(FN.ERP.GL.TAB, GRP.ID, R.ERP.GL.REC, F.ERP.GL.TAB, ERP.GL.ERR, "")
    IF (R.ERP.GL.REC) THEN
        R.DATA<EXT.CCY.DR.AMT> = R.ERP.GL.REC<EXT.CCY.DR.AMT> + CCY.DB.MVMT * (-1)
        R.DATA<EXT.LCY.DR.AMT> = R.ERP.GL.REC<EXT.LCY.DR.AMT> + LCY.DB.MVMT * (-1)

        R.DATA<EXT.CCY.CR.AMT> = R.ERP.GL.REC<EXT.CCY.CR.AMT> + CCY.CR.MVMT
        R.DATA<EXT.LCY.CR.AMT> = R.ERP.GL.REC<EXT.LCY.CR.AMT> + LCY.CR.MVMT

        R.DATA<EXT.RESERVED.20> = R.ERP.GL.REC<EXT.RESERVED.20> + CCY.BALANCE
        R.DATA<EXT.RESERVED.19> = R.ERP.GL.REC<EXT.RESERVED.19> + LCY.BALANCE
    END ELSE
        R.DATA<EXT.CCY.DR.AMT> += CCY.DB.MVMT * (-1)
        R.DATA<EXT.LCY.DR.AMT> += LCY.DB.MVMT * (-1)

        R.DATA<EXT.CCY.CR.AMT> += CCY.CR.MVMT
        R.DATA<EXT.LCY.CR.AMT> += LCY.CR.MVMT

        R.DATA<EXT.RESERVED.20> += CCY.BALANCE
        R.DATA<EXT.RESERVED.19> += LCY.BALANCE
    END

    *   -----------------------------------------
    *   Processing Difference For Current CPL Key
    *********************************************
    DFF.DATA<1> = ""
    DIFF.ID = CO.CODE : "*" : RE.CCY
    CALL F.READU(FN.ERP.GL.TAB, DIFF.ID, R.ERP.GL.REC, F.ERP.GL.TAB, ERP.GL.ERR, "")
    IF (R.ERP.GL.REC) THEN
        DFF.DATA<1> = DIFF.ID
        DFF.DATA<2> = R.ERP.GL.REC<EXT.LCY.DR.AMT> + LCY.DB.MVMT
        DFF.DATA<3> = R.ERP.GL.REC<EXT.LCY.CR.AMT> + LCY.CR.MVMT
        DFF.DATA<4> = R.ERP.GL.REC<EXT.CCY.DR.AMT> + CCY.DB.MVMT
        DFF.DATA<5> = R.ERP.GL.REC<EXT.CCY.CR.AMT> + CCY.CR.MVMT

        *** Adding difference calculation in balance reporting
        DFF.DATA<6> = R.ERP.GL.REC<EXT.RESERVED.20> + CCY.BALANCE        ;   *   CCY Reporting
        DFF.DATA<7> = R.ERP.GL.REC<EXT.RESERVED.19> + LCY.BALANCE        ;   *   LCY Reporting
        ***
    END ELSE
        DFF.DATA<1> = DIFF.ID
        DFF.DATA<2> = LCY.DB.MVMT
        DFF.DATA<3> = LCY.CR.MVMT
        DFF.DATA<4> = CCY.DB.MVMT
        DFF.DATA<5> = CCY.CR.MVMT

        *** Adding difference calculation in balance reporting
        DFF.DATA<6> = CCY.BALANCE        ;   *   CCY Reporting
        DFF.DATA<7> = LCY.BALANCE        ;   *   LCY Reporting
        ***
    END
    *   End Difference Processing

    GOSUB WRITE.DATA
    RETURN

PROCESS.DFF:
************
    DIFF.ID = DFF.DATA<1>
    *DR.LCY.BALANCE = DFF.DATA<2>
    *CR.LCY.BALANCE = DFF.DATA<3>
    *DR.CCY.BALANCE = DFF.DATA<4>
    *CR.CCY.BALANCE = DFF.DATA<5>

    CO.CODE = FIELD(DIFF.ID, "*", 1, 1)
    RE.CCY = FIELD(DIFF.ID, "*", 2, 1)

    R.DATA = ""
    R.DATA<EXT.CURRENCY> = RE.CCY
    R.DATA<EXT.COMPANY> = "1200"

    COMP.CODE = CO.CODE
    GOSUB GET.ERP.CODE
    R.DATA<EXT.BRANCH> = ERP.CODE

    R.DATA<EXT.ACCOUNT> = "23227000"
    R.DATA<EXT.COST.CENTER> = "2105"

    R.DATA<EXT.CCY.DR.AMT> = DFF.DATA<4>    ;   *   DR.CCY.BALANCE
    R.DATA<EXT.LCY.DR.AMT> = DFF.DATA<2>    ;   *   DR.LCY.BALANCE

    R.DATA<EXT.CCY.CR.AMT> = DFF.DATA<5>    ;   *   CR.CCY.BALANCE
    R.DATA<EXT.LCY.CR.AMT> = DFF.DATA<3>    ;   *   CR.LCY.BALANCE

    R.DATA<EXT.RESERVED.20> =  DFF.DATA<6>  ;   *   CCY.CLOSE.BALANCE
    R.DATA<EXT.RESERVED.19> = DFF.DATA<7>   ;   *   LCY.CLOSE.BALANCE


    GRP.ID = DIFF.ID
    GOSUB WRITE.DATA

    RETURN

WRITE.DATA:
***********
    R.DATA<EXT.STATUS.CODE> = "NEW"
    R.DATA<EXT.LEDGER.ID> = "300000001807221"
    R.DATA<EXT.EFF.DATE> = FM.DATE
    R.DATA<EXT.SOURCE> = "T24"
    R.DATA<EXT.CATEGORY> = "GL Daily Import"
    R.DATA<EXT.BOOK.DATE> = FM.DATE
    R.DATA<EXT.ACTUAL.FLAG> = "A"

    BEGIN CASE
        CASE NOT(R.DATA<EXT.ACCOUNT>)
            R.DATA<EXT.ACCOUNT> = "DEFAULTED-56211400"
        CASE R.DATA<EXT.ACCOUNT> EQ '41110000'
            R.DATA<EXT.ACCOUNT> = "41111000"
    END CASE

    R.DATA<EXT.LOB> = "0000"
    R.DATA<EXT.INTR.COMPANY> = "0000"

    SEC.CODE = R.DATA<EXT.SECTOR.ACTV>

    R.DATA<EXT.SECTOR.ACTV> = "0000"
    R.DATA<EXT.INST.SECTOR> = "0000"

    R.DATA<EXT.FUTURE.1> = "0000"
    R.DATA<EXT.FUTURE.2> = "0000"

    R.DATA<EXT.EXCH.USER> = "User"
    R.DATA<EXT.EXCH.RATE.DATE> = R.DATA<EXT.EFF.DATE>
    R.DATA<EXT.EXCH.RATE> = 1

    IF R.DATA<EXT.CURRENCY> NE LCCY THEN
        REP.DATE = RE.DATE
        CCY.ID = R.DATA<EXT.CURRENCY>
        RET.CODE = ""
        R.CCY = ""

        CALL GET.CCY.HISTORY(REP.DATE, CCY.ID, R.CCY, RET.CODE)
        R.DATA<EXT.EXCH.RATE> = R.CCY<EB.CUR.MID.REVAL.RATE, 1>
    END

    R.DATA<EXT.JRNL.BATCH.NAME> = BATCH.NAME
    R.DATA<EXT.INTRFCE.GRP.ID> = RE.DATE

    R.DATA<EXT.CCY.DR.AMT> = R.DATA<EXT.CCY.DR.AMT> + 0
    R.DATA<EXT.CCY.CR.AMT> = R.DATA<EXT.CCY.CR.AMT> + 0

    IF NOT(R.DATA<EXT.CCY.DR.AMT>) THEN
        R.DATA<EXT.CCY.DR.AMT> = ''
    END

    IF NOT(R.DATA<EXT.CCY.CR.AMT>) THEN
        R.DATA<EXT.CCY.CR.AMT> = ''
    END

    CALL F.WRITE(F.ERP.GL.TAB, GRP.ID, R.DATA)
    CALL JOURNAL.UPDATE("")
    CALL F.RELEASE(FN.ERP.GL.TAB, GRP.ID, F.ERP.GL.TAB)

    RETURN

GET.ERP.CODE:
*************
    CALL F.READ(FN.BOK.PARAM, 'ERP.GL.COMP.CODE', ERP.PARAM.REC, F.BOK.PARAM, ERP.PARAM.ERR)
    ERP.CODE = ''

    FIND COMP.CODE IN ERP.PARAM.REC<BOK.GEN.PAR.VARIABLE.NAME> SETTING V.FLD, V.VAL, V.SUB.VAL ELSE
        ERP.CODE = CO.CODE
    END
    ERP.CODE = ERP.PARAM.REC<BOK.GEN.PAR.VARIABLE.VALUE, V.VAL>

    RETURN
END