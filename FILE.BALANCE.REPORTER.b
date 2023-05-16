	*---------------------------------------------------------------------
	* Modification History :
	*	taofeek alao
	*	December 03, 2022
	*	Meraki Systems
	*	This is file transporter for balances
	*	Initial Dev
	*---------------------------------------------------------------------
    $PACKAGE BOK.ERP.GL.EXT
    SUBROUTINE FILE.BALANCE.REPORTER

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_F.ERP.GL
    $INSERT I_F.BOK.ERP.GL.PARAMETER


******************
*   Initialisation
******************
    RE.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
    INTERMEDIATE.REC = ""

    *   Work Table
    FN.ERP.GL.TAB = 'F.ERP.GL'
    F.ERP.GL.TAB = ''
    CALL OPF(FN.ERP.GL.TAB, F.ERP.GL.TAB)

    *   Backup Directory   
    FN.BACKUP = 'ERP.GL.BACKUP'
    F.BACKUP = ''
    CALL OPF(FN.BACKUP, F.BACKUP)

    *   Parameter Table
    FN.ERP.GL.PARAM = "F.BOK.ERP.GL.PARAMETER"
    F.ERP.GL.PARAM = ""
    CALL OPF(FN.ERP.GL.PARAM, F.ERP.GL.PARAM)

    ERP.GL.PARAM.ID = "RW0010001"
    READ R.GL.PARAM FROM F.ERP.GL.PARAM, ERP.GL.PARAM.ID THEN
        EXT.DIR = R.GL.PARAM<BOK.ERP.ERP.UPLOAD.PATH>
        FILE.SEP = R.GL.PARAM<BOK.ERP.SEPARATOR>
        FILE.NAME = R.GL.PARAM<BOK.ERP.FILE.NAME>
    END

	*   Exchange Directory  /   Work Directory
    FN.EXCHANGE = EXT.DIR
    F.EXCHANGE = ''
    CALL OPF(FN.EXCHANGE, F.EXCHANGE)

    FILE.NAME = FILE.NAME:"_":RE.DATE:".csv"
    FINAL.REC = ""

	*	Set Headers
    FINAL.REC<-1> = "Status Code,Ledger ID,Effective Date of Transaction,Journal Source,Journal Category,Currency Code,Journal Entry Creation Date,Actual Flag,Company,CostCenter,Branches,Account,LoB,Intercompany,Sector of activities,Institutional sector,Future1,Future2"
    COL.COUNT = 1
    FOR CNT = 19 TO 38
        FINAL.REC<-1> = "Segment" : COL.COUNT
        COL.COUNT += 1
    NEXT CNT

    FINAL.REC<-1> = "Entered Debit Amount,Entered Credit Amount,Converted Debit Amount,Converted Credit Amount,Journal_Batch_Name"

    COL.COUNT = 1
    FOR CNT = 44 TO 66
        FINAL.REC<-1> =  "Interface" : COL.COUNT
        COL.COUNT += 1
    NEXT CNT

    FINAL.REC<-1> = "Interface Group Identifier"
    CHANGE @AM TO FILE.SEP IN FINAL.REC

***********
*   PROCESS
***********
    SEL.CMD = "SELECT ":F.ERP.GL.TAB
    CALL EB.READLIST(SEL.CMD, SEL.LIST, '', NO.OF.REC, RET.ERR)
    LOOP
        REMOVE REC.ID FROM SEL.LIST SETTING NXT.ID
    WHILE (REC.ID : NXT.ID) DO
        READ MOVEMENT.REC FROM F.ERP.GL.TAB, REC.ID THEN
            INTERMEDIATE.REC = ""
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.STATUS.CODE>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.LEDGER.ID>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.EFF.DATE>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.SOURCE>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.CATEGORY>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.CURRENCY>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.BOOK.DATE>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.ACTUAL.FLAG>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.COMPANY>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.COST.CENTER>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.BRANCH>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.ACCOUNT>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.LOB>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.INTR.COMPANY>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.SECTOR.ACTV>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.INST.SECTOR>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.FUTURE.1>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.FUTURE.2>

            FOR CNT = 19 TO 38
                INTERMEDIATE.REC<-1> = ""
            NEXT CNT

            IF MOVEMENT.REC<EXT.ACCOUNT> EQ '23227000' OR MOVEMENT.REC<EXT.ACCOUNT> EQ '48312100' THEN
                * Swapped Reporting Columns For Credit And Debit So Balance Can Net Off
                IF MOVEMENT.REC<EXT.RESERVED.20> LT 0 THEN
                    INTERMEDIATE.REC<-1> = ""
                    INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.RESERVED.20> * -1	;	*	Negative CCY Balance Reported Without Sign
                END ELSE
                    INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.RESERVED.20>	;	*	CCY Balance
                    INTERMEDIATE.REC<-1> = ""
                END
            END ELSE
                IF MOVEMENT.REC<EXT.RESERVED.20> LT 0 THEN
                    INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.RESERVED.20> * -1	;	*	Negative CCY Balance Reported Without Sign
                    INTERMEDIATE.REC<-1> = ""
                END ELSE
                    INTERMEDIATE.REC<-1> = ""
                    INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.RESERVED.20>	;	*	CCY Balance
                END
            END
            INTERMEDIATE.REC<-1> = ""
            INTERMEDIATE.REC<-1> = ""
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.JRNL.BATCH.NAME>

            FOR CNT = 44 TO 63
                INTERMEDIATE.REC<-1> =  ""
            NEXT CNT

            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.EXCH.USER>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.EXCH.RATE.DATE>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.EXCH.RATE>
            INTERMEDIATE.REC<-1> = MOVEMENT.REC<EXT.INTRFCE.GRP.ID>
        END
        CHANGE @AM TO FILE.SEP IN INTERMEDIATE.REC
        FINAL.REC<-1> = INTERMEDIATE.REC
    REPEAT

    WRITE FINAL.REC TO F.EXCHANGE, FILE.NAME ON ERROR
        STOP 'error writing into account detail to ':EXT.DIR
    END

    WRITE FINAL.REC TO F.BACKUP, FILE.NAME ON ERROR
        STOP 'error writing into account detail to ':F.BACKUP
    END

    RETURN
END