       IDENTIFICATION DIVISION.
       PROGRAM-ID. BAS110VU.
       AUTHOR.     PLANET, INC.
      ******************************************************************
      * EVALUATES VISA USA TRANSACTIONS FOR INTERCHANGE REIMBURSEMENT  *
      * QUALIFICATION.                                                 *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INVENTORY-LEVEL             PIC X(25)
                                      VALUE ' RELEASE RPQ VV.LL 02.09 '.02.09
       01  WS-COMPILE                  PIC X(20).
       01  WS-DISP-COMPILE             PIC X(8)BX(12).
       01  WORK-SPACE.
           05  BACONDEC                PIC X(08) VALUE 'BACONDEC'.
           05  DATECONV                PIC X(08) VALUE 'DATECONV'.
           05  BASCST01                PIC X(08) VALUE 'BASCST01'.      01.73
           05  BASCST02                PIC X(08) VALUE 'BASCST02'.      01.85
           05  BASVPP01                PIC X(08) VALUE 'BASVPP01'.      02.06
           05  ABEND                   PIC X(08) VALUE 'ILBOABN0'.
           05  ABEND-CODE              PIC S9(4) COMP VALUE +0.
           05  WS-FIRST-TIME-SW        PIC X VALUE 'Y'.
           05  WS-CHECK-QUAL-ONLY-SW   PIC X VALUE 'N'.
               88  WS-CHECK-QUAL-ONLY        VALUE 'Y'.
           05  WS-LOWER-RATE-SW        PIC X VALUE 'N'.
               88  WS-RATE-IS-LOWER          VALUE 'Y'.
           05  WS-MIN-MAX-FOUND-SW     PIC X VALUE 'N'.                 01.62
               88  WS-MIN-MAX-FOUND          VALUE 'Y'.                 01.62
           05  WS-PLAN-CODE-FOUND-SW   PIC X VALUE 'N'.
               88  WS-PLAN-CODE-FOUND        VALUE 'Y'.
           05  WS-CUSTOM-FOUND-SW      PIC X VALUE 'N'.                 01.74
               88  WS-CUSTOM-FOUND           VALUE 'A' THRU 'B'.        01.74
               88  WS-CUSTOM-ASSIGN          VALUE 'A'.                 01.74
               88  WS-CUSTOM-BYPASS          VALUE 'B'.                 01.74
               88  WS-NOT-CUSTOM-PLAN        VALUE 'N'.                 01.74
           05  WS-CUSTOM-PLAN-X.                                        01.74
               10  WS-CUSTOM-PLAN      PIC 9(04).                       01.74
           05  WS-MAX-USED-SW          PIC X VALUE 'N'.                 01.52
               88  WS-MAX-USED               VALUE 'Y'.                 01.52
           05  WS-MAX-BASE-FOUND-SW    PIC X VALUE 'N'.                 01.52
               88  WS-MAX-BASE-FOUND         VALUE 'Y'.                 01.52
           05  WS-ADD-PLAN-SW          PIC X VALUE 'N'.                 01.62
               88  WS-ADD-PLAN               VALUE 'Y'.                 01.62
           05  WS-PLAN-QUAL-CODE-X.
               10  WS-PLAN-QUAL-CODE   PIC 9(4).
           05  WS-MIN-MAX-PLAN-CODE-X.                                  01.62
               10  WS-MIN-MAX-PLAN-CODE                                 01.62
                                       PIC 9(4).                        01.62
           05  WS-PSUB                 PIC S9(8) COMP.                  01.48
           05  WS-SUB1                 PIC S9(4) COMP.
           05  WS-SUB-DG               PIC S9(4)V COMP.
           05  WS-ERROR-DG             PIC X(4).
           05  WS-PLAN-DG              PIC X(4).
           05  WS-END-SEARCH-PLAN      PIC X(04).                       01.82
           05  WS-SEARCH-PLAN-X.
               10  WS-SEARCH-PLAN      PIC X(04).
           05  WS-TRANS-ID             PIC 9(15).
           05  FILLER REDEFINES WS-TRANS-ID.
               10  FILLER              PIC X(02).
               10  WS-TRANS-ID-YDDD    PIC 9(04).
               10  FILLER              PIC X(09).
           05  WS-CPS-PSI              PIC X VALUE SPACE.
           05  WS-CPS-REIMB            PIC X VALUE SPACE.
           05  WS-CPS-REIMB-SW         PIC X VALUE SPACE.
               88  WS-IS-CPS-REIMB           VALUE 'Y'.
           05  WS-CPS-REGULATED-FLG    PIC X VALUE 'N'.                 01.64
               88  WS-IS-CPS-REGULATED       VALUE 'Y'.                 01.64
           05  WS-RTL-2-MRCH-SW        PIC X VALUE 'N'.                 01.64
               88  WS-IS-RTL-2-MRCH          VALUE 'Y'.                 01.64
           05  WS-CPS-CHARITY-FLG      PIC X VALUE 'N'.                 01.68
               88  WS-IS-CPS-CHARITY         VALUE 'Y'.                 01.68
           05  WS-CPS-SW               PIC X VALUE 'N'.
               88  WS-IS-CPS                 VALUE 'Y'.
           05  WS-PT-TSC-SW            PIC X VALUE 'N'.
               88  WS-IS-PT-TSC              VALUE 'Y'.
           05  WS-LRG-TKT-SW           PIC X VALUE ' '.
               88  WS-NOT-LRG-TKT            VALUE ' '.
           05  WS-LRG-TKT-ADV-SW       PIC X VALUE ' '.                 01.77
               88  WS-NOT-LRG-TKT-ADV        VALUE ' '.                 01.77
           05  WS-LRG-TKT-PURCH-SW     PIC X VALUE ' '.                 01.86
               88  WS-NOT-LRG-TKT-PURCH      VALUE ' '.                 01.86
           05  WS-QUASI-CASH-SW        PIC X VALUE 'N'.                 01.77
               88  WS-QUASI-CASH             VALUE 'Y'.                 01.77
           05  WS-GSA-LRG-TKT-SW       PIC X VALUE ' '.
               88  WS-NOT-GSA-LRG-TKT        VALUE ' '.
           05  WS-USE-UTILITY-RATE-SW  PIC X VALUE 'N'.
               88  WS-USE-UTILITY-RATE       VALUE 'Y'.
           05  WS-UTILITY-SW           PIC X VALUE 'N'.
               88  WS-IS-UTILITY             VALUE 'Y'.
           05  WS-CPS-REWARDS-SW       PIC X VALUE 'N'.
               88  WS-IS-CPS-REWARDS         VALUE 'Y'.
           05  WS-SIG-UTIL-SW          PIC X VALUE 'N'.
               88  WS-IS-SIG-UTIL            VALUE 'Y'.
           05  WS-SIG-PREF-CREDIT-SW   PIC X VALUE 'N'.                 01.55
               88  WS-IS-SIG-PREF-CREDIT     VALUE 'Y'.                 01.55
           05  WS-INFINITE-SW          PIC X VALUE ' '.                 01.98
               88  WS-IS-INFINITE-QUAL       VALUE 'Q'.                 01.98
               88  WS-IS-INFINITE-NONQUAL    VALUE 'N'.                 01.98
               88  WS-IS-NOT-INFINITE        VALUE ' '.                 01.98
           05  WS-SIG-PREF-FUEL-SW     PIC X VALUE 'N'.                 01.54
               88  WS-IS-SIG-PREF-FUEL       VALUE 'Y'.                 01.54
           05  WS-INFINITE-FUEL-SW     PIC X VALUE 'N'.                 01.98
               88  WS-IS-INFINITE-FUEL       VALUE 'Y'.                 01.98
           05  WS-DEBT-REPAY-SW        PIC X VALUE 'N'.                 01.48
               88  WS-IS-DEBT-REPAY          VALUE 'Y'.                 01.48
           05  WS-GOV-HE-PAY-SW        PIC X VALUE 'N'.                 01.86
               88  WS-IS-GOV-HE-PAY          VALUE 'Y'.                 01.86
           05  WS-USE-GOV-HE-PAY-RATE-SW PIC X VALUE 'N'.               01.86
               88  WS-USE-GOV-HE-PAY-RATE    VALUE 'Y'.                 01.86
           05  WS-USE-COMM-ELECT-ONLY-SW PIC X VALUE 'N'.               01.88B
               88  WS-USE-COMM-ELECT-ONLY    VALUE 'Y'.                 01.88B
           05  WS-USD-SML-TKT-AMT      PIC S9(12)V999 COMP-3
                                       VALUE +15.000.
           05  WS-USD-AFD-MAX-AMT      PIC S9(12)V999 COMP-3
                                       VALUE +125.000.                  01.68
           05  WS-LOW-RATE-AMT         PIC S9(12)V999 COMP-3.
           05  WS-LOW-ITEM-AMT         PIC S9(06)V999 COMP-3.
           05  WS-LOW-MRCH-AMT         PIC S9(12)V999 COMP-3.
           05  WS-STANDARD-AMT         PIC S9(12)V999 COMP-3.
           05  WS-WORK-CALC-AMT        PIC S9(12)V99999 COMP-3.
           05  WS-WORK-MAX-AMT         PIC S9(12)V99999 COMP-3.         01.62
           05  WS-WORK-MAX-ALL-9S      PIC S9(12)V99999 COMP-3          01.62
                                       VALUE +999999999999.99999.       01.62
           05  WS-WORK-AMT             PIC S9(12)V999 COMP-3.
           05  WS-DISPLAY-AMT          PIC ZZZ,ZZZ,ZZZ,ZZ9.999-.        01.74
           05  WS-DISPLAY-TRAN-SW      PIC X VALUE 'N'.                 01.74
               88  WS-DISPLAY-TRAN           VALUE 'Y'.                 01.74
      *
           05  WS-CPS-CONTROLS.
             07  WS-CPS-RWD-1-CONTROLS.
               10  WS-CPS-AUTO-FUEL-CONTROLS.
                 12  WS-CPS-AUTO-FUEL-FLG
                                       PIC X(01).
                 12  WS-CPS-AUTO-FUEL-RMB
                                       PIC X(01).
                 12  WS-CPS-AUTO-FUEL-PSI
                                       PIC X(01).
               10  WS-CPS-RTL-SST-CONTROLS.
                 12  WS-CPS-RTL-SST-FLG
                                       PIC X(01).
                 12  WS-CPS-RTL-SST-RMB
                                       PIC X(01).
                 12  WS-CPS-RTL-SST-PSI
                                       PIC X(01).
               10  WS-CPS-RTL-CONTROLS.
                 12  WS-CPS-RTL-FLG    PIC X(01).
                 12  WS-CPS-RTL-RMB    PIC X(01).
                 12  WS-CPS-RTL-PSI    PIC X(01).
               10  WS-CPS-SPR-MKT-CONTROLS.
                 12  WS-CPS-SPR-MKT-FLG
                                       PIC X(01).
                 12  WS-CPS-SPR-MKT-RMB
                                       PIC X(01).
                 12  WS-CPS-SPR-MKT-PSI
                                       PIC X(01).
             07  WS-CPS-RWD-2-CONTROLS.
               10  WS-CPS-RTL-RST-CONTROLS.
                 12  WS-CPS-RTL-RST-FLG
                                       PIC X(01).
                 12  WS-CPS-RTL-RST-RMB
                                       PIC X(01).
                 12  WS-CPS-RTL-RST-PSI
                                       PIC X(01).
               10  WS-CPS-RTL-CNP-CONTROLS.
                 12  WS-CPS-RTL-CNP-FLG
                                       PIC X(01).
                 12  WS-CPS-RTL-CNP-RMB
                                       PIC X(01).
                 12  WS-CPS-RTL-CNP-PSI
                                       PIC X(01).
               10  WS-CPS-ECOM-BASIC-CONTROLS.
                 12  WS-CPS-ECOM-BASIC-FLG
                                       PIC X(01).
                 12  WS-CPS-ECOM-BASIC-RMB
                                       PIC X(01).
                 12  WS-CPS-ECOM-BASIC-PSI
                                       PIC X(01).
               10  WS-CPS-ECOM-PREF-HC-CONTROLS.
                 12  WS-CPS-ECOM-PREF-HC-FLG
                                       PIC X(01).
                 12  WS-CPS-ECOM-PREF-HC-RMB
                                       PIC X(01).
                 12  WS-CPS-ECOM-PREF-HC-PSI
                                       PIC X(01).
               10  WS-CPS-ECOM-PREF-PT-CONTROLS.
                 12  WS-CPS-ECOM-PREF-PT-FLG
                                       PIC X(01).
                 12  WS-CPS-ECOM-PREF-PT-RMB
                                       PIC X(01).
                 12  WS-CPS-ECOM-PREF-PT-PSI
                                       PIC X(01).
               10  WS-CPS-HC-CP-CONTROLS.
                 12  WS-CPS-HC-CP-FLG  PIC X(01).
                 12  WS-CPS-HC-CP-RMB  PIC X(01).
                 12  WS-CPS-HC-CP-PSI  PIC X(01).
               10  WS-CPS-HC-CNP-CONTROLS.
                 12  WS-CPS-HC-CNP-FLG PIC X(01).
                 12  WS-CPS-HC-CNP-RMB PIC X(01).
                 12  WS-CPS-HC-CNP-PSI PIC X(01).
               10  WS-CPS-PT-CONTROLS.
                 12  WS-CPS-PT-CP-FLG  PIC X(01).                       01.77
                 12  WS-CPS-PT-CNP-FLG PIC X(01).                       01.77
                 12  WS-CPS-PT-RMB     PIC X(01).
                 12  WS-CPS-PT-PSI     PIC X(01).
               10  WS-CPS-RTL-KEY-CONTROLS.
                 12  WS-CPS-RTL-KEY-FLG
                                       PIC X(01).
                 12  WS-CPS-RTL-KEY-RMB
                                       PIC X(01).
                 12  WS-CPS-RTL-KEY-PSI
                                       PIC X(01).
             07  WS-CPS-OTHER-CONTROLS.
               10  WS-CPS-ECOM-PREF-CONTROLS.
                 12  WS-CPS-ECOM-PREF-FLG
                                       PIC X(01).
                 12  WS-CPS-ECOM-PREF-RMB
                                       PIC X(01).
                 12  WS-CPS-ECOM-PREF-PSI
                                       PIC X(01).
               10  WS-CPS-ACCT-FUND-CONTROLS.
                 12  WS-CPS-ACCT-FUND-FLG
                                       PIC X(01).
                 12  WS-CPS-ACCT-FUND-RMB
                                       PIC X(01).
                 12  WS-CPS-ACCT-FUND-PSI
                                       PIC X(01).
               10  WS-CPS-RTL-2-CONTROLS.
                 12  WS-CPS-RTL-2-FLG  PIC X(01).
                 12  WS-CPS-RTL-2-RMB  PIC X(01).
                 12  WS-CPS-RTL-2-PSI  PIC X(01).
               10  WS-CPS-SML-TKT-CONTROLS.
                 12  WS-CPS-SML-TKT-FLG
                                       PIC X(01).
                 12  WS-CPS-SML-TKT-RMB
                                       PIC X(01).
                 12  WS-CPS-SML-TKT-PSI
                                       PIC X(01).
               10  WS-CPS-RECUR-PAY-CONTROLS.                           02.07
                 12  WS-CPS-RECUR-PAY-FLG                               02.07
                                       PIC X(01).                       02.07
                 12  WS-CPS-RECUR-PAY-RMB                               02.07
                                       PIC X(01).                       02.07
                 12  WS-CPS-RECUR-PAY-PSI                               02.07
                                       PIC X(01).                       02.07
           01  VISA-CPS-REIMB-SWITCH.                                   02.06
               05  VISA-CPS-REIMB-SW   PIC X(01).                       02.06
      *
      ****************************************************************
      *    DATECONV ROUTINE FIELDS
      ****************************************************************
           COPY DCPASS.
      *
      ****************************************************************
      *    BACONDEC ROUTINE FIELDS
      ****************************************************************
           COPY KMCONDLN.
      *
      ****************************************************************  01.73
      *    BASCST01 ROUTINE FIELDS                                      01.73
      ****************************************************************  01.73
           COPY KMCCST01.                                               01.73
      *                                                                 01.73
      ****************************************************************  01.85
      *    BASCST02 ROUTINE FIELDS                                      01.85
      ****************************************************************  01.85
           COPY KMCCST02.                                               01.85
      *                                                                 01.85
      ****************************************************************  02.06
      *    BASVPP01 ROUTINE FIELDS                                      02.06
      ****************************************************************  02.06
           COPY KMCVPP01.                                               02.06
      *                                                                 02.06
       LINKAGE SECTION.
      *
           COPY KMCINTEP.
           COPY KMCPKGPA.                                               01.48
           COPY KMCVSDAT.
           COPY KMCPETRC.
           COPY KMC110VU.                                               01.58
      *
       PROCEDURE DIVISION USING KMCINTEP-DATA-AREA
                                PACKAGE-PASS-AREA                       01.48
                                VISA-STALE-DATE-RECORD
                                POST-EDIT-TRANS                         01.58
                                VU-PLAN-DATA.                           01.58
      *
       A-100-MAINLINE.
      *
           PERFORM B-100-INITIALIZATION
              THRU B-100-EXIT.
           PERFORM C-100-PROCESS
              THRU C-100-EXIT.
      *
           IF VU-RATE-IDX > +1                                          01.57
              IF VU-RTE-CPS(VU-RATE-IDX) = 'Y'                          01.57
                 GO TO A-100-RETURN.
      *
           MOVE SPACE                    TO PET-PAY-SVC-IND.
      *
       A-100-RETURN.
           GOBACK.
      *
       B-100-INITIALIZATION.
      *
           IF WS-FIRST-TIME-SW = 'Y'
              MOVE 'N'                   TO WS-FIRST-TIME-SW
              MOVE WHEN-COMPILED         TO WS-COMPILE
              MOVE WS-COMPILE            TO WS-DISP-COMPILE
              DISPLAY ' BAS110VU  ' WS-DISP-COMPILE '  '
                      INVENTORY-LEVEL
              MOVE 'I'                   TO BASCST01-CALL-TYPE          01.73
              MOVE 'BAS110VU'            TO BASCST01-SD-CALLING-PGM     01.73
              MOVE 'VS'                  TO BASCST01-SD-CARD-TYPE       01.73
              PERFORM Z-980-CALL-BASCST01                               01.73
                 THRU Z-980-EXIT                                        01.73
              MOVE 'I'                   TO BASCST02-CALL-TYPE          01.85
              MOVE 'BAS110VU'            TO BASCST02-SD-CALLING-PGM     01.85
              MOVE 'V'                   TO BASCST02-SD-CARD-TYPE       01.85
              PERFORM Z-981-CALL-BASCST02                               01.85
                 THRU Z-981-EXIT                                        01.85
              MOVE 'P'                   TO BASCST02-CALL-TYPE          01.85
              MOVE 'I'                   TO BASVPP01-CALL-TYPE          02.06
              MOVE 'BAS110VU'            TO BASVPP01-SD-CALLING-PGM     02.06
              PERFORM Z-982-CALL-BASVPP01                               02.06
                 THRU Z-982-EXIT                                        02.06
           END-IF.
      *                                                                 02.02
           MOVE INVENTORY-LEVEL(20:6) TO KDA-QUAL-PGM-VERSION.          02.02
           MOVE 'VU   '               TO KDA-QUAL-PGM.                  02.02
      *
           MOVE SPACES                   TO WS-CPS-CONTROLS.
           MOVE SPACE                    TO WS-CPS-REIMB.
           MOVE SPACE                    TO WS-CPS-PSI
                                            WS-LRG-TKT-SW
                                            WS-LRG-TKT-ADV-SW           01.77
                                            WS-GSA-LRG-TKT-SW           01.86
                                            WS-LRG-TKT-PURCH-SW         01.86
                                            KDA-VS-EIRF-TO-STD-ERR.     02.09
           MOVE 'N'                      TO WS-CPS-REIMB-SW             01.89
                                            WS-QUASI-CASH-SW            01.77
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-CHARITY-FLG          01.68
                                            WS-USE-UTILITY-RATE-SW
                                            WS-USE-GOV-HE-PAY-RATE-SW   01.86
                                            WS-CPS-REWARDS-SW
                                            WS-SIG-PREF-FUEL-SW         01.54
                                            WS-INFINITE-FUEL-SW         01.98
                                            WS-SIG-UTIL-SW              01.88B
                                            WS-USE-COMM-ELECT-ONLY-SW.  01.88B
           IF KDA-MRCH-TYP-UTILITY AND                                  01.61
             (PET-VISA-MVV NOT = SPACES)                                01.61
                 MOVE 'Y'                TO WS-UTILITY-SW               01.61
           ELSE                                                         01.61
              MOVE 'N'                   TO WS-UTILITY-SW               01.61
           END-IF.                                                      01.61
           IF KDA-CHD-AR-CREDIT AND                                     01.81
              KDA-VS-PID-IS-SIG-PREF                                    01.84
                 MOVE 'Y'                TO WS-SIG-PREF-CREDIT-SW       01.55
           ELSE                                                         01.55
              MOVE 'N'                   TO WS-SIG-PREF-CREDIT-SW       01.55
           END-IF.                                                      01.55
           IF KDA-CHD-AR-CREDIT AND                                     01.98
              KDA-VS-PID-IS-INFINITE                                    01.98
              IF PET-VS-SPEND-QUAL-IND = 'Q'                            01.98
                 SET WS-IS-INFINITE-QUAL TO TRUE                        01.98
              ELSE                                                      01.98
              IF PET-VS-SPEND-QUAL-IND = 'N' OR 'B'                     02.07
                 SET WS-IS-INFINITE-NONQUAL TO TRUE                     01.98
              ELSE                                                      01.98
                 SET WS-IS-NOT-INFINITE TO TRUE                         01.98
              END-IF                                                    01.98
              END-IF                                                    01.98
           ELSE                                                         01.98
              SET WS-IS-NOT-INFINITE TO TRUE                            01.98
           END-IF.                                                      01.98
           SET VU-MAX-MRCH-IDX TO +1.                                   01.57
           SET VU-MAX-RATE-IDX TO +1.                                   01.57
           SET VU-MRCH-IDX TO +1.                                       01.57
           SET VU-RATE-IDX TO +1.                                       01.57
           MOVE 'N'                      TO WS-ADD-PLAN-SW.             01.62
           MOVE +999999999999.999        TO WS-LOW-RATE-AMT
                                            WS-LOW-MRCH-AMT.
           MOVE +0                       TO WS-STANDARD-AMT
                                            WS-LOW-ITEM-AMT.
           MOVE +1                       TO WS-SUB-DG.
      *
           IF (KDA-MERCAT-PT-AND-TSC OR
               KDA-MRCH-TYP-FAST-FOOD OR
               KDA-MRCH-TYP-PSNGR-RAIL OR
               KDA-MRCH-TYP-AGENTS)
                 MOVE 'Y'                TO WS-PT-TSC-SW
           ELSE
              MOVE 'N'                   TO WS-PT-TSC-SW.
      *
           IF KDA-MRCH-TYP-RETAIL-2 OR                                  01.64
              KDA-MRCH-TYP-RTL-2-REG                                    01.64
                 MOVE 'Y'                TO WS-RTL-2-MRCH-SW            01.64
           ELSE                                                         01.64
              MOVE 'N'                   TO WS-RTL-2-MRCH-SW            01.64
           END-IF.                                                      01.64
      *                                                                 01.64
           IF KDA-MRCH-TYP-DEBT-REPAY AND                               01.48
              KDA-VC-SPEC-COND-IND-2-DEBT                               01.48
                 MOVE 'Y'                TO WS-DEBT-REPAY-SW            01.48
           ELSE                                                         01.48
              MOVE 'N'                   TO WS-DEBT-REPAY-SW            01.48
           END-IF.                                                      01.48
      *
           IF KDA-MRCH-TYP-GOV-HIGHER-ED AND                            01.86
             (PET-VISA-MVV NOT = SPACES) AND                            01.48
             (NOT KDA-VS-PID-IS-COMMERCIAL AND                          02.07
             (KDA-CHD-AR-IS-FND-SRC-DEBIT OR                            01.84
              KDA-CHD-AR-IS-FND-SRC-PREPAID))                           01.84
                 MOVE 'Y'                TO WS-GOV-HE-PAY-SW            01.86
           ELSE                                                         01.48
              MOVE 'N'                   TO WS-GOV-HE-PAY-SW            01.86
           END-IF.                                                      01.48
      *
           IF KDA-VC-SPEC-COND-IND-2-QUASI AND                          01.83
             (NOT KDA-VC-NOT-AC-PAY-SVC)                                01.83
              IF KDA-VS-PID-IS-COMMERCIAL                               01.83
                 IF (KDA-VS-PID-IS-BUSINESS AND                         01.86
                     KDA-CHD-AR-IS-FND-SRC-DEBIT) OR                    01.86
                     KDA-CHD-AR-IS-FND-SRC-PREPAID                      01.86
                       SET WS-QUASI-CASH TO TRUE                        01.83
                  END-IF                                                01.83
              ELSE                                                      01.83
                 IF KDA-CHD-AR-IS-FND-SRC-DEBIT                         01.83
                    SET WS-QUASI-CASH    TO TRUE                        01.83
                 END-IF                                                 01.83
              END-IF                                                    01.83
           END-IF.                                                      01.77
      *                                                                 01.77
       B-100-EXIT.
           EXIT.
      *
       C-100-PROCESS.
      *
      * SET PRIVATE LABEL RATE.                                         01.93
      *                                                                 01.93
           IF KDA-VS-PID-IS-PVT-BASIC OR                                01.93
              KDA-VS-PID-IS-PVT-STANDARD OR                             01.93
              KDA-VS-PID-IS-PVT-ENHANCED OR                             01.93
              KDA-VS-PID-IS-PVT-SPECIAL OR                              01.93
              KDA-VS-PID-IS-PVT-PREMIUM                                 01.93
              PERFORM I-830-SET-PRIVATE-LABEL                           01.93
                 THRU I-830-EXIT                                        01.93
              GO TO C-100-RATES-CHECKED.                                01.93
      *
      * ALL CARDS AT A CASH MERCHANT GET CASH RATE.
      *
           IF KDA-MRCH-TYP-CASH
              PERFORM I-050-SET-CASH
                 THRU I-050-EXIT
              GO TO C-100-RATES-CHECKED.
      *
      * ALL CARDS AT AN ATM MERCHANT GET ATM RATE.
      *
           IF KDA-MRCH-TYP-ATM
              PERFORM I-100-SET-ATM
                 THRU I-100-EXIT
              GO TO C-100-RATES-CHECKED.
      *
      * VALIDATE CPS DATA
      *
           PERFORM S-590-CHECK-CPS-COMMON-DATA
              THRU S-590-EXIT.
      *
      * ALL RETURNS GET RETURN RATES ONLY.
      *
           IF PET-TRANS-PREF = '07' OR '13'
              PERFORM I-950-SET-RETURN
                 THRU I-950-EXIT
              GO TO C-100-RATES-CHECKED.
      *
      * SET USA STANDARD RATE FOR ALL ACCOUNT TRANSACTIONS.             01.74
      *                                                                 01.74
           PERFORM I-200-SET-STANDARD                                   01.74
              THRU I-200-EXIT.                                          01.74
      *                                                                 01.74
       C-100-CHECK-HIGH-RISK.                                           01.60
      *
      * INDICATES A VISA-DETERMINED HIGH RISK SET OF CATEGORY CODES THAT
      * WILL HAVE ALL TRANSACTIONS SUBMITTED USING THE APPLICABLE
      * STANDARD RATE.
      *
           IF KDA-MRCH-TYP-HIGH-RSK-TELE
              GO TO C-100-RATES-CHECKED.
      *
      * INDICATES A VISA NON-SECURE TRANSACTION THAT WILL HAVE ALL
      * TRANSACTIONS SUBMITTED USING THE APPLICABLE STANDARD RATE.
      *
           IF KDA-VC-PHONE-ORD-8
              GO TO C-100-RATES-CHECKED.
      *
      * SET US CPS RATE FOR ALL ACCOUNT TRANSACTIONS.
      *
           PERFORM I-400-SET-CPS
              THRU I-400-EXIT.
           IF WS-CPS-SML-TKT-FLG = 'Y' AND                              01.65
              KDA-CHD-AR-IS-REGULATED AND                               01.65
              WS-IS-CPS-REGULATED                                       01.65
                 GO TO C-100-RATES-CHECKED.                             01.65
           IF KDA-US-AIR
              GO TO C-100-RATES-CHECKED.
      *
      * SET US CPS RETAIL 2
      *
           PERFORM I-600-SET-CPS-RETAIL-2
              THRU I-600-EXIT.
      *
      * SET US CPS CHARITY                                              01.68
      *                                                                 01.68
           PERFORM I-605-SET-CPS-CHARITY                                01.68
              THRU I-605-EXIT.                                          01.68
      *                                                                 01.68
      * SET US CPS REWARDS
      *
           PERFORM I-700-SET-CPS-REWARDS
              THRU I-700-EXIT.
      *                                                                 01.48
      * SET US CPS DEBT REPAY                                           01.48
      *                                                                 01.48
           PERFORM I-705-SET-CPS-DEBT-REPAY                             01.48
              THRU I-705-EXIT.                                          01.48
      *                                                                 01.48
      * SET US CPS GOV & HIGHER EDUCATION                               01.86
      *                                                                 01.48
           PERFORM I-710-SET-CPS-GOV-HE-PAY                             01.86
              THRU I-710-EXIT.                                          01.48
      *
      * SET US CPS UTILITY RATE.
      *
           PERFORM I-805-SET-UTILITY
              THRU I-805-EXIT.
      *                                                                 01.64
           IF WS-IS-CPS-REGULATED AND                                   01.64
              KDA-CHD-AR-IS-REGULATED AND                               01.64
              WS-IS-CPS                                                 01.64
                 MOVE 'Y'                   TO VU-2021-CPS              01.64
                                               VU-2521-CPS              01.64
                                               VU-2531-CPS              01.64
                                               VU-2532-CPS              01.64
                 MOVE WS-CPS-PSI            TO VU-2021-PSI              01.64
                                               VU-2521-PSI              01.64
                                               VU-2531-PSI              01.64
                                               VU-2532-PSI              01.64
                 MOVE WS-CPS-REIMB          TO VU-2021-RMB              01.64
                                               VU-2521-RMB              01.64
                                               VU-2531-RMB              01.64
                                               VU-2532-RMB              01.64
                 GO TO C-100-RATES-CHECKED                              01.64
           END-IF.                                                      01.64
      *
      * SET US ELECTRONIC RATE FOR ALL ACCOUNT TRANSACTIONS.
      *
           PERFORM I-300-SET-ELECTRONIC
              THRU I-300-EXIT.
      *
           PERFORM I-715-SET-VPP                                        02.06
              THRU I-715-EXIT.                                          02.06
      *                                                                 02.06
       C-100-CHECK-LARGE-TICKET.
      *
      * SET US GSA LARGE TICKET RATE.
      *
           PERFORM I-810-SET-GSA-LARGE-TICKET
              THRU I-810-EXIT.
      *
      * SET US PURCHASING LARGE TICKET RATE.
      *
           PERFORM I-820-SET-LARGE-TICKET
              THRU I-820-EXIT.
      *
       C-100-RATES-CHECKED.
      *
      * FIND LOWEST RATE THAT THE TRANSACTION QUALIFIED FOR AND
      * THE LOWEST RATE THAT THE MERCHANT IS SET UP TO RECEIVE.
      *
           IF VU-RATE-IDX = +1 AND                                      01.57
              VU-MRCH-IDX = +1                                          01.57
                 GO TO C-100-EXIT.
      *
           PERFORM S-800-CHECK-KEY-PACKAGE                              01.73
              THRU S-800-EXIT.                                          01.73
           IF BASCST01-RC-NORMAL                                        01.73
              MOVE BASCST01-RD-PLAN-X    TO KDA-PLAN-QUAL-CODE-X        01.73
              PERFORM C-150-SET-ORIG-PLAN-DATA                          01.73
                 THRU C-150-EXIT                                        01.73
              GO TO C-100-EXIT                                          01.73
           END-IF.                                                      01.73
      *                                                                 01.73
           IF WS-ADD-PLAN                                               01.62
              SET VU-MRCH-IDX            TO VU-RATE-IDX                 01.62
              MOVE 'X'                   TO PET-RATE-FLAG               01.62
           END-IF.                                                      01.62
      *
           IF VU-MRCH-IDX = +1                                          01.57
              SET VU-IDX                 TO +1                          01.57
              MOVE VU-RTE-PLN(VU-IDX)    TO WS-PLAN-QUAL-CODE           01.57
              MOVE WS-STANDARD-AMT       TO WS-LOW-MRCH-AMT
              PERFORM S-515-SEARCH-PACKAGE-PLAN
                 THRU S-515-EXIT
              IF NOT WS-PLAN-CODE-FOUND
                 GO TO C-100-CHECK-CUSTOM-PLAN                          01.85
              END-IF                                                    01.85
           END-IF.                                                      01.85
      *
           PERFORM C-150-SET-ORIG-PLAN-DATA                             01.73
              THRU C-150-EXIT.                                          01.73
      *                                                                 01.73
           IF WS-LOW-MRCH-AMT < KDA-LOW-MRCH-AMT
              MOVE WS-LOW-MRCH-AMT       TO KDA-LOW-MRCH-AMT
              IF VU-RTE-PKG(VU-MRCH-IDX) = ZEROS                        01.57
                 IF VU-MRCH-IDX = VU-MAX-MRCH-IDX                       01.57
                    MOVE VU-RTE-MXP(VU-MRCH-IDX)                        01.57
                                         TO KDA-PLAN-QUAL-CODE          01.52
                 ELSE                                                   01.52
                    MOVE VU-RTE-PLN(VU-MRCH-IDX)                        01.57
                                         TO KDA-PLAN-QUAL-CODE          01.52
                 END-IF                                                 01.52
              ELSE
                 MOVE VU-RTE-PKG(VU-MRCH-IDX)                           01.57
                                         TO KDA-PLAN-QUAL-CODE          01.62
              END-IF                                                    01.62
           END-IF.                                                      01.62
      *
           CALL BACONDEC USING  PET-ORIG-BANK-KEY                       01.62
                       C-TYPED  C-DECI02                                01.62
                       C-TYPER  KDA-LOW-MRCH-AMT                        01.62
                                KDA-LOW-RATE-AMT                        01.62
                                KDA-LOW-ITEM-AMT                        01.62
                       C-TYPEEND.                                       01.62
      *                                                                 01.62
       C-100-CHECK-CUSTOM-PLAN.                                         01.85
      *                                                                 01.85
           PERFORM Z-981-CALL-BASCST02                                  01.85
              THRU Z-981-EXIT.                                          01.85
      *                                                                 01.85
       C-100-EXIT.
           EXIT.
      *
       C-150-SET-ORIG-PLAN-DATA.                                        01.73
      *                                                                 01.73
           MOVE WS-LOW-RATE-AMT          TO KDA-LOW-RATE-AMT.           01.73
           MOVE WS-LOW-ITEM-AMT          TO KDA-LOW-ITEM-AMT.           01.73
           IF VU-RATE-IDX = VU-MAX-RATE-IDX                             01.73
              MOVE VU-RTE-MXP(VU-RATE-IDX)                              01.73
                                         TO KDA-ORIG-PLAN-CODE          01.73
           ELSE                                                         01.73
              MOVE VU-RTE-PLN(VU-RATE-IDX)                              01.73
                                         TO KDA-ORIG-PLAN-CODE          01.73
           END-IF.                                                      01.73
           MOVE VU-RTE-RMB(VU-RATE-IDX)  TO PET-REIMB-IND.              01.73
           MOVE VU-RTE-PGM(VU-RATE-IDX)  TO PET-FEE-PROGRAM-IND.        01.73
           IF VU-RTE-CPS(VU-RATE-IDX) = 'Y'                             01.73
              MOVE VU-RTE-PSI(VU-RATE-IDX)                              01.73
                                         TO PET-PAY-SVC-IND             01.73
              MOVE KDA-AUTH-CHAR-IND     TO PET-AUTH-CHAR-IND           01.73
           END-IF.                                                      01.73
      *                                                                 01.73
       C-150-EXIT.                                                      01.73
           EXIT.                                                        01.73
      *                                                                 01.73
       D-100-CALL-DATECONV.
      *****************************************************************
      *  THIS SECTION IS PERFORMED FROM SEVERAL DIFFERENT AREAS W/IN **
      *  BAS110VU TO CONVERT DATES.  THE CALLING AREAS ARE RESPONSIBLE*
      *  FOR SETTING UP THE WORK AREA (DATECONV-PASSAREA) BEFORE     **
      *  PERFORMING THIS SECTION FOR THE CALL.  EACH CALLING SECTION **
      *  WITHIN THIS PROGRAM ALSO HANDLES THE CHECKING OF ERROR CODES**
      *  AS EACH IS UNIQUE.                                          **
      *****************************************************************
      *
           CALL DATECONV USING DATECONV-PASSAREA.
      *
       D-100-EXIT.
           EXIT.
      *
       I-050-SET-CASH.
      *
      * SET RATE TABLE VALUES FOR ALL USA CASH TRANSACTIONS
      *
      *    0201 VISA USA STANDARD CASH RATE
      *
           MOVE VU-0201-PLN              TO WS-PLAN-QUAL-CODE-X.        01.57
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
           MOVE WS-LOW-RATE-AMT          TO WS-STANDARD-AMT.
      *
       I-050-EXIT.
           EXIT.
      *
       I-100-SET-ATM.
      *
      * SET RATE TABLE VALUES FOR ALL USA ATM TRANSACTIONS
      *
      *    0301 VISA USA STANDARD ATM RATE
      *
           MOVE VU-0301-PLN              TO WS-PLAN-QUAL-CODE-X.        01.57
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
           MOVE WS-LOW-RATE-AMT          TO WS-STANDARD-AMT.
      *
       I-100-EXIT.
           EXIT.
      *
       I-200-SET-STANDARD.
      *
      * SET RATE TABLE VALUES FOR USA STANDARD
      *
      *    0601 VISA USA STANDARD - REWARDS
      *    0621 VISA USA STANDARD - PURCHASING                          01.59
      *    0632 VISA USA STANDARD - PURCHASING PREPAID                  01.86
      *    0721 VISA USA STANDARD - CORPORATE                           01.59
      *    0732 VISA USA STANDARD - CORPORATE PREPAID                   01.86
      *    0901 VISA USA STANDARD - CONSUMER
      *    0921 VISA USA STANDARD - BUSINESS                            01.59
      *    4221 VISA USA STANDARD - BUSINESS DEBIT                      01.77
      *    3632 VISA USA STANDARD - BUSINESS PREPAID                    01.86
      *    0941 VISA USA STANDARD - SIGNATURE PREFERRED
      *    0931 VISA USA STANDARD - DEBIT
      *    0932 VISA USA STANDARD - PREPAID                             01.64
      *    2021 VISA USA STANDARD - US REGULATED COMMERCIAL DEBIT       01.64
      *    2521 VISA USA STANDARD - US REGULATED BUSINESS DEBIT         01.64
      *    2531 VISA USA STANDARD - US REGULATED DEBIT                  01.64
      *    2532 VISA USA STANDARD - US REGULATED PREPAID                01.64
      *    3040 VISA USA STANDARD - BUSINESS ENHANCED                   01.68
      *    4040 VISA USA STANDARD - BUSINESS SIGNATURE                  01.68
      *    3141 VISA USA STANDARD - SIGNATURE PREFERRED FUEL            01.84
      *    3241 VISA USA STANDARD - SIGNATURE PREFERRED FUEL MAX        01.84
      *    7133 VISA USA STANDARD - INFINITE                            01.98
      *    7331 VISA USA STANDARD - INFINITE FUEL                       01.98
      *    7231 VISA USA STANDARD - INFINITE FUEL MAX                   01.98
      *
           IF KDA-CHD-AR-IS-REGULATED                                   01.64
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 IF KDA-VS-PID-IS-COMMERCIAL                            01.84
                    MOVE VU-2021-PLN     TO WS-PLAN-QUAL-CODE-X         01.64
                 ELSE                                                   01.64
                    MOVE VU-2532-PLN     TO WS-PLAN-QUAL-CODE-X         01.64
                 END-IF                                                 01.64
              ELSE                                                      01.64
                 IF KDA-VS-PID-IS-BUSINESS                              01.84
                    MOVE VU-2521-PLN     TO WS-PLAN-QUAL-CODE-X         01.64
                 ELSE                                                   01.64
                    MOVE VU-2531-PLN     TO WS-PLAN-QUAL-CODE-X         01.64
                 END-IF                                                 01.64
              END-IF                                                    01.64
              MOVE 'N'                      TO VU-2021-CPS              01.64
                                               VU-2521-CPS              01.64
                                               VU-2531-CPS              01.64
                                               VU-2532-CPS              01.64
              MOVE SPACE                    TO WS-CPS-PSI               01.64
                                               VU-2021-PSI              01.64
                                               VU-2521-PSI              01.64
                                               VU-2531-PSI              01.64
                                               VU-2532-PSI              01.64
              MOVE '0'                      TO WS-CPS-REIMB             01.64
                                               VU-2021-RMB              01.64
                                               VU-2521-RMB              01.64
                                               VU-2531-RMB              01.64
                                               VU-2532-RMB              01.64
              GO TO I-200-PLAN-SET                                      01.64
           END-IF.                                                      01.64
      *                                                                 01.64
           IF KDA-VS-PID-IS-BUSINESS                                    01.84
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.86
                 MOVE VU-3632-PLN        TO WS-PLAN-QUAL-CODE-X         01.86
              ELSE                                                      01.86
              IF KDA-VS-PID-IS-BUS-ENH                                  01.84
                 MOVE VU-3040-PLN        TO WS-PLAN-QUAL-CODE-X         01.79
              ELSE                                                      01.68
                 IF KDA-VS-PID-IS-BUS-SIG                               01.84
                    MOVE VU-4040-PLN     TO WS-PLAN-QUAL-CODE-X         01.75
                 ELSE                                                   01.68
                    IF KDA-CHD-AR-IS-FND-SRC-DEBIT                      01.84
                       MOVE VU-4221-PLN  TO WS-PLAN-QUAL-CODE-X         01.77
                    ELSE                                                01.77
                       MOVE VU-0921-PLN  TO WS-PLAN-QUAL-CODE-X         01.77
                    END-IF                                              01.77
                 END-IF                                                 01.77
              END-IF                                                    01.68
              END-IF                                                    01.86
              GO TO I-200-PLAN-SET                                      01.68
           END-IF.                                                      01.68
      *                                                                 01.68
           IF KDA-VS-PID-IS-CORPORATE                                   01.84
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.86
                 MOVE VU-0732-PLN        TO WS-PLAN-QUAL-CODE-X         01.86
              ELSE                                                      01.86
                 MOVE VU-0721-PLN        TO WS-PLAN-QUAL-CODE-X         01.86
              END-IF                                                    01.86
           ELSE                                                         01.59
           IF KDA-VS-PID-IS-PURCHASE                                    01.84
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.86
                 MOVE VU-0632-PLN        TO WS-PLAN-QUAL-CODE-X         01.86
              ELSE                                                      01.86
                 MOVE VU-0621-PLN        TO WS-PLAN-QUAL-CODE-X         01.86
              END-IF                                                    01.86
           ELSE
           IF WS-IS-SIG-PREF-CREDIT                                     01.55
              IF KDA-MRCH-TYP-FUEL                                      01.84
                 MOVE VU-3141-PLN        TO WS-PLAN-QUAL-CODE-X         01.84
              ELSE                                                      01.84
                 MOVE VU-0941-PLN        TO WS-PLAN-QUAL-CODE-X         01.84
              END-IF                                                    01.84
           ELSE                                                         01.98
           IF WS-IS-INFINITE-QUAL                                       01.98
              IF KDA-MRCH-TYP-FUEL                                      01.98
                 MOVE VU-7331-PLN        TO WS-PLAN-QUAL-CODE-X         01.98
              ELSE                                                      01.98
                 MOVE VU-7133-PLN        TO WS-PLAN-QUAL-CODE-X         01.98
              END-IF                                                    01.98
           ELSE
           IF KDA-VS-PID-IS-TRD-REWARD                                  01.84
              MOVE VU-0601-PLN           TO WS-PLAN-QUAL-CODE-X         01.57
           ELSE
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-0901-PLN           TO WS-PLAN-QUAL-CODE-X         01.57
           ELSE
           IF KDA-CHD-AR-IS-FND-SRC-PREPAID                             01.84
              MOVE VU-0932-PLN           TO WS-PLAN-QUAL-CODE-X         01.64
           ELSE                                                         01.64
              MOVE VU-0931-PLN           TO WS-PLAN-QUAL-CODE-X.        01.57
      *
       I-200-PLAN-SET.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
           MOVE WS-LOW-RATE-AMT          TO WS-STANDARD-AMT.
      *
       I-200-EXIT.
           EXIT.
      *
       I-300-SET-ELECTRONIC.
      *
      * SET RATE TABLE VALUES FOR USA ELECTRONIC INTERCHANGE - EIRF
      *
      *    0701 VISA USA ELECTRONIC INTCHNG - REWARDS
      *    2101 VISA USA ELECTRONIC INTCHNG - CONSUMER
      *    2121 VISA USA ELECTRONIC INTCHNG - CORPORATE
      *    2131 VISA USA ELECTRONIC INTCHNG - DEBIT
      *    2132 VISA USA ELECTRONIC INTCHNG - PREPAID                   01.64
      *    2141 VISA USA ELECTRONIC INTCHNG - SIGNATURE PREFERRED
      *    2221 VISA USA ELECTRONIC INTCHNG - BUSINESS
      *    2232 VISA USA ELECTRONIC INTCHNG - FUEL - PREPAID MAX        01.64
      *    2321 VISA USA ELECTRONIC INTCHNG - PURCHASING
      *    2421 VISA USA ELECTRONIC INTCHNG - CORPORATE CPS LVL 3       01.77
      *    2621 VISA USA ELECTRONIC INTCHNG - CORPORATE NON-CPS LVL 3   01.77
      *    2721 VISA USA ELECTRONIC INTCHNG - CORPORATE TRAVEL SVC      01.77
      *    2731 VISA USA ELECTRONIC INTCHNG - DEBIT FUEL                01.84
      *    2732 VISA USA ELECTRONIC INTCHNG - FUEL - PREPAID            01.64
      *    2821 VISA USA ELECTRONIC INTCHNG - PURCHASE TRAVEL SVC       01.77
      *    3021 VISA USA ELECTRONIC INTCHNG - BUSINESS B2B
      *    3101 VISA USA ELECTRONIC INTCHNG - FUEL                      01.84
      *    3121 VISA USA ELECTRONIC INTCHNG - BUSINESS CNP
      *    3132 VISA USA ELECTRONIC INTCHNG - BUSINESS CNP PREPAID      01.89
      *    3201 VISA USA ELECTRONIC INTCHNG - FUEL MAX                  01.84
      *    4321 VISA USA ELECTRONIC INTCHNG - BUSINESS CNP DEBIT        01.77
      *    3140 VISA USA ELECTRONIC INTCHNG - BUSINESS ENHANCED         01.68
      *    3221 VISA USA ELECTRONIC INTCHNG - BUSINESS RTL
      *    3232 VISA USA ELECTRONIC INTCHNG - BUSINESS RTL PREPAID      01.86
      *    4421 VISA USA ELECTRONIC INTCHNG - BUSINESS CP DEBIT         01.77
      *    3240 VISA USA ELECTRONIC INTCHNG - BUSINESS LVL 2 ENHANCED   01.68
      *    3321 VISA USA ELECTRONIC INTCHNG - CORPORATE B2B
      *    3340 VISA USA ELECTRONIC INTCHNG - BUSINESS B2B ENHANCED     01.68
      *    3341 VISA USA ELECTRONIC INTCHNG - SIGNATURE PREFERRED B2B
      *    3421 VISA USA ELECTRONIC INTCHNG - CORPORATE CNP
      *    3440 VISA USA ELECTRONIC INTCHNG - BUSINESS CNP ENHANCED     01.68
      *    3441 VISA USA ELECTRONIC INTCHNG - SIGNATURE PREFERRED CNP
      *    3521 VISA USA ELECTRONIC INTCHNG - CORPORATE RTL
      *    3932 VISA USA ELECTRONIC INTCHNG - CORPORATE RTL PREPAID     01.86
      *    3540 VISA USA ELECTRONIC INTCHNG - BUSINESS RTL ENHANCED     01.68
      *    3541 VISA USA ELECTRONIC INTCHNG - SIGNATURE PREFERRED RTL
      *    3621 VISA USA ELECTRONIC INTCHNG - PURCHASING B2B
      *    3721 VISA USA ELECTRONIC INTCHNG - PURCHASING CNP
      *    3821 VISA USA ELECTRONIC INTCHNG - PURCHASING RTL
      *    3832 VISA USA ELECTRONIC INTCHNG - PURCHASING RTL PREPAID    01.86
      *    4140 VISA USA ELECTRONIC INTCHNG - BUSINESS SIGNATURE        01.68
      *    4240 VISA USA ELECTRONIC INTCHNG - BUSINESS LVL 2 SIGNATURE  01.68
      *    4340 VISA USA ELECTRONIC INTCHNG - BUSINESS B2B SIGNATURE    01.68
      *    4440 VISA USA ELECTRONIC INTCHNG - BUSINESS CNP SIGNATURE    01.68
      *    4540 VISA USA ELECTRONIC INTCHNG - BUSINESS RTL SIGNATURE    01.68
      *    6121 VISA USA ELECTRONIC INTCHNG - PURCHASING LVL 2          01.59
      *    6221 VISA USA ELECTRONIC INTCHNG - BUSINESS LVL 2            01.59
      *    6231 VISA USA ELECTRONIC INTCHNG - FUEL DEBIT MAX            01.84
      *    6321 VISA USA ELECTRONIC INTCHNG - PURCHASING LEVEL 3        01.80
      *    6421 VISA USA ELECTRONIC INTCHNG - CORPORATE LVL 2
      *    6521 VISA USA ELECTRONIC INTCHNG - PURCHASING NON-CPS LVL3   01.80
      *    6621 VISA USA ELECTRONIC INTCHNG - PURCHASING RTL FUEL
      *    6721 VISA USA ELECTRONIC INTCHNG - GSA G2G - PURCH/FLEET     01.48
      *    7033 VISA USA ELECTRONIC INTCHNG - INFINITE                  01.98
      *    7333 VISA USA ELECTRONIC INTCHNG - INFINITE RETAIL           01.98
      *    7233 VISA USA ELECTRONIC INTCHNG - INFINITE CNP              01.98
      *    7433 VISA USA ELECTRONIC INTCHNG - INFINITE B2B              01.98
      *
           IF WS-USE-UTILITY-RATE OR                                    01.48
              WS-USE-GOV-HE-PAY-RATE                                    01.86
                 GO TO I-300-EXIT                                       01.48
           END-IF.                                                      01.48
      *
           IF KDA-VS-PID-IS-COMMERCIAL                                  01.84
              GO TO I-300-CHK-ELECTRONIC-COMM.
      *
           IF WS-IS-INFINITE-QUAL                                       01.98
              IF WS-IS-INFINITE-FUEL                                    01.98
                 GO TO I-300-EXIT                                       01.98
              END-IF                                                    01.98
              IF WS-IS-PT-TSC                                           01.98
                 GO TO I-300-CHK-ELECTRONIC-COMM                        01.98
              ELSE                                                      01.98
                 GO TO I-300-CHK-ELEC-SIG-NON-TSC                       01.98
              END-IF                                                    01.98
           END-IF.                                                      01.98
      *                                                                 01.98
           IF WS-IS-SIG-PREF-CREDIT                                     01.55
              IF WS-IS-SIG-PREF-FUEL                                    01.54
                 GO TO I-300-EXIT                                       01.54
              END-IF                                                    01.54
              IF WS-IS-PT-TSC
                 GO TO I-300-CHK-ELECTRONIC-COMM
              ELSE
                 GO TO I-300-CHK-ELEC-SIG-NON-TSC
              END-IF
           END-IF.
      *
           IF KDA-VS-PID-IS-SIGNATURE OR                                01.84
              KDA-VS-PID-IS-INFINITE                                    01.84
                 NEXT SENTENCE
           ELSE
              GO TO I-300-SET-ELECTRONIC-CONS.
      *
       I-300-CHK-ELEC-SIG-TSC.
      *
           IF NOT WS-IS-PT-TSC
              GO TO I-300-CHK-ELEC-SIG-NON-TSC.
      *
           IF NOT WS-IS-CPS-REIMB
              IF KDA-CHD-AR-IS-FND-SRC-CREDIT                           01.84
                 IF KDA-MRCH-TYP-FUEL                                   01.84
                    MOVE VU-3101-PLN     TO WS-PLAN-DG                  01.84
                                            WS-PLAN-QUAL-CODE-X         01.84
                 ELSE                                                   01.84
                    MOVE VU-2101-PLN     TO WS-PLAN-DG                  01.84
                                            WS-PLAN-QUAL-CODE-X         01.84
                 END-IF                                                 01.84
              ELSE
                 IF KDA-MRCH-TYP-FUEL                                   01.84
                    MOVE VU-2731-PLN     TO WS-PLAN-DG                  01.84
                                            WS-PLAN-QUAL-CODE-X         01.84
                 ELSE                                                   01.84
                    MOVE VU-2131-PLN     TO WS-PLAN-DG                  01.84
                                            WS-PLAN-QUAL-CODE-X         01.84
                 END-IF                                                 01.47
              END-IF
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF WS-RATE-IS-LOWER
                 MOVE 'V020'    TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-300-EXIT.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              IF WS-CPS-PSI = VU-5601-PSI                               01.57
                 GO TO I-300-EXIT
              END-IF
              MOVE 'Y'                   TO VU-2101-CPS                 01.57
              MOVE WS-CPS-PSI            TO VU-2101-PSI                 01.57
              MOVE WS-CPS-REIMB          TO VU-2101-RMB                 01.57
              MOVE VU-2101-PLN           TO WS-PLAN-QUAL-CODE-X         01.57
           ELSE
              IF WS-CPS-PSI = VU-5631-PSI                               01.57
                 GO TO I-300-EXIT
              END-IF
              IF KDA-MRCH-TYP-FUEL                                      01.84
                 MOVE 'Y'                TO VU-2731-CPS                 01.84
                 MOVE WS-CPS-PSI         TO VU-2731-PSI                 01.84
                 MOVE WS-CPS-REIMB       TO VU-2731-RMB                 01.84
                 MOVE VU-2731-PLN        TO WS-PLAN-QUAL-CODE-X         01.84
              ELSE                                                      01.84
                 MOVE 'Y'                TO VU-2131-CPS                 01.84
                 MOVE WS-CPS-PSI         TO VU-2131-PSI                 01.84
                 MOVE WS-CPS-REIMB       TO VU-2131-RMB                 01.84
                 MOVE VU-2131-PLN        TO WS-PLAN-QUAL-CODE-X         01.84
              END-IF                                                    01.47
           END-IF.
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF NOT WS-RATE-IS-LOWER
              GO TO I-300-EXIT.
      *
           IF KDA-CHD-AR-IS-REGULATED AND                               01.64
             (WS-PLAN-QUAL-CODE-X = VU-2131-PLN OR                      01.84
              WS-PLAN-QUAL-CODE-X = VU-2731-PLN)                        01.84
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE SPACE              TO WS-CPS-PSI                  01.64
                                            VU-2532-PSI                 01.64
                 MOVE 'J'                TO WS-CPS-REIMB                01.64
                                            VU-2532-RMB                 01.64
                 MOVE VU-2532-PLN        TO WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE SPACE              TO WS-CPS-PSI                  01.64
                                            VU-2531-PSI                 01.64
                 MOVE 'J'                TO WS-CPS-REIMB                01.64
                                            VU-2531-RMB                 01.64
                 MOVE VU-2531-PLN        TO WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *                                                                 01.64
           GO TO I-300-SET-PLAN-DATA.
      *
       I-300-CHK-ELEC-SIG-NON-TSC.
      *
           IF NOT WS-IS-CPS-REIMB
              IF KDA-VS-PID-IS-SIG-PREF                                 01.84
              OR WS-IS-INFINITE-QUAL                                    01.98
                 GO TO I-300-NOT-LVL2                                   01.96
              ELSE
                 GO TO I-300-SET-ELECTRONIC-CONS
              END-IF
           END-IF.
      *
           IF WS-IS-CPS-REWARDS
              IF NOT KDA-VS-PID-IS-SIG-PREF AND                         01.98
                 NOT WS-IS-INFINITE-QUAL                                01.98
                 GO TO I-300-EXIT.
      *
           IF WS-CPS-RTL-2-FLG = 'Y' OR
              WS-CPS-SML-TKT-FLG = 'Y' OR
              WS-CPS-ECOM-PREF-FLG = 'Y' OR
              WS-CPS-ACCT-FUND-FLG = 'Y'
                 IF KDA-VS-PID-IS-SIG-PREF                              01.84
                 OR WS-IS-INFINITE-QUAL                                 01.98
                    GO TO I-300-NOT-LVL2                                01.96
                 ELSE
                    GO TO I-300-EXIT
                 END-IF
           END-IF.
      *
           IF WS-IS-SIG-UTIL                                            01.61
              GO TO I-300-EXIT                                          01.61
           END-IF.                                                      01.61
      *
           MOVE 'V020'                   TO WS-ERROR-DG.
           PERFORM S-300-PROCESS-DOWNGRADE
              THRU S-300-EXIT.
      *
           IF KDA-VS-PID-IS-SIG-PREF                                    01.84
           OR WS-IS-INFINITE-QUAL                                       01.98
              GO TO I-300-NOT-LVL2.                                     01.96
      *
           GO TO I-300-EXIT.
      *
       I-300-CHK-ELECTRONIC-COMM.
      *
           IF KDA-VS-PID-IS-BUSINESS AND                                01.84
              KDA-CHD-AR-IS-FND-SRC-DEBIT                               01.84
                 GO TO I-300-SET-BUSINESS-DEBIT                         01.86
           ELSE                                                         01.86
           IF KDA-VS-PID-IS-COMMERCIAL AND                              01.86
              KDA-CHD-AR-IS-FND-SRC-PREPAID                             01.86
                 GO TO I-300-SET-COMMERCIAL-PREPAID.                    01.86
      *                                                                 01.86
           IF WS-IS-PT-TSC
              IF NOT WS-IS-CPS-REIMB
                 MOVE VU-2121-PLN        TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                 IF KDA-VS-PID-IS-SIG-PREF                              01.84
                    MOVE VU-2141-PLN     TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                 END-IF                                                 01.68
                 IF WS-IS-INFINITE-QUAL                                 01.98
                    MOVE VU-7033-PLN     TO WS-PLAN-DG                  01.98
                                            WS-PLAN-QUAL-CODE-X         01.98
                 END-IF                                                 01.98
                 IF KDA-VS-PID-IS-BUSINESS                              01.84
                    IF KDA-VS-PID-IS-BUS-ENH                            01.84
                       MOVE VU-3140-PLN  TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                    ELSE                                                01.68
                       IF KDA-VS-PID-IS-BUS-SIG                         01.84
                          MOVE VU-4140-PLN                              01.68
                                         TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                       ELSE                                             01.68
                          MOVE VU-2221-PLN                              01.68
                                         TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                       END-IF                                           01.68
                    END-IF                                              01.68
                 END-IF                                                 01.68
                 IF KDA-VS-PID-IS-PURCHASE                              01.84
                    MOVE VU-2321-PLN     TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                 END-IF                                                 01.68
                 PERFORM S-525-CHECK-PLAN-LOW
                    THRU S-525-EXIT
                 IF WS-RATE-IS-LOWER
                    MOVE 'V020'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-300-EXIT
              ELSE
                 GO TO I-300-SET-ELECTRONIC-COMM.
      *
           IF NOT KDA-VS-PID-IS-COMMERCIAL                              01.84
              GO TO I-300-NOT-LVL2.                                     01.96
      *                                                                 01.48
           IF KDA-VS-PID-IS-GSA-PRCH-ANY                                01.84
              PERFORM I-305-GSA-PURCHASING                              01.48
                 THRU I-305-EXIT                                        01.48
              GO TO I-300-EXIT                                          01.48
           END-IF.                                                      01.48
      *
           IF KDA-VS-PID-IS-FLT-ANY                                     01.96
              PERFORM I-307-FLEET-PURCHASING                            01.96
                 THRU I-307-EXIT                                        01.96
              GO TO I-300-EXIT                                          01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           IF KDA-NOT-PQ-VS-USA-LVL3 OR                                 01.80
             (NOT KDA-HAS-LU)                                           01.80
                 NEXT SENTENCE                                          01.80
           ELSE                                                         01.80
              IF KDA-VS-PID-IS-CORPORATE AND                            01.84
                 KDA-HAS-XA AND                                         01.80
                 KDA-HAS-XL                                             01.80
                    GO TO I-300-CHK-ELECT-LVL3-CORP                     01.80
              END-IF                                                    01.80
              IF KDA-VS-PID-IS-CORPORATE AND                            02.03
                 KDA-HAS-PA AND                                         02.03
                 KDA-HAS-PL                                             02.03
                    GO TO I-300-CHK-ELECT-LVL3-CORP                     02.03
              END-IF                                                    02.03
              IF KDA-VS-PID-IS-PURCHASE AND                             01.84
                 KDA-HAS-PA AND                                         01.80
                 KDA-HAS-PL                                             01.80
                    GO TO I-300-CHK-ELECT-LVL3-PURCH                    01.80
              END-IF                                                    01.80
           END-IF.                                                      01.80
      *                                                                 01.80
           IF KDA-NOT-PQ-VS-USA-LVL2 OR
             (NOT KDA-HAS-LU)
              IF WS-IS-CPS-REIMB
                 MOVE VU-6421-PLN        TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                 IF KDA-VS-PID-IS-BUSINESS                              01.84
                    IF KDA-VS-PID-IS-BUS-ENH                            01.84
                       MOVE VU-3240-PLN  TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                    ELSE                                                01.68
                       IF KDA-VS-PID-IS-BUS-SIG                         01.84
                          MOVE VU-4240-PLN                              01.68
                                         TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                       ELSE                                             01.68
                          MOVE VU-6221-PLN                              01.68
                                         TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                       END-IF                                           01.68
                    END-IF                                              01.68
                 END-IF                                                 01.68
                 IF KDA-VS-PID-IS-PURCHASE                              01.84
                    MOVE VU-6121-PLN     TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                 END-IF                                                 01.68
              ELSE
                 MOVE VU-2121-PLN        TO WS-PLAN-DG                  02.01
                                            WS-PLAN-QUAL-CODE-X         02.01
                 IF KDA-VS-PID-IS-BUSINESS                              02.01
                    IF KDA-VS-PID-IS-BUS-ENH                            02.01
                       MOVE VU-3140-PLN  TO WS-PLAN-DG                  02.01
                                            WS-PLAN-QUAL-CODE-X         02.01
                    ELSE                                                02.01
                       IF KDA-VS-PID-IS-BUS-SIG                         02.01
                          MOVE VU-4140-PLN                              02.01
                                         TO WS-PLAN-DG                  02.01
                                            WS-PLAN-QUAL-CODE-X         02.01
                       ELSE                                             02.01
                          MOVE VU-2221-PLN                              02.01
                                         TO WS-PLAN-DG                  02.01
                                            WS-PLAN-QUAL-CODE-X         02.01
                       END-IF                                           02.01
                    END-IF                                              02.01
                 END-IF                                                 02.01
                 IF KDA-VS-PID-IS-PURCHASE                              02.01
                    MOVE VU-2321-PLN     TO WS-PLAN-DG                  02.01
                                           WS-PLAN-QUAL-CODE-X          02.01
                 END-IF                                                 02.01
              END-IF
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF WS-RATE-IS-LOWER
                 MOVE 'V022'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-300-NOT-LVL2
           END-IF.
      *
           GO TO I-300-CHK-ELECT-LVL2.                                  01.96
      *
       I-300-CHK-ELECT-LVL3-CORP.                                       01.77
      *                                                                 01.77
           IF WS-CPS-ACCT-FUND-FLG = 'Y'                                01.77
              GO TO I-300-CHK-ELECT-LVL2.                               01.77
      *                                                                 01.77
           IF NOT WS-IS-CPS-REIMB                                       01.77
              MOVE VU-2421-PLN           TO WS-PLAN-DG                  01.77
                                            WS-PLAN-QUAL-CODE-X         01.77
              PERFORM S-525-CHECK-PLAN-LOW                              01.77
                 THRU S-525-EXIT                                        01.77
              IF WS-RATE-IS-LOWER                                       01.77
                 MOVE 'V020'             TO WS-ERROR-DG                 01.77
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.77
                    THRU S-300-EXIT                                     01.77
              END-IF                                                    01.77
              MOVE VU-2621-PLN           TO WS-PLAN-DG                  01.77
                                            WS-PLAN-QUAL-CODE-X         01.77
              GO TO I-300-SET-PLAN-DATA                                 01.77
           END-IF.                                                      01.77
      *                                                                 01.77
           MOVE WS-CPS-PSI               TO VU-2421-PSI.                01.77
           MOVE WS-CPS-REIMB             TO VU-2421-RMB.                01.77
           MOVE VU-2421-PLN              TO WS-PLAN-DG                  01.77
                                            WS-PLAN-QUAL-CODE-X.        01.77
      *                                                                 01.77
           PERFORM S-525-CHECK-PLAN-LOW                                 01.77
              THRU S-525-EXIT.                                          01.77
           IF NOT WS-RATE-IS-LOWER                                      01.77
              GO TO I-300-EXIT.                                         01.77
      *                                                                 01.77
           GO TO I-300-SET-PLAN-DATA.                                   01.77
      *                                                                 01.77
       I-300-CHK-ELECT-LVL3-PURCH.                                      01.80
      *
           IF NOT WS-IS-CPS-REIMB
              MOVE VU-6321-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF WS-RATE-IS-LOWER
                 MOVE 'V020'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              MOVE VU-6521-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
              GO TO I-300-SET-PLAN-DATA
           END-IF.
      *
           MOVE WS-CPS-PSI               TO VU-6321-PSI.                01.57
           MOVE WS-CPS-REIMB             TO VU-6321-RMB.                01.57
           MOVE VU-6321-PLN              TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X.
      *
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF NOT WS-RATE-IS-LOWER
              GO TO I-300-EXIT.
      *
           GO TO I-300-SET-PLAN-DATA.
      *
       I-300-CHK-ELECT-LVL2.
      *
           IF NOT WS-IS-CPS-REIMB
              MOVE VU-6421-PLN           TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              IF KDA-VS-PID-IS-BUSINESS                                 01.84
                 MOVE VU-6221-PLN        TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                 IF KDA-VS-PID-IS-BUS-ENH                               01.84
                    MOVE VU-3240-PLN     TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                 ELSE                                                   01.68
                    IF KDA-VS-PID-IS-BUS-SIG                            01.84
                       MOVE VU-4240-PLN                                 01.68
                                         TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                    END-IF                                              01.68
                 END-IF                                                 01.68
              END-IF                                                    01.68
              IF KDA-VS-PID-IS-PURCHASE                                 01.84
                 MOVE VU-6121-PLN        TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              END-IF                                                    01.68
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF WS-RATE-IS-LOWER
                 MOVE 'V020'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              IF KDA-VS-PID-IS-BUSINESS OR                              01.88B
                 KDA-VS-PID-IS-BUS-ENH  OR                              01.88B
                 KDA-VS-PID-IS-BUS-SIG                                  01.88B
                   GO TO I-300-EXIT                                     01.88B
              ELSE                                                      01.88B
                   SET WS-USE-COMM-ELECT-ONLY TO TRUE                   01.88B
                   GO TO I-300-SET-ELECTRONIC-COMM                      01.88B
              END-IF                                                    01.88B
           END-IF.                                                      01.68
      *
           IF KDA-VS-PID-IS-BUSINESS                                    01.84
              MOVE WS-CPS-PSI            TO VU-6221-PSI                 01.68
              MOVE WS-CPS-REIMB          TO VU-6221-RMB                 01.68
              MOVE VU-6221-PLN           TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              IF KDA-VS-PID-IS-BUS-ENH                                  01.84
                 MOVE WS-CPS-PSI         TO VU-3240-PSI                 01.68
                 MOVE WS-CPS-REIMB       TO VU-3240-RMB                 01.68
                 MOVE VU-3240-PLN        TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              ELSE                                                      01.68
                 IF KDA-VS-PID-IS-BUS-SIG                               01.84
                    MOVE WS-CPS-PSI      TO VU-4240-PSI                 01.68
                    MOVE WS-CPS-REIMB    TO VU-4240-RMB                 01.68
                    MOVE VU-4240-PLN     TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                 END-IF                                                 01.68
              END-IF                                                    01.68
           END-IF.
           IF KDA-VS-PID-IS-CORPORATE                                   01.84
              MOVE WS-CPS-PSI            TO VU-6421-PSI                 01.57
              MOVE WS-CPS-REIMB          TO VU-6421-RMB                 01.57
              MOVE VU-6421-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
           IF KDA-VS-PID-IS-PURCHASE                                    01.84
              MOVE WS-CPS-PSI            TO VU-6121-PSI                 01.59
              MOVE WS-CPS-REIMB          TO VU-6121-RMB                 01.59
              MOVE VU-6121-PLN           TO WS-PLAN-DG                  01.59
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
      *
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF NOT WS-RATE-IS-LOWER
              GO TO I-300-EXIT.
      *
           GO TO I-300-SET-PLAN-DATA.
      *
       I-300-NOT-LVL2.
      *
           IF KDA-MRCH-TYP-B2B AND
             (WS-CPS-PSI NOT = SPACE)
              GO TO I-300-CHK-ELECT-B2B.
      *
           IF WS-CPS-RTL-FLG = 'Y' OR
              WS-CPS-SPR-MKT-FLG = 'Y' OR
              WS-CPS-RTL-KEY-FLG = 'Y' OR
              WS-CPS-SML-TKT-FLG = 'Y' OR
              WS-CPS-AUTO-FUEL-FLG = 'Y' OR
              WS-CPS-RTL-SST-FLG = 'Y'
                 GO TO I-300-CHK-ELECT-RTL
           END-IF.
      *
           IF WS-CPS-RTL-CNP-FLG = 'Y' OR
              WS-CPS-ECOM-PREF-FLG = 'Y' OR
              WS-CPS-ECOM-BASIC-FLG = 'Y' OR
              WS-CPS-ACCT-FUND-FLG = 'Y'                                01.96
                 GO TO I-300-CHK-ELECT-CNP
           END-IF.
      *
           IF KDA-VS-PID-IS-BUSINESS                                    01.84
              MOVE VU-3121-PLN           TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              IF KDA-VS-PID-IS-BUS-ENH                                  01.84
                 MOVE VU-3440-PLN        TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              ELSE                                                      01.68
                 IF KDA-VS-PID-IS-BUS-SIG                               01.84
                    MOVE VU-4440-PLN     TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                 END-IF                                                 01.68
              END-IF                                                    01.68
           END-IF.
           IF KDA-VS-PID-IS-CORPORATE                                   01.84
              MOVE VU-3421-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
           IF KDA-VS-PID-IS-SIG-PREF                                    01.84
              MOVE VU-3441-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
           IF WS-IS-INFINITE-QUAL                                       01.98
              MOVE VU-7233-PLN           TO WS-PLAN-DG                  01.98
                                            WS-PLAN-QUAL-CODE-X         01.98
           END-IF.                                                      01.98
           IF KDA-VS-PID-IS-PURCHASE                                    01.84
              MOVE VU-3721-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
      *
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF WS-RATE-IS-LOWER
              MOVE 'V020'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
           END-IF.
      *
           GO TO I-300-EXIT.
      *
       I-300-CHK-ELECT-B2B.
      *
           IF KDA-VS-PID-IS-BUSINESS                                    01.84
              MOVE WS-CPS-PSI            TO VU-3021-PSI                 01.68
              MOVE WS-CPS-REIMB          TO VU-3021-RMB                 01.68
              MOVE VU-3021-PLN           TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              IF KDA-VS-PID-IS-BUS-ENH                                  01.84
                 MOVE WS-CPS-PSI         TO VU-3340-PSI                 01.68
                 MOVE WS-CPS-REIMB       TO VU-3340-RMB                 01.68
                 MOVE VU-3340-PLN        TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              ELSE                                                      01.68
                 IF KDA-VS-PID-IS-BUS-SIG                               01.84
                    MOVE WS-CPS-PSI      TO VU-4340-PSI                 01.68
                    MOVE WS-CPS-REIMB    TO VU-4340-RMB                 01.68
                    MOVE VU-4340-PLN     TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                 END-IF                                                 01.68
              END-IF                                                    01.68
           END-IF.
           IF KDA-VS-PID-IS-CORPORATE                                   01.84
              MOVE WS-CPS-PSI            TO VU-3321-PSI                 01.57
              MOVE WS-CPS-REIMB          TO VU-3321-RMB                 01.57
              MOVE VU-3321-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
           IF KDA-VS-PID-IS-SIG-PREF                                    01.84
              MOVE WS-CPS-PSI            TO VU-3341-PSI                 01.57
              MOVE WS-CPS-REIMB          TO VU-3341-RMB                 01.57
              MOVE VU-3341-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
           IF WS-IS-INFINITE-QUAL                                       01.98
              MOVE WS-CPS-PSI            TO VU-7433-PSI                 01.98
              MOVE WS-CPS-REIMB          TO VU-7433-RMB                 01.98
              MOVE VU-7433-PLN           TO WS-PLAN-DG                  01.98
                                            WS-PLAN-QUAL-CODE-X         01.98
           END-IF.                                                      01.98
           IF KDA-VS-PID-IS-PURCHASE                                    01.84
              MOVE WS-CPS-PSI            TO VU-3621-PSI                 01.57
              MOVE WS-CPS-REIMB          TO VU-3621-RMB                 01.57
              MOVE VU-3621-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
      *
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF NOT WS-RATE-IS-LOWER
              GO TO I-300-EXIT.
      *
           GO TO I-300-NOT-LVL2-CHK-CPS.
      *
       I-300-CHK-ELECT-CNP.
      *
           IF KDA-VS-PID-IS-BUSINESS                                    01.84
              MOVE WS-CPS-PSI            TO VU-3121-PSI                 01.68
              MOVE WS-CPS-REIMB          TO VU-3121-RMB                 01.68
              MOVE VU-3121-PLN           TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              IF KDA-VS-PID-IS-BUS-ENH                                  01.84
                 MOVE WS-CPS-PSI         TO VU-3440-PSI                 01.68
                 MOVE WS-CPS-REIMB       TO VU-3440-RMB                 01.68
                 MOVE VU-3440-PLN        TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              ELSE                                                      01.68
                 IF KDA-VS-PID-IS-BUS-SIG                               01.84
                    MOVE WS-CPS-PSI      TO VU-4440-PSI                 01.68
                    MOVE WS-CPS-REIMB    TO VU-4440-RMB                 01.68
                    MOVE VU-4440-PLN     TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              END-IF                                                    01.68
           END-IF.
           IF KDA-VS-PID-IS-CORPORATE                                   01.84
              MOVE WS-CPS-PSI            TO VU-3421-PSI                 01.57
              MOVE WS-CPS-REIMB          TO VU-3421-RMB                 01.57
              MOVE VU-3421-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
           IF KDA-VS-PID-IS-SIG-PREF                                    01.84
              MOVE WS-CPS-PSI            TO VU-3441-PSI                 01.57
              MOVE WS-CPS-REIMB          TO VU-3441-RMB                 01.57
              MOVE VU-3441-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
           IF WS-IS-INFINITE-QUAL                                       01.98
              MOVE WS-CPS-PSI            TO VU-7233-PSI                 01.98
              MOVE WS-CPS-REIMB          TO VU-7233-RMB                 01.98
              MOVE VU-7233-PLN           TO WS-PLAN-DG                  01.98
                                            WS-PLAN-QUAL-CODE-X         01.98
           END-IF.                                                      01.98
           IF KDA-VS-PID-IS-PURCHASE                                    01.84
              MOVE WS-CPS-PSI            TO VU-3721-PSI                 01.57
              MOVE WS-CPS-REIMB          TO VU-3721-RMB                 01.57
              MOVE VU-3721-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
      *
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF NOT WS-RATE-IS-LOWER
              GO TO I-300-EXIT.
      *
           GO TO I-300-NOT-LVL2-CHK-CPS.
      *
       I-300-CHK-ELECT-RTL.
      *
           IF KDA-VS-PID-IS-BUSINESS                                    01.84
              MOVE WS-CPS-PSI            TO VU-3221-PSI                 01.68
              MOVE WS-CPS-REIMB          TO VU-3221-RMB                 01.68
              MOVE VU-3221-PLN           TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              IF KDA-VS-PID-IS-BUS-ENH                                  01.84
                 MOVE WS-CPS-PSI         TO VU-3540-PSI                 01.68
                 MOVE WS-CPS-REIMB       TO VU-3540-RMB                 01.68
                 MOVE VU-3540-PLN        TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
              ELSE                                                      01.68
                 IF KDA-VS-PID-IS-BUS-SIG                               01.84
                    MOVE WS-CPS-PSI      TO VU-4540-PSI                 01.68
                    MOVE WS-CPS-REIMB    TO VU-4540-RMB                 01.68
                    MOVE VU-4540-PLN     TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X         01.68
                 END-IF                                                 01.68
              END-IF                                                    01.68
           END-IF.
           IF KDA-VS-PID-IS-CORPORATE                                   01.84
              MOVE WS-CPS-PSI            TO VU-3521-PSI                 01.57
              MOVE WS-CPS-REIMB          TO VU-3521-RMB                 01.57
              MOVE VU-3521-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
           IF KDA-VS-PID-IS-SIG-PREF                                    01.84
              MOVE WS-CPS-PSI            TO VU-3541-PSI                 01.57
              MOVE WS-CPS-REIMB          TO VU-3541-RMB                 01.57
              MOVE VU-3541-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
           IF WS-IS-INFINITE-QUAL                                       01.98
              MOVE WS-CPS-PSI            TO VU-7333-PSI                 01.98
              MOVE WS-CPS-REIMB          TO VU-7333-RMB                 01.98
              MOVE VU-7333-PLN           TO WS-PLAN-DG                  01.98
                                            WS-PLAN-QUAL-CODE-X         01.98
           END-IF.                                                      01.98
           IF KDA-VS-PID-IS-PURCHASE                                    01.84
              MOVE WS-CPS-PSI            TO VU-3821-PSI                 01.57
              MOVE WS-CPS-REIMB          TO VU-3821-RMB                 01.57
              MOVE VU-3821-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
      *
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF NOT WS-RATE-IS-LOWER
              GO TO I-300-EXIT.
      *
       I-300-NOT-LVL2-CHK-CPS.
      *
           IF NOT WS-IS-CPS-REIMB
              MOVE 'V020'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-300-EXIT
           END-IF.
      *
           GO TO I-300-SET-PLAN-DATA.
      *
       I-300-SET-BUSINESS-DEBIT.                                        01.77
      *                                                                 01.77
           IF WS-CPS-RTL-FLG = 'Y' OR                                   01.77
              WS-CPS-RTL-KEY-FLG = 'Y' OR                               01.77
              WS-CPS-SML-TKT-FLG = 'Y' OR                               01.77
              WS-CPS-SPR-MKT-FLG = 'Y' OR                               01.77
              WS-CPS-RTL-RST-FLG = 'Y' OR                               01.77
              WS-CPS-RTL-SST-FLG = 'Y' OR                               01.77
              WS-CPS-AUTO-FUEL-FLG = 'Y' OR                             01.77
              WS-CPS-HC-CP-FLG = 'Y' OR                                 01.77
              WS-CPS-PT-CP-FLG = 'Y'                                    01.77
      *                                                                 01.77
                 MOVE WS-CPS-PSI            TO VU-4421-PSI              01.77
                 MOVE WS-CPS-REIMB          TO VU-4421-RMB              01.77
                 MOVE VU-4421-PLN           TO WS-PLAN-DG               01.77
                                               WS-PLAN-QUAL-CODE-X      01.77
           ELSE                                                         01.77
           IF WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.77
              WS-CPS-ECOM-BASIC-FLG = 'Y' OR                            01.77
              WS-CPS-ECOM-PREF-FLG = 'Y' OR                             01.77
              WS-CPS-HC-CNP-FLG = 'Y' OR                                01.77
              WS-CPS-PT-CNP-FLG = 'Y' OR                                01.77
              WS-CPS-ECOM-PREF-HC-FLG = 'Y' OR                          01.77
              WS-CPS-ECOM-PREF-PT-FLG = 'Y' OR                          01.77
              WS-CPS-ACCT-FUND-FLG = 'Y'                                01.77
      *                                                                 01.77
                 MOVE WS-CPS-PSI            TO VU-4321-PSI              01.77
                 MOVE WS-CPS-REIMB          TO VU-4321-RMB              01.77
                 MOVE VU-4321-PLN           TO WS-PLAN-DG               01.77
                                               WS-PLAN-QUAL-CODE-X      01.77
           ELSE                                                         01.77
           IF WS-CPS-RTL-FLG = 'A' OR                                   01.77
              WS-CPS-RTL-KEY-FLG = 'A' OR                               01.77
              WS-CPS-SML-TKT-FLG = 'A' OR                               01.77
              WS-CPS-SPR-MKT-FLG = 'A' OR                               01.77
              WS-CPS-RTL-RST-FLG = 'A' OR                               01.77
              WS-CPS-RTL-SST-FLG = 'A' OR                               01.77
              WS-CPS-AUTO-FUEL-FLG = 'A' OR                             01.77
              WS-CPS-HC-CP-FLG = 'A' OR                                 01.77
              WS-CPS-PT-CP-FLG = 'A'                                    01.77
                 MOVE VU-4421-PLN        TO WS-PLAN-DG                  01.77
                                            WS-PLAN-QUAL-CODE-X         01.77
                 PERFORM S-525-CHECK-PLAN-LOW                           01.77
                    THRU S-525-EXIT                                     01.77
                 IF WS-RATE-IS-LOWER                                    01.77
                    MOVE 'V020'          TO WS-ERROR-DG                 01.77
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.77
                       THRU S-300-EXIT                                  01.77
                 END-IF                                                 01.77
                 GO TO I-300-EXIT                                       01.77
           ELSE                                                         01.77
           IF WS-CPS-RTL-CNP-FLG = 'A' OR                               01.77
              WS-CPS-ECOM-BASIC-FLG = 'A' OR                            01.77
              WS-CPS-ECOM-PREF-FLG = 'A' OR                             01.77
              WS-CPS-HC-CNP-FLG = 'A' OR                                01.77
              WS-CPS-PT-CNP-FLG = 'A' OR                                01.77
              WS-CPS-ECOM-PREF-HC-FLG = 'A' OR                          01.77
              WS-CPS-ECOM-PREF-PT-FLG = 'A' OR                          01.77
              WS-CPS-ACCT-FUND-FLG = 'A'                                01.77
                 MOVE VU-4321-PLN        TO WS-PLAN-DG                  01.77
                                            WS-PLAN-QUAL-CODE-X         01.77
                 PERFORM S-525-CHECK-PLAN-LOW                           01.77
                    THRU S-525-EXIT                                     01.77
                 IF WS-RATE-IS-LOWER                                    01.77
                    MOVE 'V020'          TO WS-ERROR-DG                 01.77
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.77
                       THRU S-300-EXIT                                  01.77
                 END-IF                                                 01.77
                 GO TO I-300-EXIT                                       01.77
           END-IF                                                       01.77
           END-IF                                                       01.77
           END-IF                                                       01.77
           END-IF.                                                      01.77
      *                                                                 01.77
           PERFORM S-525-CHECK-PLAN-LOW                                 01.77
              THRU S-525-EXIT.                                          01.77
           IF NOT WS-RATE-IS-LOWER                                      01.77
              GO TO I-300-EXIT.                                         01.77
      *                                                                 01.77
           GO TO I-300-SET-PLAN-DATA.                                   01.77
      *                                                                 01.77
       I-300-SET-COMMERCIAL-PREPAID.                                    01.86
      *                                                                 01.86
           IF WS-CPS-RTL-FLG = 'Y' OR                                   01.86
              WS-CPS-RTL-KEY-FLG = 'Y' OR                               01.86
              WS-CPS-SML-TKT-FLG = 'Y' OR                               01.86
              WS-CPS-SPR-MKT-FLG = 'Y' OR                               01.86
              WS-CPS-RTL-RST-FLG = 'Y' OR                               01.86
              WS-CPS-RTL-SST-FLG = 'Y' OR                               01.86
              WS-CPS-AUTO-FUEL-FLG = 'Y' OR                             01.86
              WS-CPS-HC-CP-FLG = 'Y' OR                                 01.86
              WS-CPS-PT-CP-FLG = 'Y'                                    01.86
                 IF KDA-VS-PID-IS-CORPORATE                             01.86
                    MOVE WS-CPS-PSI            TO VU-3932-PSI           01.86
                    MOVE WS-CPS-REIMB          TO VU-3932-RMB           01.86
                    MOVE VU-3932-PLN           TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 ELSE                                                   01.86
                 IF KDA-VS-PID-IS-BUSINESS                              01.86
                    MOVE WS-CPS-PSI            TO VU-3232-PSI           01.86
                    MOVE WS-CPS-REIMB          TO VU-3232-RMB           01.86
                    MOVE VU-3232-PLN           TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 ELSE                                                   01.86
                    MOVE WS-CPS-PSI            TO VU-3832-PSI           01.86
                    MOVE WS-CPS-REIMB          TO VU-3832-RMB           01.86
                    MOVE VU-3832-PLN           TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 END-IF                                                 01.86
                 END-IF                                                 01.86
      *                                                                 01.86
           ELSE                                                         01.86
           IF WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.86
              WS-CPS-ECOM-BASIC-FLG = 'Y' OR                            01.86
              WS-CPS-ECOM-PREF-FLG = 'Y' OR                             01.86
              WS-CPS-PT-CNP-FLG = 'Y' OR                                01.86
              WS-CPS-HC-CNP-FLG = 'Y' OR                                01.86
              WS-CPS-ECOM-PREF-HC-FLG = 'Y' OR                          01.86
              WS-CPS-ECOM-PREF-PT-FLG = 'Y' OR                          01.86
              WS-CPS-ACCT-FUND-FLG = 'Y'                                01.86
                 IF KDA-VS-PID-IS-CORPORATE                             01.86
                    MOVE WS-CPS-PSI            TO VU-3432-PSI           01.86
                    MOVE WS-CPS-REIMB          TO VU-3432-RMB           01.86
                    MOVE VU-3432-PLN           TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 ELSE                                                   01.86
                 IF KDA-VS-PID-IS-BUSINESS                              01.86
                    MOVE WS-CPS-PSI            TO VU-3132-PSI           01.86
                    MOVE WS-CPS-REIMB          TO VU-3132-RMB           01.86
                    MOVE VU-3132-PLN           TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 ELSE                                                   01.86
                    MOVE WS-CPS-PSI            TO VU-3732-PSI           01.86
                    MOVE WS-CPS-REIMB          TO VU-3732-RMB           01.86
                    MOVE VU-3732-PLN           TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 END-IF                                                 01.86
                 END-IF                                                 01.86
      *                                                                 01.86
           ELSE                                                         01.86
           IF WS-CPS-RTL-FLG = 'A' OR                                   01.86
              WS-CPS-RTL-KEY-FLG = 'A' OR                               01.86
              WS-CPS-SML-TKT-FLG = 'A' OR                               01.86
              WS-CPS-SPR-MKT-FLG = 'A' OR                               01.86
              WS-CPS-RTL-RST-FLG = 'A' OR                               01.86
              WS-CPS-RTL-SST-FLG = 'A' OR                               01.86
              WS-CPS-AUTO-FUEL-FLG = 'A' OR                             01.86
              WS-CPS-HC-CP-FLG = 'A' OR                                 01.86
              WS-CPS-PT-CP-FLG = 'A'                                    01.86
                 IF KDA-VS-PID-IS-CORPORATE                             01.86
                    MOVE VU-3932-PLN     TO WS-PLAN-DG                  01.86
                                            WS-PLAN-QUAL-CODE-X         01.86
                 ELSE                                                   01.86
                 IF KDA-VS-PID-IS-BUSINESS                              01.86
                    MOVE VU-3232-PLN     TO WS-PLAN-DG                  01.86
                                            WS-PLAN-QUAL-CODE-X         01.86
                 ELSE                                                   01.86
                    MOVE VU-3832-PLN     TO WS-PLAN-DG                  01.86
                                            WS-PLAN-QUAL-CODE-X         01.86
                 END-IF                                                 01.86
                 END-IF                                                 01.86
                 PERFORM S-525-CHECK-PLAN-LOW                           01.86
                    THRU S-525-EXIT                                     01.86
                 IF WS-RATE-IS-LOWER                                    01.86
                    MOVE 'V020'          TO WS-ERROR-DG                 01.86
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.86
                       THRU S-300-EXIT                                  01.86
                 END-IF                                                 01.86
                 GO TO I-300-EXIT                                       01.86
           ELSE                                                         01.86
           IF WS-CPS-RTL-CNP-FLG = 'A' OR                               01.86
              WS-CPS-ECOM-BASIC-FLG = 'A' OR                            01.86
              WS-CPS-ECOM-PREF-FLG = 'A' OR                             01.86
              WS-CPS-HC-CNP-FLG = 'A' OR                                01.86
              WS-CPS-PT-CNP-FLG = 'A' OR                                01.86
              WS-CPS-ECOM-PREF-HC-FLG = 'A' OR                          01.86
              WS-CPS-ECOM-PREF-PT-FLG = 'A' OR                          01.86
              WS-CPS-ACCT-FUND-FLG = 'A'                                01.86
                 IF KDA-VS-PID-IS-CORPORATE                             01.86
                    MOVE VU-3432-PLN     TO WS-PLAN-DG                  01.86
                                            WS-PLAN-QUAL-CODE-X         01.86
                 ELSE                                                   01.86
                 IF KDA-VS-PID-IS-BUSINESS                              01.86
                    MOVE VU-3132-PLN     TO WS-PLAN-DG                  01.86
                                            WS-PLAN-QUAL-CODE-X         01.86
                 ELSE                                                   01.86
                    MOVE VU-3732-PLN     TO WS-PLAN-DG                  01.86
                                            WS-PLAN-QUAL-CODE-X         01.86
                 END-IF                                                 01.86
                 END-IF                                                 01.86
                 PERFORM S-525-CHECK-PLAN-LOW                           01.86
                    THRU S-525-EXIT                                     01.86
                 IF WS-RATE-IS-LOWER                                    01.86
                    MOVE 'V020'          TO WS-ERROR-DG                 01.86
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.86
                       THRU S-300-EXIT                                  01.86
                 END-IF                                                 01.86
                 GO TO I-300-EXIT                                       01.86
           END-IF                                                       01.86
           END-IF                                                       01.86
           END-IF                                                       01.86
           END-IF.                                                      01.86
      *                                                                 01.86
           PERFORM S-525-CHECK-PLAN-LOW                                 01.86
              THRU S-525-EXIT.                                          01.86
           IF NOT WS-RATE-IS-LOWER                                      01.86
              GO TO I-300-EXIT.                                         01.86
      *                                                                 01.86
           GO TO I-300-SET-PLAN-DATA.                                   01.86
      *                                                                 01.86
       I-300-SET-ELECTRONIC-COMM.
      *
           IF KDA-VS-PID-IS-BUSINESS                                    01.84
              IF WS-CPS-PSI = SPACE
                 IF KDA-VS-PID-IS-BUS-ENH OR                            01.84
                    KDA-VS-PID-IS-BUS-SIG                               01.84
                       GO TO I-300-EXIT                                 01.68
                 ELSE                                                   01.68
                    MOVE 'N'             TO VU-2221-CPS                 01.68
                    MOVE SPACE           TO VU-2221-PSI                 01.68
                    MOVE 'J'             TO VU-2221-RMB                 01.68
                    MOVE VU-2221-PLN     TO WS-PLAN-DG                  01.71
                                            WS-PLAN-QUAL-CODE-X         01.71
                 END-IF                                                 01.68
              ELSE                                                      01.68
                 IF KDA-VS-PID-IS-BUS-ENH                               01.84
                    MOVE WS-CPS-PSI      TO VU-3140-PSI                 01.72
                    MOVE WS-CPS-REIMB    TO VU-3140-RMB                 01.72
                    MOVE VU-3140-PLN     TO WS-PLAN-DG                  01.71
                                            WS-PLAN-QUAL-CODE-X         01.71
                    GO TO I-300-SET-PLAN-DATA                           01.72
                 ELSE                                                   01.71
                    IF KDA-VS-PID-IS-BUS-SIG                            01.84
                       MOVE WS-CPS-PSI   TO VU-4140-PSI                 01.75
                       MOVE WS-CPS-REIMB TO VU-4140-RMB                 01.75
                       MOVE VU-4140-PLN  TO WS-PLAN-DG                  01.75
                                            WS-PLAN-QUAL-CODE-X         01.75
                       GO TO I-300-SET-PLAN-DATA                        01.75
                    ELSE                                                01.75
                       MOVE 'Y'          TO VU-2221-CPS                 01.75
                       MOVE WS-CPS-PSI   TO VU-2221-PSI                 01.75
                       MOVE WS-CPS-REIMB TO VU-2221-RMB                 01.75
                       MOVE VU-2221-PLN  TO WS-PLAN-DG                  01.75
                                            WS-PLAN-QUAL-CODE-X         01.75
                    END-IF                                              01.75
                 END-IF                                                 01.71
              END-IF                                                    01.69
           END-IF.
      *
           IF KDA-VS-PID-IS-CORPORATE                                   01.84
              IF WS-IS-PT-TSC                                           01.77
                 IF WS-CPS-PSI = SPACE                                  01.77
                    MOVE 'N'                TO VU-2121-CPS              01.77
                    MOVE SPACE              TO VU-2121-PSI              01.77
                    MOVE 'J'                TO VU-2121-RMB              01.77
                    MOVE VU-2121-PLN        TO WS-PLAN-DG               01.77
                                               WS-PLAN-QUAL-CODE-X      01.77
                 ELSE                                                   01.77
                    MOVE 'Y'                TO VU-2721-CPS              01.77
                    MOVE WS-CPS-PSI         TO VU-2721-PSI              01.77
                    MOVE WS-CPS-REIMB       TO VU-2721-RMB              01.77
                    MOVE VU-2721-PLN        TO WS-PLAN-DG               01.77
                                               WS-PLAN-QUAL-CODE-X      01.77
                 END-IF                                                 01.77
              ELSE                                                      01.77
                 IF WS-CPS-PSI = SPACE                                  01.77
                    MOVE 'N'                TO VU-2121-CPS              01.77
                    MOVE SPACE              TO VU-2121-PSI              01.77
                    MOVE 'J'                TO VU-2121-RMB              01.77
                 ELSE                                                   01.77
                    MOVE 'Y'                TO VU-2121-CPS              01.77
                    MOVE WS-CPS-PSI         TO VU-2121-PSI              01.77
                    MOVE WS-CPS-REIMB       TO VU-2121-RMB              01.77
                 END-IF                                                 01.77
                 MOVE VU-2121-PLN           TO WS-PLAN-DG               01.77
                                               WS-PLAN-QUAL-CODE-X      01.77
              END-IF                                                    01.77
           END-IF.
      *
           IF KDA-VS-PID-IS-SIG-PREF                                    01.84
              IF WS-CPS-PSI = SPACE
                 MOVE 'N'                TO VU-2141-CPS                 01.57
                 MOVE SPACE              TO VU-2141-PSI                 01.57
                 MOVE 'J'                TO VU-2141-RMB                 01.57
              ELSE
                 MOVE 'Y'                TO VU-2141-CPS                 01.57
                 MOVE WS-CPS-PSI         TO VU-2141-PSI                 01.57
                 MOVE WS-CPS-REIMB       TO VU-2141-RMB                 01.57
              END-IF
              MOVE VU-2141-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           END-IF.
      *
           IF WS-IS-INFINITE-QUAL                                       01.98
              IF WS-CPS-PSI = SPACE                                     01.98
                 MOVE 'N'                TO VU-7033-CPS                 01.98
                 MOVE SPACE              TO VU-7033-PSI                 01.98
                 MOVE 'J'                TO VU-7033-RMB                 01.98
              ELSE                                                      01.98
                 MOVE 'Y'                TO VU-7033-CPS                 01.98
                 MOVE WS-CPS-PSI         TO VU-7033-PSI                 01.98
                 MOVE WS-CPS-REIMB       TO VU-7033-RMB                 01.98
              END-IF                                                    01.98
              MOVE VU-7033-PLN           TO WS-PLAN-DG                  01.98
                                            WS-PLAN-QUAL-CODE-X         01.98
           END-IF.                                                      01.98
      *                                                                 01.98
           IF KDA-VS-PID-IS-PURCHASE                                    01.84
              IF WS-IS-PT-TSC                                           01.77
                 IF WS-CPS-PSI = SPACE                                  01.77
                    MOVE 'N'                TO VU-2321-CPS              01.77
                    MOVE SPACE              TO VU-2321-PSI              01.77
                    MOVE 'J'                TO VU-2321-RMB              01.77
                    MOVE VU-2321-PLN        TO WS-PLAN-DG               01.77
                                               WS-PLAN-QUAL-CODE-X      01.77
                 ELSE                                                   01.77
                    MOVE 'Y'                TO VU-2821-CPS              01.77
                    MOVE WS-CPS-PSI         TO VU-2821-PSI              01.77
                    MOVE WS-CPS-REIMB       TO VU-2821-RMB              01.77
                    MOVE VU-2821-PLN        TO WS-PLAN-DG               01.77
                                               WS-PLAN-QUAL-CODE-X      01.77
                 END-IF                                                 01.77
              ELSE                                                      01.77
                 IF WS-CPS-PSI = SPACE                                  01.77
                    MOVE 'N'                TO VU-2321-CPS              01.77
                    MOVE SPACE              TO VU-2321-PSI              01.77
                    MOVE 'J'                TO VU-2321-RMB              01.77
                 ELSE                                                   01.77
                    MOVE 'Y'                TO VU-2321-CPS              01.77
                    MOVE WS-CPS-PSI         TO VU-2321-PSI              01.77
                    MOVE WS-CPS-REIMB       TO VU-2321-RMB              01.77
                 END-IF                                                 01.77
                 MOVE VU-2321-PLN           TO WS-PLAN-DG               01.77
                                               WS-PLAN-QUAL-CODE-X      01.77
              END-IF                                                    01.77
           END-IF.
      *
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
      ** PURCHASE AND CORP NON-CPS LEVEL 2 TRANS CAN ONLY QUALIFY FOR   01.88B
      ** EIRF.  STANDARD IS INVALID. SET KDA-VS-EIRF-TO-STD-ERR FLAG    02.09
      ** TO CHECK FOR STANDARD QUAL IN I-300-CHECK-ELECT-ONLY           02.09
           IF NOT WS-RATE-IS-LOWER
              IF WS-STANDARD-AMT = WS-LOW-RATE-AMT AND                  01.88B
                 WS-USE-COMM-ELECT-ONLY                                 01.88B
                   NEXT SENTENCE                                        02.09
              ELSE                                                      02.09
                GO TO I-300-EXIT                                        02.09
              END-IF                                                    02.09
            END-IF.                                                     02.09
      *
           IF WS-CPS-PSI = SPACE
              IF WS-USE-COMM-ELECT-ONLY                                 02.09
                 MOVE 'C' TO KDA-VS-EIRF-TO-STD-ERR                     02.09
              END-IF                                                    02.09
              GO TO I-300-CHECK-ELECT-COMMON
           ELSE
              GO TO I-300-SET-PLAN-DATA.
      *
       I-300-SET-ELECTRONIC-CONS.
      *
           IF KDA-VS-PID-IS-TRD-REWARD                                  01.84
              MOVE VU-0701-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE 'J'                   TO VU-2101-RMB                 01.57
              MOVE 'N'                   TO VU-2101-CPS                 01.57
              MOVE SPACE                 TO VU-2101-PSI                 01.57
              MOVE VU-2101-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
           IF KDA-CHD-AR-IS-FND-SRC-PREPAID                             01.84
              IF KDA-MRCH-TYP-FUEL                                      01.64
                 MOVE VU-2732-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE
                 MOVE VU-2132-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF
           ELSE
              IF KDA-MRCH-TYP-FUEL                                      01.84
                 MOVE 'J'                TO VU-2731-RMB                 01.84
                 MOVE 'N'                TO VU-2731-CPS                 01.84
                 MOVE SPACE              TO VU-2731-PSI                 01.84
                 MOVE VU-2731-PLN        TO WS-PLAN-DG                  01.84
                                            WS-PLAN-QUAL-CODE-X         01.84
              ELSE                                                      01.84
                 MOVE 'J'                TO VU-2131-RMB                 01.84
                 MOVE 'N'                TO VU-2131-CPS                 01.84
                 MOVE SPACE              TO VU-2131-PSI                 01.84
                 MOVE VU-2131-PLN        TO WS-PLAN-DG                  01.84
                                            WS-PLAN-QUAL-CODE-X         01.84
              END-IF                                                    01.47
           END-IF.                                                      01.47
      *
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF NOT WS-RATE-IS-LOWER
              GO TO I-300-EXIT.
      *
       I-300-CHECK-ELECT-COMMON.
      *
           IF KDA-AUTH-ERR
              IF PET-AUTH-CD = SPACES
                 MOVE 'T001'             TO WS-ERROR-DG
              ELSE
                 MOVE 'T003'             TO WS-ERROR-DG
              END-IF
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-300-CHECK-ELECT-ONLY.                             02.09
           IF PET-AUTH-CD(2:5) = '0000Y'
              MOVE 'T003'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-300-CHECK-ELECT-ONLY.                             02.09
           IF NOT KDA-VC-AUTH-SRCE-ELECT                                01.88A
              MOVE 'V005'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-300-CHECK-ELECT-ONLY.                             02.09
           IF NOT KDA-VC-POS-TERM-PS2
              MOVE 'V003'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-300-CHECK-ELECT-ONLY.                             02.09
           IF NOT KDA-VC-CARDHLDR-ID-1-4
              MOVE 'V006'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-300-CHECK-ELECT-ONLY.                             02.09
           IF PET-TMP-MERCH-ZIP = ZEROS
              MOVE 'V007'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-300-CHECK-ELECT-ONLY.                             02.09
           IF PET-TMP-CARD-ACCEPTOR = SPACES OR ZEROS
              MOVE 'V008'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-300-CHECK-ELECT-ONLY.                             02.09
           IF (KDA-MERCAT-AIR OR
               KDA-MRCH-TYP-PSNGR-RAIL)
              IF KDA-HAS-AI
              OR KDA-HAS-AN                                             01.98
                 IF KDA-NOT-PQ-VS-USA-EIRF
                    MOVE 'A002'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                    GO TO I-300-CHECK-ELECT-ONLY                        02.09
                 ELSE
                    NEXT SENTENCE
                 END-IF
              ELSE
                 IF KDA-AI-HAD-ERRORS
                    MOVE 'A003'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'A001'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
                 GO TO I-300-CHECK-ELECT-ONLY.                          02.09
      *
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT    OR                               01.98
              WS-IS-INFINITE-QUAL                                       01.98
                 GO TO I-300-SET-PLAN-DATA.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-03-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              MOVE 'T032'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-300-CHECK-ELECT-ONLY.                             02.09
      *
           IF KDA-CHD-AR-IS-REGULATED                                   01.64
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE SPACE              TO WS-CPS-PSI                  01.64
                                            VU-2532-PSI                 01.64
                 MOVE 'J'                TO WS-CPS-REIMB                01.64
                                            VU-2532-RMB                 01.64
                 MOVE VU-2532-PLN        TO WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE SPACE              TO WS-CPS-PSI                  01.64
                                            VU-2531-PSI                 01.64
                 MOVE 'J'                TO WS-CPS-REIMB                01.64
                                            VU-2531-RMB                 01.64
                 MOVE VU-2531-PLN        TO WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *                                                                 01.64
       I-300-SET-PLAN-DATA.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
           GO TO I-300-EXIT.                                            02.09
      *
       I-300-CHECK-ELECT-ONLY.                                          02.09
      ** PURCHASE AND CORP NON-CPS LEVEL 2 TRANS CAN ONLY QUALIFY FOR   02.09
      ** EIRF -- IF THIS TYPE OF TRAN ENDED UP ONLY QUALIFYING FOR STD  02.09
      ** SET FLAG TO REJECT TRANSACTION IN BAC00110                     02.09
         IF WS-USE-COMM-ELECT-ONLY AND                                  02.09
            KDA-VS-EIRF-TO-STD-ERR = 'C' AND                            02.09
            WS-STANDARD-AMT = WS-LOW-RATE-AMT                           02.09
              MOVE 'Y' TO KDA-VS-EIRF-TO-STD-ERR                        02.09
         END-IF.                                                        02.09
      *                                                                 02.09
       I-300-EXIT.
           EXIT.
      *                                                                 01.48
       I-305-GSA-PURCHASING.                                            01.48
      *                                                                 01.48
      *    6721 VISA USA ELECTRONIC INTCHNG - GSA G2G - PURCH/FLEET     01.48
      *                                                                 01.48
           IF KDA-MRCH-TYP-G2G AND                                      01.48
             (PET-VISA-MVV NOT = SPACES)                                01.48
                NEXT SENTENCE                                           01.48
           ELSE                                                         01.48
              GO TO I-305-NOT-GSA-G2G                                   01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           IF NOT WS-IS-CPS-REIMB                                       01.48
              MOVE VU-6721-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X         01.48
              PERFORM S-525-CHECK-PLAN-LOW                              01.48
                 THRU S-525-EXIT                                        01.48
              IF WS-RATE-IS-LOWER                                       01.48
                 MOVE 'V020'             TO WS-ERROR-DG                 01.48
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.48
                    THRU S-300-EXIT                                     01.48
              END-IF                                                    01.48
              GO TO I-305-GSA-G2G-NOT-CPS                               01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           MOVE WS-CPS-PSI               TO VU-6721-PSI.                01.57
           MOVE WS-CPS-REIMB             TO VU-6721-RMB.                01.57
           MOVE VU-6721-PLN              TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X.        01.48
      *                                                                 01.48
           GO TO I-305-SET-PLAN-DATA.                                   01.48
      *                                                                 01.48
       I-305-GSA-G2G-NOT-CPS.                                           01.48
      *                                                                 01.48
           IF KDA-HAS-PA AND                                            01.48
              KDA-HAS-PL AND                                            01.48
              (NOT KDA-NOT-PQ-VS-USA-LVL3)                              01.48
                 NEXT SENTENCE                                          01.48
           ELSE                                                         01.48
              MOVE VU-6521-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X         01.48
              PERFORM S-525-CHECK-PLAN-LOW                              01.48
                 THRU S-525-EXIT                                        01.48
              IF WS-RATE-IS-LOWER                                       01.48
                 MOVE 'V023'             TO WS-ERROR-DG                 01.48
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.48
                    THRU S-300-EXIT                                     01.48
              END-IF                                                    01.48
              GO TO I-305-GSA-G2G-NOT-CPS-LVL3                          01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           MOVE VU-6521-PLN              TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X.        01.48
      *                                                                 01.48
           GO TO I-305-SET-PLAN-DATA.                                   01.48
      *                                                                 01.48
       I-305-GSA-G2G-NOT-CPS-LVL3.                                      01.48
      *                                                                 01.48
           IF KDA-HAS-LU AND                                            01.48
             (NOT KDA-NOT-PQ-VS-USA-LVL2)                               01.48
                 NEXT SENTENCE                                          01.48
           ELSE                                                         01.48
                 MOVE VU-2321-PLN        TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X         01.48
                 PERFORM S-525-CHECK-PLAN-LOW                           01.48
                    THRU S-525-EXIT                                     01.48
                 IF WS-RATE-IS-LOWER                                    01.48
                    MOVE 'V022'          TO WS-ERROR-DG                 01.48
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.48
                       THRU S-300-EXIT                                  01.48
                 END-IF                                                 01.48
                 GO TO I-305-EXIT                                       01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           MOVE 'N'                      TO VU-2321-CPS.                01.57
           MOVE SPACE                    TO VU-2321-PSI.                01.57
           MOVE 'J'                      TO VU-2321-RMB.                01.57
           MOVE VU-2321-PLN              TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X.        01.48
      *                                                                 01.48
           GO TO I-305-SET-PLAN-DATA.                                   01.48
      *                                                                 01.48
       I-305-NOT-GSA-G2G.                                               01.48
      *                                                                 01.48
           IF NOT KDA-HAS-FL                                            01.51
              GO TO I-305-GSA-NOT-FLEET                                 01.51
           END-IF.                                                      01.51
           IF KDA-VS-PID-IS-FLT-ANY AND                                 01.84
              KDA-MRCH-TYP-FLEET AND                                    01.48
             (NOT KDA-NOT-PQ-VS-FUEL-TRN)                               01.48
                 NEXT SENTENCE                                          01.48
           ELSE                                                         01.48
              GO TO I-305-GSA-NOT-FLEET                                 01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           IF KDA-NOT-PQ-VS-USA-LVL2 OR                                 01.54
              (NOT KDA-HAS-LU)                                          01.50
              NEXT SENTENCE                                             01.48
           ELSE                                                         01.48
              GO TO I-305-GSA-CHECK-FLT-LVL3                            01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           IF WS-CPS-RTL-FLG = 'Y' OR                                   01.96
              WS-CPS-RTL-SST-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-KEY-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.96
              WS-CPS-AUTO-FUEL-FLG = 'Y'                                01.96
                CONTINUE                                                01.96
           ELSE                                                         01.96
              MOVE VU-6621-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.48
              PERFORM S-525-CHECK-PLAN-LOW                              01.48
                 THRU S-525-EXIT                                        01.48
              IF WS-RATE-IS-LOWER                                       01.48
                 MOVE 'V020'             TO WS-ERROR-DG                 01.48
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.48
                    THRU S-300-EXIT                                     01.48
              END-IF                                                    01.48
              GO TO I-305-EXIT                                          01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           MOVE WS-CPS-PSI               TO VU-6621-PSI.                01.96
           MOVE WS-CPS-REIMB             TO VU-6621-RMB.                01.96
           MOVE 'Y'                      TO VU-6621-CPS.                01.96
           MOVE VU-6621-PLN              TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X.        01.48
      *                                                                 01.48
           GO TO I-305-SET-PLAN-DATA.                                   01.48
      *                                                                 01.48
       I-305-GSA-CHECK-FLT-LVL3.                                        01.48
      *                                                                 01.48
           IF KDA-NOT-PQ-VS-USA-LVL3-FLT OR                             01.50
              (NOT KDA-HAS-PA AND KDA-VS-MIVFL-TYPE-OF-PURC = '3') OR   01.96
              (NOT KDA-HAS-PL AND KDA-VS-MIVFL-TYPE-OF-PURC = '3')      01.96
              NEXT SENTENCE                                             01.48
           ELSE                                                         01.48
              GO TO I-305-GSA-CHECK-LVL3-FLT-CPS                        01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           IF WS-CPS-RTL-FLG = 'Y' OR                                   01.96
              WS-CPS-RTL-SST-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-KEY-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.96
              WS-CPS-AUTO-FUEL-FLG = 'Y' OR                             01.96
              WS-CPS-ACCT-FUND-FLG = 'Y'                                01.96
                CONTINUE                                                01.96
           ELSE                                                         01.96
              MOVE VU-6121-PLN           TO WS-PLAN-DG                  01.59
                                            WS-PLAN-QUAL-CODE-X         01.48
              PERFORM S-525-CHECK-PLAN-LOW                              01.48
                 THRU S-525-EXIT                                        01.48
              IF WS-RATE-IS-LOWER                                       01.48
                 MOVE 'V020'             TO WS-ERROR-DG                 01.48
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.48
                    THRU S-300-EXIT                                     01.48
              END-IF                                                    01.48
              MOVE SPACE                 TO VU-6621-PSI                 01.96
              MOVE '0'                   TO VU-6621-RMB                 01.96
              MOVE 'N'                   TO VU-6621-CPS                 01.96
              MOVE VU-6621-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.48
              GO TO I-305-SET-PLAN-DATA                                 01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           MOVE WS-CPS-PSI               TO VU-6121-PSI.                01.59
           MOVE WS-CPS-REIMB             TO VU-6121-RMB.                01.59
           MOVE VU-6121-PLN              TO WS-PLAN-DG                  01.59
                                            WS-PLAN-QUAL-CODE-X.        01.48
      *                                                                 01.48
           GO TO I-305-SET-PLAN-DATA.                                   01.48
      *                                                                 01.48
       I-305-GSA-CHECK-LVL3-FLT-CPS.                                    01.48
      *                                                                 01.48
           IF WS-CPS-RTL-FLG = 'Y' OR                                   01.96
              WS-CPS-RTL-SST-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-KEY-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.96
              WS-CPS-AUTO-FUEL-FLG = 'Y'                                01.96
                CONTINUE                                                01.96
           ELSE                                                         01.96
              MOVE VU-6321-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X         01.48
              PERFORM S-525-CHECK-PLAN-LOW                              01.48
                 THRU S-525-EXIT                                        01.48
              IF WS-RATE-IS-LOWER                                       01.48
                 MOVE 'V020'             TO WS-ERROR-DG                 01.48
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.48
                    THRU S-300-EXIT                                     01.48
              END-IF                                                    01.48
              MOVE SPACE                 TO VU-6621-PSI                 01.96
              MOVE '0'                   TO VU-6621-RMB                 01.96
              MOVE 'N'                   TO VU-6621-CPS                 01.96
              MOVE VU-6621-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.48
              GO TO I-305-SET-PLAN-DATA                                 01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           MOVE WS-CPS-PSI               TO VU-6321-PSI.                01.57
           MOVE WS-CPS-REIMB             TO VU-6321-RMB.                01.57
           MOVE VU-6321-PLN              TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X.        01.48
      *                                                                 01.48
           GO TO I-305-SET-PLAN-DATA.                                   01.48
      *                                                                 01.48
       I-305-GSA-NOT-FLEET.                                             01.48
      *                                                                 01.48
           IF KDA-HAS-PA AND                                            01.48
              KDA-HAS-PL AND                                            01.48
              KDA-HAS-LU AND                                            01.48
             (NOT KDA-NOT-PQ-VS-USA-LVL3)                               01.48
                 NEXT SENTENCE                                          01.48
           ELSE                                                         01.48
              GO TO I-305-GSA-CHECK-LVL2                                01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           IF NOT WS-IS-CPS-REIMB                                       01.48
              MOVE VU-6321-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X         01.48
              PERFORM S-525-CHECK-PLAN-LOW                              01.48
                 THRU S-525-EXIT                                        01.48
              IF WS-RATE-IS-LOWER                                       01.48
                 MOVE 'V020'             TO WS-ERROR-DG                 01.48
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.48
                    THRU S-300-EXIT                                     01.48
              END-IF                                                    01.48
              MOVE VU-6521-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X         01.48
              GO TO I-305-SET-PLAN-DATA                                 01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           MOVE WS-CPS-PSI               TO VU-6321-PSI.                01.57
           MOVE WS-CPS-REIMB             TO VU-6321-RMB.                01.57
           MOVE VU-6321-PLN              TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X.        01.48
      *                                                                 01.48
           GO TO I-305-SET-PLAN-DATA.                                   01.48
      *                                                                 01.48
       I-305-GSA-CHECK-LVL2.                                            01.48
      *                                                                 01.48
           IF KDA-HAS-LU AND                                            01.48
             (NOT KDA-NOT-PQ-VS-USA-LVL2)                               01.48
                 NEXT SENTENCE                                          01.48
           ELSE                                                         01.48
              GO TO I-305-GSA-NOT-LVL2                                  01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           IF NOT WS-IS-CPS-REIMB                                       01.48
              MOVE VU-6121-PLN           TO WS-PLAN-DG                  01.59
                                            WS-PLAN-QUAL-CODE-X         01.48
              PERFORM S-525-CHECK-PLAN-LOW                              01.48
                 THRU S-525-EXIT                                        01.48
              IF WS-RATE-IS-LOWER                                       01.48
                 MOVE 'V020'             TO WS-ERROR-DG                 01.48
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.48
                    THRU S-300-EXIT                                     01.48
              END-IF                                                    01.48
              MOVE 'N'                   TO VU-2321-CPS                 01.57
              MOVE SPACE                 TO VU-2321-PSI                 01.57
              MOVE 'J'                   TO VU-2321-RMB                 01.57
              MOVE VU-2321-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X         01.48
              GO TO I-305-SET-PLAN-DATA                                 01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           MOVE WS-CPS-PSI               TO VU-6121-PSI.                01.59
           MOVE WS-CPS-REIMB             TO VU-6121-RMB.                01.59
           MOVE VU-6121-PLN              TO WS-PLAN-DG                  01.59
                                            WS-PLAN-QUAL-CODE-X.        01.48
      *                                                                 01.48
           GO TO I-305-SET-PLAN-DATA.                                   01.48
      *                                                                 01.48
       I-305-GSA-NOT-LVL2.                                              01.48
      *                                                                 01.48
           IF KDA-MRCH-TYP-B2B                                          01.96
              IF NOT WS-IS-CPS-REIMB                                    01.96
                 MOVE VU-3621-PLN        TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
                 PERFORM S-525-CHECK-PLAN-LOW                           01.96
                    THRU S-525-EXIT                                     01.96
                 IF WS-RATE-IS-LOWER                                    01.96
                    MOVE 'V020'             TO WS-ERROR-DG              01.96
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.96
                       THRU S-300-EXIT                                  01.96
                 END-IF                                                 01.96
                 GO TO I-305-EXIT                                       01.96
              ELSE                                                      01.96
                 MOVE WS-CPS-PSI         TO VU-3621-PSI                 01.96
                 MOVE WS-CPS-REIMB       TO VU-3621-RMB                 01.96
                 MOVE VU-3621-PLN        TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
                 GO TO I-305-SET-PLAN-DATA                              01.96
              END-IF                                                    01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           IF WS-CPS-RTL-FLG = 'Y' OR                                   01.96
              WS-CPS-SPR-MKT-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-KEY-FLG = 'Y' OR                               01.96
              WS-CPS-SML-TKT-FLG = 'Y' OR                               01.96
              WS-CPS-AUTO-FUEL-FLG = 'Y' OR                             01.96
              WS-CPS-RTL-SST-FLG = 'Y'                                  01.96
                 MOVE WS-CPS-PSI         TO VU-3821-PSI                 01.96
                 MOVE WS-CPS-REIMB       TO VU-3821-RMB                 01.96
                 MOVE VU-3821-PLN        TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
                 GO TO I-305-SET-PLAN-DATA                              01.96
           ELSE                                                         01.96
           IF WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.96
              WS-CPS-ECOM-PREF-FLG = 'Y' OR                             01.96
              WS-CPS-ECOM-BASIC-FLG = 'Y' OR                            01.96
              WS-CPS-ACCT-FUND-FLG = 'Y'                                01.96
                 MOVE WS-CPS-PSI         TO VU-3721-PSI                 01.96
                 MOVE WS-CPS-REIMB       TO VU-3721-RMB                 01.96
                 MOVE VU-3721-PLN        TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
                 GO TO I-305-SET-PLAN-DATA                              01.96
           ELSE                                                         01.96
           IF WS-CPS-RTL-FLG = 'A' OR                                   01.96
              WS-CPS-SPR-MKT-FLG = 'A' OR                               01.96
              WS-CPS-RTL-KEY-FLG = 'A' OR                               01.96
              WS-CPS-SML-TKT-FLG = 'A' OR                               01.96
              WS-CPS-AUTO-FUEL-FLG = 'A' OR                             01.96
              WS-CPS-RTL-SST-FLG = 'A'                                  01.96
                 MOVE VU-3821-PLN  TO WS-PLAN-DG                        01.96
                                      WS-PLAN-QUAL-CODE-X               01.96
                 PERFORM S-525-CHECK-PLAN-LOW                           01.96
                    THRU S-525-EXIT                                     01.96
                 IF WS-RATE-IS-LOWER                                    01.96
                    MOVE 'V020'             TO WS-ERROR-DG              01.96
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.96
                       THRU S-300-EXIT                                  01.96
                 END-IF                                                 01.96
           ELSE                                                         01.96
           IF WS-CPS-RTL-CNP-FLG = 'A' OR                               01.96
              WS-CPS-ECOM-PREF-FLG = 'A' OR                             01.96
              WS-CPS-ECOM-BASIC-FLG = 'A' OR                            01.96
              WS-CPS-ACCT-FUND-FLG = 'A'                                01.96
                 MOVE VU-3721-PLN  TO WS-PLAN-DG                        01.96
                                      WS-PLAN-QUAL-CODE-X               01.96
                 PERFORM S-525-CHECK-PLAN-LOW                           01.96
                    THRU S-525-EXIT                                     01.96
                 IF WS-RATE-IS-LOWER                                    01.96
                    MOVE 'V020'             TO WS-ERROR-DG              01.96
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.96
                       THRU S-300-EXIT                                  01.96
                 END-IF                                                 01.96
           END-IF                                                       01.96
           END-IF                                                       01.96
           END-IF                                                       01.96
           END-IF.                                                      01.96
      *                                                                 01.48
           GO TO I-305-EXIT.                                            01.48
      *                                                                 01.48
       I-305-SET-PLAN-DATA.                                             01.48
      *                                                                 01.48
           PERFORM S-500-SET-PLAN-DATA                                  01.48
              THRU S-500-EXIT.                                          01.48
      *                                                                 01.48
       I-305-EXIT.                                                      01.48
           EXIT.                                                        01.48
      *
       I-307-FLEET-PURCHASING.                                          01.96
      *                                                                 01.96
           IF NOT KDA-HAS-FL                                            01.96
              GO TO I-307-NOT-FLEET                                     01.96
           END-IF.                                                      01.96
           IF KDA-MRCH-TYP-FLEET AND                                    01.96
             (NOT KDA-NOT-PQ-VS-FUEL-TRN)                               01.96
                 NEXT SENTENCE                                          01.96
           ELSE                                                         01.96
              GO TO I-307-NOT-FLEET                                     01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           IF KDA-NOT-PQ-VS-USA-LVL2 OR                                 01.96
              (NOT KDA-HAS-LU)                                          01.96
              NEXT SENTENCE                                             01.96
           ELSE                                                         01.96
              GO TO I-307-CHECK-FLT-LVL3                                01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           IF WS-CPS-RTL-FLG = 'Y' OR                                   01.96
              WS-CPS-RTL-SST-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-KEY-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.96
              WS-CPS-AUTO-FUEL-FLG = 'Y'                                01.96
                CONTINUE                                                01.96
           ELSE                                                         01.96
              MOVE VU-6621-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
              PERFORM S-525-CHECK-PLAN-LOW                              01.96
                 THRU S-525-EXIT                                        01.96
              IF WS-RATE-IS-LOWER                                       01.96
                 MOVE 'V020'             TO WS-ERROR-DG                 01.96
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.96
                    THRU S-300-EXIT                                     01.96
              END-IF                                                    01.96
              GO TO I-307-EXIT                                          01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           MOVE VU-6121-PLN              TO WS-PLAN-DG                  02.01
                                         WS-PLAN-QUAL-CODE-X.           02.01
           PERFORM S-525-CHECK-PLAN-LOW                                 02.01
              THRU S-525-EXIT.                                          02.01
           IF WS-RATE-IS-LOWER                                          02.01
              MOVE 'V022'                TO WS-ERROR-DG                 02.01
              PERFORM S-300-PROCESS-DOWNGRADE                           02.01
                 THRU S-300-EXIT                                        02.01
           END-IF.                                                      02.01
      *                                                                 02.01
           MOVE WS-CPS-PSI               TO VU-6621-PSI.                01.96
           MOVE WS-CPS-REIMB             TO VU-6621-RMB.                01.96
           MOVE 'Y'                      TO VU-6621-CPS.                01.96
           MOVE VU-6621-PLN              TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X.        01.96
      *                                                                 01.96
           GO TO I-307-SET-PLAN-DATA.                                   01.96
      *                                                                 01.96
       I-307-CHECK-FLT-LVL3.                                            01.96
      *                                                                 01.96
           IF KDA-NOT-PQ-VS-USA-LVL3-FLT OR                             01.96
              (NOT KDA-HAS-PA AND KDA-VS-MIVFL-TYPE-OF-PURC = '3') OR   01.96
              (NOT KDA-HAS-PL AND KDA-VS-MIVFL-TYPE-OF-PURC = '3')      01.96
              NEXT SENTENCE                                             01.96
           ELSE                                                         01.96
              GO TO I-307-CHECK-LVL3-FLT-CPS                            01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           IF WS-CPS-RTL-FLG = 'Y' OR                                   01.96
              WS-CPS-RTL-SST-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-KEY-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.96
              WS-CPS-AUTO-FUEL-FLG = 'Y' OR                             01.96
              WS-CPS-ACCT-FUND-FLG = 'Y'                                01.96
                CONTINUE                                                01.96
           ELSE                                                         01.96
              MOVE VU-6121-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
              PERFORM S-525-CHECK-PLAN-LOW                              01.96
                 THRU S-525-EXIT                                        01.96
              IF WS-RATE-IS-LOWER                                       01.96
                 MOVE 'V020'             TO WS-ERROR-DG                 01.96
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.96
                    THRU S-300-EXIT                                     01.96
              END-IF                                                    01.96
              MOVE SPACES                TO VU-6621-PSI                 01.96
              MOVE '0'                   TO VU-6621-RMB                 01.96
              MOVE 'N'                   TO VU-6621-CPS                 01.96
              MOVE VU-6621-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
              GO TO I-307-SET-PLAN-DATA                                 01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           MOVE WS-CPS-PSI               TO VU-6121-PSI.                01.96
           MOVE WS-CPS-REIMB             TO VU-6121-RMB.                01.96
           MOVE VU-6121-PLN              TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X.        01.96
      *                                                                 01.96
           GO TO I-307-SET-PLAN-DATA.                                   01.96
      *                                                                 01.96
       I-307-CHECK-LVL3-FLT-CPS.                                        01.96
      *                                                                 01.96
           IF WS-CPS-RTL-FLG = 'Y' OR                                   01.96
              WS-CPS-RTL-SST-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-KEY-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.96
              WS-CPS-AUTO-FUEL-FLG = 'Y'                                01.96
                CONTINUE                                                01.96
           ELSE                                                         01.96
              MOVE VU-6321-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
              PERFORM S-525-CHECK-PLAN-LOW                              01.96
                 THRU S-525-EXIT                                        01.96
              IF WS-RATE-IS-LOWER                                       01.96
                 MOVE 'V020'             TO WS-ERROR-DG                 01.96
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.96
                    THRU S-300-EXIT                                     01.96
              END-IF                                                    01.96
              MOVE SPACES                TO VU-6621-PSI                 01.96
              MOVE '0'                   TO VU-6621-RMB                 01.96
              MOVE 'N'                   TO VU-6621-CPS                 01.96
              MOVE VU-6621-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
              GO TO I-307-SET-PLAN-DATA                                 01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           MOVE WS-CPS-PSI               TO VU-6321-PSI.                01.96
           MOVE WS-CPS-REIMB             TO VU-6321-RMB.                01.96
           MOVE VU-6321-PLN              TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X.        01.96
      *                                                                 01.96
           GO TO I-307-SET-PLAN-DATA.                                   01.96
      *                                                                 01.96
       I-307-NOT-FLEET.                                                 01.96
      *                                                                 01.96
           IF KDA-HAS-PA AND                                            01.96
              KDA-HAS-PL AND                                            01.96
              KDA-HAS-LU AND                                            01.96
             (NOT KDA-NOT-PQ-VS-USA-LVL3)                               01.96
                 NEXT SENTENCE                                          01.96
           ELSE                                                         01.96
              GO TO I-307-CHECK-LVL2                                    01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           IF NOT WS-IS-CPS-REIMB                                       01.96
              MOVE VU-6321-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
              PERFORM S-525-CHECK-PLAN-LOW                              01.96
                 THRU S-525-EXIT                                        01.96
              IF WS-RATE-IS-LOWER                                       01.96
                 MOVE 'V020'             TO WS-ERROR-DG                 01.96
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.96
                    THRU S-300-EXIT                                     01.96
              END-IF                                                    01.96
              MOVE VU-6521-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
              GO TO I-307-SET-PLAN-DATA                                 01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           MOVE WS-CPS-PSI               TO VU-6321-PSI.                01.96
           MOVE WS-CPS-REIMB             TO VU-6321-RMB.                01.96
           MOVE VU-6321-PLN              TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X.        01.96
      *                                                                 01.96
           GO TO I-307-SET-PLAN-DATA.                                   01.96
      *                                                                 01.96
       I-307-CHECK-LVL2.                                                01.96
      *                                                                 01.96
           IF KDA-HAS-LU AND                                            01.96
             (NOT KDA-NOT-PQ-VS-USA-LVL2)                               01.96
                 NEXT SENTENCE                                          01.96
           ELSE                                                         01.96
              GO TO I-307-NOT-LVL2                                      01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           IF NOT WS-IS-CPS-REIMB                                       01.96
              MOVE VU-6121-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
              PERFORM S-525-CHECK-PLAN-LOW                              01.96
                 THRU S-525-EXIT                                        01.96
              IF WS-RATE-IS-LOWER                                       01.96
                 MOVE 'V020'             TO WS-ERROR-DG                 01.96
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.96
                    THRU S-300-EXIT                                     01.96
              END-IF                                                    01.96
              MOVE 'N'                   TO VU-2321-CPS                 01.96
              MOVE SPACE                 TO VU-2321-PSI                 01.96
              MOVE 'J'                   TO VU-2321-RMB                 01.96
              MOVE VU-2321-PLN           TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
              GO TO I-307-SET-PLAN-DATA                                 01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           MOVE WS-CPS-PSI               TO VU-6121-PSI.                01.96
           MOVE WS-CPS-REIMB             TO VU-6121-RMB.                01.96
           MOVE VU-6121-PLN              TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X.        01.96
      *                                                                 01.96
           GO TO I-307-SET-PLAN-DATA.                                   01.96
      *                                                                 01.96
       I-307-NOT-LVL2.                                                  01.96
      *                                                                 01.96
           IF WS-IS-CPS-REIMB                                           02.01
              MOVE VU-6121-PLN           TO WS-PLAN-DG                  02.01
                                         WS-PLAN-QUAL-CODE-X            02.01
              PERFORM S-525-CHECK-PLAN-LOW                              02.01
                 THRU S-525-EXIT                                        02.01
              IF WS-RATE-IS-LOWER                                       02.01
                 MOVE 'V022'             TO WS-ERROR-DG                 02.01
                 PERFORM S-300-PROCESS-DOWNGRADE                        02.01
                    THRU S-300-EXIT                                     02.01
              END-IF                                                    02.01
           END-IF.                                                      02.01
      *                                                                 02.01
           IF KDA-MRCH-TYP-B2B                                          01.96
              IF NOT WS-IS-CPS-REIMB                                    01.96
                 MOVE VU-3621-PLN        TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
                 PERFORM S-525-CHECK-PLAN-LOW                           01.96
                    THRU S-525-EXIT                                     01.96
                 IF WS-RATE-IS-LOWER                                    01.96
                    MOVE 'V020'             TO WS-ERROR-DG              01.96
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.96
                       THRU S-300-EXIT                                  01.96
                 END-IF                                                 01.96
                 GO TO I-307-EXIT                                       01.96
              ELSE                                                      01.96
                 MOVE WS-CPS-PSI         TO VU-3621-PSI                 01.96
                 MOVE WS-CPS-REIMB       TO VU-3621-RMB                 01.96
                 MOVE VU-3621-PLN        TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
                 GO TO I-307-SET-PLAN-DATA                              01.96
              END-IF                                                    01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           IF WS-CPS-RTL-FLG = 'Y' OR                                   01.96
              WS-CPS-SPR-MKT-FLG = 'Y' OR                               01.96
              WS-CPS-RTL-KEY-FLG = 'Y' OR                               01.96
              WS-CPS-SML-TKT-FLG = 'Y' OR                               01.96
              WS-CPS-AUTO-FUEL-FLG = 'Y' OR                             01.96
              WS-CPS-RTL-SST-FLG = 'Y'                                  01.96
                 MOVE WS-CPS-PSI         TO VU-3821-PSI                 01.96
                 MOVE WS-CPS-REIMB       TO VU-3821-RMB                 01.96
                 MOVE VU-3821-PLN        TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
                 GO TO I-307-SET-PLAN-DATA                              01.96
           ELSE                                                         01.96
           IF WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.96
              WS-CPS-ECOM-PREF-FLG = 'Y' OR                             01.96
              WS-CPS-ECOM-BASIC-FLG = 'Y' OR                            01.96
              WS-CPS-ACCT-FUND-FLG = 'Y'                                01.96
                 MOVE WS-CPS-PSI         TO VU-3721-PSI                 01.96
                 MOVE WS-CPS-REIMB       TO VU-3721-RMB                 01.96
                 MOVE VU-3721-PLN        TO WS-PLAN-DG                  01.96
                                            WS-PLAN-QUAL-CODE-X         01.96
                 GO TO I-307-SET-PLAN-DATA                              01.96
           ELSE                                                         01.96
           IF WS-CPS-RTL-FLG = 'A' OR                                   01.96
              WS-CPS-SPR-MKT-FLG = 'A' OR                               01.96
              WS-CPS-RTL-KEY-FLG = 'A' OR                               01.96
              WS-CPS-SML-TKT-FLG = 'A' OR                               01.96
              WS-CPS-AUTO-FUEL-FLG = 'A' OR                             01.96
              WS-CPS-RTL-SST-FLG = 'A'                                  01.96
                 MOVE VU-3821-PLN  TO WS-PLAN-DG                        01.96
                                      WS-PLAN-QUAL-CODE-X               01.96
                 PERFORM S-525-CHECK-PLAN-LOW                           01.96
                    THRU S-525-EXIT                                     01.96
                 IF WS-RATE-IS-LOWER                                    01.96
                    MOVE 'V020'             TO WS-ERROR-DG              01.96
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.96
                       THRU S-300-EXIT                                  01.96
                 END-IF                                                 01.96
           ELSE                                                         01.96
           IF WS-CPS-RTL-CNP-FLG = 'A' OR                               01.96
              WS-CPS-ECOM-PREF-FLG = 'A' OR                             01.96
              WS-CPS-ECOM-BASIC-FLG = 'A' OR                            01.96
              WS-CPS-ACCT-FUND-FLG = 'A'                                01.96
                 MOVE VU-3721-PLN  TO WS-PLAN-DG                        01.96
                                      WS-PLAN-QUAL-CODE-X               01.96
                 PERFORM S-525-CHECK-PLAN-LOW                           01.96
                    THRU S-525-EXIT                                     01.96
                 IF WS-RATE-IS-LOWER                                    01.96
                    MOVE 'V020'             TO WS-ERROR-DG              01.96
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.96
                       THRU S-300-EXIT                                  01.96
                 END-IF                                                 01.96
           END-IF                                                       01.96
           END-IF                                                       01.96
           END-IF                                                       01.96
           END-IF.                                                      01.96
      *                                                                 01.96
           GO TO I-307-EXIT.                                            01.96
      *                                                                 01.96
       I-307-SET-PLAN-DATA.                                             01.96
      *                                                                 01.96
           PERFORM S-500-SET-PLAN-DATA                                  01.96
              THRU S-500-EXIT.                                          01.96
      *                                                                 01.96
       I-307-EXIT.                                                      01.96
           EXIT.                                                        01.96
      *                                                                 01.96
       I-400-SET-CPS.
      *
      * SET RATE TABLE VALUES FOR USA CPS BASED ON THE
      * VALUE OF THE AUTH CHAR INDICATOR
      *
           IF NOT WS-IS-CPS
              MOVE 'V004'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-400-EXIT.
      *
           IF KDA-VC-AUTH-CHAR-N
              MOVE 'V021'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-400-EXIT.
      *
           IF NOT KDA-VC-AUTH-CHAR-IND
              GO TO I-400-EXIT.
      *
           IF KDA-VC-AUTH-CHAR-SM-TKT                                   01.54
              PERFORM I-455-SET-CPS-SMALL-TKT                           01.54
                 THRU I-455-EXIT.                                       01.54
           IF WS-CPS-SML-TKT-FLG = 'Y' AND                              01.65
              KDA-CHD-AR-IS-REGULATED AND                               01.65
              WS-IS-CPS-REGULATED                                       01.65
                 GO TO I-400-EXIT.                                      01.65
           IF KDA-VC-AUTH-CHAR-PT-CNP                                   01.77
              PERFORM I-405-SET-CPS-PASS-TRANS-CNP                      01.77
                 THRU I-405-EXIT.                                       01.77
           IF KDA-VC-AUTH-CHAR-PT-CP                                    01.77
              PERFORM I-406-SET-CPS-PASS-TRANS-CP                       01.77
                 THRU I-406-EXIT.                                       01.77
           IF KDA-VC-AUTH-CHAR-HOT-CNP
              PERFORM I-410-SET-CPS-HOTEL-NO-CRD
                 THRU I-410-EXIT.
           IF KDA-VC-AUTH-CHAR-HOT-PRS
              PERFORM I-415-SET-CPS-HOTEL-CRD
                 THRU I-415-EXIT.
           IF KDA-VC-AUTH-CHAR-CAR-CNP
              PERFORM I-420-SET-CPS-CAR-NO-CRD
                 THRU I-420-EXIT.
           IF KDA-VC-AUTH-CHAR-CAR-PRS
              PERFORM I-425-SET-CPS-CAR-CRD
                 THRU I-425-EXIT.
           IF KDA-VC-AUTH-CHAR-CNP
              PERFORM I-430-SET-CPS-CARD-NOT-PRESENT
                 THRU I-430-EXIT.
           IF KDA-VC-AUTH-CHAR-AFD
              PERFORM I-435-SET-CPS-AUTO-FUEL
                 THRU I-435-EXIT.
           IF KDA-VC-AUTH-CHAR-RET
              PERFORM I-440-SET-CPS-RETAIL
                 THRU I-440-EXIT.
           IF KDA-VC-AUTH-CHAR-RST
              PERFORM I-445-SET-CPS-RESTAURANT
                 THRU I-445-EXIT.
           IF KDA-VC-AUTH-CHAR-KEY
              PERFORM I-450-SET-CPS-RETAIL-KEYED
                 THRU I-450-EXIT.
           IF KDA-VC-AUTH-CHAR-ECB
              PERFORM I-460-SET-CPS-E-COMM-BASIC
                 THRU I-460-EXIT.
           IF KDA-VC-AUTH-CHAR-ACT
              PERFORM I-465-SET-CPS-ACCT-FUND
                 THRU I-465-EXIT.
           IF KDA-VC-AUTH-CHAR-RSS
              PERFORM I-470-SET-CPS-SVC-STATION
                 THRU I-470-EXIT.
           IF KDA-VC-AUTH-CHAR-ECP
              PERFORM I-475-SET-CPS-E-COMM-PREF
                 THRU I-475-EXIT.
           IF KDA-VC-AUTH-CHAR-PT-ECP
              PERFORM I-480-SET-CPS-E-COMM-PREF-PT
                 THRU I-480-EXIT.
           IF KDA-VC-AUTH-CHAR-HOT-ECP
              PERFORM I-485-SET-CPS-E-COMM-PREF-H
                 THRU I-485-EXIT.
           IF KDA-VC-AUTH-CHAR-CAR-ECP
              PERFORM I-490-SET-CPS-E-COMM-PREF-C
                 THRU I-490-EXIT.
           IF KDA-VC-AUTH-CHAR-RECUR                                    02.07
              PERFORM I-495-SET-CPS-RECUR-PAY                           02.07
                 THRU I-495-EXIT.                                       02.07
      *
       I-400-EXIT.
           EXIT.
      *
       I-405-SET-CPS-PASS-TRANS-CNP.                                    01.77
      *
      * SET RATE TABLE VALUES FOR USA CPS PASSENGER TRANSPORT.
      *
      *    4301 VISA USA CPS PASSENGER TRANSPORT CNP - CREDIT           01.77
      *    4331 VISA USA CPS PASSENGER TRANSPORT CNP - DEBIT            01.77
      *    4332 VISA USA CPS PASSENGER TRANSPORT CNP - PREPAID          01.77
      *
           IF KDA-MERCAT-AIR OR
              KDA-MRCH-TYP-PSNGR-RAIL
                 NEXT SENTENCE
           ELSE
              GO TO I-405-EXIT.
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-405-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              KDA-VS-PID-IS-SIGNATURE OR                                01.98
              KDA-VS-PID-IS-INFINITE                                    01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4301-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4332-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-4331-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-405-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-PT-CNP-FLG.          01.77
      *
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-405-EXIT.
           IF KDA-CHD-AR-IS-TOKEN                                       02.07
              IF KDA-VC-PHONE-ORD-5                                     02.07
                    NEXT SENTENCE                                       02.07
              ELSE                                                      02.07
                 IF NOT WS-CHECK-QUAL-ONLY                              02.07
                    MOVE 'V002'          TO WS-ERROR-DG                 02.07
                    PERFORM S-300-PROCESS-DOWNGRADE                     02.07
                       THRU S-300-EXIT                                  02.07
                 END-IF                                                 02.07
                 GO TO I-405-EXIT.                                      02.07
           IF KDA-VC-POS-ENTRY-KEYED
                 NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-405-EXIT.
           IF NOT KDA-HAS-AI AND                                        01.98
              NOT KDA-HAS-AN                                            01.98
              IF NOT WS-CHECK-QUAL-ONLY
                 IF KDA-AI-HAD-ERRORS
                    MOVE 'A003'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'A001'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-405-EXIT.
           IF KDA-NOT-PQ-VS-USA-PT
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'A002'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-405-EXIT.
           IF PET-DESCR-FLAG NOT = 'Y'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T034'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-405-EXIT.
      *
           IF KDA-VC-CARDHLDR-ID-SPACE OR
              KDA-VC-CARDHLDR-ID-4
                 NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-405-EXIT.
      *
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
            ((DC-RESULT = 01 OR 02) AND                                 01.56
              DC-RETURN-NUM > 1)                                        01.56
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-405-EXIT                                          01.56
           END-IF.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-08-DAYS-CUTOFF-AIR   TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'                TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-405-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-REGULATED-FLG        01.89
                                            WS-CPS-PT-CNP-FLG           01.77
                                            WS-CPS-REWARDS-SW
                                            WS-CPS-REIMB-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4301-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-4301-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4332-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4332-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-4331-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4331-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-PT-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-PT-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-405-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-405-EXIT.
           EXIT.
      *
       I-406-SET-CPS-PASS-TRANS-CP.                                     01.77
      *                                                                 01.77
      * SET RATE TABLE VALUES FOR USA CPS PASSENGER TRANSPORT.          01.77
      *                                                                 01.77
      *    2001 VISA USA CPS PASSENGER TRANSPORT CP - CREDIT            01.77
      *    2231 VISA USA CPS PASSENGER TRANSPORT CP - DEBIT             01.77
      *    3131 VISA USA CPS PASSENGER TRANSPORT CP - PREPAID           01.89
      *                                                                 01.77
           IF KDA-MERCAT-AIR OR                                         01.77
              KDA-MRCH-TYP-PSNGR-RAIL                                   01.77
                 NEXT SENTENCE                                          01.77
           ELSE                                                         01.77
              GO TO I-406-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.77
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.77
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR                                       01.77
              WS-IS-GOV-HE-PAY OR                                       01.86
              KDA-VS-PID-IS-SIGNATURE OR                                01.98
              KDA-VS-PID-IS-INFINITE                                    01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW       01.77
           ELSE                                                         01.77
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.      01.77
      *                                                                 01.77
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-2001-PLN           TO WS-PLAN-DG                  01.77
                                            WS-PLAN-QUAL-CODE-X         01.77
           ELSE                                                         01.77
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-2232-PLN        TO WS-PLAN-DG                  01.77
                                            WS-PLAN-QUAL-CODE-X         01.77
              ELSE                                                      01.77
                 MOVE VU-3131-PLN        TO WS-PLAN-DG                  01.77
                                            WS-PLAN-QUAL-CODE-X         01.77
              END-IF                                                    01.77
           END-IF.                                                      01.77
      *                                                                 01.77
           IF NOT WS-CHECK-QUAL-ONLY                                    01.77
              PERFORM S-525-CHECK-PLAN-LOW                              01.77
                 THRU S-525-EXIT                                        01.77
              IF NOT WS-RATE-IS-LOWER                                   01.77
                 GO TO I-406-EXIT.                                      01.77
      *                                                                 01.77
           MOVE 'A'                      TO WS-CPS-PT-CP-FLG.           01.77
      *                                                                 01.77
           IF KDA-AUTH-ERR                                              01.77
              IF NOT WS-CHECK-QUAL-ONLY                                 01.77
                 IF PET-AUTH-CD = SPACES                                01.77
                    MOVE 'T001'          TO WS-ERROR-DG                 01.77
                 ELSE                                                   01.77
                    MOVE 'T003'          TO WS-ERROR-DG                 01.77
                 END-IF                                                 01.77
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.77
                    THRU S-300-EXIT                                     01.77
              END-IF                                                    01.77
              GO TO I-406-EXIT.                                         01.77
           IF KDA-VC-POS-ENTRY-MODE-CPS OR                              01.89
              KDA-VC-POS-ENTRY-KEYED                                    01.89
                 NEXT SENTENCE                                          01.77
           ELSE                                                         01.77
              IF NOT WS-CHECK-QUAL-ONLY                                 01.77
                 MOVE 'V001'             TO WS-ERROR-DG                 01.77
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.77
                    THRU S-300-EXIT                                     01.77
              END-IF                                                    01.77
              GO TO I-406-EXIT.                                         01.77
           IF NOT KDA-HAS-AI AND                                        01.98
              NOT KDA-HAS-AN                                            01.98
              IF NOT WS-CHECK-QUAL-ONLY                                 01.77
                 IF KDA-AI-HAD-ERRORS                                   01.77
                    MOVE 'A003'          TO WS-ERROR-DG                 01.77
                 ELSE                                                   01.77
                    MOVE 'A001'          TO WS-ERROR-DG                 01.77
                 END-IF                                                 01.77
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.77
                    THRU S-300-EXIT                                     01.77
              END-IF                                                    01.77
              GO TO I-406-EXIT.                                         01.77
           IF KDA-NOT-PQ-VS-USA-PT                                      01.77
              IF NOT WS-CHECK-QUAL-ONLY                                 01.77
                 MOVE 'A002'             TO WS-ERROR-DG                 01.77
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.77
                    THRU S-300-EXIT                                     01.77
              END-IF                                                    01.77
              GO TO I-406-EXIT.                                         01.77
           IF PET-DESCR-FLAG NOT = 'Y'                                  01.77
              IF NOT WS-CHECK-QUAL-ONLY                                 01.77
                 MOVE 'T034'             TO WS-ERROR-DG                 01.77
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.77
                    THRU S-300-EXIT                                     01.77
              END-IF                                                    01.77
              GO TO I-406-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VC-CARDHLDR-ID-SPACE OR                               01.77
              KDA-VC-CARDHLDR-ID-1-3                                    01.77
                 NEXT SENTENCE                                          01.77
           ELSE                                                         01.77
              IF NOT WS-CHECK-QUAL-ONLY                                 01.77
                 MOVE 'V006'             TO WS-ERROR-DG                 01.77
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.77
                    THRU S-300-EXIT                                     01.77
              END-IF                                                    01.77
              GO TO I-406-EXIT.                                         01.77
      *                                                                 01.77
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.                01.77
           MOVE PET-DATE                 TO DC-SEND-DATE.               01.77
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.                01.77
           MOVE 'DAK'                    TO DC-OPERATION.               01.77
           PERFORM D-100-CALL-DATECONV                                  01.77
              THRU D-100-EXIT.                                          01.77
           IF DC-NOT-SUCCESSFUL OR                                      01.77
              DC-RESULT = 03 OR                                         01.77
            ((DC-RESULT = 01 OR 02) AND                                 01.77
              DC-RETURN-NUM > 1)                                        01.77
              IF NOT WS-CHECK-QUAL-ONLY                                 01.77
                 MOVE 'T033'                TO WS-ERROR-DG              01.77
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.77
                    THRU S-300-EXIT                                     01.77
              END-IF                                                    01.77
              GO TO I-406-EXIT                                          01.77
           END-IF.                                                      01.77
      *                                                                 01.77
           MOVE PET-DATE                 TO DC-SEND-DATE.               01.77
           MOVE VSD-08-DAYS-CUTOFF-AIR   TO DC-SEND-NUM.                01.77
           MOVE 'DAJ'                    TO DC-OPERATION.               01.77
           PERFORM D-100-CALL-DATECONV                                  01.77
              THRU D-100-EXIT.                                          01.77
           IF DC-RESULT = 00 OR 01                                      01.77
              NEXT SENTENCE                                             01.77
           ELSE                                                         01.77
              IF NOT WS-CHECK-QUAL-ONLY                                 01.77
                 MOVE 'T032'                TO WS-ERROR-DG              01.77
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.77
                    THRU S-300-EXIT                                     01.77
              END-IF                                                    01.77
              GO TO I-406-EXIT.                                         01.77
      *                                                                 01.77
           MOVE 'Y'                      TO WS-CPS-REGULATED-FLG        01.89
                                            WS-CPS-PT-CP-FLG            01.77
                                            WS-CPS-REWARDS-SW           01.77
                                            WS-CPS-REIMB-SW.            01.77
      *                                                                 01.77
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-2001-PSI           TO WS-CPS-PSI                  01.77
              MOVE VU-2001-RMB           TO WS-CPS-REIMB                01.77
           ELSE                                                         01.77
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-2232-PSI        TO WS-CPS-PSI                  01.77
                 MOVE VU-2232-RMB        TO WS-CPS-REIMB                01.77
              ELSE                                                      01.77
                 MOVE VU-3131-PSI        TO WS-CPS-PSI                  01.77
                 MOVE VU-3131-RMB        TO WS-CPS-REIMB                01.77
              END-IF                                                    01.77
           END-IF.                                                      01.77
           MOVE WS-CPS-PSI               TO WS-CPS-PT-PSI.              01.77
           MOVE WS-CPS-REIMB             TO WS-CPS-PT-RMB.              01.77
      *                                                                 01.77
           IF WS-CHECK-QUAL-ONLY                                        01.77
              GO TO I-406-EXIT.                                         01.77
      *                                                                 01.77
           PERFORM S-500-SET-PLAN-DATA                                  01.77
              THRU S-500-EXIT.                                          01.77
      *                                                                 01.77
       I-406-EXIT.                                                      01.77
           EXIT.                                                        01.77
      *                                                                 01.77
       I-410-SET-CPS-HOTEL-NO-CRD.
      *
      * SET RATE TABLE VALUES FOR USA CPS HOTEL CARD NOT PRESENT.
      *
      *    4001 VISA USA CPS HOTEL/CAR CARD NOT PRESENT
      *    4031 VISA USA CPS HOTEL/CAR CARD NOT PRESENT - DEBIT
      *    4032 VISA USA CPS HOTEL/CAR CARD NOT PRESENT - REGULATED
      *
           IF NOT KDA-MERCAT-LODGING
              GO TO I-410-EXIT.
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-410-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              KDA-VS-PID-IS-SIGNATURE OR                                01.98
              KDA-VS-PID-IS-INFINITE                                    01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE '3'                   TO VU-4001-PSI                 01.57
              MOVE VU-4001-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE '3'                TO VU-4032-PSI                 01.64
                 MOVE VU-4032-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE '3'                TO VU-4031-PSI                 01.64
                 MOVE VU-4031-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-410-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-HC-CNP-FLG.
      *
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
           IF NOT KDA-VC-POS-ENTRY-KEYED                                02.07
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
           IF PET-TOTAL-AUTH-AMT = +0
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T004'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
           IF KDA-CHD-AR-IS-TOKEN                                       02.07
              IF KDA-VC-PHONE-ORD-5                                     02.07
                    NEXT SENTENCE                                       02.07
              ELSE                                                      02.07
                 IF NOT WS-CHECK-QUAL-ONLY                              02.07
                    MOVE 'V002'          TO WS-ERROR-DG                 02.07
                    PERFORM S-300-PROCESS-DOWNGRADE                     02.07
                       THRU S-300-EXIT                                  02.07
                 END-IF                                                 02.07
                 GO TO I-410-EXIT.                                      02.07
           IF KDA-VC-CARDHLDR-ID-1 OR
              KDA-VC-CARDHLDR-ID-SPACE
                 NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
           IF NOT KDA-VC-SPEC-COND-IND-2-US-LDG
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V014'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
           IF NOT KDA-HAS-LG
              IF NOT WS-CHECK-QUAL-ONLY
                 IF KDA-LG-HAD-ERRORS
                    MOVE 'A003'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'A001'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
           IF KDA-NOT-PQ-VS-USA-HC-CNP
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'A002'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
           IF NOT KDA-VC-MKT-SPC-AUTH-HTL-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V011'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
           IF PET-PURCHASE-IDENTIFIER NOT = '4'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V012'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
           IF PET-PURCHASE-IDENT = SPACES
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V013'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
           IF PET-DESCR-FLAG NOT = 'Y'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T034'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'                TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-410-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-HC-CNP-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-REIMB-SW
                                            WS-CPS-REWARDS-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4001-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-4001-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4032-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4032-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-4031-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4031-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
           MOVE WS-CPS-PSI               TO WS-CPS-HC-CNP-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-HC-CNP-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-410-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-410-EXIT.
           EXIT.
      *
       I-415-SET-CPS-HOTEL-CRD.
      *
      * SET RATE TABLE VALUES FOR USA CPS HOTEL CARD PRESENT.
      *
      *    4101 VISA USA CPS HOTEL/CAR CARD PRESENT
      *    4131 VISA USA CPS HOTEL/CAR CARD PRESENT - DEBIT
      *    4132 VISA USA CPS HOTEL/CAR CARD PRESENT - PREPAID
      *
           IF NOT KDA-MERCAT-LODGING
              GO TO I-415-EXIT.
      *
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              KDA-VS-PID-IS-SIGNATURE OR                                01.98
              KDA-VS-PID-IS-INFINITE                                    01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE '4'                   TO VU-4101-PSI                 01.57
              MOVE VU-4101-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE '4'                TO VU-4132-PSI                 01.64
                 MOVE VU-4132-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE '4'                TO VU-4131-PSI                 01.64
                 MOVE VU-4131-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-415-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-HC-CP-FLG.
      *
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-415-EXIT.
           IF NOT KDA-VC-POS-ENTRY-MODE-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-415-EXIT.
           IF PET-TOTAL-AUTH-AMT = +0
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T004'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-415-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-1-3
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-415-EXIT.
           IF WS-QUASI-CASH                                             01.77
              CONTINUE                                                  01.77
           ELSE                                                         01.77
           IF NOT KDA-VC-SPEC-COND-IND-2-US-LDG
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V014'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-415-EXIT.
           IF NOT KDA-HAS-LG
              IF NOT WS-CHECK-QUAL-ONLY
                 IF KDA-LG-HAD-ERRORS
                    MOVE 'A003'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'A001'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-415-EXIT.
           IF KDA-NOT-PQ-VS-USA-HC-CP
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'A002'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-415-EXIT.
           IF NOT KDA-VC-MKT-SPC-AUTH-HTL-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V011'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-415-EXIT.
           IF PET-PURCHASE-IDENTIFIER NOT = '4'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V012'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-415-EXIT.
           IF PET-PURCHASE-IDENT = SPACES
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V013'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-415-EXIT.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'                TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-415-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-HC-CP-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-REIMB-SW
                                            WS-CPS-REWARDS-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4101-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-4101-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4132-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4132-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-4131-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4131-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-HC-CP-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-HC-CP-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-415-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-415-EXIT.
           EXIT.
      *
       I-420-SET-CPS-CAR-NO-CRD.
      *
      * SET RATE TABLE VALUES FOR USA CPS CAR RENTAL CARD NOT PRESENT.
      *
      *    4001 VISA USA CPS HOTEL/CAR CARD NOT PRESENT
      *    4031 VISA USA CPS HOTEL/CAR CARD NOT PRESENT - DEBIT
      *    4032 VISA USA CPS HOTEL/CAR CARD NOT PRESENT - PREPAID
      *
           IF NOT KDA-MERCAT-CAR-RENT
              GO TO I-420-EXIT.
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-420-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              KDA-VS-PID-IS-SIGNATURE OR                                01.98
              KDA-VS-PID-IS-INFINITE                                    01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE '5'                   TO VU-4001-PSI                 01.57
              MOVE VU-4001-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE '5'                TO VU-4032-PSI                 01.64
                 MOVE VU-4032-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE '5'                TO VU-4031-PSI                 01.64
                 MOVE VU-4031-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-420-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-HC-CNP-FLG.
      *
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-420-EXIT.
           IF NOT KDA-VC-POS-ENTRY-KEYED                                02.07
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-420-EXIT.
           IF PET-TOTAL-AUTH-AMT = +0
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T004'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-420-EXIT.
           IF KDA-CHD-AR-IS-TOKEN                                       02.07
              IF KDA-VC-PHONE-ORD-5                                     02.07
                    NEXT SENTENCE                                       02.07
              ELSE                                                      02.07
                 IF NOT WS-CHECK-QUAL-ONLY                              02.07
                    MOVE 'V002'          TO WS-ERROR-DG                 02.07
                    PERFORM S-300-PROCESS-DOWNGRADE                     02.07
                       THRU S-300-EXIT                                  02.07
                 END-IF                                                 02.07
                 GO TO I-420-EXIT.                                      02.07
           IF KDA-VC-CARDHLDR-ID-1 OR
              KDA-VC-CARDHLDR-ID-SPACE
                 NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-420-EXIT.
           IF NOT KDA-HAS-CA
              IF NOT WS-CHECK-QUAL-ONLY
                 IF KDA-CA-HAD-ERRORS
                    MOVE 'A003'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'A001'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-420-EXIT.
           IF KDA-NOT-PQ-VS-USA-HC-CNP
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'A002'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-420-EXIT.
           IF NOT KDA-VC-MKT-SPC-AUTH-CAR-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V011'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-420-EXIT.
           IF PET-PURCHASE-IDENTIFIER NOT = '3'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V012'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-420-EXIT.
           IF PET-PURCHASE-IDENT = SPACES
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V013'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-420-EXIT.
           IF PET-DESCR-FLAG NOT = 'Y'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T034'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-420-EXIT.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'                TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-420-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-HC-CNP-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-REIMB-SW
                                            WS-CPS-REWARDS-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4001-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-4001-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4032-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4032-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-4031-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4031-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-HC-CNP-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-HC-CNP-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-420-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-420-EXIT.
           EXIT.
      *
       I-425-SET-CPS-CAR-CRD.
      *
      * SET RATE TABLE VALUES FOR USA CPS CAR RENTAL CARD PRESENT.
      *
      *    4101 VISA USA CPS HOTEL/CAR CARD PRESENT
      *    4131 VISA USA CPS HOTEL/CAR CARD PRESENT - DEBIT
      *    4132 VISA USA CPS HOTEL/CAR CARD PRESENT - PREPAID
      *
           IF NOT KDA-MERCAT-CAR-RENT
              GO TO I-425-EXIT.
      *
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              KDA-VS-PID-IS-SIGNATURE OR                                01.98
              KDA-VS-PID-IS-INFINITE                                    01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE '6'                   TO VU-4101-PSI                 01.57
              MOVE VU-4101-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE '6'                TO VU-4132-PSI                 01.64
                 MOVE VU-4132-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE '6'                TO VU-4131-PSI                 01.64
                 MOVE VU-4131-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-425-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-HC-CP-FLG.
      *
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-425-EXIT.
           IF NOT KDA-VC-POS-ENTRY-MODE-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-425-EXIT.
           IF PET-TOTAL-AUTH-AMT = +0
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T004'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-425-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-1-3
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-425-EXIT.
           IF NOT KDA-HAS-CA
              IF NOT WS-CHECK-QUAL-ONLY
                 IF KDA-CA-HAD-ERRORS
                    MOVE 'A003'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'A001'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-425-EXIT.
           IF KDA-NOT-PQ-VS-USA-HC-CP
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'A002'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-425-EXIT.
           IF NOT KDA-VC-MKT-SPC-AUTH-CAR-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V011'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-425-EXIT.
           IF PET-PURCHASE-IDENTIFIER NOT = '3'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V012'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-425-EXIT.
           IF PET-PURCHASE-IDENT = SPACES
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V013'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-425-EXIT.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'                TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-425-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-HC-CP-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-REIMB-SW
                                            WS-CPS-REWARDS-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4101-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-4101-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4132-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4132-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-4131-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4131-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-HC-CP-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-HC-CP-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-425-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-425-EXIT.
           EXIT.
      *
       I-430-SET-CPS-CARD-NOT-PRESENT.
      *
      * SET RATE TABLE VALUES FOR USA CPS CARD NOT PRESENT.
      *
      *    2701 VISA USA CPS CARD NOT PRESENT - DIRECT MARKETING
      *    5001 VISA USA CPS CARD NOT PRESENT
      *    5031 VISA USA CPS CARD NOT PRESENT - DEBIT
      *    5032 VISA USA CPS CARD NOT PRESENT - PREPAID                 01.64
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-430-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
             (KDA-CHD-AR-IS-FND-SRC-CREDIT AND                          01.84
              KDA-MRCH-TYP-CHARITY) OR                                  01.68
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              WS-IS-RTL-2-MRCH OR                                       01.64
              WS-IS-UTILITY OR                                          01.61
              KDA-VS-PID-IS-SIGNATURE OR                                01.84
              KDA-VS-PID-IS-INFINITE                                    01.84
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              IF KDA-MERCAT-DIRECT-MKT
                 MOVE VU-2701-PLN        TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
              ELSE
                 MOVE VU-5001-PLN        TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
              END-IF
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-5032-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-5031-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-430-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-RTL-CNP-FLG.
           IF WS-IS-RTL-2-MRCH                                          01.64
              MOVE 'A'                   TO WS-CPS-RTL-2-FLG.
      *
           IF KDA-VC-AUTH-CHAR-V
              IF KDA-VC-MKT-SPC-AUTH-BILL-PAY
                 NEXT SENTENCE
              ELSE
                 IF NOT KDA-VC-AVS-RESPONSE-CODE-US
                    IF NOT WS-CHECK-QUAL-ONLY
                       MOVE 'V016'       TO WS-ERROR-DG
                       PERFORM S-300-PROCESS-DOWNGRADE
                          THRU S-300-EXIT
                    END-IF
                    GO TO I-430-EXIT.
           IF KDA-VC-AUTH-CHAR-R
              IF KDA-MRCH-TYP-UTILITY
                 IF KDA-VC-MKT-SPC-AUTH-BILL-PAY
                    IF NOT WS-CHECK-QUAL-ONLY
                       MOVE 'V015'       TO WS-ERROR-DG
                       PERFORM S-300-PROCESS-DOWNGRADE
                          THRU S-300-EXIT
                    END-IF
                    GO TO I-430-EXIT
                 ELSE
                    NEXT SENTENCE
              ELSE
              IF KDA-MERCAT-HOSPITAL OR
                 KDA-MERCAT-HEALTH-CARE OR
                 WS-IS-RTL-2-MRCH                                       01.64
                    NEXT SENTENCE
              ELSE
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V015'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-430-EXIT.
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-430-EXIT.
           IF KDA-VC-PHONE-ORD-1-3 OR
              KDA-VC-POS-ENV-RECUR
                 NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V002'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-430-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-4
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-430-EXIT.
           IF PET-TOTAL-AUTH-AMT = +0
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T004'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-430-EXIT.
           IF NOT KDA-VC-POS-ENTRY-KEYED
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-430-EXIT.
           IF PET-PURCHASE-IDENTIFIER NOT = '1'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V012'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-430-EXIT.
           IF PET-PURCHASE-IDENT = SPACES
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V013'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-430-EXIT.
           IF PET-DESCR-FLAG NOT = 'Y'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T034'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-430-EXIT.
      *
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
             (DC-RESULT = 01 AND                                        01.56
              DC-RETURN-NUM > 7) OR                                     01.56
             (DC-RESULT = 02 AND                                        01.56
              DC-RETURN-NUM > 1)                                        01.56
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-430-EXIT                                          01.56
           END-IF.
      *
           MOVE VU-5001-PSI              TO WS-LRG-TKT-SW               01.77
                                            WS-LRG-TKT-ADV-SW           01.86
                                            WS-LRG-TKT-PURCH-SW.        01.86
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'                TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-430-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-RTL-CNP-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-CHARITY-FLG          01.68
                                            WS-CPS-RTL-2-FLG
                                            WS-CPS-REIMB-SW
                                            WS-CPS-REWARDS-SW.
           MOVE VU-5001-PSI              TO WS-GSA-LRG-TKT-SW.          01.57
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              IF KDA-MERCAT-DIRECT-MKT
                 MOVE VU-2701-PSI        TO WS-CPS-PSI                  01.57
                 MOVE VU-2701-RMB        TO WS-CPS-REIMB                01.57
              ELSE
                 MOVE VU-5001-PSI        TO WS-CPS-PSI                  01.57
                 MOVE VU-5001-RMB        TO WS-CPS-REIMB                01.57
              END-IF
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-5032-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-5032-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-5031-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-5031-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-RTL-CNP-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-RTL-CNP-RMB.
           IF WS-IS-RTL-2-MRCH                                          01.64
              MOVE WS-CPS-PSI            TO WS-CPS-RTL-2-PSI
              MOVE WS-CPS-REIMB          TO WS-CPS-RTL-2-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-430-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-430-EXIT.
           EXIT.
      *
       I-435-SET-CPS-AUTO-FUEL.
      *
      * SET RATE TABLE VALUES FOR USA CPS AUTOMATED FUEL DISPENSER TRANS.
      *
      *    2431 VISA USA CPS AUTOMATED FUEL DISPENSER - DEBIT MAX       01.52
      *    2432 VISA USA CPS AUTOMATED FUEL DISPENSER - PREPAID MAX     01.64
      *    5501 VISA USA CPS AUTOMATED FUEL DISPENSER
      *    5531 VISA USA CPS AUTOMATED FUEL DISPENSER - DEBIT
      *    5532 VISA USA CPS AUTOMATED FUEL DISPENSER - PREPAID         01.64
      *    5541 VISA USA CPS AUTOMATED FUEL DISPENSER - SIG PREF        01.52
      *    6201 VISA USA CPS AUTOMATED FUEL DISPENSER - MAX             01.84
      *    6341 VISA USA CPS AUTOMATED FUEL DISPENSER - SIG PREF MAX    01.84
      *    7533 VISA USA CPS FUEL - INFINITE                            01.98
      *    7531 VISA USA CPS FUEL - INFINITE MAX                        01.98
      *
           IF NOT KDA-MERCAT-AUTO-FUEL
              GO TO I-435-EXIT.
           IF KDA-TRAN-USD-AMT > WS-USD-AFD-MAX-AMT
              GO TO I-435-EXIT.
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-435-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY                                          01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              IF WS-IS-SIG-PREF-CREDIT                                  01.55
                 MOVE VU-5541-PLN        TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X         01.52
              ELSE                                                      01.52
              IF WS-IS-INFINITE-QUAL                                    01.98
                 MOVE VU-7533-PLN        TO WS-PLAN-DG                  01.98
                                            WS-PLAN-QUAL-CODE-X         01.98
              ELSE                                                      01.98
                 MOVE VU-5501-PLN        TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X         01.52
              END-IF                                                    01.52
              END-IF                                                    01.98
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-5532-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-5531-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-435-EXIT.
      *
           IF NOT WS-IS-SIG-PREF-CREDIT AND                             01.98
              NOT WS-IS-INFINITE-QUAL                                   01.98
              MOVE 'A'                   TO WS-CPS-AUTO-FUEL-FLG.       01.54
      *
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-435-EXIT.
           IF NOT KDA-VC-POS-ENTRY-MODE-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-435-EXIT.
           IF PET-ACQ-CURR-CODE = +840
              IF PET-AUTHORIZ-AMT NOT = +1.00
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'T002'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-435-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-3
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-435-EXIT.
           IF NOT KDA-VC-CAT-TERM-LVL-3
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V017'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-435-EXIT.
           IF NOT KDA-VC-POS-TERM-AFD                                   01.96
      *       DOWNGRADE ADDED DUE TO V0256 EDIT PACKAGE REJECT          01.96
              IF NOT WS-CHECK-QUAL-ONLY                                 01.96
                 MOVE 'V003'             TO WS-ERROR-DG                 01.96
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.96
                    THRU S-300-EXIT                                     01.96
              END-IF                                                    01.96
              GO TO I-435-EXIT                                          01.96
           END-IF.                                                      01.96
      *
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
            ((DC-RESULT = 01 OR 02) AND                                 01.56
              DC-RETURN-NUM > 1)                                        01.56
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-435-EXIT                                          01.56
           END-IF.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-435-EXIT.
      *
           IF WS-IS-SIG-PREF-CREDIT                                     01.55
              MOVE 'Y'                   TO WS-SIG-PREF-FUEL-SW         01.54
              MOVE 'Y'                   TO WS-CPS-REIMB-SW             02.06
              GO TO I-435-SET-PLAN-DATA.                                01.54
      *
           IF WS-IS-INFINITE-QUAL                                       01.98
              MOVE 'Y'                   TO WS-INFINITE-FUEL-SW         01.98
              MOVE 'Y'                   TO WS-CPS-REIMB-SW             02.06
              GO TO I-435-SET-PLAN-DATA.                                01.98
      *                                                                 01.98
           MOVE 'Y'                      TO WS-CPS-AUTO-FUEL-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-REWARDS-SW
                                            WS-CPS-REIMB-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-5501-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-5501-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-5532-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-5532-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-5531-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-5531-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-AUTO-FUEL-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-AUTO-FUEL-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-435-EXIT.
      *
       I-435-SET-PLAN-DATA.                                             01.54
      *                                                                 01.54
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-435-EXIT.
           EXIT.
      *
       I-440-SET-CPS-RETAIL.
      *
      * DETERMINE WHICH CPS RETAIL CRITERIA TO USE
      *
           PERFORM I-500-SET-CPS-SUPERMARKET
              THRU I-500-EXIT.
      *
           PERFORM I-530-SET-CPS-RETAIL
              THRU I-530-EXIT.
      *
       I-440-EXIT.
           EXIT.
      *
       I-445-SET-CPS-RESTAURANT.
      *
      * SET RATE TABLE VALUES FOR CPS RESTAURANT
      *
      *    2601 VISA USA CPS RESTAURANT
      *    2631 VISA USA CPS RESTAURANT - DEBIT
      *    2632 VISA USA CPS RESTAURANT - PREPAID                       01.64
      *
           IF NOT KDA-MRCH-TYP-CPS-REST
              GO TO I-445-EXIT.
      *
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              KDA-VS-PID-IS-SIGNATURE OR                                01.98
              KDA-VS-PID-IS-INFINITE                                    01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-2601-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-2632-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-2631-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-445-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-RTL-RST-FLG.
      *
           IF NOT KDA-CHD-AR-IS-FND-SRC-CREDIT                          01.84
              IF NOT KDA-VC-AUTH-CHAR-E
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V015'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-445-EXIT.
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-445-EXIT.
           IF NOT KDA-VC-POS-ENTRY-MODE-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-445-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-1-3
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-445-EXIT.
      *
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
            ((DC-RESULT = 01 OR 02) AND                                 01.56
              DC-RETURN-NUM > 1)                                        01.56
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-445-EXIT                                          01.56
           END-IF.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-445-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-RTL-RST-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-REIMB-SW
                                            WS-CPS-REWARDS-SW.          01.89
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-2601-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-2601-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-2632-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-2632-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-2631-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-2631-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-RTL-RST-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-RTL-RST-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-445-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-445-EXIT.
           EXIT.
      *
       I-450-SET-CPS-RETAIL-KEYED.
      *
      * SET RATE TABLE VALUES FOR USA CPS RETAIL KEY-ENTRY
      *
      *    3301 VISA USA CPS RETAIL KEY-ENTRY
      *    3331 VISA USA CPS RETAIL KEY-ENTRY - DEBIT
      *    3332 VISA USA CPS RETAIL KEY-ENTRY - PREPAID                 01.64
      *
           IF KDA-MRCH-TYP-NO-KEYED
              GO TO I-450-EXIT.
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-450-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
             (KDA-CHD-AR-IS-FND-SRC-CREDIT AND                          01.84
              KDA-MRCH-TYP-CHARITY) OR                                  01.68
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              WS-IS-RTL-2-MRCH OR                                       01.64
              WS-IS-UTILITY OR                                          01.61
              KDA-VS-PID-IS-SIGNATURE OR                                01.84
              KDA-VS-PID-IS-INFINITE                                    01.84
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-3301-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-3332-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-3331-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-450-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-RTL-KEY-FLG.
           IF WS-IS-RTL-2-MRCH                                          01.64
              MOVE 'A'                   TO WS-CPS-RTL-2-FLG.
      *
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-450-EXIT.
           IF NOT KDA-VC-POS-ENTRY-KEYED
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-450-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-1-2
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-450-EXIT.
           IF KDA-VC-MKT-SPC-AUTH-BILL-PAY                              01.56
              IF NOT KDA-VC-PHONE-ORD-BPAY-1-3                          01.56
                 MOVE 'V002'             TO WS-ERROR-DG                 01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
                 GO TO I-450-EXIT                                       01.56
              END-IF                                                    01.56
           ELSE                                                         01.56
              IF NOT KDA-VC-PHONE-ORD-SPACE                             01.56
                 IF NOT WS-CHECK-QUAL-ONLY                              01.56
                    MOVE 'V002'          TO WS-ERROR-DG                 01.56
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.56
                       THRU S-300-EXIT                                  01.56
                 END-IF                                                 01.56
                 GO TO I-450-EXIT                                       01.56
              END-IF                                                    01.56
           END-IF.                                                      01.56
      *                                                                 01.56
           IF KDA-VS-PID-IS-COMMERCIAL                                  01.99
                 NEXT SENTENCE                                          01.99
           ELSE                                                         01.99
              IF NOT KDA-VC-AVS-RESPONSE-CODE-KEY                       01.99
                 IF NOT WS-CHECK-QUAL-ONLY                              01.99
                    MOVE 'V016'          TO WS-ERROR-DG                 01.99
                    PERFORM S-300-PROCESS-DOWNGRADE                     01.99
                       THRU S-300-EXIT                                  01.99
                 END-IF                                                 01.99
                 GO TO I-450-EXIT.                                      01.99
      *
           IF KDA-MERCAT-CAR-RENT
              IF NOT KDA-HAS-CA
                 IF NOT WS-CHECK-QUAL-ONLY
                    IF KDA-CA-HAD-ERRORS
                       MOVE 'A003'       TO WS-ERROR-DG
                    ELSE
                       MOVE 'A001'       TO WS-ERROR-DG
                    END-IF
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
              IF KDA-NOT-PQ-VS-USA-RTL
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'A002'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
              IF NOT KDA-VC-MKT-SPC-AUTH-CAR-CPS
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V011'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
              IF PET-PURCHASE-IDENTIFIER NOT = '3'
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V012'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
              IF PET-PURCHASE-IDENT = SPACES
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V013'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
           END-IF.
           IF KDA-MERCAT-LODGING
              IF NOT KDA-HAS-LG
                 IF NOT WS-CHECK-QUAL-ONLY
                    IF KDA-LG-HAD-ERRORS
                       MOVE 'A003'       TO WS-ERROR-DG
                    ELSE
                       MOVE 'A001'       TO WS-ERROR-DG
                    END-IF
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
              IF KDA-NOT-PQ-VS-USA-RTL
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'A002'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
              IF NOT KDA-VC-MKT-SPC-AUTH-HTL-CPS
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V011'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
              IF PET-PURCHASE-IDENTIFIER NOT = '4'
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V012'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
              IF PET-PURCHASE-IDENT = SPACES
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V013'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
           END-IF.
           IF KDA-MERCAT-AIR
              IF NOT KDA-HAS-AI AND                                     01.98
                 NOT KDA-HAS-AN                                         01.98
                 IF NOT WS-CHECK-QUAL-ONLY
                    IF KDA-AI-HAD-ERRORS
                       MOVE 'A003'       TO WS-ERROR-DG
                    ELSE
                       MOVE 'A001'       TO WS-ERROR-DG
                    END-IF
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
              IF KDA-NOT-PQ-VS-USA-RTL
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'A002'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
              IF PET-DESCR-FLAG NOT = 'Y'
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'T034'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-450-EXIT
              END-IF
           END-IF.
      *
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
            ((DC-RESULT = 01 OR 02) AND                                 01.56
              DC-RETURN-NUM > 1)                                        01.56
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-450-EXIT                                          01.56
           END-IF.
      *
           MOVE VU-3301-PSI              TO WS-LRG-TKT-SW.              01.57
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-450-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-RTL-KEY-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-CHARITY-FLG          01.68
                                            WS-CPS-REIMB-SW
                                            WS-CPS-REWARDS-SW.
           IF WS-IS-RTL-2-MRCH                                          01.64
              MOVE 'Y'                   TO WS-CPS-RTL-2-FLG.
           MOVE VU-3301-PSI              TO WS-GSA-LRG-TKT-SW.          01.57
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-3301-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-3301-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-3332-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-3332-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-3331-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-3331-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-RTL-KEY-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-RTL-KEY-RMB.
           IF WS-IS-RTL-2-MRCH                                          01.64
              MOVE WS-CPS-PSI            TO WS-CPS-RTL-2-PSI
              MOVE WS-CPS-REIMB          TO WS-CPS-RTL-2-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-450-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-450-EXIT.
           EXIT.

       I-455-SET-CPS-SMALL-TKT.
      *
      * SET RATE TABLE VALUES FOR USA CPS SMALL TICKET
      *
      *    5331 VISA USA CPS RETAIL SMALL TICKET - US REGULATED DEBIT   01.65
      *    5332 VISA USA CPS RETAIL SMALL TICKET - US REGULATED PREPAID 01.65
      *    5601 VISA USA CPS RETAIL SMALL TICKET
      *    5631 VISA USA CPS RETAIL SMALL TICKET - DEBIT
      *    5632 VISA USA CPS RETAIL SMALL TICKET - PREPAID              01.64
      *
           IF KDA-MRCH-NO-SML-TKT                                       01.64
              GO TO I-455-EXIT.                                         01.64
      *                                                                 01.52
           IF KDA-TRAN-USD-AMT > WS-USD-SML-TKT-AMT
              GO TO I-455-EXIT.
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-455-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.61
              WS-IS-INFINITE-QUAL OR                                    01.98
              WS-IS-GOV-HE-PAY OR                                       01.86
              WS-IS-UTILITY                                             01.61
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           MOVE 'A'                      TO WS-CPS-SML-TKT-FLG.         01.98
      *                                                                 01.98
           IF KDA-CHD-AR-IS-REGULATED                                   01.64
              IF KDA-VS-PID-IS-BUSINESS                                 01.84
                 MOVE VU-2521-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
                 GO TO I-455-CHECK-QUAL-ONLY                            01.65
              END-IF                                                    01.64
              IF (KDA-CHD-AR-IS-FND-SRC-PREPAID AND                     01.84
                  KDA-VS-PID-IS-COMMERCIAL)                             01.84
                    MOVE VU-2021-PLN     TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
                 GO TO I-455-CHECK-QUAL-ONLY                            01.65
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *                                                                 01.64
           IF KDA-CHD-AR-IS-REGULATED                                   01.65
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-5332-PLN        TO WS-PLAN-DG                  01.65
                                            WS-PLAN-QUAL-CODE-X         01.65
              ELSE                                                      01.65
                 MOVE VU-5331-PLN        TO WS-PLAN-DG                  01.65
                                            WS-PLAN-QUAL-CODE-X         01.65
              END-IF                                                    01.65
           ELSE                                                         01.65
              IF KDA-CHD-AR-IS-FND-SRC-CREDIT                           01.84
                 MOVE VU-5601-PLN        TO WS-PLAN-DG                  01.65
                                            WS-PLAN-QUAL-CODE-X         01.65
              ELSE                                                      01.65
                 IF KDA-CHD-AR-IS-FND-SRC-PREPAID                       01.84
                    MOVE VU-5632-PLN     TO WS-PLAN-DG                  01.65
                                            WS-PLAN-QUAL-CODE-X         01.65
                 ELSE                                                   01.65
                    MOVE VU-5631-PLN     TO WS-PLAN-DG                  01.65
                                            WS-PLAN-QUAL-CODE-X         01.65
              END-IF                                                    01.65
           END-IF.                                                      01.64
      *
       I-455-CHECK-QUAL-ONLY.                                           01.65
      *
           IF NOT KDA-CHD-AR-IS-REGULATED                               01.65
              IF NOT WS-CHECK-QUAL-ONLY                                 01.65
                 PERFORM S-525-CHECK-PLAN-LOW                           01.65
                    THRU S-525-EXIT                                     01.65
                 IF NOT WS-RATE-IS-LOWER                                01.65
                    GO TO I-455-EXIT.                                   01.65
      *
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-455-EXIT.
           IF NOT KDA-VC-POS-ENTRY-MODE-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-455-EXIT.
           IF KDA-VC-CARDHLDR-ID-SPACE OR
              KDA-VC-CARDHLDR-ID-1-3
                 NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-455-EXIT.
      *
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
            ((DC-RESULT = 01 OR 02) AND                                 01.56
              DC-RETURN-NUM > 1)                                        01.56
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-455-EXIT                                          01.56
           END-IF.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-455-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-SML-TKT-FLG
                                            WS-CPS-REIMB-SW.
      *
           IF KDA-CHD-AR-IS-REGULATED                                   01.64
              IF KDA-VS-PID-IS-BUSINESS                                 01.84
                 MOVE 'Y'                TO WS-CPS-REGULATED-FLG        01.64
                 GO TO I-455-CPS-SET                                    01.64
              ELSE                                                      01.64
                 IF (KDA-CHD-AR-IS-FND-SRC-PREPAID AND                  01.84
                     KDA-VS-PID-IS-COMMERCIAL)                          01.84
                       MOVE 'Y'          TO WS-CPS-REGULATED-FLG        01.64
                    GO TO I-455-CPS-SET                                 01.64
                 END-IF                                                 01.64
              END-IF                                                    01.64
              MOVE 'Y'                   TO WS-CPS-REGULATED-FLG        01.65
           END-IF.                                                      01.64
      *                                                                 01.64
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-5601-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-5601-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-5632-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-5632-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-5631-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-5631-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
      *
       I-455-CPS-SET.                                                   01.64
      *                                                                 01.64
           MOVE WS-CPS-PSI               TO WS-CPS-SML-TKT-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-SML-TKT-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-455-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-455-EXIT.
           EXIT.
      *
       I-460-SET-CPS-E-COMM-BASIC.
      *
      * SET RATE TABLE VALUES FOR USA CPS E-COMMERCE BASIC
      *
      *    4501 VISA USA CPS E-COMMERCE BASIC
      *    4531 VISA USA CPS E-COMMERCE BASIC - DEBIT
      *    4532 VISA USA CPS E-COMMERCE BASIC - PREPAID                 01.64
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-460-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
             (KDA-CHD-AR-IS-FND-SRC-CREDIT AND                          01.84
              KDA-MRCH-TYP-CHARITY) OR                                  01.68
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              WS-IS-RTL-2-MRCH OR                                       01.64
              WS-IS-UTILITY OR                                          01.61
              KDA-VS-PID-IS-SIGNATURE OR                                01.84
              KDA-VS-PID-IS-INFINITE                                    01.84
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4501-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4532-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-4531-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-460-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-ECOM-BASIC-FLG.
           IF WS-IS-RTL-2-MRCH                                          01.64
              MOVE 'A'                   TO WS-CPS-RTL-2-FLG.
      *
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-460-EXIT.
           IF KDA-CHD-AR-IS-TOKEN
              IF NOT KDA-VC-PHONE-ORD-5                                 02.07
                 IF NOT WS-CHECK-QUAL-ONLY                              02.07
                    MOVE 'V002'          TO WS-ERROR-DG                 02.07
                    PERFORM S-300-PROCESS-DOWNGRADE                     02.07
                       THRU S-300-EXIT                                  02.07
                 END-IF                                                 02.07
                 GO TO I-460-EXIT                                       02.07
              END-IF                                                    02.07
           ELSE                                                         02.07
           IF NOT KDA-VC-PHONE-ORD-7
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V002'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-460-EXIT.
           IF NOT KDA-VC-POS-ENTRY-KEYED
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-460-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-4
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-460-EXIT.
           IF PET-TOTAL-AUTH-AMT = +0
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T004'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-460-EXIT.
           IF NOT KDA-VC-ECGI-DP
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V018'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-460-EXIT.
           IF PET-PURCHASE-IDENTIFIER NOT = '1'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V012'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-460-EXIT.
           IF PET-PURCHASE-IDENT = SPACES
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V013'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-460-EXIT.
           IF PET-DESCR-FLAG NOT = 'Y'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T034'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-460-EXIT.
      *
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
             (DC-RESULT = 01 AND                                        01.56
              DC-RETURN-NUM > 7) OR                                     01.56
             (DC-RESULT = 02 AND                                        01.56
              DC-RETURN-NUM > 1)                                        01.56
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-460-EXIT                                          01.56
           END-IF.
      *
           MOVE VU-4501-PSI              TO WS-LRG-TKT-SW               01.77
                                            WS-LRG-TKT-ADV-SW           01.86
                                            WS-LRG-TKT-PURCH-SW.        01.86
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-460-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-ECOM-BASIC-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-CHARITY-FLG          01.68
                                            WS-CPS-RTL-2-FLG
                                            WS-CPS-REIMB-SW
                                            WS-CPS-REWARDS-SW.
           MOVE VU-4501-PSI              TO WS-GSA-LRG-TKT-SW.          01.57
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4501-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-4501-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4532-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4532-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-4531-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4531-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-ECOM-BASIC-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-ECOM-BASIC-RMB.
           IF WS-IS-RTL-2-MRCH                                          01.64
              MOVE WS-CPS-PSI            TO WS-CPS-RTL-2-PSI
              MOVE WS-CPS-REIMB          TO WS-CPS-RTL-2-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-460-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-460-EXIT.
           EXIT.
      *
       I-465-SET-CPS-ACCT-FUND.
      *
      * SET RATE TABLE VALUES FOR USA CPS ACCOUNT FUNDING
      *    PREFERRED RATE.
      *
      *    5701 VISA USA CPS ACCOUNT FUNDING
      *    5731 VISA USA CPS ACCOUNT FUNDING - DEBIT
      *    5732 VISA USA CPS ACCOUNT FUNDING - PREPAID
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-465-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              WS-IS-GOV-HE-PAY OR                                       01.86
              WS-IS-SIG-PREF-CREDIT OR                                  01.98
              WS-IS-INFINITE-QUAL                                       01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-5701-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-5732-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-5731-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-465-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-ACCT-FUND-FLG.       02.01
      *
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-465-EXIT.
          IF PET-AUTHORIZ-AMT NOT = PET-ACQ-AMOUNT                      02.08
             IF NOT WS-CHECK-QUAL-ONLY                                  02.08
                MOVE 'V009'          TO WS-ERROR-DG                     02.08
                PERFORM S-300-PROCESS-DOWNGRADE                         02.08
                   THRU S-300-EXIT                                      02.08
             END-IF                                                     02.08
             GO TO I-465-EXIT.                                          02.08
           IF NOT KDA-VC-PHONE-ORD-5-7
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V002'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-465-EXIT.
           IF NOT KDA-VC-AVS-RESPONSE-CODE-FUND
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V016'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-465-EXIT.
      *
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.                02.07
           MOVE PET-DATE                 TO DC-SEND-DATE.               02.07
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.                02.07
           MOVE 'DAK'                    TO DC-OPERATION.               02.07
           PERFORM D-100-CALL-DATECONV                                  02.07
              THRU D-100-EXIT.                                          02.07
           IF DC-NOT-SUCCESSFUL OR                                      02.07
              DC-RESULT = 03 OR                                         02.07
            ((DC-RESULT = 01 OR 02) AND                                 02.07
              DC-RETURN-NUM > 1)                                        02.07
              IF NOT WS-CHECK-QUAL-ONLY                                 02.07
                 MOVE 'T033'                TO WS-ERROR-DG              02.07
                 PERFORM S-300-PROCESS-DOWNGRADE                        02.07
                    THRU S-300-EXIT                                     02.07
              END-IF                                                    02.07
              GO TO I-465-EXIT                                          02.07
           END-IF.                                                      02.07
      *                                                                 02.07
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-465-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-ACCT-FUND-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-REIMB-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-5701-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-5701-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-5732-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-5732-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-5731-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-5731-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-465-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-465-EXIT.
           EXIT.
      *
       I-470-SET-CPS-SVC-STATION.
      *
      * SET RATE TABLE VALUES FOR CPS RETAIL SERVICE STATION
      *
      *    2331 VISA USA CPS RETAIL SERVICE STATION - DEBIT MAX         01.52
      *    2332 VISA USA CPS RETAIL SERVICE STATION - PREPAID MAX       01.64
      *    2801 VISA USA CPS RETAIL SERVICE STATION
      *    2831 VISA USA CPS RETAIL SERVICE STATION - DEBIT
      *    2832 VISA USA CPS RETAIL SERVICE STATION - PREPAID           01.64
      *    2841 VISA USA CPS RETAIL SERVICE STATION - SIG PREF          01.52
      *    6301 VISA USA CPS RETAIL SERVICE STATION - MAX               01.84
      *    7533 VISA USA CPS FUEL - INFINITE                            01.98
      *    7531 VISA USA CPS FUEL - INFINITE MAX                        01.98
      *
           IF NOT KDA-MRCH-TYP-SVC-STATION
              GO TO I-470-EXIT.
      *
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY                                          01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              IF WS-IS-SIG-PREF-CREDIT                                  01.55
                 MOVE VU-2841-PLN        TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X         01.52
              ELSE                                                      01.52
              IF WS-IS-INFINITE-QUAL                                    01.98
                 MOVE VU-7533-PLN        TO WS-PLAN-DG                  01.98
                                            WS-PLAN-QUAL-CODE-X         01.98
              ELSE                                                      01.98
                 MOVE VU-2801-PLN        TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X         01.52
              END-IF                                                    01.52
              END-IF                                                    01.98
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-2832-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-2831-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF WS-IS-SIG-PREF-CREDIT AND                                 01.55
              WS-CPS-SML-TKT-FLG = 'Y'                                  01.54
                 MOVE 'Y'                TO WS-SIG-PREF-FUEL-SW         01.54
                 MOVE 'Y'                TO WS-CPS-REIMB-SW             02.06
                 GO TO I-470-SET-PLAN-DATA                              01.54
           END-IF.                                                      01.54
      *
           IF WS-IS-INFINITE-QUAL AND                                   01.98
              WS-CPS-SML-TKT-FLG = 'Y'                                  01.98
                 MOVE 'Y'                TO WS-INFINITE-FUEL-SW         01.98
                 MOVE 'Y'                TO WS-CPS-REIMB-SW             02.06
                 GO TO I-470-SET-PLAN-DATA                              01.98
           END-IF.                                                      01.98
      *                                                                 01.98
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-470-EXIT.
      *
           IF NOT WS-IS-SIG-PREF-CREDIT AND                             01.98
              NOT WS-IS-INFINITE-QUAL                                   01.98
              MOVE 'A'                   TO WS-CPS-RTL-SST-FLG.         01.54
      *
           IF NOT KDA-CHD-AR-IS-FND-SRC-CREDIT                          01.84
              IF NOT KDA-VC-AUTH-CHAR-E
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V015'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-470-EXIT.
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-470-EXIT.
           IF NOT KDA-VC-POS-ENTRY-MODE-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-470-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-1-3
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-470-EXIT.
      *
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
            ((DC-RESULT = 01 OR 02) AND                                 01.56
              DC-RETURN-NUM > 1)                                        01.56
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-470-EXIT                                          01.56
           END-IF.
      *
           MOVE VU-2801-PSI              TO WS-LRG-TKT-SW.              01.57
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-470-EXIT.
      *
           IF WS-IS-SIG-PREF-CREDIT                                     01.55
              MOVE 'Y'                   TO WS-SIG-PREF-FUEL-SW         01.54
              MOVE 'Y'                   TO WS-CPS-REIMB-SW             02.06
              GO TO I-470-SET-PLAN-DATA.                                01.54
      *                                                                 01.54
           IF WS-IS-INFINITE-QUAL                                       01.98
              MOVE 'Y'                   TO WS-INFINITE-FUEL-SW         01.98
              MOVE 'Y'                   TO WS-CPS-REIMB-SW             02.06
              GO TO I-470-SET-PLAN-DATA.                                01.98
      *                                                                 01.98
           MOVE 'Y'                      TO WS-CPS-RTL-SST-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-REWARDS-SW
                                            WS-CPS-REIMB-SW.
           MOVE VU-2801-PSI              TO WS-GSA-LRG-TKT-SW.          01.57
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-2801-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-2801-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-2832-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-2832-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-2831-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-2831-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-RTL-SST-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-RTL-SST-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-470-EXIT.
      *
       I-470-SET-PLAN-DATA.                                             01.54
      *                                                                 01.54
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-470-EXIT.
           EXIT.
      *
       I-475-SET-CPS-E-COMM-PREF.
      *
      * SET RATE TABLE VALUES FOR USA CPS E-COMMERCE
      *    PREFERRED RATE.
      *
      *    5201 VISA USA CPS E-COMMERCE PREFERRED
      *    5231 VISA USA CPS E-COMMERCE PREFERRED - DEBIT
      *    5232 VISA USA CPS E-COMMERCE PREFERRED - PREPAID             01.64
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-475-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              KDA-CHD-AR-IS-REGULATED OR                                01.64
             (KDA-CHD-AR-IS-FND-SRC-CREDIT AND                          01.84
              KDA-MRCH-TYP-CHARITY) OR                                  01.68
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              WS-IS-RTL-2-MRCH OR                                       01.64
              WS-IS-UTILITY                                             01.61
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-5201-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-5232-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-5231-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-475-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-ECOM-PREF-FLG.
           IF WS-IS-RTL-2-MRCH                                          01.64
              MOVE 'A'                   TO WS-CPS-RTL-2-FLG.
      *
           IF (KDA-VC-AUTH-CHAR-U AND
               KDA-VC-PHONE-ORD-5) OR
              (KDA-VC-AUTH-CHAR-S AND
               KDA-VC-PHONE-ORD-6)
                 NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V010'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-475-EXIT.
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-475-EXIT.
           IF NOT KDA-VC-POS-ENTRY-KEYED
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-475-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-4
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-475-EXIT.
           IF PET-TOTAL-AUTH-AMT = +0
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T004'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-475-EXIT.
           IF NOT KDA-VC-ECGI-DP
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V018'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-475-EXIT.
           IF PET-PURCHASE-IDENTIFIER NOT = '1'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V012'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-475-EXIT.
           IF PET-PURCHASE-IDENT = SPACES
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V013'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-475-EXIT.
           IF PET-DESCR-FLAG NOT = 'Y'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T034'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-475-EXIT.
      *
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
             (DC-RESULT = 01 AND                                        01.56
              DC-RETURN-NUM > 7) OR                                     01.56
             (DC-RESULT = 02 AND                                        01.56
              DC-RETURN-NUM > 1)                                        01.56
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-475-EXIT                                          01.56
           END-IF.
      *
           MOVE VU-5201-PSI              TO WS-LRG-TKT-SW               01.77
                                            WS-LRG-TKT-ADV-SW           01.86
                                            WS-LRG-TKT-PURCH-SW.        01.86
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-475-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-ECOM-PREF-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-CHARITY-FLG          01.68
                                            WS-CPS-RTL-2-FLG
                                            WS-CPS-REIMB-SW
                                            WS-CPS-REWARDS-SW.
           MOVE VU-5201-PSI              TO WS-GSA-LRG-TKT-SW.          01.57
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-5201-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-5201-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-5232-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-5232-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-5231-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-5231-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-ECOM-PREF-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-ECOM-PREF-RMB.
           IF WS-IS-RTL-2-MRCH                                          01.64
              MOVE WS-CPS-PSI            TO WS-CPS-RTL-2-PSI
              MOVE WS-CPS-REIMB          TO WS-CPS-RTL-2-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-475-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-475-EXIT.
           EXIT.
      *
       I-480-SET-CPS-E-COMM-PREF-PT.
      *
      * SET RATE TABLE VALUES FOR USA CPS E-COMMERCE PASSENGER TRANSPORT
      *    PREFERRED RATE.
      *
      *    4401 VISA USA CPS PASSENGER TRANSPORT PREFERRED
      *    4431 VISA USA CPS PASSENGER TRANSPORT PREFERRED - DEBIT
      *    4432 VISA USA CPS PASSENGER TRANSPORT PREFERRED - PREPAID    01.64
      *
           IF KDA-MERCAT-AIR OR
              KDA-MRCH-TYP-PSNGR-RAIL
                 NEXT SENTENCE
           ELSE
              GO TO I-480-EXIT.
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-480-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              KDA-VS-PID-IS-SIGNATURE OR                                01.98
              KDA-VS-PID-IS-INFINITE                                    01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4401-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4432-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-4431-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-480-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-ECOM-PREF-PT-FLG.
      *
           IF (KDA-VC-AUTH-CHAR-U AND
               KDA-VC-PHONE-ORD-5) OR
              (KDA-VC-AUTH-CHAR-S AND
               KDA-VC-PHONE-ORD-6)
                 NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V010'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-480-EXIT.
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-480-EXIT.
           IF NOT KDA-VC-POS-ENTRY-KEYED
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-480-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-4
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-480-EXIT.
           IF NOT KDA-HAS-AI AND                                        01.98
              NOT KDA-HAS-AN                                            01.98
              IF NOT WS-CHECK-QUAL-ONLY
                 IF KDA-AI-HAD-ERRORS
                    MOVE 'A003'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'A001'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-480-EXIT.
           IF KDA-NOT-PQ-VS-USA-PT
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'A002'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-480-EXIT.
           IF NOT KDA-VC-ECGI-DP
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V018'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-480-EXIT.
           IF PET-DESCR-FLAG NOT = 'Y'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T034'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-480-EXIT.
      *
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
            ((DC-RESULT = 01 OR 02) AND                                 02.07
              DC-RETURN-NUM > 1)                                        02.07
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-480-EXIT                                          01.56
           END-IF.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-08-DAYS-CUTOFF-AIR   TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-480-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-REGULATED-FLG        01.89
                                            WS-CPS-ECOM-PREF-PT-FLG
                                            WS-CPS-REWARDS-SW
                                            WS-CPS-REIMB-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4401-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-4401-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4432-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4432-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-4431-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4431-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-ECOM-PREF-PT-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-ECOM-PREF-PT-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-480-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-480-EXIT.
           EXIT.
      *
       I-485-SET-CPS-E-COMM-PREF-H.
      *
      * SET RATE TABLE VALUES FOR USA CPS E-COMMERCE HOTEL
      *    PREFERRED RATE.
      *
      *    4201 VISA USA CPS HOTEL/CAR PREFERRED
      *    4231 VISA USA CPS HOTEL/CAR PREFERRED - DEBIT
      *    4232 VISA USA CPS HOTEL/CAR PREFERRED - PREPAID              01.64
      *
           IF NOT KDA-MERCAT-LODGING
              GO TO I-485-EXIT.
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-485-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              KDA-VS-PID-IS-SIGNATURE OR                                01.98
              KDA-VS-PID-IS-INFINITE                                    01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE 'L'                   TO VU-4201-PSI                 01.57
              MOVE VU-4201-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE 'L'                TO VU-4232-PSI                 01.64
                 MOVE VU-4232-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE 'L'                TO VU-4231-PSI                 01.64
                 MOVE VU-4231-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-485-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-ECOM-PREF-HC-FLG.
      *
           IF (KDA-VC-AUTH-CHAR-U AND
               KDA-VC-PHONE-ORD-5) OR
              (KDA-VC-AUTH-CHAR-S AND
               KDA-VC-PHONE-ORD-6)
                 NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V010'          TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF NOT KDA-VC-POS-ENTRY-KEYED
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-4
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF PET-TOTAL-AUTH-AMT = +0
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T004'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF NOT KDA-VC-SPEC-COND-IND-2-US-LDG
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V014'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF NOT KDA-HAS-LG
              IF NOT WS-CHECK-QUAL-ONLY
                 IF KDA-LG-HAD-ERRORS
                    MOVE 'A003'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'A001'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF KDA-NOT-PQ-VS-USA-HC-PRF
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'A002'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF NOT KDA-VC-MKT-SPC-AUTH-HTL-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V011'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF NOT KDA-VC-ECGI-DP
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V018'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF PET-PURCHASE-IDENTIFIER NOT = '4'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V012'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF PET-PURCHASE-IDENT = SPACES
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V013'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
           IF PET-DESCR-FLAG NOT = 'Y'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T034'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-485-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-ECOM-PREF-HC-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-REIMB-SW
                                            WS-CPS-REWARDS-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4201-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-4201-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4232-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4232-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-4231-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4231-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
           MOVE WS-CPS-PSI               TO WS-CPS-ECOM-PREF-HC-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-ECOM-PREF-HC-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-485-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-485-EXIT.
           EXIT.
      *
       I-490-SET-CPS-E-COMM-PREF-C.
      *
      * SET RATE TABLE VALUES FOR USA CPS E-COMMERCE CAR RENTAL
      *    PREFERRED RATE.
      *
      *    4201 VISA USA CPS HOTEL/CAR PREFERRED
      *    4231 VISA USA CPS HOTEL/CAR PREFERRED - DEBIT
      *    4232 VISA USA CPS HOTEL/CAR PREFERRED - PREPAID
      *
           IF NOT KDA-MERCAT-CAR-RENT
              GO TO I-490-EXIT
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-490-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              KDA-VS-PID-IS-SIGNATURE OR                                01.98
              KDA-VS-PID-IS-INFINITE                                    01.98
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE 'M'                   TO VU-4201-PSI                 01.57
              MOVE VU-4201-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE 'M'                TO VU-4232-PSI                 01.64
                 MOVE VU-4232-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE 'M'                TO VU-4231-PSI                 01.64
                 MOVE VU-4231-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-490-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-ECOM-PREF-HC-FLG.
      *
           IF (KDA-VC-AUTH-CHAR-U AND
               KDA-VC-PHONE-ORD-5) OR
              (KDA-VC-AUTH-CHAR-S AND
               KDA-VC-PHONE-ORD-6)
                  NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V010'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
           IF NOT KDA-VC-POS-ENTRY-KEYED
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-4
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
           IF PET-TOTAL-AUTH-AMT = +0
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T004'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
           IF NOT KDA-HAS-CA
              IF NOT WS-CHECK-QUAL-ONLY
                 IF KDA-CA-HAD-ERRORS
                    MOVE 'A003'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'A001'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
           IF KDA-NOT-PQ-VS-USA-HC-PRF
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'A002'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
           IF NOT KDA-VC-MKT-SPC-AUTH-CAR-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V011'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
           IF NOT KDA-VC-ECGI-DP
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V018'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
           IF PET-PURCHASE-IDENTIFIER NOT = '3'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V012'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
           IF PET-PURCHASE-IDENT = SPACES
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V013'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
           IF PET-DESCR-FLAG NOT = 'Y'
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T034'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-490-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-ECOM-PREF-HC-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-REIMB-SW
                                            WS-CPS-REWARDS-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-4201-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-4201-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-4232-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4232-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-4231-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-4231-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
           MOVE WS-CPS-PSI               TO WS-CPS-ECOM-PREF-HC-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-ECOM-PREF-HC-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-490-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-490-EXIT.
           EXIT.
      *
       I-495-SET-CPS-RECUR-PAY.                                         02.07
      *                                                                 02.07
      *    4201 VISA USA CPS HOTEL/CAR PREFERRED                        02.07
      *                                                                 02.07
           IF NOT KDA-MRCH-TYP-RECUR-PAY                                02.07
              GO TO I-495-EXIT.                                         02.07
      *                                                                 02.07
           IF KDA-VS-PID-IS-TRADITIONAL OR                              02.07
              KDA-VS-PID-IS-TRD-REWARD  OR                              02.07
              KDA-VS-PID-IS-SIGNATURE   OR                              02.07
             (KDA-VS-PID-IS-INFINITE AND WS-IS-INFINITE-NONQUAL)        02.07
                CONTINUE                                                02.07
           ELSE                                                         02.07
              GO TO I-495-EXIT.                                         02.07
      *                                                                 02.07
           MOVE VU-0832-PLN           TO WS-PLAN-DG                     02.07
                                         WS-PLAN-QUAL-CODE-X.           02.07
      *                                                                 02.07
           PERFORM S-525-CHECK-PLAN-LOW                                 02.07
              THRU S-525-EXIT.                                          02.07
           IF NOT WS-RATE-IS-LOWER                                      02.07
              GO TO I-495-EXIT.                                         02.07
      *                                                                 02.07
           MOVE 'A'                      TO WS-CPS-RECUR-PAY-FLG.       02.07
      *                                                                 02.07
           IF KDA-VC-PHONE-ORD-US-RECUR                                 02.07
                  NEXT SENTENCE                                         02.07
           ELSE                                                         02.07
              IF NOT WS-CHECK-QUAL-ONLY                                 02.07
                 MOVE 'V010'             TO WS-ERROR-DG                 02.07
                 PERFORM S-300-PROCESS-DOWNGRADE                        02.07
                    THRU S-300-EXIT                                     02.07
              END-IF                                                    02.07
              GO TO I-495-EXIT.                                         02.07
           IF KDA-AUTH-ERR                                              02.07
              IF NOT WS-CHECK-QUAL-ONLY                                 02.07
                 IF PET-AUTH-CD = SPACES                                02.07
                    MOVE 'T001'          TO WS-ERROR-DG                 02.07
                 ELSE                                                   02.07
                    MOVE 'T003'          TO WS-ERROR-DG                 02.07
                 END-IF                                                 02.07
                 PERFORM S-300-PROCESS-DOWNGRADE                        02.07
                    THRU S-300-EXIT                                     02.07
              END-IF                                                    02.07
              GO TO I-495-EXIT.                                         02.07
           IF NOT KDA-VC-POS-ENTRY-KEYED                                02.07
              IF NOT WS-CHECK-QUAL-ONLY                                 02.07
                 MOVE 'V001'             TO WS-ERROR-DG                 02.07
                 PERFORM S-300-PROCESS-DOWNGRADE                        02.07
                    THRU S-300-EXIT                                     02.07
              END-IF                                                    02.07
              GO TO I-495-EXIT.                                         02.07
           IF NOT KDA-VC-MKT-SPC-AUTH-BILL-PAY                          02.07
              IF NOT WS-CHECK-QUAL-ONLY                                 02.07
                 MOVE 'V002'             TO WS-ERROR-DG                 02.07
                 PERFORM S-300-PROCESS-DOWNGRADE                        02.07
                    THRU S-300-EXIT                                     02.07
              END-IF                                                    02.07
              GO TO I-495-EXIT.                                         02.07
           IF PET-DESCR-FLAG NOT = 'Y'                                  02.07
              IF NOT WS-CHECK-QUAL-ONLY                                 02.07
                 MOVE 'T034'             TO WS-ERROR-DG                 02.07
                 PERFORM S-300-PROCESS-DOWNGRADE                        02.07
                    THRU S-300-EXIT                                     02.07
              END-IF                                                    02.07
              GO TO I-495-EXIT.                                         02.07
      *                                                                 02.07
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.                02.07
           MOVE PET-DATE                 TO DC-SEND-DATE.               02.07
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.                02.07
           MOVE 'DAK'                    TO DC-OPERATION.               02.07
           PERFORM D-100-CALL-DATECONV                                  02.07
              THRU D-100-EXIT.                                          02.07
           IF DC-NOT-SUCCESSFUL OR                                      02.07
              DC-RESULT = 03 OR                                         02.07
            ((DC-RESULT = 01 OR 02) AND                                 02.07
              DC-RETURN-NUM > 1)                                        02.07
              IF NOT WS-CHECK-QUAL-ONLY                                 02.07
                 MOVE 'T033'                TO WS-ERROR-DG              02.07
                 PERFORM S-300-PROCESS-DOWNGRADE                        02.07
                    THRU S-300-EXIT                                     02.07
              END-IF                                                    02.07
              GO TO I-495-EXIT                                          02.07
           END-IF.                                                      02.07
      *                                                                 02.07
           MOVE PET-DATE                 TO DC-SEND-DATE.               02.07
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.                02.07
           MOVE 'DAJ'                    TO DC-OPERATION.               02.07
           PERFORM D-100-CALL-DATECONV                                  02.07
              THRU D-100-EXIT.                                          02.07
           IF DC-RESULT = 00 OR 01                                      02.07
              NEXT SENTENCE                                             02.07
           ELSE                                                         02.07
              IF NOT WS-CHECK-QUAL-ONLY                                 02.07
                 MOVE 'T032'             TO WS-ERROR-DG                 02.07
                 PERFORM S-300-PROCESS-DOWNGRADE                        02.07
                    THRU S-300-EXIT                                     02.07
              END-IF                                                    02.07
              GO TO I-495-EXIT.                                         02.07
      *                                                                 02.07
           MOVE 'Y'                      TO WS-CPS-RECUR-PAY-FLG        02.07
                                            WS-CPS-REGULATED-FLG        02.07
                                            WS-CPS-REIMB-SW             02.07
                                            WS-CPS-REWARDS-SW.          02.07
      *                                                                 02.07
           MOVE VU-0832-PSI              TO WS-CPS-PSI.                 02.07
           MOVE VU-0832-RMB              TO WS-CPS-REIMB.               02.07
           MOVE WS-CPS-PSI               TO WS-CPS-RECUR-PAY-PSI.       02.07
           MOVE WS-CPS-REIMB             TO WS-CPS-RECUR-PAY-RMB.       02.07
      *                                                                 02.07
           PERFORM S-500-SET-PLAN-DATA                                  02.07
              THRU S-500-EXIT.                                          02.07
      *                                                                 02.07
       I-495-EXIT.                                                      02.07
           EXIT.                                                        02.07
      *                                                                 02.07
       I-500-SET-CPS-SUPERMARKET.
      *
      * SET RATE TABLE VALUES FOR USA SUPERMARKET
      *
      *    4832 VISA USA CPS SUPERMARKET - PREPAID MAX                  01.64
      *    7001 VISA USA CPS SUPERMARKET
      *    7031 VISA USA CPS SUPERMARKET - DEBIT
      *    7032 VISA USA CPS SUPERMARKET - PREPAID                      01.64
      *
           IF NOT KDA-MRCH-TYP-SPR-MKT
              GO TO I-500-EXIT.
      *
           IF KDA-VC-PROG-REGIST-NO
              GO TO I-500-EXIT.
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              IF NOT KDA-VC-PROG-REGIST-CREDIT
                 GO TO I-500-EXIT
              END-IF
           ELSE
              IF NOT KDA-VC-PROG-REGIST-DEBIT
                 GO TO I-500-EXIT.
      *
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              KDA-VS-PID-IS-SIGNATURE OR                                01.84
              KDA-VS-PID-IS-INFINITE                                    01.84
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-7001-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-7032-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-7031-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-500-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-SPR-MKT-FLG.
      *
           IF NOT KDA-CHD-AR-IS-FND-SRC-CREDIT                          01.84
              IF NOT KDA-VC-AUTH-CHAR-E
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V015'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-500-EXIT.
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-500-EXIT.
           IF NOT KDA-VC-POS-ENTRY-MODE-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-500-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-1-3
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-500-EXIT.
           IF NOT KDA-CHD-AR-IS-FND-SRC-CREDIT                          01.84
              IF PET-AUTHORIZ-AMT NOT = PET-ACQ-AMOUNT
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V009'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-500-EXIT.
           IF KDA-VC-MKT-SPC-AUTH-BILL-PAY                              01.56
              IF NOT KDA-VC-PHONE-ORD-BPAY-1-3                          01.56
                 MOVE 'V002'             TO WS-ERROR-DG                 01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
                 GO TO I-500-EXIT                                       01.56
              END-IF                                                    01.56
           END-IF.                                                      01.56
      *                                                                 01.56
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
            ((DC-RESULT = 01 OR 02) AND                                 01.56
              DC-RETURN-NUM > 1)                                        01.56
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-500-EXIT                                          01.56
           END-IF.
      *
           MOVE VU-7001-PSI              TO WS-LRG-TKT-SW.              01.57
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'                TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-500-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-SPR-MKT-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-REWARDS-SW
                                            WS-CPS-REIMB-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-7001-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-7001-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-7032-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-7032-RMB        TO WS-CPS-REIMB                01.64
              ELSE                                                      01.64
                 MOVE VU-7031-PSI        TO WS-CPS-PSI                  01.64
                 MOVE VU-7031-RMB        TO WS-CPS-REIMB                01.64
              END-IF                                                    01.64
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-SPR-MKT-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-SPR-MKT-RMB.
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-500-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-500-EXIT.
           EXIT.
      *
       I-530-SET-CPS-RETAIL.
      *
      * SET RATE TABLE VALUES FOR USA CPS RETAIL
      *
      *    3501 VISA USA CPS RETAIL
      *    3531 VISA USA CPS RETAIL - DEBIT
      *    3532 VISA USA CPS RETAIL - PREPAID                           01.65
      *
           IF KDA-MRCH-TYP-SPR-MKT
              GO TO I-530-EXIT.
           IF KDA-MRCH-TYP-SVC-STATION
              GO TO I-530-EXIT.
           IF KDA-MRCH-TYP-CPS-REST
              GO TO I-530-EXIT.
      *
           IF NOT KDA-VS-PID-IS-COMMERCIAL AND                          01.84
              NOT KDA-CHD-AR-IS-FND-SRC-CREDIT AND                      01.84
              KDA-MRCH-TYP-T-AND-E                                      01.77
              GO TO I-530-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.55
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-CHD-AR-IS-REGULATED OR                                01.64
             (KDA-CHD-AR-IS-FND-SRC-CREDIT AND                          01.84
              KDA-MRCH-TYP-CHARITY) OR                                  01.77
              KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              WS-IS-DEBT-REPAY OR
              WS-IS-GOV-HE-PAY OR                                       02.86
              WS-IS-UTILITY OR                                          01.61
              KDA-MRCH-TYP-CPS-REST OR
              KDA-VS-PID-IS-SIGNATURE OR                                01.84
              KDA-VS-PID-IS-INFINITE                                    01.84
                 MOVE 'Y'                TO WS-CHECK-QUAL-ONLY-SW
           ELSE
              MOVE 'N'                   TO WS-CHECK-QUAL-ONLY-SW.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-3501-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-3532-PLN        TO WS-PLAN-DG                  01.65
                                            WS-PLAN-QUAL-CODE-X         01.65
              ELSE                                                      01.65
                 MOVE VU-3531-PLN        TO WS-PLAN-DG                  01.65
                                            WS-PLAN-QUAL-CODE-X         01.65
              END-IF                                                    01.65
           END-IF.                                                      01.65
      *
           IF NOT WS-CHECK-QUAL-ONLY
              PERFORM S-525-CHECK-PLAN-LOW
                 THRU S-525-EXIT
              IF NOT WS-RATE-IS-LOWER
                 GO TO I-530-EXIT.
      *
           MOVE 'A'                      TO WS-CPS-RTL-FLG.
           IF WS-IS-RTL-2-MRCH                                          01.77
              MOVE 'A'                   TO WS-CPS-RTL-2-FLG.           01.77
      *
           IF NOT KDA-CHD-AR-IS-FND-SRC-CREDIT                          01.84
              IF NOT KDA-VC-AUTH-CHAR-E
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V015'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT.
           IF KDA-AUTH-ERR
              IF NOT WS-CHECK-QUAL-ONLY
                 IF PET-AUTH-CD = SPACES
                    MOVE 'T001'          TO WS-ERROR-DG
                 ELSE
                    MOVE 'T003'          TO WS-ERROR-DG
                 END-IF
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-530-EXIT.
           IF NOT KDA-VC-CARDHLDR-ID-1-3
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V006'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-530-EXIT.
           IF NOT KDA-VC-POS-ENTRY-MODE-CPS
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'V001'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-530-EXIT.
           IF NOT KDA-CHD-AR-IS-FND-SRC-CREDIT                          01.84
              IF NOT KDA-MRCH-TYP-DBT-NOMTCH-OK
                 IF PET-AUTHORIZ-AMT NOT = PET-ACQ-AMOUNT
                    IF NOT WS-CHECK-QUAL-ONLY
                       MOVE 'V009'          TO WS-ERROR-DG
                       PERFORM S-300-PROCESS-DOWNGRADE
                          THRU S-300-EXIT
                    END-IF
                    GO TO I-530-EXIT
                 END-IF
              END-IF
           END-IF.
           IF KDA-MERCAT-CAR-RENT
              IF NOT KDA-HAS-CA
                 IF NOT WS-CHECK-QUAL-ONLY
                    IF KDA-CA-HAD-ERRORS
                       MOVE 'A003'       TO WS-ERROR-DG
                    ELSE
                       MOVE 'A001'       TO WS-ERROR-DG
                    END-IF
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
              IF KDA-NOT-PQ-VS-USA-RTL
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'A002'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
              IF NOT KDA-VC-MKT-SPC-AUTH-CAR-CPS
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V011'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
              IF PET-PURCHASE-IDENTIFIER NOT = '3'
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V012'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
              IF PET-PURCHASE-IDENT = SPACES
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V013'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
           END-IF.
           IF KDA-MERCAT-LODGING
              IF NOT KDA-HAS-LG
                 IF NOT WS-CHECK-QUAL-ONLY
                    IF KDA-LG-HAD-ERRORS
                       MOVE 'A003'       TO WS-ERROR-DG
                    ELSE
                       MOVE 'A001'       TO WS-ERROR-DG
                    END-IF
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
              IF KDA-NOT-PQ-VS-USA-RTL
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'A002'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
              IF NOT KDA-VC-MKT-SPC-AUTH-HTL-CPS
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V011'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
              IF PET-PURCHASE-IDENTIFIER NOT = '4'
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V012'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
              IF PET-PURCHASE-IDENT = SPACES
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'V013'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
           END-IF.
           IF KDA-MERCAT-AIR
              IF NOT KDA-HAS-AI AND                                     01.98
                 NOT KDA-HAS-AN                                         01.98
                 IF NOT WS-CHECK-QUAL-ONLY
                    IF KDA-AI-HAD-ERRORS
                       MOVE 'A003'       TO WS-ERROR-DG
                    ELSE
                       MOVE 'A001'       TO WS-ERROR-DG
                    END-IF
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
              IF KDA-NOT-PQ-VS-USA-RTL
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'A002'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
              IF PET-DESCR-FLAG NOT = 'Y'
                 IF NOT WS-CHECK-QUAL-ONLY
                    MOVE 'T034'          TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
                 END-IF
                 GO TO I-530-EXIT
              END-IF
           END-IF.
      *
           IF KDA-VC-MKT-SPC-AUTH-BILL-PAY                              01.56
              IF NOT KDA-VC-PHONE-ORD-BPAY-1-3                          01.56
                 MOVE 'V002'             TO WS-ERROR-DG                 01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
                 GO TO I-530-EXIT                                       01.56
              END-IF                                                    01.56
           END-IF.                                                      01.56
      *                                                                 01.56
           MOVE PET-TRANS-IDNTFIER       TO WS-TRANS-ID.
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE WS-TRANS-ID-YDDD         TO DC-SEND-NUM.
           MOVE 'DAK'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-NOT-SUCCESSFUL OR                                      01.56
              DC-RESULT = 03 OR                                         01.56
            ((DC-RESULT = 01 OR 02) AND                                 01.56
              DC-RETURN-NUM > 1)                                        01.56
              IF NOT WS-CHECK-QUAL-ONLY                                 01.56
                 MOVE 'T033'                TO WS-ERROR-DG              01.56
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.56
                    THRU S-300-EXIT                                     01.56
              END-IF                                                    01.56
              GO TO I-530-EXIT                                          01.56
           END-IF.
      *
           MOVE VU-3501-PSI              TO WS-LRG-TKT-SW.              01.57
      *
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-02-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              IF NOT WS-CHECK-QUAL-ONLY
                 MOVE 'T032'             TO WS-ERROR-DG
                 PERFORM S-300-PROCESS-DOWNGRADE
                    THRU S-300-EXIT
              END-IF
              GO TO I-530-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-RTL-FLG
                                            WS-CPS-REGULATED-FLG        01.64
                                            WS-CPS-CHARITY-FLG          01.77
                                            WS-CPS-REIMB-SW
                                            WS-CPS-RTL-2-FLG            01.77
                                            WS-CPS-REWARDS-SW.
           MOVE VU-3501-PSI              TO WS-GSA-LRG-TKT-SW.          01.57
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-3501-PSI           TO WS-CPS-PSI                  01.57
              MOVE VU-3501-RMB           TO WS-CPS-REIMB                01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-3532-PSI        TO WS-CPS-PSI                  01.65
                 MOVE VU-3532-RMB        TO WS-CPS-REIMB                01.65
              ELSE                                                      01.65
                 MOVE VU-3531-PSI        TO WS-CPS-PSI                  01.65
                 MOVE VU-3531-RMB        TO WS-CPS-REIMB                01.65
              END-IF                                                    01.65
           END-IF.
           MOVE WS-CPS-PSI               TO WS-CPS-RTL-PSI.
           MOVE WS-CPS-REIMB             TO WS-CPS-RTL-RMB.
           IF WS-IS-RTL-2-MRCH                                          01.64
              MOVE WS-CPS-PSI            TO WS-CPS-RTL-2-PSI            01.77
              MOVE WS-CPS-REIMB          TO WS-CPS-RTL-2-RMB.           01.77
      *
           IF WS-CHECK-QUAL-ONLY
              GO TO I-530-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-530-EXIT.
           EXIT.
      *
       I-600-SET-CPS-RETAIL-2.
      *
      * SET RATE TABLE VALUES FOR USA CPS RETAIL 2
      *    (SELECTED CPS CATEGORIES)                                    01.64
      *
      *    2931 VISA USA CPS RETAIL 2 - DEBIT MAX                       01.64
      *    2932 VISA USA CPS RETAIL 2 - PREPAID MAX                     01.64
      *    3001 VISA USA CPS RETAIL 2                                   01.64
      *    3031 VISA USA CPS RETAIL 2 - DEBIT                           01.64
      *    3032 VISA USA CPS RETAIL 2 - PREPAID                         01.64
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT AND                          01.84
              KDA-MRCH-TYP-CHARITY                                      01.68
                 GO TO I-600-EXIT.                                      01.68
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              IF KDA-MRCH-TYP-RETAIL-2                                  01.64
                 NEXT SENTENCE                                          01.64
              ELSE                                                      01.64
                 GO TO I-600-EXIT                                       01.64
              END-IF                                                    01.64
           ELSE                                                         01.64
              IF KDA-MRCH-TYP-RTL-2-REG                                 01.64
                 NEXT SENTENCE                                          01.64
              ELSE                                                      01.64
                 GO TO I-600-EXIT                                       01.64
              END-IF                                                    01.64
           END-IF.                                                      01.64
      *                                                                 01.64
           IF WS-QUASI-CASH                                             01.77
              GO TO I-600-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-COMMERCIAL OR                               01.84
              WS-IS-SIG-PREF-CREDIT OR                                  01.98
              WS-IS-INFINITE-QUAL                                       01.98
                 GO TO I-600-EXIT.
      *                                                                 01.78
           IF WS-CPS-RTL-2-FLG NOT = 'Y'
              GO TO I-600-EXIT.
      *
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              IF WS-CPS-RTL-FLG = 'Y'        OR                         01.78
                 WS-CPS-RTL-KEY-FLG = 'Y'    OR                         01.78
                 WS-CPS-RTL-CNP-FLG = 'Y'    OR                         01.78
                 WS-CPS-ECOM-BASIC-FLG = 'Y' OR                         01.78
                 WS-CPS-ECOM-PREF-FLG  = 'Y'                            01.78
                    CONTINUE                                            01.78
               ELSE                                                     01.78
                    GO TO I-600-EXIT                                    01.78
               END-IF                                                   01.78
           ELSE                                                         01.78
           IF WS-CPS-RTL-CNP-FLG = 'Y'    OR                            01.78
              WS-CPS-ECOM-BASIC-FLG = 'Y' OR                            01.78
              WS-CPS-ECOM-PREF-FLG = 'Y'                                01.78
              CONTINUE                                                  01.78
           ELSE                                                         01.78
              GO TO I-600-EXIT                                          01.78
           END-IF                                                       01.78
           END-IF.                                                      01.78
      *                                                                 01.78
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE VU-3001-PLN           TO WS-PLAN-DG                  01.57
                                            WS-PLAN-QUAL-CODE-X
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-3032-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              ELSE                                                      01.64
                 MOVE VU-3031-PLN        TO WS-PLAN-DG                  01.64
                                            WS-PLAN-QUAL-CODE-X         01.64
              END-IF                                                    01.64
           END-IF.
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF NOT WS-RATE-IS-LOWER
              GO TO I-600-EXIT.
      *
           IF KDA-CHD-AR-IS-REGULATED                                   01.66
              MOVE 'Y'                   TO WS-CPS-REGULATED-FLG        01.66
              MOVE WS-CPS-RTL-2-PSI      TO WS-CPS-PSI                  01.66
              MOVE WS-CPS-RTL-2-RMB      TO WS-CPS-REIMB                01.66
              GO TO I-600-EXIT                                          01.66
           END-IF.                                                      01.66
      *                                                                 01.66
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              MOVE WS-CPS-RTL-2-PSI      TO VU-3001-PSI                 01.57
              MOVE WS-CPS-RTL-2-RMB      TO VU-3001-RMB                 01.57
           ELSE
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE WS-CPS-RTL-2-PSI   TO VU-2932-PSI                 01.64
                                            VU-3032-PSI                 01.64
                 MOVE WS-CPS-RTL-2-RMB   TO VU-2932-RMB                 01.64
                                            VU-3032-RMB                 01.64
              ELSE                                                      01.64
                 MOVE WS-CPS-RTL-2-PSI   TO VU-2931-PSI                 01.64
                                            VU-3031-PSI                 01.64
                 MOVE WS-CPS-RTL-2-RMB   TO VU-2931-RMB                 01.64
                                            VU-3031-RMB                 01.64
              END-IF                                                    01.64
           END-IF.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-600-EXIT.
           EXIT.
      *
       I-605-SET-CPS-CHARITY.                                           01.68
      *                                                                 01.68
      * SET RATE TABLE VALUES FOR USA CPS CHARITY                       01.68
      *    (SELECTED CPS CATEGORIES)                                    01.68
      *                                                                 01.68
      *    2901 VISA USA CPS CHARITY - CONSUMER CREDIT                  01.70
      *                                                                 01.68
           IF KDA-VS-PID-IS-COMMERCIAL                                  01.84
              GO TO I-605-EXIT.                                         01.70
      *                                                                 01.70
           IF WS-QUASI-CASH                                             01.77
              GO TO I-605-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT AND                          01.84
              KDA-MRCH-TYP-CHARITY AND                                  01.68
              WS-IS-CPS-CHARITY                                         01.68
                 NEXT SENTENCE                                          01.68
           ELSE                                                         01.68
              GO TO I-605-EXIT                                          01.68
           END-IF.                                                      01.68
      *                                                                 01.68
           MOVE VU-2901-PLN              TO WS-PLAN-DG                  01.68
                                            WS-PLAN-QUAL-CODE-X.        01.68
           PERFORM S-525-CHECK-PLAN-LOW                                 01.68
              THRU S-525-EXIT.                                          01.68
           IF NOT WS-RATE-IS-LOWER                                      01.68
              GO TO I-605-EXIT.                                         01.68
      *                                                                 01.68
           IF WS-CPS-RTL-FLG = 'A' OR                                   01.77
              WS-CPS-RTL-CNP-FLG = 'A' OR                               01.68
              WS-CPS-ECOM-BASIC-FLG = 'A' OR                            01.68
              WS-CPS-ECOM-PREF-FLG = 'A' OR                             01.68
              WS-CPS-RTL-KEY-FLG = 'A'                                  01.68
                 MOVE 'V020'             TO WS-ERROR-DG                 01.68
                 PERFORM S-300-PROCESS-DOWNGRADE                        01.68
                    THRU S-300-EXIT                                     01.68
              GO TO I-605-EXIT                                          01.68
           END-IF.                                                      01.68
      *                                                                 01.68
           IF WS-CPS-RTL-FLG = 'Y'                                      01.77
              MOVE WS-CPS-RTL-PSI        TO VU-2901-PSI                 01.77
              MOVE WS-CPS-RTL-RMB        TO VU-2901-RMB                 01.77
              GO TO I-605-SET-PLAN-DATA                                 01.68
           END-IF.                                                      01.68
           IF WS-CPS-RTL-CNP-FLG = 'Y'                                  01.68
              MOVE WS-CPS-RTL-CNP-PSI    TO VU-2901-PSI                 01.68
              MOVE WS-CPS-RTL-CNP-RMB    TO VU-2901-RMB                 01.68
              GO TO I-605-SET-PLAN-DATA                                 01.68
           END-IF.                                                      01.68
           IF WS-CPS-ECOM-PREF-FLG = 'Y'                                01.68
              MOVE WS-CPS-ECOM-PREF-PSI  TO VU-2901-PSI                 01.68
              MOVE WS-CPS-ECOM-PREF-RMB  TO VU-2901-RMB                 01.68
              GO TO I-605-SET-PLAN-DATA                                 01.68
           END-IF.                                                      01.68
           IF WS-CPS-ECOM-BASIC-FLG = 'Y'                               01.68
              MOVE WS-CPS-ECOM-BASIC-PSI TO VU-2901-PSI                 01.68
              MOVE WS-CPS-ECOM-BASIC-RMB TO VU-2901-RMB                 01.68
              GO TO I-605-SET-PLAN-DATA                                 01.68
           END-IF.                                                      01.68
           IF WS-CPS-RTL-KEY-FLG = 'Y'                                  01.68
              MOVE WS-CPS-RTL-KEY-PSI    TO VU-2901-PSI                 01.68
              MOVE WS-CPS-RTL-KEY-RMB    TO VU-2901-RMB                 01.68
              GO TO I-605-SET-PLAN-DATA                                 01.68
           END-IF.                                                      01.68
      *                                                                 01.68
           GO TO I-605-EXIT.                                            01.68
      *                                                                 01.68
       I-605-SET-PLAN-DATA.                                             01.68
      *                                                                 01.68
           PERFORM S-500-SET-PLAN-DATA                                  01.68
              THRU S-500-EXIT.                                          01.68
      *                                                                 01.68
       I-605-EXIT.                                                      01.68
           EXIT.                                                        01.68
      *                                                                 01.68
       I-700-SET-CPS-REWARDS.
      *
      * SET RATE TABLE VALUES FOR USA CPS REWARDS
      *
      *    4601 VISA USA CPS REWARDS 1
      *    4701 VISA USA CPS REWARDS 2
      *
           IF WS-IS-PT-TSC AND
             (KDA-VS-PID-IS-SIGNATURE OR                                01.84
              KDA-VS-PID-IS-INFINITE)                                   01.84
                 GO TO I-700-EXIT.
           IF KDA-VS-PID-IS-TRD-REWARD OR                               01.84
              KDA-VS-PID-IS-SIGNATURE OR                                01.84
             (KDA-VS-PID-IS-INFINITE AND NOT WS-IS-INFINITE-QUAL)       03.40V
                 NEXT SENTENCE
           ELSE
              GO TO I-700-EXIT.
           IF WS-IS-UTILITY                                             01.61
              GO TO I-700-EXIT.                                         01.61
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT AND                          01.84
              KDA-MRCH-TYP-CHARITY                                      01.68
                 GO TO I-700-EXIT.                                      01.68
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-700-EXIT.                                         01.77
      *                                                                 01.77
           IF WS-IS-CPS-REGULATED AND                                   01.66
              KDA-CHD-AR-IS-REGULATED AND                               01.66
              WS-IS-CPS                                                 01.66
                 MOVE VU-2531-PLN        TO WS-PLAN-DG                  01.66
                                            WS-PLAN-QUAL-CODE-X         01.66
           ELSE                                                         01.66
              MOVE VU-4601-PLN           TO WS-PLAN-DG                  01.66
                                            WS-PLAN-QUAL-CODE-X         01.66
           END-IF.                                                      01.66
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF WS-RATE-IS-LOWER
              IF WS-CPS-RTL-FLG = 'A' OR
                 WS-CPS-SPR-MKT-FLG = 'A'
                    MOVE 'V020'           TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
              ELSE
                 IF WS-CPS-RTL-FLG = 'Y'
                    MOVE WS-CPS-RTL-PSI  TO VU-4601-PSI                 01.57
                    MOVE WS-CPS-RTL-RMB  TO VU-4601-RMB                 01.57
                 END-IF
                 IF WS-CPS-SPR-MKT-FLG = 'Y'
                    MOVE WS-CPS-SPR-MKT-PSI
                                         TO VU-4601-PSI                 01.57
                    MOVE WS-CPS-SPR-MKT-RMB
                                         TO VU-4601-RMB                 01.57
                 END-IF
                 IF WS-CPS-RTL-FLG = 'Y' OR
                    WS-CPS-SPR-MKT-FLG = 'Y'
                       IF WS-IS-CPS-REGULATED AND                       01.66
                          KDA-CHD-AR-IS-REGULATED AND                   01.66
                          WS-IS-CPS                                     01.66
                             MOVE VU-4601-PSI                           01.66
                                         TO WS-CPS-PSI                  01.66
                             MOVE VU-4601-RMB                           01.66
                                         TO WS-CPS-REIMB                01.66
                       ELSE                                             01.66
                          PERFORM S-500-SET-PLAN-DATA                   01.66
                             THRU S-500-EXIT                            01.66
                       END-IF                                           01.66
                 END-IF
              END-IF
           END-IF.
      *
           IF WS-IS-CPS-REGULATED AND                                   01.66
              KDA-CHD-AR-IS-REGULATED AND                               01.66
              WS-IS-CPS                                                 01.66
                 MOVE VU-2531-PLN        TO WS-PLAN-DG                  01.66
                                            WS-PLAN-QUAL-CODE-X         01.66
           ELSE                                                         01.66
              MOVE VU-4701-PLN           TO WS-PLAN-DG                  01.66
                                            WS-PLAN-QUAL-CODE-X         01.66
           END-IF.                                                      01.66
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF WS-RATE-IS-LOWER
              IF WS-CPS-RTL-RST-FLG = 'A' OR
                 WS-CPS-RTL-CNP-FLG = 'A' OR
                 WS-CPS-ECOM-BASIC-FLG = 'A' OR
                 WS-CPS-ECOM-PREF-HC-FLG = 'A' OR
                 WS-CPS-ECOM-PREF-PT-FLG = 'A' OR
                 WS-CPS-HC-CP-FLG = 'A' OR
                 WS-CPS-HC-CNP-FLG = 'A' OR
                 WS-CPS-PT-CNP-FLG = 'A' OR                             01.77
                 WS-CPS-PT-CP-FLG = 'A' OR                              01.77
                 WS-CPS-RTL-KEY-FLG = 'A'
                    MOVE 'V020'           TO WS-ERROR-DG
                    PERFORM S-300-PROCESS-DOWNGRADE
                       THRU S-300-EXIT
              ELSE
                 IF WS-CPS-RTL-RST-FLG = 'Y'
                    MOVE WS-CPS-RTL-RST-PSI
                                         TO VU-4701-PSI                 01.57
                    MOVE WS-CPS-RTL-RST-RMB
                                         TO VU-4701-RMB                 01.57
                 END-IF
                 IF WS-CPS-RTL-CNP-FLG = 'Y'
                    MOVE WS-CPS-RTL-CNP-PSI
                                         TO VU-4701-PSI                 01.57
                    MOVE WS-CPS-RTL-CNP-RMB
                                         TO VU-4701-RMB                 01.57
                 END-IF
                 IF WS-CPS-ECOM-BASIC-FLG = 'Y'
                    MOVE WS-CPS-ECOM-BASIC-PSI
                                         TO VU-4701-PSI                 01.57
                    MOVE WS-CPS-ECOM-BASIC-RMB
                                         TO VU-4701-RMB                 01.57
                 END-IF
                 IF WS-CPS-ECOM-PREF-HC-FLG = 'Y'
                    MOVE WS-CPS-ECOM-PREF-HC-PSI
                                         TO VU-4701-PSI                 01.57
                    MOVE WS-CPS-ECOM-PREF-HC-RMB
                                         TO VU-4701-RMB                 01.57
                 END-IF
                 IF WS-CPS-ECOM-PREF-PT-FLG = 'Y'
                    MOVE WS-CPS-ECOM-PREF-PT-PSI
                                         TO VU-4701-PSI                 01.57
                    MOVE WS-CPS-ECOM-PREF-PT-RMB
                                         TO VU-4701-RMB                 01.57
                 END-IF
                 IF WS-CPS-HC-CP-FLG = 'Y'
                    MOVE WS-CPS-HC-CP-PSI
                                         TO VU-4701-PSI                 01.57
                    MOVE WS-CPS-HC-CP-RMB
                                         TO VU-4701-RMB                 01.57
                 END-IF
                 IF WS-CPS-HC-CNP-FLG = 'Y'
                    MOVE WS-CPS-HC-CNP-PSI
                                         TO VU-4701-PSI                 01.57
                    MOVE WS-CPS-HC-CNP-RMB
                                         TO VU-4701-RMB                 01.57
                 END-IF
                 IF WS-CPS-PT-CNP-FLG = 'Y'                             01.77
                 OR WS-CPS-PT-CP-FLG = 'Y'                              01.77
                    MOVE WS-CPS-PT-PSI   TO VU-4701-PSI                 01.57
                    MOVE WS-CPS-PT-RMB   TO VU-4701-RMB                 01.57
                 END-IF
                 IF WS-CPS-RTL-KEY-FLG = 'Y'
                    MOVE WS-CPS-RTL-KEY-PSI
                                         TO VU-4701-PSI                 01.57
                    MOVE WS-CPS-RTL-KEY-RMB
                                         TO VU-4701-RMB                 01.57
                 END-IF
                 IF WS-CPS-RTL-RST-FLG = 'Y' OR
                    WS-CPS-RTL-CNP-FLG = 'Y' OR
                    WS-CPS-ECOM-BASIC-FLG = 'Y' OR
                    WS-CPS-ECOM-PREF-HC-FLG = 'Y' OR
                    WS-CPS-ECOM-PREF-PT-FLG = 'Y' OR
                    WS-CPS-HC-CP-FLG = 'Y' OR
                    WS-CPS-HC-CNP-FLG = 'Y' OR
                    WS-CPS-PT-CNP-FLG = 'Y' OR                          01.77
                    WS-CPS-PT-CP-FLG = 'Y' OR                           01.77
                    WS-CPS-RTL-KEY-FLG = 'Y'
                       IF WS-IS-CPS-REGULATED AND                       01.66
                          KDA-CHD-AR-IS-REGULATED AND                   01.66
                          WS-IS-CPS                                     01.66
                             MOVE VU-4701-PSI                           01.66
                                         TO WS-CPS-PSI                  01.66
                             MOVE VU-4701-RMB                           01.66
                                         TO WS-CPS-REIMB                01.66
                       ELSE                                             01.66
                          PERFORM S-500-SET-PLAN-DATA                   01.66
                             THRU S-500-EXIT                            01.66
                       END-IF                                           01.66
                 END-IF
              END-IF
           END-IF.
      *
       I-700-EXIT.
           EXIT.
      *                                                                 01.48
       I-705-SET-CPS-DEBT-REPAY.                                        01.48
      *                                                                 01.48
      * SET RATE TABLE VALUES FOR USA CPS DEBT REPAY                    01.48
      *                                                                 01.48
      *    2031 VISA USA CPS DEBT REPAY                                 01.48
      *    2531 VISA USA CPS REGULATED - DEBIT                          01.64
      *    2532 VISA USA CPS REGULATED - PREPAID                        01.64
      *                                                                 01.48
           IF NOT WS-IS-DEBT-REPAY                                      01.48
              GO TO I-705-EXIT.                                         01.48
           IF PET-VISA-MVV = SPACES                                     01.48
              GO TO I-705-EXIT.                                         01.48
           IF NOT KDA-VS-PID-IS-COMMERCIAL AND                          02.07
             (NOT KDA-CHD-AR-IS-FND-SRC-CREDIT)                         01.84
                 NEXT SENTENCE                                          01.48
           ELSE                                                         01.48
              GO TO I-705-EXIT                                          01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           IF WS-QUASI-CASH                                             01.77
              GO TO I-705-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-CHD-AR-IS-REGULATED                                   01.64
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE WS-CPS-PSI            TO VU-2532-PSI              01.64
                 MOVE WS-CPS-REIMB          TO VU-2532-RMB              01.64
                 MOVE VU-2532-PLN           TO WS-PLAN-DG               01.64
                                               WS-PLAN-QUAL-CODE-X      01.64
              ELSE                                                      01.64
                 MOVE WS-CPS-PSI            TO VU-2531-PSI              01.64
                 MOVE WS-CPS-REIMB          TO VU-2531-RMB              01.64
                 MOVE VU-2531-PLN           TO WS-PLAN-DG               01.64
                                               WS-PLAN-QUAL-CODE-X      01.64
              END-IF                                                    01.64
           ELSE                                                         01.64
              MOVE WS-CPS-PSI               TO VU-2031-PSI              01.64
              MOVE WS-CPS-REIMB             TO VU-2031-RMB              01.64
              MOVE VU-2031-PLN              TO WS-PLAN-DG               01.64
                                               WS-PLAN-QUAL-CODE-X      01.64
           END-IF.                                                      01.64
      *                                                                 01.48
           PERFORM S-525-CHECK-PLAN-LOW                                 01.48
              THRU S-525-EXIT.                                          01.48
           IF NOT WS-RATE-IS-LOWER                                      01.48
              GO TO I-705-EXIT.                                         01.48
      *                                                                 01.48
           IF WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.77
              WS-CPS-ECOM-BASIC-FLG = 'Y' OR                            01.48
              WS-CPS-ECOM-PREF-FLG = 'Y'                                01.48
                 NEXT SENTENCE                                          01.48
           ELSE                                                         01.48
              MOVE 'V020'                TO WS-ERROR-DG                 01.48
              PERFORM S-300-PROCESS-DOWNGRADE                           01.48
                 THRU S-300-EXIT                                        01.48
              GO TO I-705-EXIT                                          01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           IF NOT KDA-VC-PHONE-ORD-BILL-PAY                             01.48
              MOVE 'V002'                TO WS-ERROR-DG                 01.48
              PERFORM S-300-PROCESS-DOWNGRADE                           01.48
                 THRU S-300-EXIT                                        01.48
              GO TO I-705-EXIT                                          01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           IF NOT KDA-VC-MKT-SPC-AUTH-BILL-PAY                          01.48
              MOVE 'V011'                TO WS-ERROR-DG                 01.48
              PERFORM S-300-PROCESS-DOWNGRADE                           01.48
                 THRU S-300-EXIT                                        01.48
              GO TO I-705-EXIT                                          01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           PERFORM S-500-SET-PLAN-DATA                                  01.48
              THRU S-500-EXIT.                                          01.48
      *                                                                 01.48
       I-705-EXIT.                                                      01.48
           EXIT.                                                        01.48
      *                                                                 01.48
       I-710-SET-CPS-GOV-HE-PAY.                                        01.86
      *                                                                 01.48
      * SET RATE TABLE VALUES FOR USA CPS GOV/HE PAYMENT                01.86
      *                                                                 01.48
      *    0731 VISA USA CPS GOV/HE PAYMENT 1 - DEBIT                   01.86
      *    2531 VISA USA CPS REGULATED - DEBIT                          01.64
      *    2532 VISA USA CPS REGULATED - PREPAID                        01.64
      *                                                                 01.48
           IF NOT WS-IS-GOV-HE-PAY                                      01.86
              GO TO I-710-EXIT.                                         01.48
      *                                                                 01.48
           IF WS-QUASI-CASH                                             01.77
              GO TO I-710-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-CHD-AR-IS-REGULATED                                   01.69
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE WS-CPS-PSI         TO VU-2532-PSI                 01.69
                 MOVE WS-CPS-REIMB       TO VU-2532-RMB                 01.69
                 MOVE VU-2532-PLN        TO WS-PLAN-DG                  01.69
                                               WS-PLAN-QUAL-CODE-X      01.69
              ELSE                                                      01.69
                 MOVE WS-CPS-PSI         TO VU-2531-PSI                 01.69
                 MOVE WS-CPS-REIMB       TO VU-2531-RMB                 01.69
                 MOVE VU-2531-PLN        TO WS-PLAN-DG                  01.69
                                               WS-PLAN-QUAL-CODE-X      01.69
              END-IF                                                    01.69
           ELSE                                                         01.69
                 MOVE WS-CPS-PSI         TO VU-0731-PSI                 02.07
                 MOVE WS-CPS-REIMB       TO VU-0731-RMB                 02.07
                 MOVE VU-0731-PLN        TO WS-PLAN-DG                  02.07
                                            WS-PLAN-QUAL-CODE-X         01.69
           END-IF.                                                      01.48
      *                                                                 01.48
           PERFORM S-525-CHECK-PLAN-LOW                                 02.07
              THRU S-525-EXIT.                                          02.07
           IF NOT WS-RATE-IS-LOWER                                      02.07
              GO TO I-710-EXIT.                                         02.07
      *                                                                 02.07
           IF WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.77
              WS-CPS-ECOM-BASIC-FLG = 'Y' OR                            01.77
              WS-CPS-ECOM-PREF-FLG = 'Y'                                01.77
                CONTINUE                                                01.77
           ELSE                                                         01.77
              MOVE 'V020'                TO WS-ERROR-DG                 01.48
              PERFORM S-300-PROCESS-DOWNGRADE                           01.48
                 THRU S-300-EXIT                                        01.48
              GO TO I-710-EXIT                                          01.48
           END-IF.                                                      01.48
      *                                                                 01.48
           MOVE 'Y'                      TO WS-USE-GOV-HE-PAY-RATE-SW.  01.86
      *                                                                 01.48
           PERFORM S-500-SET-PLAN-DATA                                  01.48
              THRU S-500-EXIT.                                          01.48
      *                                                                 01.48
       I-710-EXIT.                                                      01.48
           EXIT.                                                        01.48
      *
       I-715-SET-VPP.                                                   02.06
      *                                                                 02.06
      * SET RATE TABLE VALUES FOR USA PARTNER PROGRAM (VPP)             02.06
      *                                                                 02.06
      *    0190 VISA USA PARTNER PROGRAM (VPP) - CONSUMER               02.06
      *    0290 VISA USA PARTNER PROGRAM (VPP) - COMMERCIAL             02.06
      *                                                                 02.06
           MOVE 'N'                      TO KDA-VS-VPP-TRAN-SW          02.06
           IF KDA-MM-VS-VPP-IND = SPACE                                 02.06
             GO TO I-715-EXIT                                           02.06
           END-IF.                                                      02.06
           MOVE WS-CPS-REIMB-SW          TO VISA-CPS-REIMB-SW           02.06
           MOVE 'V'                      TO BASVPP01-CALL-TYPE          02.06
           MOVE 'BAS110VU'               TO BASVPP01-SD-CALLING-PGM     02.06
           PERFORM Z-982-CALL-BASVPP01 THRU                             02.06
                      Z-982-EXIT.                                       02.06
      *                                                                 02.06
           IF BASVPP01-RC-FATAL-ERROR                                   02.06
               GO TO I-715-EXIT.                                        02.06
           IF BASVPP01-RD-VPP-SW = 'N'                                  02.06
               GO TO I-715-EXIT.                                        02.06
           MOVE BASVPP01-RD-VPP-SW       TO KDA-VS-VPP-TRAN-SW.         02.06
           IF KDA-MM-VS-VPP-IND = 'A'                                   02.06
               IF KDA-VS-PID-IS-VPP-CONSUMER                            02.06
                   MOVE VU-0190-PLN      TO WS-PLAN-DG                  02.06
                                            WS-PLAN-QUAL-CODE-X         02.06
               ELSE                                                     02.06
                   IF KDA-VS-PID-IS-VPP-COMMERCL                        02.06
                       MOVE VU-0290-PLN  TO WS-PLAN-DG                  02.06
                                         WS-PLAN-QUAL-CODE-X            02.06
                   END-IF                                               02.06
               END-IF                                                   02.06
           END-IF.                                                      02.06
      *                                                                 02.06
           PERFORM S-500-SET-PLAN-DATA                                  02.06
              THRU S-500-EXIT.                                          02.06
      *                                                                 02.06
       I-715-EXIT.                                                      02.06
           EXIT.                                                        02.06
      *                                                                 02.06
       I-805-SET-UTILITY.
      *
      * SET RATE TABLE VALUES FOR USA UTILITY
      *
      *    2521 VISA USA CPS REGULATED BUSINESS - DEBIT                 01.64
      *    2531 VISA USA CPS REGULATED - DEBIT                          01.64
      *    2532 VISA USA CPS REGULATED - PREPAID                        01.64
      *    4901 VISA USA UTILITY
      *    4921 VISA USA UTILITY - BUSINESS
      *    4931 VISA USA UTILITY - DEBIT
      *    4932 VISA USA UTILITY - BUSINESS DEBIT/PREPAID               01.86
      *    4941 VISA USA UTILITY - SIGNATURE PREFERRED/INFINITE         01.98
      *
           IF NOT KDA-MRCH-TYP-UTILITY
              GO TO I-805-EXIT.
           IF PET-VISA-MVV = SPACES
              GO TO I-805-EXIT.
           IF KDA-VS-PID-IS-BUS-ENH OR                                  01.84
              KDA-VS-PID-IS-BUSINESS                                    01.84
                 NEXT SENTENCE                                          01.81
           ELSE
              IF KDA-VS-PID-IS-COMMERCIAL                               01.84
                 GO TO I-805-EXIT.
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-805-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-VS-PID-IS-BUS-ENH OR                                  01.84
              KDA-VS-PID-IS-BUSINESS                                    01.84
              IF KDA-CHD-AR-IS-REGULATED                                01.64
                 MOVE WS-CPS-PSI         TO VU-2521-PSI                 01.64
                 MOVE WS-CPS-REIMB       TO VU-2521-RMB                 01.64
                 MOVE VU-2521-PLN        TO WS-PLAN-DG                  01.64
              ELSE                                                      01.86
              IF KDA-VS-PID-IS-BUS AND                                  01.86
                (KDA-CHD-AR-IS-FND-SRC-DEBIT OR                         01.86
                 KDA-CHD-AR-IS-FND-SRC-PREPAID)                         01.86
                 MOVE WS-CPS-PSI         TO VU-4932-PSI                 01.86
                 MOVE WS-CPS-REIMB       TO VU-4932-RMB                 01.86
                 MOVE VU-4932-PLN        TO WS-PLAN-DG                  01.86
              ELSE                                                      01.64
                 MOVE WS-CPS-PSI         TO VU-4921-PSI                 01.64
                 MOVE WS-CPS-REIMB       TO VU-4921-RMB                 01.64
                 MOVE VU-4921-PLN        TO WS-PLAN-DG                  01.64
              END-IF                                                    01.64
              END-IF                                                    01.86
           ELSE
              IF KDA-CHD-AR-IS-REGULATED                                01.64
                 IF KDA-CHD-AR-IS-FND-SRC-PREPAID                       01.84
                    MOVE WS-CPS-PSI      TO VU-2532-PSI                 01.64
                    MOVE WS-CPS-REIMB    TO VU-2532-RMB                 01.64
                    MOVE VU-2532-PLN     TO WS-PLAN-DG                  01.64
                 ELSE                                                   01.64
                    MOVE WS-CPS-PSI      TO VU-2531-PSI                 01.64
                    MOVE WS-CPS-REIMB    TO VU-2531-RMB                 01.64
                    MOVE VU-2531-PLN     TO WS-PLAN-DG                  01.64
                 END-IF                                                 01.64
              ELSE                                                      01.64
                 IF KDA-CHD-AR-IS-FND-SRC-CREDIT                        01.84
                    IF WS-IS-SIG-PREF-CREDIT                            01.64
                    OR WS-IS-INFINITE-QUAL                              01.98
                       MOVE WS-CPS-PSI   TO VU-4941-PSI                 01.64
                       MOVE WS-CPS-REIMB TO VU-4941-RMB                 01.64
                       MOVE VU-4941-PLN  TO WS-PLAN-DG                  01.64
                    ELSE                                                01.64
                       MOVE WS-CPS-PSI   TO VU-4901-PSI                 01.64
                       MOVE WS-CPS-REIMB TO VU-4901-RMB                 01.64
                       MOVE VU-4901-PLN  TO WS-PLAN-DG                  01.64
                    END-IF                                              01.64
                 ELSE                                                   01.64
                    MOVE WS-CPS-PSI      TO VU-4931-PSI                 01.64
                    MOVE WS-CPS-REIMB    TO VU-4931-RMB                 01.64
                    MOVE VU-4931-PLN     TO WS-PLAN-DG                  01.64
                 END-IF                                                 01.64
              END-IF                                                    01.64
           END-IF.
      *
           PERFORM S-525-CHECK-PLAN-LOW                                 02.07
              THRU S-525-EXIT.                                          02.07
           IF NOT WS-RATE-IS-LOWER                                      02.07
              GO TO I-805-EXIT.                                         02.07
      *                                                                 02.07
           IF WS-CPS-RTL-CNP-FLG = 'Y' OR                               01.81
              WS-CPS-ECOM-BASIC-FLG = 'Y' OR                            01.81
              WS-CPS-ECOM-PREF-FLG = 'Y'                                01.81
                 GO TO I-805-SET-PLAN-DATA                              01.81
           END-IF.                                                      01.81
      *                                                                 01.81
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              IF WS-CPS-SML-TKT-FLG = 'Y' OR                            01.81
                 WS-CPS-RTL-FLG = 'Y' OR                                01.81
                 WS-CPS-RTL-KEY-FLG = 'Y'                               01.81
                    GO TO I-805-SET-PLAN-DATA                           01.81
              END-IF                                                    01.81
           END-IF.                                                      01.81
      *                                                                 01.81
           MOVE 'V020'                   TO WS-ERROR-DG.                01.81
           PERFORM S-300-PROCESS-DOWNGRADE                              01.81
              THRU S-300-EXIT.                                          01.81
      *                                                                 01.81
           GO TO I-805-EXIT.                                            01.81
      *                                                                 01.81
       I-805-SET-PLAN-DATA.                                             01.81
      *                                                                 01.61
           MOVE WS-PLAN-DG               TO WS-PLAN-QUAL-CODE-X.        01.82
      *                                                                 01.82
           IF KDA-CHD-AR-IS-REGULATED AND                               01.81
             (KDA-VS-PID-IS-BUSINESS OR                                 01.84
              KDA-VS-PID-IS-BUS-ENH OR                                  01.84
              KDA-CHD-AR-IS-FND-SRC-PREPAID)                            01.84
                 MOVE 'Y'                TO WS-CPS-REGULATED-FLG        01.64
           END-IF.                                                      01.64
      *                                                                 01.61
           IF WS-IS-SIG-PREF-CREDIT OR                                  01.61
              WS-IS-INFINITE-QUAL OR                                    01.98
              KDA-VS-PID-IS-SIGNATURE OR                                01.84
              KDA-VS-PID-IS-INFINITE                                    01.84
                 MOVE 'Y'                TO WS-SIG-UTIL-SW              01.61
           END-IF.                                                      01.61
      *                                                                 01.61
           IF WS-IS-SIG-PREF-CREDIT OR                                  01.61
              WS-IS-INFINITE-QUAL OR                                    01.98
            ((KDA-VS-PID-IS-BUS-ENH OR                                  01.84
              KDA-VS-PID-IS-BUSINESS) AND                               01.84
             (NOT KDA-CHD-AR-IS-REGULATED))                             01.64
                 MOVE 'Y'                TO WS-USE-UTILITY-RATE-SW      01.61
           END-IF.                                                      01.61
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-805-EXIT.
           EXIT.
      *
       I-810-SET-GSA-LARGE-TICKET.
      *
      * SET RATE TABLE VALUES FOR USA GSA LARGE TICKET
      *
      *    0821 VISA USA GSA LARGE TICKET - 0
      *    2921 VISA USA LARGE TICKET ADVANTAGE 1                       02.05
      *    3921 VISA USA LARGE TICKET ADVANTAGE 2                       02.05
      *    4021 VISA USA LARGE TICKET ADVANTAGE 3                       02.05
      *    4121 VISA USA LARGE TICKET ADVANTAGE 4                       02.05
      *
           IF NOT KDA-VS-PID-IS-GSA-PRCH AND                            02.05
              NOT KDA-VS-PID-IS-GSA-PRCH-FLT                            02.05
              GO TO I-810-EXIT.
           IF WS-IS-PT-TSC
              GO TO I-810-EXIT.
      *
           IF WS-QUASI-CASH                                             01.77
              GO TO I-810-EXIT.                                         01.77
      *                                                                 01.77
           IF KDA-CHD-IS-VLPA                                           02.05
              IF KDA-TRAN-USD-AMT > +500000.000                         02.05
                 MOVE VU-4121-PLN              TO WS-PLAN-DG            02.05
                                                  WS-PLAN-QUAL-CODE-X   02.05
                 MOVE WS-LRG-TKT-ADV-SW        TO VU-4121-PSI           02.05
              END-IF                                                    02.05
              IF KDA-TRAN-USD-AMT > +100000.000 AND                     02.05
                 KDA-TRAN-USD-AMT < +500000.010                         02.05
                 MOVE VU-4021-PLN              TO WS-PLAN-DG            02.05
                                                  WS-PLAN-QUAL-CODE-X   02.05
                 MOVE WS-LRG-TKT-ADV-SW        TO VU-4021-PSI           02.05
              END-IF                                                    02.05
              IF KDA-TRAN-USD-AMT > +25000.000 AND                      02.05
                 KDA-TRAN-USD-AMT < +100000.010                         02.05
                 MOVE VU-3921-PLN              TO WS-PLAN-DG            02.05
                                                  WS-PLAN-QUAL-CODE-X   02.05
                 MOVE WS-LRG-TKT-ADV-SW        TO VU-3921-PSI           02.05
              END-IF                                                    02.05
              IF KDA-TRAN-USD-AMT > +10000.000 AND                      02.05
                 KDA-TRAN-USD-AMT < +25000.010                          02.05
                 MOVE VU-2921-PLN              TO WS-PLAN-DG            02.05
                                                  WS-PLAN-QUAL-CODE-X   02.05
                 MOVE WS-LRG-TKT-ADV-SW        TO VU-2921-PSI           02.05
              END-IF                                                    02.05
           ELSE                                                         02.05
              MOVE VU-0821-PLN                 TO WS-PLAN-DG            02.05
                                                  WS-PLAN-QUAL-CODE-X   02.05
              MOVE WS-GSA-LRG-TKT-SW           TO VU-0821-PSI           02.05
           END-IF.                                                      02.05
      *                                                                 02.05
           IF WS-PLAN-QUAL-CODE-X = VU-2921-PLN OR VU-3921-PLN OR       02.06
                                    VU-4021-PLN OR VU-4121-PLN          02.06
              IF KDA-VS-VPP-TRAN-SW = 'Y'                               02.06
                  GO TO I-810-CHECK-LOW-RATE-DONE                       02.06
              END-IF                                                    02.06
           END-IF.                                                      02.06
      *                                                                 02.06
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF NOT WS-RATE-IS-LOWER
              GO TO I-810-EXIT.
      *
       I-810-CHECK-LOW-RATE-DONE.                                       02.06
      *                                                                 02.06
           IF (WS-NOT-GSA-LRG-TKT AND                                   02.05
                                  WS-PLAN-QUAL-CODE-X = VU-0821-PLN) OR 02.05
              (WS-NOT-LRG-TKT-ADV AND                                   02.05
                                  WS-PLAN-QUAL-CODE-X = VU-4121-PLN) OR 02.05
              (WS-NOT-LRG-TKT-ADV AND                                   02.05
                                  WS-PLAN-QUAL-CODE-X = VU-4021-PLN) OR 02.05
              (WS-NOT-LRG-TKT-ADV AND                                   02.05
                                  WS-PLAN-QUAL-CODE-X = VU-3921-PLN) OR 02.05
              (WS-NOT-LRG-TKT-ADV AND                                   02.05
                                  WS-PLAN-QUAL-CODE-X = VU-2921-PLN)    02.05
              MOVE 'V020'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-810-EXIT.
      *                                                                 02.05
           IF WS-PLAN-QUAL-CODE-X NOT = VU-0821-PLN                     02.05
              GO TO I-810-CHECK-LRG-TKT-ADVANTAGE.                      02.05
      *                                                                 02.05
           IF NOT KDA-HAS-LU
              IF KDA-LU-HAD-ERRORS
                 MOVE 'A003'             TO WS-ERROR-DG
              ELSE
                 MOVE 'A001'             TO WS-ERROR-DG
              END-IF
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-810-EXIT.
           IF KDA-HAS-PA AND
              KDA-HAS-PL
                 NEXT SENTENCE
           ELSE
              IF KDA-PA-HAD-ERRORS OR
                 KDA-PL-HAD-ERRORS
                    MOVE 'A003'          TO WS-ERROR-DG
              ELSE
                 MOVE 'A001'             TO WS-ERROR-DG
              END-IF
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-810-EXIT.
           IF KDA-NOT-PQ-VS-USA-LRG-TKT
              MOVE 'A002'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-810-EXIT.
      *
           GO TO I-810-SET-PLAN.                                        02.05
      *                                                                 02.05
       I-810-CHECK-LRG-TKT-ADVANTAGE.                                   02.05
      *                                                                 02.05
           MOVE PET-DATE                 TO DC-SEND-DATE.               02.05
           MOVE VSD-08-DAYS-CUTOFF       TO DC-SEND-NUM.                02.05
           MOVE 'DAJ'                    TO DC-OPERATION.               02.05
           PERFORM D-100-CALL-DATECONV                                  02.05
              THRU D-100-EXIT.                                          02.05
           IF DC-RESULT = 00 OR 01                                      02.05
              NEXT SENTENCE                                             02.05
           ELSE                                                         02.05
              MOVE 'T032'                TO WS-ERROR-DG                 02.05
              PERFORM S-300-PROCESS-DOWNGRADE                           02.05
                 THRU S-300-EXIT                                        02.05
              GO TO I-810-EXIT.                                         02.05
      *                                                                 02.05
       I-810-SET-PLAN.                                                  02.05
      *                                                                 02.05
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-810-EXIT.
           EXIT.
      *
       I-820-SET-LARGE-TICKET.
      *
      * SET RATE TABLE VALUES FOR USA PURCHASING LARGE TICKET
      *
      *    5921 VISA USA LARGE TICKET TIER 0 (NON-GSA)                  02.05
      *    2921 VISA USA LARGE TICKET ADVANTAGE 1                       02.05
      *    3921 VISA USA LARGE TICKET ADVANTAGE 2                       02.05
      *    4021 VISA USA LARGE TICKET ADVANTAGE 3                       02.05
      *    4121 VISA USA LARGE TICKET ADVANTAGE 4                       02.05
      *    5832 VISA USA LARGE TICKET - PURCHASE - PREPAID (NON-GSA)    02.05
      *
           IF WS-IS-PT-TSC
              GO TO I-820-EXIT.
           IF WS-QUASI-CASH                                             01.77
              GO TO I-820-EXIT.                                         01.77
      *                                                                 01.77
           IF NOT KDA-VS-PID-IS-PURCHASING AND                          01.86
              NOT KDA-VS-PID-IS-PRCH-FLT                                02.05
              GO TO I-820-EXIT.                                         01.84
      *                                                                 02.05
           IF KDA-CHD-AR-IS-FND-SRC-PREPAID                             01.86
              MOVE VU-5832-PLN                 TO WS-PLAN-DG            01.86
                                               WS-PLAN-QUAL-CODE-X      01.86
              MOVE WS-LRG-TKT-PURCH-SW         TO VU-5832-PSI           01.86
           ELSE                                                         02.05
           IF KDA-CHD-IS-VLPA                                           01.84
              IF KDA-TRAN-USD-AMT > +500000.000                         01.77
                 MOVE VU-4121-PLN              TO WS-PLAN-DG            01.77
                                                  WS-PLAN-QUAL-CODE-X   01.77
                 MOVE WS-LRG-TKT-ADV-SW        TO VU-4121-PSI           01.77
              END-IF                                                    01.84
              IF KDA-TRAN-USD-AMT > +100000.000 AND                     01.77
                 KDA-TRAN-USD-AMT < +500000.010                         01.77
                 MOVE VU-4021-PLN              TO WS-PLAN-DG            01.77
                                                  WS-PLAN-QUAL-CODE-X   01.77
                 MOVE WS-LRG-TKT-ADV-SW        TO VU-4021-PSI           01.77
              END-IF                                                    01.84
              IF KDA-TRAN-USD-AMT > +25000.000 AND                      01.77
                 KDA-TRAN-USD-AMT < +100000.010                         01.77
                 MOVE VU-3921-PLN              TO WS-PLAN-DG            01.77
                                                  WS-PLAN-QUAL-CODE-X   01.77
                 MOVE WS-LRG-TKT-ADV-SW        TO VU-3921-PSI           01.77
              END-IF                                                    01.84
              IF KDA-TRAN-USD-AMT > +10000.000 AND                      01.77
                 KDA-TRAN-USD-AMT < +25000.010                          01.77
                 MOVE VU-2921-PLN              TO WS-PLAN-DG            01.77
                                                  WS-PLAN-QUAL-CODE-X   01.77
                 MOVE WS-LRG-TKT-ADV-SW        TO VU-2921-PSI           01.77
              END-IF                                                    01.84
           ELSE                                                         02.05
              MOVE VU-5921-PLN                 TO WS-PLAN-DG            02.05
                                                  WS-PLAN-QUAL-CODE-X   02.05
              MOVE WS-LRG-TKT-SW               TO VU-5921-PSI           02.05
           END-IF                                                       02.05
           END-IF.                                                      01.84
      *                                                                 01.84
           IF WS-PLAN-QUAL-CODE-X = VU-2921-PLN OR VU-3921-PLN OR       02.06
                                    VU-4021-PLN OR VU-4121-PLN          02.06
              IF KDA-VS-VPP-TRAN-SW = 'Y'                               02.06
                  GO TO I-820-CHECK-LOW-RATE-DONE                       02.06
              END-IF                                                    02.06
           END-IF.                                                      02.06
      *                                                                 02.06
           PERFORM S-525-CHECK-PLAN-LOW
              THRU S-525-EXIT.
           IF NOT WS-RATE-IS-LOWER
              GO TO I-820-EXIT.
      *
       I-820-CHECK-LOW-RATE-DONE.                                       02.06
      *                                                                 02.06
           IF (WS-NOT-LRG-TKT AND WS-PLAN-QUAL-CODE-X = VU-5921-PLN) OR 01.86
              (WS-NOT-LRG-TKT-PURCH AND                                 01.86
                                  WS-PLAN-QUAL-CODE-X = VU-5832-PLN) OR 02.05
              (WS-NOT-LRG-TKT-ADV AND                                   02.05
                                  WS-PLAN-QUAL-CODE-X = VU-4121-PLN) OR 02.05
              (WS-NOT-LRG-TKT-ADV AND                                   02.05
                                  WS-PLAN-QUAL-CODE-X = VU-4021-PLN) OR 02.05
              (WS-NOT-LRG-TKT-ADV AND                                   02.05
                                  WS-PLAN-QUAL-CODE-X = VU-3921-PLN) OR 02.05
              (WS-NOT-LRG-TKT-ADV AND                                   02.05
                                  WS-PLAN-QUAL-CODE-X = VU-2921-PLN)    02.05
              MOVE 'V020'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-820-EXIT.
      *
           IF WS-PLAN-QUAL-CODE-X NOT = VU-5921-PLN AND                 02.05
              WS-PLAN-QUAL-CODE-X NOT = VU-5832-PLN                     02.05
              GO TO I-820-CHECK-LRG-TKT-ADVANTAGE.                      02.05
      *                                                                 02.05
           IF NOT KDA-HAS-LU
              IF KDA-LU-HAD-ERRORS
                 MOVE 'A003'             TO WS-ERROR-DG
              ELSE
                 MOVE 'A001'             TO WS-ERROR-DG
              END-IF
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-820-EXIT.
           IF KDA-HAS-PA AND
              KDA-HAS-PL
                 NEXT SENTENCE
           ELSE
              IF KDA-PA-HAD-ERRORS OR
                 KDA-PL-HAD-ERRORS
                    MOVE 'A003'          TO WS-ERROR-DG
              ELSE
                 MOVE 'A001'             TO WS-ERROR-DG
              END-IF
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-820-EXIT.
      *
           IF KDA-NOT-PQ-VS-USA-LRG-TKT
              MOVE 'A002'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-820-EXIT.
      *
       I-820-CHECK-LRG-TKT-ADVANTAGE.                                   01.77
      *                                                                 01.77
           MOVE PET-DATE                 TO DC-SEND-DATE.
           MOVE VSD-08-DAYS-CUTOFF       TO DC-SEND-NUM.
           MOVE 'DAJ'                    TO DC-OPERATION.
           PERFORM D-100-CALL-DATECONV
              THRU D-100-EXIT.
           IF DC-RESULT = 00 OR 01
              NEXT SENTENCE
           ELSE
              MOVE 'T032'                TO WS-ERROR-DG
              PERFORM S-300-PROCESS-DOWNGRADE
                 THRU S-300-EXIT
              GO TO I-820-EXIT.
      *
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-820-EXIT.
           EXIT.
      *
       I-830-SET-PRIVATE-LABEL.                                         01.93
      *                                                                 01.93
      * SET RATE TABLE VALUES FOR USA PURCHASING LARGE TICKET           01.93
      *                                                                 01.93
      *    0151 VISA PRIVATE LABEL BASIC                                01.93
      *    1151 VISA PRIVATE LABEL STANDARD                             01.93
      *    1251 VISA PRIVATE LABEL ENHANCED                             01.93
      *    1351 VISA PRIVATE LABEL SPECIALIZED                          01.93
      *    1451 VISA PRIVATE LABEL PREMIUM                              01.93
           IF KDA-VS-PID-IS-PVT-BASIC OR                                01.93
              KDA-VS-PID-IS-PVT-STANDARD OR                             01.93
              KDA-VS-PID-IS-PVT-ENHANCED OR                             01.93
              KDA-VS-PID-IS-PVT-SPECIAL OR                              01.93
              KDA-VS-PID-IS-PVT-PREMIUM                                 01.93
              NEXT SENTENCE                                             01.93
           ELSE                                                         01.93
              GO TO I-830-EXIT                                          01.93
           END-IF.                                                      01.93
           IF KDA-VS-PID-IS-PVT-BASIC                                   01.93
              MOVE VU-0151-PLN              TO WS-PLAN-DG               01.93
                                               WS-PLAN-QUAL-CODE-X      01.93
           END-IF.                                                      01.93
           IF KDA-VS-PID-IS-PVT-STANDARD                                01.93
              MOVE VU-1151-PLN              TO WS-PLAN-DG               01.93
                                               WS-PLAN-QUAL-CODE-X      01.93
           END-IF.                                                      01.93
           IF KDA-VS-PID-IS-PVT-ENHANCED                                01.93
              MOVE VU-1251-PLN              TO WS-PLAN-DG               01.93
                                               WS-PLAN-QUAL-CODE-X      01.93
           END-IF.                                                      01.93
           IF KDA-VS-PID-IS-PVT-SPECIAL                                 01.93
              MOVE VU-1351-PLN              TO WS-PLAN-DG               01.93
                                               WS-PLAN-QUAL-CODE-X      01.93
           END-IF.                                                      01.93
           IF KDA-VS-PID-IS-PVT-PREMIUM                                 01.93
              MOVE VU-1451-PLN              TO WS-PLAN-DG               01.93
                                               WS-PLAN-QUAL-CODE-X      01.93
           END-IF.                                                      01.93
           PERFORM S-500-SET-PLAN-DATA                                  01.93
              THRU S-500-EXIT.                                          01.93
      *                                                                 01.93
       I-830-EXIT.                                                      01.93
           EXIT.                                                        01.93
      *
       I-950-SET-RETURN.
      *
      * SET RATE TABLE VALUES FOR USA RETURNS
      *
      *    9001 VISA USA RETURN PASSENGER TRANSPORT                     01.69
      *    9031 VISA USA RETURN NON-PSSNGR TRN - DEBIT OR ANY REGULATED 01.69
      *    9032 VISA USA RETURN NON-PSSNGR TRN - PREPAID DBT OR REG     01.69
      *    9090 VISA USA RETURN PARTNER PROGRAM (VPP) - CONSUMER        02.06
      *    9101 VISA USA RETURN NON-PASSENGER TRANSPORT
      *    9121 VISA USA RETURN NON-PASSENGER TRANSPORT - COMMERCIAL
      *    9190 VISA USA RETURN PARTNER PROGRAM (VPP) - COMMERCIAL      02.06
      *    9201 VISA USA RETURN SELECT MOTO
      *    9301 VISA USA RETURN GSA PURCH, NON-PASS TRANSPORT 1         01.86
      *    9401 VISA USA RETURN GSA PURCH, NON-PASS TRANSPORT 2         01.86
      *    9501 VISA USA RETURN GSA PURCH, NON-PASS TRANSPORT 3         01.86
      *    9601 VISA USA RETURN GSA PURCH, NON-PASS TRANSPORT 4         01.86
      *    9701 VISA USA RETURN GSA PURCH, NON-PASS TRANSPORT 5         01.86
      *    9331 VISA USA RETURN NON-GSA PURCH, NON-PASS TRANSPORT 1     01.86
      *    9431 VISA USA RETURN NON-GSA PURCH, NON-PASS TRANSPORT 2     01.86
      *    9531 VISA USA RETURN NON-GSA PURCH, NON-PASS TRANSPORT 3     01.86
      *    9631 VISA USA RETURN NON-GSA PURCH, NON-PASS TRANSPORT 4     01.86
      *    9731 VISA USA RETURN NON-GSA PURCH, NON-PASS TRANSPORT 5     01.86
      *

           IF (KDA-MM-VS-VPP-IND = 'A')        AND                      02.06
              ((PET-VISA-MVV NOT= SPACES) AND                           02.06
              (PET-VISA-MVV NOT= ZEROS))       AND                      02.06
              (KDA-CHD-AR-IS-FND-SRC-CREDIT)   AND                      02.06
              (NOT KDA-CHD-AR-IS-REGULATED) AND                         02.06
              ((KDA-VS-PID-IS-VPP-CONSUMER) OR                          02.06
              (KDA-VS-PID-IS-VPP-COMMERCL))                             02.06
                  IF KDA-VS-PID-IS-VPP-CONSUMER                         02.06
                     MOVE VU-9090-PLN          TO WS-PLAN-DG            02.06
                                                  WS-PLAN-QUAL-CODE-X   02.06
                     MOVE '0'                  TO VU-9090-RMB           02.06
                     MOVE 'N'                  TO VU-9090-PSI           02.06
                     GO TO I-950-SET-PLAN-DATA                          02.06
                  ELSE                                                  02.06
                     MOVE VU-9190-PLN          TO WS-PLAN-DG            02.06
                                                  WS-PLAN-QUAL-CODE-X   02.06
                     MOVE '0'                  TO VU-9190-RMB           02.06
                     MOVE 'N'                  TO VU-9190-PSI           02.06
                     GO TO I-950-SET-PLAN-DATA                          02.06
                  END-IF                                                02.06
           END-IF.                                                      02.06

           IF KDA-VS-PID-IS-PVT-BASIC OR                                02.04
              KDA-VS-PID-IS-PVT-STANDARD OR                             02.04
              KDA-VS-PID-IS-PVT-ENHANCED OR                             02.04
              KDA-VS-PID-IS-PVT-SPECIAL OR                              02.04
              KDA-VS-PID-IS-PVT-PREMIUM                                 02.04
              PERFORM I-830-SET-PRIVATE-LABEL THRU I-830-EXIT           01.93
              GO TO I-950-EXIT                                          01.93
           END-IF.                                                      01.93
           IF NOT KDA-VS-PID-IS-PURCHASING AND                          01.86
              NOT KDA-VS-PID-IS-PRCH-FLT AND                            01.86
              NOT KDA-VS-PID-IS-GSA-PRCH AND                            01.86
              NOT KDA-VS-PID-IS-GSA-PRCH-FLT                            01.86
              GO TO I-950-SET-REGULAR-RATES.
      *
           IF KDA-MERCAT-AIR OR                                         01.92
              KDA-MRCH-TYP-PSNGR-RAIL                                   01.92
              GO TO I-950-SET-REGULAR-RATES.
      *
           IF KDA-VS-PID-IS-GSA-PRCH-ANY AND                            01.86
              KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.86
              IF KDA-TRAN-USD-AMT > +500000.000                         01.86
                 MOVE VU-9701-PLN              TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 MOVE '0'                      TO VU-9701-RMB           01.86
                 MOVE 'N'                      TO VU-9701-PSI           01.86
                 GO TO I-950-SET-PLAN-DATA                              01.86
              END-IF                                                    01.86
              IF KDA-TRAN-USD-AMT > +100000.000 AND                     01.86
                 KDA-TRAN-USD-AMT < +500000.010                         01.86
                 MOVE VU-9601-PLN              TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 MOVE '0'                      TO VU-9601-RMB           01.86
                 MOVE 'N'                      TO VU-9601-PSI           01.86
                 GO TO I-950-SET-PLAN-DATA                              01.86
              END-IF                                                    01.86
              IF KDA-TRAN-USD-AMT > +25000.000 AND                      01.86
                 KDA-TRAN-USD-AMT < +100000.010                         01.86
                 MOVE VU-9501-PLN              TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 MOVE '0'                      TO VU-9501-RMB           01.86
                 MOVE 'N'                      TO VU-9501-PSI           01.86
                 GO TO I-950-SET-PLAN-DATA                              01.86
              END-IF                                                    01.86
              IF KDA-TRAN-USD-AMT > +10000.000 AND                      01.86
                 KDA-TRAN-USD-AMT < +25000.010                          01.86
                 MOVE VU-9401-PLN              TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 MOVE '0'                      TO VU-9401-RMB           01.86
                 MOVE 'N'                      TO VU-9401-PSI           01.86
                 GO TO I-950-SET-PLAN-DATA                              01.86
              END-IF                                                    01.86
              IF KDA-TRAN-USD-AMT > +00000.000 AND                      01.86
                 KDA-TRAN-USD-AMT < +10000.010                          01.86
                 MOVE VU-9301-PLN              TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 MOVE '0'                      TO VU-9301-RMB           01.86
                 MOVE 'N'                      TO VU-9301-PSI           01.86
                 GO TO I-950-SET-PLAN-DATA                              01.86
              END-IF                                                    01.86
           END-IF.                                                      01.86
      *                                                                 01.86
           IF (KDA-VS-PID-IS-PURCHASING OR                              01.86
               KDA-VS-PID-IS-PRCH-FLT) AND                              01.86
               KDA-CHD-AR-IS-FND-SRC-CREDIT                             01.86
              IF KDA-TRAN-USD-AMT > +500000.000                         01.86
                 MOVE VU-9731-PLN              TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 MOVE '0'                      TO VU-9731-RMB           01.86
                 MOVE 'N'                      TO VU-9731-PSI           01.86
                 GO TO I-950-SET-PLAN-DATA                              01.86
              END-IF                                                    01.86
              IF KDA-TRAN-USD-AMT > +100000.000 AND                     01.86
                 KDA-TRAN-USD-AMT < +500000.010                         01.86
                 MOVE VU-9631-PLN              TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 MOVE '0'                      TO VU-9631-RMB           01.86
                 MOVE 'N'                      TO VU-9631-PSI           01.86
                 GO TO I-950-SET-PLAN-DATA                              01.86
              END-IF                                                    01.86
              IF KDA-TRAN-USD-AMT > +25000.000 AND                      01.86
                 KDA-TRAN-USD-AMT < +100000.010                         01.86
                 MOVE VU-9531-PLN              TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 MOVE '0'                      TO VU-9531-RMB           01.86
                 MOVE 'N'                      TO VU-9531-PSI           01.86
                 GO TO I-950-SET-PLAN-DATA                              01.86
              END-IF                                                    01.86
              IF KDA-TRAN-USD-AMT > +10000.000 AND                      01.86
                 KDA-TRAN-USD-AMT < +25000.010                          01.86
                 MOVE VU-9431-PLN              TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 MOVE '0'                      TO VU-9431-RMB           01.86
                 MOVE 'N'                      TO VU-9431-PSI           01.86
                 GO TO I-950-SET-PLAN-DATA                              01.86
              END-IF                                                    01.86
              IF KDA-TRAN-USD-AMT > +00000.000 AND                      01.86
                 KDA-TRAN-USD-AMT < +10000.010                          01.86
                 MOVE VU-9331-PLN              TO WS-PLAN-DG            01.86
                                                  WS-PLAN-QUAL-CODE-X   01.86
                 MOVE '0'                      TO VU-9331-RMB           01.86
                 MOVE 'N'                      TO VU-9331-PSI           01.86
                 GO TO I-950-SET-PLAN-DATA                              01.86
              END-IF                                                    01.86
           END-IF.                                                      01.86
      *
       I-950-SET-REGULAR-RATES.
      *
           IF KDA-CHD-AR-IS-REGULATED                                   01.69
              IF KDA-CHD-AR-IS-FND-SRC-PREPAID                          01.84
                 MOVE VU-9032-PLN        TO WS-PLAN-QUAL-CODE-X         01.69
              ELSE                                                      01.69
                 MOVE VU-9031-PLN        TO WS-PLAN-QUAL-CODE-X         01.69
              END-IF                                                    01.69
              GO TO I-950-SET-PLAN-DATA                                 01.69
           END-IF.                                                      01.69
      *                                                                 01.69
           IF KDA-VS-PID-IS-COMMERCIAL                                  01.84
              IF KDA-CHD-AR-IS-FND-SRC-CREDIT                           01.84
                 IF KDA-MERCAT-AIR OR                                   01.69
                    KDA-MRCH-TYP-PSNGR-RAIL                             01.69
                       MOVE VU-9001-PLN  TO WS-PLAN-QUAL-CODE-X         01.69
                 ELSE                                                   01.69
                       MOVE VU-9121-PLN  TO WS-PLAN-QUAL-CODE-X         01.69
                 END-IF                                                 01.69
              ELSE                                                      01.69
                 IF KDA-CHD-AR-IS-FND-SRC-PREPAID                       01.84
                    MOVE VU-9032-PLN     TO WS-PLAN-QUAL-CODE-X         01.69
                 ELSE                                                   01.69
                    MOVE VU-9031-PLN     TO WS-PLAN-QUAL-CODE-X         01.69
                 END-IF                                                 01.69
              END-IF                                                    01.69
              GO TO I-950-SET-PLAN-DATA                                 01.69
           END-IF.                                                      01.69
      *                                                                 01.69
           IF KDA-CHD-AR-IS-FND-SRC-CREDIT                              01.84
              IF KDA-VC-PHONE-ORD-MERCH AND                             01.69
                (NOT KDA-MRCH-TYP-HIGH-RSK-TELE)                        01.69
                    MOVE VU-9201-PLN     TO WS-PLAN-QUAL-CODE-X         01.69
              ELSE                                                      01.69
                 MOVE VU-9101-PLN        TO WS-PLAN-QUAL-CODE-X         01.69
              END-IF                                                    01.69
              GO TO I-950-SET-PLAN-DATA                                 01.69
           END-IF.                                                      01.69
      *                                                                 01.69
           IF KDA-CHD-AR-IS-FND-SRC-PREPAID                             01.84
              MOVE VU-9032-PLN           TO WS-PLAN-QUAL-CODE-X         01.69
           ELSE                                                         01.69
              MOVE VU-9031-PLN           TO WS-PLAN-QUAL-CODE-X         01.69
           END-IF.                                                      01.69
      *
       I-950-SET-PLAN-DATA.                                             01.69
      *                                                                 01.69
           PERFORM S-500-SET-PLAN-DATA
              THRU S-500-EXIT.
      *
       I-950-EXIT.
           EXIT.
      *
       S-300-PROCESS-DOWNGRADE.
      *
      * LOAD DOWNGRADE PLAN AND ERROR CODE
      *
           IF WS-SUB-DG < +11
              MOVE WS-PLAN-DG            TO KDA-DTE-PLAN-X(WS-SUB-DG)
              MOVE WS-ERROR-DG           TO KDA-DTE-CODE(WS-SUB-DG)
              MOVE WS-WORK-AMT           TO KDA-DTE-AMT(WS-SUB-DG)
              COMPUTE WS-SUB-DG = WS-SUB-DG + 1.
      *
       S-300-EXIT.
           EXIT.
      *
       S-500-SET-PLAN-DATA.
      *
      * SET RATE TABLE AMOUNT.
      *
           PERFORM S-510-CHECK-RATE-PLAN
              THRU S-510-EXIT.
      *
           COMPUTE WS-WORK-CALC-AMT = (KDA-TRAN-USD-AMT                 01.62
                                    * VU-RTE-DEC(VU-IDX))               01.62
                                    + VU-RTE-ITM(VU-IDX).               01.62
      *                                                                 01.62
           MOVE 'N'                      TO WS-MAX-USED-SW.             01.62
      *                                                                 01.62
           IF VU-RTE-MAX(VU-IDX) = +0                                   01.62
              MOVE WS-WORK-MAX-ALL-9S    TO WS-WORK-MAX-AMT             01.62
           ELSE                                                         01.62
              MOVE VU-RTE-MAX(VU-IDX)    TO WS-WORK-MAX-AMT             01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           IF WS-WORK-CALC-AMT < WS-WORK-MAX-AMT                        01.62
              COMPUTE WS-WORK-AMT = WS-WORK-CALC-AMT                    01.62
           ELSE                                                         01.62
              MOVE 'Y'                   TO WS-MAX-USED-SW              01.62
              COMPUTE WS-WORK-AMT = WS-WORK-MAX-AMT                     01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           IF WS-PLAN-CODE-FOUND                                        01.62
      *       BASE PLAN FOUND ON MERCHANT MASTER                        01.62
              GO TO S-500-BASE-PLAN-FOUND.                              01.62
      *                                                                 01.62
           IF KDA-IS-PASS-THRU-VS                                       01.62
      *       BASE PLAN NOT FOUND ON MERCHANT MASTER                    01.62
      *       CHECK FOR ADD                                             01.62
              GO TO S-500-PROCESS-PASS-THRU.                            01.62
      *                                                                 01.62
      * SET TO USE PLAN FOR INTERCHANGE ONLY                            01.62
      *                                                                 01.62
       S-500-NO-PASS-THRU.                                              01.62
      *                                                                 01.62
           MOVE 'N'                      TO WS-ADD-PLAN-SW.             01.62
           MOVE WS-WORK-AMT              TO WS-LOW-RATE-AMT             01.62
                                            WS-LOW-MRCH-AMT.            01.62
           MOVE VU-RTE-ITM(VU-IDX)       TO WS-LOW-ITEM-AMT.            01.62
           SET VU-RATE-IDX               TO VU-IDX.                     01.62
      *                                                                 01.62
           IF NOT KDA-MAX-PLAN-OTHER                                    01.62
      *       MIN/MAX PLANS NOT USED BY MERCHANT                        01.62
              GO TO S-500-EXIT                                          01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           IF NOT WS-MAX-USED                                           01.62
      *       MIN/MAX PLAN NOT USED BY CARD SCHEME                      01.62
              GO TO S-500-EXIT                                          01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           MOVE VU-RTE-MXP-X(VU-IDX)     TO WS-MIN-MAX-PLAN-CODE-X.     01.62
      *                                                                 01.62
           PERFORM S-540-FIND-MIN-MAX                                   01.62
              THRU S-540-EXIT.                                          01.62
           IF NOT WS-MIN-MAX-FOUND                                      01.62
      *       MIN/MAX PLAN NOT USED BY SYSTEM                           01.62
              GO TO S-500-EXIT                                          01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           MOVE WS-MIN-MAX-PLAN-CODE-X   TO WS-PLAN-QUAL-CODE-X         02.00
                                            WS-SEARCH-PLAN-X.           02.00
           PERFORM S-520-SEARCH-MRCH-PLAN                               02.00
              THRU S-520-EXIT.                                          02.00
      *                                                                 01.62
           PERFORM                                                      02.00
             VARYING WS-PSUB FROM +1 BY +1                              02.00
             UNTIL   WS-PSUB > PPA-PKG-CNT OR                           02.00
                     WS-PLAN-CODE-FOUND                                 02.00
               IF PPA-PPT-PLAN-CODE(WS-PSUB) = WS-PLAN-QUAL-CODE        02.00
                  MOVE PPA-PPT-PKG-CODE(WS-PSUB)                        02.00
                                         TO WS-SEARCH-PLAN              02.00
                  IF WS-CUSTOM-PLAN-X NOT = WS-SEARCH-PLAN-X            02.00
                     PERFORM S-520-SEARCH-MRCH-PLAN                     02.00
                        THRU S-520-EXIT                                 02.00
                     IF WS-PLAN-CODE-FOUND                              02.00
                        MOVE WS-SEARCH-PLAN                             02.00
                                         TO VU-RTE-PKG(VU-IDX)          02.00
                     END-IF                                             02.00
                  END-IF                                                02.00
               END-IF                                                   02.00
           END-PERFORM.                                                 02.00
      *                                                                 02.00
           IF WS-PLAN-CODE-FOUND                                        01.62
      *       ASSIGN MIN/MAX PLAN FOUND ON MERCHANT MASTER              01.62
              SET VU-MAX-RATE-IDX        TO VU-IDX1                     01.62
              SET VU-MAX-MRCH-IDX        TO VU-IDX1                     01.62
              MOVE WS-WORK-AMT           TO WS-LOW-RATE-AMT             01.62
                                            WS-LOW-ITEM-AMT             01.62
                                            WS-LOW-MRCH-AMT             01.62
              SET VU-RATE-IDX            TO VU-IDX1                     01.62
              SET VU-MRCH-IDX            TO VU-IDX1                     01.62
           ELSE                                                         01.62
      *       REPORT UNABLE TO ASSIGN MIN/MAX PLAN TO MERCHANT          01.62
      *       ONLY SET ORIGINAL PLAN TO MIN/MAX                         01.62
              SET VU-MAX-RATE-IDX        TO VU-IDX                      01.62
              MOVE WS-WORK-AMT           TO WS-LOW-RATE-AMT             01.62
                                            WS-LOW-ITEM-AMT             01.62
              SET VU-RATE-IDX            TO VU-IDX                      01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           GO TO S-500-EXIT.                                            01.62
      *                                                                 01.62
       S-500-PROCESS-PASS-THRU.                                         01.62
      *                                                                 01.62
           IF NOT KDA-MAX-PLAN-OTHER                                    01.62
      *       MIN/MAX PLANS NOT USED BY MERCHANT                        01.62
      *       SET TO ADD BASE PLAN TO MERCHANT                          01.62
              MOVE 'Y'                   TO WS-ADD-PLAN-SW              01.62
              MOVE WS-WORK-AMT           TO WS-LOW-RATE-AMT             01.62
                                            WS-LOW-MRCH-AMT             01.62
              MOVE VU-RTE-ITM(VU-IDX)    TO WS-LOW-ITEM-AMT             01.62
              SET VU-RATE-IDX            TO VU-IDX                      01.62
              SET VU-MRCH-IDX            TO VU-IDX                      01.62
              GO TO S-500-EXIT                                          01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           IF NOT WS-MAX-USED                                           01.62
      *       MIN/MAX PLANS CRITERIA NOT MET                            01.62
      *       SET TO ADD BASE PLAN TO MERCHANT                          01.62
              MOVE 'Y'                   TO WS-ADD-PLAN-SW              01.62
              MOVE WS-WORK-AMT           TO WS-LOW-RATE-AMT             01.62
                                            WS-LOW-MRCH-AMT             01.62
              MOVE VU-RTE-ITM(VU-IDX)    TO WS-LOW-ITEM-AMT             01.62
              SET VU-RATE-IDX            TO VU-IDX                      01.62
              SET VU-MRCH-IDX            TO VU-IDX                      01.62
              GO TO S-500-EXIT                                          01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           MOVE VU-RTE-MXP-X(VU-IDX)     TO WS-MIN-MAX-PLAN-CODE-X.     01.62
      *                                                                 01.62
           PERFORM S-540-FIND-MIN-MAX                                   01.62
              THRU S-540-EXIT.                                          01.62
           IF NOT WS-MIN-MAX-FOUND                                      01.62
      *       MIN/MAX PLAN NOT USED BY SYSTEM                           01.62
      *       SET TO ADD BASE PLAN TO MERCHANT                          01.62
              MOVE 'Y'                   TO WS-ADD-PLAN-SW              01.62
              MOVE WS-WORK-AMT           TO WS-LOW-RATE-AMT             01.62
                                            WS-LOW-MRCH-AMT             01.62
              MOVE VU-RTE-ITM(VU-IDX)    TO WS-LOW-ITEM-AMT             01.62
              SET VU-RATE-IDX            TO VU-IDX                      01.62
              SET VU-MRCH-IDX            TO VU-IDX                      01.62
              GO TO S-500-EXIT                                          01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           MOVE WS-MIN-MAX-PLAN-CODE-X   TO WS-PLAN-QUAL-CODE-X         02.00
                                            WS-SEARCH-PLAN-X.           02.00
           PERFORM S-520-SEARCH-MRCH-PLAN                               02.00
              THRU S-520-EXIT.                                          02.00
      *                                                                 02.00
           PERFORM                                                      02.00
             VARYING WS-PSUB FROM +1 BY +1                              02.00
             UNTIL   WS-PSUB > PPA-PKG-CNT OR                           02.00
                     WS-PLAN-CODE-FOUND                                 02.00
               IF PPA-PPT-PLAN-CODE(WS-PSUB) = WS-PLAN-QUAL-CODE        02.00
                  MOVE PPA-PPT-PKG-CODE(WS-PSUB)                        02.00
                                         TO WS-SEARCH-PLAN              02.00
                  IF WS-CUSTOM-PLAN-X NOT = WS-SEARCH-PLAN-X            02.00
                     PERFORM S-520-SEARCH-MRCH-PLAN                     02.00
                        THRU S-520-EXIT                                 02.00
                     IF WS-PLAN-CODE-FOUND                              02.00
                        MOVE WS-SEARCH-PLAN                             02.00
                                         TO VU-RTE-PKG(VU-IDX)          02.00
                     END-IF                                             02.00
                  END-IF                                                02.00
               END-IF                                                   02.00
           END-PERFORM.                                                 02.00
      *                                                                 02.00
           SET VU-MAX-RATE-IDX           TO VU-IDX1.                    01.62
           SET VU-MAX-MRCH-IDX           TO VU-IDX1.                    01.62
           MOVE WS-WORK-AMT              TO WS-LOW-RATE-AMT             01.62
                                            WS-LOW-ITEM-AMT             01.62
                                            WS-LOW-MRCH-AMT.            01.62
           SET VU-RATE-IDX               TO VU-IDX1.                    01.62
           SET VU-MRCH-IDX               TO VU-IDX1.                    01.62
           IF WS-PLAN-CODE-FOUND                                        01.62
      *       ASSIGN MIN/MAX PLAN FOUND ON MERCHANT MASTER              01.62
              MOVE 'N'                   TO WS-ADD-PLAN-SW              01.62
           ELSE                                                         01.62
      *       MIN/MAX PLAN NOT FOUND ON MERCHANT MASTER                 01.62
      *       SET TO ADD MIN\MAX PLAN TO MERCHANT                       01.62
              MOVE 'Y'                   TO WS-ADD-PLAN-SW              01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           GO TO S-500-EXIT.                                            01.62
      *                                                                 01.62
      * BASE PLAN FOUND ON MERCHANT MASTER                              01.62
      *                                                                 01.62
       S-500-BASE-PLAN-FOUND.                                           01.62
      *                                                                 01.62
           SET VU-RATE-IDX               TO VU-IDX.                     01.62
           SET VU-MRCH-IDX               TO VU-IDX.                     01.62
           MOVE 'N'                      TO WS-ADD-PLAN-SW.             01.62
           MOVE WS-WORK-AMT              TO WS-LOW-RATE-AMT             01.62
                                            WS-LOW-MRCH-AMT.            01.62
           MOVE VU-RTE-ITM(VU-RATE-IDX)  TO WS-LOW-ITEM-AMT.            01.62
           IF NOT WS-MAX-USED                                           01.62
              GO TO S-500-EXIT                                          01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           IF NOT KDA-MAX-PLAN-OTHER                                    01.62
      *       MIN/MAX PLANS NOT USED BY MERCHANT                        01.62
              GO TO S-500-EXIT                                          01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           MOVE VU-RTE-MXP-X(VU-IDX)     TO WS-MIN-MAX-PLAN-CODE-X.     01.62
      *                                                                 01.62
           PERFORM S-540-FIND-MIN-MAX                                   01.62
              THRU S-540-EXIT.                                          01.62
           IF NOT WS-MIN-MAX-FOUND                                      01.62
      *       MIN/MAX PLAN NOT USED BY SYSTEM                           01.62
      *       SET TO USE BASE PLAN FOUND ON MERCHANT MASTER             01.62
              GO TO S-500-EXIT                                          01.62
           END-IF.                                                      01.62
      *                                                                 01.62
           MOVE WS-MIN-MAX-PLAN-CODE-X   TO WS-PLAN-QUAL-CODE-X         02.00
                                            WS-SEARCH-PLAN-X.           02.00
           PERFORM S-520-SEARCH-MRCH-PLAN                               02.00
              THRU S-520-EXIT.                                          02.00
      *                                                                 02.00
           PERFORM                                                      02.00
             VARYING WS-PSUB FROM +1 BY +1                              02.00
             UNTIL   WS-PSUB > PPA-PKG-CNT OR                           02.00
                     WS-PLAN-CODE-FOUND                                 02.00
               IF PPA-PPT-PLAN-CODE(WS-PSUB) = WS-PLAN-QUAL-CODE        02.00
                  MOVE PPA-PPT-PKG-CODE(WS-PSUB)                        02.00
                                         TO WS-SEARCH-PLAN              02.00
                  IF WS-CUSTOM-PLAN-X NOT = WS-SEARCH-PLAN-X            02.00
                     PERFORM S-520-SEARCH-MRCH-PLAN                     02.00
                        THRU S-520-EXIT                                 02.00
                     IF WS-PLAN-CODE-FOUND                              02.00
                        MOVE WS-SEARCH-PLAN                             02.00
                                         TO VU-RTE-PKG(VU-IDX)          02.00
                     END-IF                                             02.00
                  END-IF                                                02.00
               END-IF                                                   02.00
           END-PERFORM.                                                 02.00
      *                                                                 02.00
           IF WS-PLAN-CODE-FOUND                                        01.62
      *       ASSIGN MIN/MAX PLAN FOUND ON MERCHANT MASTER              01.62
              SET VU-MAX-RATE-IDX        TO VU-IDX1                     01.62
              SET VU-MAX-MRCH-IDX        TO VU-IDX1                     01.62
              MOVE WS-WORK-AMT           TO WS-LOW-RATE-AMT             01.62
                                            WS-LOW-ITEM-AMT             01.62
                                            WS-LOW-MRCH-AMT             01.62
              SET VU-RATE-IDX            TO VU-IDX1                     01.62
              SET VU-MRCH-IDX            TO VU-IDX1                     01.62
              GO TO S-500-EXIT                                          01.62
           END-IF.                                                      01.62
      *    MIN/MAX PLAN NOT FOUND ON MERCHANT MASTER                    01.62
           IF KDA-IS-PASS-THRU-VS OR                                    02.00F
             (KDA-CAP-VISA-X NOT = SPACES AND                           02.00F
              KDA-CAP-VISA-X NOT = ZEROES)                              02.00F
      *       SET TO ADD MIN/MAX PLAN TO MERCHANT                       01.62
              SET VU-MAX-RATE-IDX        TO VU-IDX1                     01.62
              SET VU-MAX-MRCH-IDX        TO VU-IDX1                     01.62
              MOVE 'Y'                   TO WS-ADD-PLAN-SW              01.62
              MOVE WS-WORK-AMT           TO WS-LOW-RATE-AMT             01.62
                                            WS-LOW-ITEM-AMT             01.62
                                            WS-LOW-MRCH-AMT             01.62
              SET VU-RATE-IDX            TO VU-IDX1                     01.62
              SET VU-MRCH-IDX            TO VU-IDX1                     01.62
           ELSE                                                         01.62
      *       REPORT UNABLE TO ASSIGN MIN/MAX PLAN                      01.62
              MOVE 'T035'                TO WS-ERROR-DG                 01.62
              PERFORM S-300-PROCESS-DOWNGRADE                           01.62
                 THRU S-300-EXIT                                        01.62
           END-IF.                                                      01.62
      *                                                                 01.62
       S-500-EXIT.
           EXIT.
      *
       S-510-CHECK-RATE-PLAN.
      *
      * SEARCH RATE TABLE FOR DESIRED PLAN CODE.
      *
           PERFORM S-530-FIND-RATE-ENTRY
              THRU S-530-EXIT.
      *
           PERFORM S-515-SEARCH-PACKAGE-PLAN
              THRU S-515-EXIT.
      *
       S-510-EXIT.
           EXIT.
      *
       S-515-SEARCH-PACKAGE-PLAN.
      *
      * SEARCH MERCHANT FOR PACKAGE PLAN.
      *
           MOVE ZEROS                    TO VU-RTE-PKG(VU-IDX).         01.57
      *                                                                 01.74
           PERFORM S-815-IS-PLAN-IN-CUSTOM                              01.74
              THRU S-815-EXIT.                                          01.74
           IF WS-CUSTOM-ASSIGN                                          01.74
              MOVE 'Y'                   TO WS-PLAN-CODE-FOUND-SW       01.74
              GO TO S-515-EXIT                                          01.74
           END-IF.                                                      01.74
      *                                                                 01.74
           MOVE WS-PLAN-QUAL-CODE-X      TO WS-SEARCH-PLAN-X.
           PERFORM S-520-SEARCH-MRCH-PLAN
              THRU S-520-EXIT.
      *
           PERFORM
             VARYING WS-PSUB FROM +1 BY +1                              01.48
             UNTIL   WS-PSUB > PPA-PKG-CNT OR                           01.48
                     WS-PLAN-CODE-FOUND                                 01.48
               IF PPA-PPT-PLAN-CODE(WS-PSUB) = WS-PLAN-QUAL-CODE        01.48
                  MOVE PPA-PPT-PKG-CODE(WS-PSUB)                        01.48
                                         TO WS-SEARCH-PLAN              01.74
                  IF WS-CUSTOM-PLAN-X NOT = WS-SEARCH-PLAN-X            01.74
                     PERFORM S-520-SEARCH-MRCH-PLAN                     01.74
                        THRU S-520-EXIT                                 01.74
                     IF WS-PLAN-CODE-FOUND                              01.74
                        MOVE WS-SEARCH-PLAN                             01.83
                                         TO VU-RTE-PKG(VU-IDX)          01.74
                     END-IF                                             01.74
                  END-IF                                                01.74
               END-IF
           END-PERFORM.
      *
           IF NOT WS-PLAN-CODE-FOUND                                    01.83
              IF (KDA-CAP-VISA-X = SPACES OR ZEROS) OR                  02.00F
                  KDA-IS-PASS-THRU-VS                                   02.00F
                 NEXT SENTENCE                                          01.83
              ELSE                                                      01.83
                 MOVE 'Y'                TO WS-PLAN-CODE-FOUND-SW       01.83
                 MOVE KDA-CAP-VISA       TO VU-RTE-PKG(VU-IDX)          01.83
              END-IF                                                    01.83
           END-IF.                                                      01.83
      *                                                                 01.83
       S-515-EXIT.
           EXIT.
      *
       S-520-SEARCH-MRCH-PLAN.
      *
      * SEARCH MERCHANT PLANS FOR DESIRED PLAN CODE.
      *
           SET KDA-PC-INDX TO 1.
           SEARCH KDA-PLAN-CODE-ENTRY
              VARYING KDA-PC-INDX
                 AT END
                    MOVE 'N'             TO WS-PLAN-CODE-FOUND-SW
                 WHEN KDA-PLAN-CODE(KDA-PC-INDX) = +0
                    MOVE 'N'             TO WS-PLAN-CODE-FOUND-SW
                 WHEN KDA-PLAN-CODE(KDA-PC-INDX) = WS-SEARCH-PLAN
                    MOVE 'Y'             TO WS-PLAN-CODE-FOUND-SW
           END-SEARCH.                                                  01.62
           MOVE WS-PLAN-QUAL-CODE-X      TO WS-END-SEARCH-PLAN.         01.82
      *
       S-520-EXIT.
           EXIT.
      *
       S-525-CHECK-PLAN-LOW.
      *
      * CHECK FOR PLAN RATE LOWER THAN LAST ACCEPTED
      *
           PERFORM S-530-FIND-RATE-ENTRY
              THRU S-530-EXIT.
      *
           COMPUTE WS-WORK-AMT = (KDA-TRAN-USD-AMT
                               * VU-RTE-DEC(VU-IDX))                    01.58
                               + VU-RTE-ITM(VU-IDX).                    01.57
      *
           IF VU-RTE-MAX(VU-IDX) > +0                                   01.57
              IF WS-WORK-AMT > VU-RTE-MAX(VU-IDX)                       01.57
                 COMPUTE WS-WORK-AMT = VU-RTE-MAX(VU-IDX).              01.57
      *
           IF WS-WORK-AMT < WS-LOW-RATE-AMT
              MOVE 'Y'                   TO WS-LOWER-RATE-SW
           ELSE
              MOVE 'N'                   TO WS-LOWER-RATE-SW.
      *
       S-525-EXIT.
           EXIT.
      *
       S-530-FIND-RATE-ENTRY.
      *
      * FIND RATES FOR PLAN
      *
           SET VU-IDX TO +1.                                            01.57
           SEARCH VU-RTE-ENTRY                                          01.57
              AT END
                 DISPLAY '***** ABORT BAS110VU *****'                   01.62
                 DISPLAY 'PLAN CODE ' WS-PLAN-QUAL-CODE
                 DISPLAY ' NOT FOUND IN TABLE'
                 DISPLAY '      CALL PROGRAMMING      '
                 COMPUTE ABEND-CODE = +984
                 PERFORM Z-999-ABEND-PGM
                    THRU Z-999-EXIT
                 SET VU-IDX TO +1                                       01.62
              WHEN WS-PLAN-QUAL-CODE = VU-RTE-PLN(VU-IDX)               01.58
                 NEXT SENTENCE.
           MOVE WS-PLAN-QUAL-CODE-X      TO WS-END-SEARCH-PLAN.         01.82
      *
       S-530-EXIT.
           EXIT.
      *
       S-540-FIND-MIN-MAX.                                              01.62
      *                                                                 01.62
      * FIND MIN OR MAX PLAN FOR BASE PLAN                              01.62
      *                                                                 01.62
           SET VU-IDX1 TO +1.                                           01.62
           SEARCH VU-RTE-ENTRY                                          01.62
             VARYING VU-IDX1                                            01.62
               AT END                                                   01.62
                 MOVE 'N'                TO WS-MIN-MAX-FOUND-SW         01.62
               WHEN WS-PLAN-QUAL-CODE = VU-RTE-PLN-X(VU-IDX1)           01.62
                 IF VU-RTE-IS-USED(VU-IDX1)                             01.62
                    MOVE 'Y'             TO WS-MIN-MAX-FOUND-SW         01.62
                 ELSE                                                   01.62
                    MOVE 'N'             TO WS-MIN-MAX-FOUND-SW         01.62
                 END-IF                                                 01.62
           END-SEARCH.                                                  01.62
           MOVE WS-PLAN-QUAL-CODE-X      TO WS-END-SEARCH-PLAN.         01.82
      *                                                                 01.62
       S-540-EXIT.                                                      01.62
           EXIT.                                                        01.62
      *                                                                 01.62
       S-590-CHECK-CPS-COMMON-DATA.
      *
      * VERIFY THAT THE MINIMUM CPS DATA IS PRESENT.
      *
           MOVE 'N'                      TO WS-CPS-SW.
      *
           IF PET-TMP-CARD-ACCEPTOR = SPACES OR ZEROS
              GO TO S-590-EXIT.
           IF PET-TRANS-IDNTFIER = +0
              GO TO S-590-EXIT.
           IF PET-AUTHORIZ-AMT = +0
              GO TO S-590-EXIT.
           IF PET-AUTHORIZ-CURR-CD = SPACES OR ZEROS
              GO TO S-590-EXIT.
           IF KDA-VC-AUTH-RESP-CD-1 AND
              KDA-VC-AUTH-RESP-CD-2
                 NEXT SENTENCE
           ELSE
              GO TO S-590-EXIT.
           IF NOT KDA-VC-AUTH-SRCE
              GO TO S-590-EXIT.
           IF NOT KDA-VC-POS-TERM-PS2
              GO TO S-590-EXIT.
           IF KDA-VC-NOT-AC-PAY-SVC
              GO TO S-590-EXIT.
           IF PET-VALID-CD = SPACES
              GO TO S-590-EXIT.
           IF NOT KDA-VC-POS-ENTRY-MODE
              GO TO S-590-EXIT.
      *
           MOVE 'Y'                      TO WS-CPS-SW.
      *
       S-590-EXIT.
           EXIT.
      *
       S-800-CHECK-KEY-PACKAGE.                                         01.73
      *                                                                 01.73
           IF VU-RATE-IDX = VU-MAX-RATE-IDX                             01.73
              SET VU-CUSTOM-IDX TO +1                                   01.74
              SEARCH VU-RTE-ENTRY                                       01.73
                VARYING VU-CUSTOM-IDX                                   01.74
                  AT END                                                01.73
                    MOVE VU-RTE-PLN-X(VU-RATE-IDX)                      01.73
                                         TO BASCST01-SD-PLAN-X          01.73
                  WHEN VU-RTE-PLN-X(VU-RATE-IDX) =                      01.74
                                            VU-RTE-MXP-X(VU-CUSTOM-IDX) 01.74
                    MOVE VU-RTE-PLN-X(VU-CUSTOM-IDX)                    01.74
                                         TO BASCST01-SD-PLAN-X          01.73
              END-SEARCH                                                01.73
           ELSE                                                         01.73
              MOVE VU-RTE-PLN-X(VU-RATE-IDX)                            01.73
                                         TO BASCST01-SD-PLAN-X          01.73
           END-IF.                                                      01.73
           MOVE 'P'                      TO BASCST01-CALL-TYPE.         01.73
           MOVE 'K'                      TO BASCST01-SD-PLAN-TYPE.      01.73
           PERFORM Z-980-CALL-BASCST01                                  01.73
              THRU Z-980-EXIT.                                          01.73
      *                                                                 01.73
       S-800-EXIT.                                                      01.73
           EXIT.                                                        01.73
      *                                                                 01.73
       S-810-CHECK-CUSTOM-PACKAGE.                                      01.74
      *                                                                 01.74
           MOVE 'K'                      TO BASCST01-CALL-TYPE.         01.73
           MOVE 'K'                      TO BASCST01-SD-PLAN-TYPE.      01.73
           MOVE WS-SEARCH-PLAN-X         TO BASCST01-SD-PLAN-X.         01.73
           PERFORM Z-980-CALL-BASCST01                                  01.73
              THRU Z-980-EXIT.                                          01.73
      *                                                                 01.74
       S-810-EXIT.                                                      01.73
           EXIT.                                                        01.73
      *                                                                 01.73
       S-815-IS-PLAN-IN-CUSTOM.                                         01.74
      *                                                                 01.74
           MOVE 'N'                      TO WS-CUSTOM-FOUND-SW.         01.74
           MOVE SPACES                   TO WS-CUSTOM-PLAN-X.           01.74
           PERFORM                                                      01.74
             VARYING WS-PSUB FROM +1 BY +1                              01.74
             UNTIL   WS-PSUB > PPA-PKG-CNT OR                           01.74
                     WS-CUSTOM-FOUND                                    01.74
               IF PPA-PPT-PLAN-CODE(WS-PSUB) = WS-PLAN-QUAL-CODE        01.74
                  MOVE PPA-PPT-PKG-CODE(WS-PSUB)                        01.74
                                         TO WS-SEARCH-PLAN              01.74
                  PERFORM S-810-CHECK-CUSTOM-PACKAGE                    01.74
                     THRU S-810-EXIT                                    01.74
                  IF BASCST01-RC-CUSTOM                                 01.74
                     MOVE WS-SEARCH-PLAN-X                              01.74
                                         TO WS-CUSTOM-PLAN-X            01.74
                     PERFORM S-520-SEARCH-MRCH-PLAN                     01.74
                        THRU S-520-EXIT                                 01.74
                     IF WS-PLAN-CODE-FOUND                              01.74
                        IF BASCST01-RC-CUSTOM-ASSIGN                    01.74
                           MOVE WS-SEARCH-PLAN-X                        01.74
                                         TO VU-RTE-PKG(VU-IDX)          01.74
                           MOVE 'A'      TO WS-CUSTOM-FOUND-SW          01.74
                        ELSE                                            01.74
                           MOVE 'B'      TO WS-CUSTOM-FOUND-SW          01.74
                        END-IF                                          01.74
                     END-IF                                             01.74
                  END-IF                                                01.74
               END-IF                                                   01.74
           END-PERFORM.                                                 01.74
      *                                                                 01.74
       S-815-EXIT.                                                      01.74
           EXIT.                                                        01.74
      *                                                                 01.74
       Z-980-CALL-BASCST01.                                             01.73
           CALL BASCST01 USING BASCST01-CALL-PARAMETERS                 01.73
                               PACKAGE-PASS-AREA                        01.73
                               KMCINTEP-DATA-AREA                       01.73
                               POST-EDIT-TRANS.                         01.73
       Z-980-EXIT.                                                      01.73
           EXIT.                                                        01.73
      *                                                                 01.73
       Z-981-CALL-BASCST02.                                             01.85
           CALL BASCST02 USING BASCST02-CALL-PARAMETERS                 01.85
                               PACKAGE-PASS-AREA                        01.85
                               KMCINTEP-DATA-AREA                       01.85
                               POST-EDIT-TRANS.                         01.85
       Z-981-EXIT.                                                      01.85
           EXIT.                                                        01.85
      *                                                                 01.85
       Z-982-CALL-BASVPP01.                                             02.06
           CALL BASVPP01 USING BASVPP01-CALL-PARAMETERS                 02.06
                               KMCINTEP-DATA-AREA                       02.06
                               POST-EDIT-TRANS                          02.06
                               WS-CPS-REIMB-SW.                         02.06
       Z-982-EXIT.                                                      02.06
           EXIT.                                                        02.06
      *                                                                 02.06
       Z-999-ABEND-PGM.
           CALL ABEND USING ABEND-CODE.
       Z-999-EXIT.
           EXIT.
      *
      *================================================================*
      *                    PROGRAM CHANGE HISTORY                      *
      *================================================================*
      *                                                                *
      *  DATE     CHANGE                                               *
      * REVISED   REQUEST  DESCRIPTION                                 *
      * -------------------------------------------------------------- *
      * 08/20/03   01.01   US COMPLIANCE - INTERCHANGE REIMBURSEMENT   *
      * 02/11/04   01.02   SPRING 04 LEGAL & REG                       *
      * 04/10/04   01.03   ADD PLAN PACKAGES                           *
      * 08/11/04   01.04   FALL 2004 LEGAL & REG                       *
      * 01/15/05   01.05   ENHANCE INTERCHANGE PRICING                 *
      * 07/21/05   01.06   MODIFY DOWNGRADE DISPLAY                    *
      *                    PSR REPORT - PHASE I                        *
      * 07/22/05   01.07   MODIFY CPS AUTH CHAR CHECK                  *
      * 07/25/05   01.08   MODIFY AUTH AMT CHECK                       *
      *                    ALLOW FUTURE AUTH DATE IN TRANS ID          *
      * 07/27/05   01.09   MODIFY SIG CARD CHECKS                      *
      * 07/28/05   01.10   MODIFY SIG CARD CHECKS                      *
      * 08/02/05   01.11   MODIFY COMMERCIAL ELECTRONIC CHECKS         *
      * 08/09/05   01.12   MODIFY DOWNGRADES                           *
      * 08/29/05   01.13   FALL 2005 LEGAL & REG                       *
      * 08/31/05   01.14   MODIFY UTILITY                              *
      * 09/13/05   01.15   REMOVE COMMERCIAL AND EPS COMBO CPS CHECKS  *
      *                    DON'T CHECK IF RATE LOWER FOR QUAL ONLY     *
      * 09/21/05   01.16   TEMPORARY REWARDS CONVERSION PROCESSING     *
      * 09/23/05   01.17   MODIFY EPS STALE DATE DAYS                  *
      *                    REMOVE TEMPORARY REWARDS CONV PROCESSING    *
      * 10/13/05   01.18   MODIFY SIG REWARDS CHECKS                   *
      * 10/18/05   01.19   MODIFY SIG REWARDS CHECKS                   *
      *                    DON'T RESET AUTH CHAR                       *
      * 11/01/05   01.20   MODIFY RETURN PROCESSING                    *
      * 01/19/06   01.21   MODIFY COMMERCIAL EIRF REIMB J QUAL         *
      * 02/14/06   01.22   ADD REWARDS 0601 & 0701                     *
      * 03/08/06   01.23   SPRING 2006 LEGAL & REG                     *
      * 03/29/06   01.24   MODIFY REWARDS CHECKS                       *
      * 04/04/06   01.25   MODIFY EIRF LVL2 CHECKS                     *
      *                    REWORK RETAIL 2 CHECKS                      *
      *                    REMOVE SIG/INF AT TRV SVC MRCHS FROM REWARDS*
      * 06/13/06   01.26   PREVENT COMMERCIAL CARDS FROM RETAIL 2      *
      * 07/18/06   01.27   MODIFY AUTH ERROR CODE                      *
      *                    ADD ADDENDA HAD ERRORS CHECKS               *
      *                    ADD POS TERM CHECK FOR CPS RETAIL           *
      * 08/03/06   01.28   ADD MAX INTERCHANGE FEE AMOUNT TO CALC      *
      * 08/15/06   01.29   ADD POS TERM CHECK FOR CPS RETAIL 2 F2F     *
      * 12/12/06   01.30   MODIFY AUTH CHAR AND ECOM LOAD              *
      * 02/22/07   01.31   INCLUDE 5814 (FAST FOOD) AS TRAVEL SVC      *
      *                    MODIFY EIRF CHECKS FOR FLEET                *
      *                    SIG/INF CARDS ARE NOT REWARDS               *
      * 04/08/07   01.32   SPRING 2007 LEGAL & REG                     *
      * 03/15/07   01.33   SPRING 2007 LEGAL & REG - FIX EIRF B2B      *
      * 03/20/07   01.34   SPRING 2007 LEGAL & REG - FIX EIRF CNP/RTL  *
      * 03/27/07   01.35   SPRING 2007 LEGAL & REG - PLAN 6521 REIMB   *
      * 03/30/07   01.36   SPRING 2007 LEGAL & REG - PLAN 6621 REIMB   *
      *                    NO REWARDS FOR UTILITY WITH MVV             *
      * 04/19/07   01.37   MODITY INF, SIG, & SIG PREFER DESIGNATION   *
      * 04/20/07   01.39   MODITY SIG PREFER CHECKS                    *
      * 04/22/07   01.40   MODITY SIG PREFER TO BE LIKE COMMERCIAL     *
      * 04/25/07   01.41   MODITY CPS RTL DBT AMT/AUTH CHECK           *
      *                    MODITY SIG/PREF IDENT PROCESS               *
      * 07/10/07   01.42   CORRECT GSA/REG LRG TKT PAY SVC IND ASSIGN  *
      * 10/12/07   01.43   FALL 2007 LEGAL & REG                       *
      * 10/23/07   01.44   CORRECT UTILITY BUSINESS & SEG PREF         *
      * 04/04/08   01.45   SPRING 2008 LEGAL & REG                     *
      * 04/07/08   01.46   CORRECT FPI ON PLAN 2221                    *
      * 07/19/08   01.47   VISA JUL 19 ADD MAX RATE ON FUEL            *
      * 10/03/08   01.48   FALL 2008 LEGAL & REG                       *01.48
      * 10/06/08   01.49   CORRECT GSA LVL2                            *01.48
      * 11/14/08   01.50   CORRECT PSI FOR GSA LVL3 PLAN 3721          *01.50
      * 12/19/08   01.51   CORRECT CODE FOR NON FUEL GSA TRANS         *01.51
      * 04/17/09   01.52   SPRING 2009 LEGAL & REG                     *01.52
      *                    CORRECT TX2 AMOUNT TO 3.95                  *01.52
      * 04/27/09   01.53   MODIFY GSA LRG TKT PAY SVC AND REIMB        *01.53
      * 10/16/09   01.54   FALL 2009 LEGAL & REG                       *01.54
      * 10/19/09   01.55   ADD CREDIT CHECK TO SIG PREF                *01.55
      * 12/01/09   01.56   CORRECT BILL PAY CHECKS                     *01.56
      *                    CORRECT AUTH VS TRAN DATE CHECKS            *01.56
      * 01/31/10   01.57   REMOVE TERM CAP CHECK FOR CPS RETAIL        *01.57
      *                    ADD NEW INTERCHANGE PLAN TABLE COPYBOOK     *01.57
      * 10/15/10   01.58   MODIFY RATE PROCESSING                      *01.58
      * 10/15/10   01.59   FALL 2010 LEGAL & REG                       *01.59
      * 10/15/10   01.60   REMOVE KDA REDUCED INTCHANGE FLAG           *01.60
      * 04/15/11   01.61   CORRECT UTILITY AND SMALL TICKET QUAL       *01.61
      * 04/15/11   01.62   SPRING 2011 LEGAL & REG                     *01.62
      * 04/18/11   01.63   CORRECT TAX1 QUALIFICATION                  *01.63
      * 10/01/11   01.64   DEBIT REGULATORY CHANGES                    *01.64
      * 10/02/11   01.65   DEBIT REGULATORY CHANGES CORRECTIONS        *01.65
      * 10/05/11   01.66   DEBIT REGULATORY CHANGES MORE CORRECTIONS   *01.66
      * 10/06/11   01.67   DEBIT REGULATORY CHANGES RETURNS            *01.67
      * 10/15/11   01.68   FALL 2011 LEGAL & REG                       *01.68
      * 10/17/11   01.69   CORRECT RETURNS, TAX2, AND EIRF             *01.69
      * 10/17/11   01.70   CORRECT CHARITY                             *01.70
      * 12/14/11   01.71   CORRECT BUSINESS ENHANCED EIRF              *01.71
      * 01/17/12   01.72   CORRECT BUSINESS ENHANCED EIRF              *01.72
      * 01/30/12   01.73   ADD CUSTOM INTERFACE SUBROUTINE             *01.73
      * 02/02/12   01.74   MODIFY CUSTOM CHECKS                        *01.74
      * 02/06/12   01.75   CORRECT BUSINESS/CORPORATE                  *01.75
      * 03/19/12   01.76   CORRECT BUSINESS SIG & ENH STANDARD         *01.76
      * 04/13/12   01.77   SPRING 2012 LEGAL & REG                MILNE*01.77
      * 04/16/12   01.78   FIX CONSUMER DB/PP RETAIL 2 QUALS      MILNE*01.78
      * 05/18/12   01.79   REMOVE 01.76 FIX. RESET TO 01.72.           *01.79
      * 05/24/12   01.80   CORRECT LVL2 VS LVL3 FOR PURCHASING CARD    *01.80
      * 08/08/12   01.81   CORRECT UTILITY STANDARD FOR BUSINESS       *01.81
      * 08/08/12   01.82   CORRECT AUTH CHAR OF SPACE                  *01.82
      * 06/30/12   01.83   ADD CATCH-ALL PACKAGE                       *01.83
      * 10/14/12   01.84   FALL 2012 LEGAL & REG                       *01.84
      * 11/01/12   01.85   ADD CUSTOM PLAN PROCESSING                  *01.85
      * 04/19/13   01.86   SPRING 2013 LEGAL & REG                     *01.86
      * 05/18/13   01.87   AS0001-1318: REMOVE MASKING OF MOTO/ECOM    *01.87
      *                    FOR NON-CPS, NON BILL PAY, NON DIRECT MKT   *01.87
      *                    TRANSACTIONS.                               *01.87
      * 06/04/13   01.88A  TICKET AS0001-721. RECLASS FROM EIRF TO STD *01.88A
      * 06/04/13   01.88B  TICKET AS0001-721. RECLASS FROM CORP OR     *01.88B
      *                    PURCH STD TO CORP OR PURCH EIRF.            *01.88B
      * 06/04/13   01.89   AIRLINES                                    *01.89
      * 07/09/13   01.90   AS0001-1454: RECLASS FROM COMMERCIAL        *01.90
      *                    RETURNS TO GSA LARGE TICKET RETURNS         *01.90
      * 08/07/13   01.91   ALLOW CPS RETAIL 2 TO QUALIFY FOR CPS RET.  *01.91
      * 08/30/13   01.92   AS0001-1454: CREDIT VOUCHER RECLASSES       *01.92
      * 10/18/13   01.93   FALL 2013 LEGAL & REG                       *01.93
      * 11/07/13   01.94   FALL 2013 LEGAL & REG                       *01.94
      * 11/15/13   01.95   AS0001-1813 REMOVED KDA-VS-PID-IS-TRD-REWARD*01.95
      *                    AND  KDA-VS-PID-IS-SIGNATURE FROM THE       *01.95
      *                    INITIAL CHECK IN I-470-SET-CPS-SVC-STATION  *01.95
      * 11/14/13   01.96   AS0001-1821 FLEET/PETROLEUM SUPPORT         *01.96
      * 01/09/14   01.97   AS0001-1821 FIXED CODE THAT WAS FALLING     *01.97
      *                    THRU TO RATE LOWER CHECK WHEN IT SHOULDNT   *01.97
      * 04/11/14   01.98   SPRING 2014 LEGAL & REG                     *01.98
      * 05/06/14   01.99   RE-ADD AVS CHECK TO KEY ENTER FOR COMMERCIAL*01.99
      * 06/02/14   02.00   FIX MAX PLAN/PACKAGES                       *02.00
      * 06/02/14   02.00F  USE CATCH-ALL PACKAGES FOR MAX PLANS        *02.00F
      * 06/02/14   02.01   FIX PURCHASE & BUSINESS LVL2 DOWNGRADES     *02.01
      * 07/22/14   02.02   AD0001-998 ADD PGM VER TO BAC00110 EDIT RPT *02.02
      * 07/22/14   02.03   AS0001-2329 FIX 14.1 CODE FOR CORP PA/PL    *02.03
      * 07/22/14   02.04   AS0001-2361 FIX PRIVATE LABEL CREDITS       *02.04
      * 07/22/14   02.05   AS-2301 FIX LARGE TICKET QUALS              *02.05
      * 10/17/14   02.06   AS0001-2355 - SUPPORT VISA PARTNER PROGRAM  *02.06
      * 10/17/14   02.07   AD-854 FALL 2014 LEGAL & REGS               *02.07
      * 10/17/14   02.08   AD0001-1060 FIX AMOUNT TOLERANCES           *02.08
      * 10/17/14   02.09   AS0001-2363 WHEN TRAN IS EIRF ONLY (CANNOT  *02.09 HOTFIXED VERS 8/26/14
      *                    QUAL AS STD), EDIT CHK COMMON EIRF DATA,    *02.09
      *                    IF DWNGR SET FLG TO REJ TRN IN BAC00110     *02.09
