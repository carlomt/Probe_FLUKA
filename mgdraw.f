*$ CREATE MGDRAW.FOR
*COPY MGDRAW
*                                                                      *
*=== mgdraw ===========================================================*
*                                                                      *
      SUBROUTINE MGDRAW ( ICODE, MREG )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2013      by        Alfredo Ferrari           *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     MaGnetic field trajectory DRAWing: actually this entry manages   *
*                                        all trajectory dumping for    *
*                                        drawing                       *
*                                                                      *
*     Created on   01 March 1990   by        Alfredo Ferrari           *
*                                              INFN - Milan            *
*     Last change   12-Nov-13      by        Alfredo Ferrari           *
*                                              INFN - Milan            *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(CASLIM)'
      INCLUDE '(COMPUT)'
      INCLUDE '(SOURCM)'
      INCLUDE '(FHEAVY)'
* Start_Devel_seq
      INCLUDE '(FLKMAT)'
* End_Devel_seq
      INCLUDE '(FLKSTK)'
      INCLUDE '(GENSTK)'
      INCLUDE '(MGDDCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(QUEMGD)'
      INCLUDE '(SUMCOU)'
      INCLUDE '(TRACKR)'

      INCLUDE '(OPPHST)'

      INCLUDE 'phcount.inc'
*
      DIMENSION DTQUEN ( MXTRCK, MAXQMG )
*
      CHARACTER*20 FILNAM
      LOGICAL LFCOPE
      LOGICAL LFIRST

      SAVE LFCOPE
      DATA LFCOPE / .FALSE. /


      DATA LFIRST / .TRUE. / 
      SAVE LFIRST

      SAVE nPTHE, nSIPM, nSOU, nALL, nAria, nPVC

      IF ( LFIRST ) THEN
         LFIRST = .FALSE.
         WRITE (LUNOUT,*) "==>                                     <=="
         WRITE (LUNOUT,*) "==> customized mgdraw                   <=="
         WRITE (LUNOUT,*) "==> by Carlo Mancini, 02/12/2015        <=="
c$$$         IF(lTeletrasporto) THEN
c$$$            WRITE (LUNOUT,*) "==> con teletrasporto  "
c$$$         END IF
         WRITE (LUNOUT,*) "==>                                     <=="
         CALL FFLUSH
         CALL FLUSH(LUNOUT)
      
         CALL GEON2R("PMT     ",nSIPM, IERR)
         IF ( IERR .GT. 0 ) 
     &        CALL FLABRT ( 'MGDRAW', 'PMT not recognized' )

         CALL GEON2R("PTERF   ",nPTHE, IERR)
         IF ( IERR .GT. 0 ) 
     &        CALL FLABRT ( 'MGDRAW', 'PTERF not recognized' )

         CALL GEON2R("CAP_SOU ",nSOU, IERR)
         IF ( IERR .GT. 0 ) 
     &        CALL FLABRT ( 'MGDRAW', 'CAP_SOU not recognized' )

         CALL GEON2R("ALL     ",nALL, IERR)
         IF ( IERR .GT. 0 ) 
     &        CALL FLABRT ( 'MGDRAW', 'ALL not recognized' )

c$$$         CALL GEON2R("ARIA    ",nAria, IERR)
c$$$         IF ( IERR .GT. 0 ) 
c$$$     &        CALL FLABRT ( 'MGDRAW', 'ARIA not recognized' )

         CALL GEON2R("ARIA    ",nAria, IERR)
         IF ( IERR .GT. 0 ) 
     &        CALL FLABRT ( 'MGDRAW', 'ARIA not recognized' )

         CALL GEON2R("PVC     ",nPVC, IERR)
         IF ( IERR .GT. 0 ) 
     &        CALL FLABRT ( 'MGDRAW', 'PVC not recognized' )

      END IF
*
*----------------------------------------------------------------------*
*                                                                      *
*     Icode = 1: call from Kaskad                                      *
*     Icode = 2: call from Emfsco                                      *
*     Icode = 3: call from Kasneu                                      *
*     Icode = 4: call from Kashea                                      *
*     Icode = 5: call from Kasoph                                      *
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
c$$$         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
c$$$     &          'UNFORMATTED' )
      END IF
c$$$      WRITE (IODRAW) NTRACK, MTRACK, JTRACK, SNGL (ETRACK),
c$$$     &               SNGL (WTRACK)
c$$$      WRITE (IODRAW) ( SNGL (XTRACK (I)), SNGL (YTRACK (I)),
c$$$     &                 SNGL (ZTRACK (I)), I = 0, NTRACK ),
c$$$     &               ( SNGL (DTRACK (I)), I = 1, MTRACK ),
c$$$     &                 SNGL (CTRACK)
*  +-------------------------------------------------------------------*
*  |  Quenching is activated
      IF ( LQEMGD ) THEN
         IF ( MTRACK .GT. 0 ) THEN
            RULLL  = ZERZER
            CALL QUENMG ( ICODE, MREG, RULLL, DTQUEN )
c$$$            WRITE (IODRAW) ( ( SNGL (DTQUEN (I,JBK)), I = 1, MTRACK ),
c$$$     &                         JBK = 1, NQEMGD )
D           IF ( ICHRGE (JTRACK) .EQ. 0 )
D    &         CALL FLABRT ( 'MGDRAW', 'MTRACK>0 && ICH == 0' )
D           IF ( MEDFLK (MREG,IPRODC) .LE. 2 )
D    &         CALL FLABRT ( 'MGDRAW', 'MTRACK>0 && MEDIUM <= 2' )
D        ELSE
D           IF ( MEDFLK (MREG,IPRODC) .GT. 2
D    &          .AND. ICHRGE (JTRACK) .NE. 0 )
D    &      CALL FLABRT ( 'MGDRAW', 'MTRACK=0 .NEQV. MEDIUM <=2' )
         END IF
      END IF
*  |  End of quenching
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     Boundary-(X)crossing DRAWing:                                    *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             19: boundary crossing                                    *
*     Icode = 2x: call from Emfsco                                     *
*             29: boundary crossing                                    *
*     Icode = 3x: call from Kasneu                                     *
*             39: boundary crossing                                    *
*     Icode = 4x: call from Kashea                                     *
*             49: boundary crossing                                    *
*     Icode = 5x: call from Kasoph                                     *
*             59: boundary crossing                                    *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY BXDRAW ( ICODE, MREG, NEWREG, XSCO, YSCO, ZSCO )

c$$$      IF(lTeletrasporto) THEN
c$$$         rRange= 0.605D+1
c$$$         rZcentro= -50.D+1
c$$$         rXcentro= ZERZER
c$$$         rZmin= rZcentro - rRange 
c$$$         rZmax= rZcentro +rRange 
c$$$         rXmin= rXcentro -rRange 
c$$$         rXmax= rXcentro +rRange 
c$$$         rYmin= ZERZER
c$$$         IF((Xflk.GT.rXmin) .AND. (Xflk.LT.rXmax)
c$$$     &        .AND.
c$$$     &        (Zflk.GT.rZmin) .AND. (Zflk.LT.rZmax)
c$$$     &        .AND.
c$$$     &        (Yflk.LT.rYmin) ) THEN
c$$$            WRITE (LUNOUT,*) "Teletrasporto da ",Yflk
c$$$            Yflk=Yflk - 100.D+1
c$$$         END IF
c$$$      END IF


      IF(NEWREG.eq.nPTHE .and. MREG.eq.nPVC) THEN
         nInPartS=nInPartS+1            
         rEInPteS=rEInPteS+(Etrack-AM(JTRACK))
         IF(JTRACK.EQ.7 .OR. JTRACK.EQ.-1) THEN
            rEInPtePhS=rEInPtePhS+(Etrack-AM(JTRACK))
         ELSE
            rEInPteChS=rEInPteChS+(Etrack-AM(JTRACK))
         END IF
      END IF

      IF(NEWREG.eq.nALL .and. MREG.eq.nAria) THEN
         nInPart=nInPart+1            
c$$$         IF(nInPart.GT.IMAXPHC) THEN
c$$$            WRITE(LUNOUT,*)"ERROR: N sec in Pterph greater than PHCIMAX"
c$$$            CALL FFLUSH 
c$$$            CALL FLUSH(LUNOUT)
c$$$            CALL FLABRT ( 'MGDRAW', 'PHCIMAX too small' )
c$$$         END IF

c$$$         nPid(nInPart)=JTRACK
c$$$         rElEk(nInPart)=ETRACK-AM(JTRACK)
c$$$         IF(MREG.eq.nAirIn) THEN
c$$$            nSideIn(nInPart)=1
c$$$         ELSE IF(MREG.eq.npvc1) THEN
c$$$            nSideIn(nInPart)=2
c$$$         ELSE
c$$$            nSideIn(nInPart)=-1
c$$$         END IF
         rEInPte=rEInPte+(Etrack-AM(JTRACK))
         IF(JTRACK.EQ.7 .OR. JTRACK.EQ.-1) THEN
            rEInPtePh=rEInPtePh+(Etrack-AM(JTRACK))
         ELSE
            rEInPteCh=rEInPteCh+(Etrack-AM(JTRACK))
         END IF

         IF (Ltrack.EQ.2) THEN
            rEprimPte=(Etrack-AM(JTRACK))
         END IF

      END IF

      IF( (MREG.eq.nPTHE) .and. (NEWREG.eq.nSIPM) ) THEN
c         IF(JTRACK.EQ.-1 .AND. LSTOPP.GT.0) THEN
         IF(JTRACK.EQ.-1) THEN
c$$$  WRITE(LUNOUT,*)"DEBUG: optical photon"
c$$$  CALL FFLUSH 
c$$$  CALL FLUSH(LUNOUT)
c$$$  WRITE(LUNOUT,*)"DEBUG:  LSTOPP ",LSTOPP
c$$$  CALL FFLUSH 
c$$$  CALL FLUSH(LUNOUT)
c$$$  WRITE(LUNOUT,*)"DEBUG:  CMPOPP(LSTOPP) ",CMPOPP(LSTOPP)
c$$$  WRITE(LUNOUT,*)"DEBUG:  POPTPH(LSTOPP) ",POPTPH(LSTOPP)
c$$$  CALL FFLUSH 
c$$$  CALL FLUSH(LUNOUT)
            nPhot=nPhot+1
         END IF
      END IF
      
      IF(MREG.eq.nSOU) THEN
         rEOutSou=Etrack-AM(JTRACK)
      END IF

      RETURN
*     
*======================================================================*
*     *
*     Event End DRAWing:                                               *
*     *
*======================================================================*
*     *
      ENTRY EEDRAW ( ICODE )
      RETURN
*
*======================================================================*
*                                                                      *
*     ENergy deposition DRAWing:                                       *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             10: elastic interaction recoil                           *
*             11: inelastic interaction recoil                         *
*             12: stopping particle                                    *
*             13: pseudo-neutron deposition                            *
*             14: escape                                               *
*             15: time kill                                            *
*     Icode = 2x: call from Emfsco                                     *
*             20: local energy deposition (i.e. photoelectric)         *
*             21: below threshold, iarg=1                              *
*             22: below threshold, iarg=2                              *
*             23: escape                                               *
*             24: time kill                                            *
*     Icode = 3x: call from Kasneu                                     *
*             30: target recoil                                        *
*             31: below threshold                                      *
*             32: escape                                               *
*             33: time kill                                            *
*     Icode = 4x: call from Kashea                                     *
*             40: escape                                               *
*             41: time kill                                            *
*             42: delta ray stack overflow                             *
*     Icode = 5x: call from Kasoph                                     *
*             50: optical photon absorption                            *
*             51: escape                                               *
*             52: time kill                                            *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY ENDRAW ( ICODE, MREG, RULL, XSCO, YSCO, ZSCO )
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
      END IF
c$$$      WRITE (IODRAW)  0, ICODE, JTRACK, SNGL (ETRACK), SNGL (WTRACK)
c$$$      WRITE (IODRAW)  SNGL (XSCO), SNGL (YSCO), SNGL (ZSCO), SNGL (RULL)
*  +-------------------------------------------------------------------*
*  |  Quenching is activated : calculate quenching factor
*  |  and store quenched energy in DTQUEN(1, jbk)
      IF ( LQEMGD ) THEN
         RULLL = RULL
         CALL QUENMG ( ICODE, MREG, RULLL, DTQUEN )
c$$$         WRITE (IODRAW) ( SNGL (DTQUEN(1, JBK)), JBK = 1, NQEMGD )
      END IF
*  |  end quenching
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     SOurce particle DRAWing:                                         *
*                                                                      *
*======================================================================*
*
      ENTRY SODRAW
c$$$      IF ( .NOT. LFCOPE ) THEN
c$$$         LFCOPE = .TRUE.
c$$$         IF ( KOMPUT .EQ. 2 ) THEN
c$$$            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
c$$$         ELSE
c$$$            FILNAM = CFDRAW
c$$$         END IF
c$$$         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
c$$$     &          'UNFORMATTED' )
c$$$      END IF
c$$$      WRITE (IODRAW) -NCASE, NPFLKA, NSTMAX, SNGL (TKESUM),
c$$$     &                SNGL (WEIPRI)
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope: it works only for 1 source particle on
*  |  the stack for the time being
c$$$      IF ( ILOFLK (NPFLKA) .GE. 100000 .AND. LRADDC (NPFLKA) ) THEN
c$$$         IARES  = MOD ( ILOFLK (NPFLKA), 100000  )  / 100
c$$$         IZRES  = MOD ( ILOFLK (NPFLKA), 10000000 ) / 100000
c$$$         IISRES = ILOFLK (NPFLKA) / 10000000
c$$$         IONID  = ILOFLK (NPFLKA)
c$$$         WRITE (IODRAW) ( IONID,SNGL(-TKEFLK(I)),
c$$$     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
c$$$     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
c$$$     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
c$$$     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
*  |
*  +-------------------------------------------------------------------*
*  |  Patch for heavy ions: it works only for 1 source particle on
*  |  the stack for the time being
c$$$      ELSE IF ( ABS (ILOFLK (NPFLKA)) .GE. 10000 ) THEN
c$$$         IONID = ILOFLK (NPFLKA)
c$$$         CALL DCDION ( IONID )
c$$$         WRITE (IODRAW) ( IONID,SNGL(TKEFLK(I)+AMNHEA(-IONID)),
c$$$     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
c$$$     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
c$$$     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
c$$$     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
*  |
*  +-------------------------------------------------------------------*
*  |  Patch for heavy ions: ???
c$$$      ELSE IF ( ILOFLK (NPFLKA) .LT. -6 ) THEN
c$$$         WRITE (IODRAW) ( IONID,SNGL(TKEFLK(I)+AMNHEA(-ILOFLK(NPFLKA))),
c$$$     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
c$$$     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
c$$$     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
c$$$     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
*  |
*  +-------------------------------------------------------------------*
*  |
c$$$      ELSE
c$$$         WRITE (IODRAW) ( ILOFLK(I), SNGL (TKEFLK(I)+AM(ILOFLK(I))),
c$$$     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
c$$$     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
c$$$     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
c$$$     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
c$$$      END IF
*  |
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     USer dependent DRAWing:                                          *
*                                                                      *
*     Icode = 10x: call from Kaskad                                    *
*             100: elastic   interaction secondaries                   *
*             101: inelastic interaction secondaries                   *
*             102: particle decay  secondaries                         *
*             103: delta ray  generation secondaries                   *
*             104: pair production secondaries                         *
*             105: bremsstrahlung  secondaries                         *
*             110: decay products                                      *
*     Icode = 20x: call from Emfsco                                    *
*             208: bremsstrahlung secondaries                          *
*             210: Moller secondaries                                  *
*             212: Bhabha secondaries                                  *
*             214: in-flight annihilation secondaries                  *
*             215: annihilation at rest   secondaries                  *
*             217: pair production        secondaries                  *
*             219: Compton scattering     secondaries                  *
*             221: photoelectric          secondaries                  *
*             225: Rayleigh scattering    secondaries                  *
*             237: mu pair     production secondaries                  *
*     Icode = 30x: call from Kasneu                                    *
*             300: interaction secondaries                             *
*     Icode = 40x: call from Kashea                                    *
*             400: delta ray  generation secondaries                   *
*  For all interactions secondaries are put on GENSTK common (kp=1,np) *
*  but for KASHEA delta ray generation where only the secondary elec-  *
*  tron is present and stacked on FLKSTK common for kp=npflka          *
*                                                                      *
*======================================================================*
*
      ENTRY USDRAW ( ICODE, MREG, XSCO, YSCO, ZSCO )
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
      END IF
* No output by default:
      RETURN
*=== End of subrutine Mgdraw ==========================================*
      END

