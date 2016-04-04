*$ CREATE SOURCE.FOR
*COPY SOURCE
*
*=== source ===========================================================*
*
      SUBROUTINE SOURCE ( NOMORE )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2013      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     New source for FLUKA9x-FLUKA20xy:                                *
*                                                                      *
*     Created on 07 January 1990   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on  19-May-13    by    Alfredo Ferrari               *
*                                                                      *
*  This is just an example of a possible user written source routine.  *
*  note that the beam card still has some meaning - in the scoring the *
*  maximum momentum used in deciding the binning is taken from the     *
*  beam momentum.  Other beam card parameters are obsolete.            *
*                                                                      *
*       Output variables:                                              *
*                                                                      *
*              Nomore = if > 0 the run will be terminated              *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(BEAMCM)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(FLKSTK)'
      INCLUDE '(IOIOCM)'
      INCLUDE '(LTCLCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(SOURCM)'
      INCLUDE '(SUMCOU)'

      INCLUDE 'phcount.inc'
*
      LOGICAL LFIRST
      LOGICAL LExtrFailed
*
      SAVE rymax1, rymax2
      SAVE rE3, rE4, rE5
      SAVE rBr1, rBr2, rBr3, rBr4, rBr5
      SAVE LFIRST

      DATA LFIRST / .TRUE. /

      rFlagIsotope=WHASOU(1)

      rdummy=17.D+0
      
      lTeletrasporto=.FALSE.

c      rymaxY=((rwmaxY/AMELCT)**4-(rwmaxY/AMELCT)**2)*7.D-2
c      rymaxSr=((rwmaxSr/AMELCT)**4-(rwmaxSr/AMELCT)**2)*7.D-2


*======================================================================*
*                                                                      *
*                 BASIC VERSION                                        *
*                                                                      *
*======================================================================*
      NOMORE = 0
*  +-------------------------------------------------------------------*
*  |  First call initializations:
      IF ( LFIRST ) THEN
         rymax1=0.D+0
         rymax2=0.D+0

         WRITE (LUNOUT,*) "==>                                     <=="
         WRITE (LUNOUT,*) "==> customized source                   <=="
         WRITE (LUNOUT,*) "==> by Carlo Mancini, 19/10/2015        <=="
         WRITE (LUNOUT,*) "==> Cs137                               <=="
         WRITE (LUNOUT,*) "==>                                     <=="
         WRITE (LUNOUT,*) "==> WHASOU(1):",
     &                            WHASOU(1),"                      <=="

         IF( rFlagIsotope.EQ.ZERZER ) THEN
c     beta
            rBr1tmp=0.947D+0
            rwmax1=523.D-6 
            
c     beta
            rBr2tmp=1.D+0 - rBr1tmp
            rwmax2=1176.D-6 
            
c     e-
            rBr3tmp=rBr1tmp * 0.078D+0
            rE3=624.D-6
            
c     e-
            rBr4tmp=rBr1tmp * 0.014D+0
            rE4=656.D-6
            
c     gamma
            rBr5tmp=rBr1tmp * (1.D+0 - 0.18D+0)
            rE5=662.D-6
            
            rBrTot = rBr1tmp + rBr2tmp + rBr3tmp + rBr4tmp + rBr5tmp
            WRITE (LUNOUT,*) "normalization factor ",rBrTot
            
            rBr1=rBr1tmp/rBrTot
            rBr2=rBr2tmp/rBrTot
            rBr3=rBr3tmp/rBrTot
            rBr4=rBr4tmp/rBrTot
            rBr5=rBr5tmp/rBrTot
            
            WRITE (LUNOUT,*)"end point beta- a ",rwmax1," GeV Br: ",rBr1
            WRITE (LUNOUT,*)"end point beta- b ",rwmax2," GeV Br: ",rBr2
            WRITE (LUNOUT,*)"monochromatic e- a ",rE3," GeV Br: ",rBr3
            WRITE (LUNOUT,*)"monochromatic e- b ",rE4," GeV Br: ",rBr4
            WRITE (LUNOUT,*)"monochromatic gamma ",rE5," GeV Br: ",rBr5
            WRITE (LUNOUT,*)"AMELCT ",AMELCT
            CALL FFLUSH
            CALL FLUSH(LUNOUT)
            DO I=1,1000
               rtmpX=(I*ONEONE)/1000.D+0 *rwmax2
               rTmp1=rDist(rtmpX,rwmax1)
               rTMp2=rDist(rtmpX,rwmax2)
               WRITE(51,*) rtmpX,rTmp1,rTmp2
               IF(rTmp1.GT.rymax1) THEN
                  rymax1=rTmp1
               END IF
               IF(rTmp2.GT.rymax2) THEN
                  rymax2=rTmp2
               END IF
            END DO
            WRITE (LUNOUT,*) "Max dist 1 ",rymax1
            WRITE (LUNOUT,*) "Max dist 2 ",rymax2
            rymax1=rymax1+rymax1*1.D-1
            rymax2=rymax2+rymax2*1.D-1
            WRITE (LUNOUT,*) "Max dist 1 ",rymax1
            WRITE (LUNOUT,*) "Max dist 2 ",rymax2
            
         END IF
         
         CALL FFLUSH
         CALL FLUSH(LUNOUT)
*     |  *** The following 3 cards are mandatory ***
         TKESUM = ZERZER
         LFIRST = .FALSE.
         LUSSRC = .TRUE.
*  |  *** User initialization ***
      END IF
*  |
*  +-------------------------------------------------------------------*
*  Push one source particle to the stack. Note that you could as well
*  push many but this way we reserve a maximum amount of space in the
*  stack for the secondaries to be generated
*  Npflka is the stack counter: of course any time source is called it
*  must be =0
      NPFLKA = NPFLKA + 1
*  Wt is the weight of the particle
      WTFLK  (NPFLKA) = ONEONE
      WEIPRI = WEIPRI + WTFLK (NPFLKA)
*  Particle type (1=proton.....). Ijbeam is the type set by the BEAM
*  card
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IJBEAM .EQ. -2 .AND. LRDBEA ) THEN
         IARES  = IPROA
         IZRES  = IPROZ
         IISRES = IPROM
         CALL STISBM ( IARES, IZRES, IISRES )
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( IJBEAM .EQ. -2 ) THEN
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         ILOFLK (NPFLKA) = IJHION
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |
*  +-------------------------------------------------------------------*
*  |  Normal hadron:
      ELSE
         IONID = IJBEAM
         ILOFLK (NPFLKA) = IJBEAM
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
      END IF
*  |
*  +-------------------------------------------------------------------*
*  From this point .....
*  Particle generation (1 for primaries)
      LOFLK  (NPFLKA) = 1
*  User dependent flag:
      LOUSE  (NPFLKA) = 0
c DEV version
c*  No channeling:
c      KCHFLK (NPFLKA) = 0
c      ECRFLK (NPFLKA) = ZERZER
c*  Extra infos:
c      INFSTK (NPFLKA) = 0
c      LNFSTK (NPFLKA) = 0
c      ANFSTK (NPFLKA) = ZERZER
c*  Parent variables:
c      IPRSTK (NPFLKA) = 0
c      EKPSTK (NPFLKA) = ZERZER
c non dev vesion
*  No channeling:
      LCHFLK (NPFLKA) = .FALSE.
      DCHFLK (NPFLKA) = ZERZER
c end 
*  User dependent spare variables:
      DO 100 ISPR = 1, MKBMX1
         SPAREK (ISPR,NPFLKA) = ZERZER
 100  CONTINUE
*  User dependent spare flags:
      DO 200 ISPR = 1, MKBMX2
         ISPARK (ISPR,NPFLKA) = 0
 200  CONTINUE
*  Save the track number of the stack particle:
      ISPARK (MKBMX2,NPFLKA) = NPFLKA
      NPARMA = NPARMA + 1
      NUMPAR (NPFLKA) = NPARMA
      NEVENT (NPFLKA) = 0
      DFNEAR (NPFLKA) = +ZERZER
*  ... to this point: don't change anything


 777  CONTINUE

      nPrimRe=0
      IF( rFlagIsotope.EQ.ZERZER ) THEN
         rtoc=FLRNDM(rdummy)
         IF(rtoc.LT.rBr1) THEN
            rwmax=rwmax1
            rymax=rymax1
            nPrimRe=1
            nPidPrim = 3
         ELSE IF(rtoc.LT.rBr2+rBr1) THEN
            rwmax=rwmax2
            rymax=rymax2
            nPrimRe=2
            nPidPrim = 3
         ELSE IF(rtoc.LT.rBr3+rBr2+rBr1) THEN
            nPrimRe=3
            nPidPrim = 3
            rEprim=rE3
         ELSE IF(rtoc.LT.rBr4+rBr3+rBr2+rBr1) THEN
            nPrimRe=4
            nPidPrim = 3
            rEprim=rE4       
         ELSE
            nPrimRe=5
            nPidPrim = 7
            rEprim=rE5
         END IF
         
         
         IF(nPrimRe.EQ.1 .OR. nPrimRe.EQ.2) THEN
            
            NEstrMax=1000
            LExtrFailed=.TRUE.
            
            DO II=1, NEstrMax
               rwestratto=FLRNDM(rdummy)*rwmax
               ryestratto=FLRNDM(rdummy)*rymax
               rthisval=rDist(rwestratto,rwmax)
c     (rwmax-rwestratto)**2 *x*sqrt(rwestratto**2-ONEONE)
               IF(rthisval.GT.rymax) THEN
                  WRITE(LUNOUT,*)"ERROR rthisval ",rthisval
     &                 ," rymax ",rymax
                  CALL FLABRT ( 'SOURCE', 'rymax' )
               END IF
c     WRITE(LUNOUT,*) "Y ",lY,ryestratto,rthisval
               IF(ryestratto.LT.rthisval) THEN
                  rEprim=rwestratto
                  LExtrFailed=.FALSE.
                  EXIT
               END IF
            END DO
            
            IF(LExtrFailed) THEN
               WRITE(LUNOUT,*) "ERROR LExtrFailed"
c     CALL FLABRT ( 'SOURCE', 'extraction LExtrFailed' )
               GO TO 777
            END IF
            
         END IF

         rXprim = ZERZER 
         rYprim = ZERZER 
         rZprim = ZERZER 
         
      ELSE
         nPidPrim=IJBEAM
         IONID = nPidPrim
         rEMax=SQRT ( PBEAM**2 + AM (IONID)**2 ) - AM (IONID)
         rEprim=FLRNDM(rdummy)*rEMax

         rRaggioMax=5.0D+0
         rRaggioEstratto=FLRNDM(rdummy)*rRaggioMax
         rThetaEstratto=FLRNDM(rdummy)*Twopip

         rXprim = rRaggioEstratto*cos(rThetaEstratto)
         rYprim = rRaggioEstratto*sin(rThetaEstratto)
         rZprim = ZBEAM 
         
c$$$         ru=UBEAM
c$$$         rv=VBEAM
c$$$         rw=WBEAM
      END IF

      CALL RACO(ru,rv,rw)

      TKEFLK (NPFLKA) = rEprim
      ILOFLK (NPFLKA) = nPidPrim
      IONID = nPidPrim

*  Particle age (s)
      AGESTK (NPFLKA) = +ZERZER
      AKNSHR (NPFLKA) = -TWOTWO
*  Kinetic energy of the particle (GeV)
*      TKEFLK (NPFLKA) = SQRT ( PBEAM**2 + AM (IONID)**2 ) - AM (IONID)
*  Particle momentum
*      PMOFLK (NPFLKA) = PBEAM
      rPprim = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
     &     + TWOTWO *  AM (IONID) ) )
      PMOFLK (NPFLKA) = rPprim
      rMprim = AM (IONID) 

*     Cosines (tx,ty,tz)
c$$$  TXFLK  (NPFLKA) = UBEAM
c$$$  TYFLK  (NPFLKA) = VBEAM
c$$$  TZFLK  (NPFLKA) = WBEAM
      
      TXFLK  (NPFLKA) = ru
      TYFLK  (NPFLKA) = rv
      TZFLK  (NPFLKA) = rw

      rCXprim= TXFLK   (NPFLKA)
      rCYprim= TYFLK   (NPFLKA)
      rCZprim= TZFLK   (NPFLKA)
      
*     TZFLK  (NPFLKA) = SQRT ( ONEONE - TXFLK (NPFLKA)**2
*    &                       - TYFLK (NPFLKA)**2 )
*  Polarization cosines:
      TXPOL  (NPFLKA) = -TWOTWO
      TYPOL  (NPFLKA) = +ZERZER
      TZPOL  (NPFLKA) = +ZERZER
*  Particle coordinates
      rXprim = rXprim + XBEAM 
      rYprim = rYprim + YBEAM 
      rZprim = rZprim + ZBEAM 
      
      XFLK   (NPFLKA) = rXprim
      YFLK   (NPFLKA) = rYprim
      ZFLK   (NPFLKA) = rZprim

* increment the number of primaries
      nPrim=nPrim+1

*  Calculate the total kinetic energy of the primaries: don't change
      IF ( ILOFLK (NPFLKA) .EQ. -2 .OR. ILOFLK (NPFLKA) .GT. 100000 )
     &   THEN
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
         TKESUM = TKESUM + ( TKEFLK (NPFLKA) + AMDISC (ILOFLK(NPFLKA)) )
     &          * WTFLK (NPFLKA)
      ELSE
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      END IF
      RADDLY (NPFLKA) = ZERZER
*  Here we ask for the region number of the hitting point.
*     NREG (NPFLKA) = ...
*  The following line makes the starting region search much more
*  robust if particles are starting very close to a boundary:
      CALL GEOCRS ( TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
      CALL GEOREG ( XFLK  (NPFLKA), YFLK  (NPFLKA), ZFLK  (NPFLKA),
     &              NRGFLK(NPFLKA), IDISC )
*  Do not change these cards:
      CALL GEOHSM ( NHSPNT (NPFLKA), 1, -11, MLATTC )
      NLATTC (NPFLKA) = MLATTC
      CMPATH (NPFLKA) = ZERZER
      CALL SOEVSV
      RETURN
*=== End of subroutine Source =========================================*
      END




      DOUBLE PRECISION FUNCTION rDist(rxf, rxmaxf)
      implicit none

c      INCLUDE '(DBLPRC)'
      DOUBLE PRECISION rxf, rxmaxf
      double precision rwf, rwmaxf
      double precision rElMassMev
      rElMassMev = 0.510998928D+0
      rwf=rxf/rElMassMev * 10.D+3 +1.D+0
      rwmaxf=rxmaxf/rElMassMev* 10.D+3  +1.D+0

      IF(rwf.GT.1 .AND. rwf.LT.rwmaxf) THEN
         rDist=(rwmaxf-rwf)**2 *rwf*sqrt(rwf**2 - 1.D+0)
c         rDist=(rTmp-1.D+0) * rElMassMev
      ELSE
         rDist=0.D+0
      END IF

c$$$      IF(rDist.LT.0) THEN
c$$$         rDist=0.D+0
c$$$      END IF
c$$$
      RETURN
      END FUNCTION rDist
