*$ CREATE SOURCE.FOR
*COPY SOURCE
*
*=== source ===========================================================*

c
c 1 raggio minimo sorgente
c 2 raggio massimo
c 3 seleziona isotopo: 1 solo Sr, 2 solo Y
c 4 Z minima
c 5 Z massima
c 6 taglio E minima in MeV (non piu')
c 6 taglio regione

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
      LOGICAL  lY, lSr
*
      SAVE nSOU1, nSOU2
      SAVE rymaxY, rymaxSr
      SAVE rwmaxY, rwmaxSr
      SAVE LFIRST
      SAVE rRaggioInterno, rRaggioEsterno
      SAVE rZmin, rZmax
      SAVE rtaglio
      SAVE rdummy
      SAVE nWantedRegion

      DATA LFIRST / .TRUE. /


*======================================================================*
*                                                                      *
*                 BASIC VERSION                                        *
*                                                                      *
*======================================================================*
      NOMORE = 0
*  +-------------------------------------------------------------------*
*  |  First call initializations:
      IF ( LFIRST ) THEN
         
         rRaggioInterno=WHASOU(1)
         rRaggioEsterno=WHASOU(2)
         rZmin=WHASOU(4)
         rZmax=WHASOU(5)

         nWantedRegion=-1
         rtaglio=ZERZER
         nWantedRegion=WHASOU(6)
c         rtaglio=WHASOU(6)*1.D-3

         rymaxY=0.D+0
         rymaxSr=0.D+0

         rdummy=17.D+0
         
         rwmaxY=2.23D-3 
         rwmaxSr=0.53D-3 
c     rymaxY=((rwmaxY/AMELCT)**4-(rwmaxY/AMELCT)**2)*7.D-2
c     rymaxSr=((rwmaxSr/AMELCT)**4-(rwmaxSr/AMELCT)**2)*7.D-2


         WRITE (LUNOUT,*) "==>                                     <=="
         WRITE (LUNOUT,*) "==> customized source SrY               <=="
         WRITE (LUNOUT,*) "==> by Cicciociolla,  25/06/2015        <=="
         WRITE (LUNOUT,*) "==>                                     <=="
         WRITE (LUNOUT,*) "==> by Carlo Mancini, 28/01/2016        <=="
         WRITE (LUNOUT,*) "==>                                     <=="
         WRITE (LUNOUT,*) "end point Y ",rwmaxY
         WRITE (LUNOUT,*) "end point Sr ",rwmaxSr
         WRITE (LUNOUT,*) "AMELCT ",AMELCT
         WRITE (LUNOUT,*) "Ek min: ",rtaglio
         WRITE (LUNOUT,*) "Raggio min: ",rRaggioInterno
         WRITE (LUNOUT,*) "Raggio max: ",rRaggioEsterno
         IF(WHASOU(3).EQ.0) THEN 
            WRITE (LUNOUT,*) "Entrambi gli isotopi"
         ELSE IF(WHASOU(3).EQ.1) THEN  
            WRITE (LUNOUT,*) "Solo Sr"
         ELSE IF(WHASOU(3).EQ.2) THEN 
            WRITE (LUNOUT,*) "Solo Y"
         END IF
         IF(rZmin.GE.rZmax) THEN
            WRITE (LUNOUT,*) "sorgente bidimesionale"
         ELSE
            WRITE (LUNOUT,*) "Z min: ",rZmin
            WRITE (LUNOUT,*) "Z max: ",rZmax
         END IF
         
         CALL FFLUSH
         CALL FLUSH(LUNOUT)
         DO I=1,1000
            rtmpX=(I*ONEONE)/1000.D+0 *(rwmaxY-rtaglio)+rtaglio
            rTmpY=rDist(rtmpX,rwmaxY)
            rTMpSr=rDist(rtmpX,rwmaxSr)
            WRITE(51,*) rtmpX,rTmpY,rTmpSr
            IF(rTmpY.GT.rymaxY) THEN
               rymaxY=rTmpY
            END IF
            IF(rTmpSr.GT.rymaxSr) THEN
               rymaxSr=rTmpSr
            END IF
         END DO
         WRITE (LUNOUT,*) "Max dist Y ",rymaxY
         WRITE (LUNOUT,*) "Max dist Sr ",rymaxSr
         rymaxY=rymaxY+rymaxY*1.D-1
         rymaxSr=rymaxSr+rymaxSr*1.D-1
         WRITE (LUNOUT,*) "Max dist Y ",rymaxY
         WRITE (LUNOUT,*) "Max dist Sr ",rymaxSr

         CALL GEON2R("CAP_SOU ",nSOU2, IERR)
         IF ( IERR .GT. 0 ) 
     &        CALL FLABRT ( 'SOURCE', 'CAP_SOU not recognized' )

         CALL GEON2R("SPA_SOU ",nSOU1, IERR)
         IF ( IERR .GT. 0 ) 
     &        CALL FLABRT ( 'SOURCE', 'SPA_SOU not recognized' )

         IF(nWantedRegion.GT.0) THEN
            IF(nWantedRegion.EQ.1) THEN
               WRITE (LUNOUT,*) "Generating in region SPA_SOU"
            ELSE IF(nWantedRegion.EQ.2) THEN
               WRITE (LUNOUT,*) "Generating in region CAP_SOU"
            ELSE
               WRITE (LUNOUT,*) "Generating region not recognized: "
     &              ,nWantedRegion
               CALL FLABRT ( 'SOURCE', 'Generating region')
            END IF
         END IF
               
         CALL FFLUSH
         CALL FLUSH(LUNOUT)
*  |  *** The following 3 cards are mandatory ***
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
*  No channeling:
*devel
c$$$      KCHFLK (NPFLKA) = 0
c$$$      ECRFLK (NPFLKA) = ZERZER
c$$$*  Extra infos:
c$$$      INFSTK (NPFLKA) = 0
c$$$      LNFSTK (NPFLKA) = 0
c$$$      ANFSTK (NPFLKA) = ZERZER
c$$$*  Parent variables:
c$$$      IPRSTK (NPFLKA) = 0
c$$$      EKPSTK (NPFLKA) = ZERZER
*non devel
*  No channeling:
      LCHFLK (NPFLKA) = .FALSE.
      DCHFLK (NPFLKA) = ZERZER

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

*      rtoc=FLRNDM(rdummy)
      IF(WHASOU(3).EQ.0) THEN 
         rtoc=FLRNDM(rdummy)
      ELSE IF(WHASOU(3).EQ.1) THEN  
         rtoc=0.2
      ELSE IF(WHASOU(3).EQ.2) THEN 
         rtoc=0.7
      END IF

      IF(rtoc.GT.0.5) THEN
         rwmax=rwmaxY
         rymax=rymaxY
         lY=.TRUE.
         lSr=.FALSE.
      ELSE
         rwmax=rwmaxSr
         rymax=rymaxSr
         lY=.FALSE.
         lSr=.TRUE.
      END IF



 333  CONTINUE
      NEstrMax=1000
      LExtrFailed=.TRUE.

      DO II=1, NEstrMax
         rwestratto=FLRNDM(rdummy)*(rwmax-rtaglio)+rtaglio
         ryestratto=FLRNDM(rdummy)*rymax
         rthisval=rDist(rwestratto,rwmax)
c     (rwmax-rwestratto)**2 *x*sqrt(rwestratto**2-ONEONE)
         IF(rthisval.GT.rymax) THEN
            WRITE(LUNOUT,*) "ERROR rthisval ",rthisval," rymax ",rymax
            CALL FLABRT ( 'SOURCE', 'rymax' )
         END IF
c         WRITE(LUNOUT,*) "Y ",lY,ryestratto,rthisval
         IF(ryestratto.LT.rthisval) THEN
            rEprim=rwestratto
            
            rbuoni=rbuoni+ONEONE
            TKEFLK (NPFLKA) = rEprim
            LExtrFailed=.FALSE.
            EXIT
         END IF
      END DO
      
      IF(LExtrFailed) THEN
         WRITE(LUNOUT,*) "ERROR LExtrFailed"
c         CALL FLABRT ( 'SOURCE', 'extraction LExtrFailed' )
         GO TO 777
      END IF
      
*  Particle age (s)
      AGESTK (NPFLKA) = +ZERZER
      AKNSHR (NPFLKA) = -TWOTWO
*  Kinetic energy of the particle (GeV)
*      TKEFLK (NPFLKA) = SQRT ( PBEAM**2 + AM (IONID)**2 ) - AM (IONID)
*  Particle momentum
*      PMOFLK (NPFLKA) = PBEAM
      IONID = 3
      nPidPrim = IONID
      rPprim = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
     &     + TWOTWO * AMELCT ) )
      PMOFLK (NPFLKA) = rPprim
      rMprim = AMELCT
*  Cosines (tx,ty,tz)
c$$$      TXFLK  (NPFLKA) = UBEAM
c$$$      TYFLK  (NPFLKA) = VBEAM
c$$$      TZFLK  (NPFLKA) = WBEAM
      CALL RACO(ru,rv,rw)
      TXFLK  (NPFLKA) = ru
      TYFLK  (NPFLKA) = rv
      TZFLK  (NPFLKA) = rw
      
      rCXprim= TXFLK   (NPFLKA)
      rCYprim= TYFLK   (NPFLKA)
      rCZprim= TZFLK   (NPFLKA)

c$$$      WRITE(52,*)   TKEFLK(NPFLKA),TXFLK(NPFLKA)
c$$$     &     ,TYFLK(NPFLKA),TZFLK(NPFLKA)
*     TZFLK  (NPFLKA) = SQRT ( ONEONE - TXFLK (NPFLKA)**2
*    &                       - TYFLK (NPFLKA)**2 )
*  Polarization cosines:
      TXPOL  (NPFLKA) = -TWOTWO
      TYPOL  (NPFLKA) = +ZERZER
      TZPOL  (NPFLKA) = +ZERZER
*  Particle coordinates
 778  CONTINUE
      rXestr=FLRNDM(rdummy)*TWOTWO*rRaggioEsterno-rRaggioEsterno
      rYestr=FLRNDM(rdummy)*TWOTWO*rRaggioEsterno-rRaggioEsterno
      IF(rZmin.GE.rZmax) THEN
         rZestr=ZERZER
         ELSE 
*            rZestr=FLRNDM(rdummy)*(rZmin+rZmax)-rZmin
            rZestr=FLRNDM(rdummy)*(rZmax-rZmin)+rZmin
         END IF
      rRaggioEstr=SQRT(rXestr**2+rYestr**2)
      IF(rRaggioEstr.GT.rRaggioEsterno .OR.
     &     rRaggioEstr.LT.rRaggioInterno) THEN
         GO TO 778
      END IF

      rXprim= rXestr + xbeam
      rYprim= rYestr + ybeam
      rZprim= rZestr + zbeam
      
      XFLK   (NPFLKA) = rXprim
      YFLK   (NPFLKA) = rYprim
      ZFLK   (NPFLKA) = rZprim

      nPidPrim=3

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

      IF( NRGFLK(NPFLKA).EQ. nSOU1) THEN
         nRegPrim=1
      ELSE IF( NRGFLK(NPFLKA).EQ. nSOU2) THEN
         nRegPrim=2
      ELSE
         nRegPrim=-1
      END IF
      
      IF(nWantedRegion.GT.0) THEN
         IF(nWantedRegion.EQ.1 .AND.  NRGFLK(NPFLKA).NE.nSOU1) THEN
            GO TO 777
         END IF
         IF(nWantedRegion.EQ.2 .AND.  NRGFLK(NPFLKA).NE.nSOU2) THEN
            GO TO 777
         END IF
      END IF
      
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
