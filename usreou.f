*$ CREATE USREOU.FOR
*COPY USREOU
*
*=== Usreou ===========================================================*
*
      SUBROUTINE USREOU ( NOMORE )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1991-2012      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     USeR Event OUtput: this routine is called at the end of each     *
*     event                                                            *
*                                                                      *
*     Created on 01 January 1991   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on 20-Apr-12     by    Alfredo Ferrari               *
*                                                                      *
*                                                                      *
*       Input variables:                                               *
*                                                                      *
*              Nomore = if > 0 the run will be terminated              *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE 'phcount.inc'

      LOGICAL LFIRST
      SAVE LFIRST
      DATA LFIRST / .TRUE. / 
      
      IF ( LFIRST ) THEN
         LFIRST = .FALSE.
         
         WRITE (LUNOUT,*) "==>                                     <=="
         WRITE (LUNOUT,*) "==> customized usreou                   <=="
         WRITE (LUNOUT,*) "==> by Carlo Mancini, 28/01/2016        <=="
         WRITE (LUNOUT,*) "==>                                     <=="
         CALL FFLUSH
         CALL FLUSH(LUNOUT)
         
      END IF
      
      IF ((nPhot.GT.0 .OR. rEInPte.GT.0)) THEN
         WRITE(38,*) nPrim,nPhot,nInPart
     &        , nPrimRe, rEprim, nPidPrim, rEOutSou
     &        , rEInPte, rEInPtePh, rEInPteCh
     &        , rXprim, rYprim, rZprim
     &        , rCXprim, rCYprim, rCZprim
     &        , rPprim, rMprim, rEprimPte
     &        , nRegPrim
     &        , nInPart
     &        , rEInPteS, rEInPtePhS, rEInPteChS

      END IF
      
*     reset the common 
      nRegPrim=-1
      lTrig=.FALSE.
      rEInPte=ZERZER
      rEInPtePh=ZERZER
      rEInPteCh=ZERZER
      rEInPteS=ZERZER
      rEInPtePhS=ZERZER
      rEInPteChS=ZERZER
      rEprim=ZERZER
      rPprim=ZERZER
      rMprim=ZERZER
      rEOutSou=ZERZER
      rXprim=ZERZER
      rYprim=ZERZER
      rZprim=ZERZER
      rCXprim=ZERZER
      rCYprim=ZERZER
      rCZprim=ZERZER
      rEprimPte=ZERZER
      nPidPrim=0
      nPhot=0
      nInPart=0
      nInPartS=0
      nPrimRe=0

* increment the number of primaries
      nPrim=nPrim+1


c$$$      WRITE (LUNOUT,*) "==>                end usreo              <=="
c$$$      CALL FFLUSH
c$$$      CALL FLUSH(LUNOUT)
      
      RETURN
*=== End of subroutine Usreou =========================================*
      END

