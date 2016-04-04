*$ CREATE USRINI.FOR
*COPY USRINI
*
*=== usrini ===========================================================*
*
      SUBROUTINE USRINI ( WHAT, SDUM )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1991-2014      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     USeR INItialization: this routine is called every time the       *
*                          USRICALL card is found in the input stream  *
*                                                                      *
*                                                                      *
*     Created on 01 January 1991   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on  26-May-14    by    Alfredo Ferrari               *
*                                                                      *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE 'phcount.inc'

      DIMENSION WHAT (6)
      CHARACTER SDUM*8
* Start_Devel_seq
      CHARACTER SDUM10*10
* End_Devel_seq

*
*  Don't change the following line:
      LUSRIN = .TRUE.

* Start_Devel_seq
      SDUM   = SDUM10 (1:8)
* End_Devel_seq

* *** Write from here on *** *

      WRITE (LUNOUT,*) "==>                                     <=="
      WRITE (LUNOUT,*) "==> customized usrini                   <=="
      WRITE (LUNOUT,*) "==> by Carlo Mancini, 26/11/2015        <=="
      WRITE (LUNOUT,*) "==>                                     <=="
      CALL FFLUSH
      CALL FLUSH(LUNOUT)

      nPrim=1

      lTrig=.FALSE.
      rEInPte=ZERZER
      rEInPtePh=ZERZER
      rEInPteCh=ZERZER
      rEInPteS=ZERZER
      rEInPtePhS=ZERZER
      rEInPteChS=ZERZER
      rEprim=ZERZER
      rEprimPte=ZERZER
      rPprim=ZERZER
      rMprim=ZERZER
      rEOutSou=ZERZER
      rXprim=ZERZER
      rYprim=ZERZER
      rZprim=ZERZER
      rCXprim=ZERZER
      rCYprim=ZERZER
      rCZprim=ZERZER
      nPidPrim=0
      nPhot=0
      nInPart=0
      nInPartS=0
      nPrimRe=0
   
      RETURN
*=== End of subroutine Usrini =========================================*
      END

