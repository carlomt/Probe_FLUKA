*----------------------------------------------------------------------*
      PARAMETER (IMAXPHC = 1000)
      PARAMETER (IBINMAXPH = 100)
      PARAMETER (rBINSIZE = 5.D-3)
      LOGICAL  lTrig
      COMMON / PHCOUNT / 
     &     rEInPte   !Ek quando entra in pterfenile
     &     , rEInPtePh   !Ek quando entra in pterfenile
     &     , rEInPteCh   !Ek quando entra in pterfenile
     &     , rEInPteS   !Ek quando entra in pterfenile de lato
     &     , rEInPtePhS   !Ek quando entra in pterfenile de lato
     &     , rEInPteChS   !Ek quando entra in pterfenile de lato
     &     , rEprim             !Ek primario quando nasce
     &     , rEprimPte             !Ek primario quando entra nella sonda
     &     , rPprim             !P primario quando nasce
     &     , rMprim             !M primario
     &     , rEOutSou           !Ek primario quando esce
     &     , rXprim
     &     , rYprim
     &     , rZprim
     &     , rCXprim
     &     , rCYprim
     &     , rCZprim
     &     , nPidPrim
     &     , nPhot
     &     , nPrim              !n primario
     &     , nInPart            !n secondari entrati nell'alluminio
     &     , nInPartS           !n secondari entrati nel pterfenile de lato
     &     , nPrimRe            !flag processo che ha prodotto il primario
     &     , nRegPrim           !regione in cui e' stato prodotto il primario
     &     , lTrig              !evento non vuoto
      SAVE / PHCOUNT /
