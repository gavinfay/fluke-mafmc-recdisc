!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!SESSF Operating Model
!as per BET_opmod_GF.doc
!Gavin Fay
!Last Updated August 2019
!
!Requires following files for successful compilation:
!   Common.FOR
!	C.F90
!	Dminim.For
!	Matrix.for
!   GenData.f90
!	Sinatra.INC
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	works using Compaq Visual Fortran
!
!	Compile as a DLL for linking to R controller:
!	df /warn:noalign Sinatra.f90 /dll
!	(calls are to subroutines 'sinatra' for historical 
!	period and 'sinatranew' for projections given an RBC
!
!	Alternatively, the model can be compiled as an executable
!	to test the historical period and perform a simple projection
!
!	df /warn:noalign Sinatra.f90
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!       Updated 2014 May 27, Gavin Fay
!
!       Text above (and below) is out of date, can compile as dll but most 
!       recent work has been as calling the exe directly, full MSE stuff 
!	implemented now.
!	See email re structure of new files, body of operating model
!       still works as below.
!	Work of R controller program is now all done internally.
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	Following files required to run historical period:
!
!	Sinatra.ctl
!	catches.dat
!	dataspec.ctl
!
!	Additionally, the following are needed to run the projection
!	
!	RBC.out
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	Model structure:
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	for each simulation,
!	  {
!		call to Sinatra or (E) Testsinatra
!		the model then runs the historical projection
!		and generates the historical data
!10		  - conducts assessment
!		  - calculates RBC
!		call to sinatranew for a projection
!		 -	length of projection depends on input frequency of assessment
!		sinatranew calls futureproj, does a population projection given RBC and timespan of that RBC 
!		generates data for this projection, appends to existing datafiles
!		if not at end of projection period
!			go to 10
!		else
!			calculate and store performance metrics for simulation
!     }
!
!*****************************************************************************************
!***	*over view of Historical projection
!	Readin : reads in specifications, and catch data
!	HistSetUp : sets up necessary quantities & fills in vectors for time-varying parameters
!				(e.g. growth, recruitment devs, selectivity)
!	GetInit : Finds initial Numbers at age given movement and mortality, calculate S0
!	Then loop over years of historical period calling
!	 {
!	  PopUpdate : calculates annual exploitation rate, and updates population dynamics
!				  calculate Spawning biomass
!    }
!	FindHistData : Generates the historical data, writes to *.inp files
!
!	returns values for time-carying components (inlcuding Numbers at age) for last year
!*****************************************************************************************
!***	*over view of Future projection
!	Finds what type of assessment / control rule to apply
!     runs assessment model
!     applies control rule to generate RBC
!   Call to Sinatranew passes: values in current year for N@age, Selectivity, Growth, etc.
!							   duration of projection (determined by frequency of assessment)
!   Reads in RBC - turns into catch (by fleet)
!	Futureproj : does projection by calling popupdate
!	FindNewData : generates the data for the projection period as per FindHistdata, appends to *.inp files
!	
!	returns values for time-carying components for last year of projection
!
!*****************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	PROGRAM ToothSinatra

	IMPLICIT NONE

	INCLUDE 'Sinatra.INC'

!   LOCAL VARIABLES
	INTEGER OpScen(1000),II,Iproj,Yr2,Yr1,Isim  !,Nsim
    CHARACTER*25 OpModFile
	REAL*8 DUM

!	OPEN MASTER CTL FILE
    OPEN(UNIT=13,FILE='master.ctl')
	READ(13,*)
	READ(13,*) Nsim
    READ(13,*)
	READ(13,*) ISEEDX
	READ(13,*) ISEEDZ
	READ(13,*)
	READ(13,*) OpModFile
    READ(13,*)
    READ(13,*) OpScen(1:Nsim)	
	CLOSE(13)

!	OPEN GENERAL JUNKFILE 
	OPEN(UNIT=99,FILE='Control.junk',STATUS="REPLACE")

!	GET HARVEST STRATEGY SPECIFICATIONS
    CALL GetHCRspecs()
    Nproj = CEILING(FLOAT(HCRspecs(1))/FLOAT(HCRspecs(2)))
	
	WRITE(99,*) 'Stepping into ReadIn'

!	READ IN CONTROL VARIABLES AND SET-UP
	CALL ReadIn(1)
	WRITE(99,*) 'ReadIN finished'



!	WRITE OUTPUT FILE HEADERS
    OPEN (UNIT=121,FILE='spawbio.out')
	WRITE(121,'(1000(I4,1x))') (II,II=Fyear,Lyear+HCRspecs(1))
    CLOSE(121)
    OPEN (UNIT=122,FILE='totcatch.out')
	WRITE(122,'(1000(I4,1x))') (II,II=Fyear,Lyear+HCRspecs(1))
    CLOSE(122)
    OPEN (UNIT=123,FILE='exprate.out')
	WRITE(123,'(A2,1x,1000(I4,1x))') "FL",(II,II=Fyear,Lyear+HCRspecs(1))
    CLOSE(123)
    OPEN (UNIT=123,FILE='rbctrack.out')
	WRITE(123,'(A32)') 'Isim, Iproj, estF, RBC(1),RBC(2)'
    CLOSE(123)


!	GLOBAL FLAG IDENTIFYING READ IN FROM POSTERIOR RATHER THAN USE SINATRA.CTL VALUES
	UseMCMC = 0
!	NOMpars is number of op mod parameters
    !NOMpars(2) = 7  !number of growth parameters
    !NOMpars(3) = 19  !number of recruitment devs
    !NOMpars(4) = 20  !number of selex parameters
    !NOMpars(5) = 2  !number of tagging parameters
    !NOMpars(1) = SUM(NOMpars(2:4))+ 1 + 2 + (Nreg-1) + Nreg
	CALL GetMCMCspecs()



!   LOOP OVER SIMUALTIONS
    DO 4000 Isim=1,Nsim

!   GET SEEDS
	OPEN(UNIT=13,FILE='master.ctl')
	DO II=1,6
	 READ(13,*)
	ENDDO
	READ(13,*) OpModFile
	READ(13,*)
	READ(13,*) (OpScen(II),II=1,Isim)
    DO II=1,OpScen(Isim)
	 READ(13,*)
    ENDDO
    READ(13,*) ISEEDX,ISEEDZ
    CLOSE(13)


!	OPERATING MODEL PARAMETERS
	OPEN(UNIT=13,FILE=OpModFile)	
    DO II=1,OpScen(Isim)
	 READ(13,*)
	ENDDO
	IF (DUMcol.GE.1) THEN
	 READ(13,*) DUM,DUM,(OpModPars(II),II=1,NOMpars(1))
	ELSE
	 READ(13,*) (OpModPars(II),II=1,NOMpars(1))
	ENDIF
	CLOSE(13)
!	WRITE(*,*) (OpModPars(II),II=1,NOMpars(1))
!	STOP

!	DO HISTORICAL PROJECTION
	WRITE(99,*) 'Starting Historical Projection'
	CALL HistProj()
	WRITE(99,*) 'Historical Projection finished'

!	GET HISTORICAL DATA
	CALL FindHistData()
!call estimation model & do harvest strategy
!      CALL EstMod(Yr2,Isim)   going to need this here for sim testing....

	IF (HCRspecs(3).EQ.1.OR.HCRspecs(3).EQ.8) THEN
	! OPEN(UNIT=18,FILE='forereport.out')
	 !WRITE(18,*) '#dump file for forecast-report.sso'
	 !CLOSE(18)
	 OPEN(UNIT=18,FILE='ss3par.out')
	 WRITE(18,*) '#dump file for ss3.par'
	 CLOSE(18)
    ENDIF


!		move this somewhere sensible later - moved from DoTier 1 4th May 2011
	 OPEN(UNIT=15,FILE='tooth_ss3_tier1.ctl')
	 DO II=1,35
	  READ(15,*)
	 ENDDO
     READ(15,*) Nprojyrs
	 READ(15,*)
	 READ(15,*) (MaxCatch(II),II=1,Nflt)
	 READ(15,*)
	 READ(15,*) Forecat
	 CLOSE(15)

    
!	 DO PROJECTIONS
	 Yr2 = Lyear
     DO 4001 Iproj=1,Nproj

 	  !call estimation model & do harvest strategy
      CALL EstMod(Yr2,Isim,Iproj)

	  Yr1 = Yr2 + 1
	  Yr2 = Yr1 + HCRspecs(2) - 1	

      !#if first projection, get allocation parameters
      !if (iproj == 1)
      !get.alloc(hcr$alloc.yrs,nfleet,nregion)

  	  !future projection
	  CALL FutureProj(Yr1,Yr2)
      !GET FUTURE DATA
	  CALL FindNewData(Yr1,Yr2)

4001 CONTINUE
	!WRITE TO OUTPUT FILES
  !#evaluate and save performance metrics
    CALL SaveOutputs(Yr2)
	!CLOSE SIMULATION LOOP
4000 CONTINUE

	STOP

	END PROGRAM ToothSinatra

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SUBROUTINE GetMCMCspecs

	IMPLICIT NONE
	INTEGER II,Iflt
	
	INCLUDE 'Sinatra.INC'

	OPEN(UNIT=13,FILE='use_mcmc.ctl')
	READ(13,*)
	READ(13,*) UseMCMC
	READ(13,*)
	READ(13,*) DUMcol
	READ(13,*)
	READ(13,*) OMNsex
	READ(13,*)
	READ(13,*) OMAgeL1
	READ(13,*)
	READ(13,*)
	OMgrowth = 0
	READ(13,*) (OMgrowth(II),II=1,7)
	IF(OMNsex.EQ.2) THEN
	 READ(13,*)
	 READ(13,*) (OMgrowth(II),II=8,14)
	ENDIF
	NOMpars(2) = 0
	DO II=1,14
	 IF (OMgrowth(II).EQ.1) NOMpars(2) = NOMpars(2) + 1
	ENDDO
	DO II=1,5
	 READ(13,*)
	ENDDO
	OMsteep = 0
	READ(13,*) OMsteep
	READ(13,*)
	READ(13,*) OMrecFY,OMrecLY
	NOMpars(3) = OMrecLY-OMrecFY+1
	READ(13,*)
	READ(13,*) (OMseltype(Iflt),Iflt=1,Nflt)
	READ(13,*)
	READ(13,*) (OMNselpars(Iflt),Iflt=1,Nflt)
	NOMpars(4) = SUM(OMNselpars(1:Nflt))
	READ(13,*)
	READ(13,*) (OMdoreten(Iflt),Iflt=1,Nflt)
	READ(13,*)
	READ(13,*) 	NOMpars(5) !Number of tag params
	NOMpars(1) = SUM(NOMpars(2:5)) + 1 + Nreg + 2*SUM(OMdoreten(1:Nflt)) + (Nreg-1)
	NOMpars(1) = NOMpars(2) + Nreg + NOMpars(3) + NOMpars(4) + 2*SUM(OMdoreten(1:Nflt)) + NOMpars(5) + OMsteep
	IF (Nreg.GT.1) NOMpars(1) = NOMpars(1) + Nreg
	CLOSE(13)

	RETURN

	STOP

	END SUBROUTINE 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


	SUBROUTINE SaveOutputs(Yr2)

	IMPLICIT NONE
	INTEGER Iyr,Yr2,Iflt

	INCLUDE 'Sinatra.INC'
	
	OPEN(UNIT=18,FILE='spawbio.out',POSITION='APPEND')
	WRITE(18,'(1000(F10.2,1x))') (SUM(SpawBio(1:Nstk,0,Iyr)),Iyr=Fyear,Yr2)
	CLOSE(18)
	OPEN(UNIT=18,FILE='totcatch.out',POSITION='APPEND')
	WRITE(18,'(1000(F10.2,1x))') (SUM(RetCatch(1:Nflt,1:Nreg,Iyr)),Iyr=Fyear,Yr2)
	CLOSE(18)
	OPEN(UNIT=18,FILE='exprate.out',POSITION='APPEND')
	DO Iflt=1,Nflt
	WRITE(18,'(I2,1x,1000(F10.7,1x))') Iflt,(Ufleet(Iflt,1:Nreg,Iyr),Iyr=Fyear,Yr2)
	ENDDO
	CLOSE(18)

	RETURN

	STOP

	END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!! This subroutine does the estimation, and applies the harvest strategy

    SUBROUTINE EstMod(Yr2,Isim2,Iproj2)

	IMPLICIT NONE
	INTEGER Yr2,Isim2,Iproj2,II,Jyr,IDUM,Iflt,Jflt,Jreg,Ireg,JJ,ITempArray(100,100),Npts
	REAL*8 TempTot(2500),Temp,Temp2,TempVec(200),DiscountUse,EstDisc(100),TempArray(100,100)


	INCLUDE 'Sinatra.INC'
    
	!WRITE(*,*) 'GAV1'

	!! Tier 1 with stochastic projection (CCAMLR)
	IF (HCRspecs(3).EQ.8) CALL DoTier1(1,Isim2,Iproj2,Yr2)
	!! Tier 1
	IF (HCRspecs(3).EQ.1) CALL DoTier1(0,Isim2,Iproj2,Yr2)
	!! Tier 3
    IF (HCRspecs(3).EQ.3) CALL DoTier3(Yr2,Iproj2)
	!! Tier 4
	IF (HCRspecs(3).EQ.4) CALL DoTier4(Yr2,Iproj2)
	!! Tier 1 with pseudo-assessment
	IF (HCRspecs(3).EQ.11) CALL DoTier11(Yr2,Iproj2)
	!! ICES FMSY Btrigger with pseudo-assessment
	IF (HCRspecs(3).EQ.51) CALL DoTier51(Yr2,Iproj2)

    !WRITE(*,*) 'GAV1'

	!figure out effect of discards on Retained rbc
	TempArray = 0.d0
	ItempArray = 0
	TempTot = 0.d0
	II=0
	EstDisc = 0.d0
	OPEN(UNIT=14,FILE='Discard.inp')
	READ(14,*)
4021 READ(14,*,END=4020,ERR=4020) Jyr,IDUM,Iflt,Temp
	!WRITE(99,*) Jyr,Iflt,Temp
	IF (Jyr.GE.(Yr2-HCRspecs(4)+1).AND.Jyr.LE.Yr2) THEN
	 TempArray((HCRspecs(4)-(Yr2-Jyr)),Iflt) = Temp
	 ItempArray((HCRspecs(4)-(Yr2-Jyr)),Iflt) = 1
	ENDIF
	GOTO 4021
4020 CONTINUE	  
	CLOSE(14)
	DO Jyr=1,HCRspecs(4)
	 Npts = SUM(ItempArray(Jyr,1:Nflt))
	 IF (Npts.EQ.1) EstDisc(Jyr) = SUM(TempArray(Jyr,1:Nflt))
	 IF (Npts.GT.1) THEN
	  TempTot = 0.d0
	  DO Iflt=1,Nflt
	   TempTot(Jyr) = TempTot(Jyr) + ItempArray(Jyr,Iflt)*SUM(RetCatch(Iflt,1:Nreg,(Yr2-HCRspecs(4)+Jyr)))
	   TempArray(Jyr,Iflt) = TempArray(Jyr,Iflt)*TempTot(Iflt)
	  ENDDO
	  EstDisc(Jyr) = SUM(TempArray(Jyr,1:Nflt))/TempTot(Jyr)
	 ENDIF
	ENDDO
	Temp = 0.d0
	Temp2 = 0.d0
	DO II=1,HCRspecs(4)
!	 WRITE(*,*) II, EstDisc(II),(1.d0/(2.d0**(HCRspecs(4)-II)))
	 IF (SUM(ItempArray(II,1:Nflt)).GT.0) THEN
	  Temp = Temp + (1.d0/(2.d0**(HCRspecs(4)-II)))*EstDisc(II) !TempTot(Yr2-II+1)
	  Temp2 = Temp2 + (1.d0/(2.d0**(HCRspecs(4)-II)))
	 ENDIF
	ENDDO
	Temp = Temp/Temp2
	!WRITE(99,*) Temp
	 IF (SUM(ItempArray(1:HCRspecs(4),1:Nflt)).GT.0) EstRBC(2) = EstRBC(1)*(1.d0-Temp)
    !WRITE(*,*) 'estrbc',EstRBC(2),EstRBC(1)

	!apply discount factor
!	WRITE(*,*) Discountfactor
	DiscountUse = Discountfactor
	IF (DiscountStableYrs.GT.0) THEN
	 !WRITE(*,*) 'here'
	 EstQuant(6) = 0
	 IF (HCRspecs(3).EQ.4.OR.HCRspecs(3).EQ.3) THEN
	  OPEN(UNIT=15,FILE='Tier4.ctl')
	  READ(15,*)
	  READ(15,*)
	  READ(15,*)
	  READ(15,*)
	  READ(15,*) Jflt
	  READ(15,*)
	  READ(15,*) Jreg
	  CLOSE(15)
	  OPEN(UNIT=15,FILE='CPUE.inp')
	  READ(15,*)
	  JJ=0
	  TempVec=0.d0
4022  READ(15,*,END=4023,ERR=4023) Jyr,Ireg,Iflt,Temp
	  IF (Iflt.EQ.Jflt.AND.Ireg.EQ.Jreg.AND.Jyr.GE.(Yr2-DiscountStableYrs+1)) THEN
       JJ=JJ+1
	   TempVec(JJ) = Temp
	  ENDIF
	  GOTO 4022
4023  CONTINUE
	  CLOSE(15)
	  IF (JJ.LT.1) JJ = 1
	  Temp = SUM(TempVec(1:JJ))/FLOAT(JJ)
	  TempVec(1:JJ) = (TempVec(1:JJ)-Temp)**2.d0
	  IF (JJ.LT.2) JJ = 2
	  Temp2 = SQRT(SUM(TempVec(1:JJ))/(FLOAT(JJ-1)))/Temp
	  IF (Temp2.LT.DiscountStableCV) THEN
	   DiscountUse = 0
	   EstQuant(6) = 1
	  ENDIF
	  !WRITE(*,*) Yr2,Temp,Temp2,DiscountStableYrs,DiscountStableCV,EstQUant(5),Discountfactor
	 ENDIF
	ENDIF
	
	!WRITE(*,*) EstQuant(5),DiscountUse

    EstRBC(1:2) = (1.d0-DiscountUse)*EstRBC(1:2)
    !WRITE(*,*) 'estrbc',EstRBC(2),EstRBC(1)
	IF (EstRBC(1).GT.999999.d0) EstRBC(1:2) = 999999.d0

	IF (MaxChange.NE.-1) THEN
	IF (Iproj2.EQ.1) TAC = SUM(RetCatch(1:Nflt,1:Nreg,(Yr2-2):Yr2))/3.d0
	IF ((EstRBC(2)/TAC).GT.(1.d0+MaxChange)) THEN
	 TAC = (1.d0+MaxChange)*TAC
	ELSEIF ((EstRBC(2)/TAC).LT.(1.d0-MaxChange)) THEN
	 TAC = (1.d0-MaxChange)*TAC
	ELSE
     TAC = EstRBC(2)
	ENDIF
	ELSE
	 TAC = EstRBC(2)
	ENDIF

	! Write RBC to file and keep track of decision
	OPEN(UNIT=18,FILE='rbctrack.out',POSITION='APPEND')
	WRITE(18,'(I4,1x,I3,1x,100(F14.7,1x))') Isim2,Iproj2,EstDep,EstRBC(1),EstRBC(2),(EstQuant(II),II=1,7),TAC,(EstQuant(II),II=8,9)
	CLOSE(18)

    ! write RBC 
	OPEN(UNIT=18,FILE='RBC.out')
    WRITE(18,'(A5)') '# RBC'
	WRITE(18,'(2(F14.7,1x))') TAC   !EstRBC(2),EstRBC(1)
	CLOSE(18)

    RETURN

    STOP

	END SUBROUTINE EstMod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! This subroutine does the pseudo-tier 1 assessment

    SUBROUTINE DoTier11(Yr2,Iproj2)

	IMPLICIT NONE
    !INTEGER Yr2,Iproj2,II,Ftype,Iyr,Ilen,Age,Iflt,Sex
    !REAL*8 Muse,Fref,Bref(2),Temp(2),Ftarg,Fuse,GetFtarg,pSelAge(0:100),pCatch(1:10),pRetLen,Mbase
	!modded local variables as now globals so only evl inputs on proj 1. 
	!not implemented in here, just for 51, change here for bookkeeping
	INTEGER Yr2,Iproj2,II,Iyr,Ilen,Age,Iflt,Sex
	REAL*8 Muse,Temp(2),Fuse,GetFtarg,pSelAge(0:100),pCatch(1:10),pRetLen

	EXTERNAL GetFtarg

	INCLUDE 'Sinatra.INC'

	!get specifications
	OPEN(UNIT=13,FILE='pseudorefs.ctl')

	DO II =1,6
	 READ(13,*)
	ENDDO
	READ(13,*) Ftype
	READ(13,*)
	READ(13,*) Muse
	READ(13,*)
	READ(13,*) Fref
	READ(13,*)
	READ(13,*) Bref(1),Bref(2)
	READ(13,*)
	READ(13,*) Mbase
	CLOSE(13)

	IF (Muse.LT.0) THEN
	 II = Yr2+NINT(Muse)+1
     !WRITE(*,*) Yr2,Muse,II,NINT(Muse)
	 IF (II.LT.Fyear) II=Fyear
	 Muse = SUM(M(1,1,1,MaxAge,II:Yr2))/(Yr2-II+1)
	 !WRITE(*,*) M(1,1,1,MaxAge,Yr2),Muse	 
	ENDIF
	EstQuant(4) = Muse
	EstQuant(5) = M(1,1,1,MaxAge,Yr2)
	IF (Mbase.GE.1) THEN
	 II=Yr2-NINT(Mbase)+1
	 IF (II.LT.Fyear) II=Fyear
	 Mbase = SUM(M(1,1,1,MaxAge,II:Yr2))/(Yr2-II+1)
	ENDIF
	IF (Mbase.GT.0) EstQuant(6) = Mbase

	 !get current year's quantities
	 OPEN(UNIT=13,FILE='estB.inp')
	 READ(13,*)
4021 READ(13,*,END=4020,ERR=4020) Iyr,Temp(1),Temp(2)
	 IF (Iyr.EQ.Yr2) THEN
	  EstDep = Temp(1)
	  EstQuant(1) = Temp(2)
	  GOTO 4020
	 ENDIF
	 GOTO 4021
4020 CONTINUE
	 CLOSE(13)

	!WRITE(*,*) Muse
	!WRITE(*,*) Fref
	!WRITE(*,*) Bref(1:2)

	pCatch = 0.d0
	DO Iflt=1,Nflt
     pCatch(Iflt) = SUM(RetCatch(Iflt,1:Nreg,Yr2))
	ENDDO
	pCatch(1:Nflt) = pCatch(1:Nflt)/SUM(pCatch(1:Nflt))
	
	Sex=1
	pSelAge =0.d0
	DO Iflt=1,Nflt
	!WRITE(*,*) pCatch(Iflt)
	DO Age=0,MaxAge
 	 pSelAge(Age) = pSelAge(Age) + SelAge(Iflt,1,Sex,Age,Yr2)*pCatch(Iflt)
	ENDDO
	ENDDO

	!get the FMSY ref point
	Ftarg = GetFtarg(Fref,Muse,Yr2,pSelAge,Mbase)
	!WRITE(*,*) Ftarg
	EstQuant(2) = Ftarg
	
	!get the Fuse
	Fuse =0.d0
	IF (EstDep.GE.Bref(1)) Fuse = Ftarg
    IF (EstDep.LT.Bref(1).AND.EstDep.GT.Bref(2)) Fuse = Ftarg*(EstDep-Bref(2))/(Bref(1)-Bref(2))
	EstQuant(3) = Fuse

	Temp(1) = 0.d0
	!get the RBC
	!DO Ilen=1,Nlen
	! pRetLen = SUM(RetLen(1:Nflt,Ilen,Yr2)*pCatch(1:Nflt))
	! DO Sex=1,2
	! DO Age=0,MaxAge
	! Temp(1) = Temp(1) + pRetLen*WtLen(Ilen,1,Sex)*Fraclen(Ilen,1,Sex,Age,Yr2)*SUM(N(1,1:Nreg,Sex,Age,Yr2))*EXP(-0.5d0*Muse)*(1.d0-EXP(-1.d0*pSelAge(Age)*Fuse))
	! ENDDO
	! ENDDO
	!ENDDO
	EstRBC(2) = Temp(1)
	EstRBC(2) = SUM(RetCatch(1:Nflt,1:Nreg,Yr2))*(1.d0-EXP(-1.d0*Fuse))/(1.d0-EXP(-1.d0*EstQuant(1)))
	EstRBC(1) = EstRBC(2)

	RETURN

	STOP

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! This subroutine does the pseudo-FMSY assessment/HCR

    SUBROUTINE DoTier51(Yr2,Iproj2)

	IMPLICIT NONE
!	INTEGER Yr2,Iproj2,II,Ftype,Iyr,Ilen,Age,Iflt,Sex
!	REAL*8 Muse,Fref,Bref(2),Temp(3),Ftarg,Fuse,GetFtarg,pSelAge(0:100),pCatch(1:10),pRetLen,Mbase
	INTEGER Yr2,Iproj2,II,Iyr,Ilen,Age,Iflt,Sex
	REAL*8 Temp(3),Fuse,GetFtarg,pSelAge(0:100),pCatch(1:10),pRetLen,Muse
	REAL*8 GetFMSY

	EXTERNAL GetFtarg,GetFMSY

	INCLUDE 'Sinatra.INC'

    IF (Iproj2.EQ.1) THEN

	!get specifications
	OPEN(UNIT=13,FILE='icesfmsyrefs.ctl')

	DO II =1,6
	 READ(13,*)
	ENDDO
	READ(13,*) Ftype
	READ(13,*)
	READ(13,*) MuseR
	READ(13,*)
	READ(13,*) Fref
	READ(13,*)
	READ(13,*) Bref(1),Bref(2)
	READ(13,*)
	READ(13,*) Mbase
	CLOSE(13)

    ENDIF

    Muse = MuseR

	IF (Muse.LT.0) THEN
	 II = Yr2+NINT(Muse)+1
     !WRITE(*,*) Yr2,Muse,II,NINT(Muse)
	 IF (II.LT.Fyear) II=Fyear
	 Muse = SUM(M(1,1,1,MaxAge,II:Yr2))/(Yr2-II+1)
	 !WRITE(*,*) M(1,1,1,MaxAge,Yr2),Muse	 
	ENDIF
	EstQuant(4) = Muse
	EstQuant(5) = M(1,1,1,MaxAge,Yr2)
	IF (Mbase.GE.1) THEN
	 II=Yr2-NINT(Mbase)+1
	 IF (II.LT.Fyear) II=Fyear
	 Mbase = SUM(M(1,1,1,MaxAge,II:Yr2))/(Yr2-II+1)
	ENDIF
	IF (Mbase.GT.0) EstQuant(6) = Mbase

	 !get current year's quantities
	 OPEN(UNIT=13,FILE='estB.inp')
	 READ(13,*)
4521 READ(13,*,END=4520,ERR=4520) Iyr,Temp(1),Temp(2)
	 IF (Iyr.EQ.Yr2) THEN
	  EstDep = Temp(1)
	  EstQuant(1) = Temp(2)
	  GOTO 4520
	 ENDIF
	 GOTO 4521
4520 CONTINUE
	 CLOSE(13)

	!WRITE(*,*) Muse
	!WRITE(*,*) Fref
	!WRITE(*,*) Bref(1:2)

	pCatch = 0.d0
	DO Iflt=1,Nflt
     pCatch(Iflt) = SUM(RetCatch(Iflt,1:Nreg,Yr2))
	ENDDO
	pCatch(1:Nflt) = pCatch(1:Nflt)/SUM(pCatch(1:Nflt))
	
	Sex=1
	pSelAge =0.d0
	DO Iflt=1,Nflt
	!WRITE(*,*) pCatch(Iflt)
	DO Age=0,MaxAge
 	 pSelAge(Age) = pSelAge(Age) + SelAge(Iflt,1,Sex,Age,Yr2)*pCatch(Iflt)
	ENDDO
	ENDDO

    IF (Iproj2.EQ.1) THEN

	!get the FMSY ref point
	!Ftarg = GetFtarg(Fref,Muse,Yr2,pSelAge,Mbase)
	Ftarg = Fref
	IF (Ftype.EQ.5) THEN
	  Ftarg = GetFMSY(Muse,Yr2,pSelAge)
	  Bref(1) = BMSY*0.5d0
	  Bref(2) = 0.d0
	ENDIF
	  EstQuant(6) = Bref(1)
	  EstQuant(7) = Bref(2)
	
    ENDIF

	!WRITE(*,*) Ftarg
	EstQuant(2) = Ftarg
	
	!get the Fuse
	Fuse = 0.d0
	IF (EstDep.GE.Bref(1)) Fuse = Ftarg
    IF (EstDep.LT.Bref(1).AND.EstDep.GT.Bref(2)) Fuse = Ftarg*(EstDep-Bref(2))/(Bref(1)-Bref(2))
	EstQuant(3) = Fuse

	Temp(1) = 0.d0
	!get the RBC
    Temp(3) = EstDep/SpawBio(1,0,Yr2)
	DO Ilen=1,Nlen
	 pRetLen = SUM(RetLen(1:Nflt,Ilen,Yr2)*pCatch(1:Nflt))
	 DO Sex=1,2
	  DO Age=0,MaxAge
	   Temp(1) = Temp(1) + pRetLen*WtLen(Ilen,1,Sex)*Fraclen(Ilen,1,Sex,Age,Yr2)*SUM(Temp(3)*N(1,1:Nreg,Sex,Age,Yr2))*EXP(-0.5d0*Muse)*(1.d0-EXP(-1.d0*pSelAge(Age)*Fuse))
	  ENDDO
	 ENDDO
	ENDDO
	EstRBC(2) = Temp(1)
	!EstRBC(2) = SUM(RetCatch(1:Nflt,1:Nreg,Yr2))*(1.d0-EXP(-1.d0*Fuse))/(1.d0-EXP(-1.d0*EstQuant(1)))
	EstRBC(1) = EstRBC(2)


	RETURN

	STOP

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	REAL*8 FUNCTION GetFMSY(Muse,Yr2,pSelAge)

	IMPLICIT NONE

	REAL*8 Fuse,Flo,Fhi,Muse,pSelAge(0:100),pCatch(1:10)
	REAL*8 pSBPR,pSBzero,Temp,testdep,GetSPR,Yield,MaxYield,GetYPR
	REAL*8 a,b,c,d,Ya,Yb,Yc,Yd,phi,GetYield,hh
	INTEGER Age,Sex,Iflt,II,Yr2,useZ

	EXTERNAL GetSPR,GetYPR,GetYield
	INCLUDE 'Sinatra.INC'

	!WRITE(*,*) pSelAge
	!WRITE(*,*) h
	 WRITE(*,*) Muse

	!pSBzero = GetSPR(0.d0,Muse,pSelAge,Yr2)
	!IF (Mbase.GT.0) THEN
	! pSBzero = GetSPR(0.d0,Mbase,pSelAge,Yr2)
	! Temp = GetSPR(0.d0,Muse,pSelAge,Yr2)
	! testdep = (4*h(1)*Temp/pSBzero-(1.d0-h(1)))/(5.d0*h(1)-1.d0)
	! IF (testdep.LE.Fref) THEN
	!  GetFTarg = 0.d0
	!  RETURN
	! ENDIF
	!ENDIF

	!WRITE(*,*) pSBzero

	a = 0.d0
    b = 9.99d0
    hh = b - a
    phi = (1.d0 + (5.d0**0.5d0))/2.d0
	MaxYield = 0.d0

    c = b - (b-a)/phi
    d = a + (b-a)/phi 
    Ya = 0.d0
    Yb = GetYield(b,Muse,pSelAge,Yr2)

   DO II=1,20
    Yc = GetYield(c,Muse,pSelAge,Yr2)
    Yd = GetYield(d,Muse,pSelAge,Yr2)
    !WRITE(*,*) II,a,c,d,b,Ya,Yc,Yd,Yb

    IF (Yc.GT.Yd) THEN
     b = d
     Yb = Yd
     d = c
     Yd = Yc
     c = b - (b-a)/phi
    ELSE
     a = c
     Ya = Yc
     c = d
     Yc = Yd
     d = a + (b-a)/phi
    ENDIF

   ENDDO

   GetFMSY = (c + d)/2.d0
   !WRITE(*,*) GetFMSY

   !STOP

   RETURN

   END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!	II=0
!4522 II=II+1
!     Fuse = 
!     !Fuse = Flo + (Fhi-Flo)/2.d0
!     Fuse = Fb + (Fc-Fb)/3.d0
	 
!     Yield = GetYield(Fuse,Muse,pSelAge,Yr2)
!     IF (Yield.GT.Yb) THEN
!       Fa = Fb
!       Fb = Fuse
!       MaxYield = Yield
!       Ya = Yb
!       Yb = Yield
!       GOTO 4522
!     ENDIF
!     IF (Yield.LT.Yb) THEN
!       Fc = Fuse
!       Yc = Yield
!       GOTO 4522
!     ENDIF     

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


	REAL*8 FUNCTION GetYield(Fuse,Muse,pSelAge,Yr2)

	IMPLICIT NONE

	REAL*8 Fuse,Flo,Fhi,Muse,pSelAge(0:100),pCatch(1:10)
	REAL*8 pSBPR,pSBzero,Temp,testdep,GetSPR,GetYPR
	INTEGER Age,Sex,Iflt,II,Yr2,useZ

	EXTERNAL GetSPR,GetYPR
	
	INCLUDE 'Sinatra.INC'

	 !WRITE(*,*) II,Fuse
	 pSBzero = GetSPR(0.d0,Muse,pSelAge,Yr2)
	 pSBPR = GetSPR(Fuse,Muse,pSelAge,Yr2)
	 !WRITE(*,*) II,Fuse,pSBPR
    
    !testdep = (4*h(1)*pSBPR/pSBzero-(1.d0-h(1)))/(5.d0*h(1)-1.d0)
    !testdep = (4*h(1)*Rzero(1)*pSBPR/pSBzero-SBioZero(1)*(1.d0-h(1)))/(pSBPR*(5.d0*h(1)-1.d0)/pSBzero)
    testdep = ((4*h(1)*Rzero(1)*pSBPR)-(SBioZero(1)*(1.d0-h(1))))/(pSBPR*((5.d0*h(1))-1.d0))
	!testdep = pSBPR/pSBzero
    !dep = (4*h*SBPR/SBPR0-(1-h))/(5*h-1)
    Temp = GetYPR(Fuse,Muse,pSelAge,Yr2)
    GetYield = testdep*Temp
    BMSY = pSBPR*testdep

    !WRITE(*,*) Fuse,GetYield,testdep,Temp,pSBPR,SBioZero(1),pSBPR/pSBzero
	!IF ((Yield-MaxYield).GT.0.001d0) THEN
	! Flo = Fuse
	! MaxYield = Yield
	! GOTO 4522
	!ENDIF
	!IF ((Yield-MaxYield).LT.-0.001d0) THEN
	! Fhi = Fuse
	! GOTO 4522
	!ENDIF
	
    !GetFMSY = Fuse

    !WRITE(*,*) Yr2,Yield,MaxYield,GetFMSY

    !STOP

	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	REAL*8 FUNCTION GetFtarg(Fval,Muse,Yr2,pSelAge)

	IMPLICIT NONE

	REAL*8 Fval,Fuse,Flo,Fhi,Muse,pSelAge(0:100),pCatch(1:10)
	REAL*8 pSBPR,pSBzero,Temp,testdep,GetSPR
	INTEGER Age,Sex,Iflt,II,Yr2,useZ

	EXTERNAL GetSPR
	INCLUDE 'Sinatra.INC'

	!WRITE(*,*) pSelAge

	pSBzero = GetSPR(0.d0,Muse,pSelAge,Yr2)
	IF (Mbase.GT.0) THEN
	 pSBzero = GetSPR(0.d0,Mbase,pSelAge,Yr2)
	 Temp = GetSPR(0.d0,Muse,pSelAge,Yr2)
	 testdep = (4*h(1)*Temp/pSBzero-(1.d0-h(1)))/(5.d0*h(1)-1.d0)
	 IF (testdep.LE.Fref) THEN
	  GetFTarg = 0.d0
	  RETURN
	 ENDIF
	ENDIF

	!WRITE(*,*) pSBzero

	Fhi = 5.99d0
	Flo = 0.d0

	II=0
4022 Fuse = Flo + (Fhi-Flo)/2.d0
	 II=II+1
	 !WRITE(*,*) II,Fuse
	 pSBPR = GetSPR(Fuse,Muse,pSelAge,Yr2)
	 !WRITE(*,*) II,Fuse,pSBPR

    testdep = (4*h(1)*pSBPR/pSBzero-(1.d0-h(1)))/(5.d0*h(1)-1.d0)
	!testdep = pSBPR/pSBzero
	!WRITE(*,*) II,testdep
!dep = (4*h*SBPR/SBPR0-(1-h))/(5*h-1)
	IF ((testdep-Fval).GT.0.001d0) THEN
	 Flo = Fuse
	 GOTO 4022
	ENDIF
	IF ((testdep-Fval).LT.-0.001d0) THEN
	 Fhi = Fuse
	 GOTO 4022
	ENDIF
	
    GetFTarg = Fuse


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   REAL*8 FUNCTION GetYPR(Fuse,Muse,pSelAge,Yr2)

   REAL*8 Muse,pSelAge(0:100),Fuse,Ntemp(0:100),Zuse(0:100),Buse(0:100)

   INTEGER Age,Yr2

   INCLUDE 'Sinatra.INC'

   Ntemp(0) = 1.d0
   DO Age=1,MaxAge
    Ntemp(Age) = Ntemp(Age-1)*EXP(-1.d0*(Muse+pSelAge(Age-1)*Fuse))
   ENDDO
   Ntemp(MaxAge) = Ntemp(MaxAge)/(1.d0-EXP((-1.d0*(Muse+pSelAge(MaxAge)*Fuse))))

!   GetYPR = 0.d0
!   DO Age=1,MaxAge
!    GetYPR = GetYPR + Fuse*Ntemp(Age)*Weight(1,1,Age,Yr2)*(1.d0-EXP(-1.d0*(Muse+pSelAge(Age)*Fuse)))/(Muse+pSelAge(Age)*Fuse)
!   ENDDO


   Zuse(0:MaxAge) = Muse + Fuse*pSelAge(0:MaxAge)
   Buse(0:MaxAge) = Ntemp(0:MaxAge)*Weight(1,1,0:MaxAge,Yr2)
   GetYPR = SUM(Fuse*pSelAge(0:MaxAge)*Buse(0:MaxAge)*(1.d0/Zuse(0:MaxAge))*(1.d0-EXP(-1.d0*Zuse(0:MaxAge))))
  
   !Yield = Fuse*elem_prod(elem_prod(elem_prod(1/Z,Sel),Bio),(1-mfexp(-1.*Z)));
   !GetYPR = SUM(Ntemp(0:MaxAge)*Weight(1,1,0:MaxAge,Yr2))

   RETURN

   END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   REAL*8 FUNCTION GetSPR(Fuse,Muse,pSelAge,Yr2)

   REAL*8 Muse,pSelAge(0:100),Fuse,Ntemp(0:100)

   INTEGER Age,Yr2

   INCLUDE 'Sinatra.INC'


   Ntemp(0) = 0.5d0
   DO Age=1,MaxAge
    Ntemp(Age) = Ntemp(Age-1)*EXP(-1.d0*(Muse+pSelAge(Age-1)*Fuse))
   ENDDO
   Ntemp(MaxAge) = Ntemp(MaxAge)/(1.d0-EXP((-1.d0*(Muse+pSelAge(MaxAge)*Fuse))))

   GetSPR = SUM(Ntemp(0:MaxAge)*Fecundity(1,0:MaxAge,Yr2))


   RETURN

   END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! This subroutine does the tier 1 assessment, calls ss3, and does the harvest strategy
!!!!!! If Iflag = 1 then do CCAMLR harvest strategy via projrec, else do normal tier 1

    SUBROUTINE DoTier1(Iflag,Isim2,Iproj2,Yr2)

	IMPLICIT NONE
	INTEGER Iflag,JJ,Isim2,Iproj2,II,KK,Itemp(100),Ireg,NII,Iflt,Yr2,Iyr,Irow,fcol,lcol,limflag,Nlim,Npass,Nrecyrs,Jyr
	CHARACTER*2000 STRING1
	CHARACTER DUM3
	CHARACTER*15 DUM9
	CHARACTER*11 DUM2,DUM4
	CHARACTER*8 DUM5,DUM6
	CHARACTER*3 DUM8
	CHARACTER*5 DUM10
	CHARACTER*20 STRING2,STRING3
	REAL*8 DUM(10),Temp,TempVec(1000),LastVec(1000),TempCat(1000),Plim,LastCat,Hicat,locat,perfvec(1000),LastOne
	CHARACTER TempString


	INCLUDE 'Sinatra.INC'

	!Call SS3
	CALL SYSTEM('del report.sso')
    CALL SYSTEM('del ss3.std')
	CALL SYSTEM('ss3.exe -nox -iprint 1000')    !changed 2 Aug 2011 to always obtain .cor file
    !IF (Iflag.EQ.1)	CALL SYSTEM('ss3.exe -nox -iprint 1000')
	!IF (Iflag.EQ.0) CALL SYSTEM('ss3.exe -nohess -nox -iprint 1000')
	!CALL SYSTEM('ss3.exe -nohess -nox -iprint 1000')

    OPEN(UNIT=15,FILE='ss3.std')
	READ(15,'(A2000)',ERR=4013,END=4013) STRING1
	CLOSE(15)

	!save files (forecast report for now, appended to foereport.out)
	!OPEN(UNIT=18,FILE='forereport.out',POSITION='APPEND')
	!WRITE(18,'(A10,1x,I5,1x,I3)') '#fore dump',Isim2,Iproj2
	!OPEN(UNIT=15,FILE='forecast-report.sso')
	!READ(15,*,ERR=4011,END=4011)
	!READ(15,*,ERR=4011,END=4011)
	!READ(15,*,ERR=4011,END=4011)
	! DO 4010 JJ=1,200000
	!  READ(15,'(A2000)',ERR=4011,END=4011) STRING1
	!  WRITE(18,'(A2000)') STRING1
!4010 CONTINUE	
!4011 CLOSE(15)
!	CLOSE(18)
	OPEN(UNIT=18,FILE='ss3par.out',POSITION='APPEND')
	WRITE(18,'(A10,1x,I5,1x,I3)') '#par dump',Isim2,Iproj2
	OPEN(UNIT=15,FILE='ss3.par')
	 DO 4015 JJ=1,200000
	  READ(15,'(A2000)',ERR=4016,END=4016) STRING1
	  WRITE(18,'(A2000)') STRING1
4015 CONTINUE	
4016 CLOSE(15)
	CLOSE(18)
  
	!IF (Iflag.EQ.0) THEN

	!Get tier 1 estimate
	EstRBC(2) = 0.d0
    OPEN(UNIT=15,FILE='Report.sso')
	Irow=0
	DO 4012 JJ=1,200000
	 READ(15,'(A2000)') STRING1
	 READ(STRING1,'(A11)') DUM2
	 Irow=Irow+1
	 READ(STRING1,'(A8)') DUM5
	 READ(STRING1,'(A3,A15)') DUM8,DUM9
	 READ(STRING1,'(A3,A5)') DUM8,DUM10
	 !WRITE(99,*) DUM10
	 IF (DUM9.EQ.'RecrDist_Area_1') THEN
	  READ(STRING1,*) DUM3,DUM3,EstQuant(3)
	 ENDIF
	 IF (DUM9.EQ.'MoveParm_A_seas') THEN
	  READ(STRING1,*) DUM3,DUM3,EstQuant(4)
	  READ(15,*)
      READ(15,'(A2000)') STRING1
	  READ(STRING1,*) DUM3,DUM3,EstQuant(5)
	 ENDIF
	 IF (DUM10.EQ.'SR_R0') THEN
	  READ(STRING1,*) DUM3,DUM3,EstQuant(6)
	 ENDIF
	 IF(DUM2.EQ.' SPB_Virgin') THEN
	  READ(STRING1,*) DUM3,EstQuant(1)
	  !WRITE(*,*) DUM3,EstQuant(1)
	 ENDIF
	 IF(DUM5.EQ.' Bratio_') THEN
	  READ(STRING1,'(A8,I4)') DUM6,Jyr
	  IF (Jyr.EQ.(Yr2+1)) READ(STRING1,*) DUM3,EstDep,EstQuant(7)
	 ENDIF
	 IF(DUM2.EQ.' TotYield_B') THEN
	  READ(STRING1,*) DUM3,EstQuant(2)
	  !WRITE(*,*) DUM3,EstQuant(2)
	 ENDIF
	 !WRITE(99,*) Irow,DUM2,Jyr
     IF (DUM2.EQ.' ForeCatch_') THEN
	  READ(STRING1,'(A11,I4)') DUM4,Jyr
	  IF (Iflag.EQ.0.AND.Jyr.EQ.(Yr2+1)) THEN
	   READ(STRING1,*) DUM3,Temp
	   GOTO 4014
	  ENDIF
	  IF (Iflag.EQ.1.AND.Jyr.EQ.(Yr2+Nprojyrs)) THEN
	   READ(STRING1,*) DUM3,Temp
	   GOTO 4014
	  ENDIF
	 ENDIF 	   
4012 CONTINUE
4014 CLOSE(15)
     EstRBC(2) = Temp
	 IF (Iflag.EQ.0) WRITE(99,*) 'estrbc',EstRBC(2)

	!ENDIF

	IF (Iflag.EQ.1) THEN

  	 CALL SYSTEM('copy forecast.ss orig_forecast.ss')
	 

	 OPEN(UNIT=18,FILE='projrec.dat')
	 WRITE(18,*) '#Nsim'
	 WRITE(18,*) 100
	 WRITE(18,*) '#flag'
	 WRITE(18,*) 0
	 WRITE(18,*) '#Nyears rect not estimated'
	 OPEN(UNIT=15,FILE='tooth_ss3_tier1.ctl')
	 DO Iflt=1,5
	  READ(15,*) 
	 ENDDO
	 READ(15,*) Nrecyrs
	 CLOSE(15)
	 WRITE(18,*) Nrecyrs
     WRITE(18,*) '#Target'
	 WRITE(18,*) 0.5
	 WRITE(18,*) '#Years'
	 WRITE(18,'(I4,1x,I4)') (Yr2+2),(Yr2+Nprojyrs)
	 WRITE(18,*) '#Limit'
	 WRITE(18,*) 0.2
	 CLOSE(18)

	 CALL SYSTEM('projrec4')

	 HiCat = 5000.d0  !MAX(10000.d0,5*EstRBC(2))
	 LoCat = 0.d0
	 !Temp = EstRBC(2)  !500.d0
	 Temp = 400.d0
	 IF (Iproj2.GT.1) Temp = RBC
	 LastCat = 0.d0
	 Npass = 0
	 
4018 TempCat = 0.d0
	 Npass = Npass + 1
	 IF (Npass.GT.10) GOTO 4019
 	 CALL SYSTEM('del forecast.ss')
	 OPEN(UNIT=18,FILE='forecast.ss')
 	 WRITE(18,'(A1)') '#'
	 WRITE(18,'(I2)') 3
 	 WRITE(18,'(A1)') '#'
 	 WRITE(18,'(A1)') '#'
 	 WRITE(18,'(A1)') '#'	 
	 WRITE(18,'(I4)') Yr2-2
	 WRITE(18,'(I4)') Yr2
     WRITE(18,'(I2)') 1
	 WRITE(18,'(I2)') 2
	 WRITE(18,'(F3.1)') 0.5
	 WRITE(18,'(F3.1)') 0.5
	 WRITE(18,'(I4)') Nprojyrs
     WRITE(18,'(I2)') 0
 	 DO Iflt=1,14
	  WRITE(18,'(A1)') '#'
	 ENDDO
     WRITE(18,'(I2)') 1
	 WRITE(18,'(A10)') '#0 0 1 1 1'
	 WRITE(18,'(A1)') '#'
 	 WRITE(18,'(A1)') '#'
	 WRITE(18,'(I4)') 3*(Nprojyrs-1)
     WRITE(18,'(I2)') 2
	 WRITE(18,'(A1)') '#'
	 IF (Forecat.EQ.1) THEN
	  IF (Temp.GT.MaxCatch(3)) THEN
	   TempCat(3) = MaxCatch(3)
	   TempCat(4:5) = (Temp-MaxCatch(3))/2.d0
	  ELSE
	   TempCat(3) = Temp
	   TempCat(4:5) = 0.001d0
	  ENDIF
	  DO Iflt=1,Nflt
	   IF (TempCat(Iflt).GT.0) THEN
	    DO Iyr=Yr2+1,Yr2+Nprojyrs-1
	     WRITE(18,'(I4,1x,I2,1x,I2,1x,F14.7)') Iyr,1,Iflt,TempCat(Iflt)
	    ENDDO
	   ENDIF
	  ENDDO
     ENDIF 
	 IF (Forecat.EQ.3) THEN
	  IF (Temp.GT.MaxCatch(3)) THEN
	   TempCat(3) = MaxCatch(3)
	   TempCat(5) = 0.7d0*(Temp-MaxCatch(3))
	   TempCat(4) = 0.3d0*(Temp-MaxCatch(3))
	  ELSE
	   TempCat(3) = Temp
	   TempCat(4:5) = 0.001d0
	  ENDIF
	  DO Iflt=1,Nflt
	   IF (TempCat(Iflt).GT.0) THEN
	    DO Iyr=Yr2+1,Yr2+Nprojyrs-1
	     WRITE(18,'(I4,1x,I2,1x,I2,1x,F14.7)') Iyr,1,Iflt,TempCat(Iflt)
	    ENDDO
	   ENDIF
	  ENDDO
     ENDIF
	 IF (Forecat.EQ.2) THEN
      IF (Temp.GT.MaxCatch(3)) THEN
	   TempCat(3) = MaxCatch(3)
       DO Iyr=Yr2+1,Yr2+Nprojyrs-1
	    IF (MOD(Iyr,2).EQ.0) THEN
		 TempCat(4) = Temp-MaxCatch(3)
		 TempCat(5) = 0.001d0
        ELSE
		 TempCat(5) = Temp-MaxCatch(3)
		 TempCat(4) = 0.001d0
		ENDIF
		DO Iflt=1,Nflt
		 IF (TempCat(Iflt).GT.0) WRITE(18,'(I4,1x,I2,1x,I2,1x,F14.7)') Iyr,1,Iflt,TempCat(Iflt)
	    ENDDO
	   ENDDO
	  ELSE
	   TempCat(3) = Temp
	   TempCat(4:5) = 0.0000001d0
	   DO Iflt=1,Nflt
	    IF (TempCat(Iflt).GT.0) THEN
	     DO Iyr=Yr2+1,Yr2+Nprojyrs-1
	      WRITE(18,'(I4,1x,I2,1x,I2,1x,F14.7)') Iyr,1,Iflt,TempCat(Iflt)
	     ENDDO
	    ENDIF
	   ENDDO
	  ENDIF
     ENDIF     
	WRITE(18,*) 999
	CLOSE(18)

	!change projrec.dat
	 OPEN(UNIT=18,FILE='projrec.dat')
	 WRITE(18,*) '#Nsim'
	 IF (Npass.LE.4) WRITE(18,*) 40
	 IF (Npass.GT.4) WRITE(18,*) 100
	 !WRITE(18,*) 1
	 WRITE(18,*) '#flag'
	 WRITE(18,*) 1
	 WRITE(18,*) '#Nyears rect not estimated'
	 WRITE(18,*) Nrecyrs
     WRITE(18,*) '#Target'
	 WRITE(18,*) 0.5
	 WRITE(18,*) '#Years'
	 WRITE(18,'(I4,1x,I4)') (Yr2+2),(Yr2+Nprojyrs)
	 WRITE(18,*) '#Limit'
	 WRITE(18,*) 0.2
	 CLOSE(18)

	 CALL SYSTEM('projrec4')


	OPEN(UNIT=15,FILE='projrec4.out')
	READ(15,*)
	READ(15,*) PerfVec(1)
	READ(15,*)
	READ(15,*) PerfVec(2)
	CLOSE(15)

	WRITE(99,'(100(F14.7,1x))') Npass,(TempCat(JJ),JJ=1,Nflt)
	WRITE(99,*) 'NL',PerfVec(2)
	WRITE(99,*) 'PV',PerfVec(1)
	IF (PerfVec(2).GT.0.1d0) THEN
	 HiCat = Temp
	 Temp = LoCat + (HiCat-LoCat)/2.d0
	 GOTO 4018
	ENDIF
	IF (PerfVec(1).LT.0.5d0) THEN
	 HiCat = Temp
	 Temp = LoCat + (HiCat-LoCat)/2.d0
	 GOTO 4018
	ENDIF
	IF (ABS(LastCat-Temp).LT.1.5d0.OR.Npass.GT.10.OR.PerfVec(1).LT.0.51d0) GOTO 4019
	LastCat = Temp
	LoCat = Temp
	Temp = LoCat + (HiCat-LoCat)/2.d0	
	GOTO 4018
4019 CONTINUE
	EstRBC(2) = Temp
    WRITE(99,*) 'estrbc',EstRBC(2)



	CALL SYSTEM('del forecast.ss')
  	CALL SYSTEM('copy orig_forecast.ss forecast.ss')


	ENDIF

	EstRBC(1) = EstRBC(2)

	WRITE(99,*) Yr2,EstDep,EstRBC(2),EstQuant(2),EstQuant(7)

	RETURN

4013 CLOSE(15)
	 EstQuant(7) = 0.d0
	 EstQuant(9) = 0.d0
	 IF (Iproj2.EQ.1) THEN
	  TAC = SUM(RetCatch(1:Nflt,1:Nreg,(Yr2-2):Yr2))
	  EstQuant(2) = TAC
	  EstRBC(2) = TAC
	  EstRBC(1) = TAC+1.d0
	 ENDIF
	 EstRBC(1:2) = TAC*(EstRBC(1)/(0.001d0+EstRBC(2)))

	 WRITE(99,*) Yr2,EstDep,EstRBC(2),EstQuant(2),EstQuant(7)

	RETURN

	STOP

	END SUBROUTINE DoTier1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SUBROUTINE DoTier3(Yr2,Iproj3)

	IMPLICIT NONE

	INCLUDE 'Sinatra.INC'
	INTEGER Yr2,Iproj3,IDUM,II,Age
	REAL*8 DUM,Temp,TempSD,Muse,TempCat,Ftemp,Fuse,pSelAge(0:100),GetYPR
	CHARACTER DUMCHAR
	DOUBLE PRECISION PCUM,ZSCORE
	DOUBLE PRECISION dinvnr

	EXTERNAL GetYPR
	EXTERNAL dinvnr

	CALL SYSTEM('del punt.std')
	CALL SYSTEM('del punt.rep')
	CALL SYSTEM('punt.exe -nox')
!	CALL SYSTEM('punt.exe')


	OPEN(UNIT=15,FILE='punt.std')
	READ(15,*,ERR=4043,END=4043)
    READ(15,*)
	READ(15,*)
	READ(15,*)
	READ(15,*) IDUM,DUMCHAR,DUM,EstQuant(7)
    READ(15,*)
	READ(15,*)
	READ(15,*) IDUM,DUMCHAR,EstQuant(8),EstQuant(9)
	READ(15,*) IDUM,DUMCHAR,DUM,TempSD
	CLOSE(15) 				

	PCUM = Tier3R(4)

!	OPEN(UNIT=15,FILE='punt.rep')
!	DO II=1,25
!	 READ(15,*,ERR=4043,END=4043)
!	ENDDO
 !   READ(15,*) Temp
!	IF (Temp.GT.999999.d0) GOTO 4043
!	CLOSE(15)
	
	OPEN(UNIT=15,FILE='punt.rep')		
	READ(15,*)
	READ(15,*) EstDep
	DO II=1,13
	 READ(15,*)
	ENDDO
	DO II=1,3
	 READ(15,*) EstQuant(II)
	 READ(15,*)
	ENDDO
	READ(15,*)
	READ(15,*)
	READ(15,*) EstQuant(4)
	READ(15,*)
	READ(15,*) Temp
    IF (Temp.LT.999999.d0) THEN
	 EstRBC(2)=Temp
	ELSE
	 GOTO 4043
	ENDIF
	READ(15,*)
	READ(15,*) Temp
	READ(15,*)
	!IF (Tier3R(4).NE.0.5d0) 
	READ(15,*) (pSelAge(Age),Age=0,MaxAge)
	CLOSE(15)

	IF (Tier3R(4).NE.0.5d0) THEN
     ZSCORE = dinvnr(PCUM,1.d0-PCUM)
     Ftemp = EstDep + ZSCORE*EstQuant(7)
	 !get the Fuse
  	 Fuse =0.d0
	 IF (Ftemp.LE.EstQuant(2)) Fuse = EstQuant(1)
     IF (Ftemp.GT.EstQuant(2).AND.Ftemp.LT.EstQuant(3)) Fuse = EstQuant(1)*(Ftemp-EstQuant(3))/(EstQuant(2)-EstQuant(3))
     OPEN(UNIT=13,FILE='punt.dat')
	 READ(13,*)
     READ(13,*)
     READ(13,*)
     READ(13,*) Muse
	 DO II=1,7
     READ(13,*)
	 ENDDO
	 READ(13,*) TempCat
	 CLOSE(13)
	 EstQuant(4) = Fuse
	 !get the rbc
	 EstRBC(2) = TempCat*GetYPR(Fuse,Muse,pSelAge,Yr2)/GetYPR(Ftemp,Muse,pSelAge,Yr2)
	 IF (Fuse.EQ.0.d0) EstRBC(2) = 0.d0
	 !WRITE(*,*) EstDep,EstQuant(7),Ftemp,(EstQuant(II),II=1,4)
	 !WRITE(*,*) TempCat,Fuse,GetYPR(Fuse,Muse,pSelAge,Yr2),Ftemp,GetYPR(Ftemp,Muse,pSelAge,Yr2)
	ENDIF

	EstRBC(1) = EstRBC(2)
	IF (Temp.LT.999999.d0.AND.Temp.GT.0) EstQuant(5) = Temp

	RETURN

4043 CLOSE(15)
	 EstQuant(7) = 0.d0
	 EstQuant(9) = 0.d0
	 IF (Iproj3.EQ.1) THEN
	  TAC = SUM(RetCatch(1:Nflt,1:Nreg,(Yr2-2):Yr2))
	  EstQuant(5) = TAC
	  EstRBC(2) = TAC
	  EstRBC(1) = TAC+1.d0
	 ENDIF
	 EstRBC(1:2) = TAC*(EstRBC(1)/(0.001d0+EstRBC(2)))
	 RETURN	

	STOP

	END SUBROUTINE DoTier3


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SUBROUTINE DoTier4(Yr2,Iproj3)

	IMPLICIT NONE

	INCLUDE 'Sinatra.INC'
	INTEGER Yr2,Iflt,RefY1,RefY2,Ireg,Jflt,Jreg,Nref,Nyrs,Iyr,II,Iproj3,JJ
	REAL*8 LimCPUE,Temp,Ccurr,Ctarg(5),TempSD,TempVec(100)
	REAL*8 XNORM
	DOUBLE PRECISION PCUM,ZSCORE
	DOUBLE PRECISION dinvnr


	EXTERNAL XNORM,dinvnr

!	WRITE(*,*) 'T4'


	OPEN(UNIT=14,FILE='Tier4.ctl')
	DO II=1,4
	 READ(14,*) 
	ENDDO
	READ(14,*) Jflt
	READ(14,*)
	READ(14,*) Jreg
	READ(14,*)
	READ(14,*) RefY1,RefY2
	READ(14,*)
	READ(14,*) (Ctarg(II),II=4,5)
	READ(14,*)
	READ(14,*) Nyrs
	READ(14,*)
	READ(14,*) (Ctarg(II),II=1,3)
	READ(14,*)
	READ(14,*) LimCPUE
	READ(14,*)
	READ(14,*) PCUM
	CLOSE(14)
    !WRITE(*,*) 'Nyrs',Nyrs
    !WRITE(*,*) 'Cstar',Cstar

	 Nref = 0
	 IF (Ctarg(1).EQ.-1.d0) Cref = 0.d0
	 Ccurr = 0.d0
	 JJ=0
	 OPEN(UNIT=14,FILE='CPUE.inp')
	 READ(14,*)
4041 READ(14,*,END=4040,ERR=4040) Iyr,Ireg,Iflt,Temp
	 IF(Iflt.EQ.Jflt.AND.Ireg.EQ.Jreg.AND.Iyr.GE.RefY1.AND.Iyr.LE.RefY2) THEN
	 Nref = Nref + 1
	 IF (Ctarg(1).EQ.-1.d0) Cref = Cref + Temp
	 ENDIF
!	 WRITE(*,*) Iyr,Iflt,Ireg,Temp,Jflt,Jreg  Nyrs,Yr2
!     WRITE(*,*) Iflt,Jflt,Ireg,Jreg,Iyr,(Yr2-Nyrs+1)
	 IF (Iflt.EQ.Jflt.AND.Ireg.EQ.Jreg.AND.Iyr.GE.(Yr2-Nyrs+1)) THEN
!	  WRITE(*,*) Iyr,Ccurr,Temp
	  JJ = JJ+1
	  Ccurr = Ccurr + Temp
	  TempVec(JJ) = Temp
	 ENDIF
	 GOTO 4041
4040 CONTINUE
     CLOSE(14)
!	 WRITE(*,*) JJ, Ccurr
!	 STOP
	  !WRITE(*,*) Ctarg

	 IF (Iproj3.EQ.1) Cstar = Ctarg(4)

     IF (Ctarg(1).EQ.-1.d0) Cref = Cref / Nref
	 IF (Ctarg(1).EQ.1) Cref = Ctarg(2)
	 IF (Ctarg(1).GE.2.AND.Iproj3.EQ.1) THEN
	  TempSD = SQRT(LOG(1.d0+(Ctarg(3)/Ctarg(2))**2.d0))
!	  WRITE(*,*) TempSD,Ctarg(2)
	  Cref=Ctarg(2)*EXP(XNORM(5,0.d0,TempSD,ISEEDZ)-0.5d0*TempSD*TempSD)      
	  IF (Cref.LT.0.d0) Cref = 0.000001d0
	  TempSD = SQRT(LOG(1.d0+(Ctarg(5)/Ctarg(4))**2.d0))
!	  WRITE(*,*) TempSD,Ctarg(4)
	  Cstar=Ctarg(4)*EXP(XNORM(5,0.d0,TempSD,ISEEDZ)-0.5d0*TempSD*TempSD)      
	  IF (Cstar.LT.0.d0) Cstar = 0.000001d0
	 ENDIF

	 !WRITE(*,*) Cref

	 IF (Ctarg(1).EQ.3) THEN
	  EstRBC(1:2) = Cstar
	  EstQuant(3) = Cstar
	  RETURN
	 ENDIF

	 Ccurr = Ccurr / Nyrs
     LimCPUE = LimCPUE*Cref

	 EstQuant(1) = Cref
	 EstQuant(2) = Ccurr
	 EstQuant(3) = Cstar
	 EstQuant(4) = LimCPUE
	 IF (JJ.LT.2) JJ = 2
	 EstQuant(7) = SQRT(SUM((TempVec(1:JJ)-Ccurr)**2.d0)/(JJ-1))

	 TempSD = EstQuant(7)/SQRT(FLOAT(Nyrs))
	 IF (TempSD.GT.99.d0) TempSD = 99.d0
     ZSCORE = dinvnr(PCUM,1.d0-PCUM)
     Temp = Ccurr
	 Ccurr = Ccurr + ZSCORE*TempSD
 
	 EstRBC(1) = Cstar*(Ccurr-LimCPUE)/(Cref-LimCPUE)
	 IF (EstRBC(1).LT.0.d0) EstRBC(1) = 0.d0
	 EstRBC(2) = EstRBC(1)

	 EstDep = (Ccurr-LimCPUE)/(Cref-LimCPUE)

!     WRITE(*,*) Temp,Ccurr,ZSCORE,TEmpSD
!	 (Temp-LimCPUE)/(Cref-LimCPUE),EstDep,Cstar*(Temp-LimCPUE)/(Cref-LimCPUE),EstRBC(1)


	 !WRITE(*,*) Cref
	 !WRITE(*,*) Ccurr
	 !WRITE(*,*) LimCPUE
	 !WRITE(*,*) Cstar
	 !WRITE(*,*) EstRBC(2)


	RETURN

	STOP

	END SUBROUTINE DoTier4


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE GetHCRspecs

	IMPLICIT NONE

	INCLUDE 'Sinatra.INC'
	INTEGER II
    
    OPEN(UNIT=13,FILE='hcr.ctl')

    DO II=1,9
	 READ(13,*)
	ENDDO
	READ(13,*) HCRspecs(1)
	READ(13,*)
	READ(13,*) HCRspecs(2)
	READ(13,*)
	READ(13,*) HCRspecs(3)
    READ(13,*)
	READ(13,*) HCRspecs(4)
    DO II=1,7
	 READ(13,*) 
	ENDDO
    READ(13,*) Discountfactor
	READ(13,*)
	READ(13,*) DiscountStableYrs
	READ(13,*)
	READ(13,*) DiscountStableCV
	READ(13,*)
	READ(13,*) MaxChange
	CLOSE(13)


    RETURN

    STOP

	END SUBROUTINE GetHCRspecs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	!PROGRAM TestSinatra
	SUBROUTINE TestSinatra

	IMPLICIT NONE

	INCLUDE 'Sinatra.INC'

!	LOCAL VARIABLES
	INTEGER ISEED1,ISEED2,Yr1,Yr2,Isim !,Nsim
	CHARACTER*16 STRING1
	REAL*8 Numbers(Nstock,Nzone,2,TopAge+1),Mort(Nstock,Nzone,2,TopAge+1),Mulen(Nstock,2,TopAge+1)
	REAL*8 Siglen(Nstock,2,TopAge+1),Wt(Nstock,2,TopAge+1),Fec(Nstock,TopAge+1),Qdevs(Nfleet,Nzone)
	REAL*8 Move(Nstock,Nzone,Nzone,2,TopAge+1),Sel(Nfleet,NlenBin),Ret(Nfleet,Nlenbin),S0(Nstock)

!	****MOVE THESE TO A READFILE AT SOME POINT!!!!!
        ISEEDX = -87450
        ISEEDZ = -75575

!   OPEN MASTER CTL FILE
    OPEN(UNIT=13,FILE='master.ctl')
	READ(13,*)
	READ(13,*) Nsim
    READ(13,*)
	READ(13,*) ISEEDX
	READ(13,*) ISEEDZ
	CLOSE(13)


!	OPEN GENERAL JUNKFILE 
	OPEN(UNIT=99,FILE='Control.junk')

	WRITE(99,*) 'Stepping into ReadIn'

!	READ IN CONTROL VARIABLES AND SET-UP
	CALL ReadIn(1)
	WRITE(99,*) 'ReadIN finished'

!   LOOP OVER SIMUALTIONS
    DO 4000 Isim=1,Nsim

!	DO HISTORICAL PROJECTION
	WRITE(99,*) 'Starting Historical Projection'
	CALL HistProj()
	WRITE(99,*) 'Historical Projection finished'

!	GET HISTORICAL DATA
	CALL FindHistData()

!	ASSIGN RELEVANT QUANTITIES TO RETURN VALUES
	IF (Diag.EQ.1) THEN
	 OPEN(UNIT=98,FILE='Assignment.junk')
	 WRITE(98,*) 'assignments'
	 CLOSE(98)
	ENDIF
!	Numbers(1:Nstk,1:Nreg,1:2,1:MaxAge+1) = N(1:Nstk,1:Nreg,1:2,0:MaxAge,Lyear+1)
!	Mort(1:Nstk,1:Nreg,1:2,1:MaxAge+1) = M(1:Nstk,1:Nreg,1:2,0:MaxAge,Lyear)
!    Mulen(1:Nstk,1:2,1:MaxAge+1) = MeanLenAge(1:Nstk,1:2,0:MaxAge,Lyear+1)
!	Siglen(1:Nstk,1:2,1:MaxAge+1) = SigmaLenAge(1:Nstk,1:2,0:MaxAge,Lyear+1)
!	Wt(1:Nstk,1:2,1:MaxAge+1) = Weight(1:Nstk,1:2,0:MaxAge,Lyear+1)
!	Fec(1:Nstk,1:MaxAge+1) = Fecundity(1:Nstk,0:MaxAge,Lyear+1)
!	Move(1:Nstk,1:Nreg,1:Nreg,1:2,1:MaxAge+1) = X(1:Nstk,1:Nreg,1:Nreg,1:2,0:MaxAge,Lyear)
!	Sel(1:Nflt,1:Nlen) = SelLen(1:Nflt,1:Nlen,Lyear)
!	Ret(1:Nflt,1:Nlen) = RetLen(1:Nflt,1:Nlen,Lyear)
!	Qdevs(1:Nflt,1:Nreg) = CpueQdevs(1:Nflt,1:Nreg,Lyear)
!	S0(1:Nstk) = SBioZero

	CLOSE(99)


	CALL SYSTEM('ss3.exe -nohess -nox -iprint 1000')
!	CALL SYSTEM('ss3.exe -nox -iprint 1000')

	
    WRITE(STRING1,'(A13,I3)')    '      mkdir ',Isim
	WRITE(*,*) STRING1
    CALL SYSTEM(STRING1)
	WRITE(STRING1,'(A12,I3,A1)') ' copy ss3.* ',Isim,'\'
    CALL SYSTEM(STRING1)    
	WRITE(STRING1,'(A12,I3,A1)') ' copy *.sso ',Isim,'\'
    CALL SYSTEM(STRING1)    
 	WRITE(STRING1,'(A12,I3,A1)') '  copy *.ss ',Isim,'\'
    CALL SYSTEM(STRING1)    
    WRITE(STRING1,'(A12,I3,A1)') ' copy *.inp ',Isim,'\'
    CALL SYSTEM(STRING1)    
    WRITE(STRING1,'(A12,I3,A1)') 'copy *.junk ',Isim,'\'
    CALL SYSTEM(STRING1)    

4000 CONTINUE


!	Yr1=Lyear+1
!	Yr2=Lyear+1
!	CALL TestSinatranew(Yr1,Yr2,Numbers,Mort,Mulen,Siglen,Wt,Fec,Move,Sel,Ret,S0,ISEEDX,ISEEDZ)


	STOP

	!END PROGRAM TestSinatra
	END SUBROUTINE TestSinatra

!	CALL SYSTEM('gavss2.exe -nohess')
        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SUBROUTINE sinatra(Numbers,Mort,Mulen,Siglen,Wt,Fec,Move,Sel,Ret,S0,ISEED1,ISEED2)
!DEC$ ATTRIBUTES DLLEXPORT::sinatra
!DEC$ ATTRIBUTES C,REFERENCE,ALIAS:'sinatra_' :: SINATRA

	IMPLICIT NONE

	INCLUDE 'Sinatra.INC'

!	LOCAL VARIABLES
	INTEGER ISEED1,ISEED2
	REAL*8 Numbers(Nstock,Nzone,2,TopAge+1),Mort(Nstock,Nzone,2,TopAge+1),Mulen(Nstock,2,TopAge+1)
	REAL*8 Siglen(Nstock,2,TopAge+1),Wt(Nstock,2,TopAge+1),Fec(Nstock,TopAge+1),Qdevs(Nfleet,Nzone)
	REAL*8 Move(Nstock,Nzone,Nzone,2,TopAge+1),Sel(Nfleet,NlenBin),Ret(Nfleet,Nlenbin),S0(2,Nstock)

!	****MOVE THESE TO A READFILE AT SOME POINT!!!!!
        ISEEDX =  ISEED1
        ISEEDZ = ISEED2


!	OPEN GENERAL JUNKFILE 
	OPEN(UNIT=99,FILE='Control.junk')	
	WRITE(99,*) 'ISEEDX',ISEEDX
	WRITE(99,*) 'ISEEDZ',ISEEDZ


	WRITE(99,*) 'Stepping into ReadIn'

!	READ IN CONTROL VARIABLES AND SET-UP
	CALL ReadIn(1)
	WRITE(99,*) 'ReadIN finished'


!	DO HISTORICAL PROJECTION
	WRITE(99,*) 'Starting Historical Projection'
	CALL HistProj()
	WRITE(99,*) 'Historical Projection finished'

!	GET HISTORICAL DATA
	CALL FindHistData()

!	ASSIGN RELEVANT QUANTITIES TO RETURN VALUES
	IF (Diag.EQ.1) THEN
	 OPEN(UNIT=98,FILE='Assignment.junk')
	 WRITE(98,*) 'assignments'
	 CLOSE(98)
	ENDIF
	Numbers(1:Nstk,1:Nreg,1:2,1:MaxAge+1) = N(1:Nstk,1:Nreg,1:2,0:MaxAge,Lyear+1)
	Mort(1:Nstk,1:Nreg,1:2,1:MaxAge+1) = M(1:Nstk,1:Nreg,1:2,0:MaxAge,Lyear)
    Mulen(1:Nstk,1:2,1:MaxAge+1) = MeanLenAge(1:Nstk,1:2,0:MaxAge,Lyear+1)
	Siglen(1:Nstk,1:2,1:MaxAge+1) = SigmaLenAge(1:Nstk,1:2,0:MaxAge,Lyear+1)
	Wt(1:Nstk,1:2,1:MaxAge+1) = Weight(1:Nstk,1:2,0:MaxAge,Lyear+1)
	Fec(1:Nstk,1:MaxAge+1) = Fecundity(1:Nstk,0:MaxAge,Lyear+1)
	Move(1:Nstk,1:Nreg,1:Nreg,1:2,1:MaxAge+1) = X(1:Nstk,1:Nreg,1:Nreg,1:2,0:MaxAge,Lyear)
	Sel(1:Nflt,1:Nlen) = SelLen(1:Nflt,1:Nlen,Lyear)
	Ret(1:Nflt,1:Nlen) = RetLen(1:Nflt,1:Nlen,Lyear)
	Qdevs(1:Nflt,1:Nreg) = CpueQdevs(1:Nflt,1:Nreg,Lyear)
	S0(1,1:Nstk) = SBioZero
	S0(2,1:Nstk) = Rzero

	CLOSE(99)


	RETURN

	STOP

	END SUBROUTINE sinatra

!	CALL SYSTEM('gavss2.exe -nohess')

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine performs the historical projection
	SUBROUTINE Histproj

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Ireg,Iyr,Istk,II,NumReg,Ipar
	REAL*8 RzeroHi(Nstk),RzeroLo(Nstk),Obj,TempX(1:10),Tempsum


!	First Set Up the various matrices needed
	WRITE(99,*) 'Stepping into Set-Up'
	CALL HistSetUp()
	WRITE(99,*) 'Finished with Set-Up'

!	code for solving for target depletion to go here and loop around subroutine 'GetNumbersAtAge'
!	retain option for pre-specifying Rzero
	IF (Rzeroflag.EQ.2) THEN
     IF (Diag.EQ.1) OPEN(UNIT=91,FILE='Numbers.junk')
	 OPEN(UNIT=92,FILE='SpawBio.junk')
     OPEN(UNIT=93,FILE='TotCatch.junk')
	 OPEN(UNIT=94,FILE='ExpRate.junk')
	 II=0
	 RzeroHi = 100000000.d0
	 RzeroLo = 0.d0
700	 Rzero(1:Nstk) = RzeroLo(1:Nstk) + (RzeroHi(1:Nstk)-RzeroLo(1:Nstk))/2.d0
	 II=II+1
!	 WRITE(99,*) Rzero(1:Nstk)
	 CALL GetInit()
	 CALL GetNumbersAtAge(0)
!	 WRITE(99,*) TargDep(1:Nstk),SpawBio(1:Nstk,0,Lyear+1),SBiozero(1:Nstk)
	 Obj = SUM(ABS(TargDep(1:Nstk) - (SpawBio(1:Nstk,0,Lyear+1)/SBiozero(1:Nstk))))
!	 WRITE(99,*) SpawBio(1,0,Lyear+1),SBiozero(1),Obj
	 IF (Obj.LT.0.001d0.OR.II.GT.50) GOTO 799
	 DO 701 Istk=1,Nstk
	  IF (TargDep(Istk).GT.(SpawBio(Istk,0,Lyear+1)/SBiozero(Istk))) THEN
	   RzeroLo(Istk) = Rzero(Istk)
	  ELSE
	   RzeroHi(Istk) = Rzero(Istk)
	  ENDIF
701	 CONTINUE
	 GOTO 700
799  IF (Diag.EQ.1) CLOSE(91)
     CLOSE(92)
	 CLOSE(93)
	 CLOSE(94)
    ENDIF


	
!	GET RZERO & LAMDAZERO FROM MCMC OUTPUT
	IF (UseMCMC.EQ.1) THEN
	 Istk=1
	 Ipar = NOMpars(2)+(Nreg-1)+1
	 IF (Nreg.GT.1) Ipar = Ipar + Nreg
	 !Rzero(Istk) =EXP(OpModPars(11))
	 Rzero(Istk) =EXP(OpModPars(Ipar))
	 IF (OMsteep.EQ.1) h(1:Nstk) = OpModPars(Ipar+1)
	 WRITE(99,*) 'Rzero',Rzero(Istk)
	 TempX(1:Nreg) = 1.d0
	 DO Ireg=1,(Nreg-1)
      TempX(Ireg) = EXP(OpModPars(NOMpars(2)+Ireg))	  
     ENDDO
	 !WRITE(*,*) OpModPars(Ipar),LOG(TempX(1:Nreg))
	 Numreg=Nreg
	 LamdaZero(Istk,1:Nreg) = TempX(1:Nreg)/SUM(TempX(1:Nreg))
	 Nreg=Numreg
	 !WRITE(99,*) LamdaZero(Istk,1),LamdaZero(Istk,2)
	 !WRITE(99,*) Nreg
	ENDIF

!	OPEN some summary files
    !WRITE(99,*) Nreg
	IF (Diag.EQ.1) THEN
	 OPEN(UNIT=91,FILE='Numbers.junk')
	 WRITE(91,'(A13,200(I10,1x))') 'Year Stk R S ',(Age,Age=0,MaxAge)
	ENDIF
	!WRITE(99,*) Nreg
	OPEN(UNIT=92,FILE='SpawBio.junk')
    OPEN(UNIT=93,FILE='TotCatch.junk')
	OPEN(UNIT=94,FILE='ExpRate.junk')
	WRITE(92,'(A19,20(I10,1x))') 'Year Stk   Total  ',(Ireg,Ireg=1,Nreg)
	WRITE(93,'(A61)') 'Year Fl Rg  VBio    RetVBio  TotalC Discard Retained RetResid'
    WRITE(94,'(A17)') 'Year Fl Rg Ufleet'


!	Initial Conditions
	WRITE(99,*) 'Initializing'
    CALL GetInit()
	WRITE(99,*) 'Finished with Initialization'

!	Update for each year of historical period
	WRITE(99,*) 'Doing historical projection'
	WRITE(99,*) '...check histproj.junk'
	CALL GetNumbersAtAge(1)		!argument is a flag to write to junk files - only trip it when calling with final Rzero
	WRITE(99,*) 'Finished historical projection'

	IF (Diag.EQ.1) THEN
	CLOSE(91)
	ENDIF
	CLOSE(92)
	CLOSE(93)
	CLOSE(94)

!	Done with hist projection

	RETURN

	END
        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine sets up the various matrices of parameters needed for the historical projection
    SUBROUTINE HistSetUp

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER II,Istk,Ireg,Sex

!	Biology
	IF (Diag.EQ.1) WRITE(99,*) 'Setting up Biology - check biology.junk'
	IF (Diag.EQ.1) OPEN(UNIT=98,FILE='Biology.junk')
	!M
	CALL GetM(Fyear,Lyear)
	!Growth
	!Length at age
	CALL GetLenAge(Fyear,Lyear+1)
	!SD of len @ age
	CALL GetSDLenAge(Fyear,Lyear+1)
	!Weight at age
	CALL GetWtAge(Fyear,Lyear+1)
	CALL Get_WtLen()
	!Movement (X Matrix)
	CALL GetX(Fyear,Lyear+1)

	IF (Diag.EQ.1) CLOSE(98)
    IF (Diag.EQ.1) WRITE(99,*) 'Finished setting up Biology'

!	Selectivity
	IF (Diag.EQ.1) WRITE(99,*) 'Setting up Selectivity - check Selex.junk'
	IF (Diag.EQ.1) OPEN(UNIT=98,FILE='Selex.junk')

	!Proportions of age in length bins
	CALL GetFracLen(Fyear,Lyear+1)

	IF (Diag.EQ.1) CLOSE(98)
	IF (Diag.EQ.1) OPEN(UNIT=98,FILE='Biology.junk',POSITION='APPEND')
	!Fecundity at age
	CALL GetFracLenStart(Fyear,Lyear+1)
	CALL GetFecund(Fyear,Lyear+1)
	IF (Diag.EQ.1) CLOSE(98)
	IF (Diag.EQ.1) OPEN(UNIT=98,FILE='Selex.junk',POSITION='APPEND')


	!Correlation among length bins in annual selex deviations 
	CALL GetCorDevSel()
	!Selectivity at Age
	CALL GetSelex(Fyear,Lyear)
	!Retention
	CALL GetRetent(Fyear,Lyear)

	IF (Diag.EQ.1) CLOSE(98)
    IF (Diag.EQ.1) WRITE(99,*) 'Finished setting up Selectivity'

!	Recruitment Devs
	IF (Diag.EQ.1) WRITE(99,*) 'Setting up Recruitment devs - check RecDevs.junk'
	IF (Diag.EQ.1) OPEN(UNIT=98,FILE='RecDevs.junk')
	
!	Get the recruitment deviations
	CALL GetRecDevs(Fyear,Lyear+1)
	IF (Diag.EQ.1) CLOSE(98)
    IF (Diag.EQ.1) WRITE(99,*) 'Finished setting up Recruitment Devs'



	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	START OF MODEL EQUATIONS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.1 POPULATION DYNAMICS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.1.1 ABUNDANCE DYNAMICS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine does the annual update 
!	Section 1.1.1
!	Equations 1.1,1.2,1.3
!
	SUBROUTINE PopUpdate(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Ireg,Iflt,Iyr,Istk,Sex,Jreg
	REAL*8 Ntemp(Nreg,0:MaxAge),Ntemp2(Nreg,0:MaxAge)

!	annual header for junk file
	IF (Diag.EQ.1) THEN
	 WRITE(98,*)
	 WRITE (98,'(A15,I4)') 'popupdate for ',Iyr
	 WRITE(98,*)
	ENDIF

!	First get exploitation rate by fleet and region
!	equation 1.5
	CALL Get_Ufleet(Iyr-1)
!	First get exploitation rate by Age
!	equation 1.4
	CALL Get_Uage(Iyr-1)

	

!	update numbers at age
	 DO 2000 Istk=1,Nstk	  
	  DO 2000 Sex=1,2
	
!	   first apply mortality
!	   equation 1.3	   
	   DO 2001 Ireg=1,Nreg	    
		DO 2001 Age=0,MaxAge
         Ntemp(Ireg,Age) = N(Istk,Ireg,Sex,Age,Iyr-1)*EXP(-1.d0*M(Istk,Ireg,Sex,Age,Iyr-1))*(1.d0-Uage(Istk,Ireg,Sex,Age,Iyr-1))
2001	CONTINUE	    

!	   movement
!	   equation 1.2
	   DO 2002 Age=0,MaxAge
	    DO 2002 Ireg=1,Nreg
		 Ntemp2(Ireg,Age) = 0.d0
		 DO 2003 Jreg=1,Nreg
		  Ntemp2(Ireg,Age) = Ntemp2(Ireg,Age) + Ntemp(Jreg,Age)*X(Istk,Jreg,Ireg,Sex,Age,Iyr-1)
2003	 CONTINUE
2002   CONTINUE
		 
!	   increment ages and store
	   DO 2004 Ireg=1,Nreg
	    DO 2005 Age=1,MaxAge
	     N(Istk,Ireg,Sex,Age,Iyr) = Ntemp2(Ireg,Age-1)
		 IF (N(Istk,Ireg,Sex,Age,Iyr).LT.0) N(Istk,Ireg,Sex,Age,Iyr) = 0.d0
2005	CONTINUE
!		plus group
	    N(Istk,Ireg,Sex,MaxAge,Iyr) = N(Istk,Ireg,Sex,MaxAge,Iyr) + Ntemp2(Ireg,MaxAge)
		IF (N(Istk,Ireg,Sex,MaxAge,Iyr).LT.0) N(Istk,Ireg,Sex,MaxAge,Iyr) = 0.d0
2004   CONTINUE


2000 CONTINUE

!	 IF (Iyr.EQ.2006) WRITE(98,*) N(1,1,1,0:MaxAge,Iyr)
	 !Calculate Spawning biomass
	 !equation 1.24
     CALL GetSpawBio(Iyr)
	 IF (Iyr.GE.2007) WRITE(99,*) SBioZero(1:Nstk),SpawBio(1,0:Nreg,Iyr)

	 !Calculate Recruits
	 !equation1.18
	 DO 2010 Istk=1,Nstk
	  CALL Get_Recruits(Istk,Iyr)
	  DO 2010 Sex=1,2
	   !N(Istk,1:Nreg,Sex,0,Iyr) = 0.5d0*Recruits(Istk,1:Nreg,Iyr)
       N(Istk,1:Nreg,Sex,0,Iyr) = 0.5d0*LamdaZero(Istk,1:Nreg)*SUM(Recruits(Istk,1:Nreg,Iyr))   !MADE CHANGE ON 13th January 2011, to force proportional recruitment by area according to input.
2010 CONTINUE

	 !Vulnerable Biomass
	 !denominator of rhs equation 1.5 
	 CALL GetVBio(Iyr)

	 !Calculate Total Catches and discards for previous year
	 CALL Get_TotalCatch(Iyr-1)


	!print out N matrix
	IF (Diag.EQ.1) THEN
	 DO 2020 Istk=1,Nstk
	  DO 2020 Ireg=1,Nreg
	   DO 2020 Sex=1,2
	    WRITE(91,'(I4,1x,I2,1x,I2,1x,I1,1x,200(F10.2,1x))') Iyr,Istk,Ireg,Sex,(N(Istk,Ireg,Sex,Age,Iyr),Age=0,MaxAge)
2020 CONTINUE
	ENDIF


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the age-specific exploitation rate by region for a given year
!	Equation 1.4
	SUBROUTINE Get_Uage(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,Ireg,Iyr,Sex,Age,Istk

	IF (Diag.EQ.1) WRITE(98,'(2x,I4,A15)') Iyr,' age exp rate'
    IF (Diag.EQ.1) WRITE(98,'(A7,1x,200(I6,1x))') 'St S Rg',(Age,Age=0,MaxAge)
	
	DO 1210 Istk=1,Nstk
     DO 1210 Sex=1,2
 	  DO 1210 Ireg=1,Nreg
	   DO 1211 Age=0,MaxAge
	    Uage(Istk,Ireg,Sex,Age,Iyr) = 0.d0
        DO 1211 Iflt=1,Nflt
		 Uage(Istk,Ireg,Sex,Age,Iyr) = Uage(Istk,Ireg,Sex,Age,Iyr) + Ufleet(Iflt,Ireg,Iyr)*SelAge(Iflt,Istk,Sex,Age,Iyr)
		 !Trap for extinction....
		 IF (Uage(Istk,Ireg,Sex,Age,Iyr).GT.0.999d0) Uage(Istk,Ireg,Sex,Age,Iyr) = 0.999d0
1211   CONTINUE
	   IF (Diag.EQ.1) WRITE(98,'(I2,1x,I1,1x,I2,1x,200(F6.3,1x))') Istk,Sex,Ireg,(Uage(Istk,Ireg,Sex,Age,Iyr),Age=0,MaxAge)    
1210 CONTINUE


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the fleet-specific exploitation rate by region for a given year
!	Equation 1.5
	SUBROUTINE Get_Ufleet(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,Ireg,Iyr,Sex,Age,Istk


	IF (Diag.EQ.1) WRITE(98,'(2x,I4,A15)') Iyr,' fleet exp rate'

    IF (Diag.EQ.1) WRITE(98,'(A4,1x,100(I6,1x))') 'Iflt',(Ireg,Ireg=1,Nreg)

	DO 1200 Iflt=1,Nflt
	 DO 1201 Ireg=1,Nreg
	  IF (RetVBio(Iflt,Ireg,Iyr).LE.0) WRITE(98,'(A20,I2,1x,I2)') 'zero vuln biomass, ',Iflt,Ireg
	  Ufleet(Iflt,Ireg,Iyr)=Catch(Iflt,Ireg,Iyr)/RetVBio(Iflt,Ireg,Iyr)
	  !Trap for extinction....
 	  IF (Ufleet(Iflt,Ireg,Iyr).GT.0.999d0) Ufleet(Iflt,Ireg,Iyr) = 0.999d0
	  !write to file
	  WRITE(94,'(I4,1x,I2,1x,I2,1x,F6.4)') Iyr,Iflt,Ireg,Ufleet(Iflt,Ireg,Iyr)
1201 CONTINUE
	 IF (Diag.EQ.1) WRITE(98,'(I4,1x,20(F6.4,1x))') Iflt,(Ufleet(Iflt,Ireg,Iyr),Ireg=1,Nreg)
1200 CONTINUE

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine calculates the total vulnerable biomass for a given year
!	several, but includes denominator of rhs of Equation 1.5
	SUBROUTINE GetVBio(Year)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Istk,Ireg,Year,Sex,Iflt,Ilen
	REAL*8 Temp


	VBio(1:Nflt,1:Nreg,Year)=0.d0
	RetVBio(1:Nflt,1:Nreg,Year)=0.d0
	DO 640 Iflt=1,Nflt
	 DO 641 Ireg=1,Nreg
	  DO 641 Istk=1,Nstk
	   DO 641 Sex=1,2
	    DO 641 Age=0,MaxAge
!		  Temp = SelAge(Iflt,Istk,Sex,Age,Year)*N(Istk,Ireg,Sex,Age,Year)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Year))*WeightMid(Istk,Sex,Age,Year)
!	      VBio(Iflt,Ireg,Year) = VBio(Iflt,Ireg,Year) + Temp
!		 RetVBio(Iflt,Ireg,Year) = RetVBio(Iflt,Ireg,Year) + Temp*RetAge(Iflt,Istk,Sex,Age,Year)
  		  DO 641,Ilen=1,Nlen
		   Temp = WtLen(Ilen,Istk,Sex)*Sellen(Iflt,Ilen,Year)*Fraclen(Ilen,Istk,Sex,Age,Year)*N(Istk,Ireg,Sex,Age,Year)*EXP(-0.5d0*M(Istk,Ireg,Sex,Age,Year))
		   IF (Age.EQ.0) Temp = Temp*SelAge(Iflt,Istk,Sex,Age,Year)
!		  WRITE(98,*) Year,Iflt,Sex,Age
!		  WRITE(98,*) 
	      VBio(Iflt,Ireg,Year) = VBio(Iflt,Ireg,Year) + Temp
		  RetVBio(Iflt,Ireg,Year) = RetVBio(Iflt,Ireg,Year) + Retlen(Iflt,Ilen,Year)*Temp
!		  RetVBio(Iflt,Ireg,Year) = RetVBio(Iflt,Ireg,Year) + RetAge(Iflt,Istk,Sex,Age,Year)*Temp
641	 CONTINUE
     !VBio(Iflt,0,Year) = SUM(VBio(Iflt,1:Nreg,Year))
	 !RetVBio(Iflt,0,Year) = SUM(RetVBio(Iflt,1:Nreg,Year))
640	CONTINUE


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine finds the total catch and does some internal checking
! includes equation 1.13
	SUBROUTINE Get_TotalCatch(Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,Iflt,Ireg,Sex,Age,Istk 
	REAL*8 Temp

	!RetCatch is a check against the input retained catches (Catch)
	DO 2100 Iflt=1,Nflt
	 DO 2100 Ireg=1,Nreg
	  RetCatch(Iflt,Ireg,Iyr) = RetVBio(Iflt,Ireg,Iyr)*Ufleet(Iflt,Ireg,Iyr)
	  TotalCatch(Iflt,Ireg,Iyr) = VBio(Iflt,Ireg,Iyr)*Ufleet(Iflt,Ireg,Iyr)
	  Discard(Iflt,Ireg,Iyr) = TotalCatch(Iflt,Ireg,Iyr)-RetCatch(Iflt,Ireg,Iyr)
	  WRITE(93,'(I4,1x,I2,1x,I2,1x,100(F20.4,1x))') Iyr,Iflt,Ireg,VBio(Iflt,Ireg,Iyr),RetVBio(Iflt,Ireg,Iyr),TotalCatch(Iflt,Ireg,Iyr),Discard(Iflt,Ireg,Iyr),RetCatch(Iflt,Ireg,Iyr),Catch(Iflt,Ireg,Iyr)-RetCatch(Iflt,Ireg,Iyr)
2100 CONTINUE

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for M
	SUBROUTINE GetM(yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER II,Istk,Ireg,Sex,Age,Iyr,Yr1,Yr2
	REAL*8 NewDev,XNORM,cvM

	EXTERNAL XNORM

    !IF (UseMcMc.EQ.1.AND.OMgrowth(1).EQ.1) Mzero(1:Nstk,0:MaxAge) = OpModpars(1)

	IF (Diag.EQ.1) WRITE(98,'(A20)') 'Natural Mortality'
	IF (Diag.EQ.1) WRITE(98,'(A12,1x,100(I5,1x))') 'Yr Stk Sx Zn',(Age,Age=0,MaxAge)
    DO 100 Istk=1,Nstk
	 DO 100 Sex=1,2	  
	  DO 100 Ireg=1,Nreg
	   M(Istk,Ireg,Sex,0:MaxAge,Fyear)=Mzero(Istk,0:MaxAge)
	   IF (UseMCMC.EQ.1.AND.Sex.EQ.1.AND.OMgrowth(1).EQ.1)  M(Istk,Ireg,Sex,0:MaxAge,Fyear) = OpModpars(1)
	   IF (UseMCMC.EQ.1.AND.Sex.EQ.2.AND.OMgrowth(1).EQ.1) THEN
	     M(Istk,Ireg,Sex,0:MaxAge,Fyear) = M(Istk,Ireg,1,0:MaxAge,Fyear)
		 IF (OMgrowth(8).EQ.1) M(Istk,Ireg,Sex,0:MaxAge,Fyear) = M(Istk,Ireg,Sex,0:MaxAge,Fyear)*EXP(OpModpars(SUM(OMgrowth(1:8))))
	   ENDIF
	   IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I1,1x,I2,1x,100(F5.3,1x))') Yr1,Istk,Sex,Ireg,(M(Istk,Ireg,Sex,Age,Yr1),Age=0,MaxAge)
	   DO 100 Iyr=Yr1+1,Yr2
	     M(Istk,Ireg,Sex,0:MaxAge,Iyr)=M(Istk,Ireg,Sex,0:MaxAge,Iyr-1)
		IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I1,1x,I2,1x,100(F5.3,1x))') Iyr,Istk,Sex,Ireg,(M(Istk,Ireg,Sex,Age,Iyr),Age=0,MaxAge)
100 CONTINUE

	    IF (HCRspecs(3).EQ.11.AND.Yr2.GT.Lyear) THEN
		 OPEN(UNIT=13,FILE='Mtime.dat')
		 READ(13,*)
		 READ(13,*) cvM
		 CLOSE(13)
		 DO 101 Iyr=Yr1+1,Yr2		  
		  NewDev = XNORM(5,0.d0,cvM,ISEEDX)
	      DO 102 Istk=1,Nstk
	       DO 102 Sex=1,2	  
	        DO 102 Ireg=1,Nreg
		     M(Istk,Ireg,Sex,0:MaxAge,Iyr) = M(Istk,Ireg,Sex,0:MaxAge,Iyr-1)*EXP(NewDev-0.5d0*cvM*cvM)
     		 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I1,1x,I2,1x,100(F5.3,1x))') Iyr,Istk,Sex,Ireg,(M(Istk,Ireg,Sex,Age,Iyr),Age=0,MaxAge)
102		  CONTINUE
101		  CONTINUE
         ENDIF
	
	 	   
	RETURN
	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.1.2 SELECTIVITY
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for selectivity at age
!	Equation 1.6
	SUBROUTINE GetSelex(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'


	INTEGER Ilen,Istk,Sex,Age,Iyr,Yr1,Yr2,Iflt
	REAL*8 Stemp(0:MaxAge)
	
	!First Get Selectivity at length
	!equation 1.8
	CALL GetSelLen(Yr1,Yr2)


	!get selectivity at age
	IF (Diag.EQ.1) WRITE(98,'(A40)') 'selectivity at age'
	IF (Diag.EQ.1) WRITE(98,'(A12,1x,100(I6,1x))') 'fl Stk S  Yr',(Age,Age=0,MaxAge)

    DO 80 Iflt=1,Nflt
	 DO 80 IStk=1,Nstk
	  DO 80 Sex=1,2
	   DO 80 Iyr=Yr1,Yr2
        Stemp = 0.d0
		DO 81 Ilen=1,Nlen
		 Stemp = Stemp + SelLen(Iflt,Ilen,Iyr)*FracLen(Ilen,Istk,Sex,0:MaxAge,Iyr)
81		CONTINUE
		SelAge(Iflt,Istk,Sex,0:Maxage,Iyr) = Stemp
		SelAge(Iflt,Istk,Sex,0,Iyr) = 0.d0
		IF (Diag.EQ.1) WRITE(98,'(I2,1x,I2,1x,I1,1x,I4,1x,100(F6.4,1x))') Iflt,Istk,Sex,Iyr,(SelAge(Iflt,Istk,Sex,Age,Iyr),Age=0,MaxAge)
80   CONTINUE

	RETURN

	END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for the proportions of age by length bins at the start of the year
!	Equation 1.7
	SUBROUTINE GetFracLenStart(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'


	INTEGER Ilen,Istk,Sex,Age,Iyr,Yr1,Yr2
	REAL*8 ZSCORE,FRAC(1:NlenBin),IFRAC,Length(1:NlenBin)
    
	DO Ilen=1,Nlen
 	 Length(Ilen)=lolenbin(Ilen)	!+(hilenbin(Ilen)-lolenbin(Ilen))/2.d0
	ENDDO
	
	IF (Diag.EQ.1) WRITE(98,'(A40)') 'Proportion of age by length bin'
	IF (Diag.EQ.1) WRITE(98,'(A24,1x,100(I6,1x))') 'Yr Stk S LB LO UP ',(Age,Age=0,MaxAge)

	FraclenStart=0.d0
	DO 70 Istk=1,Nstk
	 DO 70 Sex=1,2
	  DO 70 Iyr=Yr1,Yr2
	   DO 70 Age=0,MaxAge
	    !WRITE(*,*) Istk,Sex,Iyr,Age
!		smallest length bin
		ZSCORE=(Length(1)-LenStart(Istk,Sex,Age,Iyr))/SigmaStart(Istk,Sex,Age,Iyr)
		CALL CUMNOR(ZSCORE,FRAC(1),IFRAC)
		FracLenStart(1,Istk,Sex,Age,Iyr)=FRAC(1)
!		bottom for 2nd length bin
		ZSCORE=(Length(2)-LenStart(Istk,Sex,Age,Iyr))/SigmaStart(Istk,Sex,Age,Iyr)
	    CALL CUMNOR(ZSCORE,FRAC(2),IFRAC)
		FracLenStart(1,Istk,Sex,Age,Iyr)=FRAC(2)
	    DO 71 Ilen=2,Nlen-1		
		 ZSCORE=(Length(Ilen+1)-LenStart(Istk,Sex,Age,Iyr))/SigmaStart(Istk,Sex,Age,Iyr)
	     CALL CUMNOR(ZSCORE,FRAC(Ilen+1),IFRAC)
		 FracLenStart(Ilen,Istk,Sex,Age,Iyr)=FRAC(Ilen+1)-FRAC(Ilen)
71	    CONTINUE
		FracLenStart(Nlen,Istk,Sex,Age,Iyr) = 1.d0 - FRAC(Nlen)
!		FracLen(1:Nlen,Istk,Sex,Age,Iyr)=FracLen(1:Nlen,Istk,Sex,Age,Iyr)/SUM(FracLen(1:Nlen,Istk,Sex,Age,Iyr))
!		IF (Iyr.EQ.Yr1.AND.Sex.EQ.1.AND.Istk.EQ.1) WRITE(*,'(I2,1x,100(F5.3,1x))') Age,Fraclen(1:Nlen,Istk,Sex,Age,Iyr)
70	CONTINUE

	IF (Diag.EQ.1.AND.Yr1.EQ.Fyear) THEN
	 Iyr = Yr1
	 DO 75 IStk=1,Nstk
	  DO 75 Sex=1,2
	    DO 75 Ilen=Nlen,1,-1
		 WRITE(98,'(I4,1x,I2,1x,I1,1x,I2,1x,2(F4.1,1x),100(F6.4,1x))') Iyr,Istk,Sex,Ilen,lolenbin(Ilen),hilenbin(Ilen),(FraclenStart(Ilen,Istk,Sex,Age,Iyr),Age=0,MaxAge)
75   CONTINUE
	ENDIF

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for the proportions of age by length bins
!	Equation 1.7
	SUBROUTINE GetFracLen(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'


	INTEGER Ilen,Istk,Sex,Age,Iyr,Yr1,Yr2
	REAL*8 ZSCORE,FRAC(1:NlenBin),IFRAC,Length(1:NlenBin)

	DO Ilen=1,Nlen
 	 Length(Ilen)=lolenbin(Ilen)	!+(hilenbin(Ilen)-lolenbin(Ilen))/2.d0
	ENDDO
	
	IF (Diag.EQ.1) WRITE(98,'(A40)') 'Proportion of age by length bin'
	IF (Diag.EQ.1) WRITE(98,'(A24,1x,100(I6,1x))') 'Yr Stk S LB LO UP ',(Age,Age=0,MaxAge)

	Fraclen=0.d0
	DO 70 Istk=1,Nstk
	 DO 70 Sex=1,2
	  DO 70 Iyr=Yr1,Yr2
	   DO 70 Age=0,MaxAge
!		smallest length bin
		ZSCORE=(Length(1)-MeanlenAge(Istk,Sex,Age,Iyr))/SigmaLenAge(Istk,Sex,Age,Iyr)
		CALL CUMNOR(ZSCORE,FRAC(1),IFRAC)
		FracLen(1,Istk,Sex,Age,Iyr)=FRAC(1)
!		bottom for 2nd length bin
		ZSCORE=(Length(2)-MeanlenAge(Istk,Sex,Age,Iyr))/SigmaLenAge(Istk,Sex,Age,Iyr)
	    CALL CUMNOR(ZSCORE,FRAC(2),IFRAC)
		FracLen(1,Istk,Sex,Age,Iyr)=FRAC(2)
	    DO 71 Ilen=2,Nlen-1		
		 ZSCORE=(Length(Ilen+1)-MeanlenAge(Istk,Sex,Age,Iyr))/SigmaLenAge(Istk,Sex,Age,Iyr)
	     CALL CUMNOR(ZSCORE,FRAC(Ilen+1),IFRAC)
		 FracLen(Ilen,Istk,Sex,Age,Iyr)=FRAC(Ilen+1)-FRAC(Ilen)
71	    CONTINUE
		FracLen(Nlen,Istk,Sex,Age,Iyr) = 1.d0 - FRAC(Nlen)
!		FracLen(1:Nlen,Istk,Sex,Age,Iyr)=FracLen(1:Nlen,Istk,Sex,Age,Iyr)/SUM(FracLen(1:Nlen,Istk,Sex,Age,Iyr))
!		IF (Iyr.EQ.Yr1.AND.Sex.EQ.1.AND.Istk.EQ.1) WRITE(*,'(I2,1x,100(F5.3,1x))') Age,Fraclen(1:Nlen,Istk,Sex,Age,Iyr)
70	CONTINUE

	IF (Diag.EQ.1) THEN
	 DO 75 IStk=1,Nstk
	  DO 75 Sex=1,2
	   DO 75 Iyr=Yr1,Yr2
	    DO 75 Ilen=Nlen,1,-1
		 WRITE(98,'(I4,1x,I2,1x,I1,1x,I2,1x,2(F4.1,1x),100(F6.4,1x))') Iyr,Istk,Sex,Ilen,lolenbin(Ilen),hilenbin(Ilen),(Fraclen(Ilen,Istk,Sex,Age,Iyr),Age=0,MaxAge)
75   CONTINUE
	ENDIF

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for selectivity at length
!	Equation 1.8
	SUBROUTINE GetSelLen(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Ilen,Iyr,Yr1,Yr2,Iflt
	REAL*8 Stemp,Posbit

	!First get deviations in selectivity
	!Equations 1.9 & 1.10
	CALL GetDevSel(Yr1,Yr2)


	Posbit = 0.000000000000001
	
	!Now get selectivity at length
	IF (Diag.EQ.1) THEN
	 WRITE(98,'(A40)') 'selectivity at length'
	 WRITE(98,'(A7,1x,100(I6,1x))') 'fl   Yr ',(Ilen,Ilen=1,Nlen)
    ENDIF

	IF (UseMCMC.EQ.1) CALL GetpostSel()

	DO 90 Iflt=1,Nflt
     IF (Diag.EQ.1) WRITE(98,'(I2,1x,I4,1x,100(F6.4,1x))') Iflt,Yr1,(SelLen(Iflt,Ilen,Yr1),Ilen=1,Nlen)
	 DO 90 Iyr=Yr1+1,Yr2
      DO 91 Ilen=1,Nlen
	   Stemp = LOG((Posbit+SelLen(Iflt,Ilen,Fyear))/(1+Posbit-SelLen(Iflt,Ilen,Fyear)))+DevSel(Iflt,Ilen,Iyr)
!	   Stemp = LOG((Posbit+SelLen(Iflt,Ilen,Iyr-1))/(1+Posbit-SelLen(Iflt,Ilen,Iyr-1)))+DevSel(Iflt,Ilen,Iyr)
	   SelLen(Iflt,Ilen,Iyr)=EXP(Stemp)/(1+EXP(Stemp))
91	  CONTINUE
     IF (Diag.EQ.1) WRITE(98,'(I2,1x,I4,1x,100(F6.4,1x))') Iflt,Iyr,(SelLen(Iflt,Ilen,Iyr),Ilen=1,Nlen)
90	CONTINUE


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   get selectivity at length from posterior parameters
	SUBROUTINE GetpostSel

    IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

    INTEGER Ilen,Iflt,lopar,hipar
	REAL*8 TempX(1:1000),TempUp(1:1000),TempDown(1:1000)
	REAL*8 Lengths(Nlen),Join1(1:1000),Join2(1:1000)

	Lengths(1:Nlen) = lolenbin(1:Nlen) + (hilenbin(1:Nlen)-lolenbin(1:Nlen))/2.d0

	hipar = NOMpars(2)+NOMpars(3)+Nreg+OMsteep
	IF (Nreg.GT.1) hipar = hipar + Nreg
	DO 140 Iflt=1,Nflt
	 !SS selex type 24
	 IF (Iflt.GT.1) lopar=hipar+1+(2*OMdoreten(Iflt-1))
	 IF (Iflt.EQ.1) lopar=hipar+1
	 !lopar=hipar+1
	 hipar=lopar+OMNselpars(Iflt)-1
	 TempX(1:(OMNselpars(Iflt))) = OpModPars(lopar:hipar)
	 WRITE(99,*) Iflt,lopar,hipar
     WRITE(99,*) TempX(1:4)
	 
	 IF (OMseltype(Iflt).EQ.2) THEN
	 TempX(3) = EXP(TempX(3))
	 TempX(4) = EXP(TempX(4))
     TempX(2) = TempX(1) + 5 + (0.99d0*Lengths(Nlen)-TempX(1)-5)/(1+EXP(-1.d0*TempX(2)))     
	 TempUp(1:Nlen) = EXP(-1.d0*(((Lengths(1:Nlen)-TempX(1))**2.d0)/TempX(3)))
	 TempDown(1:Nlen) = EXP(-1.d0*(((Lengths(1:Nlen)-TempX(2))**2.d0)/TempX(4)))
     !WRITE(99,'(1000(F7.4,1x))') (TempUp(Ilen),Ilen=1,Nlen)
	 !WRITE(99,'(1000(F7.4,1x))') (TempDown(Ilen),Ilen=1,Nlen)

     Join1(1:Nlen) = 1.d0/(1.d0+EXP(-1.d0*(20.d0*(Lengths(1:Nlen)-TempX(1))/(1.d0+ABS(Lengths(1:Nlen)-TempX(1))))))
     Join2(1:Nlen) = 1.d0/(1.d0+EXP(-1.d0*(20.d0*(Lengths(1:Nlen)-TempX(2))/(1.d0+ABS(Lengths(1:Nlen)-TempX(2))))))

     !WRITE(99,'(1000(F7.4,1x))') (Join1(Ilen),Ilen=1,Nlen)
	 !WRITE(99,'(1000(F7.4,1x))') (Join2(Ilen),Ilen=1,Nlen)

     SelLen(Iflt,1:Nlen,Fyear) = TempUp(1:Nlen)*(1-Join1(1:Nlen))+Join1(1:Nlen)*(1.d0*(1.d0-Join2(1:Nlen))+TempDown(1:Nlen)*Join2(1:Nlen))

	 ENDIF

	 IF (OMseltype(Iflt).EQ.1) THEN
	  SelLen(Iflt,1:Nlen,Fyear) = 1.d0/(1.d0+EXP(-1.d0*LOG(19.d0)*(Lengths(1:Nlen)-TempX(1))/TempX(2)))
	 ENDIF

140 CONTINUE  

    RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the annual deviations in selectivity at length
!	Equation 1.9
	SUBROUTINE GetDevSel(Yr1,Yr2)	

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,Ilen,Yr1,Yr2,Iyr,ISEED
	REAL*8 Devs(1:Nlen),MEANS(Nlen),SG(Nlen),TT(1:Nlen,1:Nlen)

	ISEED = ISEEDZ

	DevSel = 0

	!Get the deviations
	IF (Diag.EQ.1) WRITE(98,'(A60)') 'deviations in logit selectivity at length'
	IF (Diag.EQ.1) WRITE(98,'(A8,1x,100(I6,1x))') 'fl   Yr ',(Ilen,Ilen=1,Nlen)

	MEANS = 0.d0
	DO 210 Iflt=1,Nflt
	 SG = SQRT(VarDevSel(Iflt))
	 TT = CorDevSel(Iflt,1:Nlen,1:Nlen)
	 DO 210 Iyr=Yr1+1,Yr2
	  Devs=0.d0
      CALL GenMult(Devs,MEANS,ISEEDZ,Nlen,TT,SG,Nlen)
	  DevSel(Iflt,1:Nlen,Iyr) = Devs
      IF (Diag.EQ.1) WRITE(98,'(I2,1x,I4,1x,100(F6.4,1x))') Iflt,Iyr,(DevSel(Iflt,Ilen,Iyr),Ilen=1,Nlen)
210 CONTINUE
	 
	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the correlation matrix among length bins for the annual deviations in selectivity at length
!	Equation 1.10
	SUBROUTINE GetCorDevSel

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,Ilen,Jlen


	IF (Diag.EQ.1) WRITE(98,'(A20)') 'Correlations among length bins'
	IF (Diag.EQ.1) WRITE(98,'(A6,1x,100(I6,1x))') 'fl Len',(Ilen,Ilen=1,Nlen)
	
	DO 220 Iflt=1,Nflt 
	 DO 220 Ilen=1,Nlen
	  DO 221 Jlen=1,Ilen
	   CorDevSel(Iflt,Ilen,Jlen) = CorSel(Iflt)*(Ilen-Jlen)
	   CorDevSel(Iflt,Jlen,Ilen) = CorDevSel(Iflt,Ilen,Jlen)
221	  CONTINUE
220	CONTINUE

	IF (Diag.EQ.1) THEN
	 DO 222 Iflt=1,Nflt 
	  DO 222 Ilen=1,Nlen
	   WRITE(98,'(I2,1x,I3,1x,100(F6.4,1x))') Iflt,Ilen,(CorDevSel(Iflt,Ilen,Jlen),Jlen=1,Nlen)
222	 CONTINUE
	ENDIF

	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.1.3 RETENTION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the retention at age
!	Equation 1.11
	SUBROUTINE GetRetent(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,IStk,Sex,Age,Iyr,Yr1,Yr2,Ilen

	!Get retention pattern parameters if input as such
    IF (Retflag(1).NE.1) CALL GetRetGamma(Yr1,Yr2)

	!Get the time series of Retention at length
	CALL GetRetLen(Yr1,Yr2)

	!Now get the retention at age for the pop model
	IF (Diag.EQ.1) WRITE(98,'(A16)') 'Retention at Age'
	IF (Diag.EQ.1) WRITE(98,'(A12,1x,100(I6,1x))') 'fl Stk S Iyr',(Age,Age=0,MaxAge)

	DO 230 Iflt=1,Nflt
	 DO 230 Istk=1,Nstk
      DO 230 Sex=1,2
	   DO 230 Iyr=Yr1,Yr2
 	    DO 231 Age=0,MaxAge	    
	     RetAge(Iflt,Istk,Sex,Age,Iyr) = 0.d0
		  DO 231 Ilen=1,Nlen
		   RetAge(Iflt,Istk,Sex,Age,Iyr) = RetAge(Iflt,Istk,Sex,Age,Iyr) + RetLen(Iflt,Ilen,Iyr)*FracLen(Ilen,Istk,Sex,Age,Iyr)
231		CONTINUE
		IF (Diag.EQ.1) WRITE(98,'(I2,1x,I2,1x,I1,1x,I4,1x,100(F6.4,1x))') Iflt,Istk,Sex,Iyr,(RetAge(Iflt,Istk,Sex,Age,Iyr),Age=0,MaxAge)
230	CONTINUE

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the retention at length 
!	Equation 1.12
	SUBROUTINE GetRetLen(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,IStk,Sex,Age,Iyr,Yr1,Yr2,Ilen,lopar,hipar
	REAL*8 MidLens(1:Nlen),TempX(2)

	MidLens = loLenBin+(hiLenBin-loLenBin)/2.d0

	IF (Diag.EQ.1) WRITE(98,'(A16)') 'Retention at length'
	IF (Diag.EQ.1) WRITE(98,'(A7,1x,100(I6,1x))') 'fl  Yr ',(Ilen,Ilen=1,Nlen)

	!If retention parameters input:
	IF (Retflag(1).NE.1) THEN
	 
     DO 240 Iflt=1,Nflt
	  DO 240 Iyr=Yr1,Yr2	   
  	   IF (Retflag(Iflt+1).EQ.1) THEN
	    Retlen(Iflt,1:Nlen,Iyr) = RetGamma(Iflt,1,Iyr)
       ELSEIF (Retflag(Iflt+1).EQ.2) THEN
	    Retlen(Iflt,1:Nlen,Iyr) = 1+EXP(-1.d0*(MidLens(1:Nlen)-RetGamma(Iflt,1,Iyr))/RetGamma(Iflt,2,Iyr))
	   ELSEIF (Retflag(Iflt+1).EQ.3) THEN
	    Retlen(Iflt,1:Nlen,Iyr) = RetGamma(Iflt,1,Iyr)*(1+EXP(-1.d0*(MidLens(1:Nlen)-RetGamma(Iflt,2,Iyr))/RetGamma(Iflt,3,Iyr)))  
	   ENDIF
240	 CONTINUE

    ENDIF

	!If inital ret@length pattern input
	IF (Retflag(1).EQ.1) THEN
	 DO 241 Iflt=1,Nflt
	  DO 241 Iyr = Yr1+1, Yr2
	   Retlen(Iflt,1:Nlen,Iyr) = Retlen(Iflt,1:Nlen,Iyr-1)
241  CONTINUE
	
	ENDIF

    IF (UseMCMC.EQ.1.AND.SUM(OMdoreten(1:Nflt)).GT.0) THEN
     hipar = NOMpars(2)+NOMpars(3)+1+OMsteep
	 DO 243 Iflt=1,Nflt
	  hipar = hipar + OMNselpars(Iflt)
	  IF (OMdoreten(Iflt).EQ.1) THEN
 	   lopar = hipar+1
	   hipar = hipar+2
	   TempX(1:2) = OpModPars(lopar:hipar)
!	   WRITE(99,*) Iflt
!	   WRITE(99,*) TempX(1:2)	  
	   RetLen(Iflt,1:Nlen,Yr1) = 1.d0/(1.d0+EXP(-1.d0*(MidLens(1:Nlen)-TempX(1))/TempX(2)))	  
	   DO 244 Iyr = Yr1+1, Yr2
	    Retlen(Iflt,1:Nlen,Iyr) = Retlen(Iflt,1:Nlen,Iyr-1)
244	   CONTINUE
	  ENDIF
243  CONTINUE
	ENDIF

	!print values to file
	IF (Diag.EQ.1) THEN
	 DO 242 Iflt=1,Nflt
	  DO 242 Iyr=Yr1,Yr2
	   WRITE(98,'(I2,1x,I4,1x,100(F6.4,1x))') Iflt,Iyr,(RetLen(Iflt,Ilen,Iyr),Ilen=1,Nlen)
242	 CONTINUE
	ENDIF


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the time series of retention parameters
!	for use in 1.12
	SUBROUTINE GetRetGamma(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,Igam,Iyr,Jyr,Yr1,Yr2

	IF (Diag.EQ.1) WRITE(98,'(A16)') 'Retention Parameters'
	IF (Diag.EQ.1) WRITE(98,'(A6,1x,100(I6,1x))') 'fl IG ',(Iyr,Iyr=Yr1,Yr2)

	Iyr=Yr1+1
	DO 250 Iflt=1,Nflt
	 DO 250 Igam=1,Retflag(Iflt+1)
	  RetGamma(Iflt,Igam,Iyr:Yr2)=RetGamma(Iflt,Igam,Fyear)
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,I1,1x,200(F6.3,1x))') Iflt,Igam,(RetGamma(Iflt,Igam,Jyr),Jyr=Yr1,Yr2)
250	CONTINUE	 

	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.1.4 GROWTH
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for Length At Age
!	equations 1.14, 1.15
!	currently no time-varying growth
	SUBROUTINE GetLenAge(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER II,Istk,Sex,Ilen,Age,Iyr,Yr1,Yr2,ipar
	REAL*8 TempX(1:1000),LA1,Kuse,Linf,t0use,Tempwt(1:1000),TempLen(1:1000)

	IF (Diag.EQ.1) WRITE(98,'(A20)') 'Mean Length at Age'
	IF (Diag.EQ.1) WRITE(98,'(A12,1x,100(I5,1x))') 'Yr Stk Sx',(Age,Age=0,MaxAge)
	
!	Get Mean L@A in first year if parameters
!	Equation 1.14
	IF (LAflag.EQ.2) THEN
	 DO 20 Istk=1,Nstk
	  DO 20 Sex=1,2
	   DO 20 Age=0,MaxAge
	   MeanLenAge(Istk,Sex,Age,Fyear)= VBLinf(istk,Sex,Fyear)*(1.d0-EXP(-1.d0*VBK(istk,Sex,Fyear)*(Age+0.5d0-VBTzero(istk,Sex,Fyear))))
	   LenStart(Istk,Sex,Age,Fyear)= VBLinf(istk,Sex,Fyear)*(1.d0-EXP(-1.d0*VBK(istk,Sex,Fyear)*(Age-VBTzero(istk,Sex,Fyear))))
20   CONTINUE
	ENDIF
    IF (UseMCMC.EQ.0) THEN
	 DO 29 Istk=1,Nstk
	  DO 29 Sex=1,2
	   DO 29 Age=0,MaxAge
	   LenStart(Istk,Sex,Age,Fyear)= VBLinf(istk,Sex,Fyear)*(1.d0-EXP(-1.d0*VBK(istk,Sex,Fyear)*(Age-VBTzero(istk,Sex,Fyear))))
	   !WRITE(*,*) LenStart(Istk,Sex,Age,Fyear)
29   CONTINUE
	ENDIF


	IF (UseMCMC.EQ.1) THEN
     TempX(1:NOMpars(2)) = OpModPars(1:NOMpars(2))
	 !WRITE(*,*) TempX(1:NOMpars(2))
	 DO 21 Istk=1,Nstk
	  !DO 21 Sex=1,2
	   Sex=1
	   ipar = 0
	   IF (OMgrowth(1).EQ.1) ipar=ipar+1
	   IF (OMgrowth(3).EQ.1) THEN
	    ipar=ipar+1
		LA1 = TempX(ipar)
	   ENDIF
	   IF (OMgrowth(4).EQ.1) THEN		
	    ipar=ipar+1
		Linf = TempX(ipar)
	   ELSE 
	    Linf = VBLinf(istk,Sex,Fyear)
	   ENDIF
	   IF (OMgrowth(5).EQ.1) THEN
	    ipar=ipar+1
		Kuse = TempX(ipar)
	   ELSE
	    Kuse = VBK(Istk,Sex,Fyear)
	   ENDIF
	   IF (DUMcol.GT.1.AND.OMgrowth(4).EQ.1) Linf = LA1+(Linf-LA1)/(1.d0-EXP(-Kuse*(DUMcol-OMAgeL1)))  !added 16th May 2011 to allow for non Linf in L2 in MCMC file (cf morwong)
	   t0use = -1.d0*((-1.d0*LOG(1.d0-(LA1/Linf))/Kuse)-OMAgeL1)
	   !TempX(1) = -1.d0*((-1.d0*LOG(1.d0-(TempX(1)/VBLinf(istk,Sex,Fyear)))/TempX(2))-OMAgeL1)
	   DO 22 Age=0,MaxAge
	    !MeanLenAge(Istk,Sex,Age,Fyear)= VBLinf(istk,Sex,Fyear)*(1.d0-EXP(-1.d0*TempX(2)*(Age-TempX(1))))      
	    !MeanLenAge(Istk,Sex,Age,Fyear)= VBLinf(istk,Sex,Fyear)*(1.d0-EXP(-1.d0*TempX(2)*(Age+0.5d0-TempX(1))))
		MeanLenAge(Istk,Sex,Age,Fyear)= Linf*(1.d0-EXP(-1.d0*Kuse*(Age+0.5d0-t0use)))
		!LenStart(Istk,Sex,Age,Fyear)= VBLinf(istk,Sex,Fyear)*(1.d0-EXP(-1.d0*TempX(2)*(Age-TempX(1))))
		LenStart(Istk,Sex,Age,Fyear)= Linf*(1.d0-EXP(-1.d0*Kuse*(Age-t0use)))
	    IF ((Age+0.5d0).LT.OMAgeL1) THEN
		 MeanLenAge(Istk,Sex,Age,Fyear) = lolenbin(1)+((Age+0.5d0)/OMAgeL1)*(Linf*(1.d0-EXP(-1.d0*Kuse*(OMAgeL1-t0use)))-lolenbin(1))      
		 LenStart(Istk,Sex,Age,Fyear) = lolenbin(1)+((Age)/OMAgeL1)*(Linf*(1.d0-EXP(-1.d0*Kuse*(OMAgeL1-t0use)))-lolenbin(1))      
		ENDIF
		IF (MeanLenAge(Istk,Sex,Age,Fyear).LT.lolenbin(1)) MeanLenAge(Istk,Sex,Age,Fyear) = lolenbin(1)
	    IF (LenStart(Istk,Sex,Age,Fyear).LT.lolenbin(1)) LenStart(Istk,Sex,Age,Fyear) = lolenbin(1)
		IF (MeanLenAge(Istk,Sex,Age,Fyear).LE.0) MeanLenAge(Istk,Sex,Age,Fyear) = 0.01d0
		IF (LenStart(Istk,Sex,Age,Fyear).LE.0) LenStart(Istk,Sex,Age,Fyear) = 0.01d0
22     CONTINUE
	   !adjustment for plus group
	   !IF (Nreg.EQ.1) THEN
	   TempWt = 0.d0
	   TempLen = 0.d0
	   II=0
	   DO 23 Age=MaxAge,(2*MaxAge)
		II=II+1
		TempWt(II) = EXP(-0.2*Age)
		TempLen(II) = Linf*(1.d0-EXP(-1.d0*Kuse*(Age+0.5d0-t0use)))      
23	   CONTINUE
	   TempWt(1:II) = TempWt(1:II)/SUM(TempWt(1:II))
	   MeanLenAge(Istk,Sex,MaxAge,Fyear) = SUM(TempWt(1:II)*TempLen(1:II))
	   !MeanLenAge(Istk,2,MaxAge,Fyear) = MeanLenAge(Istk,1,MaxAge,Fyear)
	   TempWt = 0.d0
	   TempLen = 0.d0
	   II=0
	   DO 24 Age=MaxAge,(2*MaxAge)
		II=II+1
		TempWt(II) = EXP(-0.2*Age)
		TempLen(II) = Linf*(1.d0-EXP(-1.d0*Kuse*(Age-t0use)))      
24	   CONTINUE
	   TempWt(1:II) = TempWt(1:II)/SUM(TempWt(1:II))
	   LenStart(Istk,Sex,MaxAge,Fyear) = SUM(TempWt(1:II)*TempLen(1:II))
	   !MeanLenAge(Istk,2,MaxAge,Fyear) = MeanLenAge(Istk,1,MaxAge,Fyear)
	   !ENDIF
	   Sex=2
	   IF (OMNsex.EQ.1) THEN
	    MeanLenAge(Istk,2,0:MaxAge,Fyear) = MeanLenAge(Istk,1,0:MaxAge,Fyear)
		LenStart(Istk,2,0:MaxAge,Fyear) = LenStart(Istk,1,0:MaxAge,Fyear)
	   ELSE
	    ipar = ipar + SUM(OMgrowth(6:9))
		!WRITE(*,*) ipar,SUM(OMgrowth(6:9))
	    IF (OMgrowth(10).EQ.1) THEN
		 ipar = ipar+1
		 LA1 = LA1*EXP(TempX(ipar))
		ENDIF
		IF (OMgrowth(11).EQ.1) THEN
		 ipar = ipar+1
		 Linf = Linf*EXP(TempX(ipar))
		ELSE
		 Linf = VBLinf(istk,Sex,Fyear)
		ENDIF
 	    IF (OMgrowth(12).EQ.1) THEN
	     ipar=ipar+1
		 Kuse = Kuse*EXP(TempX(ipar))
	    ELSE
	     Kuse = VBK(Istk,Sex,Fyear)
	    ENDIF
	    IF (OMgrowth(10).EQ.1) THEN
		 t0use = -1.d0*((-1.d0*LOG(1.d0-(LA1/Linf))/Kuse)-OMAgeL1)
		ELSE
		 t0use = VBTzero(Istk,Sex,Fyear)
		ENDIF
		!WRITE(*,*) Linf,Kuse,t0use,LA1
	   !TempX(4) = OpModPars(1)*EXP(TempX(4))
	   !TempX(5) = VBLinf(istk,1,Fyear)*EXP(TempX(5))
	   !TempX(6) = TempX(2)*EXP(TempX(6))
	   !TempX(4) = -1.d0*((-1.d0*LOG(1.d0-(TempX(4)/TempX(5)))/TempX(6))-OMAgeL1)
 	   DO 26 Age=0,MaxAge
	    !MeanLenAge(Istk,Sex,Age,Fyear)= TempX(5)*(1.d0-EXP(-1.d0*TempX(6)*(Age-TempX(4))))
	    !MeanLenAge(Istk,Sex,Age,Fyear)= TempX(5)*(1.d0-EXP(-1.d0*TempX(6)*(Age+0.5d0-TempX(4))))
		!LenStart(Istk,Sex,Age,Fyear)= TempX(5)*(1.d0-EXP(-1.d0*TempX(6)*(Age-TempX(4))))
		MeanLenAge(Istk,Sex,Age,Fyear)= Linf*(1.d0-EXP(-1.d0*Kuse*(Age+0.5d0-t0use)))
		LenStart(Istk,Sex,Age,Fyear)= Linf*(1.d0-EXP(-1.d0*Kuse*(Age-t0use)))
		IF ((Age+0.5d0).LT.OMAgeL1) THEN
		 MeanLenAge(Istk,Sex,Age,Fyear) = lolenbin(1)+((Age+0.5d0)/OMAgeL1)*(Linf*(1.d0-EXP(-1.d0*Kuse*(OMAgeL1-t0use)))-lolenbin(1))      
		 LenStart(Istk,Sex,Age,Fyear) = lolenbin(1)+((Age)/OMAgeL1)*(Linf*(1.d0-EXP(-1.d0*Kuse*(OMAgeL1-t0use)))-lolenbin(1))      
		ENDIF
		IF (MeanLenAge(Istk,Sex,Age,Fyear).LT.lolenbin(1)) MeanLenAge(Istk,Sex,Age,Fyear) = lolenbin(1)
	    IF (LenStart(Istk,Sex,Age,Fyear).LT.lolenbin(1)) LenStart(Istk,Sex,Age,Fyear) = lolenbin(1)
        IF (MeanLenAge(Istk,Sex,Age,Fyear).LE.0) MeanLenAge(Istk,Sex,Age,Fyear) = 0.01d0
		IF (LenStart(Istk,Sex,Age,Fyear).LE.0) LenStart(Istk,Sex,Age,Fyear) = 0.01d0
26     CONTINUE 
		!IF (Nreg.EQ.1) THEN
	   !adjustment for plus group
	   TempWt = 0.d0
	   TempLen = 0.d0
	   II=0
	   DO 27 Age=MaxAge,(2*MaxAge)
		II=II+1
		TempWt(II) = EXP(-0.2*Age)
		TempLen(II) = Linf*(1.d0-EXP(-1.d0*Kuse*(Age+0.5d0-t0use)))      
27	   CONTINUE
	   TempWt(1:II) = TempWt(1:II)/SUM(TempWt(1:II))
	   MeanLenAge(Istk,Sex,MaxAge,Fyear) = SUM(TempWt(1:II)*TempLen(1:II))
	   !MeanLenAge(Istk,2,MaxAge,Fyear) = MeanLenAge(Istk,1,MaxAge,Fyear)
	   TempWt = 0.d0
	   TempLen = 0.d0
	   II=0
	   DO 28 Age=MaxAge,(2*MaxAge)
		II=II+1
		TempWt(II) = EXP(-0.2*Age)
		TempLen(II) = Linf*(1.d0-EXP(-1.d0*Kuse*(Age-t0use)))      
28	   CONTINUE
	   TempWt(1:II) = TempWt(1:II)/SUM(TempWt(1:II))
	   LenStart(Istk,Sex,MaxAge,Fyear) = SUM(TempWt(1:II)*TempLen(1:II))
       !ENDIF
       ENDIF

21	   CONTINUE


!	   ENDIF
	ENDIF


!	MeanL@Age constant through time
	DO 25 Istk=1,Nstk
	 DO 25 Sex=1,2	 
 	  IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Yr1,Istk,Sex,(MeanLenAge(Istk,Sex,Age,Fyear),Age=0,MaxAge) 
	  DO 25 Iyr=Yr1+1,Yr2
 	   MeanLenAge(Istk,Sex,0:MaxAge,Iyr) = MeanLenAge(Istk,Sex,0:MaxAge,Iyr-1)
	   LenStart(Istk,Sex,0:MaxAge,Iyr) = LenStart(Istk,Sex,0:MaxAge,Iyr-1)
  	   IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Iyr,Istk,Sex,(MeanLenAge(Istk,Sex,Age,Iyr),Age=0,MaxAge) 
25	CONTINUE


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for the SD of length At Age
!	would be 1.17 , currently just input directly
	SUBROUTINE GetSDLenAge(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER II,Istk,Sex,Ilen,Age,Iyr,Yr1,Yr2,ipar
    REAL*8 TempX(1:2)

	IF (Diag.EQ.1) WRITE(98,'(A40)') 'standard deviation of Length at Age'
	IF (Diag.EQ.1) WRITE(98,'(A12,1x,100(I5,1x))') 'Yr Stk Sx',(Age,Age=0,MaxAge)
	
!	Get SD of L@A in first year if parameters
!	Equation 1.16
	IF (SLAflag.EQ.2) THEN
	 DO 30 Istk=1,Nstk
	  DO 30 Sex=1,2
	   DO 30 Age=0,MaxAge
	   SigmaLenAge(Istk,Sex,Age,Fyear)= MeanLenAge(istk,Sex,Age,Fyear)*(CVlen0(istk,Sex)+(Age/MaxAge)*(CVlenX(Istk,Sex)-CVlen0(Istk,Sex)))
	   SigmaStart(Istk,Sex,Age,Fyear)= LenStart(istk,Sex,Age,Fyear)*(CVlen0(istk,Sex)+(Age/MaxAge)*(CVlenX(Istk,Sex)-CVlen0(Istk,Sex)))
30   CONTINUE
	ENDIF

	IF (UseMCMC.EQ.0) THEN
	 DO 36 Istk=1,Nstk
	  DO 36 Sex=1,2
	   DO 36 Age=0,MaxAge
	   !SigmaLenAge(Istk,Sex,Age,Fyear)= MeanLenAge(istk,Sex,Age,Fyear)*(CVlen0(istk,Sex)+(Age/MaxAge)*(CVlenX(Istk,Sex)-CVlen0(Istk,Sex)))
	   SigmaStart(Istk,Sex,Age,Fyear)= LenStart(istk,Sex,Age,Fyear)*(CVlen0(istk,Sex)+(Age/MaxAge)*(CVlenX(Istk,Sex)-CVlen0(Istk,Sex)))
36   CONTINUE
	ENDIF



	IF (UseMCMC.EQ.1) THEN
	 !*************HERE 02May2011***************
	 ipar = SUM(OMgrowth(1:5))
	 IF (OMgrowth(6).EQ.1) THEN
	  ipar = ipar +1
	  TempX(1:2) = OpModPars(ipar)
	 ENDIF
	 IF (OMNsex.EQ.2) THEN
	  ipar = ipar + SUM(OMgrowth(7:12)) + 1
	  !WRITE(*,*) ipar, OpModPars(ipar)
	  IF (OMgrowth(13).EQ.1) THEN
	   TempX(2) = TempX(1)*EXP(OpModPars(ipar)) 
	  ELSE
	   TempX(2) = TempX(1)
	  ENDIF
	 ENDIF
	 !TempX(1) = OpModPars(3)
     !TempX(2) = OpModPars(3)*EXP(OpModPars(7))
	 DO 31 Istk=1,Nstk
	  DO 31 Sex=1,2
	   DO 31 Age=0,MaxAge
	   SigmaLenAge(Istk,Sex,Age,Fyear)= MeanLenAge(istk,Sex,Age,Fyear)*TempX(Sex)
	   SigmaStart(Istk,Sex,Age,Fyear)= LenStart(istk,Sex,Age,Fyear)*TempX(Sex)
31   CONTINUE
	ENDIF

!	SDL@Age constant through time
	DO 35 Istk=1,Nstk
	 DO 35 Sex=1,2	 
 	  IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Yr1,Istk,Sex,(SigmaLenAge(Istk,Sex,Age,Fyear),Age=0,MaxAge) 
	  DO 35 Iyr=Yr1+1,Yr2
 	   SigmaLenAge(Istk,Sex,0:MaxAge,Iyr) = SigmaLenAge(Istk,Sex,0:MaxAge,Iyr-1)
	   SigmaStart(Istk,Sex,0:MaxAge,Iyr) = SigmaStart(Istk,Sex,0:MaxAge,Iyr-1)
  	   IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Iyr,Istk,Sex,(SigmaLenAge(Istk,Sex,Age,Iyr),Age=0,MaxAge) 
35	CONTINUE


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine calculates the weight at length
!	equation 1.16
!	
	SUBROUTINE Get_Wtlen

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Ilen,Istk,Sex
	REAL*8 Lengths(Nlen)

	Lengths(1:Nlen) = lolenbin(1:Nlen) + (hilenbin(1:Nlen)-lolenbin(1:Nlen))/2.d0
	
	IF (Diag.EQ.1) WRITE(98,*) 'Weight at length'

	DO 650 Istk=1,Nstk
	 DO 650 Sex=1,2
	  DO 651 Ilen=1,Nlen
	   Wtlen(Ilen,Istk,Sex) = WtLenPars(1,Istk,Sex)*(Lengths(Ilen)**WtLenPars(2,Istk,Sex))
651	  CONTINUE
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,I1,1x,200(F8.4,1x))') Istk,Sex,(WtLen(Ilen,Istk,Sex),Ilen=1,Nlen)
650	CONTINUE

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for the Weight At Age
	SUBROUTINE GetWtAge(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER II,Istk,Sex,Ilen,Age,Iyr,Yr1,Yr2

	IF (Diag.EQ.1) WRITE(98,'(A20)') 'Weight at Age'
	IF (Diag.EQ.1) WRITE(98,'(A12,1x,100(I5,1x))') 'Yr Stk Sx',(Age,Age=0,MaxAge)
	
!	Get SD of L@A in first year if parameters
!	Equation 1.15
	IF (WAflag.EQ.2.OR.UseMCMC.EQ.1) THEN
	 DO 40 Istk=1,Nstk
	  DO 40 Sex=1,2
	   Weight(Istk,Sex,0:MaxAge,Fyear)= WtLenPars(1,Istk,Sex)*(MeanLenAge(istk,Sex,0:MaxAge,Fyear)**WtLenPars(2,Istk,Sex))
	   !DO 40 Age=0,MaxAge
	   ! Weight(Istk,Sex,Age,Fyear) = 0.d0
	   !DO 40 Ilen=1,Nlen
	    !Weight(Istk,Sex,Age,Fyear) = Weight(Istk,Sex,Age,Fyear) + WtLen(Ilen,Istk,Sex)*Fraclen(Ilen,Istk,Sex,Age,Fyear)
40   CONTINUE
	ENDIF

!	Weight@Age updated through time (if input directly, no change through time - if pars, potential to change with changes in mean length @ Age)
	DO 45 Istk=1,Nstk
	 DO 45 Sex=1,2	 
 	  IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Yr1,Istk,Sex,(Weight(Istk,Sex,Age,Yr1),Age=0,MaxAge) 
 	  IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Yr1,Istk,Sex,(WeightMid(Istk,Sex,Age,Yr1),Age=0,MaxAge) 
	  DO 45 Iyr=Yr1+1,Yr2
       IF (WAflag.EQ.1) Weight(Istk,Sex,0:MaxAge,Iyr) = Weight(Istk,Sex,0:MaxAge,Iyr-1)
	   IF (WAflag.EQ.1) WeightMid(Istk,Sex,0:MaxAge,Iyr) = WeightMid(Istk,Sex,0:MaxAge,Iyr-1)
       IF (WAflag.EQ.2) Weight (Istk,Sex,0:MaxAge,Iyr)= WtLenPars(1,Istk,Sex)*(MeanLenAge(istk,Sex,0:MaxAge,Iyr)**WtLenPars(2,Istk,Sex))
  	   IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Iyr,Istk,Sex,(Weight(Istk,Sex,Age,Iyr),Age=0,MaxAge) 
	   IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Iyr,Istk,Sex,(WeightMid(Istk,Sex,Age,Iyr),Age=0,MaxAge) 
45	CONTINUE

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.1.5 RECRUITMENT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine gets the annual recruits, by stock
!	Equations 1.18 and 1.19
	SUBROUTINE Get_Recruits(Istk,Iyr)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,Ireg,Iyr,Sex,Age,Istk
	REAL*8 ExpRecs(0:Nreg)

	
!	Get lamda for this year and stock (FracRec)
	FracRec(Istk,1:Nreg,Iyr) = SpawBio(Istk,1:Nreg,Iyr)/SpawBio(Istk,0,Iyr)

    !IF (Iyr.EQ.2007) 	WRITE(99,*) Iyr,Rzero(Istk),SBioZero(Istk),FracRec(Istk,1:Nreg,Iyr)

!	Expected recruits given SRR	
!	equation 1.18
!	total
	ExpRecs(0) = 4.d0*h(Istk)*Rzero(Istk)*SpawBio(Istk,0,Iyr)/((SBioZero(Istk)*(1.d0-h(Istk)))+(SpawBio(Istk,0,Iyr)*(5.d0*h(Istk)-1.d0)))
!	by region
	ExpRecs(1:Nreg) = FracRec(Istk,1:Nreg,Iyr)*ExpRecs(0)
	

!	add devs to get #'s of Recruits
	DO 690 Ireg=1,Nreg
	 Recruits(Istk,Ireg,Iyr) = ExpRecs(Ireg)
	 IF (Iyr.NE.Fyear) Recruits(Istk,Ireg,Iyr) = ExpRecs(Ireg)*EXP(RecDevs(Ireg,Iyr)-0.5d0*(SigmaR(Iyr)**2.d0))
	 !IF (UseMCMC.EQ.1.AND.Iyr.LT.1975) Recruits(Istk,Ireg,Iyr) = ExpRecs(Ireg) !*EXP(RecDevs(Ireg,Iyr)-0.5d0*(SigmaR(Iyr)**2.d0))
690 CONTINUE

!	IF (Iyr.EQ.2007) THEN
!	 WRITE(99,*) 'recruits'
!	 WRITE(99,*) SpawBio(Istk,1,Iyr),ExpRecs(1:Nreg),Recruits(Istk,1:Nreg,Iyr)
!	ENDIF


	RETURN

	END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the time series of recruitment deviations
!	Equations 1.20, 1.21, 1.22
	SUBROUTINE GetRecDevs(Yr1,Yr2)
     ! USE Numerical_Libraries

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iyr,Yr1,Yr2,Ireg,Jreg,ISEED,Frecest,Lrecest,II
	REAL*8 RanNum,MEANS(Nreg),SG(Nreg),TT(1:Nreg,1:Nreg),Devs(Nreg),XTEMP
	REAL*4 RAN5
	REAL*8 XNORM

	EXTERNAL RAN5,XNORM

	ISEED = ISEEDZ

!	Get the time series of the variance covariance matrix & sigma R's
	DO 260 Iyr=Yr1,Yr2
	 !Equation 1.22
	 SigmaR(Iyr)= SigmaRVals(1)
	 RanNum = RAN5(ISEEDZ)
	 IF (RanNum.GT.SigmaRProbs(1)) SigmaR(Iyr)=SigmaRVals(2)     
	 !Variance-covariance matrix of rec devs, Equation 1.21
	 VarRecDevs(1:Nreg,1:Nreg,Iyr) = SigmaR(Iyr)*SigmaR(Iyr)*CorRecDevs(1:Nreg,1:Nreg)
260	CONTINUE

!	print to file
	IF (Diag.EQ.1) THEN
	 WRITE(98,*) 'Sigma R values'	
	 WRITE(98,'(100(I5,1x))') (Iyr,Iyr=Yr1,Yr2)
	 WRITE(98,'(100(F5.3,1x))') (SigmaR(Iyr),Iyr=Yr1,Yr2)
     WRITE(98,*) 'Variance covariance matrix for rec devs'
	 WRITE(98,'(A8,100(I6,1x))') ' Yr  Reg',(Ireg,Ireg=1,Nreg)
	 DO 261 Iyr=Yr1,Yr2
	  DO 261 Ireg=1,Nreg	
       WRITE(98,'(I4,1x,I2,1x,100(F6.4,1x))') Iyr,Ireg,(VarRecDevs(Ireg,Jreg,Iyr),Jreg=1,Nreg)
261	 CONTINUE
	ENDIF

	IF (RecDevFlag(1).EQ.0.OR.Yr1.GT.Lyear) THEN 
	 IF (Yr1.GT.Lyear.AND.RecDevFlag(1).EQ.1.AND.RecDevFlag(3).GE.Yr1) GOTO 264
!	 Now get the deviations 
 	 MEANS=0.d0
	 DO 262 Iyr=Yr1,Yr2
	  SG = SigmaR(Iyr)
	  TT = CorRecDevs(1:Nreg,1:Nreg)
      !WRITE(99,*) TT(1:Nreg,1:Nreg)
	  Devs=0.d0
      CALL GenMult(Devs,MEANS,ISEED,Nreg,TT,SG,Nreg)
	  RecDevs(1:Nreg,Iyr) = Devs
	  IF (SUM(CorRecDevs(1:Nreg,1:Nreg)).EQ.(Nreg**2.d0)) THEN
	   Devs=0.d0
	   CALL GenMult(XTEMP,0.d0,ISEEDZ,1,1.d0,SG,1)
	   DO Ireg=1,Nreg
	    RecDevs(Ireg,Iyr) = XTEMP
		RecDevs(Ireg,Iyr) = XNORM(5,0.d0,SG,ISEEDZ)
	   ENDDO
	  ENDIF
262	 CONTINUE
	ENDIF

264 CONTINUE

    IF (UseMCMC.EQ.1.AND.Yr1.LT.Lyear) THEN
	 Frecest = OMrecFY !1985   !need to automate the read-in of this, so that it can change
	 Lrecest = OMrecLY !2003
	 II=0
     II = NOMpars(2) + Nreg + OMsteep
	 IF (Nreg.GT.1) II = II + Nreg
     DO 265 Iyr=Frecest,Lrecest
	  II=II+1
!	  RecDevs(1:Nreg,Iyr) = OpModPars(11+II)
	  RecDevs(1:Nreg,Iyr) = OpModPars(II)
265  CONTINUE
    ENDIF
     

!	print to file
	IF (Diag.EQ.1) THEN
	 WRITE(98,*) 'Recruitment deviations'
	 WRITE(98,'(A4,100(I7,1x))') 'Reg ',(Iyr,Iyr=Yr1,Yr2)
	 DO 263 Ireg=1,Nreg
	  WRITE(98,'(I3,1x,100(F7.4,1x))') Ireg,(RecDevs(Ireg,Iyr),Iyr=Yr1,Yr2)
263	 CONTINUE
	ENDIF
 
	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine calculates the spawning biomass for a given year
!	Equation 1.24
	SUBROUTINE GetSpawBio(Year)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Istk,Ireg,Year,Ilen

	SpawBio(1:Nstk,0:Nreg,Year)=0.d0
	DO 630 IStk=1,Nstk
	 DO 631 Ireg=1,Nreg
	  DO 631 Age=1,MaxAge
	   IF (Diag.EQ.1) WRITE(98,*) N(Istk,Ireg,1,Age,Year),Fecundity(Istk,Age,Year),Weight(Istk,1,Age,Year)
	   !DO 631 Ilen=1,Nlen
       !SpawBio(Istk,Ireg,Year)=SpawBio(Istk,Ireg,Year)+N(Istk,Ireg,1,Age,Year)*Fraclen(Ilen,Istk,1,Age,Year)*Fecundity(Istk,Ilen,Year)*WtLen(Ilen,Istk,1)
	   SpawBio(Istk,Ireg,Year)=SpawBio(Istk,Ireg,Year)+N(Istk,Ireg,1,Age,Year)*Fecundity(Istk,Age,Year)   !*Weight(Istk,1,Age,Year)
631	 CONTINUE
     SpawBio(Istk,0,Year) = SUM(SpawBio(Istk,1:Nreg,Year))
	 WRITE(92,'(I4,1x,I2,1x,100(F20.4,1x))') Year,IStk,(SpawBio(Istk,Ireg,Year),Ireg=0,Nreg)
630	CONTINUE


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for the Fecundity at age
	SUBROUTINE GetFecund(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER II,Istk,Sex,Ilen,Age,Iyr,Yr1,Yr2

	IF (Diag.EQ.1) WRITE(98,'(A20)') 'Fecundity at Age'
	IF (Diag.EQ.1) WRITE(98,'(A10,1x,100(I5,1x))') 'Yr Stk ',(Age,Age=0,MaxAge)
	
	DO Istk=1,Nstk
	 DO Age=0,MaxAge
	  Fecundity(Istk,Age,Yr1) = 0.d0
      DO Ilen=1,Nlen
	   !WRITE(99,'(I2,1x,1000(F8.4,1x))') Age,Fecundity(Istk,Age,Yr1),Maturity(Istk,Ilen,Yr1),Fraclen(Ilen,Istk,1,Age,Yr1),WtLen(Ilen,Istk,1)
	   Fecundity(Istk,Age,Yr1) = Fecundity(Istk,Age,Yr1) + FraclenStart(Ilen,Istk,1,Age,Yr1)*Maturity(Istk,Ilen,Fyear)*WtLen(Ilen,Istk,1)
	  ENDDO
     ENDDO
	ENDDO

!	Fecundity at age constant through time
	DO 50 Istk=1,Nstk
 	 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,100(F6.2,1x))') Yr1,Istk,(Fecundity(Istk,Age,Yr1),Age=0,MaxAge) 
! 	 IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,100(F6.2,1x))') Yr1,Istk,(Fecundity(Istk,Age,Yr1),Age=1,Nlen) 
	 DO 50 Iyr=Yr1+1,Yr2+1
	  Fecundity(Istk,0:MaxAge,Iyr) = Fecundity(Istk,0:MaxAge,Iyr-1) 
 	   IF (Diag.EQ.1.AND.Yr1.EQ.Fyear) WRITE(98,'(I4,1x,I2,1x,100(F6.2,1x))') Iyr,Istk,(Fecundity(Istk,Age,Iyr),Age=0,MaxAge) 
!	  Fecundity(Istk,0:Nlen,Iyr) = Fecundity(Istk,0:Nlen,Iyr-1) 
! 	   IF (Diag.EQ.1) WRITE(98,'(I4,1x,I2,1x,100(F6.2,1x))') Iyr,Istk,(Fecundity(Istk,Age,Iyr),Age=1,Nlen) 
50	CONTINUE

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.1.6 MOVEMENT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine gets the values for the Movement Matrix (X)
!	Section 1.1.6
!	equations 1.25, 1.26, 1.27, 1.28
!	equations Not written yet, currently set to no movement
!	(although have tested with simple movement assumptions)
!
	SUBROUTINE GetX(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER II,Istk,Sex,Age,Iyr,Ireg,Jreg,Yr1,Yr2
	REAL*8 Xbar(Nstk,Nreg,Nreg,1:2,0:MaxAge),propmove,TempX(1:100)
	
	IF (Diag.EQ.1) WRITE(98,'(A20)') 'Movement Matrix Xbar'
	IF (Diag.EQ.1) WRITE(98,'(A16,1x,100(I5,1x))') 'Yr Stk Sx A R',(Ireg,Ireg=1,Nreg)

        Xbar =0.d0
	DO 60 Istk=1,Nstk
	 DO 60 Sex=1,2
	    !Relmove(Istk,Sex,0:MaxAge,Fyear)=Relmove(Istk,Sex,0:MaxAge,Fyear)/MAX(Relmove(Istk,Sex,0:MaxAge,Fyear))
	    II=0
		DO 63 Ireg=1,Nreg
	     DO 62 Jreg=Ireg+1,Nreg
		   II=II+1
	       Xbar(Istk,Ireg,Jreg,Sex,0:MaxAge) = Maxmovers(Istk,II)*2*LamdaZero(Istk,Jreg)/(LamdaZero(Istk,Ireg)+LamdaZero(Istk,Jreg))
           Xbar(Istk,Jreg,Ireg,Sex,0:MaxAge) = Maxmovers(Istk,II)*2*LamdaZero(Istk,Ireg)/(LamdaZero(Istk,Ireg)+LamdaZero(Istk,Jreg))
	       DO 62 Age=0,MaxAge
	        Xbar(Istk,Ireg,Jreg,Sex,Age) = Xbar(Istk,Ireg,Jreg,Sex,Age)*Relmove(Istk,Sex,Age)
            Xbar(Istk,Jreg,Ireg,Sex,Age) = Xbar(Istk,Jreg,Ireg,Sex,Age)*Relmove(Istk,Sex,Age)
62	     CONTINUE
         DO 63 Age=0,Maxage
	      Xbar(Istk,Ireg,Ireg,Sex,Age) = 1.d0 - SUM(Xbar(Istk,Ireg,1:Nreg,Sex,Age))
		  IF (Diag.EQ.1) WRITE(98,'(I2,1x,I2,1x,I3,1x,I2,1x,20(F5.3,1x))') Istk,Sex,Age,Ireg,(Xbar(Istk,Ireg,Jreg,Sex,Age),Jreg=1,Nreg)
63	     CONTINUE
!	    Now get the X's
    IF (Diag.EQ.1) WRITE(98,'(A20)') 'Movement Matrix X'
	IF (Diag.EQ.1) WRITE(98,'(A16,1x,100(I5,1x))') 'Yr Stk Sx A R',(Ireg,Ireg=1,Nreg)
!	    call to Xdevs here
            DO 60 Iyr=Yr1,Yr2
             X(Istk,1:Nreg,1:Nreg,Sex,0:MaxAge,Iyr) = Xbar(Istk,1:Nreg,1:Nreg,Sex,0:MaxAge)
	     DO 60 Age=0,MaxAge
	     DO 60 Ireg=1,Nreg
	     IF (Diag.EQ.1.AND.Iyr.EQ.Yr1) WRITE(98,'(I4,1x,I2,1x,I1,1x,I3,1x,I2,1x,20(F5.3,1x))') Iyr,Istk,Sex,Age,Ireg,(X(Istk,Ireg,Jreg,Sex,Age,Iyr),Jreg=1,Nreg) 
60	CONTINUE		  

	!WRITE(99,*) 'xmid',Yr1

   IF (UseMCMC.EQ.1) THEN
        Xbar =0.d0
	DO 64 Istk=1,Nstk
	 DO 64 Sex=1,2
	    !Relmove(Istk,Sex,0:MaxAge,Fyear)=Relmove(Istk,Sex,0:MaxAge,Fyear)/MAX(Relmove(Istk,Sex,0:MaxAge,Fyear))
	    II=0
		DO 65 Ireg=1,Nreg
		 DO 67 Jreg=1,Nreg
			TempX(Jreg) = 1
			IF (Jreg.NE.Ireg) THEN
			 II=II+1
			 TempX(Jreg) = EXP(OpModPars(II+8))
			ENDIF
67		 CONTINUE
         TempX(1:Nreg) = TempX(1:Nreg)/SUM(TempX(1:Nreg))
		 DO 66 Jreg=1,Nreg
            Xbar(Istk,Ireg,Jreg,Sex,0:MaxAge) = TempX(Jreg) 
			!Xbar(Istk,Ireg,Jreg,Sex,0) = LamdaZero(Istk,Jreg)

!	       Xbar(Istk,Ireg,Jreg,Sex,0:MaxAge) = Maxmovers(Istk,II)*2*LamdaZero(Istk,Jreg)/(LamdaZero(Istk,Ireg)+LamdaZero(Istk,Jreg))
!           Xbar(Istk,Jreg,Ireg,Sex,0:MaxAge) = Maxmovers(Istk,II)*2*LamdaZero(Istk,Ireg)/(LamdaZero(Istk,Ireg)+LamdaZero(Istk,Jreg))
!	       DO 66 Age=0,MaxAge
!	        Xbar(Istk,Ireg,Jreg,Sex,Age) = Xbar(Istk,Ireg,Jreg,Sex,Age)*Relmove(Istk,Sex,Age)
!            Xbar(Istk,Jreg,Ireg,Sex,Age) = Xbar(Istk,Jreg,Ireg,Sex,Age)*Relmove(Istk,Sex,Age)
66	     CONTINUE
         DO 65 Age=0,Maxage
	      Xbar(Istk,Ireg,1:Jreg,Sex,Age) = Xbar(Istk,Ireg,1:Nreg,Sex,Age)/SUM(Xbar(Istk,Ireg,1:Nreg,Sex,Age))
		  IF (Diag.EQ.1) WRITE(98,'(I2,1x,I2,1x,I3,1x,I2,1x,20(F5.3,1x))') Istk,Sex,Age,Ireg,(Xbar(Istk,Ireg,Jreg,Sex,Age),Jreg=1,Nreg)
65	     CONTINUE
!	    Now get the X's
    IF (Diag.EQ.1) WRITE(98,'(A20)') 'Movement Matrix X'
	IF (Diag.EQ.1) WRITE(98,'(A16,1x,100(I5,1x))') 'Yr Stk Sx A R',(Ireg,Ireg=1,Nreg)
!	    call to Xdevs here
	!	WRITE(99,*) 'xmid2',Yr1
            DO 64 Iyr=Yr1,Yr2
             X(Istk,1:Nreg,1:Nreg,Sex,0:MaxAge,Iyr) = Xbar(Istk,1:Nreg,1:Nreg,Sex,0:MaxAge)
	     IF (Diag.EQ.1.AND.Iyr.EQ.Yr1) THEN 
		  DO 68 Age=0,MaxAge
	      DO 68 Ireg=1,Nreg
		   WRITE(98,'(I4,1x,I2,1x,I1,1x,I3,1x,I2,1x,20(F5.3,1x))') Iyr,Istk,Sex,Age,Ireg,(X(Istk,Ireg,Jreg,Sex,Age,Iyr),Jreg=1,Nreg) 
68	      CONTINUE
		 ENDIF
64	CONTINUE		  
	!WRITE(99,*) 'xe1',Yr1
   ENDIF
	!WRITE(99,*) 'xe2',Yr1
	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	1.1.7 INITIAL CONDITIONS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine solves for the initial conditions given R0 and initial lamdas
!	Section 1.1.7
!	Equations 1.29,1.30,1.31
!
	SUBROUTINE GetInit

	!DECEMBER 8th 2010


	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER II,JJ,Age,Sex,Istk,JAge,Ireg,Jreg
	REAL*8 Ntemp(1:Nreg),NproVec(Nreg*(MaxAge+1)),TM(Nreg*(MaxAge+1),Nreg*(MaxAge+1)),NewVec(Nreg*(MaxAge+1))
	REAL*8 TempMat(Nreg*(MaxAge+1),Nreg*(MaxAge+1)),Temp,TempVec(0:MaxAge)


	IF (Diag.EQ.1) THEN
	 WRITE(99,*) 'check Init.junk'
	 OPEN(UNIT=98,FILE='Init.junk')
	 WRITE(98,*) 'Setting up initial population vector'
	ENDIF

	DO 600 Istk=1,Nstk
	 DO 600 Sex=1,2

	  NproVec=0.d0
!	  place initial proportions in matrix form to get equilibrium age structure
	  II=0	  	  
	  DO 605 Ireg=1,Nreg
	    II=(Ireg-1)*(MaxAge+1)+1
		!equation 1.30
		NproVec(II) = 0.5*LamdaZero(Istk,Ireg)
605	  CONTINUE

	  TM=0.d0
	  DO 606 Ireg=1,Nreg
!	   II=(Ireg-1)*(MaxAge)+1
!       TM(II,II) = 1.d0
!	    DO Jreg=1,Nreg
!		 II=(Ireg-1)*(MaxAge)+1
!		 JJ=(Jreg-1)*(MaxAge)+1
!         TM(II,JJ) = X(IStk,Jreg,Ireg,Sex,1,Fyear)
!		ENDDO
!
!	   Equation 1.31
	   DO 606 Age=1,MaxAge
        II=(Ireg-1)*(MaxAge+1)+Age+1
		DO 606 Jreg=1,Nreg
	      JJ=(Jreg-1)*(MaxAge+1)+Age
          TM(II,JJ)= X(IStk,Jreg,Ireg,Sex,Age,Fyear)*EXP(-1.d0*M(IStk,Jreg,Sex,Age-1,Fyear))
          IF (Age.EQ.MaxAge) TM(II,JJ+1)= X(IStk,Jreg,Ireg,Sex,Age,Fyear)*EXP(-1.d0*M(IStk,Jreg,Sex,Age,Fyear))
606   CONTINUE
	  
	  IF (Diag.EQ.1) THEN
	   WRITE(98,*) 'Initial Vector'
	   II=0
	   DO 607 Ireg=1,Nreg
	    DO 607 Age=0,MaxAge
		 II=II+1
		 WRITE(98,'(I3,1x,I2,1x,I3,1x,F6.4)') II,Ireg,Age,NproVec(II)
607	   CONTINUE
	   WRITE(98,*) 'Transition Matrix'
       DO 608 II=1,(Nreg*(MaxAge+1))
	     WRITE(98,'(500(F5.3,1x))') (TM(II,JJ),JJ=1,(Nreg*(MaxAge+1)))
608	   CONTINUE
	  ENDIF

!	  Now get the equilibrium age structure
!	  equation 1.29
	  TempMat = 0.d0
	  DO II=1,(Nreg*(MaxAge+1))
	   TempMat(II,II) = 1.d0
	  ENDDO

	  TempMat = TempMat - TM

	  IF (Diag.EQ.1) THEN
	   WRITE(98,*) 'Transition Matrix'
       DO 618 II=1,(Nreg*(MaxAge+1))
	     WRITE(98,'(500(F5.3,1x))') (TempMat(II,JJ),JJ=1,(Nreg*(MaxAge+1)))
618	   CONTINUE
	  ENDIF

	  !what to do depends on sex (spawning biomass only depends on fems)
!      IF (Sex.EQ.1) THEN
!       CALL GetFemEqRecs(TempMat,Istk)
!	  ELSE
       CALL SolveM(TempMat,NproVec,Nreg*(MaxAge+1),NewVec,Nreg*(MaxAge+1))   !equation 1.29
	   !NewVec = NewVec/SUM(NewVec)
	   IF (Diag.EQ.1) THEN
	    WRITE(98,*) 'Initial Vector - Sex = ',Sex
	    II=0
	    DO 609 Ireg=1,Nreg
	     DO 609 Age=0,MaxAge
		  II=II+1
		  WRITE(98,'(I3,1x,I2,1x,I3,1x,2(F6.4,1x))') II,Ireg,Age,NproVec(II),NewVec(II)
609	    CONTINUE
	   ENDIF
	   !fill in initial age sturcture
	   II=0
	   DO 610 Ireg=1,Nreg
	    DO 610 Age=0,MaxAge
	     II = (Ireg-1)*(MaxAge+1) + Age + 1
 	     N(Istk,Ireg,Sex,Age,Fyear) = NewVec(II)
610	   CONTINUE
!	  ENDIF

600	CONTINUE

!    DO 611 Istk=1,Nstk!
!	 Temp = SUM(N(Istk,1:Nreg,1:2,0,Fyear))
!	 DO 611 Sex=1,2
!	  DO 611 Ireg=1,Nreg
!	  TempVec = N(Istk,Ireg,Sex,0:MaxAge,Fyear)/N(Istk,Ireg,Sex,0,Fyear)     
 !     N(Istk,Ireg,Sex,0,Fyear) = N(Istk,Ireg,Sex,0,Fyear)/Temp
!	  N(Istk,Ireg,Sex,1:MaxAge,Fyear) = N(Istk,Ireg,Sex,0,Fyear)*TempVec(1:MaxAge)
!611 CONTINUE
	 
	!print out initial age structure
	IF (Diag.EQ.1) THEN
	 WRITE(98,*) '*** final initial age structure ****'
	 WRITE(98,'(A8,100(I6,1x))') 'Stk R S ',(Age,Age=0,MaxAge)
	 DO 615 Istk=1,Nstk
	  DO 615 Ireg=1,Nreg
	   DO 615 Sex=1,2
	    WRITE(98,'(I2,1x,I2,1x,I1,1x,100(F6.4,1x))') Istk,Ireg,Sex,(N(Istk,Ireg,Sex,Age,Fyear),Age=0,MaxAge)
615	 CONTINUE
	ENDIF

	WRITE(99,*) N(1,1,1,0,Fyear)

	WRITE(98,*) Rzero(1)

	!Numbers in first year
	DO 616 Istk=1,Nstk
	 N(Istk,1:Nreg,1:2,0:MaxAge,Fyear) = Rzero(Istk)*N(Istk,1:Nreg,1:2,0:MaxAge,Fyear)
	 IF (RecDevFlag(1).EQ.1) THEN
	  DO 619 Sex=1,2
	   IF (SUM(ABS(RecDevs(1:Nreg,Fyear))).NE.0) N(Istk,1:Nreg,Sex,0,Fyear) = N(Istk,1:Nreg,Sex,0,Fyear)*EXP(RecDevs(1:Nreg,Fyear)-0.5*(SigmaR(Fyear)**2.d0))
619	  CONTINUE
     ENDIF
	 IF (Diag.EQ.1) THEN
	  DO 617 Ireg=1,Nreg
	   DO 617 Sex=1,2
	    WRITE(91,'(I4,1x,I2,1x,I2,1x,I1,1x,200(F10.2,1x))') Fyear,Istk,Ireg,Sex,(N(Istk,Ireg,Sex,Age,Fyear),Age=0,MaxAge)
617	  CONTINUE
	 ENDIF
616	CONTINUE

	!get initial relevant quantities
	CALL GetSpawBio(Fyear)
    CALL GetVBio(Fyear)

	!SB Zero
	SBioZero = SpawBio(1:Nstk,0,Fyear)

	CLOSE(98)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	this subroutine gets the numbers at age
	SUBROUTINE GetNumbersAtAge(DiagFlag)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Istk,Ireg,Sex,Age,Iyr,DiagFlag

	!Update for all remaining years of the historical projection
	IF (Diag.EQ.1.AND.DiagFlag.EQ.1) OPEN(UNIT=98,FILE='histproj.junk')
	DO 500 Iyr=Fyear+1,Lyear+1
	 CALL PopUpdate(Iyr)
500	CONTINUE
	CLOSE(98)


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine solves for the female equilibrium age structure given an update matrix
    SUBROUTINE GetFemEqRecs(TranMat,Istk)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	REAL*8 TranMat(Nreg*(MaxAge+1),Nreg*(MaxAge+1)),Vec(Nreg*(MaxAge+1)),NewVec(Nreg*(MaxAge+1))
	REAL*8 Lamdatemp(Nreg),SBeq(Nreg),Diff(Nreg),SS,EqRecFunk,STEP(100),VAR(100)
	INTEGER Istk,II,JJ,Npar,Ireg,KK,Ipar,IFail,Age
	DOUBLE PRECISION XF(Nreg-1)

	EXTERNAL EqRecFunk,FUNCT

	IF (Diag.EQ.1) WRITE(98,*) 'Getting female age structure'

    Npar=Nreg-1
	XF=0.d0
    XF(1:Npar) = LOG(LamdaZero(Istk,1:Npar)/(1-LamdaZero(Istk,1:Npar)))

	IF (Diag.EQ.1) THEN
	 WRITE(98,*) 'Initial XF'
	 WRITE(98,'(100(F6.4,1x))') XF
    ENDIF	

	GrangerI = Istk
	IF (Diag.EQ.1) WRITE(98,'(A5,1x,I2)') 'Istk ',GrangerI
	II = Nreg*(MaxAge+1)
	DO JJ=1,II
	 GrangerM(JJ,1:II) = TranMat(JJ,1:II)
	 IF (Diag.EQ.1) WRITE(98,'(500(F6.4,1x))') (GrangerM(JJ,KK),KK=1,II)
	ENDDO
	

!	CALL FIT(XF,SS,Npar,EqRecFunk)
      DO 901 II = 1,Npar
       Step(II) = 0.01*XF(II)
901   CONTINUE       
!      CALL MINIM(XF,STEP,Npar,SS,5000,-1,0.0000000001d0,1,0,0.9d0,VAR,FUNCT,6,IFAIL)
     !CALL MINIM(XF,STEP,Npar,SS,5000,-1,0.00001d0,5,0,0.9d0,VAR,FUNCT,6,IFAIL)


    SS = EqRecFunk(XF)

	IF (Diag.EQ.1) THEN
	 WRITE(98,'(A25)') 'final vector - females'
	 !WRITE(98,'(A11)') 'Stk Reg Lam'
	 DO 902 Ireg=1,Nreg
	  DO 902 Age=0,MaxAge
	  WRITE(98,'(I2,1x,I2,1x,I4,1x,2(F7.4,1x))') Istk,Ireg,Age,N(Istk,Ireg,1,Age,Fyear)
902  CONTINUE	 
	ENDIF
	  

	RETURN
	END
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This function does the objective function for solving for equilibrium
    REAL*8 FUNCTION EqRecFunk(XF)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	REAL*8 TranMat(Nreg*(MaxAge+1),Nreg*(MaxAge+1)),Vec(Nreg*(MaxAge+1)),NewVec(Nreg*(MaxAge+1))
	REAL*8 Lamdatemp(Nreg),SBeq(Nreg),Diff(Nreg),Lamdacum,Temp,Lamdarem
	REAL*8 XF(Nreg-1),Obj
	INTEGER Npar,II,Istk,Age,Ireg,JJ


	 Npar=Nreg-1

	 Lamdacum=0.d0
	 Lamdarem=1.d0
     DO 1030 II=1,Npar
	  IF (II.EQ.1) THEN
	   Lamdatemp(1)=EXP(XF(1))/(1+EXP(XF(1))) 
	  ELSE
       Lamdatemp(II)=Lamdarem*EXP(XF(II))/(1+EXP(XF(II)))
      ENDIF
	  Lamdarem = 1.d0-Lamdatemp(II)
	  Lamdacum = Lamdacum+Lamdatemp(II)
1030 CONTINUE
     Lamdatemp(Nreg)=Lamdarem
	 
	 IF (Diag.EQ.1) WRITE(98,'(A1,1x,100(F6.4,1x))') '*',(Lamdatemp(Ireg),Ireg=1,Nreg)
!
!	  place proportions in vector form to get equilibrium age structure
	  II=0
	  Vec=0.d0	  	  
	  DO 1040 Ireg=1,Nreg
	    II=(Ireg-1)*(MaxAge+1)+1
		Vec(II) = 0.5*Lamdatemp(Ireg)
1040  CONTINUE

!	 get relevant quantities
	 Istk=GrangerI
	 II=Nreg*(MaxAge+1)
	 TranMat = GrangerM(1:II,1:II)

	 !Solve for age structure given the lamdatemp
	 NewVec=0.d0
     IF (Diag.EQ.1) WRITE(98,'(A5,500(F6.4,1x))') 'Vec  ',Vec
     CALL SolveM(TranMat,Vec,Nreg*(MaxAge+1),NewVec,Nreg*(MaxAge+1))
     IF (Diag.EQ.1) WRITE(98,'(A5,500(F6.4,1x))') 'NVec ',NewVec

	 !Calculate spawning biomass
	 SBeq=0.d0
	 IF (Diag.EQ.1) WRITE(98,*) 'spawning biomass'
     IF (Diag.EQ.1) WRITE(98,'(A17)') 'Ireg SBfrac Lamda'
	 DO 1010 Ireg=1,Nreg
	  DO 1010 Age=1,MaxAge
	   JJ = (Ireg-1)*(MaxAge+1)+Age+1
	   SBeq(Ireg) = SBeq(Ireg)+NewVec(JJ)*Fecundity(Istk,Age,Fyear)*Weight(Istk,1,Age,Fyear)
1010 CONTINUE
	 SBeq = SBeq/SUM(SBeq(1:Nreg))
	 IF (Diag.EQ.1) THEN
	  DO 1011 Ireg=1,Nreg
	   WRITE(98,'(I2,1x,F6.4,1x,F6.4)') Ireg,SBeq(Ireg),Lamdatemp(Ireg)
1011  CONTINUE
	 ENDIF

	 !objective function
	 Obj=0.d0
	 DO 1020 Ireg=1,Nreg
      ! Obj = Obj +ABS(SBeq(Ireg)-Lamdatemp(Ireg))
	  Obj = Obj+(SBeq(Ireg)-Lamdatemp(Ireg))**2.d0
1020 CONTINUE
	
	 IF (Diag.EQ.1) WRITE(98,'(A5,1x,F12.10)') 'Objfn',Obj

	 !save Lamda
	 LamdaZero(Istk,1:Nreg) = Lamdatemp
	 FracRec(Istk,1:Nreg,Fyear) = Lamdatemp

	 !save age structure
     DO 1050 Ireg=1,Nreg
	  DO 1050 Age=0,MaxAge
	   II = (Nreg-1)*(MaxAge+1)+Age+1
	   N(Istk,Ireg,1,Age,Fyear)=NewVec(II)
1050 CONTINUE

    EqRecFunk=Obj

	RETURN

	END

! =========================================================================
!      
      SUBROUTINE FUNCT(PARS,DEV)
!
! Thsi subroutine call EQRecFUNK
!      
      IMPLICIT NONE
      INCLUDE 'Sinatra.INC'
!
!     Global variables
      REAL*8 PARS(100),DEV
!
!     Local variables
      REAL*8 EqRecFunk
!
      DEV = EqRecFunk(PARS)
!
      RETURN
      END  

!      
! =========================================================================
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	INCLUDE 'Common.FOR'
	INCLUDE 'C.F90'
	INCLUDE 'Dminim.For'
	INCLUDE 'Matrix.for'

	INCLUDE 'GenData.f90'
	!INCLUDE 'projrec3.f90'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	This subroutine is the test call for the future projections
!
	SUBROUTINE TestSinatranew(Yr1,Yr2,Numbers,Mort,Mulen,Siglen,Wt,Fec,Move,Sel,Ret,S0,ISEED1,ISEED2)

	IMPLICIT NONE

	INCLUDE 'Sinatra.INC'

!	LOCAL VARIABLES
	INTEGER Yr1,Yr2,ISEED1,ISEED2
	REAL*8 Numbers(Nstock,Nzone,2,TopAge+1),Mort(Nstock,Nzone,2,TopAge+1),Mulen(Nstock,2,TopAge+1)
	REAL*8 Siglen(Nstock,2,TopAge+1),Wt(Nstock,2,TopAge+1),Fec(Nstock,TopAge+1),Qdevs(Nfleet,Nzone)
	REAL*8 Move(Nstock,Nzone,Nzone,2,TopAge+1),Sel(Nfleet,NlenBin),Ret(Nfleet,Nlenbin),S0(Nstock)
!	REAL*4 RAN1
!	EXTERNAL RAN1

!	****MOVE THESE TO A READFILE AT SOME POINT!!!!!
        ISEEDX =  ISEED1
        ISEEDZ = ISEED2


!	OPEN GENERAL JUNKFILE 
	OPEN(UNIT=99,FILE='Control.junk',POSITION='APPEND')
	WRITE(99,'(A25,I4,A4,I4)') 'starting projection from ',Yr1,' to ',Yr2+1

!	WRITE(99,*) RAN1(ISEED)


!	READ IN CONTROL VARIABLES AND SET-UP
	WRITE(99,*) 'Stepping into projReadIn'
	CALL ReadIn(2)	
	WRITE(99,*) 'projReadIN finished'
	
!	ASSIGN Input variables to proper places
	WRITE(99,*) 'Assigning inputs to proper vals'
	IF (Diag.EQ.1) WRITE(99,*) 'check assignment.junk'
	CALL AssignInputs(Yr1,Numbers,Mort,Mulen,Siglen,Wt,Fec,Move,Sel,Ret,Qdevs,S0)

!	DO FUTURE PROJECTION
	WRITE(99,*) 'starting future projection'
	CALL FutureProj(Yr1,Yr2)

!	GET FUTURE DATA
	CALL FindNewData(Yr1,Yr2)

!	ASSIGN RELEVANT QUANTITIES TO RETURN VALUES
	CALL AssignOutputs(Yr2,Numbers,Mort,Mulen,Siglen,Wt,Fec,Move,Sel,Ret,Qdevs,S0)

	CLOSE(99)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	This subroutine is the call for the future projections, is mainly admin
!	most of the projection work is done by futureproj
!
	SUBROUTINE sinatranew(Yr1,Yr2,Numbers,Mort,Mulen,Siglen,Wt,Fec,Move,Sel,Ret,S0,ISEED1,ISEED2)
!DEC$ ATTRIBUTES DLLEXPORT::sinatranew
!DEC$ ATTRIBUTES C,REFERENCE,ALIAS:'sinatranew_' :: sinatranew

	IMPLICIT NONE

	INCLUDE 'Sinatra.INC'

!	LOCAL VARIABLES
	INTEGER Yr1,Yr2,ISEED1,ISEED2
	REAL*8 Numbers(Nstock,Nzone,2,TopAge+1),Mort(Nstock,Nzone,2,TopAge+1),Mulen(Nstock,2,TopAge+1)
	REAL*8 Siglen(Nstock,2,TopAge+1),Wt(Nstock,2,TopAge+1),Fec(Nstock,TopAge+1),Qdevs(Nfleet,Nzone)
	REAL*8 Move(Nstock,Nzone,Nzone,2,TopAge+1),Sel(Nfleet,NlenBin),Ret(Nfleet,Nlenbin),S0(2,Nstock)

        ISEEDX =  ISEED1
        ISEEDZ = ISEED2


!	OPEN GENERAL JUNKFILE 
	OPEN(UNIT=99,FILE='Control.junk',POSITION='APPEND')
	WRITE(99,'(A25,I4,A4,I4)') 'starting projection from ',Yr1,' to ',Yr2+1

!	WRITE(99,*) RAN1(ISEED)


!	READ IN CONTROL VARIABLES AND SET-UP
	WRITE(99,*) 'Stepping into projReadIn'
	CALL ReadIn(2)	
	WRITE(99,*) 'projReadIN finished'
	
!	ASSIGN Input variables to proper places
	WRITE(99,*) 'Assigning inputs to proper vals'
	IF (Diag.EQ.1) WRITE(99,*) 'check assignment.junk'
	WRITE(99,*) 'SB0',S0
	CALL AssignInputs(Yr1,Numbers,Mort,Mulen,Siglen,Wt,Fec,Move,Sel,Ret,Qdevs,S0)

!	DO FUTURE PROJECTION
	WRITE(99,*) 'starting future projection'
	CALL FutureProj(Yr1,Yr2)

!	GET FUTURE DATA
	CALL FindNewData(Yr1,Yr2)

!	ASSIGN RELEVANT QUANTITIES TO RETURN VALUES
	WRITE(99,*) 'assigning outputs'
	CALL AssignOutputs(Yr2,Numbers,Mort,Mulen,Siglen,Wt,Fec,Move,Sel,Ret,Qdevs,S0)

	WRITE(99,*) 'returning to R.....'

	CLOSE(99)

	RETURN

	END SUBROUTINE sinatranew

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine performs the future projections
!	
	SUBROUTINE Futureproj(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Age,Ireg,Iyr,Yr1,Yr2

!	OPEN some summary files
   IF (Diag.EQ.1) THEN
	 OPEN(UNIT=91,FILE='Numbers.junk',POSITION='APPEND')
   ENDIF
	 OPEN(UNIT=92,FILE='SpawBio.junk',POSITION='APPEND')
     OPEN(UNIT=93,FILE='TotCatch.junk',POSITION='APPEND')
	 OPEN(UNIT=94,FILE='ExpRate.junk',POSITION='APPEND')
!	ENDIF

!	Iyr = Yr1

!	First Set Up the various matrices needed
	WRITE(99,*) 'Stepping into Set-Up'
	CALL FutureSetUp(Yr1,Yr2)
	WRITE(99,*) 'Finished with Set-Up'

!	Yr1 = Iyr

!	WRITE(99,*) Yr1

!	Update for each year of future period
	WRITE(99,*) 'Doing future projection'
	IF (Diag.EQ.1) WRITE(99,*) '...check histproj.junk'
	IF (Diag.EQ.1) OPEN(UNIT=98,FILE='histproj.junk',POSITION='APPEND')
	 DO 3010 Iyr=Yr1+1,Yr2+1
	  CALL PopUpdate(Iyr)
	  IF (Iyr.LE.Yr2) CALL GetNewCatch(Iyr)
3010 CONTINUE
	CLOSE(98)
	WRITE(99,*) 'Finished future projection'

	IF (Diag.EQ.1) THEN
	CLOSE(91)
	ENDIF
	CLOSE(92)
	CLOSE(93)
	CLOSE(94)
!	ENDIF
!	Done with future projection

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine sets up the various matrices of parameters needed for the historical projection
    SUBROUTINE FutureSetUp(Yr1,Yr2)

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER II,Istk,Ireg,Sex,Yr1,Yr2,Iyr

	WRITE(99,*) Yr1


!	Biology
	IF (Diag.EQ.1) THEN
	 WRITE(99,*) 'Setting up Biology - check biology.junk'
	 OPEN(UNIT=98,FILE='Biology.junk',POSITION='APPEND')
	 WRITE(98,*) 'projection',Yr1,Yr2
	ENDIF
	!M
	CALL GetM(Yr1-1,Yr2)
!		WRITE(99,*) 'm',Yr1
	!Growth
	!Length at age
	CALL GetLenAge(Yr1,Yr2+1)
!		WRITE(99,*) 'la',Yr1
	!SD of len @ age
	CALL GetSDLenAge(Yr1,Yr2+1)
!		WRITE(99,*) 'sdla',Yr1
	!Weight at age
	CALL GetWtAge(Yr1,Yr2+1)
!		WRITE(99,*) 'wa',Yr1
    CALL Get_WtLen()
!		WRITE(99,*) 'wl',Yr1
	!Fecundity at age
	!CALL GetFecund(Yr1,Yr2+1)
!		WRITE(99,*) 'f',Yr1
!		Iyr = Yr1
	!Movement (X Matrix)
	CALL GetX(Yr1,Yr2)
!		WRITE(99,*) 'x',Yr1
!	Yr1 = Iyr

	IF (Diag.EQ.1) CLOSE(98)
    IF (Diag.EQ.1) WRITE(99,*) 'Finished setting up Biology'

!	Selectivity
	IF (Diag.EQ.1) WRITE(99,*) 'Setting up Selectivity - check Selex.junk'
	IF (Diag.EQ.1) OPEN(UNIT=98,FILE='Selex.junk',POSITION='APPEND')

	!Proportions of age in length bins
	CALL GetFracLen(Yr1-1,Yr2+1)

	IF (Diag.EQ.1) CLOSE(98)
	IF (Diag.EQ.1) OPEN(UNIT=98,FILE='Biology.junk',POSITION='APPEND')
	!Fecundity at age
    CALL GetFracLenStart(Yr1-1,Yr2+1)
	CALL GetFecund(Yr1,Yr2+1)
	IF (Diag.EQ.1) CLOSE(98)
	IF (Diag.EQ.1) OPEN(UNIT=98,FILE='Selex.junk',POSITION='APPEND')

	!Correlation among length bins in annual selex deviations 
	CALL GetCorDevSel()
	!Selectivity at Age
	CALL GetSelex(Yr1-1,Yr2)
	!Retention
	CALL GetRetent(Yr1-1,Yr2)

	IF (Diag.EQ.1) CLOSE(98)
    IF (Diag.EQ.1) WRITE(99,*) 'Finished setting up Selectivity'


!	Recruitment Devs
	IF (Diag.EQ.1) THEN
	 WRITE(99,*) 'Setting up Recruitment devs - check RecDevs.junk'
	 OPEN(UNIT=98,FILE='RecDevs.junk',POSITION='APPEND')
	 WRITE(98,*) 'projection',Yr1,Yr2
	ENDIF
		
!	Get the recruitment deviations
	CALL GetRecDevs(Yr1+1,Yr2+1)
	IF (Diag.EQ.1) CLOSE(98)
    IF (Diag.EQ.1) 	WRITE(99,*) 'Finished setting up Recruitment Devs'
!	WRITE(99,*) Yr1

!	Get the RBC
	WRITE(99,*) 'loading RBC'	
	CALL GetRBC(Yr1)

!	WRITE(99,*) 'got rbc'
!	WRITE(99,*) Yr1

!	Get the vulnerable biomass for the first year
	CALL GetVBio(Yr1)

!	WRITE(99,*) 'got vbio'

!	Get the catch for the first year
	CALL GetNewCatch(Yr1)
	IF (Diag.EQ.1) WRITE(99,*)'check catches.junk'

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE GetRBC(Yr1)

!	This subroutine reads in the RBC and allocation parameters

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Npar,II,Ipar,Yr1,Iflt
	REAL*8 MinCatch,ImplCV,Temp,XNORM,DUM1,ImplA,ImplB,EstMSY

	EXTERNAL XNORM

	OPEN(UNIT=14,FILE='RBC.out')

	  READ(14,*)
	  READ(14,*) RBC

	  IF (RBC.LT.0) STOP

	CLOSE(14)


	   OPEN(UNIT=14,FILE='alloc.out')
	  !find the number of active fleets
	  READ(14,*) NactiveF
	  !the active fleets
	  READ(14,*) (ActiveF(II),II=1,NactiveF)
	  Npar = INT(NactiveF*Nreg)
!	  WRITE(99,*) Npar
	  DO 3009 Ipar=1,3
	   READ(14,*) (AllocPars(Ipar,II),II=1,Npar)
!	   WRITE(99,*) AllocPars(Ipar,1:Npar)
3009  CONTINUE	 

	CLOSE(14)


	OPEN(UNIT=14,FILE='hcr.ctl')

	Npar=17
	DO II=1,Npar
	 READ(14,*)
	ENDDO
	READ(14,*) MinCatch
	READ(14,*)
	READ(14,*) SigmaAlloc
	READ(14,*)
	READ(14,*) ImplA,ImplB,ImplCV
	CLOSE(14)
	IF (HCRspecs(3).EQ.1) EstMSY = EstQuant(2)
	IF (HCRspecs(3).EQ.4) EstMSY = EstQuant(3)
    IF (HCRspecs(3).EQ.3) EstMSY = EstQuant(5)
    IF (HCRspecs(3).EQ.51) EstMSY = EstQuant(2)

 	IF (EstMSY.LE.0d0) EstMSY = SUM(TotalCatch(1:Nflt,1:Nreg,(Lyear-4):Lyear))/5.d0
	RBC = RBC/EstMSY
	RBC = ImplA*RBC+ImplB
	IF (ImplCV.GT.0) THEN
	 Temp = XNORM(5,0.d0,ImplCV,ISEEDX)
	 RBC = RBC*EXP(Temp-(0.5d0*(ImplCV**2.d0)))
	ENDIF
	RBC = RBC*EstMSY
	IF (RBC.LT.MinCatch) RBC = MinCatch
	!RBC = 1000

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE GetNewCatch(Iyr)

!	This subroutine apportions catch by fleet and region

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,Ireg,DUM,Iyr,Npar,II,Ipar,RBCuse,ISEED,JJ,Jflt
	REAL*8 XNORM,Catchprops(2000),CatchVec(2000),AllocDev,ExtraCatch(1:Nfleet,1:Nreg),AllocCat,SpareCatch(1:Nfleet,1:Nreg)

	EXTERNAL XNORM

	ISEED = ISEEDX

	OPEN(UNIT=97,FILE='Catches.junk',POSITION='APPEND')

	Catch(1:Nflt,1:Nreg,Iyr)=0.d0
	IF (NoCatch.EQ.0) THEN
	  Catchprops = 0.d0
	  II=0
	  DO 3010 Iflt=1,NactiveF
	   DO 3010 Ireg=1,Nreg
		 II=II+1
		 AllocDev = XNORM(5,0.d0,SigmaAlloc,ISEEDX)
		 IF (FleetRegions(ActiveF(Iflt),Ireg).GT.0) Catchprops(II) = AllocPars(1,II) + AllocPars(2,II)*(VBio(ActiveF(Iflt),Ireg,Iyr)**AllocPars(3,II))
		 Catchprops(II) = Catchprops(II) * EXP(AllocDev)
3010  CONTINUE
	Catchprops = Catchprops/SUM(Catchprops)
	RBCuse = INT(RBC)
	CALL GenMul(Catchprops,RBCuse,CatchVec,ISEEDX,.FALSE.,1)
	  II=0
	  DO 3011 Iflt=1,NactiveF
	   DO 3011 Ireg=1,Nreg
		 II=II+1	
	     Catch(ActiveF(Iflt),Ireg,Iyr) = CatchVec(II)
3011  CONTINUE
	ENDIF

    IF (Forecat.NE.0) THEN  !added 2 May 2011 to give forecat = 0 the default (i.e. for alloc.out pattern)
	!added feb 15th 2011 for at limits, catch strategies, etc.
    Catch(1:Nflt,1:Nreg,Iyr) = 0.d0
	!MaxCatch(3) = 140.d0
	!Forecat = 1
	WRITE(99,*) 'rbcuse',RBCuse
	!WRITE(99,*) (MaxCatch(Iflt),Iflt=1,Nflt)
	IF (RBCuse.LE.MaxCatch(3)) THEN
	 DO Ireg=1,Nreg
	   IF (FleetRegions(3,Ireg).GT.0) Catch(3,Ireg,Iyr) = RBCuse
	  ENDDO
	ELSE
	 DO Ireg=1,Nreg
	   IF (FleetRegions(3,Ireg).GT.0) Catch(3,Ireg,Iyr) = MaxCatch(3)
	 ENDDO
	 AllocCat = RBCuse - MaxCatch(3)
	 WRITE(99,*) 'rem cat',AllocCat
	 IF (Forecat.EQ.1) THEN
	  DO Iflt=4,5
	   DO Ireg=1,Nreg
	    IF (FleetRegions(Iflt,Ireg).GT.0) Catch(Iflt,Ireg,Iyr) = AllocCat/2.d0
	   ENDDO
	  ENDDO
	 ELSEIF (Forecat.EQ.3) THEN
	  DO Iflt=4,5
	   DO Ireg=1,Nreg
	    IF (Iflt.EQ.4.AND.FleetRegions(Iflt,Ireg).GT.0) Catch(Iflt,Ireg,Iyr) = 0.3d0*AllocCat
		IF (Iflt.EQ.5.AND.FleetRegions(Iflt,Ireg).GT.0) Catch(Iflt,Ireg,Iyr) = 0.7d0*AllocCat
	   ENDDO
	  ENDDO
	 ELSEIF (Forecat.EQ.2) THEN
	  IF (MOD(Iyr,2).EQ.0) THEN
	   DO Ireg=1,Nreg
	   IF (FleetRegions(4,Ireg).GT.0) Catch(4,Ireg,Iyr) = AllocCat
       IF (FleetRegions(5,Ireg).GT.0) Catch(5,Ireg,Iyr) = 0.d0
	   ENDDO
	  ELSE
	   DO Ireg=1,Nreg
	   IF (FleetRegions(5,Ireg).GT.0) Catch(5,Ireg,Iyr) = AllocCat
       IF (FleetRegions(4,Ireg).GT.0) Catch(4,Ireg,Iyr) = 0.d0
	   ENDDO
	  ENDIF
	 ENDIF
	ENDIF
	ENDIF

	!!added 19th September 2010 to correct for spatial shortfall in TAC
	!!could need to do something about multiple fleets in a region, but not needed now.
	IF (Nreg.GT.1) THEN
     CALL GetVBio(Iyr)
     ExtraCatch = 0.d0
	 SpareCatch = 0.d0
     DO 3012 Iflt=1,Nflt
	  DO 3013 Ireg=1,Nreg
	   !WRITE(99,*) Iyr,Ireg,Catch(Iflt,Ireg,Iyr)
	   IF (0.999*RetVBio(Iflt,Ireg,Iyr).LE.Catch(Iflt,Ireg,Iyr)) THEN
	    ExtraCatch(Iflt,Ireg) = Catch(Iflt,Ireg,Iyr) - 0.999*RetVBio(Iflt,Ireg,Iyr)
       ELSE
	    IF (FleetRegions(Iflt,Ireg).GT.0) SpareCatch(Iflt,Ireg) = 0.999*RetVBio(Iflt,Ireg,Iyr) - Catch(Iflt,Ireg,Iyr)
       ENDIF
3013  CONTINUE
      !WRITE(99,*) Iyr,SUM(ExtraCatch(Iflt,1:Nreg))
      !WRITE(99,*) Iyr,SUM(SpareCatch(Iflt,1:Nreg))
      IF (SUM(ExtraCatch(Iflt,1:Nreg)).LE.0) GOTO 3017
      IF (SUM(SpareCatch(Iflt,1:Nreg)).GT.0) THEN
       Catchprops = 0.d0 
	   DO 3014 Ireg=1,Nreg
	    IF (ExtraCatch(Iflt,Ireg).EQ.0) THEN
		 Catchprops(Ireg) = Catch(Iflt,Ireg,Iyr)
         IF (SpareCatch(Iflt,Ireg).GT.0.AND.Catchprops(Ireg).EQ.0) Catchprops(Ireg) = Catchprops(Ireg)+0.0001d0
        ENDIF
        !WRITE(99,*) Ireg,CatchProps(Ireg)
3014   CONTINUE
       Catchprops = SUM(ExtraCatch(Iflt,1:Nreg))*Catchprops/SUM(Catchprops)
       DO 3015 Ireg=1,Nreg
	    IF (Catchprops(Ireg).GT.SpareCatch(Iflt,Ireg)) Catchprops(Ireg) = SpareCatch(Iflt,Ireg)
		Catch(Iflt,Ireg,Iyr) = Catch(Iflt,Ireg,Iyr) + Catchprops(Ireg)
		SpareCatch(Iflt,Ireg) = SpareCatch(Iflt,Ireg) - Catchprops(Ireg)
3015   CONTINUE
       AllocCat = SUM(Catchprops)
	   Catchprops = 0.d0
       DO 3016 Ireg=1,Nreg
	    IF(ExtraCatch(Iflt,Ireg).GT.0) Catchprops(Ireg) = ExtraCatch(Iflt,Ireg)
3016   CONTINUE
       CatchProps = AllocCat*Catchprops/SUM(Catchprops)
	   DO Ireg=1,Nreg
	   Catch(Iflt,Ireg,Iyr) = Catch(Iflt,Ireg,Iyr) - CatchProps(Ireg)
	   ExtraCatch(Iflt,Ireg) = ExtraCatch(Iflt,Ireg) - CatchProps(Ireg)
       ENDDO
!	   Catch(Iflt,1:Nreg,Iyr) = Catch(Iflt,1:Nreg,Iyr) - CatchProps(1:Nreg)
!	   ExtraCatch(Iflt,1:Nreg) = ExtraCatch(Iflt,1:Nreg) - CatchProps(1:Nreg)
	  ENDIF
3017  CONTINUE
3012  CONTINUE

      

     DO 3025 Iflt=1,Nflt
	  DO 3025 Ireg=1,Nreg
        SpareCatch(Iflt,Ireg) = 0.999*RetVBio(Iflt,Ireg,Iyr) - Catch(Iflt,Ireg,Iyr)
        !SpareCatch(Iflt,Ireg) = MAX(SpareCatch(Iflt,Ireg),0.d0)
		!WRITE(99,*) Iyr,Ireg,Catch(Iflt,Ireg,Iyr)
3025 CONTINUE

!     GOTO 3026
!     WRITE(99,*) SUM(ExtraCatch(1:Nflt,1:Nreg))
!     WRITE(99,*) SUM(SpareCatch(1:Nflt,1:Nreg))


     IF (SUM(ExtraCatch(1:Nflt,1:Nreg)).GT.0.AND.SUM(SpareCatch(1:Nflt,1:Nreg)).GT.0) THEN
      DO 3020 Iflt=1,Nflt 
       IF(SUM(ExtraCatch(Iflt,1:Nreg)).GT.0) THEN
	    Catchprops = 0.d0 
	    II = 0
        DO 3021 Jflt=1,Nflt
	     DO 3021 Ireg=1,Nreg
		  II=II+1
	      IF (Jflt.NE.Iflt.AND.SpareCatch(Jflt,Ireg).GT.0.AND.FleetRegions(Jflt,Ireg).GT.0) Catchprops(II) = Catch(Jflt,Ireg,Iyr)
3021    CONTINUE
        IF(SUM(Catchprops).GT.0) Catchprops = SUM(ExtraCatch(Iflt,1:Nreg))*Catchprops/SUM(Catchprops)
        II=0
	    DO 3022 Jflt=1,Nflt
	     DO 3022 Ireg=1,Nreg
	     II = II+1
		 IF (Jflt.NE.Iflt.AND.Catchprops(II).GT.SpareCatch(Jflt,Ireg).AND.FleetRegions(Jflt,Ireg).GT.0) Catchprops(II) = SpareCatch(Jflt,Ireg)
		 Catch(Jflt,Ireg,Iyr) = Catch(Jflt,Ireg,Iyr) + Catchprops(II)
		 SpareCatch(Jflt,Ireg) = SpareCatch(Jflt,Ireg) - Catchprops(II)
3022   CONTINUE
       !WRITE(99,*) Iflt,SUM(Catchprops)
       AllocCat = SUM(Catchprops)
	   Catchprops = 0.d0
	   DO 3023 Ireg=1,Nreg
	    IF(ExtraCatch(Iflt,Ireg).GT.0) Catchprops(Ireg) = ExtraCatch(Iflt,Ireg)
3023   CONTINUE
!       WRITE(99,*) Iflt,SUM(Catchprops)
       CatchProps = AllocCat*Catchprops/SUM(Catchprops)
!	   WRITE(99,*) Iyr, Iflt
	   DO 3024 Ireg=1,Nreg
		!WRITE(99,*) Ireg, Catchprops(Ireg)
        !WRITE(99,*) Ireg, Catch(Iflt,Ireg,Iyr)
        !WRITE(99,*) Ireg, ExtraCatch(Iflt,Ireg)
		Catch(Iflt,Ireg,Iyr) = Catch(Iflt,Ireg,Iyr) - Catchprops(Ireg)
	    ExtraCatch(Iflt,Ireg) = ExtraCatch(Iflt,Ireg) - Catchprops(Ireg)
3024   CONTINUE
	   ENDIF
3020  CONTINUE

      ENDIF
	  
	  ENDIF
	  
!3026 CONTINUE  


	IF (Diag.EQ.1) THEN
	 WRITE(97,'(6x,120(I6,1x))') Iyr
	 DO Iflt=1,Nflt
	  DO Ireg=1,Nreg
	   WRITE(97,'(I2,1x,I2,1x,120(F6.1,1x))') Iflt,Ireg,Catch(Iflt,Ireg,Iyr)
      ENDDO
	 ENDDO     
	 DO Ireg=1,Nreg
	  WRITE(97,'(I2,1x,I2,1x,120(F6.1,1x))') 0,Ireg,SUM(Catch(1:Nflt,Ireg,Iyr))
     ENDDO
	 DO Iflt=1,Nflt
	  WRITE(97,'(I2,1x,I2,1x,120(F6.1,1x))') Iflt,0,SUM(Catch(Iflt,1:Nreg,Iyr))
     ENDDO
	ENDIF

	CLOSE(97)

	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   This subroutine reads in the set up specs from Sinatra.CTL
    SUBROUTINE ReadIn(Itype)

!	Array order is Fleet:Stock:Region:LengthBin:Sex:Age:Time

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

!	Local Variables
	INTEGER Nskip,II,Istk,Ireg,Jreg,Iflt,Ilen,Age,Sex,Npar,Itype,Iyr,Npair
	CHARACTER*20 Catchfile

!	Open Junk file
	IF (Itype.EQ.1)	OPEN(UNIT=98,FILE='ReadIn.junk')
	IF (Itype.EQ.2)	OPEN(UNIT=98,FILE='ProjReadIn.junk')

!	OPEN CONTROL FILE
	OPEN(UNIT=13,FILE='Sinatra.CTL')	

!	Check Diagnostics
    Nskip=9
	DO II=1,Nskip
	 READ(13,*)
    ENDDO
	READ(13,*) Diag
	READ(13,*)
	READ(13,*) NoCatch
    IF (Diag.EQ.1) WRITE(98,*) 'NoCatch',NoCatch

!   Model Structure
    Nskip=4
	DO II=1,Nskip
	 READ(13,*)
    ENDDO
	READ(13,*) Nreg
	READ(13,*)
	READ(13,*) Nstk
	READ(13,*)
	DO Istk=1,Nstk
	 READ(13,*) (LamdaZero(Istk,Ireg),Ireg=1,Nreg)
	ENDDO
    READ(13,*)
	READ(13,*) Nflt
	READ(13,*)
	FleetRegions=0
	DO Iflt=1,Nflt
	 READ(13,*) (FleetRegions(Iflt,Ireg),Ireg=1,Nreg)
    ENDDO
	IF (Diag.EQ.1) THEN
	 WRITE(98,'(A5,1x,I2)') 'Nreg',Nreg
     WRITE(98,'(A5,1x,I2)') 'Nstk',Nstk
	 DO Istk=1,Nstk
	  WRITE(98,'(100(F5.3,1x))') (LamdaZero(Istk,Ireg),Ireg=1,Nreg)
	 ENDDO
     WRITE(98,'(A5,1x,I3)') 'Nflt',Nflt
     DO Iflt=1,Nflt
	  WRITE(98,'(100(I1,1x))') (FleetRegions(Iflt,Ireg),Ireg=1,Nreg)
	 ENDDO
	ENDIF

!   Length bin structure
    Nskip=3
	DO II=1,Nskip
	 READ(13,*)
    ENDDO
	READ(13,*) Nlen
	READ(13,*)
	READ(13,*) (LenBins(Ilen),Ilen=1,Nlen)
	READ(13,*)
	READ(13,*) LenBins(Nlen+1)
    IF (Diag.EQ.1) THEN
	 WRITE(98,'(A4,1x,I3)') 'Nlen',Nlen
     WRITE(98,'(A6,100(1x,F5.1))') 'LenBins',(LenBins(Ilen),Ilen=1,Nlen+1)
	ENDIF
	loLenBin(1:Nlen) = LenBins(1:Nlen)
	hiLenBin(1:Nlen) = LenBins(2:Nlen+1)

!   Year set-up
    Nskip=3
	DO II=1,Nskip
	 READ(13,*)
    ENDDO
	READ(13,*) Fyear
	READ(13,*)
	READ(13,*) Lyear
    IF (Diag.EQ.1) THEN
	 WRITE(98,'(A5,1x,I4)') 'Fyear',Fyear
	 WRITE(98,'(A5,1x,I4)') 'Lyear',Lyear
     WRITE(98,*) 'Done with model structure read-in set-up'
    ENDIF


!   Biological Parameters
!	---------------------
    Nskip=4
	DO II=1,Nskip
	 READ(13,*)
    ENDDO

!	maxage    
	READ(13,*) MaxAge
    IF (Diag.EQ.1) WRITE(98,'(A6,1x,I4)') 'maxage',MaxAge
    READ(13,*)
	READ(13,*)

!	M
	DO Istk=1,Nstk
	 READ(13,*) (Mzero(Istk,Age),Age=0,MaxAge)
	ENDDO
    IF (Diag.EQ.1) THEN
     WRITE(98,*) 'M'
	 DO Istk=1,Nstk
	  WRITE(98,'(101(F5.3,1x))') (Mzero(Istk,Age),Age=0,MaxAge)
	 ENDDO
    ENDIF
	READ(13,*)

!	Growth

!	length at age input flag
	READ(13,*) LAflag
	READ(13,*)

!	Mean lengths at age
	DO Istk=1,Nstk
	 DO Sex=1,2
	  READ(13,*) (MeanLenAge(Istk,Sex,Age,Fyear),Age=0,MaxAge)
	 ENDDO
	ENDDO    
	IF (Diag.EQ.1) THEN
	 WRITE(98,'(A6,1x,I1)') 'LAflag',LAflag
     WRITE(98,*) 'Stk,Sex,lengths at age'
	 DO Istk=1,Nstk
	  DO Sex=1,2
	   WRITE(98,'(I2,1x,I1,1x,101(F6.2,1x))') Istk,Sex,(MeanLenAge(Istk,Sex,Age,Fyear),Age=0,MaxAge)
	  ENDDO
	 ENDDO
	ENDIF
    READ(13,*)

!	growth curve parameters
	DO Istk=1,Nstk
	 DO Sex=1,2
	  READ(13,*) VBLinf(Istk,Sex,Fyear),VBK(Istk,Sex,Fyear),VBTzero(Istk,Sex,Fyear)
	 ENDDO
	ENDDO
	IF (Diag.EQ.1) THEN
      WRITE(98,*) 'Stk,Sex,growth curve pars'
	 DO Istk=1,Nstk
	  DO Sex=1,2
	   WRITE(98,'(I2,1x,I1,1x,3(F7.3,1x))') Istk,Sex,VBLinf(Istk,Sex,Fyear),VBK(Istk,Sex,Fyear),VBTzero(Istk,Sex,Fyear)
	  ENDDO
	 ENDDO          
    ENDIF

!   std dev of len at age flag
	READ(13,*) 
	READ(13,*) SLAflag
    READ(13,*)
!	std dev of len at age
	DO Istk=1,Nstk
	 DO Sex=1,2
	  READ(13,*) (SigmaLenAge(Istk,Sex,Age,Fyear),Age=0,MaxAge)
	 ENDDO
	ENDDO
	IF (Diag.EQ.1) THEN
	 WRITE(98,'(A7,1x,I1)') 'SLAflag',SLAflag
     WRITE(98,*) 'Stk,Sex,std dev of lengths at age'
	 DO Istk=1,Nstk
	  DO Sex=1,2
	   WRITE(98,'(I2,1x,I1,1x,101(F6.2,1x))') Istk,Sex,(SigmaLenAge(Istk,Sex,Age,Fyear),Age=0,MaxAge)
	  ENDDO
	 ENDDO
    ENDIF
    READ(13,*)

!   CV of length at age for age zero and maxage by stock and sex
	DO Istk=1,Nstk
	 DO Sex=1,2
	  READ(13,*) CVLen0(Istk,Sex),CVLenX(Istk,Sex)
	 ENDDO
	ENDDO
	IF (Diag.EQ.1) THEN
     WRITE(98,*) 'Stk,Sex,CV of Len at Age 0 & x'
	 DO Istk=1,Nstk
	  DO Sex=1,2
	   WRITE(98,'(I2,1x,I1,1x,2(F6.3,1x))') Istk,Sex,CVLen0(Istk,Sex),CVLenX(Istk,Sex)
	  ENDDO
	 ENDDO          
	ENDIF
    READ(13,*)

!	wt @ age flag
	READ(13,*) WAflag
	READ(13,*)
!	wt @ age
	DO Istk=1,Nstk
	 DO Sex=1,2
	  READ(13,*) (Weight(Istk,Sex,Age,Fyear),Age=0,MaxAge)
	 ENDDO
	ENDDO
    IF (Diag.EQ.1) THEN
	 WRITE(98,'(A7,1x,I1)') 'WAflag',WAflag
     WRITE(98,*) 'Stk,Sex, Weights at age (start)'
	 DO Istk=1,Nstk
	  DO Sex=1,2
	   WRITE(98,'(I2,1x,I1,1x,101(F6.3,1x))') Istk,Sex,(Weight(Istk,Sex,Age,Fyear),Age=0,MaxAge)
	  ENDDO
	 ENDDO
	ENDIF
	
!	wt @ age (middle of yr)
	READ(13,*)
	DO Istk=1,Nstk
	 DO Sex=1,2
	  READ(13,*) (WeightMid(Istk,Sex,Age,Fyear),Age=0,MaxAge)
	 ENDDO
	ENDDO
    IF (Diag.EQ.1) THEN
     WRITE(98,*) 'Stk,Sex, Weights at age (middle)'
	 DO Istk=1,Nstk
	  DO Sex=1,2
	   WRITE(98,'(I2,1x,I1,1x,101(F6.3,1x))') Istk,Sex,(WeightMid(Istk,Sex,Age,Fyear),Age=0,MaxAge)
	  ENDDO
	 ENDDO
	ENDIF

    
!   wt len pars
	READ(13,*)	
	DO Istk=1,Nstk
	 DO Sex=1,2
	  READ(13,*) (WtLenPars(II,Istk,Sex),II=1,2)
	 ENDDO
	ENDDO
    IF (Diag.EQ.1) THEN
	 WRITE(98,*) 'Stk,Sex, Wt-Len Pars'
	 DO Istk=1,Nstk
	  DO Sex=1,2
	   WRITE(98,'(I2,1x,I1,1x,2(F10.7,1x))') Istk,Sex,(WtLenPars(II,Istk,Sex),II=1,2)
	  ENDDO
	 ENDDO          
	ENDIF
    READ(13,*)	
!	MAturity at Length by stock
    DO Istk=1,Nstk
	 !READ(13,*) (Fecundity(Istk,Age,Fyear),Age=0,MaxAge)	
	 READ(13,*) (Maturity(Istk,Age,Fyear),Age=1,Nlen)	
	ENDDO
    IF (Diag.EQ.1) THEN
     WRITE(98,*) 'Stk, Fecundity at Length'
	 DO Istk=1,Nstk
	  WRITE(98,'(I2,1x,101(F5.3,1x))') Istk,(Maturity(Istk,Age,Fyear),Age=1,Nlen)
	 ENDDO
	ENDIF
!	Movement Rules to generate X matrix
	READ(13,*)

!	Maximum annual proportion of movers by stock and sex
	READ(13,*)
	IF (Diag.EQ.1) WRITE(98,*) 'Stk, Max movers'
	Npair = INT(0.5*(Nreg-1)+0.5*(Nreg-1)**2.d0)
	DO Istk=1,Nstk 
	 READ(13,*) (Maxmovers(Istk,II),II=1,Npair)
	 IF (Diag.EQ.1) WRITE(98,'(I2,1x,101(F5.3,1x))') Istk,(Maxmovers(Istk,II),II=1,Npair)
	ENDDO

	!relative age-specific movement
	READ(13,*)
	IF (Diag.EQ.1) WRITE(98,*) 'Stk, Sex, relative move probs by age'
	DO Istk=1,Nstk
	 DO Sex=1,2
	  READ(13,*) (Relmove(Istk,Sex,Age),Age=0,MaxAge)
	  IF (Diag.EQ.1) WRITE(98,'(I2,1x,I2,1x,101(F5.3,1x))') Istk,Sex,(Relmove(Istk,Sex,Age),Age=0,MaxAge)
	 ENDDO
	ENDDO

	
!	spit out biological params
!	 write out movement things here
!	done with biological read
    IF (Diag.EQ.1) WRITE(98,*) 'Done with biological params read-in'
    
!	parameters governing Recruitment processes

!	Target Depletion or Rzero by stock
	READ(13,*)
	READ(13,*) Rzeroflag
	READ(13,*)
	IF (Rzeroflag.EQ.1) THEN
	 READ(13,*) (Rzero(Istk),Istk=1,Nstk)
	 IF (Diag.EQ.1) WRITE(98,'(A5,1x,10(F8.1,1x))') 'Rzero',(Rzero(Istk),Istk=1,Nstk)
	ELSEIF (Rzeroflag.EQ.2) THEN
	 READ(13,*) (TargDep(Istk),Istk=1,Nstk)
	 IF (Diag.EQ.1) WRITE(98,'(A8,1x,10(F5.3,1x))') 'Targ Dep',(TargDep(Istk),Istk=1,Nstk)
	ENDIF
	READ(13,*)
!	Steepness
	READ(13,*) (h(Istk),Istk=1,Nstk)
	IF (Diag.EQ.1) WRITE(98,'(A5,1x,10(F5.3,1x))') 'Steep',(h(Istk),Istk=1,Nstk)
!	SigmaR values
	READ(13,*)
	READ(13,*) (SigmaRVals(II),II=1,2)
	IF (Diag.EQ.1) WRITE(98,'(A8,1x,2(F5.3,1x))') 'SigRVals',(SigmaRVals(II),II=1,2)
	READ(13,*)
!	probability of normal recruitment
	READ(13,*) SigmaRProbs(1)
	SigmaRProbs(1)=SigmaRProbs(1)/1.d0
	SigmaRProbs(2)=1.d0-SigmaRProbs(1)
	IF (Diag.EQ.1) WRITE(98,'(A8,1x,2(F5.3,1x))') 'SigRProb',(SigmaRProbs(II),II=1,2)
	READ(13,*)
!	Correlation of rec't residuals among regions
	IF (Diag.EQ.1) WRITE(98,'(A10)') 'CorRecDevs'
	DO Ireg=1,Nreg
	 READ(13,*) (CorRecDevs(Ireg,II),II=1,Nreg)
	 IF (Diag.EQ.1) WRITE(98,'(20(F5.3,1x))') (CorRecDevs(Ireg,II),II=1,Nreg)
	ENDDO
	READ(13,*)
	READ(13,*) (RecDevFlag(II),II=1,3)
	IF (Diag.EQ.1) WRITE(98,'(I2,1x,2(I4,1x))') (RecDevFlag(II),II=1,3)
	RecDevs = 0.d0
	DO Ireg=1,Nreg
	 READ(13,*) (RecDevs(Ireg,Iyr),Iyr=RecDevFlag(2),RecDevFlag(3))
	 IF (Diag.EQ.1) WRITE(98,'(200(F7.4,1x))') (RecDevs(Ireg,Iyr),Iyr=RecDevFlag(2),RecDevFlag(3))
	ENDDO
	IF (RecDevFlag(1).EQ.0) RecDevs = 0.d0


	IF (Diag.EQ.1) WRITE(98,'(A35)') 'Done with Recruitment parms read-in'  


!	Exploitation related quantities
	Nskip=4
	DO II=1,Nskip
	 READ(13,*)
    ENDDO
!	file with retained catches by fleet and region
	READ(13,*) Catchfile
!	Get Catches
	IF (Itype.EQ.1) CALL GetHistCatch(Catchfile)
	IF (Itype.EQ.2) CALL GetAllCatch()
	IF (Diag.EQ.1) WRITE(98,'(A20)') 'Check Catches.junk'
	READ(13,*)

!	Selectivity at length
	IF (Diag.EQ.1) WRITE(98,*) 'Selex @ Len'
	DO Iflt=1,Nflt
  	 READ(13,*) (SelLen(Iflt,Ilen,Fyear),Ilen=1,Nlen)
	 IF (Diag.EQ.1) WRITE(98,'(I2,1x,100(F5.3,1x))') Iflt,(SelLen(Iflt,Ilen,Fyear),Ilen=1,Nlen)
	ENDDO
!	random walk in Selex pars
	READ(13,*)
	READ(13,*) (VarDevSel(Iflt),Iflt=1,Nflt)
	IF (Diag.EQ.1) WRITE(98,'(A9,1x,20(F6.3,1x))') 'VarDevSel',(VarDevSel(Iflt),Iflt=1,Nflt)
	READ(13,*)
	READ(13,*) (CorSel(Iflt),Iflt=1,Nflt)
	IF (Diag.EQ.1) WRITE(98,'(A9,1x,20(F6.3,1x))') 'CorSel',(CorSel(Iflt),Iflt=1,Nflt)

!	Retention
	READ(13,*) 
	READ(13,*) RETflag(1:Nflt+1)
	IF (Diag.EQ.1) WRITE(98,'(A9,1x,10(I2,1x))') 'Retenflag',RETflag(1:Nflt+1)	
	READ(13,*)
    IF (RETflag(1).EQ.1) THEN
!	 read in retention at length for each fleet
	 DO Iflt=1,Nflt
	  READ(13,*) (RetLen(Iflt,Ilen,Fyear),Ilen=1,Nlen)
      IF (Diag.EQ.1) WRITE(98,'(I2,1x,100(F5.3,1x))') Iflt,(RetLen(Iflt,Ilen,Fyear),Ilen=1,Nlen)
	 ENDDO
    ELSE
!	 read in parameters for retention by fleet as determined by Retflag(1+Iflt)
	 DO Iflt=1,Nflt
	  Npar=RETflag(1+Iflt)
	  READ(13,*) (RetGamma(Iflt,II,Fyear),II=1,Npar)
      IF (Diag.EQ.1) WRITE(98,'(I2,1x,100(F5.3,1x))') Iflt,(RetGamma(Iflt,II,Fyear),II=1,Npar)
	 ENDDO
	ENDIF

	IF (Diag.EQ.1) WRITE(98,'(A35)') 'Done with Exploitation parms read-in'  

    CLOSE(98)
	CLOSE(13)

	RETURN

	END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE GetAllCatch()

!	This subroutine reads in the catches by fleet and region from TotCatch.junk
!	it is called during the projection, so that future catches are accounted for when writing SS2.dat files

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	INTEGER Iflt,Ireg,Iyr,II
	REAL*8 DUM,Temp

	OPEN(UNIT=14,FILE='TotCatch.junk')
	OPEN(UNIT=97,FILE='Catches.junk',POSITION='APPEND')

	READ(14,*)
	
	 DO 3100 II=1,200000
 	  READ(14,*,END=3101,ERR=3101) Iyr,Iflt,Ireg,DUM,DUM,DUM,DUM,Temp
	  Catch(Iflt,Ireg,Iyr) = Temp
3100 CONTINUE

3101 CLOSE(14)
	  
	IF (Diag.EQ.1) THEN
	 WRITE(97,'(6x,120(I6,1x))') (Iyr,Iyr=Fyear,Lyear)
	 DO Iflt=1,Nflt
	  DO Ireg=1,Nreg
	   WRITE(97,'(I2,1x,I2,1x,120(F6.1,1x))') Iflt,Ireg,(Catch(Iflt,Ireg,Iyr),Iyr=Fyear,Lyear)
      ENDDO
	 ENDDO     
	 DO Ireg=1,Nreg
	  WRITE(97,'(I2,1x,I2,1x,120(F6.1,1x))') 0,Ireg,(SUM(Catch(1:Nflt,Ireg,Iyr)),Iyr=Fyear,Lyear)
     ENDDO
	 DO Iflt=1,Nflt
	  WRITE(97,'(I2,1x,I2,1x,120(F6.1,1x))') Iflt,0,(SUM(Catch(Iflt,1:Nreg,Iyr)),Iyr=Fyear,Lyear)
     ENDDO
	ENDIF

	CLOSE(97)


	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE GetHistCatch(TheFile)

!	This subroutine reads in the catches by fleet and region from the catch file
!	This file is a separate datafile from Sinatra.CTL

	IMPLICIT NONE
	INCLUDE 'Sinatra.INC'

	CHARACTER*20 TheFile
	INTEGER Iflt,Ireg,DUM,Iyr


	OPEN(UNIT=14,FILE=TheFile)
	OPEN(UNIT=97,FILE='Catches.junk')

	IF (Diag.EQ.1) WRITE(97,'(1x,A30)') 'Fleet, Region, The Catches'

	READ(14,*)

	Catch=0.d0
	IF (NoCatch.EQ.0) THEN
	 DO Iflt=1,Nflt
	  READ(14,*)
	  DO Iyr=Fyear,Lyear
	   READ(14,*) DUM,(Catch(Iflt,Ireg,Iyr),Ireg=1,Nreg)
	  ENDDO
	 ENDDO
	ENDIF

	CLOSE(14)
	
	IF (Diag.EQ.1) THEN
	 WRITE(97,'(6x,120(I6,1x))') (Iyr,Iyr=Fyear,Lyear)
	 DO Iflt=1,Nflt
	  DO Ireg=1,Nreg
	   WRITE(97,'(I2,1x,I2,1x,120(F6.1,1x))') Iflt,Ireg,(Catch(Iflt,Ireg,Iyr),Iyr=Fyear,Lyear)
      ENDDO
	 ENDDO     
	 DO Ireg=1,Nreg
	  WRITE(97,'(I2,1x,I2,1x,120(F6.1,1x))') 0,Ireg,(SUM(Catch(1:Nflt,Ireg,Iyr)),Iyr=Fyear,Lyear)
     ENDDO
	 DO Iflt=1,Nflt
	  WRITE(97,'(I2,1x,I2,1x,120(F6.1,1x))') Iflt,0,(SUM(Catch(Iflt,1:Nreg,Iyr)),Iyr=Fyear,Lyear)
     ENDDO
	ENDIF

	CLOSE(97)

	RETURN

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine places the input vectors into the correct variables for the future projection
!	
	SUBROUTINE AssignInputs(Yr1,Numbers,Mort,Mulen,Siglen,Wt,Fec,Move,Sel,Ret,Qdevs,S0)

	IMPLICIT NONE

	INCLUDE 'Sinatra.INC'

!	local variables
	INTEGER Yr1,Istk,Ireg,Sex,Age,Iflt,Ilen,Jreg
	REAL*8 Numbers(Nstock,Nzone,2,TopAge+1),Mort(Nstock,Nzone,2,TopAge+1),Mulen(Nstock,2,TopAge+1)
	REAL*8 Siglen(Nstock,2,TopAge+1),Wt(Nstock,2,TopAge+1),Fec(Nstock,TopAge+1),S0(2,Nstock)
	REAL*8 Move(Nstock,Nzone,Nzone,2,TopAge+1),Sel(Nfleet,NlenBin),Ret(Nfleet,Nlenbin),Qdevs(Nfleet,Nzone)


!	Numbers
	N(1:Nstk,1:Nreg,1:2,0:MaxAge,Yr1) = Numbers(1:Nstk,1:Nreg,1:2,1:MaxAge+1)
!	Natural Mortality rate
	M(1:Nstk,1:Nreg,1:2,0:MaxAge,Yr1-1) = Mort(1:Nstk,1:Nreg,1:2,1:MaxAge+1)	
!	Mean & SD Length
	MeanLenAge(1:Nstk,1:2,0:MaxAge,Yr1) = Mulen(1:Nstk,1:2,1:MaxAge+1)
	SigmaLenAge(1:Nstk,1:2,0:MaxAge,Yr1) = Siglen(1:Nstk,1:2,1:MaxAge+1)
!	Weight
	Weight(1:Nstk,1:2,0:MaxAge,Yr1)	= Wt(1:Nstk,1:2,1:MaxAge+1)
!	Fecundity
	Fecundity(1:Nstk,0:MaxAge,Yr1) = Fec(1:Nstk,1:MaxAge+1)
!	X
	X(1:Nstk,1:Nreg,1:Nreg,1:2,0:MaxAge,Yr1-1) = Move(1:Nstk,1:Nreg,1:Nreg,1:2,1:MaxAge+1)
!	Selectivity at length
	SelLen(1:Nflt,1:Nlen,Yr1-1) = Sel(1:Nflt,1:Nlen) 
!	Retention at length
	RetLen(1:Nflt,1:Nlen,Yr1-1) = Ret(1:Nflt,1:Nlen)
!	Q devs
	CpueQdevs(1:Nflt,1:Nreg,Yr1-1) = Qdevs(1:Nflt,1:Nreg)
!	SB zero
	SBioZero(1:Nstk) = S0(1,1:Nstk)
!	Rzero
	Rzero(1:Nstk) = S0(2,1:Nstk)

!	write them to file if checking diagnostics
	IF (Diag.EQ.1) THEN
	 OPEN(UNIT=98,FILE='Assignment.junk',POSITION='APPEND')
	 WRITE(98,*)
	 WRITE(98,*) Yr1
	 WRITE(98,*) 'N'
	 DO 3000 Istk=1,Nstk
	  DO 3000 Ireg=1,Nreg
	   DO 3000 Sex=1,2
	    WRITE(98,'(I4,1x,I2,1x,I2,1x,I1,1x,100(F10.2,1x))') Yr1,Istk,Ireg,Sex,(N(Istk,Ireg,Sex,Age,Yr1),Age=0,MaxAge)
3000 CONTINUE
	 WRITE(98,*) 'M'
	 DO 3001 Istk=1,Nstk
	  DO 3001 Sex=1,2
	   DO 3001 Ireg=1,Nreg
	    WRITE(98,'(I4,1x,I2,1x,I1,1x,I2,1x,100(F5.3,1x))') Yr1-1,Istk,Sex,Ireg,(M(Istk,Ireg,Sex,Age,Yr1-1),Age=0,MaxAge)
3001 CONTINUE
	 WRITE(98,*) 'Length'
	 DO 3002 Istk=1,Nstk
	  DO 3002 Sex=1,2	 
  	   WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Yr1,Istk,Sex,(MeanLenAge(Istk,Sex,Age,Yr1),Age=0,MaxAge) 
3002 CONTINUE
	 WRITE(98,*) 'Sigma Length'
	 DO 3003 Istk=1,Nstk
	  DO 3003 Sex=1,2	 
  	   WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Yr1,Istk,Sex,(SigmaLenAge(Istk,Sex,Age,Yr1),Age=0,MaxAge) 
3003 CONTINUE
	 WRITE(98,*) 'Weight'
	 DO 3004 Istk=1,Nstk
	  DO 3004 Sex=1,2	 
  	   WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Yr1,Istk,Sex,(Weight(Istk,Sex,Age,Yr1),Age=0,MaxAge) 
3004 CONTINUE
	 WRITE(98,*) 'Fecundity'
	 DO 3005 Istk=1,Nstk
  	   WRITE(98,'(I4,1x,I2,1x,I1,1x,100(F6.2,1x))') Yr1,Istk,Sex,(Fecundity(Istk,Age,Yr1),Age=0,MaxAge) 
3005 CONTINUE
	 WRITE(98,*) 'X'
 	 DO 3006 Istk=1,Nstk
	  DO 3006 Sex=1,2
	   DO 3006 Age =0,MaxAge
	    DO 3006 Ireg=1,Nreg
		 WRITE(98,'(I4,1x,I2,1x,I1,1x,I3,1x,I2,1x,20(F5.3,1x))') Yr1-1,Istk,Sex,Age,Ireg,(X(Istk,Ireg,Jreg,Sex,Age,Yr1-1),Jreg=1,Nreg) 
3006 CONTINUE		  
	 WRITE(98,*) 'Selex at length'
	 DO 3007 Iflt=1,Nflt
      WRITE(98,'(I2,1x,I4,1x,100(F6.4,1x))') Iflt,Yr1-1,(SelLen(Iflt,Ilen,Yr1-1),Ilen=1,Nlen)
3007 CONTINUE
	 WRITE(98,*) 'Retention at length'
	 DO 3008 Iflt=1,Nflt
      WRITE(98,'(I2,1x,I4,1x,100(F6.4,1x))') Iflt,Yr1-1,(RetLen(Iflt,Ilen,Yr1-1),Ilen=1,Nlen)
3008 CONTINUE
	 WRITE(98,*) 'SB zero'
	 WRITE(98,*) (SBiozero(Istk),Istk=1,Nstk)
	 WRITE(98,*) 'R zero'
	 WRITE(98,*) (Rzero(Istk),Istk=1,Nstk)
	 CLOSE(98)
	ENDIF

	RETURN

	END 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	This subroutine assigns necessary quantities to their output variables
!	
    SUBROUTINE AssignOutputs(Yr2,Numbers,Mort,Mulen,Siglen,Wt,Fec,Move,Sel,Ret,Qdevs,S0)

	IMPLICIT NONE

	INCLUDE 'Sinatra.INC'

!	local variables
	INTEGER Yr2,Istk,Ireg,Sex,Age,Iflt,Ilen,Jreg
	REAL*8 Numbers(Nstock,Nzone,2,TopAge+1),Mort(Nstock,Nzone,2,TopAge+1),Mulen(Nstock,2,TopAge+1)
	REAL*8 Siglen(Nstock,2,TopAge+1),Wt(Nstock,2,TopAge+1),Fec(Nstock,TopAge+1),S0(2,Nstock)
	REAL*8 Move(Nstock,Nzone,Nzone,2,TopAge+1),Sel(Nfleet,NlenBin),Ret(Nfleet,Nlenbin),Qdevs(Nfleet,Nzone)


!	Numbers
	Numbers(1:Nstk,1:Nreg,1:2,1:MaxAge+1) = N(1:Nstk,1:Nreg,1:2,0:MaxAge,Yr2+1)
!	Natural Mortality rate
	Mort(1:Nstk,1:Nreg,1:2,1:MaxAge+1) = M(1:Nstk,1:Nreg,1:2,0:MaxAge,Yr2)	
!	Mean & SD Length
	Mulen(1:Nstk,1:2,1:MaxAge+1) = MeanLenAge(1:Nstk,1:2,0:MaxAge,Yr2+1)
	Siglen(1:Nstk,1:2,1:MaxAge+1) = SigmaLenAge(1:Nstk,1:2,0:MaxAge,Yr2+1)
!	Weight
	Wt(1:Nstk,1:2,1:MaxAge+1) = Weight(1:Nstk,1:2,0:MaxAge,Yr2+1)
!	Fecundity
	Fec(1:Nstk,1:MaxAge+1) = Fecundity(1:Nstk,0:MaxAge,Yr2+1)
!	X
	Move(1:Nstk,1:Nreg,1:Nreg,1:2,1:MaxAge+1) = X(1:Nstk,1:Nreg,1:Nreg,1:2,0:MaxAge,Yr2)
!	Selectivity at length
	Sel(1:Nflt,1:Nlen) = SelLen(1:Nflt,1:Nlen,Yr2)
!	Retention at length
	Ret(1:Nflt,1:Nlen) = RetLen(1:Nflt,1:Nlen,Yr2)
!	Q devs
	Qdevs(1:Nflt,1:Nreg) = CpueQdevs(1:Nflt,1:Nreg,Yr2)
!	SB zero
	S0(1,1:Nstk) = SBioZero(1:Nstk)
!	R zero
	S0(2,1:Nstk) = Rzero(1:Nstk)

	RETURN

	END 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
