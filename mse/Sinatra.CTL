###################################################
#Control file for SESSF Operating model 'Sinatra'
# Summer flounder (MAFMC recreational discard MSE)
# G.Fay
# UMass Dartmouth
# Last updated: 05/10/2021 from F2021_2019_v1_MCMC.dat
#
###################################################
#Show detailed Diagnostic files? 1=Yes,0=No
1
#Check equilibrium by setting catches to zero. 1=Yes, 0=No
0
#
#Model Structure
#
#Number of regions
1
#Number of stocks
1
#Allocation of recruits in stocks to regions (lamda zero, rows are stocks)
1
#Number of fleets
4
#Allocation of fleets to region (rows are fleets)
1
1
1
1
#
#Length bin structure
#Number of length bins
42
#Lower length limit of length bins
10	12	14	16	18	20	22	24	26	28	30	32	34	36	38	40	42	44	46	48	50	52	54	56	58	60	62	64	66	68	70	72	74	76	78	80	82	84	86	88	90	92
#Upper limit of top bin
94
#
#Years for historical (pre-HSF) period
#1st year of historical period
2019
#last year of available catch
2019
#######################
#BIOLOGICAL PARAMETERS#
#######################
#Maximum age, x
7
#Natural Mortality by stock and age
#0 1 2 3 4 5 6 7 8 9 10 
0.26	0.26	0.26	0.25	0.25	0.25	0.25	0.24
#Input length at age directly (=1, use growth parameters to calculate=0)
1
#Mean lengths at age by stock and sex (fem,mal)
6.9371	19.0716	29.2072	37.6731	44.7444	50.6509	55.5844	59.7052
5.4998	15.1201	23.1556	29.8674	35.4736	40.1562	44.0676	47.3345
#growth parameters, Linfin,K,tzero by stock and sex
80.6	0.18	-0.1
63.9	0.18	-0.1
#Input s.d. at age directly (=1, use CV parameters to calculate=0)
1
#std dev of lengths at age by stock and sex (fem,mal)
0.6937	1.9072	2.9207	3.7673	4.4744	5.0651	5.5584	5.9705
0.5500	1.5120	2.3156	2.9867	3.5474	4.0156	4.4068	4.7335
#CV 0 & x by stock and sex
0.1 0.1
0.1 0.1
#Input weight at age directly=1, use parameters to calculate=0
1
#Mean Weights at age at start of year by stock and sex (fem,mal)
0.1349	0.3188	0.5747	0.805	1.0652	1.3858	1.6724	2.0643
0.1349	0.3188	0.5747	0.805	1.0652	1.3858	1.6724	2.0643
#Mean Weights at age in middle of year by stock and sex (fem,mal)
0.1349	0.3188	0.5747	0.805	1.0652	1.3858	1.6724	2.0643
0.1349	0.3188	0.5747	0.805	1.0652	1.3858	1.6724	2.0643
#W@L Pars (alpha & beta) by stock and sex
0.00000244  3.34694
0.00000244  3.34694
#SSB definiftion / Fecundity type, maximum fecundity (1=Biomass, 2 = max fecundity), 2nd number is max number of offspring (used when FecType = 2)
1 100
#Fecundity at Age by stock
0.132686938	0.225051705	0.355368219	0.511352192	0.66515624	0.790393553	0.877422014	0.931450184	0.962677545	0.979985185	0.989355454	0.994364105	0.997023092	0.998429564	0.999172085	0.999563686	0.999770104	0.999878879	0.99993619	0.999966384	0.999982291	0.999990671	0.999995085	0.999997411	0.999998636	0.999999282	0.999999622	0.999999801	0.999999895	0.999999945	0.999999971	0.999999985	0.999999992	0.999999996	0.999999998	0.999999999	0.999999999	1	1	1	1	1
#Getting the X Movement Matrix
#maximum avg. movers under uniform connectivity by stock and sex (each line is stock)
0.5
#relative age-specific movement by stock and sex
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1 1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1 1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
#Rzero flag (1= read in R0 by stock, 2= read in Target Depletion by stock
1
#Target Depletion #Rzero by stock
50290  #0.4	#12727.4964 #
#Steepness, h
0.99
#Sigma R values
0.8 1.5
#probability of 'normal' recruitment
1
#correlation of recruitment residuals among regions
1 1
#recruitment dev flag (if 1st =1, then input devs directly for year range, if 2 read n at age for iniital year
2 2019 2019
0.32
#################################
#EXPLOITATION-RELATED QUANTITIES#
#################################
#Filename with Retained catch by fleet and region for 'historical' years
fluke_om_catch.dat
#Initial Selectivity at Length by fleet
0.0230	0.0308	0.0411	0.0547	0.0724	0.0953	0.1246	0.1611	0.2059	0.2592	0.3208	0.3894	0.4626	0.5374	0.6106	0.6792	0.7408	0.7941	0.8389	0.8754	0.9047	0.9276	0.9453	0.9589	0.9692	0.9770	0.9829	0.9873	0.9905	0.9930	0.9948	0.9961	0.9971	0.9979	0.9984	0.9988	0.9991	0.9994	0.9995	0.9996	0.9997	0.9998
0.0230	0.0308	0.0411	0.0547	0.0724	0.0953	0.1246	0.1611	0.2059	0.2592	0.3208	0.3894	0.4626	0.5374	0.6106	0.6792	0.7408	0.7941	0.8389	0.8754	0.9047	0.9276	0.9453	0.9589	0.9692	0.9770	0.9829	0.9873	0.9905	0.9930	0.9948	0.9961	0.9971	0.9979	0.9984	0.9988	0.9991	0.9994	0.9995	0.9996	0.9997	0.9998
0.0230	0.0308	0.0411	0.0547	0.0724	0.0953	0.1246	0.1611	0.2059	0.2592	0.3208	0.3894	0.4626	0.5374	0.6106	0.6792	0.7408	0.7941	0.8389	0.8754	0.9047	0.9276	0.9453	0.9589	0.9692	0.9770	0.9829	0.9873	0.9905	0.9930	0.9948	0.9961	0.9971	0.9979	0.9984	0.9988	0.9991	0.9994	0.9995	0.9996	0.9997	0.9998
0.0230	0.0308	0.0411	0.0547	0.0724	0.0953	0.1246	0.1611	0.2059	0.2592	0.3208	0.3894	0.4626	0.5374	0.6106	0.6792	0.7408	0.7941	0.8389	0.8754	0.9047	0.9276	0.9453	0.9589	0.9692	0.9770	0.9829	0.9873	0.9905	0.9930	0.9948	0.9961	0.9971	0.9979	0.9984	0.9988	0.9991	0.9994	0.9995	0.9996	0.9997	0.9998
#Variance in random walk in selectivity over time by fleet
0 0 0 0 0 0 0 0
#Omega par by fleet- par determining correlation in r.w. among length bins (0=perfect corr)
0 0 0 0 0 0 0 0
#Retention flags (1st: 1=indicates whether read-in by length, 2=params, subsequent numbers by fleet indicate type of retention 1=market,2=size,3=both)
1 1 1 1 1
#Retention patterns (1's for no retention pattern)/parameters
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
#####################################################################################


