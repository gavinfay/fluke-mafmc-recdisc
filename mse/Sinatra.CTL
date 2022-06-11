###################################################
#Control file for SESSF Operating model 'Sinatra'
# Summer flounder (MAFMC recreational discard MSE)
# G.Fay
# UMass Dartmouth
# Last updated: 05/10/2021 from F2021_2019_v1_MCMC.dat
#
###################################################
#Show detailed Diagnostic files? 1=Yes,0=No
0
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
2
#Mean lengths at age by stock and sex (fem,mal)
25.0908	34.2061	41.7744	48.0581	53.2753	57.6070	61.2035	64.1896
25.4848	32.0363	36.8367	40.3540	42.9312	44.8195	46.2031	47.2169
#growth parameters, Linfin,K,tzero by stock and sex
78.797 0.186 -1.561
49.996 0.311 -1.792
#Input s.d. at age directly (=1, use CV parameters to calculate=0)
2
#std dev of lengths at age by stock and sex (fem,mal)
3.7636	5.1309	6.2662	7.2087	7.9913	8.6411	9.1805	9.6284
3.8227	4.8054	5.5255	6.0531	6.4397	6.7229	6.9305	7.0825
#CV 0 & x by stock and sex
0.15 0.15
0.15 0.15
#Input weight at age directly=1, use parameters to calculate=0
2
#Mean Weights at age at start of year by stock and sex (fem,mal)
0.0603	0.2345	0.5309	0.9231	1.3747	1.8519	2.3276	2.7826
0.0851	0.2286	0.4044	0.5809	0.7396	0.8734	0.9814	1.0662
#Mean Weights at age in middle of year by stock and sex (fem,mal)
0.1313	0.3686	0.7172	1.1437	1.6120	2.0913	2.5586	2.9984
0.1504	0.3148	0.4940	0.6631	0.8098	0.9305	1.0265	1.1010
#W@L Pars (alpha & beta) by stock and sex
0.00000287	3.33
0.00000434	3.228
#SSB definiftion / Fecundity type, maximum fecundity (1=Biomass, 2 = max fecundity), 2nd number is max number of offspring (used when FecType = 2)
1 100
#Maturity at Length by stock
3.55418E-09	2.67085E-08	2.00706E-07	1.50824E-06	1.13338E-05	8.51635E-05	0.000639621	0.0047866	0.03488202	0.213589854	0.671160187	0.938790794	0.99139827	0.998846743	0.999846379	0.999979554	0.999997279	0.999999638	0.999999952	0.999999994	0.999999999	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
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
#Initial Selectivity at Length by fleet: comland, comdisc, recland, recdisc
3.29764E-85	1.29442E-79	5.08095E-74	1.99442E-68	7.82865E-63	3.07297E-57	1.20623E-51	4.73478E-46	1.85854E-40	7.29528E-35	2.8636E-29	1.12404E-23	4.41219E-18	1.73191E-12	6.79823E-07	0.210640631	0.999990453	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
5.95562E-64	4.95052E-59	4.11505E-54	3.42057E-49	2.8433E-44	2.36345E-39	1.96459E-34	1.63303E-29	1.35744E-24	1.12835E-19	9.37924E-15	7.79636E-10	6.48019E-05	0.843429835	0.999997767	1	1	1	1	1	1	1	1	1	0.999999854	0.999126769	0.160640177	3.20116E-05	5.3547E-09	8.95674E-13	1.49818E-16	2.50599E-20	4.19174E-24	7.01147E-28	1.1728E-31	1.96173E-35	3.28135E-39	5.48868E-43	9.18084E-47	1.53567E-50	2.56869E-54	4.29661E-58
2.26012E-80	2.13063E-75	2.00856E-70	1.89349E-65	1.785E-60	1.68274E-55	1.58633E-50	1.49544E-45	1.40976E-40	1.329E-35	1.25285E-30	1.18108E-25	1.11341E-20	1.04962E-15	9.89483E-11	9.32784E-06	0.467901345	0.999987937	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1.06479E-50	8.46413E-46	6.72824E-41	5.34835E-36	4.25147E-31	3.37955E-26	2.68644E-21	2.13548E-16	1.69752E-11	1.34938E-06	0.096872899	0.999882733	0.999999999	1	1	1	1	1	1	1	0.999999997	0.999839179	0.117920984	2.8746E-06	6.18122E-11	1.32914E-15	2.85802E-20	6.14555E-25	1.32147E-29	2.84153E-34	6.11009E-39	1.31384E-43	2.82513E-48	6.07482E-53	1.30626E-57	2.80882E-62	6.03977E-67	1.29872E-71	2.79261E-76	6.00491E-81	1.29123E-85	2.7765E-90
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


3.782648e-14 2.668516e-13 1.882538e-12 1.32806e-11 9.368965e-11 6.609453e-10 4.662721e-09 3.289375e-08 2.32053e-07 1.637045e-06 1.154862e-05 8.14655e-05 0.0005744252 0.004038309 0.02780882 0.1679095 0.5873855 0.9094428 0.9860817 0.9980032 0.9997165 0.9999598 0.9999943 0.9999992 0.9999999 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

9.914621e-20 1.049247e-18 1.1104e-17 1.175118e-16 1.243607e-15 1.316087e-14 1.392793e-13 1.473969e-12 1.559876e-11 1.650789e-10 1.747002e-09 1.848822e-08 1.956576e-07 2.070607e-06 2.191244e-05 0.0002318469 0.002448157 0.02531454 0.2155985 0.7441649 0.9685366 0.9969398 0.99971 0.9999726 0.9999974 0.9999998 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
