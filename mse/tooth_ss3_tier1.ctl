##############################################################################################
#tier 1/2 specifications
1
#############################################################################################
#number of years to not estimate recruitment
7
#number of years to avergae over to calculate discard rate
4
#type of assessment 
1 #1= 1 assessment, aggregate regions, assessment fleets are gears, #2= 1 assessment, assessment fleets are unique combinations of gear and region
#string of fleet names
AT_Trawl%NV_Trawl%AT_LL%NMR_LL%SMR_LL
#ss3 ctl file
2area.ctl
# Number of Aging Error Matrices
0.5	1.5	2.5	3.5	4.5	5.5	6.5	7.5	8.5	9.5	10.5	11.5	12.5	13.5	14.5	15.5	16.5	17.5	18.5	19.5	20.5	21.5	22.5	23.5	24.5	25.5	26.5	27.5	28.5	29.5	30.5	31.5	32.5	33.5	34.5	35.5	36.5	37.5	38.5	39.5	40.5	41.5	42.5	43.5	44.5	45.5	46.5	47.5	48.5	49.5
0.8087	0.8164	0.8266	0.8392	0.8543	0.8717	0.8916	0.9139	0.9386	0.9657	0.9953	1.0272	1.0616	1.0984	1.1376	1.1793	1.2233	1.2698	1.3187	1.37	1.4237	1.4799	1.5385	1.5995	1.6629	1.7287	1.797	1.8676	1.9407	2.0162	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942	2.0942
#tag loss line
-10 10 -9 9 1 0.001 -4 0 0 0 0 0 0 0 # TG_loss_init_1_
#chronic loss line
-10 10 -9 9 1 0.001 -4 0 0 0 0 0 0 0 # TG_loss_chronic_1_
#tag overdispersion line
1 100 1.001 2 1 0.001 -4 0 0 0 0 0 0 0 # TG_overdispersion_1_    #-10 10 0 0 1 0.001 -4 0 0 0 0 0 0 0 # TG_overdispersion_1_
#tag report rate by fleet
-10 10 9.5 9 1 0.001 -4 0 0 0 0 0 0 0 # TG_report_fleet:_1_
#decay in report rate (all fleets)
-4 4 0 0 0 2 -4 0 0 0 0 0 0 0 # TG_rpt_decay_fleet:_1_
#assumed fixed tag age, negative integer indicates number of tag subgroups, with age determined from median length given assumed growth, -99 indicates ages for each length bin, -101 indicates true age structure
-5 -5 -5 -5 -5
#guessed VB parameters
185.5 
0.042
-0.781
#CCAMLR projection
#n yrs
36
#maximum catch by fleet
1000000 1000000 100000 100000 100000 100000 100000 
#method of catch (1=(equal C by fleet),2=south first yr, north 2nd yr, etc.,3=70:30 S:N)
0             
#future tag rate
3