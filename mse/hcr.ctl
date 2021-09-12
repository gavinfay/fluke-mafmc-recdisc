##########################################################################################
#control file for harvest strategy specifications
#
#G.Fay 
#University of Washington
#6/28/2007
#
#########################################################################################
#Number of years for projection
30
#frequency of assessment, update RBC (every n yrs)
1
#tier rule
61
#number of years to base allocation estimation on
1
#min catch in mt (ie if RBC = 0)
1
#allocation overdispersion
0
#CV in implementation error on TAC  (mu=catch/msy = a*(tac/msy) + b , CV=c), i.e. no implementation error is c(1,0,0)
1 0 0 #0.612 0.147 0.41 #(0.267)
#discount factor (%)
0
#N yrs for CPUE stability
-1
#CV of CPUE for not using discount
0.2
#MaxCHange
-1 #0.5
#######################
