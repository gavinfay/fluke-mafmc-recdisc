#projection plot
projection.plot <- function(project.results) {
  project.results <- project.results %>% 
    #filter(type %in% c("biomass","catch")) %>% 
    I()
  project.results %>% 
    #group_by(scenario, type, year) %>% 
    #median_qi(value, .width = c(.5, .8, .95)) %>%
    ggplot() +  
    stat_lineribbon(aes(x = year, y = value, fill = scenario),
                    #show.legend = FALSE,
                    alpha = 0.35) +
    # geom_lineribbon(aes(x = year, y = value, ymin = .lower, ymax = .upper, fill = scenario),
    #                 show.legend = FALSE) +
    scale_fill_brewer(type = "qual", palette = 2) +
    facet_wrap(~type, scale = "free") + 
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==1), lty=1,lwd=0.5,col=gray(0.7),alpha=0.5) +
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==2), lty=1,lwd=0.5,col=gray(0.7),alpha=0.5) +
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==3), lty=1,lwd=0.5,col=gray(0.7),alpha=0.5) +
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==4), lty=1,lwd=0.5,col=gray(0.7),alpha=0.5) +
    ylim(0,NA) + 
    ylab("") + 
    theme_bw() +
    theme(legend.position="bottom") + #,
          #axis.text.y = element_blank()) +
    labs(fill = "")
}

# add radar plot
do_radar_plot <- function(metrics) {
  # metrics is a dataframe of metrics, expecting columns
  # metrics <- tibble(metric = rep(letters[1:5],each=3),
  #                   value = runif(15),
  #                   mp = as.character(rep(1:3,5)))
  # # metric 
  # # value
  # # mp (management procedure)
  summaries <- metrics %>% 
    group_by(metric) %>% 
    summarize(min = min(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE)) %>% 
    #pivot_longer(cols = c(2:3),names_to = "mp",
    #             values_to = "value") %>% 
    #pivot_wider(names_from = metric, values_from = value) %>% 
    #arrange(desc(mp)) %>% 
    I()
  #summaries  
  nmetrics <- length(unique(metrics$metric))
  nmp <- length(unique(metrics$mp))
  
  d <- metrics %>% 
    group_by(mp) %>% 
    left_join(summaries) %>% 
    mutate(value = value/(max + 1e-08)) %>% 
    select(mp, metric, value) %>% 
    pivot_wider(names_from = metric, values_from = value) %>% 
    ungroup()
  
  bounds <- tibble(mp = c("max","min"),
                   "spawning biomass" = c(1,0),
                   "total catch" = c(1,0),
                   "kept:released" = c(1,0),
                   "kept per trip" = c(1,0),
                   "not overfishing" = c(1,0),
                   "not overfished" = c(1,0),
                   "expense" = c(1,0),
                   "mulen_keep" = c(1,0),
                   "mulen_release" = c(1,0),
                   "trophy" = c(1,0),
                   "rec_removals" = c(1,0),
                   "keep_one" = c(1,0),
                   "ntrips" = c(1,0)) #,
  #d = c(1,0),
  #e = c(1,0))
  
  dd <- bounds %>% 
    bind_rows(d)
  
  #NEW PLOT
  #colorblind colors
  colors_fill2<-c(alpha("#000000",0.1),
                  alpha("#E69F00",0.1),
                  alpha("#56B4E9",0.1),
                  alpha("#009E73",0.1),
                  alpha("#F0E442",0.1),
                  alpha("#E69F00",0.1),
                  alpha("#56B4E9",0.1),
                  alpha("#009E73",0.1))
  colors_line2<-c(alpha("#000000",0.9),
                  alpha("#E69F00",0.9),
                  alpha("#56B4E9",0.9),
                  alpha("#009E73",0.9),
                  alpha("#F0E442",0.9),
                  alpha("#E69F00",0.9),
                  alpha("#56B4E9",0.9),
                  alpha("#009E73",0.9))
  colors_line2<-c(alpha("#1b9e77",0.9),
                  alpha("#d95f02",0.9),
                  alpha("#56B4E9",0.9),
                  alpha("#dc14cf",0.9),
                  #alpha("#009E73",0.9),
                  alpha("#F0E442",0.9),
                  alpha("#E69F00",0.9),
                  alpha("#56B4E9",0.9),
                  alpha("#009E73",0.9))
  
  
  #d95f02
  
  #colorblind
  radarchart(dd[,-1],seg=5,pcol=colors_line2,
             pfcol=colors_fill2,plwd=2,
             vlabels=names(dd)[-1], vlcex=0.8,
             plty=c(rep(1,nmetrics),rep(2,nmetrics)),
             pdensity=0)
  rows<<-rownames(dd[-c(1,2),])
  colors_line<<-colors_line2
  legend("bottomright",inset=0,title ="",title.adj = 0.2,
         legend=unique(d$mp),
         pch=16,
         col=colors_line2[1:nmp],
         lty=1, cex=0.8, bty= 'n', y.intersp=1)
  
}

#
library(tidyverse)
library(ggdist)
library(scales)
library(fmsb)


### general scenario
### 
read_results <- function(scendir = "~/Dropbox/fluke-mse/07-01/",scen.name="mgmt_scenario_7",
                         fsim = 1) {
  
# scendir <- "~/Dropbox/fluke-mse/sims/2022-05-24/01-01/"
# scen.name="MP1"
# fsim = 1

spawbio <- read.table(paste0(scendir,"spawbio.out"), header = TRUE)
totcatch <- read.table(paste0(scendir, "totcatch.out"), header = TRUE)
recoutput <- read.table(paste0(scendir, "recoutput.out"), header = FALSE, skip = 1) #TRUE)
recoutput2 <- read.table(paste0(scendir, "recoutput2.out"), header = FALSE, skip = 1) #TRUE)
rbctrack <- read.table(paste0(scendir, "rbctrack.out"), header = FALSE, skip = 1)
fout <- read.table(paste0(scendir,"frate.out"), header = TRUE)
names(spawbio) <- str_sub(names(spawbio), start=2)
names(totcatch) <- str_sub(names(totcatch), start=2)
names(fout) <- str_sub(names(fout), start=2)
results <- spawbio %>% 
  as_tibble() %>% 
  rowid_to_column(var = "isim") %>% 
  pivot_longer(cols= 2:ncol(.), names_to = "year", values_to = "biomass") %>% 
  mutate(year = as.integer(year)) %>% 
  I()
res2 <- totcatch %>% 
  tibble() %>% 
  #rowid_to_column(var = "isim") %>% 
  mutate(isim = rep(1:(nrow(.)/4),each=4)) %>% 
  rename(fleet = L) %>%
  select(isim,everything()) %>% 
  pivot_longer(cols= 3:ncol(.), names_to = "year", values_to = "catch") %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(isim, year) %>% 
  summarize(totcat_wt = sum(catch),
             keep_wt = sum(catch*(fleet==3)),
             release_wt = sum(catch*(fleet==4))) %>% 
  #pivot_wider(names_from = fleet, names_glue = "fleet_{fleet}", values_from = catch) %>% 
  I()
res2
results <- results %>% 
  left_join(res2)

#recoutput - getting rid of extra outpt from iscen=1 & unfinished simulations
nyrs <- length(unique(recoutput[,2]))
# dummy <- recoutput %>% 
#   tibble() %>% 
#   slice(1:nyrs)
# dummy2 <- recoutput %>% 
#   tibble() %>% 
#   slice(((nyrs+1):nrow(.))) %>% 
#   filter(V1 != 1)
# temp <- bind_rows(dummy,dummy2)
# nsim <- floor(nrow(temp)/nyrs)
# recoutput <-  temp %>% 
#   slice((1:(nsim*nyrs)))

recoutput[,1] <- rep(1:100, each = 2*nyrs)
res3 <- recoutput[,1:4] %>% as_tibble()
names(res3) <- c("isim", "year", "type", "number") #,"trips")
res3 <- res3 %>% 
  mutate(#year = 2019 + year,
    type = ifelse(type==1, "keep_num", "release_num")) %>% 
  pivot_wider(names_from = "type", values_from = "number") %>% 
  I()


#recoutput2 - getting rid of extra outpt from iscen=1 & unfinished simulations
nyrs <- length(unique(recoutput2[,2]))
# dummy <- recoutput2 %>% 
#   tibble() %>% 
#   slice(1:(10*nyrs))
# dummy2 <- recoutput2 %>% 
#   tibble() %>% 
#   slice(((10*nyrs+1):nrow(.))) %>% 
#   filter(V1 != 1)
# temp <- bind_rows(dummy,dummy2)
# nsim <- floor(nrow(temp)/nyrs)
# recoutput2 <-  temp %>% 
#   slice((1:(nsim*nyrs*10)))
recoutput2[,1] <- rep(1:100, each = 10*nyrs)
names(recoutput2) <- c("isim","year","state","ntrips","nchoice","change_cs","cost",
                       "keep_one", "mulen_keep","mulen_release","trophy")
coast_recoutput <- recoutput2 %>% 
  filter(state == 1)

res3 <- recoutput[,1:4] %>% as_tibble()
names(res3) <- c("isim", "year", "type", "number") #,"trips")
res3 <- res3 %>% 
  mutate(#year = 2019 + year,
    type = ifelse(type==1, "keep_num", "release_num")) %>% 
  pivot_wider(names_from = "type", values_from = "number") %>% 
  left_join(coast_recoutput) %>% 
  I()

results <- results %>% 
  left_join(res3)

rbc2 <- rbctrack %>% 
  tibble() %>% 
  select(1,2,4,15,9,10,11,12,18) %>% 
  I()
names(rbc2) <- c("isim","year","abc","fmsy","est_ofl","true_ofl","rhl","bmsy","msy")
rbc2 <- rbc2 %>% 
  mutate(year = 2019 + 2*year - 1) %>% 
  I()
results <- results %>% 
  left_join(rbc2)
  #mutate(scenario = rep("coastwide 14",nrow(.)))

temp <- results %>% 
  filter(year!=2019) %>% 
  fill("abc","fmsy","est_ofl","true_ofl","rhl","bmsy","msy")

results <- results %>% 
  filter(year==2019) %>% 
  bind_rows(temp) %>% 
  arrange(isim,year)


# refpts <- read.table("mse/coast14/refpts.out", header = FALSE, skip = 1)
# names(refpts) <- c("isim", "fref", "bref", "cref")
# coast_results <- coast_results %>% 
#   left_join(refpts)

res4 <- fout %>% 
  as_tibble() %>% 
  rowid_to_column(var = "isim") %>% 
  pivot_longer(cols= 2:ncol(.), names_to = "year", values_to = "frate") %>% 
  mutate(year = as.integer(year)) %>% 
  I()
results <- results %>% 
  left_join(res4)


results <- results %>% 
  mutate(scenario = rep(scen.name,nrow(.)),
         isim = isim + fsim -1)

return(results)
}

#results <- read_results(scendir = "~/Dropbox/fluke-mse/07-01/",scen.name="mgmt_scenario_7")

get_diag_ts <- function(results) {
  
diag_ts <- results %>% 
  mutate(keep_to_rel = keep_num/release_num,
         "kept per trip" = 1000*keep_num/ntrips,
         "F/Fref" = frate/fmsy,
         "B/Bref" = biomass/bmsy,
         "expense" = cost,
         cs_per_trip = change_cs/ntrips,
         rec_removals = keep_wt + release_wt,
         keep_one = keep_one/ntrips)  %>% 
  select(scenario, isim, year, biomass, totcat_wt, rec_removals, keep_to_rel, "kept per trip", "F/Fref", "B/Bref",
         mulen_keep,
         mulen_release,
         trophy,
         expense,
         change_cs,
         ntrips,
         keep_one,
         cs_per_trip
         ) %>% 
  rename("spawning biomass" = biomass,
         "total catch" = totcat_wt,
         "kept:released" = keep_to_rel) %>% 
  pivot_longer(cols=c("spawning biomass","total catch","kept:released", "kept per trip", 
                      "F/Fref", "B/Bref",
                      rec_removals,
                      mulen_keep,
                      mulen_release,
                      trophy,
                      expense,
                      change_cs,
                      ntrips,
                      keep_one,
                      cs_per_trip
                      ),names_to = "type", values_to = "value") %>% 
  I()
 return(diag_ts)
}


#results <- read_results(scendir = "~/Dropbox/fluke-mse/07-01/",scen.name="mgmt_scenario_7")

#change the folder names for the scenarios you want to summarize results for and give them a unique name
# params <- list(
# scendir <- c("~/Dropbox/fluke-mse/01-01/",
#              "~/Dropbox/fluke-mse/04-01/",
#              "~/Dropbox/fluke-mse/07-01/",
#              "~/Dropbox/fluke-mse/sims/01-02/",
#              "~/Dropbox/fluke-mse/sims/01-03/",
#              #"~/Dropbox/fluke-mse/sims/01-04/",
#              "~/Dropbox/fluke-mse/sims/04-02/",
#              "~/Dropbox/fluke-mse/sims/04-03/",
#              #"~/Dropbox/fluke-mse/sims/04-04/",
#              "~/Dropbox/fluke-mse/sims/07-02/",
#              "~/Dropbox/fluke-mse/sims/07-03/",
#              "~/Dropbox/fluke-mse/mbp/01-04/",
#              "~/Dropbox/fluke-mse/mbp/04-04/",
#              "~/Dropbox/fluke-mse/mbp/07-04/"),
# scen.name <- c("MP 1",
#                "MP 4",
#                "MP 7",
#                "MP 1",
#                "MP 1",
#                "MP 4",
#                "MP 4",
#                "MP 7",
#                "MP 7",
#                "MP 1",
#                "MP 4",
#                "MP 7"),
# fsim <- c(1, 1, 1, 26, 51,26, 51, 26, 51,76,76,76))
# 
# params <- list(
#   scendir <- c("~/Dropbox/fluke-mse/01-01/",
#                "~/Dropbox/fluke-mse/sims/01-02/",
#                "~/Dropbox/fluke-mse/sims/01-03/",
#                "~/Dropbox/fluke-mse/mbp/01-04/",
#                "~/Dropbox/fluke-mse/sims/02-01/",
#                "~/Dropbox/fluke-mse/sims/02-02/",
#                "~/Dropbox/fluke-mse/sims/03-01/",
#                "~/Dropbox/fluke-mse/sims/03-02/",
#                "~/Dropbox/fluke-mse/sims/05-01/",
#                "~/Dropbox/fluke-mse/sims/05-02/"),
#   scen.name <- c("MP 1",
#                  "MP 1",
#                  "MP 1",
#                  "MP 1",
#                  "MP 2",
#                  "MP 2",
#                  "MP 3",
#                  "MP 3",
#                  "MP 5",
#                  "MP 5"),
#   fsim <- c(1,  26, 51,  76, 1, 26, 1, 26, 1, 26))
# 
# #change the folder names for the scenarios you want to summarize results for and give them a unique name
# params <- list(
#   scendir <- c("~/Dropbox/fluke-mse/01-01/",
#                "~/Dropbox/fluke-mse/04-01/",
#                "~/Dropbox/fluke-mse/07-01/",
#                "~/Dropbox/fluke-mse/sims/01-02/",
#                "~/Dropbox/fluke-mse/sims/01-03/",
#                #"~/Dropbox/fluke-mse/sims/01-04/",
#                "~/Dropbox/fluke-mse/sims/04-02/",
#                "~/Dropbox/fluke-mse/sims/04-03/",
#                #"~/Dropbox/fluke-mse/sims/04-04/",
#                "~/Dropbox/fluke-mse/sims/07-02/",
#                "~/Dropbox/fluke-mse/sims/07-03/",
#                "~/Dropbox/fluke-mse/mbp/01-04/",
#                "~/Dropbox/fluke-mse/mbp/04-04/",
#                "~/Dropbox/fluke-mse/mbp/07-04/",
#                "~/Dropbox/fluke-mse/sims/02-01/",
#                "~/Dropbox/fluke-mse/sims/02-02/",
#                "~/Dropbox/fluke-mse/sims/03-01/",
#                "~/Dropbox/fluke-mse/sims/03-02/",
#                "~/Dropbox/fluke-mse/sims/05-01/",
#                "~/Dropbox/fluke-mse/sims/05-02/"),
#   scen.name <- c("MP 1",
#                  "MP 4",
#                  "MP 7",
#                  "MP 1",
#                  "MP 1",
#                  "MP 4",
#                  "MP 4",
#                  "MP 7",
#                  "MP 7",
#                  "MP 1",
#                  "MP 4",
#                  "MP 7",
#                  "MP 2",
#                  "MP 2",
#                  "MP 3",
#                  "MP 3",
#                  "MP 5",
#                  "MP 5"),
#   fsim <- c(1, 1, 1, 26, 51,26, 51, 26, 51,76,76,76, 1, 26, 1, 26, 1, 26))

params <- list(
  scendir <- c("~/Dropbox/fluke-mse/sims/2022-05-24/01-01/",
               "~/Dropbox/fluke-mse/sims/2022-05-24/01-02/",
               "~/Dropbox/fluke-mse/sims/2022-05-24/01-03/",
               "~/Dropbox/fluke-mse/sims/2022-05-24/01-04/",
               "~/Dropbox/fluke-mse/sims/2022-05-24/01-06/",
               "~/Dropbox/fluke-mse/sims/2022-05-24/01-07/",
               "~/Dropbox/fluke-mse/sims/2022-05-24/01-08/"),
  scen.name <- c("MP 1",
                 "MP 2",
                 "MP 3",
                 "MP 4",
                 "MP 6",
                 "MP 7",
                 "MP 8"),
  fsim <- rep(1,7))

#summarize the output files
all_results <- purrr::pmap_dfr(params,read_results)
#generate time series of metrics
diag_ts <- get_diag_ts(all_results) %>% 
  mutate(nurow = ifelse(type == "spawning biomass" & value == 0,0,1),
         nurow = ifelse(type == "B/Bref" & value == 0,0,nurow)) %>% 
  filter(nurow==1) %>% 
  select(-nurow)
  

# time series plots
p1 <- diag_ts %>% 
  filter(year != 2019) %>% 
  mutate(type = fct_relevel(type,c("spawning biomass", "total catch"))) %>% 
  projection.plot()

# join ref pts & F

# add additional metrics

# add boxplots
p2 <- diag_ts %>% 
  filter(year >= 2022) %>% 
  ggplot() +
  aes(x = scenario, y = value, fill = scenario) +
  geom_boxplot(outlier.shape=NA) +
  scale_fill_brewer(type = "qual", palette = 2) +
  facet_wrap(~type, scale = "free_y") +
  #ylim(0,NA) +
  ylab("") +
  xlab("") +
  #coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom",
        #axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  labs(fill = "")

# add table summaries

#plots
ggsave("trajectory-comparisons.png",p1,width=8,height=8)
ggsave("boxplot-comparisons.png",p2,width=8,height=8)
#ggsave("m2-5-trajectory-comparisons.png",p1,width=8,height=8)
#ggsave("m2-5-boxplot-comparisons.png",p2,width=8,height=8)

# radar chart - not sure how to save this to file automagically
p3 <- diag_ts %>% 
  filter(year >= 2022) %>% 
  group_by(scenario, type) %>% 
  summarize(value = case_when(
    type == "F/Fref" ~ mean(value<1, na.rm=TRUE),
    type == "B/Bref" ~ mean(value>0.5, na.rm=TRUE),
    TRUE ~ mean(value, na.rm=TRUE))) %>% 
  distinct() %>%  #unsure what is happening, but this helps.
  rename(metric = type,
         mp = scenario) %>% 
  mutate(metric = case_when(
    metric == "F/Fref" ~ "not overfishing",
    metric == "B/Bref" ~ "not overfished",
    TRUE ~ metric)) %>% 
  select(metric,value,mp) %>% 
  do_radar_plot()
#ggsave("radarplot-comparisons.png",p3,width=8,height=8)


######
###### summarizing metrics

metrics <- diag_ts %>% 
  filter(year >= 2036) %>% 
  group_by(scenario, type, isim) %>% 
  summarize(value = case_when(
    type == "F/Fref" ~ mean(value<1, na.rm=TRUE),
    type == "B/Bref" ~ mean(value>0.5, na.rm=TRUE),
    TRUE ~ mean(value, na.rm=TRUE)), .groups = "drop") %>% 
  distinct() %>%  #unsure what is happening, but this helps.
  rename(metric = type,
         mp = scenario) %>% 
  mutate(metric = case_when(
    metric == "F/Fref" ~ "not overfishing",
    metric == "B/Bref" ~ "not overfished",
    TRUE ~ metric))

# add boxplots
p2 <- metrics %>% 
  ggplot() +
  aes(x = mp, y = value, fill = mp) +
  geom_boxplot(outlier.shape=NA) +
  scale_fill_brewer(type = "qual", palette = 2) +
  facet_wrap(~metric, scale = "free_y") +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank()) +
  labs(fill = "")

# add table summaries
ggsave("boxplot-metrics.png",p2,width=8,height=8)

median_metrics <- metrics %>% 
  group_by(mp, metric) %>% 
  summarize(value = median(value, na.rm=TRUE))

write_csv(median_metrics,file = "performance-metrics-median-over-sims.csv")
minmax_metrics <- median_metrics %>% 
  group_by(metric) %>% 
  summarize(min_val = min(value),
            max_val = max(value))  

write_csv(minmax_metrics,file = "performance-metrics-minmax-of-medians.csv")


# radar chart - not sure how to save this to file automagically
p3 <- median_metrics %>% 
  filter(!metric %in% c("change_cs", "cs_per_trip")) %>% 
  do_radar_plot()



### state information ###

read_state_results <- function(scendir = "~/Dropbox/fluke-mse/07-01/",scen.name="mgmt_scenario_7",
                         fsim = 1) {

recoutput2 <- read.table(paste0(scendir, "recoutput2.out"), header = FALSE, skip = 1) #TRUE)
nyrs <- length(unique(recoutput2[,2]))
recoutput2[,1] <- rep(1:100, each = 10*nyrs)
names(recoutput2) <- c("isim","year","state","ntrips","nchoice","change_cs","cost",
                       "keep_one", "mulen_keep","mulen_release","trophy")
state_recoutput <- recoutput2 %>% 
  mutate(scenario = rep(scen.name,nrow(.)),
         isim = isim + fsim -1)

return(state_recoutput)
}

#generate time series of state metrics
state_results <- purrr::pmap_dfr(params,read_state_results)

