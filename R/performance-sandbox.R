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
    facet_wrap(~type, scale = "free_y") + 
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==1), lty=1,lwd=0.5,col=gray(0.7),alpha=0.5) +
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==2), lty=1,lwd=0.5,col=gray(0.7),alpha=0.5) +
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==3), lty=1,lwd=0.5,col=gray(0.7),alpha=0.5) +
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==4), lty=1,lwd=0.5,col=gray(0.7),alpha=0.5) +
    ylim(0,NA) + 
    ylab("") + 
    theme_bw() +
    theme(legend.position="bottom",
          axis.text.y = element_blank()) +
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
                   "not overfished" = c(1,0)) #,
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
                  alpha("#009E73",0.9),
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

spawbio <- read.table("mse/spawbio.out", header = TRUE)
totcatch <- read.table("mse/totcatch.out", header = TRUE)
recoutput <- read.table("mse/recoutput.out", header = FALSE, skip = 1) #TRUE)
rbctrack <- read.table("mse/rbctrack.out", header = FALSE, skip = 1)
names(spawbio) <- str_sub(names(spawbio), start=2)
names(totcatch) <- str_sub(names(totcatch), start=2)
results <- spawbio %>% 
  as_tibble() %>% 
  rowid_to_column(var = "isim") %>% 
  pivot_longer(cols= 2:ncol(.), names_to = "year", values_to = "biomass") %>% 
  I()
res2 <- totcatch %>% 
  as_tibble() %>% 
  rowid_to_column(var = "isim") %>% 
  pivot_longer(cols= 2:ncol(.), names_to = "year", values_to = "catch") %>% 
  I()
results <- results %>% 
  left_join(res2)

res3 <- recoutput[,1:5] %>% as_tibble()
names(res3) <- c("isim", "year", "type", "number","trips")
res3 <- res3 %>% 
  mutate(#year = 2019 + year,
         type = ifelse(type==1, "keep", "release"),
         isim = rep(1:(nrow(.)/(2*length(unique(year)))), each = 2*length(unique(year)))) %>% 
  pivot_wider(names_from = "type", values_from = "number") %>% 
  I()

results <- results %>% 
  mutate(year = as.integer(year)) %>% 
  left_join(res3)

refpts <- read.table("mse/refpts.out", header = FALSE, skip = 1)
names(refpts) <- c("isim", "fref", "bref", "cref")
results <- results %>% 
  left_join(refpts)

fout <- read.table("mse/frate.out", header = TRUE)
names(fout) <- str_sub(names(fout), start=2)
res4 <- fout %>% 
  as_tibble() %>% 
  rowid_to_column(var = "isim") %>% 
  pivot_longer(cols= 2:ncol(.), names_to = "year", values_to = "frate") %>% 
  mutate(year = as.integer(year)) %>% 
  I()
results <- results %>% 
  left_join(res4)


rbc2 <- rbctrack %>% 
  as_tibble() %>% 
  select(1:4, 6:7, 9) %>% 
  I()
names(rbc2) <- c("isim","year","est_ssb","abc","est_f","est_fref","m_use")
rbc2 <- rbc2 %>% 
  mutate(year = 2019 + year) %>% 
  I()
results <- results %>% 
  left_join(rbc2)

### coastwide scenario
spawbio <- read.table("mse/coast14/spawbio.out", header = TRUE)
totcatch <- read.table("mse/coast14/totcatch.out", header = TRUE)
recoutput <- read.table("mse/coast14/recoutput.out", header = FALSE, skip = 1) #TRUE)
rbctrack <- read.table("mse/coast14/rbctrack.out", header = FALSE, skip = 1)
names(spawbio) <- str_sub(names(spawbio), start=2)
names(totcatch) <- str_sub(names(totcatch), start=2)
coast_results <- spawbio %>% 
  as_tibble() %>% 
  rowid_to_column(var = "isim") %>% 
  pivot_longer(cols= 2:ncol(.), names_to = "year", values_to = "biomass") %>% 
  I()
res2 <- totcatch %>% 
  as_tibble() %>% 
  rowid_to_column(var = "isim") %>% 
  pivot_longer(cols= 2:ncol(.), names_to = "year", values_to = "catch") %>% 
  I()
coast_results <- coast_results %>% 
  left_join(res2)

res3 <- recoutput[,1:5] %>% as_tibble()
names(res3) <- c("isim", "year", "type", "number","trips")
res3 <- res3 %>% 
  mutate(#year = 2019 + year,
    type = ifelse(type==1, "keep", "release"),
    isim = rep(1:(nrow(.)/(2*length(unique(year)))), each = 2*length(unique(year)))) %>% 
  pivot_wider(names_from = "type", values_from = "number") %>% 
  I()

coast_results <- coast_results %>% 
  mutate(year = as.integer(year)) %>% 
  left_join(res3)

refpts <- read.table("mse/coast14/refpts.out", header = FALSE, skip = 1)
names(refpts) <- c("isim", "fref", "bref", "cref")
coast_results <- coast_results %>% 
  left_join(refpts)

fout <- read.table("mse/coast14/frate.out", header = TRUE)
names(fout) <- str_sub(names(fout), start=2)
res4 <- fout %>% 
  as_tibble() %>% 
  rowid_to_column(var = "isim") %>% 
  pivot_longer(cols= 2:ncol(.), names_to = "year", values_to = "frate") %>% 
  mutate(year = as.integer(year)) %>% 
  I()
coast_results <- coast_results %>% 
  left_join(res4)

rbc2 <- rbctrack %>% 
  as_tibble() %>% 
  select(1:4, 6:7, 9) %>% 
  I()
names(rbc2) <- c("isim","year","est_ssb","abc","est_f","est_fref","m_use")
rbc2 <- rbc2 %>% 
  mutate(year = 2019 + year) %>% 
  I()
coast_results <- coast_results %>% 
  left_join(rbc2) %>% 
  mutate(scenario = rep("coastwide 14",nrow(.)))

results <- results %>% 
  mutate(scenario = rep("status quo",nrow(.))) %>% 
  bind_rows(coast_results)
  


diag_ts <- results %>% 
  mutate(keep_to_rel = keep/release,
         "kept per trip" = keep/trips,
         "F/Fref" = frate/fref,
         "B/Bref" = biomass/bref)  %>% 
  select(scenario, isim, year, biomass, catch, keep_to_rel, "kept per trip", "F/Fref", "B/Bref") %>% 
  rename("spawning biomass" = biomass,
         "total catch" = catch,
         "kept:released" = keep_to_rel) %>% 
  pivot_longer(cols=c("spawning biomass","total catch","kept:released", "kept per trip", 
                      "F/Fref", "B/Bref"),names_to = "type", values_to = "value") %>% 
  I()


# time series plots
diag_ts %>% 
  filter(year != 2019) %>% 
  projection.plot()

# join ref pts & F

# add additional metrics

# add boxplots
diag_ts %>% 
  filter(year >= 2022) %>% 
  ggplot() +
  aes(x = scenario, y = value, fill = scenario) +
  geom_boxplot() +
  scale_fill_brewer(type = "qual", palette = 2) +
  facet_wrap(~type, scale = "free_y") +
  ylim(0,NA) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_blank())
  
# add table summaries

# radar chart
diag_ts %>% 
  filter(year >= 2022) %>% 
  group_by(scenario, type) %>% 
  summarize(value = case_when(
    type == "F/Fref" ~ mean(value<1),
    type == "B/Bref" ~ mean(value>0.5),
    TRUE ~ mean(value))) %>% 
  distinct() %>%  #unsure what is happening, but this helps.
  rename(metric = type,
         mp = scenario) %>% 
  mutate(metric = case_when(
    metric == "F/Fref" ~ "not overfishing",
    metric == "B/Bref" ~ "not overfished",
    TRUE ~ metric)) %>% 
  select(metric,value,mp) %>% 
  do_radar_plot()

