weights<-readRDS("weights.rds")
names(weights)<-c("Stakeholder","1. Angler Experience Quality","2. Equity of Angler Experience","3. Stock Sustainability","4. Socio-economic")
box.weights<-weights %>% 
  pivot_longer(!Stakeholder,names_to = "Objective",values_to = "Weights") %>% 
  ggplot() +
  aes(x = Objective, y = Weights) +
  geom_boxplot(outlier.shape=NA) +
  scale_fill_brewer(type = "qual", palette = 2) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("outputs/plots/weights.png",box.weights,width=9,height=5)