#####
# Load libraries:
#####
library(sf)
library(dplyr)
library(tmap)
library(terra)
library(ggplot2)
#####
# Generate base raster:
#####
baserast = rast(nrows = 10,
                ncols = 10,
                xmin = 0,
                xmax = 10,
                ymin = 0, 
                ymax = 10)
values(baserast) = 0
landscape = as.data.frame(baserast,xy=TRUE)
#####
# Generate three voting scenarios:
#####
voter_scenario_1 = data.frame(x = rep(0.5:9.5, 10),
            y = rep(0.5:9.5, each = 10),
            vote = c(rep("Republican", 22),
                     rep("Democrat", 2),
                     rep("Republican",2),
                     rep("Democrat",2),
                     rep("Republican", 4),
                     rep("Democrat", 6),
                     rep("Republican",4),
                     rep("Democrat",6),
                     rep("Republican", 4),
                     rep("Democrat", 6),
                     rep("Republican",4),
                     rep("Democrat",6),
                     rep("Republican", 4),
                     rep("Democrat", 6),
                     rep("Republican",4),
                     rep("Democrat",6),
                     rep("Republican",12)),
            scenario = "Normal") %>%
  st_as_sf(.,coords=c('x','y'))

voter_scenario_2 = data.frame(x = rep(0.5:9.5, 10),
                              y = rep(0.5:9.5, each = 10),
                              vote = c(rep("Republican", 22),
                                       rep("Democrat", 6),
                                       rep("Republican", 4),
                                       rep("Democrat", 6),
                                       rep("Republican",4),
                                       rep("Democrat",6),
                                       rep("Republican", 4),
                                       rep("Democrat", 6),
                                       rep("Republican",4),
                                       rep("Democrat",6),
                                       rep("Republican", 4),
                                       rep("Democrat", 6),
                                       rep("Republican",4),
                                       rep("Democrat",6),
                                       rep("Republican",5),
                                       rep("Democrat",3),
                                       rep("Republican",4)),
                              scenario = "Moderate Wave +5% Dem.") %>%
  st_as_sf(.,coords=c('x','y'))

voter_scenario_3 = data.frame(x = rep(0.5:9.5, 10),
                              y = rep(0.5:9.5, each = 10),
                              vote = c(rep("Republican", 11),
                                       rep("Democrat",2),
                                       rep("Republican",1),
                                       rep("Democrat",2),
                                       rep("Republican",6),
                                       rep("Democrat", 7),
                                       rep("Republican", 3),
                                       rep("Democrat", 6),
                                       rep("Republican",4),
                                       rep("Democrat",6),
                                       rep("Republican", 3),
                                       rep("Democrat", 7),
                                       rep("Republican",3),
                                       rep("Democrat",7),
                                       rep("Republican", 4),
                                       rep("Democrat", 6),
                                       rep("Republican",4),
                                       rep("Democrat",6),
                                       rep("Republican",5),
                                       rep("Democrat",3),
                                       rep("Republican",4)),
                              scenario = "Extreme Wave +12% Dem.") %>%
  st_as_sf(.,coords=c('x','y'))

length(which(voter_scenario_3$vote=="Republican"))
length(which(voter_scenario_3$vote=="Democrat"))

voters1 = rbind(voter_scenario_1,voter_scenario_2,voter_scenario_3) %>%
  mutate(Gerrymander = "Equitable")
voters2 = rbind(voter_scenario_1,voter_scenario_2,voter_scenario_3) %>%
  mutate(Gerrymander = "Moderate")
voters3 = rbind(voter_scenario_1,voter_scenario_2,voter_scenario_3) %>%
  mutate(Gerrymander = "Extreme")
voters = rbind(voters1,voters2,voters3)
#####
# Generate first district scenario:
#####
district_1_scenario_1 = data.frame(x = c(0,4,4,2,2,0,0),
                                   y = c(0,0,2,2,3,3,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_2_scenario_1 = data.frame(x = c(4,8,8,6,6,4,4),
                                   y = c(0,0,2,2,3,3,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_3_scenario_1 = data.frame(x = c(8,10,10,8,8),
                                   y = c(0,0,5,5,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_4_scenario_1 = data.frame(x = c(2,4,4,6,6,2,2),
                                   y = c(2,2,3,3,5,5,2)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Democrat")
district_5_scenario_1 = data.frame(x = c(0,2,2,0,0),
                                   y = c(3,3,8,8,3))%>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_6_scenario_1 = data.frame(x = c(2,6,6,5,5,3,3,2,2),
                                   y = c(5,5,7,7,8,8,7,7,5)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Democrat")
district_7_scenario_1 = data.frame(x = c(0,2,2,3,3,4,4,5,5,0,0),
                                   y = c(8,8,7,7,8,8,9,9,10,10,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_8_scenario_1 = data.frame(x = c(6,8,8,10,10,6,6),
                                   y = c(2,2,5,5,6,6,2)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Democrat")
district_9_scenario_1 = data.frame(x = c(6,8,8,7,7,5,5,4,4,5,5,6,6),
                                   y = c(6,6,8,8,10,10,9,9,8,8,7,7,6)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Democrat")
district_10_scenario_1 = data.frame(x = c(8,10,10,7,7,8,8),
                                    y = c(6,6,10,10,8,8,6)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")

scenario_1 = rbind(district_1_scenario_1,
                   district_2_scenario_1,
                   district_3_scenario_1,
                   district_4_scenario_1,
                   district_5_scenario_1,
                   district_6_scenario_1,
                   district_7_scenario_1,
                   district_8_scenario_1,
                   district_9_scenario_1,
                   district_10_scenario_1) %>%
  mutate(Gerrymander = "Equitable")
#####
# Generate second district scenario:
#####
district_1_scenario_2 = data.frame(x = c(1,4,4,2,2,1,1),
                                   y = c(0,0,4,4,2,2,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_2_scenario_2 = data.frame(x = c(4,8,8,6,6,4,4),
                                   y = c(0,0,3,3,2,2,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_3_scenario_2 = data.frame(x = c(8,10,10,8,8),
                                   y = c(0,0,5,5,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_4_scenario_2 = data.frame(x = c(4,6,6,3,3,4,4),
                                   y = c(2,2,6,6,4,4,2)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Democrat")
district_5_scenario_2 = data.frame(x = c(0,1,1,2,2,3,3,2,2,0,0),
                                   y = c(0,0,2,2,4,4,6,6,5,5,0))%>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_6_scenario_2 = data.frame(x = c(0,2,2,4,4,5,5,3,3,2,2,0,0),
                                   y = c(5,5,6,6,7,7,8,8,7,7,8,8,5)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_7_scenario_2 = data.frame(x = c(0,2,2,3,3,4,4,5,5,0,0),
                                   y = c(8,8,7,7,8,8,9,9,10,10,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_8_scenario_2 = data.frame(x = c(6,8,8,10,10,8,8,6,6),
                                   y = c(3,3,5,5,6,6,7,7,2)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Democrat")
district_9_scenario_2 = data.frame(x = c(6,8,8,7,7,5,5,4,4,5,5,4,4,6),
                                   y = c(7,7,8,8,10,10,9,9,8,8,7,7,6,6)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Democrat")
district_10_scenario_2 = data.frame(x = c(8,10,10,7,7,8,8),
                                    y = c(6,6,10,10,8,8,6)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
scenario_2 = rbind(district_1_scenario_2,
                   district_2_scenario_2,
                   district_3_scenario_2,
                   district_4_scenario_2,
                   district_5_scenario_2,
                   district_6_scenario_2,
                   district_7_scenario_2,
                   district_8_scenario_2,
                   district_9_scenario_2,
                   district_10_scenario_2) %>%
  mutate(Gerrymander = "Moderate")
#####
# Generate third district scenario:
#####
district_1_scenario_3 = data.frame(x = c(0,2,2,3,3,4,4,2,2,1,1,0,0),
                                   y = c(0,0,1,1,4,4,5,5,3,3,2,2,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_2_scenario_3 = data.frame(x = c(2,5,5,4,4,3,3,2,2),
                                   y = c(0,0,5,5,4,4,1,1,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_3_scenario_3 = data.frame(x = c(8,10,10,9,9,8,8,6,6,7,7,8,8),
                                   y = c(0,0,2,2,3,3,4,4,2,2,1,1,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_4_scenario_3 = data.frame(x = c(5,8,8,7,7,6,6,8,8,5,5),
                                   y = c(0,0,1,1,2,2,4,4,5,5,0)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_5_scenario_3 = data.frame(x = c(0,1,1,2,2,4,4,1,1,0,0),
                                   y = c(2,2,3,3,5,5,7,7,4,4,2)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_6_scenario_3 = data.frame(x = c(0,1,1,5,5,3,3,1,1,0,0),
                                   y = c(4,4,7,7,8,8,9,9,8,8,4)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_7_scenario_3 = data.frame(x = c(0,1,1,3,3,5,5,6,6,5,5,0,0),
                                   y = c(8,8,9,9,8,8,7,7,9,9,10,10,8)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_8_scenario_3 = data.frame(x = c(9,10,10,9,9,4,4,8,8,9,9), 
                                   y = c(2,2,5,5,6,6,5,5,3,3,2)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Democrat")
district_9_scenario_3 = data.frame(x = c(9,10,10,9,9,8,8,4,4,9,9),
                                   y = c(5,5,9,9,8,8,7,7,6,6,5)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
district_10_scenario_3 = data.frame(x = c(6,8,8,9,9,10,10,5,5,6,6),
                                    y = c(7,7,8,8,9,9,10,10,9,9,7)) %>%
  st_as_sf(coords = c("x","y")) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  mutate(result = "Republican")
scenario_3 = rbind(district_1_scenario_3,
                   district_2_scenario_3,
                   district_3_scenario_3, 
                   district_4_scenario_3,
                   district_5_scenario_3, 
                   district_6_scenario_3,
                   district_7_scenario_3,
                   district_8_scenario_3,
                   district_9_scenario_3,
                   district_10_scenario_3) %>%
  mutate(Gerrymander = "Extreme")
#####
# Tally the votes!
#####
voting_results1 = rbind(scenario_1,scenario_2,scenario_3) %>%
  mutate(scenario = "Normal")
voting_results2 = rbind(scenario_1,scenario_2,scenario_3) %>%
  mutate(scenario = "Moderate Wave +5% Dem.")
voting_results3 = rbind(scenario_1,scenario_2,scenario_3) %>%
  mutate(scenario = "Extreme Wave +12% Dem.")

voting_results = rbind(voting_results1,voting_results2,voting_results3) %>%
  mutate(result = NA)
for(i in 1:nrow(voting_results)){
    count = st_intersection(voting_results[i,],
                            voters %>%
                              filter(scenario == voting_results[i,]$scenario,
                                     Gerrymander == voting_results[i,]$Gerrymander)) %>%
      summarize(tot_rep = length(which(vote=="Republican")),
                tot_dem = length(which(vote=="Democrat"))) %>%
      st_drop_geometry()
    if(count$tot_rep>count$tot_dem){voting_results[i,]$result = "Republican"}
    if(count$tot_dem>count$tot_rep){voting_results[i,]$result = "Democrat"}
    if(count$tot_dem==count$tot_rep){voting_results[i,]$result = "Tie"}
}
#####
# Viz:
#####
voting_results$Gerrymander = factor(voting_results$Gerrymander,
                                    levels = c("Equitable","Moderate","Extreme"))
voting_results$scenario = factor(voting_results$scenario,
                                 levels = c("Normal","Moderate Wave +5% Dem.","Extreme Wave +12% Dem."))
voters$Gerrymander = factor(voters$Gerrymander,
                            levels = c("Equitable","Moderate","Extreme"))
voters$scenario = factor(voters$scenario,
                         levels = c("Normal","Moderate Wave +5% Dem.","Extreme Wave +12% Dem."))

p1 = ggplot(data = as.data.frame(baserast,xy=TRUE),
       aes(x,y))+
  geom_tile()+
  geom_sf(data = voting_results,
          aes(fill = result),
          col = 'black',
          #alpha = .3,
          inherit.aes = F)+
  geom_sf(data = voters,
          col = 'black',
          size = 3,
          inherit.aes = F)+
  geom_sf(data = voters,
          aes(col = vote),
          size = 2.25,
          inherit.aes = F)+
  scale_fill_manual(values = c("#B3CDE3","#FBB4AE","#BEAED4"))+
  scale_color_manual(values = c("blue","red"))+
  facet_grid(scenario~Gerrymander)+
  xlab("")+
  ylab("")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Voting scenario", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Gerrymander", breaks = NULL, labels = NULL))+
  theme_bw()+
  theme(legend.position = 'none',panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank());p1
ggsave(plot = p1,filename = paste0(getwd(),'/Dummymandering/Figures/Plot.jpeg'),
       dpi = 300)
