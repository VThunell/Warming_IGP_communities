
#### CREATE PLOTS FROM MATCONT DATA FOR THUNELL et al. ######
### Warming effects on intraguild predation communities #####
  
# Either download the data from or start a new version control project with 
# Viktor Thunells GitHub repository Warming_IGP_communities using the URL
# https://github.com/VThunell/Warming_IGP_communities/tree/master/Data

library(R.matlab)
library(tidyverse)
library(patchwork)
library(RColorBrewer)

#### Figure 1 Plot equilibrium biomass over Temp with min and max of amplitudes for cyclic region #####

# Import Data for default scenario, Fig. 1 & 2
EP_T1 <- readMat("Data/default/EP_T_default(1).mat")
EP_T2 <- readMat("Data/default/EP_T_default(2).mat")
EP_T3 <- readMat("Data/default/EP_T_default_ASS(1).mat")
EP_T4 <- readMat("Data/default/EP_T_default_ASS(2).mat")

# Create a column indicating dynamic stability based on eigenvalues in Matcont f and s table
EP_Tx1 <- as.data.frame(t(cbind(EP_T1$x, EP_T2$x))) # the stable and unstable coexistence
EP_Tx1["D"] <- "S1"                                 # set all values to stable PCR state
EP_Tx1$D[(which(EP_Tx1[5]==max(EP_Tx1[5]))+1):ncol(EP_T1$x)] <- "U" # The values above the index of max temp (LP in s) until the value before the first index of EP_T2 are unstable

EP_Tx2 <- as.data.frame(t(cbind(EP_T3$x, EP_T4$x))) # the alternative stable (CR) state
EP_Tx2["D"] <- "S2" #all values hear are in the alternative stable (CR) state

# Combine EP_Tx data 
EP_Tx <- rbind(EP_Tx1, EP_Tx2) 
colnames(EP_Tx) <- c("R","Pj","Pa","C","T","D")

EP_Tx$D <- factor(as.factor(EP_Tx$D), levels = c(levels(as.factor(EP_Tx$D)), "Cyc"))
EP_Tx_long <- gather(EP_Tx, coord, eq_bmd, Pa, Pj, C, R) # change to long format for ggplot
EP_Tx_long$coord_f = factor(EP_Tx_long$coord, levels=c('Pa','Pj','C','R')) # to get coordinatess in the same order of facets as previous plots
str(EP_Tx_long)

# Fig labs
Fig1bmd_labs <- c("Adult predator", "Juvenile predator", "Consumer", "Resource")
names(Fig1bmd_labs) <- c("Pa", "Pj", "C", "R")

PCRcyclic281  <- readMat("Data/default/P_281.mat")
PCRcyclic281 <- as.data.frame(cbind(t(PCRcyclic281$x[1:4,]), PCRcyclic281$t[,1]))
PCRcyclic281 <- as.data.frame(PCRcyclic281[which(PCRcyclic281[,5] >= 2000), ])
PCRcyclic281 <- cbind(rbind(apply(PCRcyclic281[,1:4],2,min), apply(PCRcyclic281[,1:4],2,max)), c(281,281),c("min","max"))
colnames(PCRcyclic281) <- c("R","Pj","Pa","C","Temp","minmax")

PCRcyclic282  <- readMat("Data/default/P_282.mat")
PCRcyclic282 <- as.data.frame(cbind(t(PCRcyclic282$x[1:4,]), PCRcyclic282$t[,1]))
PCRcyclic282 <- as.data.frame(PCRcyclic282[which(PCRcyclic282[,5] >= 2000), ])
PCRcyclic282 <- cbind(rbind(apply(PCRcyclic282[,1:4],2,min), apply(PCRcyclic282[,1:4],2,max)), c(282,282),c("min","max"))
colnames(PCRcyclic282) <- c("R","Pj","Pa","C","Temp","minmax")

PCRcyclic283  <- readMat("Data/default/P_283.mat")
PCRcyclic283 <- as.data.frame(cbind(t(PCRcyclic283$x[1:4,]), PCRcyclic283$t[,1]))
PCRcyclic283 <- as.data.frame(PCRcyclic283[which(PCRcyclic283[,5] >= 2000), ])
PCRcyclic283 <- cbind(rbind(apply(PCRcyclic283[,1:4],2,min), apply(PCRcyclic283[,1:4],2,max)), c(283,283),c("min","max"))
colnames(PCRcyclic283) <- c("R","Pj","Pa","C","Temp","minmax")

PCRcyclic284  <- readMat("Data/default/P_284.mat")
PCRcyclic284 <- as.data.frame(cbind(t(PCRcyclic284$x[1:4,]), PCRcyclic284$t[,1]))
PCRcyclic284 <- as.data.frame(PCRcyclic284[which(PCRcyclic284[,5] >= 2000), ])
PCRcyclic284 <- cbind(rbind(apply(PCRcyclic284[,1:4],2,min), apply(PCRcyclic284[,1:4],2,max)), c(284,284),c("min","max"))
colnames(PCRcyclic284) <- c("R","Pj","Pa","C","Temp","minmax")

PCRcyclic285  <- readMat("Data/default/P_285.mat")
PCRcyclic285 <- as.data.frame(cbind(t(PCRcyclic285$x[1:4,]), PCRcyclic285$t[,1]))
PCRcyclic285 <- as.data.frame(PCRcyclic285[which(PCRcyclic285[,5] >= 2000), ])
PCRcyclic285 <- cbind(rbind(apply(PCRcyclic285[,1:4],2,min), apply(PCRcyclic285[,1:4],2,max)), c(285,285),c("min","max"))
colnames(PCRcyclic285) <- c("R","Pj","Pa","C","Temp","minmax")

PCRcyclic286  <- readMat("Data/default/P_286.mat")
PCRcyclic286 <- as.data.frame(cbind(t(PCRcyclic286$x[1:4,]), PCRcyclic286$t[,1]))
PCRcyclic286 <- as.data.frame(PCRcyclic286[which(PCRcyclic286[,5] >= 2000), ])
PCRcyclic286 <- cbind(rbind(apply(PCRcyclic286[,1:4],2,min), apply(PCRcyclic286[,1:4],2,max)), c(286,286),c("min","max"))
colnames(PCRcyclic286) <- c("R","Pj","Pa","C","Temp","minmax")

PCRcyclic287  <- readMat("Data/default/P_287.mat")
PCRcyclic287 <- as.data.frame(cbind(t(PCRcyclic287$x[1:4,]), PCRcyclic287$t[,1]))
PCRcyclic287 <- as.data.frame(PCRcyclic287[which(PCRcyclic287[,5] >= 2000), ])
PCRcyclic287 <- cbind(rbind(apply(PCRcyclic287[,1:4],2,min), apply(PCRcyclic287[,1:4],2,max)), c(287,287),c("min","max"))
colnames(PCRcyclic287) <- c("R","Pj","Pa","C","Temp","minmax")

PCRcyclic288  <- readMat("Data/default/P_288.mat")
PCRcyclic288 <- as.data.frame(cbind(t(PCRcyclic288$x[1:4,]), PCRcyclic288$t[,1]))
PCRcyclic288 <- as.data.frame(PCRcyclic288[which(PCRcyclic288[,5] >= 2000), ])
PCRcyclic288 <- cbind(rbind(apply(PCRcyclic288[,1:4],2,min), apply(PCRcyclic288[,1:4],2,max)), c(288,288),c("min","max"))
colnames(PCRcyclic288) <- c("R","Pj","Pa","C","Temp","minmax")

PCRcyclic289  <- readMat("Data/default/P_289.mat")
PCRcyclic289 <- as.data.frame(cbind(t(PCRcyclic289$x[1:4,]), PCRcyclic289$t[,1]))
PCRcyclic289 <- as.data.frame(PCRcyclic289[which(PCRcyclic289[,5] >= 2000), ])
PCRcyclic289 <- cbind(rbind(apply(PCRcyclic289[,1:4],2,min), apply(PCRcyclic289[,1:4],2,max)), c(289,289), c("min","max"))
colnames(PCRcyclic289) <- c("R","Pj","Pa","C","Temp","minmax")

PCRcyclic290  <- readMat("Data/default/P_290.mat")
PCRcyclic290 <- as.data.frame(cbind(t(PCRcyclic290$x[1:4,]), PCRcyclic290$t[,1]))
PCRcyclic290 <- as.data.frame(PCRcyclic290[which(PCRcyclic290[,5] >= 2000), ])
PCRcyclic290 <- cbind(rbind(apply(PCRcyclic290[,1:4],2,min), apply(PCRcyclic290[,1:4],2,max)), c(290,290),c("min","max"))
colnames(PCRcyclic290) <- c("R","Pj","Pa","C","Temp","minmax")

PCRcyclic2904  <- readMat("Data/default/P_2904.mat")
PCRcyclic2904 <- as.data.frame(cbind(t(PCRcyclic2904$x[1:4,]), PCRcyclic2904$t[,1]))
PCRcyclic2904 <- as.data.frame(PCRcyclic2904[which(PCRcyclic2904[,5] >= 2000), ])
PCRcyclic2904 <- cbind(rbind(apply(PCRcyclic2904[,1:4],2,min), apply(PCRcyclic2904[,1:4],2,max)), c(290.4,290.4),c("min","max"))
colnames(PCRcyclic2904) <- c("R","Pj","Pa","C","Temp","minmax")

PCRcyclic <- as.data.frame(rbind(PCRcyclic281,PCRcyclic282,PCRcyclic283,
                                 PCRcyclic284,PCRcyclic285,PCRcyclic286,
                                 PCRcyclic287,PCRcyclic288,PCRcyclic289,
                                 PCRcyclic290,PCRcyclic2904))
PCRcyclic["D"]="Cyc"
PCRcyclic_long <- gather(PCRcyclic, coord_f, cyc_minmax, Pa, Pj, C, R) #long format 
PCRcyclic_long$coord_f <- factor(PCRcyclic_long$coord, levels=c('Pa','Pj','C','R')) # to get coords in the same order of factes as previous plots
PCRcyclic_long["Temp"] <- as.numeric(unlist(PCRcyclic_long["Temp"]))
PCRcyclic_long["cyc_minmax"] <- as.numeric(unlist(PCRcyclic_long["cyc_minmax"]))
PCRcyclic_long$minmax <- factor(PCRcyclic_long$minmax, levels=c('min','max'))

EP_Tx_long_filt <- 
  EP_Tx_long %>%
  filter(T < 305 & T > 280) %>%
  filter(D != "S1" | T > 290.39) %>% # temp at lower Hopf 
  mutate(T = T-292)

PCRcyclic_long_filt <- 
  PCRcyclic_long %>%
  filter(Temp < 305 & Temp > 280) %>%
  mutate(Temp = Temp-292)


#pdf("MS1_Fig1.pdf", width = 8, height = 3.5)
ggplot() +
  geom_line(EP_Tx_long_filt, size=0.7, mapping = aes(T, eq_bmd, color = D, linetype = D)) +
  geom_line(PCRcyclic_long_filt, linetype= "dotted", 
            mapping = aes(Temp, cyc_minmax, group = minmax)) +
  facet_grid(.~coord_f, labeller = labeller(coord_f = Fig1bmd_labs) )+
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dotted"), 
                        name = "State", labels = c("PCR", "CR", "Unst. PCR", "Limit cycles"), drop = FALSE) +
  scale_color_manual(values = c("#000000", "#999999", "#000000", "#000000"),
                     name = "State", labels = c("PCR", "CR", "Unst. PCR", "Limit cycles"), drop = FALSE) +
  # scale_x_continuous(expression(paste("Temperature relative to ",italic("T")["0"])),  
  #                    limits= c(-10,10), breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                     limits= c(-10,10), breaks = scales::pretty_breaks(n = 6)) +
  ylab(expression(paste("Equilibrium biomass density "))) + #, "[", g ~V^{-1}, "]"))) +
  coord_cartesian(ylim=c(0, 2.5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.key.width = unit(1.5,"line"),
        legend.key = element_rect(fill = "white", colour = "white"),
        strip.text.x = element_text(size = 10)
  )
#dev.off()

#### Figure 3 Plot equilibrium biomass over T, beta = 0.15  ##########

# import
EP_T1_b015 <- readMat("Data/beta015/EP_T_beta015_PCR(1).mat")
EP_T2_b015 <- readMat("Data/beta015/EP_T_beta015_PCR(2).mat")
EP_T3_b015 <- readMat("Data/beta015/BP_T_beta015_uns_PR(1).mat")
EP_T5_b015 <- readMat("Data/beta015/BP_T_beta015_PR(2).mat")
EP_T6_b015 <- readMat("Data/beta015/BP_T_beta015_CR(2).mat")

# create a column indicating dynamic stability
EP_T2b_b015 <- t(EP_T2_b015$x) #the order of the data needs to be right for the geom_path not to close its line
EP_Tx1_b015 <- as.data.frame(rbind(EP_T2b_b015[order(EP_T2b_b015[,5]),],t(EP_T1_b015$x))) # the stable and unstable coexistence
EP_Tx1_b015["D"] <- "U1"                                 # set all values to unstable PCR state
#1:47 in EP_T1_b015 and 1:27 in EP_T2_b015 are stable
EP_Tx1_b015$D[(ncol(EP_T2_b015$x)-26):ncol(EP_T2_b015$x)] <- "S1" # the values above the index of max temp (LP) and until the matcont-backwards in EP_T2 are unstable
EP_Tx1_b015$D[1:(ncol(EP_T2_b015$x)-27)] <- "U2" # the values above the index of max temp (LP) and until the matcont-backwards in EP_T2 are unstable
EP_Tx1_b015$D[(ncol(EP_T2_b015$x)):(ncol(EP_T2_b015$x)+52)] <- "S1" #

EP_Tx2_b015 <- as.data.frame(t(EP_T3_b015$x)) # the unstable PR-state leading to CR
EP_Tx2_b015["D"] <- "U2" #all values hera are unstable-PR state (or at least unstable state)
EP_Tx3_b015 <- as.data.frame(t(EP_T5_b015$x))
EP_Tx3_b015["D"] <- "S2"                          # set all values to stable PR state
EP_Tx4_b015 <- as.data.frame(t(EP_T6_b015$x))
EP_Tx4_b015["D"] <- "S3"                          # set all values to stable PR state

EP_Tx_b015 <- rbind(EP_Tx1_b015, EP_Tx2_b015, EP_Tx3_b015, EP_Tx4_b015) 
colnames(EP_Tx_b015) <- c("R","Pj","Pa","C","T","D")
EP_Tx_b015$D <- as.factor(EP_Tx_b015$D)

EP_Tx_b015_long <- gather(EP_Tx_b015, coord, eq_bmd, Pa, Pj, C, R, factor_key=T) #long format 

# create plot of coordinates over T
Fig1bmd_labs <- c("Adult predator", "Juvenile predator", "Consumer", "Resource")
names(Fig1bmd_labs) <- c("Pa", "Pj", "C", "R")

#pdf("MS1_Fig3.pdf", width = 8, height = 3.5)
EP_Tx_b015_long %>%
  filter(T < 305 & T > 274) %>%
  filter(eq_bmd >= 0 ) %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T,eq_bmd, color = D, linetype = D)) +
    geom_path(size=0.7) +
    facet_grid(.~coord, labeller = labeller(coord = Fig1bmd_labs) )+
    ylab(expression(paste("Equilibrium biomass density "))) + #, "[", g ~V^{-1}, "]"))) +   
    coord_cartesian(ylim=c(0, 2.5)) +
    scale_linetype_manual(values = c("solid","solid", "solid", "dashed", "dashed"), name = "State", labels = c("PCR", "PR", "CR", "Unst. PCR", "Unst. PR")) +
    scale_color_manual(values = c("#000000","#DADADA","#999999","#000000","#DADADA"), name = "State", labels = c("PCR", "PR",  "CR", "Unst. PCR", "Unst. PR")) +
    scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                       limits= c(-10,10), breaks = scales::pretty_breaks(n = 6))  +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.key.width = unit(1.5,"line"),
        legend.key = element_rect(fill = "white", colour = "white"),
        #legend.box.background = element_rect(fill = "transparent", colour = "white"),
        strip.text.x = element_text(size = 10)
)
#dev.off()

#### Figure 2 Plot rate in user functions over T, default parameter values excluding cyclic region #######################

UF_T1 <- readMat("Data/default/EP_T_default(1).mat")
UF_T2 <- readMat("Data/default/EP_T_default(2).mat")

# The .mat h rows correspond to user functions (* multiplies with coordinate for population level biomass rate,  rownumber in UF_T, see next # create..)
#3.	 mat – mr     (*Pj, 1)
#4.	 rep – rr     (*Pa, 2)    
#5.	 BmpCs – BC   (*C, 3)    
#6.	 BmpPj – BJ   (*Pj, 4)    
#7.	 BmpPa – BA   (*Pa, 5)      
#8. Csmort - mC - not used but is not deleted from m.file
#9. IngPaC – AC  (*Pa, 7)    #Adult on Consumer
#10. IngPaR – AR  (*Pa, 8)    #Adult on Resource
#11. IngPjR - PR  (*Pj, 9)   #Juvenile on Resource
#12. IngCsR - CR  (*C, 10)    #Consumer on resource

# create a df for per capita level rates, i.e. rate per unit mass g/day/g
UF_T <- as.data.frame(t(cbind(rbind(UF_T1$h[3:12,], UF_T1$x), rbind(UF_T2$h[3:12,], UF_T2$x)))) 
colnames(UF_T) <- c("mr","rr","BC","BJ","BA","mC","AC","AR","PR","CR", "R","Pj","Pa","C","T")
UF_T["D"] <- "S"   # set all values to stable PCR state
UF_T$D[(which(UF_T$T==max(UF_T$T))+1):ncol(UF_T1$x)] <- "U" # set the values above the index of max temp (LP) and until the end of UF_T1 to unstable
UF_T["type"] <- "Per_biomass"

UF_T_long <- gather(UF_T, key = UF, value = bm_day_bm, mr,rr,BC,BJ,BA,AC,AR,PR,CR) #long format 
str(UF_T_long)

# create a vector with names to replace existing UF-names
UF_T_supp.labs <- c("Maturation", "Reproduction", "Biom prod C", "Biom prod Pj", 
                    "Biom prod Pa", "Predation", "Consump. Pa", "Consump. Pj", "Consump. C")
names(UF_T_supp.labs) <- c("mr","rr","BC","BJ","BA","AC","AR","PR","CR")

# UF multiplied with corresponding coordinates for population level rates
UF_TC <- as.data.frame(cbind(UF_T[,1]*UF_T[,12], UF_T[,2]*UF_T[,13], UF_T[,3]*UF_T[,14],
                             UF_T[,4]*UF_T[,12], UF_T[,5]*UF_T[,13], UF_T[,6]*UF_T[,14],
                             UF_T[,7]*UF_T[,13], UF_T[,8]*UF_T[,13], UF_T[,9]*UF_T[,12], 
                             UF_T[,10]*UF_T[,14], UF_T[,11], UF_T[,12], UF_T[,13], 
                             UF_T[,14], UF_T[,15]))

UF_TC["D"] <- UF_T[,16]
UF_TC["type"] <- "Population"
colnames(UF_TC) <- c("mr","rr","BC","BJ","BA","mC","AC","AR","PR","CR","R","Pj","Pa","C","T","D","type")
str(UF_TC)

UF_both <- rbind(UF_T, UF_TC)
UF_both_long <- gather(UF_both, key = UF, value = bm_day, mr,rr,BC,BJ,BA,AC,AR,PR,CR)#, factor_key=T) #long format 
str(UF_both_long)

UF_both_long %>%
  filter(UF %in% c("mr","rr"))  %>% 
  filter(T < 305 & T > 273) %>%
  filter(type %in% "Population") #%>% 

bmp <- UF_both_long %>%
  filter(UF %in% c("BC","BJ","BA"))  %>%
  filter(T < 305 & T > 289) %>%
  filter(D %in% "S") %>%
  filter(type %in% "Population") %>%
  mutate(T = T-292) %>%
   ggplot(., aes(T, bm_day, color=UF)) +
    geom_line(size=0.7, show.legend = FALSE) +
    scale_colour_manual(values = c("#000000","#DADADA","#999999"), name = "Biomass production", labels = c("Adult predator", "Consumer", "Juvenile predator"))+
    scale_x_continuous("T[K]", limits= c(-0.5,4.5), breaks = c(0,1,2,3,4)) +
    scale_linetype_manual(values = c("dotted", "solid"), name = "Type", labels = c("Per unit biomass", "Population")) +
    ggtitle("Biomass production") +
    annotate(geom="text", -Inf, Inf, label="A", hjust = -17, vjust = 2, size= 4, fontface = "bold") +
    ylab(expression(paste("Population level rate "))) +#, "[", m ~time^{-1}, "]"))) +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=10)
        )

bmms <- UF_both_long %>%
  filter(UF %in% c("BC","BJ","BA"))  %>%
  filter(T < 305 & T > 289) %>%
  filter(D %in% "S") %>%
  filter(type %in% "Per_biomass") %>%
  mutate(T = T-292) %>%
   ggplot(., aes(T, bm_day, color=UF)) +
    geom_line(size=0.7, show.legend = FALSE) +
    scale_colour_manual(values = c("#000000","#DADADA","#999999"), name = "Biomass production", labels = c("Adult predator", "Consumer", "Juvenile predator"))+
    scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                       limits= c(-0.5,4.5), breaks = c(0,1,2,3,4)) +
    scale_linetype_manual(values = c("dotted", "solid"), name = "Type", labels = c("Per unit biomass", "Population")) +
    annotate(geom="text", -Inf, Inf, label="D", hjust = -17, vjust = 2, size= 4, fontface = "bold") +
    ylab(expression(paste("Mass-specific rate "))) +#, "[", m~m^{-1}, time^{-1}, "]"))) +
  #scale_y_continuous(labels=c("0.0" = "0.00", "0.5" = "0.50", "1.0" = "1.00", "1.5" = "1.50"))+
   theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10)
)

irp <- UF_both_long %>%
  filter(UF %in% c("AC","PR","CR"))  %>% 
  filter(T < 305 & T > 289) %>%
  filter(D %in% "S") %>%
  filter(type %in% "Population") %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T, bm_day, color=UF)) +
   geom_line(size=0.7, show.legend = FALSE) +
   scale_colour_manual(values = c("#000000", "#DADADA", "#999999"), name = "Rate", labels = c("Predation", "Consump. C", "Consump. Pj"))+
   scale_x_continuous("T[K]", limits= c(-0.5,4.5), breaks = c(0,1,2,3,4)) +
   scale_linetype_manual(values = c("dotted", "solid"), name = "Type", labels = c("Per unit biomass", "Population")) +
   ggtitle("Consumption / Predation") +
   annotate(geom="text", -Inf, Inf, label="B", hjust = -17, vjust = 3, size= 4, fontface = "bold") +
   scale_y_continuous(labels=c("0" = "0.0", "1" = "1.0", "2" = "2.0", "3" = "3.0",  "4" = "4.0")) +
   theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

irms <- UF_both_long %>%
  filter(UF %in% c("AC","PR","CR"))  %>% 
  filter(T < 305 & T > 289) %>%
  filter(D %in% "S") %>%
  filter(type %in% "Per_biomass") %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T, bm_day, color=UF)) +
   geom_line(size=0.7, show.legend = FALSE) +
   scale_colour_manual(values = c("#000000", "#DADADA", "#999999"), name = "Rate", labels = c("Predation", "Consump. C", "Consump. Pj"))+
   scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")), 
                      limits= c(-0.5,4.5), breaks = c(0,1,2,3,4)) + # scales::pretty_breaks(n = 5)) +
   scale_linetype_manual(values = c("dotted", "solid"), name = "Type", labels = c("Per unit biomass", "Population")) +
   annotate(geom="text", -Inf, Inf, label="E", hjust = -18.5, vjust = 2, size= 4, fontface = "bold") +
   scale_y_continuous(labels=c("0" = "0.0", "1" = "1.0", "2" = "2.0", "3" = "3.0",  "4" = "4.0", "5"="5.0")) +
   theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=10) )

mrp <- UF_both_long %>%
  filter(UF %in% c("mr","rr"))  %>%
  filter(T < 305 & T > 289) %>%
  filter(type %in% "Population") %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T, bm_day, linetype = D, color=UF)) +
   geom_line(size=0.7,show.legend = FALSE) +
   scale_colour_manual(values = c("#999999","#000000"), name = "Biomass production", labels = c( "Maturation rate", "Reproduction rate"))+
   scale_linetype_manual(values = c("solid", "dashed"), name = "D", labels = c("stable", "unstable")) +
   scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                      limits= c(-0.5,4.5), breaks = c(0,1,2,3,4)) +
   annotate(geom="text", -Inf, Inf, label="C", hjust = -17, vjust = 2, size= 4, fontface = "bold") +
   ggtitle("Maturation / Reproduction") + 
   theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size=10),
        axis.title.x = element_text(size=10),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", colour = "black") ,
        legend.position = c(.1, .2),
        legend.key.size = unit(.5, "cm"),
        legend.text = element_text(size= 8)
  )

empty <- UF_both_long %>%
  ggplot(., aes(T, bm_day, linetype = D, color=UF)) +
  scale_y_continuous(labels=c("0" = "0.000", "1" = "1.000", "2" = "2.000", "3" = "3.000",  "4" = "4.000", "5"="5.000")) +  
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        strip.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = "transparent", colour = "white"),
        axis.title = element_blank(),
        axis.text = element_text(color = "white"),
        plot.title = element_blank(),
        axis.ticks = element_blank()
    )

#pdf("MS1_Fig2.pdf", width = 8, height = 6)
(bmp|irp|mrp)/(bmms|irms|empty)
#dev.off()

#pdf("MS1_legendFig2.pdf", width = 7, height = 6)
ggplot(EP_Tx_b015_long, aes(T,eq_bmd, color = D, linetype = D)) +
  geom_path(size=0.7) +
  theme(legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.box.margin = margin(1,1,1,1),
        legend.key.width = unit(1.5,"line"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.box.background = element_rect(fill = "transparent", colour = "white")
  ) +
  scale_linetype_manual(values = c("solid","solid", "solid", "solid", "dashed"), name = "State", labels = c("Adult predator", "Juvenile predator", "Consumer", "Stable PCR", "Unstable PCR")) +
  scale_color_manual(values = c("#000000","#999999","#DADADA","#000000","#000000"), name = "State", labels = c("Adult predator", "Juvenile predator", "Consumer", "Stable PCR", "Unstable PCR")) 
#dev.off()


### Figure 4 Plot community composition over T and beta ############

#Import and fix data for the PCR line
PCRLP_def  <- readMat("Data/default/LP_LP_TBeta_default(1).mat")
PCRLP_defx <- as.data.frame(t(PCRLP_def$x[5:6,]))
PCRLP_defx["Bf"] <- "PCR"
PCR5_def <- readMat("Data/default/BP_LP_TBeta_default(5).mat")
PCR5_defx <- as.data.frame(t(PCR5_def$x[5:6,]))
PCR5_defx["Bf"] <- "PCR"
PCR5_defx <- head(PCR5_defx, -6)
PCR6_def <- readMat("Data/default/BP_LP_TBeta_default(6).mat")
PCR6_defx <- as.data.frame(t(PCR6_def$x[5:6,]))
PCR6_defx["Bf"] <- "PCR"
PCR1_def <- readMat("Data/default/BP_LP_TBeta_default(1).mat")
PCR1_defx <- as.data.frame(t(PCR1_def$x[5:6,]))
PCR1_defx["Bf"] <- "PCR"
PCR2_def <- readMat("Data/default/BP_LP_TBeta_default(2).mat")
PCR2_defx <- as.data.frame(t(PCR2_def$x[5:6,]))
PCR2_defx["Bf"] <- "PCR"


#Import and fix data for the PR line
PR4_def <- readMat("Data/default/BP_LP_TBeta_default(4).mat")
PR4_defx <- as.data.frame(t(PR4_def$x[5:6,]))
PR4_defx["Bf"] <- "PR"
PR3_def <- readMat("Data/default/BP_LP_TBeta_default(3).mat")
PR3_defx <- as.data.frame(t(PR3_def$x[5:6,]))
PR3_defx["Bf"] <- "PR"
PR5_def <- readMat("Data/default/BP_LP_TBeta_default(5).mat")
PR5_defx <- as.data.frame(t(PR5_def$x[5:6,]))
PR5_defx["Bf"] <- "PR"
PR5_defx <- head(PR5_defx, -6)
PR6_def <- readMat("Data/default/BP_LP_TBeta_default(6).mat")
PR6_defx <- as.data.frame(t(PR6_def$x[5:6,]))
PR6_defx["Bf"] <- "PR"
PR1_def <- readMat("Data/default/BP_LP_TBeta_default(1).mat")
PR1_defx <- as.data.frame(t(PR1_def$x[5:6,]))
PR1_defx["Bf"] <- "PR"
PR2_def <- readMat("Data/default/BP_LP_TBeta_default(2).mat")
PR2_defx <- as.data.frame(t(PR2_def$x[5:6,]))
PR2_defx["Bf"] <- "PR"

defx <-as.data.frame(rbind(PCRLP_defx,PCR5_defx[order(PCR5_defx[,1], decreasing = T),],
                           PCR1_defx[order(PCR1_defx[,1], decreasing = T),],PCR2_defx,
                           PR4_defx[order(PR4_defx[,1]),],PR3_defx,PR5_defx[order(PR5_defx[,1], decreasing = T),],
                           PR1_defx[order(PR1_defx[,1], decreasing = T),],PR2_defx))

defx[nrow(defx)+1,] <- list(274.01,0,"PCR") #an extra point to make the geom_polygon close properly
colnames(defx) <- c("T","Beta","Bf")

#create the CR-state
T <-    c(275.0001,304.9999,304.9999,274.0001)
Beta <- c(0,0,1,1)
CR1_defx <- data.frame(T,Beta)
CR1_defx["Bf"] <- "CR"

#create the PCR/PR-state
PCRPR1 <- PCRLP_defx[which(PCRLP_defx[,1] <= 295.2355),] #remove points above T=295.2355
PCRPR2 <- rbind(PR3_defx,PR5_defx[order(PR5_defx[,1], decreasing = T),],
                PR1_defx[order(PR1_defx[,1], decreasing = T),],PR2_defx)
PCRPR2 <- PCRPR2[which(PCRPR2[,1] <= 295.2355),]  #remove points above T=295.2355
PCRPR2 <- PCRPR2[which(PCRPR2[,2] <= 0.5),]  #remove points above BEta=0.5
PCRPR2 <- PCRPR2[which(PCRPR2[,1] > 293.4648),]  #remove points below T= 2.934647919994109e+02
PCRPR_defx <- rbind(PCRPR2,PCRPR1[order(PCRPR1[,1]),])
PCRPR_defx[,3] <- "PCRPR"
colnames(PCRPR_defx) <- c("T","Beta","Bf")

defx <- rbind(defx,CR1_defx,PCRPR_defx)
defx$Bf <- factor(defx$Bf, levels = c("CR", "PR", "PCR","PCRPR"))

# fix the Ip line 
Ip1 <- readMat("Data/default/BP_BP_TBeta_PreInv(1).mat")
Ip2 <- readMat("Data/default/BP_BP_TBeta_PreInv(2).mat")
#PR6_defx <- as.data.frame(t(PR6_def$x[5:6,]))
Ip <- as.data.frame(rbind(t(Ip1$x[5:6,]),t(Ip2$x[5:6,]))) # the stable and unstable coexistence
colnames(Ip) <- c("T","Beta")
Ip["T"] <- Ip["T"]-292
str(Ip)

# beta-Temp plot
#pdf("MS1_Fig4.pdf", width = 5, height = 4)
defx %>% 
  filter(T < 305 && T > 274) %>%
  mutate(T = T-292) %>%
  ggplot(.) + 
   geom_polygon(aes(T,Beta, fill=Bf)) + 
   geom_line(data = Ip, aes(T,Beta), linetype = "dashed", size= 0.2, colour = "black") +   
   scale_fill_manual(values = c("white", "#DADADA", "#999999", "#000000"), name = "State",
                     labels = c("CR","PR/CR","PCR/CR","PCR/PR/CR")) +
   scale_color_manual(values = c("black","transparent","transparent","transparent"))+
   scale_y_continuous(expand = c(0, 0), breaks=c(0.1,0.3,0.5,0.7,0.9)) +
   scale_x_continuous(expand = c(0, 0), breaks=c(-8,-4,0,4,8))+
   ylab(expression(paste("Predator resource preference (", beta, ")"))) +   
   xlab(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]"))) +
   coord_cartesian(xlim = c(-8.5, 8.5), ylim = c(0, 1))+
   theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        legend.title = element_blank(),
        legend.key.size = unit(.4, "cm"),
        legend.text = element_text(size= 8))
#dev.off()

### APPENDIX A1, Non-dynamic intake rates over Temp #####

#IGP_Mod8Temp

Rc = Pj = Pa = Cs = 1

d = 1
K = 2.50000000000000
I_P = 2.50000000000000
I_Cs = 10.0000000000000
M_Cs=1
M_P=0.300000000000000
u_Cs=0.100000000000000
u_P=0.0300000000000000
Hc=1
Hp=1
ap=0.500000000000000
ac=0.500000000000000
z=0.0100000000000000
beta=0
k=8.61733200000000e-05
rE=0.400000000000000
KE=-0.400000000000000
ME=0.600000000000000
Eri=1
Eru= 1
T= 273:310
T0=292
omb=-0.0500000000000000
oma=1

r_d = exp(rE*(T-T0)/(k*T*T0))   
r_K = exp(KE*(T-T0)/(k*T*T0))
r_i = exp(ME*Eri*(T-T0)/(k*T*T0))
r_u = exp(ME*Eru*(T-T0)/(k*T*T0))
r_m = exp(ME*(T-T0)/(k*T*T0))

om = oma + omb*(T-T0)

# for Coordinates = 1 & beta=0
Int_Cs   = (I_Cs*Rc / (Hc + Rc))*r_i
Int_Pj   = (I_P*Rc / (Hp + Rc))*r_i
#Int_Pa_Rc= (I_P*om*beta*Rc / (Hp + beta*Rc + (1-beta)*Cs))*r_i
Int_Pa_Cs= (I_P*om*(1-beta)*Cs / (Hp + beta*Rc + (1-beta)*Cs))*r_i

intake <- as.data.frame(cbind(log(Int_Cs),log(Int_Pj),log(Int_Pa_Cs),T))
colnames(intake) <- c("Cs","Pj","Pa","T")#,"f")
str(intake)
intake_long <- gather(intake, key = "coord", value = "rate",1:3)#, eq_bmd, Pa, Pj, C, R) #long format 
str(intake_long)

#pdf("MS1_AppendixA1.pdf", width = 5, height = 3)
intake_long %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T, rate, colour = coord)) +
  geom_line(size=0.8) +
  ylab(expression(paste("ln(Intake rate) "))) +#, "[", m ~m^{-1}~time^{-1}, "]"))) +
  scale_colour_manual(values = c("#DADADA","#000000","#999999"), name = "Species/Stage", labels = c("Consumer", "Adult predator", "Juvenile predator"))+
  scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                     limits= c(-8,8), breaks = scales::pretty_breaks(n = 5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "white", colour = "white"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        legend.title = element_blank()
        )
#dev.off()


#### APPENDIX A3 Varying Temp effect processes #####
# Null: no tempeffect (EAr=0.6). EP_T_rEA_0.6_notempeffect(2) EP_T_rEA_0.6_notempeffect(1)
# Delta: only delta temp effect (EAr=0.4). EP_T_rEA_0.4_deltaonly(2) EP_T_rEA_0.4_deltaonly(1)
# DeltaAndK: reosurces as default (EAr=0.4, EAK=-0.4).  EP_T_rEA_04_KEA_-04_muEA_06(2), EP_T_rEA_04_KEA_-04_muEA_06(1)
# NoTMort: without temp-dep mort (EAmu = 0). EP_T_mort0(2) EP_T_mort0(1)
# Default: with omegab =-0.05. default 

#Null:
Null_1 <- readMat("Data/default/EP_T_rEA_0.6_notempeffect(1).mat") #292 < T and ustable
Null_2 <- readMat("Data/default/EP_T_rEA_0.6_notempeffect(2).mat") #292 > T

Null_x <- as.data.frame(t(cbind(Null_1$x, Null_2$x)))
Null_x["D"] <- "S1" # set all values to stable PCR state

Null_x$Sce <- "Null"
#straight line, no max value
colnames(Null_x) <- c("R","Pj","Pa","C","T","D","Sce")
str(Null_x)

#Only delta 0.4
Delta_1 <- readMat("Data/default/EP_T_rEA_0.4_deltaonly(1).mat") # 292 > T
Delta_2 <- readMat("Data/default/EP_T_rEA_0.4_deltaonly(2).mat") # 292 < T and unstable

Delta_x <- as.data.frame(t(cbind(Delta_1$x, Delta_2$x))) # the stable and unstable coexistence
Delta_x["D"] <- "S1"                                         # set all values to stable PCR state
Delta_x$D[(which(Delta_x[5]==max(Delta_x[5]))+1):nrow(Delta_x)] <- "U" # the values above the index of max temp (the LP) and until the matcont-backwards in EP_T2 are unstable
Delta_1$s # the index of the Hopf is 78
Delta_x$D[78:ncol(Delta_1$x)]  <- "Cyc" # set row below Hopf to cyclic state
Delta_x$Sce <- "Delta"

colnames(Delta_x) <- c("R","Pj","Pa","C","T","D","Sce")
str(Delta_x)

# Delta & K
DeltaAndK_1 <- readMat("Data/default/EP_T_rEA_04_KEA_-04_muEA_06(1).mat") #292 > T
DeltaAndK_2 <- readMat("Data/default/EP_T_rEA_04_KEA_-04_muEA_06(2).mat") #292 < T and ustable

DeltaAndK_x <- as.data.frame(t(cbind(DeltaAndK_1$x, DeltaAndK_2$x))) # the stable and unstable coexistence
DeltaAndK_x["D"] <- "S1"                                         # set all values to stable PCR state
DeltaAndK_x$D[(which(DeltaAndK_x[5]==max(DeltaAndK_x[5]))+1):nrow(DeltaAndK_x)] <- "U" # the values above the index of max temp (LP) and until the matcont-backwards in EP_T2 are unstable
DeltaAndK_1$s # the index of the Hopf is 35
DeltaAndK_x$D[35:ncol(DeltaAndK_1$x)]  <- "Cyc" # set row below Hopf to cyclic state
DeltaAndK_x$Sce <- "DeltaAndK"

colnames(DeltaAndK_x) <- c("R","Pj","Pa","C","T","D","Sce")
str(DeltaAndK_x)

# NoTMort 
NoTMort_1 <- readMat("Data/default/EP_T_mort0(1).mat") #292 < T and ustable
NoTMort_2 <- readMat("Data/default/EP_T_mort0(2).mat") #292 > T

NoTMort_x <- as.data.frame(t(cbind(NoTMort_2$x[,1:240], NoTMort_1$x))) # the stable and unstable coexistence, remove temps below ca. 270 K so that the 1 and 2 curves dont overlap
NoTMort_x["D"] <- "S1"                                         # set all values to stable PCR state
NoTMort_x$D[(which(NoTMort_x[5]==max(NoTMort_x[5]))+1):nrow(NoTMort_x)] <- "U" # the values above the index of max temp (LP) and until the matcont-backwards in EP_T2 are unstable
NoTMort_2$s # the index of the Hopf is 24
NoTMort_x$D[24:240] <- "Cyc" # set values below Hopf to cyclic, note that only T > 270 K used (240 cols)
NoTMort_x$Sce <- "NoTMort"

colnames(NoTMort_x) <- c("R","Pj","Pa","C","T","D","Sce")
str(NoTMort_x)

# Default 
EP_T1 <- readMat("Data/default/EP_T_default(1).mat")
EP_T2 <- readMat("Data/default/EP_T_default(2).mat")

# create a column indicating dynamic stability
Default_x <- as.data.frame(t(cbind(EP_T1$x, EP_T2$x))) # the stable and unstable coexistence
Default_x["D"] <- "S1"                                 # set all values to stable PCR state
Default_x$D[(which(EP_Tx1[5]==max(EP_Tx1[5]))+1):ncol(EP_T1$x)] <- "U" # the values above the index of max temp (LP) and until the matcont-backwards in EP_T2 are unstable
Default_x <- Default_x %>% filter(D != "S2") # filter out alternative state
EP_T2$s # the index of the Hopf is 24
Default_x$D[(ncol(EP_T1$x)+24):nrow(Default_x)] <- "Cyc" # set values below Hopf to cyclic, a bit comlpex 
Default_x$Sce <- "Default"

colnames(Default_x) <- c("R","Pj","Pa","C","T","D","Sce")
str(Default_x)

# bind all scenarios 
A3_df <- rbind(Null_x,Delta_x,DeltaAndK_x,NoTMort_x,Default_x)
A3_df_long <- gather(A3_df, coord, eq_bmd, Pa, Pj, C, R) #long format 
A3_df_long$coord_f = factor(A3_df_long$coord, levels=c('Pa','Pj','C','R')) # to get coords in the same order of factes as previous plots
A3_df_long$Sce_f = factor(A3_df_long$Sce, levels=c('Null','Delta','DeltaAndK','NoTMort','Default')) # to get coords in the same order of factes as previous plots
str(A3_df_long)


#pdf("MS1_AppendixA3_1.pdf", width = 8, height = 3.5)
A3_df_long %>%
  filter(T > 275) %>%
  filter(D != "Cyc") %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T,eq_bmd, color = Sce_f, linetype = D, alpha = D)) +
  geom_line(size=0.4) +
  facet_grid(.~coord_f, labeller = labeller(coord_f = Fig1bmd_labs)) +
  ylab(expression(paste("Equilibrium Biomass density "))) + #, "[", g ~V^{-1}, "]"))) +   
  scale_linetype_manual(values = c("solid", "dashed"), name = "State", labels = c("Stable PCR", "Unst. PCR")) +
  scale_colour_brewer(palette="Set1", name = NULL, 
                      labels = c("Null", "Resource turnover", "Resource turnover & Rmax", "Mortality temp. independent", "Default")) +
  scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                     breaks = scales::pretty_breaks(n = 6)) +
  scale_alpha_manual(values = c(1,0.5), name = "State", labels = c("Stable PCR", "Unst. PCR")) +
  coord_cartesian(ylim = c(0, 2))+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.key.width = unit(1.5,"line"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.box.background = element_rect(fill = "transparent", colour = "white"),
        strip.text.x = element_text(size = 10) )
#dev.off()


### Rate functions for each temperature scenario

# .mat h rows corresponding to user functions (* multiplies with coordinate for rate of coordinate biomass,  rownumber in UF_T, see next # create..)
#3.	 mat – mr     (*Pj, 1)
#4.	 rep – rr     (*Pa, 2)    
#5.	 BmpCs – BC   (*C, 3)    
#6.	 BmpPj – BJ   (*Pj, 4)    
#7.	 BmpPa – BA   (*Pa, 5)      
#8. Csmort - mC - not used but is not deleted from m.file
#9. IngPaC – AC  (*Pa, 7)    #Adult on Consumer
#10. IngPaR – AR  (*Pa, 8)    #Adult on Resource
#11. IngPjR - PR  (*Pj, 9)   #Juvenile on Resource
#12. IngCsR - CR  (*C, 10)    #Consumer on resource

#'Null','Delta','DeltaAndK','NoTMort','Default')

UF_Null <- t(rbind(Null_2$h[3:12,],Null_2$x)) %>% # bind UF data with coordinates and Temp
  .[order(.[,15], decreasing=T), ] %>% # backwards or forwards data ordered
  rbind(., t(rbind(Null_1$h[3:12,], Null_1$x))) %>%
  as.data.frame(.)
colnames(UF_Null) <- c("mr","rr","BC","BJ","BA","mC","AC","AR","PR","CR", "R","Pj","Pa","C","T")
UF_Null["D"] <- "S"   # set all values to stable PCR state
UF_Null["type"] <- "Per_biomass"
UF_Null["Sce"] <- "Null"
str(UF_Null)

UF_Delta <- t(rbind(Delta_1$h[3:12,1:78],Delta_1$x[,1:78])) %>% # the index of the Hopf is 78
  .[order(.[,15], decreasing=F), ] %>%
  rbind(., t(rbind(Delta_2$h[3:12,], Delta_2$x))) %>%
  as.data.frame(.)
colnames(UF_Delta) <- c("mr","rr","BC","BJ","BA","mC","AC","AR","PR","CR", "R","Pj","Pa","C","T")
UF_Delta["D"] <- "S"   # set all values to stable PCR state
UF_Delta$D[(which(UF_Delta$T==max(UF_Delta$T))+1):nrow(UF_Delta)] <- "U" # set the values above the index of max temp (LP) and until the end of UF_T1 to unstable
UF_Delta["type"] <- "Per_biomass"
UF_Delta["Sce"] <- "Delta"
str(UF_Delta)

UF_DeltaAndK <- t(rbind(DeltaAndK_1$h[3:12,1:35], DeltaAndK_1$x[,1:35])) %>% # the index of the Hopf is 35
  .[order(.[,15], decreasing=F), ] %>%
  rbind(., t(rbind(DeltaAndK_2$h[3:12,], DeltaAndK_2$x))) %>%
  as.data.frame(.)
colnames(UF_DeltaAndK) <- c("mr","rr","BC","BJ","BA","mC","AC","AR","PR","CR", "R","Pj","Pa","C","T")
UF_DeltaAndK["D"] <- "S"   # set all values to stable PCR state
UF_DeltaAndK$D[(which(UF_DeltaAndK$T==max(UF_DeltaAndK$T))+1):nrow(UF_DeltaAndK)] <- "U" # set the values above the index of max temp (LP) and until the end of UF_T1 to unstable
UF_DeltaAndK["type"] <- "Per_biomass"
UF_DeltaAndK["Sce"] <- "DeltaAndK"
str(UF_DeltaAndK)

UF_NoTMort <- t(rbind(NoTMort_2$h[3:12,1:24],NoTMort_2$x[,1:24])) %>% # the index of the Hopf is 24
  .[order(.[,15], decreasing=F), ] %>%
  rbind(., t(rbind(NoTMort_1$h[3:12,], NoTMort_1$x))) %>%
  as.data.frame(.)
colnames(UF_NoTMort) <- c("mr","rr","BC","BJ","BA","mC","AC","AR","PR","CR", "R","Pj","Pa","C","T")
UF_NoTMort["D"] <- "S"   # set all values to stable PCR state
UF_NoTMort$D[(which(UF_NoTMort$T==max(UF_NoTMort$T))+1):nrow(UF_NoTMort)] <- "U" # set the values above the index of max temp (LP) and until the end of UF_T1 to unstable
UF_NoTMort["type"] <- "Per_biomass"
UF_NoTMort["Sce"] <- "NoTMort"
str(UF_NoTMort)

Default_1 <- UF_T1 
Default_2 <- UF_T2
UF_Default <- t(rbind(Default_2$h[3:12,1:24],Default_2$x[,1:24])) %>% # the index of the Hopf is 24
  .[order(.[,15], decreasing=F), ] %>%
  rbind(., t(rbind(Default_1$h[3:12,], Default_1$x))) %>%
  as.data.frame(.)
colnames(UF_Default) <- c("mr","rr","BC","BJ","BA","mC","AC","AR","PR","CR", "R","Pj","Pa","C","T")
UF_Default["D"] <- "S"   # set all values to stable PCR state
UF_Default$D[(which(UF_Default$T==max(UF_Default$T))+1):nrow(UF_Default)] <- "U" # set the values above the index of max temp (LP) and until the end of UF_T1 to unstable
UF_Default["type"] <- "Per_biomass"
UF_Default["Sce"] <- "Default"
str(UF_Default)

A3_UF <- rbind(UF_Null,UF_Delta,UF_DeltaAndK,UF_NoTMort,UF_Default)
A3_UF_C <- as.data.frame(cbind(A3_UF[,1]*A3_UF[,12], A3_UF[,2]*A3_UF[,13], A3_UF[,3]*A3_UF[,14],
                                      A3_UF[,4]*A3_UF[,12], A3_UF[,5]*A3_UF[,13], A3_UF[,6]*A3_UF[,14],
                                      A3_UF[,7]*A3_UF[,13], A3_UF[,8]*A3_UF[,13], A3_UF[,9]*A3_UF[,12], 
                                      A3_UF[,10]*A3_UF[,14], A3_UF[,11], A3_UF[,12], A3_UF[,13], 
                                      A3_UF[,14], A3_UF[,15]))
A3_UF_C["D"] <- A3_UF[,16]
A3_UF_C["type"] <- "Population"
A3_UF_C["Sce"] <- A3_UF[,18]
colnames(A3_UF_C) <- c("mr","rr","BC","BJ","BA","mC","AC","AR","PR","CR","R","Pj","Pa","C","T","D","type","Sce")
str(A3_UF_C)
str(A3_UF)

A3_UF_both <- rbind(A3_UF, A3_UF_C)
A3_UF_both_long <- gather(A3_UF_both, key = UF, value = bm_day, mr,rr,BC,BJ,BA,AC,AR,PR,CR)#, factor_key=T) #long format 
A3_UF_both_long$Sce_f = factor(A3_UF_both_long$Sce, levels=c('Null','Delta','DeltaAndK','NoTMort','Default')) # to get coords in the same order of factes as previous plots
A3_UF_both_long$UF_f = factor(A3_UF_both_long$UF, levels=c("AR", "AC", "PR", "CR", "BA", "BJ", "BC",  "rr", "mr")) # to get coords in the same order of factes as previous plots
str(A3_UF_both_long)

### Population level rate plot for each scenario
bmp_UF <- A3_UF_both_long %>%
  filter(UF_f %in% c("BC","BJ","BA")) %>% #"AC","AR","PR","CR","mr","rr"
  filter(T < 325 & T > 274) %>%
  filter(D %in% "S") %>%
  filter(type %in% "Population") %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T, bm_day, color = Sce_f)) +
  geom_line(size=0.7, show.legend = FALSE ) +  
  facet_grid(UF_f~., scales="free_y") + #, labeller = labeller(coord_f = Fig1bmd_labs) )+ 
  scale_colour_brewer(palette="Set1", name = NULL) +#, labels = c("Null (No temp. effect)", "Resource turnover", "Resource turnover & Rmax", "No temp. mortality", "Default")) +  
  scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                     limits= c(-10,30), breaks = scales::pretty_breaks(n = 5)) +
  ggtitle("Biomass production") +
  ylab(expression(paste("Population level rate "))) +#, "[", m ~time^{-1}, "]"))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        plot.title = element_text(hjust = 0.5, size=10),
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12) )

supp.labsA3_UF <- c("Adult predator", "Juvenile predator", "Consumer", "Maturation rate", "Reproduction rate")
names(supp.labsA3_UF) <- c("AC", "PR", "CR","mr", "rr")

irp_UF <- A3_UF_both_long %>%
  filter(UF_f %in% c("AC","PR","CR")) %>%
  filter(T < 325 & T > 274) %>%
  filter(D %in% "S") %>%
  filter(type %in% "Population") %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T, bm_day, color = Sce_f)) +
  geom_line(size=0.7, show.legend = FALSE) +  
  facet_grid(UF_f~., scales="free_y", labeller = labeller(UF_f = supp.labsA3_UF) )+ 
  scale_colour_brewer(palette="Set1", name = NULL) +
  scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                     limits= c(-10,30), breaks = scales::pretty_breaks(n = 5)) +
  ggtitle("Consumption / Predation") +
  ylab(expression(paste("Population level rate ", "[", m ~time^{-1}, "]"))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        plot.title = element_text(hjust = 0.5, size=10),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=12) )

mrr_UF <-  A3_UF_both_long %>%
  filter(UF_f %in% c("mr","rr")) %>%
  filter(T < 325 & T > 274) %>%
  filter(bm_day < 0.15) %>%
  filter(type %in% "Population") %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T, bm_day, color = Sce_f, linetype = D, alpha = D)) +
  geom_line(size=0.7, show.legend = TRUE ) +  
  facet_grid(UF_f~., scales="free_y", labeller = labeller(UF_f = supp.labsA3_UF) )+ 
  scale_alpha_manual(values = c(1, 0.5), name = "State") +
  guides(alpha=FALSE, color = guide_legend(order = 1), linetype = guide_legend(order = 2))  +
  scale_colour_brewer(palette="Set1", name = NULL, labels = c("Null", "Resource turnover", "Resource turnover & Rmax", "Mortality temp. independent", "Default")) +
  scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                     limits= c(-10,30), breaks = scales::pretty_breaks(n = 5)) +
  scale_linetype_manual(values = c("solid", "dashed"), name = NULL, labels = c("Stable PCR", "Unstable PCR")) +
  ggtitle("Maturation / Reproduction") +
  ylab(expression(paste("Population level rate ", "[", m ~time^{-1}, "]"))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        plot.title = element_text(hjust = 0.5, size=10),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.key = element_rect(fill = "transparent")
)

#pdf("MS1_AppendixA3_2.pdf", width = 8, height = 6)
bmp_UF|irp_UF|mrr_UF
#dev.off()

# Mass specific rate plots for each scenario

bmms_UF <- A3_UF_both_long %>%
  filter(UF_f %in% c("BC","BJ","BA")) %>%
  filter(T < 325 & T > 274) %>%
  filter(D %in% "S") %>%
  filter(type %in% "Per_biomass") %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T, bm_day, color = Sce_f)) +
  geom_line(size=0.7, show.legend = FALSE ) +  
  facet_grid(UF_f~., scales="free_y") + #, labeller = labeller(coord_f = Fig1bmd_labs) )+ 
  scale_colour_brewer(palette="Set1", name = NULL) +#, labels = c("Null (No temp. effect)", "Resource turnover", "Resource turnover & Rmax", "No temp. mortality", "Default")) +  
  scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                     limits= c(-10,30), breaks = scales::pretty_breaks(n = 5)) +
  ggtitle("Biomass production") +
  ylab(expression(paste("Mass-specific rate "))) +#, "[", m~m^{-1}, time^{-1}, "]"))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        plot.title = element_text(hjust = 0.5, size=10),
        strip.text = element_blank(),
        axis.title.y = element_text(size=12)
)

supp.labsA3_UF <- c("Adult predator", "Juvenile predator", "Consumer", "Maturation rate", "Reproduction rate")
names(supp.labsA3_UF) <- c("AC", "PR", "CR","mr", "rr")

irms_UF <- A3_UF_both_long %>%
  filter(UF_f %in% c("AC","PR","CR")) %>%
  filter(T < 325 & T > 274) %>%
  filter(D %in% "S") %>%
  filter(type %in% "Per_biomass") %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T, bm_day, color = Sce_f)) +
  geom_line(size=0.7, show.legend = TRUE) +  
  facet_grid(UF_f~., scales="free_y", labeller = labeller(UF_f = supp.labsA3_UF) )+ 
  guides(alpha=FALSE, color = guide_legend(order = 1), linetype = guide_legend(order = 2))  +
  scale_colour_brewer(palette="Set1", name = NULL, labels = c("Null", "Resource turnover", "Resource turnover & Rmax", "Mortality temp. independent", "Default")) +
  scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                     limits= c(-10,30), breaks = scales::pretty_breaks(n = 5)) +
  ggtitle("Consumption / Predation") +
  ylab(expression(paste("Mass-specific rate ", "[", m~m^{-1}, time^{-1}, "]"))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        plot.title = element_text(hjust = 0.5, size=10),
        axis.title.y = element_blank(),
        legend.key = element_rect(fill = "transparent")
  )

mrrx_UF <- A3_UF_both_long %>%
  filter(UF_f %in% c("mr","rr")) %>%
  filter(T < 325 & T > 274) %>%
  filter(bm_day < 0.15) %>%
  filter(type %in% "Per_biomass") %>%
  mutate(T = T-292) %>%
  ggplot(., aes(T, bm_day, color = Sce_f, linetype = D, alpha = D)) +
  geom_line(size=0.7, show.legend = TRUE ) +  
  facet_grid(UF_f~., scales="free_y", labeller = labeller(UF_f = supp.labsA3_UF) )+ 
  scale_alpha_manual(values = c("1", "0.5"), name = "State") +
  guides(alpha=FALSE, color = guide_legend(order = 1), linetype = guide_legend(order = 2))  +
  scale_colour_brewer(palette="Set1", name = NULL, labels = c("Null", "Resource turnover", "Resource turnover & Rmax", "Mortality temp. independent", "Default")) +
  scale_x_continuous(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]")),
                     limits= c(-10,30), breaks = scales::pretty_breaks(n = 5)) +
  scale_linetype_manual(values = c("solid", "dashed"), name = NULL, labels = c("Stable PCR", "Unstable PCR")) +
  ggtitle("Maturation / Reproduction") +
  ylab(expression(paste("Mass-specific rate ", "[", m~m^{-1}, time^{-1}, "]"))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        plot.title = element_text(hjust = 0.5, size=10),
        axis.title.y = element_blank()
)

#pdf("MS1_AppendixA3_3.pdf", width = 8, height = 6)
bmms_UF|irms_UF
#dev.off()


### APPENDIX A4, Plot community composition over T and beta, I_Cs = 15 ####

#Import and fix data for the PCR line
PCRLP_ICs15  <- readMat("Data/I_Cs_15/LP_LP_predaExclu(1).mat")
PCRLP_ICs15x <- as.data.frame(t(PCRLP_ICs15$x[5:6,]))
PCRLP_ICs15x["Bf"] <- "PCR"
PCR_ICs15 <- readMat("Data/I_Cs_15/BP_LP_conInva(2).mat")
PCR_ICs15x <- as.data.frame(t(PCR_ICs15$x[5:6,])) 
PCR_ICs15x <- PCR_ICs15x[which(PCR_ICs15x[,1] <= min(PCRLP_ICs15x[,1])),]
PCR_ICs15x["Bf"] <- "PCR"

#Import and fix data for the PR line
PR1_ICs15 <- readMat("Data/I_Cs_15/BP_LP_conInva(1).mat")
PR1_ICs15x <- as.data.frame(t(PR1_ICs15$x[5:6,]))
PR1_ICs15x["Bf"] <- "PR"
PR2_ICs15 <- readMat("Data/I_Cs_15/BP_LP_conInva(2).mat")
PR2_ICs15x <- as.data.frame(t(PR2_ICs15$x[5:6,]))
PR2_ICs15x["Bf"] <- "PR"

ICs15x <-as.data.frame(rbind(PCRLP_ICs15x,PCR_ICs15x,
                             PR2_ICs15x[order(PR2_ICs15x[,1], decreasing = F),],PR1_ICs15x))
                           
ICs15x[nrow(ICs15x)+1,] <- list(274.01,0,"PCR") #an extra point to make the geom_polygon close properly for the PCR/PR state
colnames(ICs15x) <- c("T","Beta","Bf")

#create the CR-state
T <-    c(275.0001,304.9999,304.9999,274.0001)
Beta <- c(0,0,1,1)
CR1_ICs15x <- data.frame(T,Beta)
CR1_ICs15x["Bf"] <- "CR"

#create the PCR/PR-state
PCRPR1_ICs15 <- PCRLP_ICs15x[which(PCRLP_ICs15x[,1] <= 292.566633),] #remove points above T=292.566633 (the corner to the right in the shape)
PCRPR2_ICs15 <- rbind(PR2_ICs15x[order(PR2_ICs15x[,1], decreasing = F),],PR1_ICs15x)
PCRPR2_ICs15 <- PCRPR2_ICs15[which(PCRPR2_ICs15[,1] <= 292.566633),]  #remove points above T=295.2355
PCRPR2_ICs15 <- PCRPR2_ICs15[which(PCRPR2_ICs15[,2] <= 0.4274),]  #remove points above Beta=0.427439425586470 (i.e. max of beta, th top corner of the shape)
PCRPR2_ICs15 <- PCRPR2_ICs15[which(PCRPR2_ICs15[,1] > 291.089),]  #remove points below T= 2.934647919994109e+02 (the corner to the right in the shape)
PCRPR_ICs15x <- rbind(PCRPR2_ICs15[order(PCRPR2_ICs15[,1], decreasing = T),], PCRPR1_ICs15[order(PCRPR1_ICs15[,1], decreasing = F),])
PCRPR_ICs15x["Bf"] <- "PCRPR"
colnames(PCRPR_ICs15x) <- c("T","Beta","Bf")

ICs15x <- rbind(ICs15x,CR1_ICs15x,PCRPR_ICs15x)
ICs15x$Bf <- factor(ICs15x$Bf, levels = c("CR", "PR", "PCR","PCRPR"))

# Fix the Ip line 
Ip1_ICs15 <- readMat("Data/I_Cs_15/BP_LP_predInva(1).mat")
Ip2_ICs15 <- readMat("Data/I_Cs_15/BP_LP_predInva(2).mat")
Ip_ICs15 <- as.data.frame(rbind(t(Ip1_ICs15$x[5:6,]),t(Ip2_ICs15$x[5:6,]))) # the stable and unstable coexistence
Ip_ICs15$T <- Ip_ICs15$T-292
colnames(Ip_ICs15) <- c("T","Beta")

# beta-Temp plot
#pdf("MS1_AppendixA4.pdf", width = 5, height = 4)
ICs15x %>% 
  filter(T < 305 && T > 274) %>%
  mutate(T = T-292) %>%
  ggplot(.) + 
  geom_polygon(aes(T,Beta, fill=Bf))  +
  geom_line(data = Ip_ICs15, aes(T,Beta), linetype = "dashed", size= 0.2, colour = "black") +   
  scale_fill_manual(values = c("white", "#DADADA", "#999999", "#000000"), name = "State",
                   labels = c("CR","PR/CR","PCR/CR","PCR/PR/CR")) +
  scale_color_manual(values = c("black","transparent","transparent","transparent"))+
  scale_y_continuous(expand = c(0, 0), breaks=c(0.1,0.3,0.5,0.7,0.9)) +
  scale_x_continuous(expand = c(0, 0), breaks=c(-8,-4,0,4,8)) +
  ylab(expression(paste("Predator resource preference (", beta, ")"))) +   
  xlab(expression(paste(italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]"))) +
  coord_cartesian(xlim = c(-8.5, 8.5), ylim = c(0, 1))+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        legend.title = element_blank(),
        legend.key.size = unit(.4, "cm"),
        legend.text = element_text(size= 8))
#dev.off()

### APPENDIX A5, Predator extinction temperature( beta=0,0.3) with a warm adapted predator ####

ombPCR_LP_def1 <- readMat("Data/omb_var/LP_TempOmb(1).mat")
ombPCR_LP_def2 <- readMat("Data/omb_var/LP_TempOmb(2).mat")
ombPCR_LP_def1 <- as.data.frame(t(ombPCR_LP_def1$x[5:6,]))
ombPCR_LP_def2 <- as.data.frame(t(ombPCR_LP_def2$x[5:6,]))
ombPCR_LP_def <- rbind(ombPCR_LP_def1,ombPCR_LP_def2)
ombPCR_LP_def["beta"] <- 0

ombPCR_LP_beta03_1 <- readMat("Data/omb_var/LP_Tempomb_beta03(1).mat")
ombPCR_LP_beta03_2 <- readMat("Data/omb_var/LP_Tempomb_beta03(2).mat")
ombPCR_LP_beta03_1 <- as.data.frame(t(ombPCR_LP_beta03_1$x[5:6,]))
ombPCR_LP_beta03_2 <- as.data.frame(t(ombPCR_LP_beta03_2$x[5:6,]))
ombPCR_LP_beta03 <- rbind(ombPCR_LP_beta03_1,ombPCR_LP_beta03_2)
ombPCR_LP_beta03["beta"] <- 0.3

ombPCR_LP <- rbind(ombPCR_LP_def,ombPCR_LP_beta03)
colnames(ombPCR_LP) <- c("T","Omb","beta")

#pdf("MS1_AppendixA5.pdf", width = 5, height = 4)
ombPCR_LP %>% 
  filter(T < 305 && T > 274) %>%
  mutate(T = T-292) %>%
  ggplot(.,aes(Omb, T, linetype = as.factor(beta))) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed"), name = expression(beta)) +
  ylab(expression(paste("P extinction temp, ",italic(Delta),italic("T "),"(relative to ",italic("T")["0"],")"," [K]"))) +   
  xlab("Temp effect on max. intake rate of P") +
  coord_cartesian(ylim = c(0, 15), xlim = c(-0.10, 0.05)) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_rect(fill = "transparent", colour = "black"),
        legend.key.size = unit(.4, "cm"),
        legend.text = element_text(size = 8),
        legend.box.margin = margin(1,1,1,1),
        legend.key.width = unit(1.5,"line"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.box.background = element_rect(fill = "transparent", colour = "white"),
        strip.text.x = element_text(size = 10))
#dev.off()
