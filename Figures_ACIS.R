#Update
library(cowplot)
library(decisionSupport)
library(dplyr)
library(gganimate)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(gridExtra)
library(tidyverse)
#install.packages("gridExtra")
#install.packages("cowplot")
#install.packages("gganimate")
#install.packages("ggpubr")

# 1. PLOPTING NPV####
#1.1. NPV: economic, social and env costs and benefits####
# Plotting combined NPV for comparisons
# Subset NPV data
mc_result<-read.csv("MCResults/mcSimulationResults.csv",header = TRUE,sep=",")
npv_table<-mc_result[,c(144:147)]

# Convert data into million unit
npv_million<-npv_table/1000000

# Create data frame (not just vector) for NPV plot. The data frame contains
# values of different NPVs (groups) for plotting histograms 
#The data frame might need to be grouped to be able to use ggplot 
# https://statisticsglobe.com/draw-overlaying-histograms-with-ggplot2-in-r
npv_data_frame <- data.frame(values = c(npv_million$NPV_Intervention1,                    
                              npv_million$NPV_Intervention2,
                              npv_million$NPV_Intervention3,
                              npv_million$NPV_Intervention4),
                   group = c(rep("1) Weather station-SMS-Gender", 10000),
                             rep("2) SMS-Gender", 10000),
                             rep("3) SMS-Loudspeaker", 10000),
                             rep("4) Paper-Loudspeaker", 10000)))
# Plot smoothed density estimation
# https://www.data-to-viz.com/graph/density.html
# https://chemicalstatistician.wordpress.com/2013/06/09/exploratory-data-analysis-kernel-density-estimation-in-r-on-ozone-pollution-data-in-new-york-and-ozonopolis/

npvplot= ggplot(npv_data_frame, aes(x = values, fill = group, color=group)) +                       
  geom_density(alpha = 0.05)+
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Net Present Value (in million $)")
# Remove background color
# https://felixfan.github.io/ggplot2-remove-grid-background-margin/
# Remove legend
# https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
npv<-npvplot+ theme_bw() +theme(legend.position = c(.7, .8))+
  theme(legend.title = element_blank())+
  ggtitle("a)")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
npv

#1.2. Bcr plot without long x values####

bcrx<-mc_result[,c(150:153)]
bcrx_data_frame <- data.frame(benefitcostratio= c(bcrx$Benefit_Cost_Ratio_Intervention1,                    
                                                  bcrx$Benefit_Cost_Ratio_Intervention2,
                                                  bcrx$Benefit_Cost_Ratio_Intervention3,
                                                  bcrx$Benefit_Cost_Ratio_Intervention4),
                              Intervention=c(rep("1)",10000),rep("2)",10000),
                                             rep("3)",10000),rep("4)",10000)))

bcrxplot <- ggplot(data=bcrx_data_frame, aes(x=Intervention, y=benefitcostratio, fill=Intervention)) + geom_boxplot()

bcrxplotsave<-bcrxplot + scale_fill_discrete(name="",
                                        breaks=c("1)", "2)", "3)", "4)"),
              labels=c("1) Weather station-SMS-Gender", "2) SMS-Gender", "3) SMS-Loudspeaker","4) Paper-Loudspeaker"))+
  theme_bw() +theme(legend.position = c(.3, .8))+
  ylab("Benefit cost ratio")+
  ggtitle("b)")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

bcrx
ggsave("bcrxplotsave.jpeg", dpi = 500, width = 7, height = 7) 


#1.3 Plot for both npv and bcr####
combinenpvbcrx<- plot_grid(npv,bcrxplotsave)

ggsave("6.combinenpvbcrx.jpeg", dpi = 500, width = 9,
       height = 5)

# 2. PLOTING VIP and EVPI####

# Calculate EVPI####
#Call the Monte Carlo simulation results from the input table
mc_acis<-mc_result[,c(2:143,144:149)]

#Running the EVPI can take few hours
#results_all <- multi_EVPI(mc_acis,"NPV_Intervention1",write_table = TRUE)

#plot(results_all, "output_1")
#plot(results_all, "output_2")
#plot(results_all, "output_3")
#plot(results_all, "output_4")

# Create data frame for VIP and EVPI
#2.1. PLOTTING VIP####
#2.1.1. PLOTTING VIP1####
# Reading the VIP scores for Intervention 1
vip1<-read.csv("MCResults/NPV_Intervention1_pls_results.csv",header = TRUE,sep=",")

# Reading EVPI score for Intervention 1
evpi1<-read.csv("EVPI_table_NPV_Intervention2.csv")
evpi1_omit<-na.omit(evpi1)
# Merge data of vip and evpi
vip_evpi1 <-merge(vip1, evpi1_omit)

# Creating Category for variables based on value of coefficient  
vip_evpi1$Category[vip_evpi1$Coefficient> 0] = "cadetblue"
vip_evpi1$Category[vip_evpi1$Coefficient< 0] = "firebrick"
vip_evpi1_threshold <- filter(vip_evpi1, VIP>=1)

# Creating plot with grid and border

vip1plot<-ggplot(vip_evpi1_threshold,aes(x=VIP,y=reorder(Label,VIP)))+
  geom_bar(aes(fill=Category),stat ="identity")+ 
  xlim(0,6)+
  xlab("VIP:Weather station-SMS-Gender")+
  ylab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.title.x =element_text(color="black", size=8), 
        axis.ticks.x = element_blank())+
  geom_vline(xintercept = 1.0, size=0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))

vip1plot
#2.1.2. PLOTTING VIP2####
# Reading the VIP scores for Intervention 2
vip2<-read.csv("MCResults/NPV_Intervention2_pls_results.csv",header = TRUE,sep=",")

# Reading EVPI score for Intervention 2
evpi2<-read.csv("EVPI_table_NPV_Intervention2.csv")
# Remove NA value
evpi2_omit<-na.omit(evpi2)

# Merge data of vip and evpi
vip_evpi2 <-merge(vip2, evpi2_omit)

# Creating Category for variables based on value of coefficient  
vip_evpi2$Category[vip_evpi2$Coefficient> 0] = "cadetblue"
vip_evpi2$Category[vip_evpi2$Coefficient< 0] = "firebrick"
vip_evpi2_threshold <- filter(vip_evpi2, VIP>=1.0)
# Creating plot with grid and border
  
  vip2plot<-ggplot(vip_evpi2_threshold,aes(x=VIP,y=reorder(Label,VIP)))+
    geom_bar(aes(fill=Category),stat ="identity")+ 
    xlab("VIP: SMS-Gender")+
    xlim(0,6)+
    ylab(NULL)+
    scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
    theme(axis.title.x =element_text(color="black", size=10), 
          axis.ticks.x = element_blank())+
          geom_vline(xintercept = 1.0, size=0.2)+
          theme_bw()+
          theme(legend.position = "none")+
          theme(plot.title = element_text(hjust = 0.5))
    vip2plot
     
#2.1.3. PLOTTING VIP3####
    # Reading the VIP scores for Intervention 3
    vip3<-read.csv("MCResults/NPV_Intervention3_pls_results.csv",header = TRUE,sep=",")
    
    # Reading EVPI score for Intervention 3
    evpi3<-read.csv("EVPI_table_NPV_Intervention3.csv")
    # Remove NA value
    evpi3_omit<-na.omit(evpi3)
    
    # Merge data of vip and evpi
    vip_evpi3 <-merge(vip3, evpi3_omit)
    
    # Creating Category for variables based on value of coefficient  
    vip_evpi3$Category[vip_evpi3$Coefficient> 0] = "cadetblue"
    vip_evpi3$Category[vip_evpi3$Coefficient< 0] = "firebrick"
    vip_evpi3_threshold <- filter(vip_evpi3, VIP>=1.0)
    # Creating plot with grid and border
    
  vip3plot<-ggplot(vip_evpi3_threshold,aes(x=VIP,y=reorder(Label,VIP)))+
      geom_bar(aes(fill=Category),stat ="identity")+ 
      xlab("VIP: SMS-Loudspeaker")+
      xlim(0,6)+
      ylab(NULL)+
      scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
      theme(axis.title.x =element_text(color="black", size=10), 
            axis.ticks.x = element_blank())+
      geom_vline(xintercept = 1.0, size=0.2)+
      theme_bw()+
      theme(legend.position = "none")+
      theme(plot.title = element_text(hjust = 0.5))
  vip3plot
#2.1.4. PLOTTING VIP4####
    # Reading the VIP scores for Intervention 4
    vip4<-read.csv("MCResults/NPV_Intervention4_pls_results.csv",header = TRUE,sep=",")
    
    # Reading EVPI score for Intervention 4
    evpi4<-read.csv("EVPI_table_NPV_Intervention4.csv")
    # Remove NA value
    evpi4_omit<-na.omit(evpi4)
    
    # Merge data of vip and evpi
    vip_evpi4 <-merge(vip4, evpi4_omit)
    
    # Creating Category for variables based on value of coefficient  
    vip_evpi4$Category[vip_evpi4$Coefficient> 0] = "cadetblue"
    vip_evpi4$Category[vip_evpi4$Coefficient< 0] = "firebrick"
    vip_evpi4_threshold <- filter(vip_evpi4, VIP>=1.0)
    # Creating plot with grid and border
    
    vip4plot<-ggplot(vip_evpi4_threshold,aes(x=VIP,y=reorder(Label,VIP)))+
      geom_bar(aes(fill=Category),stat ="identity")+ 
      xlab("VIP: Paper-Loudspeaker")+
      xlim(0,6)+
      ylab(NULL)+
      scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
      theme(axis.text.x =element_text(color="black", size=12), 
            axis.ticks.x = element_blank())+
      geom_vline(xintercept = 1.0, size=0.2)+
      theme_bw()+
      theme(legend.position = "none")+
      theme(plot.title = element_text(hjust = 0.5))
     vip4plot   
# Combine VIP plots####
 
combinevip <- plot_grid(vip1plot, vip2plot, vip3plot, vip4plot,
                       labels = c('a)', 'b)', 'c)', 'd)'),
                       scale=0.95,
                       hjust=-0.2, vjust = 1) 

ggsave("7.combinevip.jpeg", dpi = 500, 
        width = 9,
        height = 8)

min(vip_evpi1_threshold$VIP)
max(vip_evpi1_threshold$VIP)
min(vip_evpi2_threshold$VIP)
max(vip_evpi2_threshold$VIP)
min(vip_evpi3_threshold$VIP)
max(vip_evpi3_threshold$VIP)
min(vip_evpi4_threshold$VIP)
max(vip_evpi4_threshold$VIP)



# 2.2 Plotting EVPI####


# 2.2.1 EVPI 1#### 
  
  # Creating plot with grid and border
  
  evpiplot1<-ggplot(vip_evpi1_threshold,aes(x=EVPI_do,y=reorder(Label,VIP)))+
    geom_bar(aes(fill=Category),stat ="identity")+ 
    xlab("1) Weather station-SMS-Gender")+
    ylab(NULL)+
    scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
    theme(axis.title.x =element_text(color="black", size=12), 
          axis.ticks.x = element_blank())+
    geom_vline(xintercept = 0, size=0.2)+
    theme_bw()+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5)) 
  evpiplot1
    # 2.2.2 EVPI 2#### 
    
    # Creating plot with grid and border
    
   evpiplot2<- ggplot(vip_evpi2_threshold,aes(x=EVPI_do,y=reorder(Label,VIP)))+
      geom_bar(aes(fill=Category),stat ="identity")+ 
      xlab("2) SMS-Gender")+
      ylab(NULL)+
      scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
      theme(axis.title.x =element_text(color="black", size=12), 
            axis.ticks.x = element_blank())+
      geom_vline(xintercept = 0, size=0.2)+
      theme_bw()+
      theme(legend.position = "none")+
      theme(plot.title = element_text(hjust = 0.5))     
  evpiplot2
    # 2.2.3 EVPI 3#### 
    
    # Creating plot with grid and border
    evpiplot3<- ggplot(vip_evpi3_threshold,aes(x=EVPI_do,y=reorder(Label,VIP)))+
      geom_bar(aes(fill=Category),stat ="identity")+ 
      xlab("3) SMS-Loudspeaker")+
      ylab(NULL)+
      scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
      theme(axis.title.x =element_text(color="black", size=12), 
            axis.ticks.x = element_blank())+
      geom_vline(xintercept = 0, size=0.2)+
      theme_bw()+
      theme(legend.position = "none")+
      theme(plot.title = element_text(hjust = 0.5)) 
  evpiplot3
    # 2.2.4 EVPI 4#### 
    
    # Creating plot with grid and border
    
    evpiplot4<-ggplot(vip_evpi4_threshold,aes(x=EVPI_do,y=reorder(Label,VIP)))+
      geom_bar(aes(fill=Category),stat ="identity")+ 
      xlab("4) Paper-Loudspeaker")+
      ylab(NULL)+
      scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
      theme(axis.title.x =element_text(color="red", size=12), 
            axis.ticks.x = element_blank())+
      geom_vline(xintercept = 0, size=0.2)+
      theme_bw()+
      theme(legend.position = "none")+
      theme(plot.title = element_text(hjust = 0.5)) 
evpiplot4

#3.1 Plotting annual costs####
 #3.1. 1 Plotting annual costs box plot####  
  # Create data frame (not just vector) for plot. The data frame contains
  # values of different (groups) for ploting 
  #The data frame might need to be grouped to be able to use ggplot 
 annual_costs<-mc_result[,c(154:173)]/1000
  annual_costs_data_frame <- data.frame(Annual_costs= c(
  annual_costs$Intervention1_total_costs1,                    
  annual_costs$Intervention1_total_costs2,annual_costs$Intervention1_total_costs3,
  annual_costs$Intervention1_total_costs4,annual_costs$Intervention1_total_costs5,
  annual_costs$Intervention2_total_costs1,annual_costs$Intervention2_total_costs2,
  annual_costs$Intervention2_total_costs3,annual_costs$Intervention2_total_costs4, 
  annual_costs$Intervention2_total_costs5,
  annual_costs$Intervention3_total_costs1,annual_costs$Intervention3_total_costs2,
  annual_costs$Intervention3_total_costs3,annual_costs$Intervention3_total_costs4,
  annual_costs$Intervention3_total_costs5,
  annual_costs$Intervention4_total_costs1,annual_costs$Intervention4_total_costs2,
  annual_costs$Intervention4_total_costs3,annual_costs$Intervention4_total_costs4, 
  annual_costs$Intervention4_total_costs5),
  Year = c(rep("1", 10000), rep("2", 10000),rep("3", 10000),rep("4", 10000),rep("5", 10000),
  rep("1", 10000), rep("2", 10000),rep("3", 10000),rep("4", 10000),rep("5", 10000),
  rep("1", 10000), rep("2", 10000),rep("3", 10000),rep("4", 10000),rep("5", 10000),
  rep("1", 10000), rep("2", 10000),rep("3", 10000),rep("4", 10000),rep("5", 10000)),
  Intervention=c(rep("1) Weather station-SMS-Gender",50000),rep("2) SMS-Gender",50000),
                                             rep("3) SMS-Loudspeaker",50000),
                                             rep("4) Paper-Loudspeaker",50000)))
 
 #view(annual_costs_data_frame)
 #Point geom####
 # ggplot(data = annual_costs_data_frame, aes(x=Year, y= Annual_costs, 
 #                  color=Intervention))+ geom_point()
 # box_plot geom####   
  annualcostplot<-ggplot(data = annual_costs_data_frame, aes(x=Year, y= Annual_costs, 
    color=Intervention,))+ 
      geom_boxplot()+theme_bw()+
    ylab("Annual costs (thousand USD)")+
    theme(legend.position = c(.9, .9),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6))+
    theme(legend.title = element_blank())
  
  ggsave("8.annualcostplot.jpeg", dpi=500, width = 5, height = 4)
  
  

  
  
  
  

         










