library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
library(reshape)

setwd ("C:/Users/twlodarczyk/Desktop/All documents/PhD/CNRS + Synch/Field/Data/2022_05_06")
dt <- read.csv2("Saginaw_Hill_Tailing_Lab_analysis_05.06.2022_R.csv", header = T, sep = ",")

tr <- matrix(data = NA, ncol = ncol(dt[,c(1:14)]), nrow=nrow(dt))
colnames(tr) <- colnames(dt[,c(1:14)])
for (i in 4:14)
{
  tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
}

for(i in 1:3)
{
  tr[,c(i)] <- dt[,c(i)]
}
tr   

typeof(tr$Cu) # error, not for dt$Ni


# to check if variables are numeric or character
#sapply(tr, class)



#cols.num <- c("Cr","Mn", "Fe", "Co", "Ni", "Cu", "Zn", "As", "Se", "Rb", "Cd", "Re", "Hg", "TI", "Pb")
#cols.num <- c("Ni")
#tr[cols.num] <- sapply(tr[cols.num],as.numeric)
#sapply(tr, class)

#tr[,6:21] <- sapply(tr[,6:21],as.numeric)

typeof(tr$Ni) # shows error, because we have a vector, not a dataframe
is.atomic(tr) # not is true, as we have a vector
data_x <- as.data.frame(t(tr)) # convert data to dataframe
is.atomic(data_x) # now is true




tr <- as.data.frame.matrix(tr) #POPRAWNA KOMENDA zamiany vectora 
tr[,4:14] <- sapply(tr[,4:14],as.numeric) # zamien character na numeric

is.atomic(tr)
typeof(tr$Cu)



# melt data to have variable column with elements. File column is broken.

mtr <- melt(tr, id=c("Material", "Sample", "?..File"))

#"Mn_uncertainty", "Fe_uncertainty", "Co_uncertainty", "Ni_uncertainty", "Cu_uncertainty", "Zn_uncertainty"
mtr_plants <- subset(mtr, Material=="Plant")
mtr_plants2 <- subset(mtr_plants, variable!="Fe")
mtr_plants3 <- subset(mtr_plants2, variable!="Se")
mtr_plants4 <- subset(mtr_plants3, variable!="Cd")
mtr_plants5 <- subset(mtr_plants4, variable!="Tl") # this time it read TI as Tl (L)!!!!!!!
mtr_plants6 <- subset(mtr_plants5, variable!="Pb")
mtr_plants7 <- subset(mtr_plants6, variable!="Hg")

mtr_plants7

mtr_plants_Zn <- subset(mtr_plants, variable=="Zn")


new<-ggplot(data=mtr_plants7, aes(x=Sample, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  #geom_errorbar(aes(ymin=value-sd(value), ymax=value + sd(value)), position=position_dodge())+
  theme_bw()+
  labs(title="Elemental concentrations in plants")+
  geom_hline(yintercept = 300, color = "#D95F02")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(panel.grid.major.x = element_blank())+
  scale_fill_brewer(palette="Dark2")
new



selected<-ggplot(data=mtr_plants_Zn, aes(x=Sample, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  #geom_errorbar(aes(ymin=value-sd(value), ymax=value + sd(value)), position=position_dodge())+
  theme_bw()+
  labs(title="Elemental concentrations in plants")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(panel.grid.major.x = element_blank())+
  scale_fill_brewer(palette="Dark2")
selected


plot42<-ggplot(data=mtr_plants7, aes(x=Sample, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())+
  ylim(0,900) +theme_minimal()+labs(title="Elemental concentrations in plants")+
  geom_hline(yintercept = 300, color = "#D95F02")+
  scale_y_continuous(breaks=seq(0,900,100))+
  scale_fill_manual(values=c("Cu"="#D95F02", "Mn"="#1B9E77", "Zn"="#7570B3", "Re"="#66A61E", "As"="#E7298A"))
plot42
