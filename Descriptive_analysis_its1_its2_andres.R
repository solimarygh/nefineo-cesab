#Title: 
#Created 19/6/2025 by Micaela Santos
#Description:

# 1. Open NEFINEO_MS project #####
setwd("~/nefineo-cesab")

# 2. Open raw datasets (sites and new.ID) ####
# 2.1 final dataset its1 
its1= read.csv("Data/new.its1.csv", row.names=1)
#filtering by papers to keep and bioregion
its1.f= subset(its1, paper_to_keep=="yes" & 
                 morrone_biogeoregions_Region== "Neotropical") 
#Remember: samples were grouped by <90m of proximity and sampling year (new.ID)
#length(unique(its1.f$grouped_samples)) #466 grouped samples by proximity 
n.samples.its1= length(unique(its1.f$new.ID)) #470 grouped samples by proximity and sampling year

# 2.3 final dataset its2
its2= read.csv("Data/new.its2.csv", row.names= 1)
#filtering by papers to keep and bioregion
its2.f= subset(its2, paper_to_keep=="yes" & 
                 morrone_biogeoregions_Region== "Neotropical") 
#Remember: samples were grouped by <90m of proximity and sampling year (new.ID)
#length(unique(its2.f$grouped_samples)) #608 grouped samples by proximity
n.samples.its2= length(unique(its2.f$new.ID)) #624 grouped samples by proximity and sampling year

# 3. Open Andres´s datasets (OTUs) ####
# 3.1 Its1
its1.ecm= read.csv("Data/its1_andres.csv", sep=",", fileEncoding="latin1") #ok, same length of its1.f
#filtering by its1.f 
to.keep= match(its1.f$PermanentID, its1.ecm$PermanentID) 
its1.ecm.f= its1.ecm[to.keep,] #ok, same nº of obs its.f (1688)
unique(its1.ecm.f$morrone_biogeoregions_Region) #ok, check only neotropical

#3.2 its2
its2.ecm= read.csv("Data/its2_andres.csv", sep=",", fileEncoding="latin1") #ok, same length of its1.f
#filtering by its1.f
to.keep2= match(its2.f$PermanentID, its2.ecm$PermanentID) 
its2.ecm.f= its2.ecm[to.keep2,] #ok, same nº of obs its.f (1208) 
unique(its2.ecm.f$morrone_biogeoregions_Region) #ok, check only neotropical

# 3.3 Add new.Id col from its1.f/its2.f
its1.ecm.f= cbind(new.ID=its1.f$new.ID, its1.ecm.f)
its2.ecm.f= cbind(new.ID=its2.f$new.ID, its2.ecm.f)

# 4. Figures ####
# 4.1 Fig 1.a Histogram of ECM frequency (no grouped samples)
#jpeg(file="Figs/histograms_ecm.jpg",
#    width=800, height=400)
par(mfrow=c(1,2))

ecm.hist= its1.ecm.f$ectomycorrhizal #vector ecm to plot  
ecm.hist[which(ecm.hist>40)]=40 #ecm abundance >40 = 40 to plot
hist.ecm1= hist(ecm.hist, xaxt="n", main="Neotropical ectomycorrizal (its1)",
                xlab= "ECM reads per sample (no grouped)")
labels= hist.ecm1$breaks
labels[length(labels)]= ">40"
axis(1, at=hist.ecm1$breaks, labels=labels)
sum(its1.ecm.f$ectomycorrhizal) #5869 reads
text(35,300, labels="reads = 5869")

ecm2.hist= its2.ecm.f$ectomycorrhizal #vector ecm to plot  
ecm2.hist[which(ecm2.hist>40)]=40 #ecm abundance >40 =40 to plot
hist.ecm2= hist(ecm2.hist, xaxt="n", main="Neotropical ectomycorrizal (its2)",
                xlab= "ECM reads per sample (no grouped)", ylim = c(0, 1400))
labels2= hist.ecm2$breaks
labels2[length(labels2)]= ">40"
axis(1, at=hist.ecm2$breaks, labels=labels2)
sum(its2.ecm.f$ectomycorrhizal) #5187 reads
text(35,300, labels="reads = 5187")

#dev.off()

# 4.2 Fig 1.a Histogram of ECM frequency (grouped samples)
#jpeg(file="Figs/histograms_ecm_grouped.jpg",
#     width=800, height=400)
par(mfrow=c(1,2))

#its1 grouped samples
ecm1.g= aggregate(its1.ecm.f$ectomycorrhizal, by= list(its1.ecm.f$new.ID), FUN= "sum") 
#sum(ecm1.g$x) #5869
#max(ecm1.g$x) #the grouped sample with the highest abundance of ECM=535
#which(ecm1.g$x==max(ecm1.g$x)) #NEF_GloFung-its1_651_2017
#its1.f[its1.f$new.ID=="NEF_GloFung-its1_651_2017",c(5,7,10)] #paper_id= Smith_2017_2AFC

ecm1.g.hist= ecm1.g$x
ecm1.g.hist[which(ecm1.g.hist>150)]=150 #ecm abundance >250 =250 to plot
breaks= c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150)
hist.ecm1.g= hist(ecm1.g.hist, xaxt="n", main="Neotropical ectomycorrizal (its1)",
                xlab= "ECM reads per grouped samples (<90m of proximity and year)",
                breaks= breaks, ylim= c(0,550))
labels.g= hist.ecm1.g$breaks
labels.g[length(labels.g)]= ">150"
axis(1, at=hist.ecm1.g$breaks, labels=labels.g)
sum(its1.ecm.f$ectomycorrhizal) #5869 reads
text(140,120, labels="reads = 5869")
text(138,90, labels="samples = 470")

#its2 grouped samples
ecm2.g= aggregate(its2.ecm.f$ectomycorrhizal, by= list(its2.ecm.f$new.ID), FUN= "sum") 
#sum(ecm2.g$x) #5187
#max(ecm2.g$x) #the grouped sample with the highest abundance of ECM=549
#which(ecm2.g$x==max(ecm2.g$x)) #NEF_GloFung-its2_107_2022 
#its2.f[its2.f$new.ID=="NEF_GloFung-its2_107_2022",c(5,7,10)] #paper_id= BermudezContreras_2022_XA

ecm2.g.hist= ecm2.g$x
ecm2.g.hist[which(ecm2.g.hist>150)]=150 #ecm abundance >250 =250 to plot
hist.ecm2.g= hist(ecm2.g.hist, xaxt="n", main="Neotropical ectomycorrizal (its2)",
                  xlab= "ECM reads per grouped samples (<90m of proximity and year)",
                  ylim= c(0,550))
labels.g2= hist.ecm2.g$breaks
labels.g2[length(labels.g2)]= ">150"
axis(1, at=hist.ecm2.g$breaks, labels=labels.g2)
sum(its2.ecm.f$ectomycorrhizal) #5187 reads
text(140,120, labels="reads = 5187")
text(138,90, labels="samples = 624")

#dev.off()

# 4.2 Fig 1.b map

# 4.3 Fig 1.c 