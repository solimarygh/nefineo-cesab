#Title: Filtering_datasets
#Created 29/5/2025 
#Authors Solimary García & Micaela Santos
#Description:mmm

#Set working directoy ####
setwd("~/nefineo-cesab/Data") #github directory
list.files() #check files in folder
length(unique(its2$PermanentID)) #check ID by sample #2380 samples
length(unique(its1$PermanentID)) #check ID by sample #2815 samples

#Open dataset ####
#final dataset its1 
its1= read.table("globfungi_metadata_filtered_complemented_its1_soil.tsv", 
                 sep ="\t",)
#final dataset of articles to keep in its1 (no plantations)
its1.plant= read.csv("article_list_noplantation.its1.csv")
#final dataset its2
its2= read.table("globfungi_metadata_filtered_complemented_its2_soil.tsv", 
                 sep ="\t",)
#final dataset of articles to keep in its2 (no plantations)
its2.plant= read.csv("article_list_noplantation.its2.csv")

#New vector of articles to keep in datasets its1/its2 ####
#its1
keep.its1= subset(its1.plant, To_keep=="yes", select= title) #articles to keep
nrow(keep.its1) #20 articles to keep
v.keep.its1= match(its1$paper_id, keep.its1$title) #vector with articles to keep
v.keep.its1= ifelse(is.na(v.keep.its1), "no", "yes") #reemplace Na by "no"
#its2
keep.its2= subset(its2.plant, To_keep=="yes", select= title) #articles to keep
nrow(keep.its2) #28 articles to keep
v.keep.its2= match(its2$paper_id, keep.its2$title) #vector with articles to keep
v.keep.its2= ifelse(is.na(v.keep.its2), "no", "yes") #reemplace Na by "no"

#Add new vector to dataset ####
new.its1= cbind(its1[,1:5], To_keep=v.keep.its1, its1[,6:171])
new.its2= cbind(its2[,1:5], To_keep=v.keep.its2, its2[,6:171])

new.its1