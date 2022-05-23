## Script for running cluster analyses ON FOCAL MUM LOCAL DENSITY/HABITAT DATA BEHAVIOUR ##
##  THIS ONE RUNS MCA ON RAW DATA (I.E. NOT SUMMARISED BY FOCAL NUMBER)

# NAME: Sean Twiss 
# DATE: 3/3/21

################################################################################

##############################################################################
rm(list=ls())

objects()
search()

## load packages ##

library(lme4)   # need to run GLMM
library(MASS) 	# general package
library(car)  	# genearl package
#library(MuMIn) 	# need to dredge through full model

library(dplyr)
library(psych)
library(FactoMineR)
library("factoextra")
library("corrplot")
library(ggplot2)
library(GGally) # for ggpairs
library(lubridate)
library(pracma)  # for Mode fnc

# SET WORKING DIR - MAY NEED TO CHANGE FOR DIFF YEARS / datasets etc
setwd("C:\\Users\\smann\\OneDrive\\Documents\\R\\Seal project\\Final Analysis")


# READ DATA FILEs ====================================================
# NB alter file name and year as NEEDED
DF2 <- read.csv(file = "MERGED-DATASET.csv", header = TRUE, 
                   na.strings=c("","NA"))



##############################################################################
## NOW FOR MFA (OR PCA)



 names(DF2)
 #http://factominer.free.fr/factomethods/multiple-factor-analysis.html
 #http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/


# LETS FIRST SUBSET THE VARIABLES WE WANT IN THE ANALYSIS
names(DF2)

DF3 <- select(DF2, "ID", "INDEX", "FOCAL", "NFEM.2M","NFEM.5M",
                 "SUN.YN", "DRY.YN","INPOOL.YN","TOPO.FLAT.YN","TOPO.MOD.YN", "TOPO.ROUGH.YN", 
                 "SUBST.GRASS.YN", "SUBST.MUD.YN",  "SUBST.ROCK.YN")
DF3 <- arrange(DF3, DF3$INDEX)
############################################################
###########################################################
# HCA: hierarchical clustering - see ?dist for alternate methods ofr generating distance matrix
# distance matrix
head(DF3[4:14])# select just the binary/test variables
summary(DF3)
DF3.HCA <- DF3[4:14]

d <- dist(DF3.HCA, method = 'euclidean') # create 'distance matrix'

# do HCA - see?hclust for alt methods etc
HC1 <- hclust(d, method = 'ward.D')  

# plot the dendrogram
par(mfrow = c(1,1))
plot(HC1)

# cut tree into clusters - pick the number
clusters <- cutree(HC1, k = 3)
# redraw with boxes around each cluster
rect.hclust(HC1, k = 3, border = 'red')

# use table to cf clusters with a priori identifier e.g.  ID
table(DF3$ID, clusters)  # NB - using 2 diff dataframes here - need to be sure
# that order and no. records is correct and working


#######################################
#######################################
## K-MEANS clustering

# the number = the requested no. of groups
KM1 <- kmeans(DF3[5:13], 3)

# request cluster means  FOR EACH VAR OF INTEREST
aggregate(DF3[,5:13], by = list(KM1$cluster), FUN =  mean)


# append cluster assignment to original DF I.E. ASSIGNS A CLUSTER BACK TO ORIGINAL DF FOR EACH SEAL/TEST
DF3 <- data.frame(DF3, KM1$cluster)

# This works tho'' - could do some kind of confusion matrix a la Courtney???
table(DF3$ID, KM1$cluster) #BASICALLLY SIMILAR TO HCLUST RESULT



###############################################
###############################################
#
# MCA MCA MCA MCA MCA MCA MCA
#
################################################
#MCA with  MCA() (FactoMineR)
# number of categories per variable
DF3.MCA <- DF3[4:14] # need ENSURE all 1/0  integers ARE CONVERTED to factors with 2 levels (1/0)
str(DF3.MCA)
DF3.MCA[] <- lapply(DF3.MCA, factor)
str(DF3.MCA)
names(DF3.MCA)

topo.sub.MCA <- DF3[9:14] #TOPOGRAPHY AND SUBSTRATE MCA ONLY
topo.sub.MCA[] <- lapply(topo.sub.MCA, factor)
names(DF3[9:14])

# check
cats = apply(DF3.MCA, 2, function(x) nlevels(as.factor(x)))
cats2 = apply(topo.sub.MCA, 2, function(x) nlevels(as.factor(x)))
cats # tells us the number of categories in each variable
cats2
# apply MCA
mca1 = MCA(DF3.MCA, graph = FALSE)

mca2 = MCA(topo.sub.MCA, graph = FALSE)

# list of results
mca1
mca2

# table of eigenvalues
mca1$eig 
mca2$eig


# data frame with variable coordinates
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))
mca2_vars_df = data.frame(mca2$var$coord, Variable = rep(names(cats2), cats2))

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#data frame with observation coordinates (i.e. gives coordinates across PC axes for each original record
# can use these - rejoin to original file - and calc a mean PC1/PC2 for each focal - or just use as a continuous varibales???)
# IF the mca has done something senisble that is!
mca1_obs_df = data.frame(mca1$ind$coord)
mca2_obs_df = data.frame(mca2$ind$coord)
# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
   geom_hline(yintercept = 0, colour = "gray70") +
   geom_vline(xintercept = 0, colour = "gray70") +
   geom_text(aes(colour=Variable)) +
   ggtitle("MCA plot of variables using R package FactoMineR")

ggplot(data=mca2_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca2_vars_df))) +
   geom_hline(yintercept = 0, colour = "gray70") +
   geom_vline(xintercept = 0, colour = "gray70") +
   geom_text(aes(colour=Variable)) +
   ggtitle("MCA plot of variables using R package FactoMineR")

# MCA plot of observations and categories


#MCA
library(PCAmixdata)
# https://cran.r-project.org/web/packages/PCAmixdata/vignettes/PCAmixdata.html
par(mfrow = c(1,1))
mca.new <- PCAmix(X.quali=topo.sub.MCA[,1:6],rename.level=TRUE,graph=FALSE)
mca.new
plot(mca.new)

?plot.PCAmix
par(mfrow=c(2,2))
plot(mca.new,choice="ind",coloring.ind=topo.sub.MCA$TOPO.ROUGH.YN,label=FALSE,
     posleg="bottomright", main="Observations")
plot(mca.new,choice="levels", main="Levels")
plot(mca.new,choice="sqload",coloring.var=T, leg=TRUE,
     posleg="topright", main="All variables")

dev.off()
# varimax orthogonal rotation
rot<-PCArot(mca.new,2)  # 2 mean useonly 1st 2 axes
plot(rot,choice="ind",main="Scores after rotation")
plot(rot, choice="sqload", main="Correlation ratios after rotation")
plot(rot, choice="levels", main="Levels after rotation")

rot
rot$eig # % variance by PC axes
rot$quali # loading of variables on each axis
rot$ind  # values for each row in data - what we need as our continous measure of habitat 'location'

head(mca2_obs_df[,1:2])
x <- as.data.frame(rot$ind)

#rename the new vaiabkles if you wish


x <- as.data.frame(rot$levels)
colnames(x) <- (c("ROUGHNESS", "GRASSINESS"))
x$points <- row.names(x)
x$points <- replace(x$points, x$points=="TOPO.FLAT.YN=0", "NOT FLAT") 
x$points <- replace(x$points, x$points=="TOPO.FLAT.YN=1", "FLAT")
x$points <- replace(x$points, x$points=="TOPO.MOD.YN=0", "NOT MODERATE")
x$points <- replace(x$points, x$points=="TOPO.MOD.YN=1", "MODERATE")
x$points <- replace(x$points, x$points=="TOPO.ROUGH.YN=0", "NOT ROUGH")
x$points <- replace(x$points, x$points=="TOPO.ROUGH.YN=1", "ROUGH")
x$points <- replace(x$points, x$points=="SUBST.GRASS.YN=0", "NOT GRASS")
x$points <- replace(x$points, x$points=="SUBST.GRASS.YN=1", "GRASS")
x$points <- replace(x$points, x$points=="SUBST.MUD.YN=0", "NOT MUD")
x$points <- replace(x$points, x$points=="SUBST.MUD.YN=1", "MUD")
x$points <- replace(x$points, x$points=="SUBST.ROCK.YN=0", "NOT ROCK")
x$points <- replace(x$points, x$points=="SUBST.ROCK.YN=1", "ROCK")

# if the original input file DF2/DF3is in same order you should just be able to merge these now 
dev.off()
library(ggrepel)
p2 <- ggplot(x,aes(x = ROUGHNESS, y=GRASSINESS)) + 
   labs(x="Roughness (33.55%)", y = "Grassiness (31.22%)")+
   theme_classic() +
   geom_point(size =2) + 
   geom_hline(yintercept=0, linetype="dashed") + geom_vline(xintercept=0, linetype="dashed")+
   theme_classic() +
   geom_label_repel(aes(label=points,size = 8), nudge_y = 0.1) + 
   theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p2

p2 + scale_y_continuous(breaks = seq(0.0, 1.7, by=0.5), limits=c(0.0,1.7))
p2 + scale_x_continuous(breaks = seq(20, 90, by=10), limits=c(20,95))

p2 +theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10,face="bold"))


DF3 <- merge(DF3, x, by="row.names", all=TRUE)  # merge by row names (by=0 or by="row.names")
str(DF3)

boxplot(DF3$GRASSINESS ~DF3$ID)
boxplot(DF3$ROUGHNESS ~DF3$ID)

DF3 <- arrange(DF3, INDEX)

outfile.name <- paste0("MSA-ROT-DATASET.csv")
write.table(DF3, file = outfile.name, col.names=T, row.names=F, sep = ",", append = FALSE)


##probably best to compute a mean per focal for these new measures?? for use in models
##see other new code

# exploring mca output
## Eigenvalues / Variances
# The proportion of variances retained by the different dimensions (axes) can be extracted using the function get_eigenvalue() [factoextra package] as follow:
#library("factoextra")
eig.val <- get_eigenvalue(mca)
head(eig.val)

eig.val2 <- get_eigenvalue(mca2)
head(eig.val2)

# To visualize the percentages of inertia explained by each MCA dimensions, 
# use the function fviz_eig() or fviz_screeplot() [factoextra package]:

fviz_screeplot(mca1, addlabels = TRUE, ylim = c(0, 45))


# Biplot
# The function fviz_mca_biplot() [factoextra package] is used to draw the biplot 
#of individuals and variable categories:

fviz_mca_biplot(mca1, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

fviz_mca_biplot(mca2, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())


##Graph of variables
#  Results
#  The function get_mca_var() [in factoextra] is used to extract the results for variable categories. This function returns a list containing the coordinates, the cos2 and the contribution of variable categories:

var <- get_mca_var(mca1)
var
#The different components can be accessed as follow:

# Coordinates
head(var$coord)
#  Coordinates of variable categories
#  The R code below displays the coordinates of each variable categories in each dimension (1, 2 and 3):
head(round(var$coord, 2), 4)

# Cos2: quality on the factor map
#  Quality of representation of variable categories
#  The quality of the representation is called the squared cosine (cos2), which measures 
# the degree of association between variable categories and a particular axis. 
# The cos2 of variable categories can be extracted as follow:
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

## !!!!!!!!!!! probably more useful way to see whats happening - these next two
#Correlation between variables and principal dimensions
#To visualize the correlation between variables and MCA principal dimensions, type this:
## !!!! this is really useful for reducing the number of params - and nicely seems to show 2 axes with
# dim1 = aggression, dim2 = chilled!!
fviz_mca_var(mca1, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(mca2, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


#Use the function fviz_mca_var() [in factoextra] to visualize only variable categories:

fviz_mca_var(mca1, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(mca2, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


#    It's possible to color variable categories by their cos2 values using the argument col.var = "cos2". This produces a gradient colors, which can be customized using the argument gradient.cols. For instance, gradient.cols = c("white", "blue", "red") means that:

#      variable categories with low cos2 values will be colored in "white"
#    variable categories with mid cos2 values will be colored in "blue"
#    variable categories with high cos2 values will be colored in "red"
# Color by cos2 values: quality on the factor map
fviz_mca_var(mca1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal()) 

fviz_mca_var(mca2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal()) 

# Change the transparency by cos2 values (basically same as above without colors)
fviz_mca_var(mca1, alpha.var="cos2",
             repel = TRUE,
             ggtheme = theme_minimal())



#   You can visualize the cos2 of row categories on all the dimensions using the corrplot package:
par(mfrow = c(1,1))
#!!!!!!!!!!!!! useful
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
#    It's also possible to create a bar plot of variable cos2 using the function fviz_cos2()[in factoextra]:

# Cos2 of variable categories on Dim.1 and Dim.2
fviz_cos2(mca1, choice = "var", axes = 1:2)
fviz_cos2(mca1, choice = "var", axes = 1)


#  Contribution of variable categories to the dimensions
#  The contribution of the variable categories (in %) to the definition of the dimensions 
#  can be extracted as follow:

head(round(var$contrib,2), 4)
# Contributions of rows to dimension 1
fviz_contrib(mca1, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(mca1, choice = "var", axes = 2, top = 15)

# Total contribution to dimension 1 and 2
fviz_contrib(mca1, choice = "var", axes = 1:2, top = 15)

#   The most important (or, contributing) variable categories can be highlighted on the scatter plot as follow:
# as above but uses contrib (%) rather than cos2
fviz_mca_var(mca1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)



#  Graph of individuals (IN THIS CASE THESE WILL BE ROWS OF DATA - I.E. NOT 'SEAL INDIVIDUALS)
#
#  The function get_mca_ind() [in factoextra] is used to extract the results for individuals. This function returns a list containing the coordinates, the cos2 and the contributions of individuals:

ind <- get_mca_ind(mca1)
ind

# Plots: quality and contribution
# The function fviz_mca_ind() [in factoextra] is used to visualize only individuals. Like variable categories, it's also possible to color individuals by their cos2 values:

fviz_mca_ind(mca1, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())


#   Color individuals by groups            
fviz_mca_ind(mca1, 
             label = "none", # hide individual labels
             habillage = "TOPO.FLAT.YN", # color by groups 
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 



fviz_ellipses(mca1, c("TOPO.FLAT.YN", "SUBST.GRASS.YN"),
              geom = "point")


#SEE http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
# FOR MORE INFO AND MORE OPTIONS

