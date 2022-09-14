This is from Carolyn: lbw <- read.csv ("C:/Users/carol/OneDrive - The George Washington University/Documents/Maternal Nutrition 2020-2021 work/Data analysis/B12 Meta Analysis/Human data analysis/CSVs/MM4MN_humananalysis_LBW.csv")
lbw <-metagen(logOR, logSE,
              studlab=paste(ID), data=lbw,
              fixed= FALSE, random=TRUE,
              subset=NULL, sm="OR",
              method.tau="REML",
              subgroup =lbw$Trimester,
              title="LBW2")
summary (lbw)
#generate forest plot:HERE
forest(lbw, sortvar=logOR, comb.random, leftcols = "studlab", colgap.forest.left = "30mm" ,
       fs.hetstat = 9, header=TRUE, overall=FALSE, col.diamond = "#33CC66", col.study= "#333333" )
grid.text("Low birthweight by Trimester", .5, .89, gp=gpar(cex=1))