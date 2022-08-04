***META ANALYSIS WITH IPD DATA***
  I first should filter data to pre_alpha and alpha, pre_alpha and delta, pre_alpha and omicron etc. Run seperate analyses on each set then(?) Need to ask e about this. 
I first should filter data to pre_alpha and alpha, pre_alpha and delta, pre_alpha and omicron etc. Run separate analyses on each set then(?) Need to ask e about this. 

*METAFOR PACKAGE USING A FUNCTION*
  ```{r}
# outcomes: heamorrhage, diab_prepreg, preterm_labor, pre_or_eclampsia, eclampsia, covid_icu
@@ -313,6 +314,72 @@ meta_fun <- function(outcome.var, strain.var, strain.var2) {
  ```
  
  NOTE: use above code from metafor or this code from metagen only.
  *METAGEN PACKAGE WITHOUT A FUNCTION*
    ```{r}
  df14.negpos.outcome <- df14 %>% mutate(neg = tot_n - events)
  df14.alpha <- df14.negpos.outcome %>% 
    dplyr::filter(dominant_variant %in% c("Pre_alpha", "Alpha")) %>%
    group_by(site_name, dominant_variant, outcome) %>%
    summarise(pos = sum(events), 
              neg = sum(neg)) %>%
    mutate(dominant_variant = if_else(dominant_variant=="Pre_alpha",
                                      "Pre_alpha",
                                      "treatment_strain")) %>%
    pivot_wider(names_from = dominant_variant, values_from = c("pos", "neg")) %>%
    drop_na() # drops countries that don't have one or the other dominant strain.
  
##re-naming the strains
## including the signs +/- next to strain names brought errors in 'mutate log_rr, log SE' part

colnames(df14.alpha)[which(names(df14.alpha) == "pos_treatment_strain")] <- "pos_Alpha"
colnames(df14.alpha)[which(names(df14.alpha) == "neg_treatment_strain")] <- "neg_Alpha"
colnames(df14.alpha)[which(names(df14.alpha) == "pos_Pre_alpha")] <- "pos_Prealpha"
colnames(df14.alpha)[which(names(df14.alpha) == "neg_Pre_alpha")] <- "neg_Prealpha"
colnames(df14.alpha)[which(names(df14.alpha) == "outcome")] <- "Outcome"
  
  ### used this link to calc: https://www.medcalc.org/manual/relative-risk-odds-ratio.php
  ### function for calcuating rr
  rr_ff <- function(pos1, pos2, neg1, neg2, adj=0.5){
    useadj <- pos1==0 | neg1==0 | pos2==0 | neg2==0
    
    a <- pos1+useadj*adj
    b <- neg1+useadj*adj
    c <- pos2+useadj*adj
    d <- neg2+useadj*adj
    return((a/(a+b))/((c/(c+d))))
  }
  ### this calculates se of logRR not just RR.
  se_logrr_ff <- function(pos1, pos2, neg1, neg2, adj=0.5){
    useadj <- pos1==0 | neg1==0 | pos2==0 | neg2==0
    a <- pos1+useadj*adj
    b <- neg1+useadj*adj
    c <- pos2+useadj*adj
    d <- neg2+useadj*adj
    return(sqrt((1/a)+(1/c)-(1/(a+b))-(1/(c+d))))
  }
  
  df14.alpha <- df14.alpha %>%
    mutate(logRR = log(rr_ff(pos_Prealpha, pos_Alpha, neg_Prealpha, neg_Alpha)),
           logSE = se_logrr_ff(pos_Prealpha, pos_Alpha, neg_Prealpha, neg_Alpha))
  
  # Can we now use "dat" file that has the logRR and generate RR and SEs that we can feed into to metagen package. 
  # to="all" says add 1/2 to all the 2X2 cells (when events =0 for a set)
  ### metagen 
  
  df14.alpha
  mymeta <-metagen(logRR, logSE,
                   studlab=paste(site_name), data=df14.alpha,
                   fixed= FALSE, random=TRUE,
                   subset=NULL, sm="RR",
                   method.tau="REML",
                   subgroup =df14.alpha$Outcome,
                   title="Variant")
  # summary (mymeta)
  #generate forest plot:HERE
  pdf(file='plots/VariantStudy_MetaForestPlot_alpha-prealpha.pdf', width=13, height=22) # pdf saving has to go before making the plot and at the end have to say dev.off()
  

  forest(mymeta, sortvar=logRR, comb.random, leftcols = c("studlab", "pos_Prealpha", "pos_Alpha", "neg_Prealpha", "neg_Alpha"), 
         colgap.forest.left = "5mm" , fontsize = 12,
         fs.hetstat = 9, header=TRUE, overall=FALSE, col.diamond = "#33CC66", col.study= "#333333", 
         , col.by = "gray0")
  # grid.text("Variant study by outcome", .5, .89, gp=gpar(cex=1)) # only works in R version 3.
  dev.off()
  ```
  
