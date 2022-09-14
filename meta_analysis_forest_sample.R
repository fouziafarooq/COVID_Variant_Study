#___________________________________________
#_________ Meta-analysis seroprevalence____
#___________________________________________

rm(list=ls()) 

library(meta)
library(tidyverse)
library(metafor)
library(readxl)
library(maps)


# data import 
dat0 <- read_excel("data/data_jenny_1Dec.xlsx", sheet = "Blood banks ")


# Analisis
##this was dat0$year_se before but I changed it to decade
dat0$before_after_2000 <- dat0$Decade
dat0$before_after_2000[dat0$Decade <2000 ] <- '< 2000'
dat0$before_after_2000[dat0$Decade >=2000 ] <- '> 2000'
dat0$label <- paste(dat0$firstauthor, dat0$year_p)
dat0$Country <- paste(dat0$country, dat0$year_se)


#  Conditions of my meta-analysis

dat <- filter(dat0, !is.na(total), included != 0, total > 500)
dat$new_pos <- dat$positive
dat$new_pos[is.na(dat$new_pos)] <- as.integer(dat$total[is.na(dat$new_pos)] * dat$prevalence[is.na(dat$new_pos)])
dat <- filter(dat, !is.na(new_pos))

dat2 <- paste(dat$firstauthor, dat$year_p)
write.csv(dat2, "sum.csv")

# ------------- Epsecify levels for plotting

dat$Decade <- factor(dat$Decade, levels = c(1970, 1980, 1990, 2000, 2010))
dat$Num_test <- factor(dat$Ntest , levels = c(1, 2, 3))
event.e <- dat$Country
event.c <- dat$Risk

## =============== My Forest_function

my_forest_function <- function(meta_file, max_prev = 20) {
  forest(meta_file, 
         xlim=c(-0.2,5),
         pscale=100,
         rightcols= FALSE,
         leftcols=c("studlab", "Country", "Risk", "event", "n", "effect", "ci"),
         leftlabs=c("Study","Country/Study Year", "Risk", "Cases", "Total", "Prevalence(%)", "95% C.I."),
         xlab="T. cruzi seroprevalence  (%)", smlab="", 
         weight.study="random", 
         squaresize=0.5, 
         col.square="navy",
         col.square.lines="navy",
         col.diamond="maroon", 
         col.diamond.lines="maroon",
         Rpooled.totals=FALSE,
         comb.fixed=FALSE,
         fs.hetstat=10,
         print.tau2=TRUE,
         print.Q=TRUE,
         print.pval.Q=TRUE,
         print.I2=TRUE,
         digits=2,
         test.subgroup.random = TRUE
         
         
  
  )
  
  
}


# General Meta-Analysis

meta_gen_decade <- metaprop(event   = new_pos, 
                     n         = total, 
                     studlab   = label,
                     sm        = 'PLOGIT', 
                     method.ci = "NAsm",
                     data      = dat,
                     byvar     = Decade)

saveRDS(meta_gen_decade, 'test/meta_gen_decade.RDS') # Save Meta-analysis file for later plotting


#endemic <- endemic %>% filter(Decade != 1970)

#meta-analysis by endemicity
endemic <- dat[dat$endemic_nonendemic == 'Endemic', ]
meta_endemic_decade <- metaprop(event     = new_pos, 
                         n         = total, 
                         studlab   = label,
                         sm        = "PLOGIT", 
                         method.ci = "NAsm" ,
                         data      = endemic,
                         byvar=Decade)


saveRDS(meta_endemic_decade, 'test/meta_endemic_decade.RDS')
trimfill(meta_endemic_decade) 

pdf("FinalPlots/en_endemic_by_decade.pdf", width = 15, height = 45)
my_forest_function(meta_endemic_decade)
dev.off()

res <- rma(yi=TE, sei=seTE, data=meta_endemic_decade)
regtest(res, model = "lm")

pdf("Funnel_plots/funnel_endemic.pdf")
taf <- trimfill(meta_endemic_decade)
funnel_endemic <- funnel (taf, legend=TRUE, main = "B. Endemic countries", comb.random = TRUE, level = 0.95, 
                          col.random = "red", xlim=c(-9,1)
                          )
dev.off()



nonendemic <- dat[dat$endemic_nonendemic == 'Non-endemic',]
meta_non_endemic_decade <- metaprop(event     = new_pos, 
                                n         = total, 
                                studlab   = label,
                                sm        = 'PLOGIT', 
                                method.ci = "NAsm" ,
                                data      = nonendemic,
                                byvar=Decade,
                                title = 'meta_non_endemic_decade')

saveRDS(meta_non_endemic_decade, 'test/meta_non_endemic_decade.RDS')

pdf("FinalPlots/en_non-endemic_by_decade.pdf", width = 15, height = 10)
my_forest_function(meta_non_endemic_decade, max_prev = 5)
dev.off()


pdf("Funnel_plots/funnel_non-endemic.pdf")
taf1 <- trimfill(meta_non_endemic_decade)
funnel_endemic <- funnel (taf1, legend=TRUE, main = "A. Non-endemic countries", 
                          comb.random = TRUE, level = 0.95, col.random = "red", 
                         xlim=c(-15,-5))
dev.off()


#--------------------By endemicity, by country, and by century 

dat$country_century <- NA
dat$country_century <- paste(dat$country, dat$before_after_2000)
levels_coutry_century <- sort(unique(dat$country_century))
dat$country_century <- factor(dat$country_century, levels = levels_coutry_century)


both <- filter(dat,  country == "Venezuela" | country == "Chile" | country == "Mexico"
              | country == "Colombia" | country == "Ecuador" | country == "El Salvador" 
               | country == "Nicaragua" | country == "Panama" | country == "Paraguay" | country == "Uruguay" | country == "Costa Rica")



meta_both <- metaprop(event     = new_pos, 
                                 n         = total, 
                                 studlab   = label,
                                 sm        = 'PLOGIT', 
                                 method.ci = "NAsm",
                                 data      = both,
                                 byvar     = country_century)

my_forest_function(meta_both, max_prev = 5)

pdf (file="FinalPlots/meta_both.pdf", width = 15, height = 50)
my_forest_function(meta_both, max_prev = 5)
dev.off()

pdf("FinalPlots/funnel_both.pdf")
taf <- trimfill(meta_both)
funnel_both <- funnel (taf, legend=TRUE, main = "Random-Effects Model", 
                       comb.random = TRUE, level = 0.95, col.random = "red")
dev.off()


#country-decade----------------------------------------------------------------------

##country with more than five studies by decade 


##European countries by decade 
levels_european_countries <- sort(unique(dat$country_century))
dat$country_century <- factor(dat$country_century, levels = levels_coutry_century)
european_countries <- filter(dat, country == "France" | country == "United Kingdom" | country == "Spain"
                             | country == "Switzerland" | country == "Sweden")

meta_european_countries <- metaprop(event     = new_pos, 
                                         n         = total, 
                                         studlab   = label,
                                         sm        = 'PLOGIT', 
                                         method.ci = "NAsm",
                                         data      = european_countries,
                                         byvar     = Decade)

saveRDS(meta_european_countries, 'test/meta_european_countries.RDS')

pdf (file="FinalPlots/meta_european_countries.pdf", width = 15, height = 10)
my_forest_function(meta_european_countries, max_prev = 5)
dev.off()

trimfill(meta_european_countries)
meta_european_countries$TE.random



pdf("Funnel_plots/funnel_europeancountries.pdf")
taf <- trimfill(meta_european_countries)
funnel_europeancountries <- funnel (taf, legend=TRUE, main = "C. Europe region", 
                                    comb.random = TRUE, level = 0.95,
                                    xlim=c(-10,0),
                                    col.random = "red")
dev.off()



##North American countries by decade 

levels_Northamerican <- sort(unique(dat$country_century))
dat$country_century <- factor(dat$country_century, levels = levels_coutry_century)


northamerican_countries <- filter(dat, country == "Canada" | country == "United States")
meta_northamerican_countries <- metaprop(event     = new_pos, 
                                    n         = total, 
                                    studlab   = label,
                                    sm        = 'PLOGIT', 
                                    method.ci = "NAsm",
                                    data      = northamerican_countries,
                                    byvar     = Decade)

saveRDS(meta_northamerican_countries, 'test/meta_northamerican_countries.RDS')

pdf (file="Funnel_plots/meta_northamerican_countries.pdf", width = 12, height = 10)
my_forest_function(meta_northamerican_countries, max_prev = 5)
dev.off()

meta_northamerican_countries$TE.random

pdf("Funnel_plots/funnel_northamerican.pdf")
taf <- trimfill(meta_northamerican_countries)
funnel_northamerican <- funnel (taf, legend=TRUE, main = "D. North America region", 
                                comb.random = TRUE, level = 0.95, col.random = "red",
                                xlim=c(-15,-4))
dev.off()



##Latin American countries by decade 

southamerican <- filter(dat, country == "Argentina" | country == "Brazil" | country == "Bolivia" | country =="Chile" 
                                  | country == "Colombia" 
                                  | country == "Guyana"
                                  | country == "Paraguay" | country == "Peru"
                                  | country == "Uruguay" | country == "Venezuela")
meta_southamerican <- metaprop(event     = new_pos, 
                                         n         = total, 
                                         studlab   = label,
                                         sm        = 'PLOGIT', 
                                         method.ci = "NAsm",
                                         data      = southamerican,
                                         byvar     = Decade)
saveRDS(meta_southamerican, 'test/meta_southamerican.RDS')

pdf (file="FinalPlots/meta_southamerican.pdf", width = 15, height = 60) 
my_forest_function(meta_southamerican, max_prev = 5)
dev.off()

pdf("Funnel_plots/funnel_southamerican.pdf")
taf <- trimfill(meta_southamerican)
funnel_southamerican <- funnel (taf, legend=TRUE, main = "F. South America region", 
                                comb.random = TRUE, level = 0.95, col.random = "red", 
                                xlim=c(-9,2))
dev.off()





#--------------------------

centralamerican_mexico <- filter(dat, country == "Costa Rica"| country == "El Salvador" | country == "Honduras" | country == "Guatemala"
                          | country == "Nicaragua" | country == "Panama" | country == "Mexico")

meta_centralamerican_mexico <- metaprop(event     = new_pos, 
                               n         = total, 
                               studlab   = label,
                               sm        = 'PLOGIT', 
                               method.ci = "NAsm",
                               data      = centralamerican_mexico,
                               byvar     = Decade)

saveRDS(meta_centralamerican_mexico, 'test/meta_centralamerican_mexico.RDS')

pdf (file="FinalPlots/meta_centralamerican_mexico.pdf", width = 20, height = 60) 
my_forest_function(meta_centralamerican_mexico, max_prev = 5)
dev.off()


pdf("Funnel_plots/funnel_meso.pdf")
taf <- trimfill(meta_centralamerican_mexico)
funnel_centralamerican_mexico <- funnel (taf, legend=TRUE, main = "E. Meso America region", 
                                         comb.random = TRUE, level = 0.95, 
                                         col.random = "red", xlim=c(-8,-1))
dev.off()



##number of reports ----------------------------------------------------------------------------

argentina <- filter(dat, country == "Argentina")
meta_argentina <- metaprop(event     = new_pos, 
                         n         = total, 
                         studlab   = label,
                         sm        = 'PLOGIT', 
                         method.ci = "NAsm",
                         data      = argentina,
                         byvar     = Decade)
saveRDS(meta_argentina, 'test/meta_argentina.RDS')

pdf (file="FinalPlots/meta_argentina.pdf", width = 20, height = 70)
my_forest_function(meta_argentina, max_prev = 5)
dev.off()

bolivia <- filter(dat, country == "Bolivia")
meta_bolivia <- metaprop(event     = new_pos, 
                           n         = total, 
                           studlab   = label,
                           sm        = 'PLOGIT', 
                           method.ci = "NAsm",
                           data      = bolivia,
                           byvar     = Decade)
saveRDS(meta_bolivia, 'test/meta_bolivia.RDS')

pdf (file="FinalPlots/meta_bolivia.pdf", width = 20, height = 70)
my_forest_function(meta_bolivia, max_prev = 5)
dev.off()



brazil <- filter(dat, country == "Brazil")

meta_brazil <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = brazil,
                        byvar     = Decade)
saveRDS(meta_brazil, 'test/meta_brazil.RDS')

pdf (file="FinalPlots/meta_brazil.pdf", width = 20, height = 70)
my_forest_function(meta_brazil, max_prev = 5)
dev.off()



chile <- filter(dat, country == "Chile")
chile$total <- chile$total + 1

meta_chile <- metaprop (event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = chile,
                        byvar     = Decade)

saveRDS(meta_chile, 'test/meta_chile.RDS')

pdf (file="FinalPlots/meta_chile.pdf", width = 20, height = 70)
my_forest_function(meta_chile)
dev.off()



colombia <- filter(dat, country == "Colombia")

meta_colombia <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = colombia,
                        byvar     = Decade)

saveRDS(meta_colombia, 'test/meta_colombia.RDS')

pdf (file="FinalPlots/meta_colombia.pdf", width = 20, height = 70)
my_forest_function(meta_colombia, max_prev = 5)
dev.off()

costarica <- filter(dat, country == "Costa Rica")

meta_costarica <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = costarica,
                        byvar     = Decade)

saveRDS(meta_costarica, 'test/meta_costarica.RDS')

pdf (file="FinalPlots/meta_costarica.pdf", width = 20, height = 70)
my_forest_function(meta_costarica, max_prev = 5)
dev.off()


ecuador <- filter(dat, country == "Ecuador")

meta_ecuador <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = ecuador,
                        byvar     = Decade)

saveRDS(meta_ecuador, 'test/meta_ecuador.RDS')

pdf (file="FinalPlots/meta_ecuador.pdf", width = 20, height = 70)
my_forest_function(meta_ecuador, max_prev = 5)
dev.off()

elsalvador <- filter(dat, country == "El Salvador")

meta_elsalvador <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = elsalvador,
                        byvar     = Decade)

saveRDS(meta_elsalvador, 'test/meta_elsalvador.RDS')

pdf (file="FinalPlots/meta_elsalvador.pdf", width = 20, height = 70)
my_forest_function(meta_elsalvador, max_prev = 5)
dev.off()

guatemala <- filter(dat, country == "Guatemala")

meta_guatemala <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = guatemala,
                        byvar     = Decade)

saveRDS(meta_guatemala, 'test/meta_guatemala.RDS')

pdf (file="FinalPlots/meta_guatemala.pdf", width = 20, height = 70)
my_forest_function(meta_guatemala, max_prev = 5)
dev.off()

honduras <- filter(dat, country == "Honduras")

meta_honduras <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = honduras,
                        byvar     = Decade)

saveRDS(meta_honduras, 'test/meta_honduras.RDS')

pdf (file="FinalPlots/meta_honduras.pdf", width = 20, height = 70)
my_forest_function(meta_honduras, max_prev = 5)
dev.off()

mexico <- filter(dat, country == "Mexico")

meta_mexico <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = mexico,
                        byvar     = Decade)

saveRDS(meta_mexico, 'test/meta_mexico.RDS')

pdf (file="FinalPlots/meta_mexico.pdf", width = 20, height = 70)
my_forest_function(meta_mexico, max_prev = 5)
dev.off()

nicaragua <- filter(dat, country == "Nicaragua")

meta_nicaragua <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = nicaragua,
                        byvar     = Decade)

saveRDS(meta_nicaragua, 'test/meta_nicaragua.RDS')

pdf (file="FinalPlots/meta_nicaragua.pdf", width = 20, height = 70)
my_forest_function(meta_nicaragua, max_prev = 5)
dev.off()

panama <- filter(dat, country == "Panama")

meta_panama <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = panama,
                        byvar     = Decade)

saveRDS(meta_panama, 'test/meta_panama.RDS')

pdf (file="FinalPlots/meta_panama.pdf", width = 20, height = 70)
my_forest_function(meta_panama, max_prev = 5)
dev.off()


paraguay <- filter(dat, country == "Paraguay")

meta_paraguay <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = paraguay,
                        byvar     = Decade)

saveRDS(meta_paraguay, 'test/meta_paraguay.RDS')

pdf (file="FinalPlots/meta_paraguay.pdf", width = 20, height = 70)
my_forest_function(meta_paraguay, max_prev = 5)
dev.off()

peru <- filter(dat, country == "Peru")

meta_peru <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = peru,
                        byvar     = Decade)

saveRDS(meta_peru, 'test/meta_peru.RDS')

pdf (file="FinalPlots/meta_peru.pdf", width = 20, height = 70)
my_forest_function(meta_peru, max_prev = 5)
dev.off()

usa <- filter(dat, country == "United States")

meta_usa <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = usa,
                        byvar     = Decade)

saveRDS(meta_usa, 'test/meta_usa.RDS')

pdf (file="FinalPlots/meta_usa.pdf", width = 20, height = 70)
my_forest_function(meta_usa, max_prev = 5)
dev.off()

uruguay <- filter(dat, country == "Uruguay")

meta_uruguay <- metaprop(event     = new_pos, 
                        n         = total, 
                        studlab   = label,
                        sm        = 'PLOGIT', 
                        method.ci = "NAsm",
                        data      = uruguay,
                        byvar     = Decade)

saveRDS(meta_uruguay, 'test/meta_uruguay.RDS')

pdf (file="FinalPlots/meta_uruguay.pdf", width = 20, height = 70)
my_forest_function(meta_uruguay, max_prev = 5)
dev.off()


venezuela <- filter(dat, country == "Venezuela")

meta_venezuela <- metaprop(event     = new_pos, 
                         n         = total, 
                         studlab   = label,
                         sm        = 'PLOGIT', 
                         method.ci = "NAsm",
                         data      = venezuela,
                         byvar     = Decade)

saveRDS(meta_venezuela, 'test/meta_venezuela.RDS')

pdf (file="FinalPlots/meta_venezuela.pdf", width = 20, height = 70)
my_forest_function(meta_venezuela, max_prev = 5)
dev.off()


a <- sort(dat$firstauthor)
write.csv(a, "FilesPlots/summary.csv")
b <- sort(dat)
dat
view(dat)
write.csv(dat, "FinalPlots/dat summary.csv")
