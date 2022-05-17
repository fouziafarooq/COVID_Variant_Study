library(dplyr)
library(ggplot2)

df <- read.csv('data/casesbymonth_220329_ff.csv', header = TRUE, na.strings = "")
variant_df <- read.csv('data/variant_data.csv', header = TRUE, na.strings = "")
  

  df <- df %>% dplyr::mutate(year = as.integer(substring(Strata, 10, 13)),
                      month = as.integer(substring(Strata, 14,15)))
  df <- df %>% dplyr::mutate(pandemic_month = month + (year-2020)*12)

df <- left_join(df, variant_df, by=c('year', 'month', 'Country'))
df <- df %>%
  mutate(Country = factor(Country),
         Strain_ff = factor(Strain_ff))


ggplot() +
  scale_x_continuous(name="month") +
  scale_y_continuous(name="Country") +
  geom_rect(data=df, mapping=aes(xmin=pandemic_month,
                                 xmax=pandemic_month+1,
                                 ymin=as.integer(Country)+0.1,
                                 ymax=as.integer(Country)+0.9,
                                 fill=Strain_ff), alpha=0.9) +

   geom_rect(data=(df %>% filter(Events>0)), mapping=aes(xmin=pandemic_month,
                                 xmax=pandemic_month+1,
                                 ymin=as.integer(Country)+0.4,
                                 ymax=as.integer(Country)+0.6),
             fill = 'grey1') +
  scale_y_continuous(breaks = 1:length(levels(df$Country))+0.5,
                     labels = levels(df$Country)) +
  xlim(1,25)+
  scale_x_continuous(breaks = 1:25) +
  xlab('Months Jan 2020 - Dec 2021 (1-24 months)')
  
  
  #  geom_text(data=d, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=4) +
  #  opts(title="geom_rect", plot.title=theme_text(size=40, vjust=1.5))

# geom_rect(data=df, mapping=aes(xmin=pandemic_month,
#                                xmax=pandemic_month+1,
#                                ymin=as.integer(Country)+0.4,
#                                ymax=as.integer(Country)+0.6,
#                                alpha=ifelse(Events>0, 1,0)),
#           fill = 'grey1') +
  
variant_df <- df %>% dplyr::select(year, month, Country, Strain_ff)
write.csv(variant_df, file = 'data/variant_data.csv', row.names = FALSE, na = "")
