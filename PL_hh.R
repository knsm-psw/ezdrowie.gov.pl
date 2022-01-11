## -- R--
library("knitr")
library("dplyr")
library("ggplot2")
library("tidyverse")
library("scales")
##
spanV <- 0.5
munit <- 1000000
mainBreaks <- "3 months"
mainBreaks <- "6 months"
popCZ <- 38000000
pWd <- 9
note <- "© NI-KW || dane: https://ezdrowie.gov.pl"


h0 <- read.csv("hospitalizacje_2018-2021PL_short.csv", sep = ';',  header=T, na.string="NA") %>%
select(data, hospitalizacje ) %>%
mutate (date = as.Date( sprintf ("%s-1", data)))


p1 <- ggplot(h0, aes(x= date, y=hospitalizacje)) + 
 geom_smooth(method="loess", se=F, span=spanV) +
 geom_point(size=1, alpha=.5, color=default_red) +
 ##geom_line(size=1, alpha=.5) +
 xlab(label="") +
 ##ylab(label="age") +
 scale_x_date( labels = date_format("%Y/%m"), breaks = mainBreaks) +
 theme_nikw() +
 ggtitle(sprintf("Liczba hospitalizacji w PL (2018--2021 dane miesięczne)"),  
         subtitle=note)

p1
ggsave(plot=p1, "PL_hospitalizacje.png")


## wg kategorii chorobowych
h1 <- read.csv("hospitalizacje_2018-2021PL.csv", sep = ';',  header=T, na.string="NA") %>%
select(data, kat, hospitalizacje ) %>%
mutate (date = as.Date( sprintf ("%s-1", data)))

h <- h1 %>% filter (kat == 'J00-J99 Choroby układu oddechowego' |
                            kat == 'I00-I99 Choroby układu krążenia' |
                            kat == 'C00-D48 Nowotwory' )
p1a <- ggplot(h, aes(x= date, y=hospitalizacje, color=kat)) + 
        geom_smooth(method="loess", se=F, span=spanV) +
        geom_point(size=1, alpha=.5) +
        ##geom_line(size=1, alpha=.5) +
        xlab(label="") +
        ##ylab(label="age") +
        scale_x_date( labels = date_format("%Y/%m"), breaks = mainBreaks) +
        theme_nikw() +
        ggtitle(sprintf("Liczba hospitalizacji w PL (2018--2021 dane miesięczne)"),  
                subtitle=note)

p1a
ggsave(plot=p1a, "PL_hospitalizacje_J00_99.png")


h <- h1 %>% filter (kat == 'F00-F99 Zaburzenia psychiczne i zaburzenia zachowania')
p1a <- ggplot(h, aes(x= date, y=hospitalizacje, color=kat)) + 
        geom_smooth(method="loess", se=F, span=spanV) +
        geom_point(size=1, alpha=.5) +
        ##geom_line(size=1, alpha=.5) +
        xlab(label="") +
        ##ylab(label="age") +
        scale_x_date( labels = date_format("%Y/%m"), breaks = mainBreaks) +
        theme_nikw() +
        ggtitle(sprintf("Liczba hospitalizacji w PL (2018--2021 dane miesięczne)"),  
                subtitle=note)

p1a
ggsave(plot=p1a, "PL_hospitalizacje_J00_99.png")

h <- h1 %>% filter (
   kat == 'C00-D48 Nowotwory' |
   kat == 'F00-F99 Zaburzenia psychiczne i zaburzenia zachowania' |
   kat == 'G00-G99 Choroby układu nerwowego' |
   kat == 'H00-H59 Choroby oka i przydatków oka' |
   kat == 'I00-I99 Choroby układu krążenia' |
   kat == 'J00-J99 Choroby układu oddechowego' |
   kat == 'K00-K93 Choroby układu pokarmowego' |
   kat == 'L00-L99 Choroby skóry i tkanki podskórnej' |
   kat == 'M00-M99 Choroby układu mięśniowo-szkieletowego i tkanki łącznej' |
   kat == 'N00-N99 Choroby układu moczowo-płciowego' |
   kat == 'O00-O99 Ciąża, poród i okres połogu' |
   kat == 'S00-T98 Urazy, zatrucia i inne określone skutki działania czynników zewnętrznych'
           ) %>%
        mutate(kat=recode(kat, 
          'C00-D48 Nowotwory' = 'Nowotwory',
          'F00-F99 Zaburzenia psychiczne i zaburzenia zachowania'='Zaburzenia psychiczne',
          'G00-G99 Choroby układu nerwowego' = 'Układu nerwowego',
          'H00-H59 Choroby oka i przydatków oka' = 'Oka',
          'I00-I99 Choroby układu krążenia' = 'Układu krążenia',
          'J00-J99 Choroby układu oddechowego' = 'Układu oddechowego',
          'K00-K93 Choroby układu pokarmowego' = 'Układu pokarmowego',
          'L00-L99 Choroby skóry i tkanki podskórnej' = 'Skóry',
          'M00-M99 Choroby układu mięśniowo-szkieletowego i tkanki łącznej' = 'Układu mięśniowo-szkieletowego',
          'N00-N99 Choroby układu moczowo-płciowego' = 'Układu moczowo-płciowego',
          'O00-O99 Ciąża, poród i okres połogu' = 'Ciąża, poród',
          'S00-T98 Urazy, zatrucia i inne określone skutki działania czynników zewnętrznych' = 'Urazy, zatrucia'
        ))
                          
 

p1c <- ggplot(h, aes(x= date, y=hospitalizacje)) + 
        geom_smooth(method="loess", se=F, span=spanV) +
        geom_point(size=1, alpha=.5) +
        facet_wrap( ~kat, scales = "free_y") +
        ##geom_line(size=1, alpha=.5) +
        xlab(label="") +
        ##ylab(label="age") +
        scale_x_date( labels = date_format("%y/%m"), breaks = mainXBreaks) +
        theme_nikw() +
        ggtitle(sprintf("Liczba hospitalizacji w PL (2018--2021 dane miesięczne)"),  
                subtitle=note)
p1c
ggsave(plot=p1c, "PL_hospitalizacje_XXXX.png", width = 10)
