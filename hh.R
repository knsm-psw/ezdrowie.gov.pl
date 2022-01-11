## -- R--
library("knitr")
library("dplyr")
library("ggplot2")
library("tidyverse")
library("scales")
library("ggpubr")
##
spanV <- 0.5
munit <- 1000000
mainBreaks <- "3 months"
mainXBreaks <- "6 months"
popCZ <- 38000000
pWd <- 9
note <- "© NI-KW || dane: https://ezdrowie.gov.pl lub https://github.com/knsm-psw/ezdrowie.gov.pl"

## Plik csv zawiera informacje nt liczby hospitacji rozbite na województwa i kategorie
## chorobowe; specjalne `wojewodztwo' o nazwie `PL' zawiera podsumowanie dla całego kraju
## Na podstawie danych
## Liczba hospitalizacji i liczba punktów za hospitalizacje wg rozpoznań i sekcji produktów
h0 <- read.csv("hospitalizacje_2018-2021WW.csv", sep = ';',  header=T, na.string="NA") %>%
        select(data, kat, woj, hospitalizacje ) %>%
        mutate (date = as.Date( sprintf ("%s-1", data))) %>%
        mutate(kat=recode(kat, 
          'A00-B99 Wybrane choroby zakaźne i pasożytnicze' = 'Zakaźne',
          'C00-D48 Nowotwory' = 'Nowotwory',
          'F00-F99 Zaburzenia psychiczne i zaburzenia zachowania' = 'Zaburzenia psychiczne',
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
## Dla woj=PL liczymy sumę po wszystkich kategoriach
h <- h0 %>% filter (woj == 'PL') %>%
        group_by(date) %>%
        summarise( hospitalizacje = sum(hospitalizacje, na.rm=T)) %>% ungroup()

## W1: PL ogółem 
p1 <- ggplot(h, aes(x= date, y=hospitalizacje)) + 
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
ggsave(plot=p1, "PL_hospitalizacje_PL.png")

## Różne grupy chorobowe

ch1 <- c('Układu krążenia', 'Nowotwory', 'Układu oddechowego', 'Zakaźne')
ch2 <- c('Zaburzenia psychiczne')
ch3 <- c('Zakaźne', 'Nowotwory', 'Zaburzenia psychiczne', 'Układu nerwowego', 'Oka',
        'Układu krążenia', 'Układu oddechowego',
        'Układu pokarmowego', 'Skóry', 'Układu mięśniowo-szkieletowego',
        'Układu moczowo-płciowego', 'Ciąża, poród', 'Urazy, zatrucia' )
 ch4 <- c('Zaburzenia psychiczne', 'Układu nerwowego', 'Urazy, zatrucia', 
          'Układu pokarmowego', 'Ciąża, poród')       

## Dla woj =PL grupujemy po kategoriach
h <- h0 %>% filter (woj == 'PL') %>%
        group_by(date, kat) %>%
        summarise( hospitalizacje = sum(hospitalizacje, na.rm=T)) %>%
        ungroup()

## W2: wg chorób z grupy ch1
p1a <- h %>% filter (kat %in% ch1) %>%
        ggplot(aes(x= date, y=hospitalizacje, color=kat)) + 
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

## W3: wg chorób z grupy ch2
p1b <- h %>% filter (kat %in% ch2) %>%
        ggplot(aes(x= date, y=hospitalizacje, color=kat)) + 
        geom_smooth(method="loess", se=F, span=spanV) +
        geom_point(size=1, alpha=.5) +
        ##geom_line(size=1, alpha=.5) +
        xlab(label="") +
        ##ylab(label="age") +
        scale_x_date( labels = date_format("%Y/%m"), breaks = mainBreaks) +
        theme_nikw() +
        ggtitle(sprintf("Liczba hospitalizacji w PL (2018--2021 dane miesięczne)"),  
                subtitle=note)

p1b
ggsave(plot=p1b, "PL_hospitalizacje_J00_99.png")


## W4: wg chorób z grupy ch3
p1c <- h %>% filter (kat %in% ch3) %>%
        ggplot(aes(x= date, y=hospitalizacje)) + 
        geom_smooth(method="loess", se=F, span=spanV) +
        geom_point(size=1, alpha=.5) +
        facet_wrap( ~kat, scales = "free_y") +
        ##geom_line(size=1, alpha=.5) +
        xlab(label="") +
        ##ylab(label="age") +
        scale_x_date( labels = date_format("%y/%m"), breaks = mainXBreaks) +
        theme_nikw() +
        ggtitle(sprintf("Liczba hospitalizacji w PL (wg kategorii ICD | 2018--2021 dane miesięczne)"),  
                subtitle=note)
p1c
ggsave(plot=p1c, "PL_hospitalizacje_XXXX.png", width = 10)

## Procentowo wzg. chorób z grupu ch3
h.addr.pl <- h %>% filter (kat %in% ch3) %>%
        group_by(kat) %>%
        summarise(s = mean(hospitalizacje, na.rm = T))

h.p.pl <- left_join(h.addr.pl, h, by='kat') %>%
        mutate( hp = hospitalizacje / s * 100)
str(h.p.pl)

## W5: wg chorób z grupy ch3 (%)
p1c.p <- ggplot(h.p.pl, aes(x= date, y=hp)) + 
        geom_smooth(method="loess", se=F, span=spanV) +
        geom_point(size=1, alpha=.5) +
        facet_wrap( ~kat, scales = "fixed") +
        ##geom_line(size=1, alpha=.5) +
        xlab(label="") +
        ylab(label="% średniej 2018--21") +
        ##geom_hline(yintercept = 100, color="green1", alpha=.3, size=0.4) +
        geom_hline(yintercept = 100, color="green1", alpha=.2, size=0.6) +
        geom_hline(yintercept = 50, color="red", alpha=.2, size=0.6) +
        scale_x_date( labels = date_format("%y/%m"), breaks = mainXBreaks) +
        theme_nikw() +
        ggtitle(sprintf("Liczba hospitalizacji w PL w %% średniej (wg kategorii ICD | 2018--2021 | miesięcznie)"),  
                subtitle=note)
p1c.p
ggsave(plot=p1c.p, "PL_hospitalizacje_PLP.png", width = 10)


## Pomiń PL wybrane choroby
h <- h0 %>% filter ( woj != 'PL') %>% filter (kat %in% ch3) 

# W6: wszystko na jednym wykresie 
p1d <- h %>% filter (! kat %in% ch4 ) %>%
        ggplot(aes(x= date, y=hospitalizacje, color=kat)) + 
        geom_smooth(method="loess", size=.4, se=F, span=spanV) +
        ##geom_point(size=1, alpha=.5) +
        facet_wrap( ~woj, scales = "free_y") +
        ##geom_line(size=1, alpha=.5) +
        xlab(label="") +
        ##ylab(label="age") +
        scale_x_date( labels = date_format("%y/%m"), breaks = mainXBreaks) +
        theme_nikw() +
        ggtitle(sprintf("Liczba hospitalizacji w PL (wg kategorii ICD | 2018--2021 dane miesięczne)"),  
                subtitle=note)
p1d
ggsave(plot=p1d, "PL_hospitalizacje_WW.png", width = 10)


## Pomorskie

h.pom <- h %>% filter (woj == 'Pomorskie') 
       
p1e <- ggplot(h.pom, aes(x= date, y=hospitalizacje)) + 
                geom_smooth(method="loess", se=F, span=spanV) +
                geom_point(size=1, alpha=.5) +
                facet_wrap( ~kat, scales = "free_y") +
                ##geom_line(size=1, alpha=.5) +
                xlab(label="") +
                ##ylab(label="age") +
                scale_x_date( labels = date_format("%y/%m"), breaks = mainXBreaks) +
                theme_nikw() +
                ggtitle(sprintf("Liczba hospitalizacji w woj Pomorskim (wg kategorii ICD | 2018--2021 dane miesięczne)"),  
                        subtitle=note)
p1e
ggsave(plot=p1e, "PL_hospitalizacje_T22.png", width = 10)

##  Pomorskie w %%      
h.addr.pom <- h.pom %>% group_by(kat) %>%
        summarise(s = mean(hospitalizacje, na.rm = T)) %>% ungroup()

h.p.pom <- left_join(h.addr.pom, h.pom, by='kat') %>%
        mutate( hp = hospitalizacje / s * 100)
str(h.p.pom)

p1f <- ggplot(h.p.pom, aes(x= date, y=hp)) + 
        geom_smooth(method="loess", se=F, span=spanV) +
        geom_point(size=1, alpha=.5) +
        facet_wrap( ~kat, scales = "fixed") +
        ##geom_line(size=1, alpha=.5) +
        xlab(label="") +
        ylab(label="% średniej 2018--21") +
        geom_hline(yintercept = 100, color="green1", alpha=.2, size=0.6) +
        geom_hline(yintercept = 50, color="red", alpha=.2, size=0.6) +
        scale_x_date( labels = date_format("%y/%m"), breaks = mainXBreaks) +
        theme_nikw() +
        ggtitle(sprintf("Liczba hospitalizacji w woj Pomorskim w %% średniej (wg kategorii ICD | 2018--2021 | miesięcznie)"),  
                subtitle=note)
p1f
ggsave(plot=p1f, "PL_hospitalizacje_T22P.png", width = 10)


## Na jednym wykresie PL i woj POM w %%

h.pl_and_pom <- left_join(h.p.pl, h.p.pom, by=c('kat', 'date')) %>%
        select (data, kat, PL=hp.x, POM=hp.y) %>%
        mutate (date = as.Date( sprintf ("%s-1", data))) %>%
        pivot_longer(cols=c('PL', 'POM'), names_to = 'obszar', values_to = 'hp')

note.det <- "% średniej za okres 2018--2021"
p1g <- ggplot(h.pl_and_pom, aes(x= date, y=hp, color=obszar)) + 
        geom_smooth(method="loess", size=.6, se=F, span=spanV) +
        geom_point(size=1, alpha=.5) +
        facet_wrap( ~kat, scales = "fixed") +
        ##geom_line(size=1, alpha=.5) +
        xlab(label="") +
        ylab(label="% średniej 2018--21") +
        geom_hline(yintercept = 100, color="green1", alpha=.2, size=0.6) +
        geom_hline(yintercept = 50, color="red", alpha=.2, size=0.6) +
        scale_x_date( labels = date_format("%y/%m"), breaks = mainXBreaks) +
        theme_nikw() +
        labs(caption=note.det) +
        ggtitle(sprintf("Liczba hospitalizacji w PL i woj Pomorskim (wg kategorii ICD | 2018--2021 | miesięcznie)"),  
                subtitle=note)
p1g
ggsave(plot=p1g, "PL_hospitalizacje_PL_vs_POM.png", width = 10)
### // koniec //