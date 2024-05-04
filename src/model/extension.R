"
This file applies our analysis to

- Dai, Y., & Kustov, A. (2023). The (in)Effectiveness of Populist Rhetoric: a Conjoint Experiment of Campaign Messaging. Political Science
  Research and Methods, (), 1–8. Http://Dx.Doi.Org/10.1017/Psrm.2023.55
  (Data for tihs replication is available at https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/C9CMQ2;
  Access: Jan 13, 2024)

- Neuner, Fabian G.; Wratil, Christopher, 2020, Replication Data for: The Populist Marketplace: Unpacking the Role of Thin and Thick
  Ideology, https://doi.org/10.7910/DVN/Z66XXP, Harvard Dataverse, V1, UNF:6:p//+ecIT7mUDRQeNT7ME4w== [fileUNF]

- Silva, B. C., ; Neuner, F. G.; Wratil, C., 2022, Replication Data for: Populism and Candidate Support in the US: The Effects of Thin
   and Host Ideology, https://doi.org/10.7910/DVN/5AEGPM, Harvard Dataverse, V1, UNF:6:KcjLEHCNRommd/f8xjz/eg== [fileUNF]

"

library(sjmisc)
library(writexl)
library(ggpubr)
library(expss)
library(furrr)
library(fixest)
library(broom)
library(glue)
library(labelled)
library(kableExtra)
library(tidyverse)
library(cregg) #0.3.0
library(dotwhisker) #0.5.0
library(random.polychor.pa) #1.1.4-3
library(psych)
##
source("../__paths__.R")
source("../__constants__.R")
## 
SAVE = FALSE
SAVE = TRUE
##

## * functions

get_pos <- function(tab, pos='none', rw)
{
    
    cases = (
        tab 
        %>% filter(str_detect(term, pattern=rw))
        %>% filter(str_detect(ideo, pattern="Right")) 
    )
    if (pos=='x')
    {
        cases = (
            cases
            %>% distinct(term, .keep_all=TRUE)  
            %>% dplyr::pull(term)  %>% as.character(.)
        )
    }
    if (pos=='y')
    {
        cases = (
            cases %>% dplyr::pull(conf.high) %>% max(.) +.05
        )
    }
    return(cases)
}

## * -------- dai2023the --------
## * loading data

## not provided (with replication files)
fn = file.path("~/Dropbox/CienciasSociais/data/replication/dai2023the/rep/Conjoint_clean.RData")
load(fn)
df = d_stack %>% as_tibble() 

## * overview

df %>% select(matches(".")) %>% names()
df %>% select(matches("congruent")) %>% names()
df %>% frq(attention.pass)
df %>% frq(populismatt)
df %>% frq(partyid2)
df %>% cross_cases(economy2, economy2congruent)
df %>% frq(moralism3)


## * recoding

## ## moralism
## ## df %>% frq(moralism)
## antielitism = c("But there're bad people in Washington who don't care about Americans.",
##               "Sadly, Congress is full of insiders who only care about themselves now.",
##               "Sadly, so many in Washington are out of touch with the American people.",
##               "Sadly, the corrupt Washington elites only listen to special interests.",
##               "I'll protect Americans against all evil in Washington.",
##               "I'll fix the mess in Washington for the sake of America.",
##               "I believe politicians in Congress talk too much and take too little action.",
##               "Unfortunately, the government has created more problems than it has solved.")
## elitism = c("I believe our government is there to help people, even though it's not easy.",
##              "I'll work with all my colleagues in Congress who want what's best for Americans.",
##              "I trust our Congress is mostly full of honest people who care for Americans.",
##              "I believe that Washington needs more qualified people like me at the moment."
##             )
## ## 
## ## pluralism
## ## df %>% frq(pluralism)
## people.centric = c("I am running to represent the voice of the American people.",
##                    "I believe the government is to respond to the will of the people.",
##                    "I believe we, the people, share the same values and interests.",
##                    "This campaign is not about me, it is about the American people.")
## non.people.centric = c("As your representative, I will make sure to listen to the experts.",
##                        ## "I'm running to bring expertise back to politics in Washington.",
##                        "I 'll do what is best for America even if the people disagree.",
##                        "I'll bring the best people to solve our problems in America.")
## pluralist = c("I'll make sure to listen to all the different voices of the people.",
##               "I'm running to defend all our rights, no matter our differences.",
##               ## "I'll serve everyone in America regardless of their convictions.",
##               "I am running to represent our diverse American voices.")
## ## 

df = (
    df 
    %>% mutate(
            ## voters
            ideo  = ideology3 %>% factor(., levels=c('Liberal', 'Moderate',
                                                     'Conservative')),
            ptyid = partyid2,
            popatt1= populismattstr,
            popatt2= populismatt,
            ## 
            cand.origen   = case_when(job2 == 'elite' ~1,
                                      job2 == 'non-elite' ~0),
            cand.exp      = case_when(office=='held an office for many years'~1,
                                      office=='never held an office before'  ~0),
            cand.leading  = case_when(polls2 == 'leading'  ~ 1,
                                      polls2 == 'trailing' ~ 0),
            ## 
            cand.econ.rw = case_when(economy2congruent == 'congruent' ~ 1,
                                     economy2congruent == 'non-congruent'  ~ 0),
            cand.ai.rw   = case_when(immigration2congruent=='congruent '      ~1,
                                     immigration2congruent=='non-congruent '  ~0),
            cand.ant = case_when(
                antiestablishment2 == 'pro-establishment' ~ 0,
                antiestablishment2 == 'anti-establishment' ~ 1,
            ),
            cand.ppl = case_when(
                antipluralism3 == 'pluralist / non-people-centric'  ~ 0,
                antipluralism3 == 'pluralist / people-centric'      ~ 0,
                antipluralism3 == 'anti-pluralist / people-centric' ~ 1,
            ),
            cand.pop      = cand.ant * cand.ppl,
            cand.econ.rwp = cand.econ.rw * cand.pop,
            cand.ai.rwp   = cand.ai.rw * cand.pop,
            )
)

## ## checking
## ## --------
## df %>% expss::cross_cases(economy2congruent, cand.econ.rw)
## df %>% expss::cross_cases(economy2, cand.econ.rw)
## df %>% expss::cross_cases(immigration2congruent, cand.ai.rw)
## df %>% expss::cross_cases(moralism, cand.ant)
## df %>% expss::cross_cases(pluralism, cand.ppl)
## df %>% expss::cross_cases(cand.econ.rw, cand.rwp.econ, cand.pop)
## df %>% expss::cross_cases(cand.ai.rw, cand.rwp.ai, cand.pop)
## df %>% expss::cross_cases(cand.ant, cand.pop, cand.ppl)

## * subsetting

df = (
    df 
    %>% filter(attention.pass==1)  
)

df %>% select(matches(".")) %>% names()
df %>% frq(ideo)
df %>% frq(ptyid)
df %>% frq(popatt1)
df %>% frq(popatt2)



## * extension
## ** Estimation

ideo = 'ptyid'
ideo = 'ideo'
popatt = 'popatt1'
popatt = 'popatt2'
## 
formula = formula(Y ~
    cand.econ.rw +
    cand.ai.rw +
    cand.ant +
    cand.ppl +
    cand.pop +
    cand.econ.rwp +
    cand.ai.rwp +
    cand.origen +
    cand.exp +
    cand.leading | id)
res.dai = (
    df
    %>% nest(-!!ideo, -popatt)   
    %>% drop_na() 
    %>% mutate(
            fit = future_map(.x=data, function(.x)
                feols(formula, cluster=~id, data=.x)
                ),
            summ = future_map(.x=fit, function(.x) tidy(.x, conf.int=TRUE)),
            glance = future_map(.x=fit, function(.x) glance(.x)),
        ) 
    %>% ungroup(.)  
    %>% drop_na() 
)
res.dai


## ** Figure H1

figure = 'fig-h1'
## 
tab = (
    res.dai
    %>% select(ideo, popatt, summ) 
    %>% unnest(summ)  
    %>% filter(str_detect(term, pattern="rw"))   
    %>% mutate(term = str_replace_all(term,
                                      c(
                                          'cand.econ.rwp'= 'Economic policy (RWP)',
                                          'cand.econ.rw'= 'Economic policy (RW)',
                                          'cand.ai.rwp'= ' Immigration (RWP)',
                                          'cand.ai.rw' = ' Immigration (RW)'
                                      )),
               )
)
tab
## 
x = "term"
y = "estimate"
color    = NULL
fill     = popatt
shape    = popatt
facet1   = ideo
facet2   = NULL
leg      = 'Populist Attitudes'
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .3
ylab     = glue("Average Marginal Effect of Candidate\n",
                "Attributes on Voters' Support for the Candidate")
xlab     = NULL
g = (
    tab
    %>% ggplot(.)
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
    + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high",
                               color=fill), width=.05,
                    position=position_dodge(dodge)
                    ) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill),
                 position=position_dodge(dodge))
    + scale_x_discrete(labels = scales::wrap_format(10))
    + scale_shape_manual(values=c(25,24))
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_color_grey(start = 0.7, end = 0,  na.value="red") 
    + facet_wrap( paste("~", facet1))  
    + labs(
          x        = NULL,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
    + ggguides()
)
g
gi = (
    g 
    + geom_line(aes_string(x=x, y=y, group=fill, color=fill),
                linetype=2,
                data=. %>% filter(str_detect(term, pattern="Econ")) 
                )
    + geom_line(aes_string(x=x, y=y, group=fill, color=fill),
                linetype=2,
                data=. %>% filter(str_detect(term, pattern="Immi")) 
                )
)
gi
if (SAVE) {
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_SM_FIGURES, fn)
    for (fn in fns){ggsave(gi, filename=fn, width=9.5, height=4.5 )}
}

## ** Table H1

table = 'tab-h1'
## 
ideo = 'ideo'
pop  = 'popatt2'
tab = (
    res.dai  
    %>% unnest(summ)
    %>% filter(str_detect(term, pattern="rw")) 
    %>% select(ideo, popatt=!!pop, term, b=estimate, se=`std.error`)
    %>% mutate(thin = case_when(
                   str_detect(term, pattern=".*\\.rw$") ~ "RW",
                   str_detect(term, pattern=".*\\.rwp$") ~ "RWP"
               ),
               thick = case_when(
                   str_detect(term, pattern="econ") ~ 'Economic liberalism',
                   str_detect(term, pattern="cand.ai") ~ 'Anti-immigration',
                   ),
               popatt = case_when(
                  popatt=="populist" ~ 'High',
                  popatt=="non-populist" ~ 'Low',
                  ),
               ) 
    %>% select(-term)
    %>% nest(-ideo, -popatt)
    %>% mutate(data = future_map(.x=data, function(data=.x)
        data
        %>% pivot_wider(id_cols=thick,
                        names_from=thin,
                        values_from=c(b, se)
                        )
        %>% mutate(
                perc.diff = 100*(b_RWP - b_RW)/abs(b_RW),
                t.diff = (b_RWP -b_RW)/sqrt(se_RWP^2 + se_RW^2),
                p.value = 1-pnorm(abs(t.diff))
            ))) 
    %>% unnest(data) 
    %>% arrange(desc(ideo),thick, popatt, p.value  ) 
    %>% rowid_to_column()
)
tab %>% print(., n=Inf, width=Inf) 
## 
row.number.bold = (
    tab 
    %>% filter(ideo=='Conservative' & popatt=='High' &
               str_detect(thick, pattern="Econ")) 
    %>% pull(rowid)
)
caption=glue("\\label{{{table}}}Statistics from Figure \\ref{{fig-h1}}",
             " ordered by p-value") 
caption
tabl = (
    tab       
    %>% select(popatt, ideo, everything())
    %>% mutate(
            across(where(is.numeric), round, 4),
            b_RW = glue("{b_RW} ({se_RW})"),
            b_RWP = glue("{b_RWP} ({se_RWP})"),
            )
    %>% select(
            "Voters' Ideology" = ideo,
            "Populist Attitudes" = popatt,
            "Candidate Attribute"=thick,
            "$\\beta_{RW}$" = b_RW,
            "$\\beta_{RWP}$" = b_RWP,
            't-stat'=t.diff,
            'p-value'=p.value
        )
    %>% kable(., "latex", booktabs = T, caption=caption,
              escape=F, align=c("l", rep("c", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL)
    %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
                      position = "center", font_size=NULL) 
    %>% row_spec(row.number.bold, bold=T, hline_after = F)
)
tabl

## 
if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_SM_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}



## * -------- neuner2022the --------
## * loading 

path = '~/Dropbox/CienciasSociais/data/replication/neuner2022the/data/final/'
load(file.path(path,'survey.Rdata'))

## * recoding



## ## ppl
## Pop1. I would rather be represented by an ordinary citizen than by a specialized politician.
## Pop3. The politicians in the U.S. Congress need to follow the will of the people.
## Pop5. The people, and not politicians, should make our most important political decisions.
## ## ant
## Pop6. Elected officials talk too much and take too little action.
## Pop4. What people call “compromise” in politics is really just selling out on one’s principles.
## Pop8. Political parties only want peoples’ votes, they do not care about their opinions.
## ## man
## Pop2. The political diff b2 the elite and the people are greater than the differences among the people.
## Pop7. The people are often in agreement but the politicians pursue quite different goals.
## Pop9. The people in the U.S. agree, on principle, about what should happen politically.

df.neuner = (
    neuner2022the
    %>% mutate(
            lfdn          = lfdn_W3,
            id            = lfdn_W3,
            ## 
            eu.membership = europ_union_mitgliedschaft, 
            eurozone      = europ_vereinigung         , 
            globalization = globalisierung            , 
            refugees_war  = fluechtlinge_krieg        , 
            immigrants    = einwanderer               , 
            wealthy       = reiche                    , 
            ## 
            Pop1 = pop1_volksabstimmungen,
            Pop2 = pop2_buerger_vs_politiker,
            Pop3 = pop3_einfacher_buerger,
            Pop4 = pop4_parteien,
            Pop5 = pop5_responsivitaet_bundestag,
            Pop6 = pop6_einigkeit_buerger,
            Pop7 = pop7_buerger_vs_politiker2,
            Pop8 = pop8_kompromiss_verrat
        ) 
)
## 
## PCA/FA
## ------
## 
## populist attitudes
popatt <- (
    df.neuner 
    %>% select("id", "Pop1", "Pop2", "Pop3", "Pop4", "Pop5", "Pop6", "Pop7", "Pop8")
    %>% distinct(id, .keep_all = T)  
    %>% rename_all(list(~str_replace(string=., pattern="thin_p", replacement="P")))
)
fa.popatt <- fa(as.matrix(popatt[,-1]),
                nfactors = 1, scores = "regression", cor = "poly")
scores.popatt <- popatt %>% select(id) %>% bind_cols(., popatt.fa=c(fa.popatt$scores))
## policy position variables
polpos <- (
    df.neuner
    %>% select_("id",
                "europ_union_mitgliedschaft",
                "europ_vereinigung", 
                "globalisierung", 
                "fluechtlinge_krieg", 
                "einwanderer",
                "reiche"
                )
    %>%remove_val_labels() 
    %>%group_by(id) 
    %>%distinct(., .keep_all=TRUE) 
    %>%ungroup(.) 
)
fa.polpos <- fa(as.matrix(polpos[, -1]),
                nfactors = 1, scores = "regression", cor = "poly")
scores.polpos = polpos %>% select(id) %>% bind_cols(., polpos.fa=c(fa.polpos$scores))
## 
df.neuner = (
    df.neuner  
    ## pca/fa score
    ## ------------
    %>% select(-matches('popatt.fa|polpos.fa'))
    %>% left_join(., scores.popatt,  by=c('id')) 
    %>% left_join(., scores.polpos, by=c('id')) 
    %>% mutate(
            pop.silva   = popatt.fa,
            pop.silva2c = case_when(popatt.fa >= 0 ~ 'High',
                                    popatt.fa  < 0 ~ 'Low') %>% as.factor(),
            ##
            pplscore =  prcomp(across(c(Pop1, Pop3, Pop5)), scale=T, center=T)$x[,1],
            antscore =  prcomp(across(c(Pop6, Pop4, Pop8)), scale=T, center=T)$x[,1],
            manscore =  prcomp(across(c(Pop2, Pop7)), scale=T, center=T)$x[,1],
            antscore01 = (antscore - min(antscore)) / (max(antscore) - min(antscore)),
            manscore01 = (manscore - min(manscore)) / (max(manscore) - min(manscore)),
            pplscore01 = (pplscore - min(pplscore)) / (max(pplscore) - min(pplscore)),
        )
    %>% rowwise(.)
    %>% mutate(
            popmin = min(pplscore, antscore, manscore),
            popadd = pplscore + antscore + manscore,
            popmul = pplscore01 * antscore01 * manscore01,
         )
    %>% ungroup(.)  
    %>% mutate(
            popmin2c = cut(popmin,
                           breaks = quantile(popmin, probs= seq(from=0, to=1, length=3)),
                           labels = c('Low', 'High'),
                           include.lowest=TRUE),
            )
    ## other variables
    ## ---------------
    %>% mutate_at(.vars=vars(matches("^[BC][0-9]_")),
                  ~str_replace(string=.,
                               pattern="^No priority[0-9]+$",
                               replacement="0")) 
    %>% mutate_at(.vars=vars(matches("^[BC][0-9]_")),
                  ~str_replace(string=.,
                               pattern="^Priority[0-9]+$", replacement="1"))
    %>% mutate_at(vars(matches("^[BC][0-9]_")), ~as.numeric(.)) 
    ## 
    %>% mutate(across(where(is.labelled), as_factor))
    %>% mutate(
            ideo = case_when(polpos.fa >=0~'Right-wing',
                             polpos.fa <0 ~'Left-wing'), 
               cand.rwi = case_when(
                   A1_refugees == "Is for the deportation of some refugees"  ~1,
                   A1_refugees == "Is for the deportation of a great many refugees" ~1,
                   A1_refugees == "Is for the admission of some new refugees" ~0,
                   A1_refugees == "Is for the admission of a great many new refugees"  ~0),
               cand.rwl = case_when(
                   A3_tax == "Is for much higher taxes on the rich"     ~ 0,
                   A3_tax == "Is for somewhat higher taxes on the rich" ~ 0,
                   A3_tax == "Is for somewhat lower taxes on the rich"  ~ 1,
                   A3_tax == "Is for much lower taxes on the rich"      ~ 1),
               cand.ppl = case_when(B4_citizens==1 | B3_democracy ==1 ~ 1,
                                    TRUE                              ~ 0),
               cand.ant = case_when(B2_elite ==1 | B5_crisis == 1  ~ 1,
                                    TRUE                           ~ 0),
               cand.pop  = cand.ppl * cand.ant,
               cand.rwip = cand.pop * cand.rwi,
               cand.rwlp = cand.pop * cand.rwl,
             )
    %>% glimpse()
)


## * overview

## df %>% look_for(".", details=F)
## df %>% sjmisc::frq(ptyid)
## df %>% expss::cross_cases(ptyid, popatt)
## df %>% sjmisc::frq(eurozone)

## describeBy(df$scores, group=df$popatt, mat=T, fast=T)
## boxplot(scores~popatt, data=df)

## tab = df %>% select(matches('^pop|^ppl|^man|^ant|silva$'))
## tab %>% names
## GGally::ggpairs(tab)


## tab = df %>% select(matches('^pop[0-9]$'), popmin, popmin2c, pop.silva, pop.silva2c)
## tab %>% names
## GGally::ggpairs(tab)


## tab = df %>% select( eu.membership,eurozone,globalization,refugees_war,immigrants,wealthy, ideo)
## tab %>% names
## GGally::ggpairs(tab)


## * extension
## ** estimation


popatt = 'pop.silva2c'
popatt = 'popmin2c'
ideo   = 'ideo'
y      = 'praeferenz'
res.neuner = (
    df.neuner 
    %>% sjlabelled::as_label()
    %>% rename(y=!!y, popatt=!!popatt, ideo=!!ideo) 
    %>% drop_na(popatt, ideo) 
    ## 
    %>% nest(-popatt, -ideo)  
    %>% mutate(
            formula = glue("y ~ cand.rwl + cand.rwlp + cand.rwi + cand.rwip | id"),
            fit = future_map2(.x=formula, .y=data, function(.x, .y) 
                feols(formula(.x), cluster=~id, data=.y)) ,
            summ = future_map(.x=fit, function(.x) tidy(.x, conf.int=TRUE)),
        )
    %>% glimpse()
)
res.neuner

## ** Figure H2

figure = 'fig-h2-neuner2022the'
## 
## 
labels = c(
    cand.rwi  = "Deport refugees\n(RW)",
    cand.rwip = "Deport refugees\n(RWP)",
    ## 
    cand.rwl  = "Economic policy\n(RW)",
    cand.rwlp = "Economic policy\n(RWP)"
)
tab = (
    res.neuner 
    %>% select(-data) 
    %>% unnest(summ) 
    %>% mutate(term = factor(term, names(labels), labels)) 
    %>% glimpse()
)
## 
x = "term"
y = "estimate"
color    = NULL
fill     = 'popatt'
facet1   = 'ideo'
facet2   = NULL
leg      = "Populist Attitudes"
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .1
ylab = glue("Average Marginal Effect of Candidate\n",
            "Attributes on Voters' Support for the Candidate")
xlab     = NULL
dodge=.3
g = (
    tab
    %>% ggplot(.)
    + geom_line(aes_string(x=x, y=y, group=fill, color=fill),
                show.legend = F,
                linetype=2,
                position=position_dodge(dodge),
                data= . %>% filter(str_detect(term, pattern=".eport")) )
    + geom_line(aes_string(x=x, y=y, group=fill, color=fill),
                show.legend = F,
                linetype=2,
                position=position_dodge(dodge),
                data= . %>% filter(str_detect(term, pattern=".conomic")) )
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill), position=position_dodge(dodge))
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
    + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high", color=fill), width=.05,
                  , position=position_dodge(dodge)) 
    ## 
    + scale_x_discrete(labels = scales::wrap_format(10))
    + scale_shape_manual(values=c(25,24))
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_color_grey(start = 0.7, end = 0,  na.value="red") 
    ## 
    + facet_wrap(glue("~ {facet1}"), ncol = , scales='free_x')
    ## 
    + ggguides()
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
)
g
if (SAVE) {
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_SM_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=9.5, height=4 )}
}




## ** Table H2

table = 'tab-h2-neuner2022the'
## 
ideo = 'ideo'
pop  = 'popatt'
tab = (
    res.neuner
    %>% unnest(summ)
    %>% filter(str_detect(term, pattern="rw")) 
    %>% select(ideo, !!pop, term, b=estimate, se=`std.error`)
    %>% mutate(thin = case_when(
                   str_detect(term, pattern=".*\\.rw.$") ~ "RW",
                   str_detect(term, pattern=".*\\.rw.p$") ~ "RWP"
               ),
               thick = case_when(
                   str_detect(term, pattern="wl") ~ 'Economic liberalism',
                   str_detect(term, pattern="rwi") ~ 'Deport refugees',
                   )
               ) 
    %>% select(-term)
    %>% nest(-ideo, -pop)
    %>% mutate(data = future_map(.x=data, function(data=.x)
        data
        %>% pivot_wider(id_cols=thick,
                        names_from=thin,
                        values_from=c(b, se)
                        )
        %>% mutate(
                perc.diff = 100*(b_RWP - b_RW)/abs(b_RW),
                t.diff = (b_RWP -b_RW)/sqrt(se_RWP^2 + se_RW^2),
                p.value = 1-pnorm(abs(t.diff))
            ))) 
    %>% unnest(data) 
    %>% arrange(desc(ideo),thick, popatt, p.value  ) 
    %>% rowid_to_column()
)
tab %>% print(., n=Inf, width=Inf) 
## 
row.number.bold = (
    tab 
    %>% filter(ideo=='Right-wing' & popatt=='High' &
               str_detect(thick, pattern="Econ")) 
    %>% pull(rowid)
)
caption=glue("\\label{{{table}}}Statistics from Figure \\ref{{fig-h2-neuner2022the}}",
             " ordered by p-value") 
caption
tabl = (
    tab       
    %>% select(pop=!!pop, ideo=!!ideo, everything())
    %>% mutate(
            across(where(is.numeric), round, 4),
            b_RW = glue("{b_RW} ({se_RW})"),
            b_RWP = glue("{b_RWP} ({se_RWP})"),
            )
    %>% select(
            "Voters' Ideology" = ideo,
            "Populist Attitudes" = pop,
            "Candidate Attribute"=thick,
            "$\\beta_{RW}$" = b_RW,
            "$\\beta_{RWP}$" = b_RWP,
            't-stat'=t.diff,
            'p-value'=p.value
        )
    %>% kable(., "latex", booktabs = T, caption=caption,
              escape=F, align=c("l", rep("c", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL)
    %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
                      position = "center", font_size=NULL) 
    %>% row_spec(row.number.bold, bold=T, hline_after = F)
)
tabl
## 
if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_SM_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}


## * --------  silva2023populism
## * loading

fn = file.path('~/Dropbox/CienciasSociais/data/replication/silva2023populism/rep/',
               'castanho_silva_et_al_replication_dataset.csv')
dfa = read.csv(fn, stringsAsFactors = T) %>% as_tibble() %>% rename(ptyid=party_id)  

## * recoding


## ## ppl
## Pop1. I would rather be represented by an ordinary citizen than by a specialized politician.
## Pop3. The politicians in the U.S. Congress need to follow the will of the people.
## Pop5. The people, and not politicians, should make our most important political decisions.
## ## ant
## Pop6. Elected officials talk too much and take too little action.
## Pop4. What people call “compromise” in politics is really just selling out on one’s principles.
## Pop8. Political parties only want peoples’ votes, they do not care about their opinions.
## ## man
## Pop2. The political diff b2 the elite and the people are greater than the differences among the people.
## Pop7. The people are often in agreement but the politicians pursue quite different goals.

df.silva=dfa
## populist attitudes
pop <- (
    df.silva
    %>% select("id", "thin_pop1", "thin_pop2",
               "thin_pop3", "thin_pop4", "thin_pop5",
               "thin_pop6", "thin_pop7", "thin_pop8")
    %>% distinct(id, .keep_all = T)  
    %>% rename_all(list(~str_replace(string=., pattern="thin_p", replacement="P")))
)
pop
fa.pop <- fa(pop[,2:9], nfactors = 1, scores = "regression", cor = "poly")
scores <- data_frame(id = pop$id, scores = c(fa.pop$scores))
## 
df.silva = (
    df.silva
    %>% rename_all(list(~str_replace(string=., pattern="thin_p", replacement="P")))
    %>% drop_na(matches("^Pop[0-9]")) 
    %>% select(-matches('scores'))
    %>% left_join(., scores, by=c('id')) 
    %>% mutate(
            pop.silva   = scores,
            pop.silva2c = case_when(scores >= 0 ~ 'High',
                                    scores  < 0 ~ 'Low') %>%
                factor(., levels=  c('Low', 'High'),labels = c('Low', 'High')),
            ##
            pplscore =  -1*prcomp(across(c(Pop1, Pop3, Pop5)), scale=T, center=T)$x[,1],
            antscore =  -1*prcomp(across(c(Pop6, Pop4, Pop8)), scale=T, center=T)$x[,1],
            manscore =  -1*prcomp(across(c(Pop2, Pop7)), scale=T, center=T)$x[,1],
            antscore01 = (antscore - min(antscore)) / (max(antscore) - min(antscore)),
            manscore01 = (manscore - min(manscore)) / (max(manscore) - min(manscore)),
            pplscore01 = (pplscore - min(pplscore)) / (max(pplscore) - min(pplscore)),
        )
    %>% rowwise(.)
    %>% mutate(
            popmin = min(pplscore, antscore, manscore),
            popadd = pplscore + antscore + manscore,
            popmul = pplscore01 * antscore01 * manscore01,
         )
    %>% ungroup(.)  
    %>% mutate(
            popmin2c = cut(popmin,
                           breaks = quantile(popmin, probs= seq(from=0, to=1, length=3)),
                           labels = c('Low', 'High'),
                           include.lowest=TRUE),
        ) 
    ## other variables
    ## ---------------
    %>% mutate_at(vars(matches("^[BC][0-9]_")),
                  list(~str_replace(string=., pattern="^No priority[0-9]+$", replacement="0"))) 
    %>% mutate_at(vars(matches("^[BC][0-9]_")),
                  list(~str_replace(string=., pattern="^Priority[0-9]+$", replacement="1")))   
    %>% mutate_at(vars(matches("^[BC][0-9]_")), list(~as.numeric(.)))
    %>% mutate(
               cand.rwi = case_when(
                   A1_immigration == "Is for greatly decreasing the number of legal immigrants"  ~1,
                   A1_immigration == "Is for somewhat decreasing the number of legal immigrants" ~1,
                   A1_immigration == "Is for somewhat increasing the number of legal immigrants" ~0,
                   A1_immigration == "Is for greatly increasing the number of legal immigrants"  ~0),
               cand.rwl = case_when(
                   A3_redistribution == "Is for much higher taxes on the rich"     ~ 0,
                   A3_redistribution == "Is for somewhat higher taxes on the rich" ~ 0,
                   A3_redistribution == "Is for somewhat lower taxes on the rich"  ~ 1,
                   A3_redistribution == "Is for much lower taxes on the rich"      ~ 1),
               cand.ppl = case_when(B5_citizens==1 | B4_democracy ==1 ~ 1,
                                    TRUE                              ~ 0),
               cand.ant = case_when(B3_elite ==1 | B2_parties == 1  ~ 1,
                                    TRUE                           ~ 0),
               cand.pop  = cand.ppl * cand.ant,
               cand.rwip = cand.pop * cand.rwi,
               cand.rwlp = cand.pop * cand.rwl,
               )  
    %>% glimpse()
)


## * overview

## df %>% look_for(".", details=F)
## df %>% sjmisc::frq(ptyid)
## df %>% expss::cross_cases(ptyid, popatt)

## describeBy(df$scores, group=df$popatt, mat=T, fast=T)
## boxplot(scores~popatt, data=df)

## tab = df %>% select(matches('^pop|^ppl|^man|^ant|silva$')) %>% distinct(., .keep_all=TRUE) 
## tab %>% names
## GGally::ggpairs(tab)


## tab = df %>% select(matches('^pop[0-9]$'), popmin, popmin2c, pop.silva, pop.silva2c)  %>% distinct(., .keep_all=TRUE) 
## tab %>% names
## GGally::ggpairs(tab)


## * extension
## ** estimation

popatt = 'pop.silva2c'
popatt = 'popmin2c'
ideo   = 'ptyid'
y      = 'votefor'
res.silva = (
    df.silva
    %>% filter(att1==3) 
    %>% select(id, y=!!y, popatt=!!popatt, ideo=!!ideo, contains("cand."))
    %>% filter(ideo!='Independent')  
    %>% drop_na(popatt, ideo) 
    %>% glimpse()
    ## 
    %>% nest(-popatt, -ideo)  
    %>% mutate(
            formula = glue("y ~ cand.rwl + cand.rwlp + cand.rwi + cand.rwip | id"),
            fit = future_map2(.x=formula, .y=data, function(.x, .y) 
                feols(formula(.x), cluster=~id, data=.y)) ,
            summ = future_map(.x=fit, function(.x) tidy(.x, conf.int=TRUE)),
        ) 
    %>% glimpse()
)
## 

## ** Figure H3

figure = 'fig-h3-silva2023populism'
## 
labels = c(
    cand.rwi  = "Anti-(legal)\nimmigrants\n(RW)",
    cand.rwip = "Anti-(legal)\nimmigrants\n(RWP)",
    ## 
    cand.rwl  = "Economic policy\n(RW)",
    cand.rwlp = "Economic policy\n(RWP)"
)
tab = (
    res.silva
    %>% select(-data) 
    %>% unnest(summ) 
    %>% mutate(term = factor(term, names(labels), labels)) 
    %>% glimpse()
)
## 
## 
## tab
x = "term"
y = "estimate"
color    = NULL
fill     = 'popatt'
facet1   = 'ideo'
facet2   = NULL
leg      = 'Populist Attitudes'
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .1
ylab = glue("Average Marginal Effect of Candidate\n",
            "Attributes on Voters' Support for the Candidate")
xlab     = NULL
dodge=.3
g = (
    tab
    %>% ggplot(.)
    + geom_line(aes_string(x=x, y=y, group=fill, color=fill),
                linetype=2,
                show.legend = F,
                position=position_dodge(dodge),
                data= . %>% filter(str_detect(term, pattern="immig")) )
    + geom_line(aes_string(x=x, y=y, group=fill, color=fill),
                linetype=2,
                show.legend = F,
                position=position_dodge(dodge),
                data= . %>% filter(str_detect(term, pattern=".conomic")) )
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill), position=position_dodge(dodge))
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
    + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high", color=fill), width=.05,
                  , position=position_dodge(dodge)) 
    ## 
    + scale_x_discrete(labels = scales::wrap_format(10))
    + scale_shape_manual(values=c(25,24))
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_color_grey(start = 0.7, end = 0,  na.value="red") 
    ## 
    + facet_wrap(glue("~ {facet1}"), ncol = , scales='free_x')
    ## 
    + ggguides()
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
)
g

if (SAVE) {
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_SM_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=9.5, height=4 )}
}

## ** Table H3

table = 'tab-h3-silva2023populism'
## 
ideo = 'ideo'
pop  = 'popatt'
tab = (
    res.silva
    %>% unnest(summ)
    %>% filter(str_detect(term, pattern="rw")) 
    %>% select(ideo, !!pop, term, b=estimate, se=`std.error`)
    %>% mutate(thin = case_when(
                   str_detect(term, pattern=".*\\.rw.$") ~ "RW",
                   str_detect(term, pattern=".*\\.rw.p$") ~ "RWP"
               ),
               thick = case_when(
                   str_detect(term, pattern="wl") ~ 'Economic liberalism',
                   str_detect(term, pattern="rwi") ~ 'Anti-immigration',
                   )
               ) 
    %>% select(-term)
    %>% nest(-ideo, -pop)
    %>% mutate(data = future_map(.x=data, function(data=.x)
        data
        %>% pivot_wider(id_cols=thick,
                        names_from=thin,
                        values_from=c(b, se)
                        )
        %>% mutate(
                perc.diff = 100*(b_RWP - b_RW)/abs(b_RW),
                t.diff = (b_RWP -b_RW)/sqrt(se_RWP^2 + se_RW^2),
                p.value = 1-pnorm(abs(t.diff))
            ))) 
    %>% unnest(data) 
    %>% arrange(desc(ideo),thick, popatt, p.value  )  
    %>% rowid_to_column()
)
tab %>% print(., n=Inf, width=Inf) 
## 
row.number.bold = (
    tab 
    %>% filter(ideo=='Republican' & popatt=='High' &
               str_detect(thick, pattern="Econ")) 
    %>% pull(rowid)
)
caption=glue("\\label{{{table}}}Statistics from Figure \\ref{{fig-h3-silva2023populism}}",
             " ordered by p-value") 
caption
tabl = (
    tab       
    %>% select(pop=!!pop, ideo=!!ideo, everything())
    %>% mutate(
            across(where(is.numeric), round, 4),
            b_RW = glue("{b_RW} ({se_RW})"),
            b_RWP = glue("{b_RWP} ({se_RWP})"),
            )
    %>% select(
            "Voters' Ideology" = ideo,
            "Populist Attitudes" = pop,
            "Candidate Attribute"=thick,
            "$\\beta_{RW}$" = b_RW,
            "$\\beta_{RWP}$" = b_RWP,
            't-stat'=t.diff,
            'p-value'=p.value
        )
    %>% kable(., "latex", booktabs = T, caption=caption,
              escape=F, align=c("l", rep("c", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL)
    %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
                      position = "center", font_size=NULL)  
    %>% row_spec(row.number.bold, bold=T, hline_after = F)
)
tabl
## 
if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_SM_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}


