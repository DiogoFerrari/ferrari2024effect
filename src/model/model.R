library(tidyverse)
library(ggh4x)
library(writexl)
library(sjmisc)
library(magrittr)
library(glue)
library(furrr)
library(labelled)
library(gtsummary)
library(stringr)
library(patchwork)
library(grid)
library(ggpubr)
library(ggtext)
library(ggbrace)
library(modelsummary)
library(fixest)
library(kableExtra)
library(readxl)
## 
library(broom)
##
source("../__paths__.R")
source("../__constants__.R")
##
SAVE=FALSE
SAVE=TRUE

## * function

create_formula <- function(v)
{
    v = paste(paste("`", v, '`', sep='') , collapse=' + ')
    return(v)
}
create_formula_interaction <- function(v, interaction)
{
    v = paste("`", v, '`', sep='')
    v = glue("{v}*{interaction}") 
    v = paste(v, collapse=' + ')
    return(v)
}

get_pos <- function(tab, pos='none', rw)
{
    
    cases = (
        tab 
        %>% filter(str_detect(term, pattern=rw))
        %>% filter(str_detect(thick_value, pattern="Right")) 
        %>% mutate(
                thick_value = factor(thick_value, levels=c("Left-wing voters",
                                                           'Centrist voters',
                                                           'Right-wing voters')),
                )
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

get_data <- function(df, y, thin=NULL, thin_value=NULL,
                     thick=NULL, thick_value=NULL,
                     candprofile, covars)
{
    res = (
        df
        %>% select(y, thin=thin, thick=thick, candprofile, rid)
        %>% filter(thin==thin_value) 
        %>% filter(thick==thick_value) 
    )
    if (covars!='1') {
        res = res %>% bind_cols(., df %>% select(covars)) 
    }
    return(res)
}
get_data_subgroup <- function(df, y, subgroup=NULL, subgroup_value=NULL,
                              candprofile, covars, other.covars=NULL)
{
    res = (
        df
        %>% select(y, subgroup=subgroup,  candprofile, rid, other.covars)
        %>% filter(subgroup==subgroup_value) 
        %>% select(-subgroup)
    )
    if (covars!='1') {
        res = res %>% bind_cols(., df %>% select(covars)) 
    }
    return(res)
}

## * loading

fn = file.path(PATH_DATA_FINAL,"survey.csv")
df = read.csv(file=fn, sep=";", check.names=F) %>% as_tibble(.)

## * Estimation
## ** Pooled, thick, thin

glue("Estimating pooled and by thick and thin  separately...") 
ideo = 'ideo'
libcons = 'libcons'
pop = 'popmul2c'
tab = df
res.sep = (
    tibble(case = c(ideo, libcons, pop)) 
    %>% crossing(covars=c('1'))
    %>% rowwise(.)
    %>% mutate(case_value = list(tab %>% pull(case) %>% unique(.)))
    %>% unnest(case_value)
    %>% drop_na(.) 
    %>% rowwise(.)
    %>% mutate(data = list(get_data_subgroup(tab,
                                             y=DEPVAR, subgroup=case, 
                                             subgroup_value=case_value,
                                             candprofile=names(ATT), covars=c(covars))))
    %>% bind_rows(tibble(case='pooled', covars='1', case_value='Pooled',
                         data=list(tab %>% select(DEPVAR, names(ATT), 'rid'))) )
    %>% ungroup(.) 
    %>% mutate(
            formula = glue("{DEPVAR} ~ {create_formula(names(ATT))} | rid  "),
            est = purrr::map2(.x=formula, .y=data, function(.x, .y)
                feols(formula(.x), cluster=~rid, data=.y)),
            summ = furrr::future_map(.x=est, function(.x) 
                tidy(.x, conf.int=TRUE)),
            )
)
## res.sep %>% glimpse()

## ** By thick x thin 

glue("Estimating by thick and thin combined...") 

y           = 'chr'
thin        = c('popmul2c', 'popmin2c', 'popadd2c')
thick       = c('libcons', 'ideo', 'ptyid')
candprofile = names(ATT)
covars      = c('1')
tab         = df
## 
res.comb = (
    tibble(y) 
    %>% crossing(thin) 
    %>% crossing(thick)
    %>% crossing(covars) 
    %>% crossing(tasks = list(full=1:15, first=1:7, second=8:15)) 
    %>% rowwise(.)
    %>% mutate(
            ntasks = length(tasks),
            tasks_case = case_when(
                ntasks==15 ~ 'All tasks',
                ntasks< 15 & max(tasks) < 15~ 'First half',
                ntasks< 15 & max(tasks) ==15~ 'Second half',
            ),
            thin_value = list(tab %>% dplyr::pull(thin) %>% unique(.) ),
            thick_value = list(tab %>% dplyr::pull(thick) %>% unique(.) )
        )
    %>% unnest(thin_value)
    %>% unnest(thick_value)
    %>% drop_na() 
    %>% rowwise(.)
    %>% mutate(data=list(get_data(tab, y, thin, thin_value, thick,
                                  thick_value,
                                  c(candprofile, 'task'),
                                  covars) %>%
                         rename_at(vars(names(candprofile)),
                                   ~paste(candprofile)) %>% 
                         filter(task %in% tasks) 
                         )
               )
    %>% ungroup(.)
    %>% mutate(
            formula = glue("{y} ~ {create_formula(candprofile)} + {covars} | rid"),
            est = purrr::map2(.x=formula, .y=data, function(.x, .y) 
                feols(formula(.x), cluster=~rid, data=.y)),
            summ = furrr::future_map(.x=est, function(.x) 
                tidy(.x, conf.int=TRUE)),
            glance = furrr::future_map(.x=est, function(.x)
                glance(.x)),
            )
)

## ** RW vs RWP
## *** Raw effects

print("Estimating RW vs RWP...")


y           = 'chr'
thin        = c('popmul2c', 'popmin2c', 'popadd2c')
thick       = c('libcons', 'ideo', 'ptyid')
candprofile = c('RWP (economic liberalism)',
                'RWP (conservatism)'       ,
                'RWP (anti-immigration)')
covars      = c('1')
## 
res.rwp = (
    tibble(y) 
    %>% crossing(thin) 
    %>% crossing(thick)
    %>% crossing(candprofile) 
    %>% crossing(covars) 
    %>% crossing(tasks = list(full=1:15, first=1:7, second=8:15)) 
    %>% rowwise(.)
    %>% mutate(
            ntasks = length(tasks),
            tasks_case = case_when(
                ntasks==15 ~ 'All tasks',
                ntasks< 15 & max(tasks) < 15~ 'First half',
                ntasks< 15 & max(tasks) ==15~ 'Second half',
            ),
            thin_value = list(df %>% dplyr::pull(thin) %>% unique(.) ),
            thick_value = list(df %>% dplyr::pull(thick) %>% unique(.) )
        )
    %>% unnest(thin_value)
    %>% unnest(thick_value)
    %>% drop_na() 
    %>% rowwise(.)
    %>% mutate(data=list(get_data(df, y, thin, thin_value, thick,
                                  thick_value,
                                  c(candprofile, 'task'),
                                  covars) %>% 
                         filter(task %in% tasks) 
                         )
               )
    %>% ungroup(.)
    %>% mutate(
            formula = glue("{y} ~ `{candprofile}` + {covars} | rid"),
            est = purrr::map2(.x=formula, .y=data, function(.x, .y) 
                feols(formula(.x), cluster=~rid, data=.y)),
            summ = furrr::future_map(.x=est, function(.x) 
                tidy(.x, conf.int=TRUE)),
            glance = furrr::future_map(.x=est, function(.x)
                glance(.x)),
        )
)

## *** Effect difference RW vs RWP (populist added effect)

thin = 'popmul2c'
thick = 'ideo'
## 
res.diff = (
    res.rwp  
    %>% nest(-tasks_case) 
    %>% mutate(data = future_map(.x=data, function(.x)
        .x
        %>% filter(thin==!!thin & thick==!!thick) 
        %>% unnest(summ)
        %>% filter(!str_detect(term, pattern="Interc")) 
        %>% filter(!str_detect(term, pattern="LW")) 
        %>% mutate(term = stringr::str_replace_all(string=term,
                                                   pattern='`RWP.*`',
                                                   replacement=""),
                   term = factor(term, levels= CAT_ORDER[['candidates']]),
                   ## 
                   thick_value = glue("{thick_value} voters"),
                   thick_value = str_replace(string=thick_value,
                                             pattern="Center",
                                             replacement="Centrist"),
                   thick_value = factor(thick_value, levels=c("Left-wing voters",
                                                              'Centrist voters',
                                                              'Right-wing voters')),
                   ## 
                   thin_value = factor(thin_value, levels=c('Low', 'High')),
                   )
        ),
        data = future_map(.x=data, function(.x)
            .x
            %>% filter(thick_value=='Right-wing voters') 
            %>% filter(thin_value=='High') 
            %>% select(thin, thick, term, b=estimate, se=`std.error`)
            %>% mutate(thin = case_when(str_detect(term, pattern="RWP") ~ 'RWP',
                                        T ~ 'RW'),
                       thick = stringr::str_replace(string=term, pattern="RW[ |P]",
                                                    replacement="") %>% str_trim(.),
                       )
            %>% tidyr::pivot_wider(id_cols=thick,
                                   names_from=thin,
                                   values_from=c(b, se)
                                   )
            %>% mutate(
                    perc.diff = (b_RWP - b_RW)/abs(b_RW),
                    t.diff = (b_RWP -b_RW)/sqrt(se_RWP^2 + se_RW^2),
                    p.diff = 1-pnorm(t.diff)
                )
          , .progress=TRUE)
        )
    %>% unnest(data)
)
res.diff   %>% print(., n=Inf, width=Inf) 

## * Figures
## ** Figure 1 (pooled, by thick, by thin separately)

figure = 'fig-1'
## 
tab = (
    res.sep
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Intercep")) 
    %>% mutate(term=str_replace_all(string=term, pattern="`|2", replacement=""),
               term=factor(term, levels=names(ATT), labels=ATT))
)
tab
## plot
## ----
x = "estimate"
y = "term"
color=NULL
fill="case_value"
facet1='case'
facet2=NULL
dodge=.3
## Pooled
## ------
leg_title = 'Polled'
gpooled = (
    tab
    %>% filter(case=='pooled') 
    %>% ggplot(.)
    + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high"),
                     height=.1, 
                     position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill), position = position_dodge(dodge))
    + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
    + scale_y_discrete(limits=rev)
    + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_fill_grey(start = 0., end = 0,  na.value="red") 
    + scale_shape_manual(values=c( 25, 23, 24))
    + labs(
          x        = NULL,
          y        = NULL,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + ggguides(ncol=10)
    + theme(panel.border       = element_rect(colour="black", fill='transparent'))
)
## thick ideology
## --------------
leg_title = 'Voter Self-Identification'
gthick = (
    tab
    %>% filter(case==!!ideo) 
    %>% mutate(case_value=factor(case_value, levels=c('Left-wing',
                                                      'Center',
                                                      'Right-wing')))
    %>% ggplot(.)
    + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high",
                                color=fill), height=.3,
                     position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill), position = position_dodge(dodge))
    + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
    + scale_y_discrete(limits=rev)
    + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_shape_manual(values=c( 25, 23, 24))
    + labs(
          x        = NULL,
          y        = NULL,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + ggguides(ncol=1)
    + theme(axis.text.y = element_blank(),
          panel.border       = element_rect(colour="black", fill='transparent'))
)
## thick ideology
## --------------
leg_title = 'Populist Attitudes'
gthin = (
    tab
    %>% filter(case==!!pop) 
    %>% mutate(case_value=factor(case_value, levels=c('Low', 'High')))
    %>% ggplot(.)
    + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high",
                                color=fill), height=.3,
                     position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill), position = position_dodge(dodge))
    + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
    + scale_y_discrete(limits=rev)
    + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_shape_manual(values=c( 25,  24))
    + labs(
          x        = NULL,
          y        = NULL,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + ggguides(ncol=2)
    + theme(axis.text.y = element_blank(),
            panel.border       = element_rect(colour="black", fill='transparent'),
            )
)
## 
xlab = glue("Average Marginal Effect of Candidate ",
            "Attributes on Voter's Support for the Candidate")
xlab=text_grob(xlab, size = 11.5, face = "bold", hjust=.4, vjust=.1)   #### this worked for me
ylab = text_grob("Candidates' Attributes", face='bold', rot=90, vjust=1)
g = gpooled + gthick + gthin + plot_annotation()
g = gridExtra::grid.arrange(patchwork::patchworkGrob(g),
                            left = ylab, bottom = xlab)
g


if (SAVE) {
    print(glue("Saving {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=8.5, height=4.5 )}
}


## ** Figure 2 (thick and thin combined)

figure = 'fig-2'
## 
tab = (
    res.comb  
    %>% filter(ntasks==15) 
    %>% filter(thin=='popmul2c' & thick=='ideo') 
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Intercep")) 
    %>% mutate(term=str_replace_all(string=term, pattern="`|2", replacement=""),
               term=factor(term, levels=names(ATT), labels=ATT),
               ## 
               thick_value = glue("{thick_value} voters"),
               thick_value = str_replace(string=thick_value,
                                         pattern="Center",
                                         replacement="Centrist"),
               thick_value = factor(thick_value, levels=c("Left-wing voters",
                                                          'Centrist voters',
                                                          'Right-wing voters')),
               thin_value = factor(thin_value, levels=c('Low', 'High'))
               )
)
tab
## 
## plot
## ----
x = "estimate"
y = "term"
color="thin_value"
fill="thin_value"
shape="thin_value"
facet1='thick_value'
facet2=NULL
dodge=.3
xlab = glue("Average Marginal Effect of Candidate ",
            "Attributes on Voter's Support for the Candidate")
ylab = "Candidates' Attributes"
leg_title = "Voters' Populist attitudes"
g = (
    tab
    %>% ggplot(.)
    + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high",
                                color=fill),
                     height=.1, 
                     position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=shape), position = position_dodge(dodge))
    + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
    + scale_y_discrete(limits=rev)
    + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_shape_manual(values=c( 25, 24))
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + facet_wrap( paste("~", facet1))  
    + ggguides(ncol=2)
    + theme(panel.border = element_rect(colour="black", fill='transparent'))
)
g

if (SAVE) {
    print(glue("Saving {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=8.5, height=4.5 )}
}

## ** Figure 3 (RW vs RWP support)


figure = 'fig-3'
## 
thin = 'popmul2c'
thick = 'ideo'
tab = (
    res.rwp
    %>% filter(ntasks==15) 
    %>% filter(thin==!!thin & thick==!!thick) 
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Interc")) 
    %>% filter(!str_detect(term, pattern="LW")) 
    %>% mutate(term = stringr::str_replace_all(string=term,
                                               pattern='`RWP.*`',
                                               replacement=""),
               term = factor(term, levels= CAT_ORDER[['candidates']]),
               ## 
               thick_value = glue("{thick_value} voters"),
               thick_value = str_replace(string=thick_value,
                                         pattern="Center",
                                         replacement="Centrist"),
               thick_value = factor(thick_value, levels=c("Left-wing voters",
                                                          'Centrist voters',
                                                          'Right-wing voters')),
               ## 
               thin_value = factor(thin_value, levels=c('Low', 'High')),
               )
)
tab 
## plot
## ----
x = "term"
y = "estimate"
color="thin_value"
fill="thin_value"
shape="thin_value"
facet1="thick_value"
facet2=NULL
dodge=.3
leg_title='Populist Attitudes'
ylab = glue("Average Marginal Effect of Candidate\n",
            "Attributes on Voters' Support for the Candidate")
g = (
    tab 
    %>% ggplot(.)
    + geom_rect(aes(xmin=0.5, xmax=2.5, ymin=-Inf, ymax=Inf),
                fill="grey", alpha=0.025, inherit_aes = F)
    + geom_rect(aes(xmin=4.5, xmax=6.5, ymin=-Inf, ymax=Inf),
                fill="grey", alpha=0.025, inherit_aes = F)
    + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high",
                               color=fill),
                    width=.05, position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=shape),
                 position=position_dodge(dodge))
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
    + scale_x_discrete(labels = c('<i>RW', '<b>RWP',
                                  '<i>RW', '<b>RWP',
                                  '<i>RW', '<b>RWP'))
    + scale_shape_manual(values=c(25,24))
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_color_grey(start = 0.7, end = 0,  na.value="red") 
    + facet_wrap( paste("~", facet1), ncol = , scales='free')  
    + ggguides(ncol=2)
    ## 
    + geom_text(aes_string(x=1.5, y="-Inf", label="'Anti-\nImmigration'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                ) 
    + geom_text(aes_string(x=3.5, y="-Inf", label="'Social\nconservative'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                ) 
    + geom_text(aes_string(x=5.5, y="-Inf", label="'Econonic\nliberal'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                )
    + labs(
          x        = NULL,
          y        = ylab,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + theme(axis.text.x = element_markdown())
)
## g
df %>% glimpse()
df_placeholder = tibble(
    thick_value=factor('Right-wing voters',
                       levels=c('Left-wing voters',
                                'Centrist voters',
                                'Right-wing voters'))) 
gi = (
    g
    +  geom_bracket(
           xmin       = get_pos(tab, pos='x', rw='anti-immigration')[1],
           xmax       = get_pos(tab, pos='x', rw='anti-immigration')[2],
           y.position = get_pos(tab, pos='y', rw='anti-immigration'),
           label = "(a)",
           type = "text",
           label.size=4,
           bracket.nudge.y=0,
           size=.3,
           clip='off',
           linetype=2,
           vjust=-.5,
           tip.length = c(0.37, 0.02),
           data = df_placeholder
       )
    +  geom_bracket(
           xmin       = get_pos(tab, pos='x', rw='social conservative')[1],
           xmax       = get_pos(tab, pos='x', rw='social conservative')[2],
           y.position = get_pos(tab, pos='y', rw='social conservative'),
           label = "(b)",
           type = "text",
           label.size=4,
           bracket.nudge.y=0,
           size=.3,
           clip='off',
           linetype=2,
           vjust=-.5,
           tip.length = c(0.27, 0.02),
           data = df_placeholder
       )
    +  geom_bracket(
           xmin       = get_pos(tab, pos='x', rw='economic liberal')[1],
           xmax       = get_pos(tab, pos='x', rw='economic liberal')[2],
           y.position = get_pos(tab, pos='y', rw='economic liberal'),
           label = "(c)",
           type = "text",
           label.size=4,
           bracket.nudge.y=0,
           size=.3,
           clip='off',
           linetype=2,
           vjust=1.7,
           tip.length = c(0.23, 0.02),
           data = df_placeholder
       )
)
gi
## 
if (SAVE)
{
    print(glue("Saving {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(gi, filename=fn, width=9.5, height=4 )}
}

## Difference on anti-immigration support
## --------------------------------------
tab = (
    res.rwp
    %>% filter(ntasks==15) 
    %>% filter(thin=='popmul2c') 
    %>% filter(thin_value=='Low') 
    %>% filter(thick=='ideo') 
    %>% filter(!str_detect(thick_value, pattern="Center")) 
    %>% filter(str_detect(candprofile, pattern="anti-")) 
    %>% unnest(summ)
    %>% filter(str_detect(term, pattern="anti-")) 
    %>% filter(str_detect(term, pattern="RW ")) 
    %>% select(term, thin_value, thick_value, estimate)
    %>% tidyr::pivot_wider(id_cols=term,
                           names_from=thick_value,
                           values_from=estimate)
    %>% mutate(ratio_left_right = `Left-wing`/`Right-wing`)
)
tab

## Percentage of right-wing populists
## ---------------------------------
(
    df
    %>% distinct(rid, .keep_all=TRUE) 
    %>% mutate(rwp_voters = case_when(
                   popmul2c=='High' & ideo=='Right-wing' ~ 'Right-wing populist voters',
                   T ~ 'Others',
               ))
    %>% sjmisc::frq(rwp_voters)
)

## * Supplementary Material
## ** Estimation
## *** LW vs LWP support

print("Estimating LW and LWP support...")

y           = 'chr'
thin        = c('popmul2c', 'popmin2c', 'popadd2c')
thick       = c('libcons', 'ideo', 'ptyid')
candprofile = c('RWP (economic liberalism)',
                'RWP (conservatism)'       ,
                'RWP (anti-immigration)')
covars      = c('1')
## reset reference level
df.lwp = df %>% mutate(
                    `RWP (economic liberalism)` = as.factor(`RWP (economic liberalism)`),
                    `RWP (economic liberalism)` = relevel(`RWP (economic liberalism)`,
                                                          'RW economic liberalism'),
                    ## 
                    `RWP (conservatism)` = as.factor(`RWP (conservatism)`),
                    `RWP (conservatism)` = relevel(`RWP (conservatism)`,
                                                   'RW social conservative'),
                    ## 
                    `RWP (anti-immigration)` = as.factor(`RWP (anti-immigration)`),
                    `RWP (anti-immigration)` = relevel(`RWP (anti-immigration)`,
                                                       'RW anti-immigration'),
                    )
## 
res.lwp = (
    tibble(y) 
    %>% crossing(thin) 
    %>% crossing(thick)
    %>% crossing(candprofile) 
    %>% crossing(covars) 
    %>% rowwise(.)
    %>% mutate(
            thin_value = list(df.lwp %>% dplyr::pull(thin) %>% unique(.) ),
            thick_value = list(df.lwp %>% dplyr::pull(thick) %>% unique(.) )
        )
    %>% unnest(thin_value)
    %>% unnest(thick_value)
    %>% drop_na() 
    %>% rowwise(.)
    %>% mutate(data=list(get_data(df.lwp,
                                  y, thin, thin_value, thick,
                                  thick_value, candprofile, covars)))
    %>% ungroup(.)
    %>% mutate(
            formula = glue("{y} ~ `{candprofile}` + {covars}"),
            est = purrr::map2(.x=formula, .y=data, function(.x, .y) 
                lm(.x, data=.y)),
            summ = furrr::future_map(.x=est, function(.x) 
                tidy(.x, conf.int=TRUE)),
            glance = furrr::future_map(.x=est, function(.x)
                glance(.x)),
        )
)
## res.lwp


## *** Robustness to attention check exclusion
## **** Full data

print('Loading removed data (due to attention checks)...')

## get data
## --------
fn = file.path(PATH_DATA_FINAL, "survey-removed.csv")
dfremoved = read_delim(file=fn, delim=";")
dfmerged = df %>% bind_rows(dfremoved)
## 

## **** Pooled, thick, thin (full data)

print('Reestinating with full data (Figure 1) ....')

attchk = "attchkcj_keep"
ideo = 'ideo'
libcons = 'libcons'
pop = 'popmul2c'
res.sep.full = (
    tibble(case = c(ideo, libcons, pop)) 
    %>% crossing(covars=c('1'))
    %>% rowwise(.)
    %>% mutate(case_value = list(df %>% pull(case) %>% unique(.)))
    %>% unnest(case_value)
    %>% drop_na(.) 
    %>% rowwise(.)
    %>% mutate(data = list(get_data_subgroup(dfmerged,
                                             y=DEPVAR, subgroup=case, 
                                             subgroup_value=case_value,
                                             candprofile=names(ATT),
                                             covars=c(covars),
                                             other.covars = attchk
                                             )))
    %>% bind_rows(tibble(case='pooled', covars='1', case_value='Pooled',
                         data=list(dfmerged %>% select(DEPVAR, names(ATT), 'rid', attchk))) )
    %>% ungroup(.) 
    %>% mutate(
            formula = glue("{DEPVAR} ~ {create_formula(names(ATT))} | rid  "),
            est = purrr::map2(.x=formula, .y=data, function(.x, .y)
                feols(formula(.x), cluster=~rid, data=.y)),
            summ = furrr::future_map(.x=est, function(.x) 
                tidy(.x, conf.int=TRUE)),
            ## glance = furrr::future_map(.x=est, function(.x)
            ##     glance(.x)),
            ## pred = furrr::future_map(.x=est, function(.x)
            ##     augment(.x))
            )
)
res.sep.full

## **** By thick x thin  (full data)

print('Reestinating with full data (Figure 2) ....')

## 
y           = 'chr'
thin        = c('popmul2c', 'popmin2c', 'popadd2c')
thick       = c('libcons', 'ideo', 'ptyid')
candprofile = names(ATT)
covars      = c('1')
## 
res.comb.full = (
    tibble(y) 
    %>% crossing(thin) 
    %>% crossing(thick)
    %>% crossing(covars) 
    %>% rowwise(.)
    %>% mutate(
            thin_value = list(df %>% dplyr::pull(thin) %>% unique(.) ),
            thick_value = list(df %>% dplyr::pull(thick) %>% unique(.) ))
    %>% unnest(thin_value)
    %>% unnest(thick_value)
    %>% drop_na() 
    %>% rowwise(.)
    %>% mutate(data=list(get_data(dfmerged,
                                  y, thin, thin_value, thick,
                                  thick_value, candprofile, covars) %>%
                         rename_at(vars(names(candprofile)),
                                   ~paste(candprofile))))
    %>% ungroup(.)
    %>% mutate(
            formula = glue("{y} ~ {create_formula(candprofile)} + {covars} | rid"),
            est = purrr::map2(.x=formula, .y=data, function(.x, .y) 
                feols(formula(.x), cluster=~rid, data=.y)),
            summ = furrr::future_map(.x=est, function(.x) 
                tidy(.x, conf.int=TRUE)),
            glance = furrr::future_map(.x=est, function(.x)
                glance(.x)),
            )
)
res.comb.full

## **** RW vs RWP (full data)

print('Reestinating with full data (Figure 3) ....')

y           = 'chr'
thin        = c('popmul2c', 'popmin2c', 'popadd2c')
thick       = c('libcons', 'ideo', 'ptyid')
candprofile = c('RWP (economic liberalism)',
                'RWP (conservatism)'       ,
                'RWP (anti-immigration)')
covars      = c('1')
## 
res.rwp.full = (
    tibble(y) 
    %>% crossing(thin) 
    %>% crossing(thick)
    %>% crossing(candprofile) 
    %>% crossing(covars) 
    %>% rowwise(.)
    %>% mutate(
            thin_value = list(df %>% dplyr::pull(thin) %>% unique(.) ),
            thick_value = list(df %>% dplyr::pull(thick) %>% unique(.) )
        )
    %>% unnest(thin_value)
    %>% unnest(thick_value)
    %>% drop_na() 
    %>% rowwise(.)
    %>% mutate(data=list(get_data(dfmerged, y, thin, thin_value, thick,
                                  thick_value, candprofile, covars)))
    %>% ungroup(.)
    %>% mutate(
            formula = glue("{y} ~ `{candprofile}` + {covars} | rid"),
            est = purrr::map2(.x=formula, .y=data, function(.x, .y) 
                feols(formula(.x), cluster=~rid, data=.y)),
            summ = furrr::future_map(.x=est, function(.x) 
                tidy(.x, conf.int=TRUE)),
            glance = furrr::future_map(.x=est, function(.x)
                glance(.x)),
        )
)
res.rwp.full

## *** Diagnostics
## **** Task and order effects (interaction)

print('Estimating diagnostics(interaction)...')

res.diag = (
    tibble(case = c('ideo', 'libcons', 'popmul2c')) 
    %>% crossing(covars=c('1'))
    %>% crossing(interaction=c('task', 'order')) 
    %>% crossing(tasks = list(full=1:15, first=1:7, second=8:15)) 
    %>% rowwise(.)
    %>% mutate(case_value = list(df %>% pull(case) %>% unique(.)))
    %>% unnest(case_value)
    %>% drop_na(.) 
    %>% rowwise(.)
    %>% mutate(data = list(get_data_subgroup(df,
                                             y=DEPVAR,
                                             subgroup=case, 
                                             subgroup_value=case_value,
                                             candprofile=c(names(ATT), 'task', 'order'),
                                             covars=covars
                                             )),
               )
    %>% bind_rows(
            tibble(case='pooled',
                   covars='1',
                   case_value='Pooled',
                   interaction=c('task', 'order')
                   ) 
            %>% crossing(tasks = list(full=1:15, first=1:7, second=8:15)) 
            %>% rowwise(.)
            %>% mutate(data=list(df %>% select(DEPVAR, names(ATT), 'rid', , 'task', 'order')))
        ) 
    %>% rowwise(.)
    %>% mutate(
            formula = glue("{DEPVAR}~{create_formula_interaction(names(ATT), interaction)} | rid  "),
            formula = as.character(formula),
            ntasks  = length(tasks),
            tasks_case = case_when(
                ntasks==15 ~ 'All tasks',
                ntasks< 15 & max(tasks) < 15~ 'First half',
                ntasks< 15 & max(tasks) ==15~ 'Second half',
            )
        )
    %>% ungroup(.)  
    %>% mutate(
            data = furrr::future_map2(.x=data, .y=tasks, function(.x, tasks=.y)
                .x 
                %>% filter(task %in% tasks)
                ),
            est = purrr::map2(.x=formula, .y=data, function(.x, .y)
                feols(formula(.x), cluster=~rid, data=.y)),
            summ = furrr::future_map(.x=est, function(.x) 
                tidy(.x, conf.int=TRUE)),
            glance = furrr::future_map(.x=est, function(.x)
                glance(.x)),
            )
)
## res.diag

## **** Task effects (task order; all, first, and second half)

print('Estimating diagnostics (all, first, and second half)...')

res.task.effect = (
    tibble(case = c('ideo', 'libcons', 'popmul2c')) 
    %>% crossing(covars=c('1'))
    %>% crossing(interaction=c('task', 'order')) 
    %>% crossing(tasks = list(full=1:15, first=1:7, second=8:15)) 
    %>% rowwise(.)
    %>% mutate(case_value = list(df %>% pull(case) %>% unique(.)))
    %>% unnest(case_value)
    %>% drop_na(.) 
    %>% rowwise(.)
    %>% mutate(data = list(get_data_subgroup(df,
                                             y=DEPVAR,
                                             subgroup=case, 
                                             subgroup_value=case_value,
                                             candprofile=c(names(ATT), 'task', 'order'),
                                             covars=covars
                                             )),
               )
    %>% bind_rows(
            tibble(case='pooled',
                   covars='1',
                   case_value='Pooled',
                   interaction=c('task', 'order'),
                   )  
            %>% crossing(tasks = list(full=1:15, first=1:7, second=8:15)) 
            %>% rowwise(.)
            %>% mutate(data=list(df %>% select(DEPVAR, names(ATT), 'rid',
                                               'task', 'order')))
        ) 
    %>% rowwise(.)
    %>% mutate(
            formula = glue("{DEPVAR}~{create_formula_interaction(names(ATT), interaction)} | rid  "),
            formula = as.character(formula),
            ntasks  = length(tasks),
            tasks_case = case_when(
                ntasks==15 ~ 'All tasks',
                ntasks< 15 & max(tasks) < 15~ 'First half',
                ntasks< 15 & max(tasks) ==15~ 'Second half',
            )
        )
    %>% ungroup(.)  
    %>% mutate(
            data = furrr::future_map2(.x=data, .y=tasks, function(.x, tasks=.y)
                .x 
                %>% filter(task %in% tasks)
                %>% mutate(task = as.character(task) %>% factor(., levels=tasks)) 
                %>% rename_all(funs(str_replace(string=., pattern="2", replacement="")) )
                ),
            est = purrr::map2(.x=formula, .y=data, function(.x, .y)
                feols(formula(.x), cluster=~rid, data=.y)),
            summ = furrr::future_map(.x=est, function(.x) 
                tidy(.x, conf.int=TRUE)),
            glance = furrr::future_map(.x=est, function(.x)
                glance(.x)),
            )
)
res.task.effect 

## ** Tables
## *** Table C1 (figure 1)

table = 'tab-c1'
## 
tab = (
    res.sep
    %>% filter(case!='libcons') 
    %>% mutate(case_value = factor(case_value,
                                   levels=c(
                                       "Pooled", "Left-wing", "Center",
                                       "Right-wing", "Low", "High"
                                   ), ordered = TRUE))
    %>% arrange(case_value  ) 
)
mods = tab %>% dplyr::pull(est) 
names(mods) = tab %>% dplyr::pull(case_value) %>% as.character(.)
coefs = c(
    "cand.rwl" = "Economic liberalism",
    "cand.rwc" = "Social conservatism",
    "cand.rwi" = "Anti-immigration",
    "cand.ppl" = "People centrism",
    "cand.ant" = "Anti-elitism",
    "cand.man" = "Manichean outlook",
    "cand.pop" = "Populism",
    "cand.out" = "Outsider",
    "cand.fem" = "Female")
## 
# table
tab = modelsummary(mods,
                   statistic='({conf.low}, {conf.high})',
                   stars=TRUE, ## c('*' = .1, '**' = .05, "***"=0.01),
                   vcov = 'classical', #"classical", "robust", "stata", "HC4", "HC0",
                   coef_omit = "Intercept",
                   coef_rename=coefs,
                   gof_omit = 'R2 Within|BIC', # regexp to exclude stats summary (AIC, etc)
                   ## coef_map=c(), ## to reorder and rename/will omit those not included
                   ## align=c("l", "c"),
                   ## notes = '',
                   output='data.frame',
                   ) %>% 
    select(-part, -statistic)%>% 
    mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term))
## 
tab
## latex
caption=glue("\\label{{{table}}}Linear regression of voters' support for the candidate",
             " on candidate's attribute (rows) by subgroups of voters (columns). ",
             " Standard errors are clustered by respondente (rid)."
             ) 
pvalues = glue("\\\\multicolumn{{{ncol(tab)}}}{{r}}{{\\\\rule{{0pt}}{{1em}}}",
               "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001}") 
tabl = (
    tab
    %>% kable(., "latex", booktabs = T, caption=caption, escape=F,
              align=c("l", rep("c", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL)
    ## %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
    ##                   position = "center", font_size=NULL)
    %>% add_header_above(
            header = c(
                " "= 2,
                "Voters' Ideology (self-placement)"=3,
                "Voters' Populist attitudes"=2
            ), escape=FALSE)  ## NOTE: for math notation: escape=FALSE and four backslashes \\beta
    ## add nlines
    %>% row_spec(., 18, extra_latex_after = '\\midrule')  
    ## restrict column size
    ## column_spec(2:ncol(tab), width = "3cm")
    %>% landscape() 
    %>% str_replace(string=.,
                    pattern="\\\\bottomrule",
                    replacement=glue("\\\\bottomrule\n{pvalues}") )
)
## cat(tabl)
## 
if (SAVE)
{
    print(glue("Saving {table}...") )
    fn = file.path(PATH_SM_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    print("done!")
}

## *** Table C2 (figure 2)

table = 'tab-c2'
## 
tab = (
    res.comb 
    %>% filter(ntasks==15) 
    %>% filter(thick=='ideo') 
    %>% filter(thin=='popmul2c') 
    %>% mutate(
            thick_value = factor(thick_value,
                                 levels=c("Left-wing", "Center", "Right-wing"),
                                 ordered = TRUE),
            thin_value = factor(thin_value,
                                levels=c("Low", 'High'),
                                ordered = TRUE)
        )
    %>% arrange(thin_value, thick_value) 
)
tab
## 
mods = tab %>% dplyr::pull(est) 
names(mods) = tab %>% dplyr::pull(thick_value) %>% as.character(.)
coefs = c(
    "cand.rwl" = "Economic liberalism",
    "cand.rwc" = "Social conservatism",
    "cand.rwi" = "Anti-immigration",
    "cand.ppl" = "People centrism",
    "cand.ant" = "Anti-elitism",
    "cand.man" = "Manichean outlook",
    "cand.pop" = "Populism",
    "cand.out" = "Outsider",
    "cand.fem" = "Female")
## 
# table
tab = modelsummary(mods,
                   statistic='({conf.low}, {conf.high})',
                   stars=TRUE, ## c('*' = .1, '**' = .05, "***"=0.01),
                   vcov = 'classical', #"classical", "robust", "stata", "HC4", "HC0",
                   coef_omit = "Intercept",
                   coef_rename=coefs,
                   gof_omit = 'R2 Within|BIC', # regexp to exclude stats summary (AIC, etc)
                   output='data.frame',
                   )%>% 
    select(-part, -statistic)%>% 
    mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term)) %>% 
    rename(' '=term) 
## 
tab
## latex
caption = glue("\\label{{{table}}}Linear regression of voters' support for the candidate",
               " on candidate's attribute (rows) by subgroups of voters (columns). ",
               " Standard errors are clustered by respondente (rid).") 
pvalues = glue("\\\\multicolumn{{{ncol(tab)}}}{{r}}{{\\\\rule{{0pt}}{{1em}}}",
               "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001}") 
tabl = (
    tab
    %>% kable(., "latex", booktabs = T, caption=caption, escape=F,
              align=c("l", rep("c", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL)
    ## %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
    ##                   position = "center", font_size=NULL)
    %>% add_header_above(
            header = c(
                " "= 1,
                "Voters' populist attitudes: Low"=3,
                "Voters' populist attitudes: High"=3
            ), escape=FALSE)  
    ## add nlines
    %>% row_spec(., 18, extra_latex_after = '\\midrule') 
    ## restrict column size
    ## %>% column_spec(2:ncol(tab), width = "3cm")
    %>% landscape() 
    %>% str_replace(string=.,
                    pattern="\\\\bottomrule",
                    replacement=glue("\\\\bottomrule\n{pvalues}") )
)
cat(tabl)
## tabl
## 
if (SAVE)
{
    print(glue("Saving {table}...") )
    fn = file.path(PATH_SM_FIGURES , glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    print("done!")

}

## *** Table C3 to C5 (figure 3)

i = 3
## 
thick_positions = c('anti-immigration',
                    'conservatism',
                    'economic liberalism')
for (thick in thick_positions)
{
    table = glue("tab-c{i}") 
    print(glue("Generating {table}...") )
    tab = (
        res.rwp 
        %>% filter(ntasks==15) 
        %>% filter(thick == 'ideo') 
        %>% filter(thin  == 'popmul2c') 
        %>% filter(str_detect(candprofile, pattern=!!thick)) 
        %>% mutate(
                thick_value = factor(thick_value,
                                     levels=c("Left-wing", "Center", "Right-wing"),
                                     ordered = TRUE),
                thin_value = factor(thin_value,
                                    levels=c("Low", 'High'),
                                    ordered = TRUE)
            )
        %>% arrange(thick_value, thin_value) 
    )
    tab
    ## 
    mods = tab %>% dplyr::pull(est) 
    names(mods) = tab %>% dplyr::pull(thin_value) %>% as.character(.)
    ## 
    # table
    tab = modelsummary(mods,
                       statistic='({conf.low}, {conf.high})',
                       stars=TRUE, ## c('*' = .1, '**' = .05, "***"=0.01),
                       vcov = 'classical', #"classical", "robust", "stata", "HC4", "HC0",
                       coef_omit = "Intercept|LWP",
                       gof_omit = 'R2 Within|BIC', # regexp to exclude stats summary (AIC, etc)
                       output='data.frame',
                       )%>% 
        select(-part, -statistic)%>% 
        mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term))
    ## latex
    caption = glue("Linear regression of voters' support for the candidate",
                   " on candidate's attribute (rows) by subgroups of voters (columns). ",
                   " Standard errors are clustered by respondente (rid).",
                   " \\label{{{table}}}") 
    pvalues = glue("\\\\multicolumn{{{ncol(tab)}}}{{r}}{{\\\\rule{{0pt}}{{1em}}}",
                   "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001}") 
    tab
    tabl = (
        tab 
        %>% rename(' '=term) 
        %>% kable(., "latex", booktabs = T, caption=caption, escape=F,
                  align=c("l", rep("c", ncol(.)-1)),
                  digits=4, longtable = F, table.envir = "table",
                  linesep = NULL)
        %>% add_header_above(
                header = c(
                    " "= 1,
                    "Left-wing voters"=2,
                    "Centrist voters"=2,
                    "Right-wing voters"=2
                ), escape=FALSE)  
        ## %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
        ##                   position = "center", font_size=NULL)
        %>% row_spec(., 4, extra_latex_after = '\\midrule') 
        %>% landscape() 
        %>% str_replace_all(string=.,
                            pattern=glue("RWP \\({thick}\\)") ,
                            replacement="") 
        %>% str_replace(string=.,
                        pattern="\\\\bottomrule",
                        replacement=glue("\\\\bottomrule\n{pvalues}") )
    )
    cat(tabl)
    ## 
    if (SAVE)
    {
        print(glue("Saving {table}...") )
        fn = file.path(PATH_SM_TABLES, glue("{table}"))
        write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
        write_xlsx(x=tab, path=glue("{fn}.xlsx"))
        writeLines(text=tabl, glue("{fn}.tex"))
        print("done!")
    }
    i = i+1
}


## *** Table D1 (RW VS RWP)

table = 'tab-d1'
## 
thin = 'popmul2c'
thick = 'ideo'
tab = (
    res.rwp.full
    %>% filter(thin==!!thin & thick==!!thick) 
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Interc")) 
    %>% filter(!str_detect(term, pattern="LW")) 
    %>% mutate(term = stringr::str_replace_all(string=term,
                                               pattern='`RWP.*`',
                                               replacement=""),
               term = factor(term, levels= CAT_ORDER[['candidates']]),
               ## 
               thick_value = glue("{thick_value} voters"),
               thick_value = str_replace(string=thick_value,
                                         pattern="Center",
                                         replacement="Centrist"),
               thick_value = factor(thick_value, levels=c("Left-wing voters",
                                                          'Centrist voters',
                                                          'Right-wing voters')),
               ## 
               thin_value = factor(thin_value, levels=c('Low', 'High')),
               )
)
## 
## 
thin = 'popmul2c'
thick = 'ideo'
tab = (
    tab
    %>% filter(thick_value=='Right-wing voters') 
    %>% filter(thin_value=='High') 
    %>% select(thin, thick, term, b=estimate, se=`std.error`)
    %>% mutate(thin = case_when(str_detect(term, pattern="RWP") ~ 'RWP',
                                T ~ 'RW'),
               thick = stringr::str_replace(string=term, pattern="RW[ |P]",
                                            replacement="") %>% str_trim(.),
               )
    %>% tidyr::pivot_wider(id_cols=thick,
                           names_from=thin,
                           values_from=c(b, se)
                           )
    %>% mutate(
            perc.diff = 100*(b_RWP - b_RW)/abs(b_RW),
            t.diff = (b_RWP -b_RW)/sqrt(se_RWP^2 + se_RW^2),
            p.diff = 1-pnorm(abs(t.diff)),
            ##
            )    
    %>% mutate(
            across(where(is.numeric), round, 4),
            Case = case_when(
                thick == 'anti-immigration'~'(a)',
                thick == 'social conservative'~'(b)',
                thick == 'economic liberalism'~'(c)'
            )
        ) 
    %>% rowwise(.) 
    %>% mutate(
            'Case'= Case,
            'Candidate Attribute' = thick %>% stringr::str_to_title(.),
            '$\\beta_{RW}$' = glue("{b_RW} ({se_RW})") ,
            '$\\beta_{RWP}$' = glue("{b_RWP} ({se_RWP})") ,
            `\\% Change` = perc.diff,
            't-stat'   = t.diff,
            'p-value'  = p.diff
        )
    %>% select(-contains("b_"), -contains("se_"), -contains("."), -thick)
    )
tab
## 
caption = glue("\\label{{{table}}}Results (t-statistics, and p-value)",
               " of difference between causal effects of candidates RW and RWP attributes",
               " among right-wing populist voters (cases (a), (b), and (c) of Figure D6,",
               " analogous to Figure 3).",
               " Candidate attributes' effect standard error shown in parentheses.") 
tabl = (tab
     %>% kable(., "latex", booktabs = T, caption=caption,
             escape=F, align=c("l", rep("c", ncol(.)-1)),
             digits=4, longtable = F, table.envir = "table",
             linesep = NULL) 
    %>% add_header_above(
            header = c(' '= 2,
                       'Attribute Effect'=2,
                       'Diffrence Statistics'=2),
            escape=FALSE) 
    ## NOTE: for math notation: escape=FALSE and four backslashes \\beta
    ## %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
    ##                   position = "center", font_size=NULL) 
)
tabl
if (SAVE) {
    print(glue("Saving {table}...") )
    fn = file.path(PATH_SM_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    print("done!")
}


## *** Table G1 (RW vs RWP by task)

table = 'tab-g1'
## 
thin = 'popmul2c'
thick = 'ideo'
tabt = (
    res.rwp
    %>% filter(thin==!!thin & thick==!!thick) 
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Interc")) 
    %>% filter(!str_detect(term, pattern="LW")) 
    %>% mutate(term = stringr::str_replace_all(string=term,
                                               pattern='`RWP.*`',
                                               replacement=""),
               term = factor(term, levels= CAT_ORDER[['candidates']]),
               ## 
               thick_value = glue("{thick_value} voters"),
               thick_value = str_replace(string=thick_value,
                                         pattern="Center",
                                         replacement="Centrist"),
               thick_value = factor(thick_value, levels=c("Left-wing voters",
                                                          'Centrist voters',
                                                          'Right-wing voters')),
               ## 
               thin_value = factor(thin_value, levels=c('Low', 'High')),
               )
)
tabt
## 
## 
thin = 'popmul2c'
thick = 'ideo'
tab = (
    tabt
    %>% nest(-tasks_case) 
    %>% mutate(data = future_map(.x=data, function(.x)
        .x
        %>% filter(thick_value=='Right-wing voters') 
        %>% filter(thin_value=='High') 
        %>% select(thin, thick, term, b=estimate, se=`std.error`)
        %>% mutate(thin = case_when(str_detect(term, pattern="RWP") ~ 'RWP',
                                    T ~ 'RW'),
                   thick = stringr::str_replace(string=term, pattern="RW[ |P]",
                                                replacement="") %>% str_trim(.),
                   )
        %>% tidyr::pivot_wider(id_cols=thick,
                               names_from=thin,
                               values_from=c(b, se)
                               )
        %>% mutate(
                perc.diff = 100*(b_RWP - b_RW)/abs(b_RW),
                t.diff = (b_RWP -b_RW)/sqrt(se_RWP^2 + se_RW^2),
                p.diff = 1-pnorm(abs(t.diff))
                ##
                )    
        %>% mutate(
                across(where(is.numeric), round, 4),
                Case = case_when(
                    thick == 'anti-immigration'~'(a)',
                    thick == 'social conservative'~'(b)',
                    thick == 'economic liberalism'~'(c)'
                )
            ) 
        %>% rowwise(.) 
        %>% mutate(
                'Case'= Case,
                'Candidate Attribute' = thick %>% stringr::str_to_title(.),
                '$\\beta_{RW}$' = glue("{b_RW} ({se_RW})") ,
                '$\\beta_{RWP}$' = glue("{b_RWP} ({se_RWP})") ,
                `\\% Change` = perc.diff,
                't-stat'   = t.diff,
                'p-value'  = p.diff
            )
        %>% select(-contains("b_"), -contains("se_"), -contains("."), -thick)
        )
        )
    %>% unnest(data) 
    %>% arrange(Case)  
    %>% rename(' '=tasks_case) 
)
tab

## 
caption = glue("\\label{{{table}}}Results (t-statistics, and p-value)",
               " of difference between causal effects of candidates RW and RWP attributes",
               " among right-wing populist voters (cases (a), (b), and (c) of Figure D6,",
               " analogous to Figure 3).",
               " Candidate attributes' effect standard error shown in parentheses.") 
tabl = (tab
     %>% kable(., "latex", booktabs = T, caption=caption,
             escape=F, align=c("l", rep("c", ncol(.)-1)),
             digits=4, longtable = F, table.envir = "table",
             linesep = NULL) 
    %>% add_header_above(
            header = c(' '= 2,
                       'Attribute Effect'=2,
                       'Diffrence Statistics'=2),
            escape=FALSE) 
    ## NOTE: for math notation: escape=FALSE and four backslashes \\beta
    ## %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
    ##                   position = "center", font_size=NULL) 
)
tab %>% print(., n=Inf, width=Inf) 
## tabl
if (SAVE) {
    print(glue("Saving {table}...") )
    fn = file.path(PATH_SM_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    print("done!")
}




## *** Demographics (F1-F5)
## **** loading census data

fn = file.path(PATH_DATA_FINAL, "quotas.xlsx")
census_age  = read_excel(path=fn, sheet='age-census') 
census_educ = read_excel(path=fn, sheet='educ-census')
census_inc  = read_excel(path=fn, sheet='inc-census')
census_race = read_excel(path=fn, sheet='race-census')

## **** Table F1 (age)

table = 'tab-f1'
## 
tab=(
    df
    %>% sjmisc::frq(agec)
    %>% data.frame() %>% as_tibble() 
    %>% mutate(
            age=val %>% as.character(.),
            freq.survey=raw.prc
        )
    %>% left_join(.,
                  census_age
                  %>% mutate(freq.census=100*freq)
                , by=c('age')) 
    %>% select(
            `Age group` = age,
            `Sample Frequency` =freq.survey,
            `Census Frequency` =freq.census,
        ) 
    %>% drop_na() 
)
tab %>% print(., n=Inf, width=Inf) 
## 
caption=glue('Age frequency in the sample and census\\label{{{table}}}') 
tabl = (
        tab
        %>% kableExtra::kable(., "latex", booktabs = T, caption=caption,
                              escape=T, align=c("l", rep("c", ncol(.)-1)),
                              digits=4, longtable = F, table.envir = "table",
                              linesep = NULL) 
    )
## 
if (SAVE) {
    ## fn=file.path(PATH_SM_TABLES, glue("{label}.tex") )
    ## tabl %>% writeLines(., con = fn) 
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_SM_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}

## **** Table F2 (income)

table = 'tab-f2'
## 
tab = (
    df
    %>% sjmisc::frq(inc10c) %>% data.frame() %>% as_tibble() 
    %>% rename(inc10c=val) 
    %>% left_join(., census_inc, by=c('inc10c')) 
    %>% select(
            `Income group` = inc,
            `Sample frequency`=raw.prc,
            `Census frequency`=freq
        )
    %>% drop_na() 
)
tab %>% print(., n=Inf, width=Inf) 
## 
caption=glue('Income group frequency in the sample and census\\label{{{table}}}') 
tabl = (
        tab
        %>% kableExtra::kable(., "latex", booktabs = T, caption=caption,
                              escape=T, align=c("l", rep("c", ncol(.)-1)),
                              digits=4, longtable = F, table.envir = "table",
                              linesep = NULL) 
)
## tabl
## 
if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_SM_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}


## **** Table F3 (education)

table = 'tab-f3'
## 
tab = (
    df
    %>% frq(educ) %>% data.frame() %>% as_tibble() 
    %>% rename(educ=val) 
    %>% left_join(., census_educ, by=c('educ')) 
    %>% select(
            `Education group` = educ5c,
            `Sample frequency`=raw.prc,
            `Census frequency`=freq
        )
    %>% drop_na() 
)
tab %>% print(., n=Inf, width=Inf) 
##
caption=glue('Education group frequency in the sample and census\\label{{{table}}}') 
tabl = (
    tab
    %>% kableExtra::kable(., "latex", booktabs = T, caption=caption,
                          escape=T, align=c("l", rep("c", ncol(.)-1)),
                          digits=4, longtable = F, table.envir = "table",
                          linesep = NULL) 
)
## 
if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_SM_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}


## **** Table F4 (race)

table='tab-f4'
## 
tab = (
    df
    %>% frq(race) %>% data.frame() %>% as_tibble() 
    %>% rename(race=val) 
    %>% left_join(., census_race, by=c('race')) 
    %>% select(
            `Race group` = race_group,
            `Sample frequency`=raw.prc,
            `Census frequency`=freq
        )
    %>% drop_na() 
)
tab %>% print(., n=Inf, width=Inf) 
## 
caption=glue('Race group frequency in the sample and census\\label{{{table}}}') 
tabl = (
    tab
    %>% kableExtra::kable(., "latex", booktabs = T, caption=caption,
                          escape=T, align=c("l", rep("c", ncol(.)-1)),
                          digits=2, longtable = F, table.envir = "table",
                          linesep = NULL) 
)
## 
if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_SM_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}


## **** Table F5 (summary statistics)

table = 'tab-f5'
## 
tab = (
    df
    %>% distinct(rid, .keep_all=TRUE) 
    %>% mutate(White = case_when(race==1~1,
                                 T ~ 0),
               inc10 = as.numeric(inc10c),
               Female = female.resp,
               ptyid = case_when(ptyid=='Democrat'   ~-1,
                                 ptyid=='Independent'~ 0,
                                 ptyid=='Republican' ~ 1),
               )
    %>% select(Female, Age=age, Education=educ, White, Income=inc10,
               `Party identification`=ptyid,
               `Liberal-Conservative`=libcons.raw,
               `Left-right`=ideo.raw,
               `Populist attitudes`=popmul)
    %>% datasummary_skim(data=., output='data.frame', fmt="%.2f")
)
tab
##
caption=glue('Summary Statistics\\label{{{table}}}')
tabl = (tab
        %>% kable(., "latex", booktabs = T, caption=caption,
                  escape=T, align=c("l", rep("c", ncol(.)-1)),
                  digits=4, longtable = F, table.envir = "table",
                  linesep = NULL) 
        %>% kable_styling(latex_options = c("scale_down"),
                                      position = "center", , font_size=NULL) 
        %>% str_replace(string=., pattern="\\\\begin\\{table\\}",
                        replacement="\\\\begin\\{table\\}[!htbp]")
    )
cat(tabl)
## 
if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_SM_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}

## ** Figures
## *** Figure D1-D3 (robustiness ptyid, lib-cons, popatt)

## res.rwp$thin %>% unique
## res.rwp$thick %>% unique
for (thin in unique(res.rwp$thin))
{
    for (thick in unique(res.rwp$thick))
    {
        ## thin = 'popmul2c'
        ## thick = 'ptyid'
        print(glue("Creating plot for {thin} and {thick}...") ) 
        ##
        tab = (
            res.rwp  
            %>% filter(ntasks==15) 
            %>% filter(thin==!!thin & thick==!!thick) 
            %>% unnest(summ)
            %>% filter(!str_detect(term, pattern="Interc")) 
            %>% filter(!str_detect(term, pattern="LW")) 
            %>% mutate(term = stringr::str_replace_all(string=term,
                                                       pattern='`RWP.*`',
                                                       replacement=""),
                       term = factor(term, levels= CAT_ORDER[['candidates']]),
                       thin_value = factor(thin_value, levels=c('Low', 'High')),
                       )
        )
        if (thick=='ptyid') {
            tab = tab %>% mutate(thick_value  = factor(thick_value , levels=CAT_ORDER[['ptyid']]))
        }
        if (thick=='libcons') {
            tab = tab %>% mutate(thick_value   = factor(thick_value , levels=CAT_ORDER[['libcons']]))
        }
        if (thick=='ideo') {
            tab = tab %>% mutate(thick_value   = factor(thick_value , levels=CAT_ORDER[['ideo']]))
        }
        tab 
        ## plot
        ## ----
        x = "term"
        y = "estimate"
        color="thin_value"
        fill="thin_value"
        shape="thin_value"
        facet1="thick_value"
        facet2=NULL
        dodge=.3
        leg_title='Populist Attitudes'
        ylab = glue("Average Marginal Effect of Candidate\n",
                    "Attributes on Voters' Support for the Candidate")
        g = (
            tab 
            %>% ggplot(.)
            + geom_rect(aes(xmin=0.5, xmax=2.5, ymin=-Inf, ymax=Inf),
                        fill="grey", alpha=0.025, inherit_aes = F)
            + geom_rect(aes(xmin=4.5, xmax=6.5, ymin=-Inf, ymax=Inf),
                        fill="grey", alpha=0.025, inherit_aes = F)
            + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high",
                                       color=fill),
                            width=.05, position = position_dodge(dodge)) 
            + geom_point(aes_string(x=x, y=y, fill=fill, shape=shape),
                         position=position_dodge(dodge))
            + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
            + scale_x_discrete(labels = c('<i>RW', '<b>RWP',
                                          '<i>RW', '<b>RWP',
                                          '<i>RW', '<b>RWP'))
            + scale_shape_manual(values=c(25,24))
            + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
            + scale_color_grey(start = 0.7, end = 0,  na.value="red") 
            + facet_wrap( paste("~", facet1), ncol = , scales='free')  
            + ggguides(ncol=2)
            ## 
            + geom_text(aes_string(x=1.5, y="-Inf", label="'Anti-\nImmigration'"),
                        colour="black", show_legend = F, parse=F,
                        vjust=-.2, hjust=.50,
                        position=position_dodge(0), angle=0, size=3,
                        data=tibble(ideo3c='Center')
                        ) 
            + geom_text(aes_string(x=3.5, y="-Inf", label="'Social\nconservative'"),
                        colour="black", show_legend = F, parse=F,
                        vjust=-.2, hjust=.50,
                        position=position_dodge(0), angle=0, size=3,
                        data=tibble(ideo3c='Center')
                        ) 
            + geom_text(aes_string(x=5.5, y="-Inf", label="'Econonic\nliberal'"),
                        colour="black", show_legend = F, parse=F,
                        vjust=-.2, hjust=.50,
                        position=position_dodge(0), angle=0, size=3,
                        data=tibble(ideo3c='Center')
                        )
            + labs(
                  x        = NULL,
                  y        = ylab,
                  color    = leg_title, 
                  fill     = leg_title,
                  linetype = NULL,
                  shape    = leg_title,
                  title    = NULL,
                  subtitle = NULL,
                  caption  = NULL
              )
            + theme(axis.text.x = element_markdown())
        )
        g
        if (SAVE) {
            fn = file.path(PATH_SM_FIGURES, glue("fig-D-{thin}-{thick}.pdf") )
            ggsave(g, filename=fn, width=9.5, height=4 ) 
        }
    }
}


## *** Figure D4 (pooled, by thick, by thin separately; full data)

figure = 'fig-d4'
## 
tab = (
    res.sep.full
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Intercep")) 
    %>% mutate(term=str_replace_all(string=term, pattern="`|2", replacement=""),
               term=factor(term, levels=names(ATT), labels=ATT))
)
tab
## plot
## ----
x = "estimate"
y = "term"
color=NULL
fill="case_value"
facet1='case'
facet2=NULL
dodge=.3
## Pooled
## ------
leg_title = 'Polled'
gpooled = (
    tab
    %>% filter(case=='pooled') 
    %>% ggplot(.)
    + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high"),
                     height=.1, 
                     position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill), position = position_dodge(dodge))
    + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
    + scale_y_discrete(limits=rev)
    + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_fill_grey(start = 0., end = 0,  na.value="red") 
    + scale_shape_manual(values=c( 25, 23, 24))
    + labs(
          x        = NULL,
          y        = NULL,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + ggguides(ncol=10)
    + theme(panel.border       = element_rect(colour="black", fill='transparent'))
)
## thick ideology
## --------------
leg_title = 'Voter Self-Identification'
gthick = (
    tab
    %>% filter(case==!!ideo) 
    %>% mutate(case_value=factor(case_value, levels=c('Left-wing',
                                                      'Center',
                                                      'Right-wing')))
    %>% ggplot(.)
    + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high",
                                color=fill), height=.3,
                     position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill), position = position_dodge(dodge))
    + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
    + scale_y_discrete(limits=rev)
    + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_shape_manual(values=c( 25, 23, 24))
    + labs(
          x        = NULL,
          y        = NULL,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + ggguides(ncol=1)
    + theme(axis.text.y = element_blank(),
          panel.border       = element_rect(colour="black", fill='transparent'))
)
## thick ideology
## --------------
leg_title = 'Populist Attitudes'
gthin = (
    tab
    %>% filter(case==!!pop) 
    %>% mutate(case_value=factor(case_value, levels=c('Low', 'High')))
    %>% ggplot(.)
    + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high",
                                color=fill), height=.3,
                     position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill), position = position_dodge(dodge))
    + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
    + scale_y_discrete(limits=rev)
    + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_shape_manual(values=c( 25,  24))
    + labs(
          x        = NULL,
          y        = NULL,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + ggguides(ncol=2)
    + theme(axis.text.y = element_blank(),
            panel.border       = element_rect(colour="black", fill='transparent'),
            )
)
## 
xlab = glue("Average Marginal Effect of Candidate ",
            "Attributes on Voter's Support for the Candidate")
xlab=text_grob(xlab, size = 11.5, face = "bold", hjust=.4, vjust=.1)   #### this worked for me
ylab = text_grob("Candidates' Attributes", face='bold', rot=90, vjust=1)
g = gpooled + gthick + gthin + plot_annotation()
g = gridExtra::grid.arrange(patchwork::patchworkGrob(g),
                            left = ylab, bottom = xlab)
g
if (SAVE) {
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_SM_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=8.5, height=4.5 )}
}




## *** Figure D5 (thick and thin combined; full data)

figure = 'fig-d5'
## 
tab = (
    res.comb.full
    %>% filter(thin=='popmul2c' & thick=='ideo') 
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Intercep")) 
    %>% mutate(term=str_replace_all(string=term, pattern="`|2", replacement=""),
               term=factor(term, levels=names(ATT), labels=ATT),
               ## 
               thick_value = glue("{thick_value} voters"),
               thick_value = str_replace(string=thick_value,
                                         pattern="Center",
                                         replacement="Centrist"),
               thick_value = factor(thick_value, levels=c("Left-wing voters",
                                                          'Centrist voters',
                                                          'Right-wing voters')),
               thin_value = factor(thin_value, levels=c('Low', 'High'))
               )
)
tab
## plot
## ----
x = "estimate"
y = "term"
color="thin_value"
fill="thin_value"
shape="thin_value"
facet1='thick_value'
facet2=NULL
dodge=.3
xlab = glue("Average Marginal Effect of Candidate ",
            "Attributes on Voter's Support for the Candidate")
ylab = "Candidates' Attributes"
leg_title = "Voters' Populist attitudes"
g = (
    tab
    %>% ggplot(.)
    + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high",
                                color=fill),
                     height=.1, 
                     position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=shape), position = position_dodge(dodge))
    + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
    + scale_y_discrete(limits=rev)
    + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_shape_manual(values=c( 25, 24))
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + facet_wrap( paste("~", facet1))  
    + ggguides(ncol=2)
    + theme(panel.border = element_rect(colour="black", fill='transparent'))
)
g
## 
if (SAVE) {
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_SM_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=8.5, height=4.5 )}
}




## *** Figure D6 (RW vs RWP support; full data)

figure = 'fig-d6'
## 
thin = 'popmul2c'
thick = 'ideo'
tab = (
    res.rwp.full
    %>% filter(thin==!!thin & thick==!!thick) 
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Interc")) 
    %>% filter(!str_detect(term, pattern="LW")) 
    %>% mutate(term = stringr::str_replace_all(string=term,
                                               pattern='`RWP.*`',
                                               replacement=""),
               term = factor(term, levels= CAT_ORDER[['candidates']]),
               ## 
               thick_value = glue("{thick_value} voters"),
               thick_value = str_replace(string=thick_value,
                                         pattern="Center",
                                         replacement="Centrist"),
               thick_value = factor(thick_value, levels=c("Left-wing voters",
                                                          'Centrist voters',
                                                          'Right-wing voters')),
               ## 
               thin_value = factor(thin_value, levels=c('Low', 'High')),
               )
)
tab 
## plot
## ----
x = "term"
y = "estimate"
color="thin_value"
fill="thin_value"
shape="thin_value"
facet1="thick_value"
facet2=NULL
dodge=.3
leg_title='Populist Attitudes'
ylab = glue("Average Marginal Effect of Candidate\n",
            "Attributes on Voters' Support for the Candidate")
g = (
    tab 
    %>% ggplot(.)
    + geom_rect(aes(xmin=0.5, xmax=2.5, ymin=-Inf, ymax=Inf),
                fill="grey", alpha=0.025, inherit_aes = F)
    + geom_rect(aes(xmin=4.5, xmax=6.5, ymin=-Inf, ymax=Inf),
                fill="grey", alpha=0.025, inherit_aes = F)
    + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high",
                               color=fill),
                    width=.05, position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=shape),
                 position=position_dodge(dodge))
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
    + scale_x_discrete(labels = c('<i>RW', '<b>RWP',
                                  '<i>RW', '<b>RWP',
                                  '<i>RW', '<b>RWP'))
    + scale_shape_manual(values=c(25,24))
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_color_grey(start = 0.7, end = 0,  na.value="red") 
    + facet_wrap( paste("~", facet1), ncol = , scales='free')  
    + ggguides(ncol=2)
    ## 
    + geom_text(aes_string(x=1.5, y="-Inf", label="'Anti-\nImmigration'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                ) 
    + geom_text(aes_string(x=3.5, y="-Inf", label="'Social\nconservative'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                ) 
    + geom_text(aes_string(x=5.5, y="-Inf", label="'Econonic\nliberal'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                )
    + labs(
          x        = NULL,
          y        = ylab,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + theme(axis.text.x = element_markdown())
)
## g
df_placeholder = tibble(
    thick_value=factor('Right-wing voters',
                       levels=c('Left-wing voters',
                                'Centrist voters',
                                'Right-wing voters'))) 
gi = (
    g
    +  geom_bracket(
           xmin       = get_pos(tab, pos='x', rw='anti-immigration')[1],
           xmax       = get_pos(tab, pos='x', rw='anti-immigration')[2],
           y.position = get_pos(tab, pos='y', rw='anti-immigration'),
           label = "(a)",
           type = "text",
           label.size=4,
           bracket.nudge.y=0,
           size=.3,
           clip='off',
           linetype=2,
           vjust=-.5,
           tip.length = c(0.37, 0.02),
           data = df_placeholder
       )
    +  geom_bracket(
           xmin       = get_pos(tab, pos='x', rw='social conservative')[1],
           xmax       = get_pos(tab, pos='x', rw='social conservative')[2],
           y.position = get_pos(tab, pos='y', rw='social conservative'),
           label = "(b)",
           type = "text",
           label.size=4,
           bracket.nudge.y=0,
           size=.3,
           clip='off',
           linetype=2,
           vjust=-.5,
           tip.length = c(0.27, 0.02),
           data = df_placeholder
       )
    +  geom_bracket(
           xmin       = get_pos(tab, pos='x', rw='economic liberal')[1],
           xmax       = get_pos(tab, pos='x', rw='economic liberal')[2],
           y.position = get_pos(tab, pos='y', rw='economic liberal'),
           label = "(c)",
           type = "text",
           label.size=4,
           bracket.nudge.y=0,
           size=.3,
           clip='off',
           linetype=2,
           vjust=1.7,
           tip.length = c(0.23, 0.02),
           data = df_placeholder
       )
)
gi
## 
if (SAVE)
{
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_SM_FIGURES, fn)
    for (fn in fns){ggsave(gi, filename=fn, width=9.5, height=4 )}
}


## *** Figure E1 (LW vs LWP)

figure = 'fig-e1'
## 
thin = 'popmul2c'
thick = 'ideo'
tab = (
    res.lwp
    %>% filter(thin==!!thin & thick==!!thick) 
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Interc")) 
    %>% filter(str_detect(term, pattern="LW")) 
    %>% mutate(term = stringr::str_replace_all(string=term,
                                               pattern='`RWP.*`',
                                               replacement=""),
               term = factor(term, levels= CAT_ORDER[['candidates_lw']]),
               ##            ## 
               thick_value = glue("{thick_value} voters"),
               thick_value = str_replace(string=thick_value,
                                         pattern="Center",
                                         replacement="Centrist"),
               thick_value = factor(thick_value, levels=c("Left-wing voters",
                                                          'Centrist voters',
                                                          'Right-wing voters')),
               ## 
               thin_value = factor(thin_value, levels=c('Low', 'High')),
               )
)
tab
## plot
## ----
x = "term"
y = "estimate"
color="thin_value"
fill="thin_value"
shape="thin_value"
facet1="thick_value"
facet2=NULL
dodge=.3
leg_title='Populist Attitudes'
ylab = glue("Average Marginal Effect of Candidate\n",
            "Attribute on Voter's Support for the Candidate")
g = (
    tab 
    %>% select(!!x, !!y, color, facet1, estimate, contains("conf."))
    %>% ggplot(.)
    + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high",
                               color=fill),
                    width=.05, position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=shape),
                 position=position_dodge(dodge))
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
    + scale_x_discrete(labels = c('<i>LW', '<b>LWP',
                                  '<i>LW', '<b>LWP',
                                  '<i>LW', '<b>LWP'))
    + scale_shape_manual(values=c(25,24))
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_color_grey(start = 0.7, end = 0,  na.value="red") 
    + facet_wrap( paste("~", facet1), ncol = , scales='free')  
    + ggguides(ncol=2)
    ## 
    + geom_text(aes_string(x=1.5, y="-Inf", label="'Favorable on\nImmigration'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                ) 
    + geom_text(aes_string(x=3.5, y="-Inf", label="'Social\nliberal'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                ) 
    + geom_text(aes_string(x=5.5, y="-Inf", label="'Pro-\nredistribution'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                )
    + labs(
          x        = NULL,
          y        = ylab,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + theme(axis.text.x = element_markdown())
)
g
## 
if (SAVE) {
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_SM_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=9.5, height=4 )}
}

## *** Figure G1 and G2 (diagnotics: carry over and order effects; facets) 

interaction_cases = c('task', 'order')
ntasks = 7
figures = c(
    'order' = 'fig-g1',
    'task'  = 'fig-g2'
)
## 
for (interaction_case in interaction_cases)
{
    figure = figures[interaction_case]
    print(glue("Creating figure {figure}...") )
    ## 
    tab = (
        res.diag
        %>% unnest(summ)
        %>% filter(!str_detect(term, pattern="Intercep"))  
        %>% filter(str_detect(term, pattern=":")) 
        %>% filter(interaction==interaction_case) 
        %>% mutate(term = str_replace_all(string=term,
                                          pattern="orderb|task|:|`|2",
                                          replacement="")   %>% 
                       str_replace_all(., ATT) %>%
                       factor(., levels=ATT)
                   ) 
    )
    ## plot
    ## ----
    x = "estimate"
    y = "term"
    color=NULL
    fill="case_value"
    facet1='case'
    facet2='tasks_case'
    dodge=.3
    title = glue("Diagnistics: {interaction_case} effect") 
    leg_title = 'Polled'
    gpooled = (
        tab 
        %>% filter(case=='pooled') 
        %>% ggplot(.)
        + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high"),
                         height=.1, 
                         position = position_dodge(dodge)) 
        + geom_point(aes_string(x=x, y=y, fill=fill), position = position_dodge(dodge))
        + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
        + scale_y_discrete(limits=rev)
        + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
        + scale_fill_grey(start = 0., end = 0,  na.value="red") 
        + scale_shape_manual(values=c( 25, 23, 24))
        + facet_grid(paste(facet2, " ~ .")) 
        + labs(
              x        = NULL,
              y        = NULL,
              color    = leg_title, 
              fill     = leg_title,
              linetype = NULL,
              shape    = leg_title,
              title    = title,
              subtitle = NULL,
              caption  = NULL
          )
        + ggguides(ncol=10)
        + theme(panel.border       = element_rect(colour="black", fill='transparent'))
    )
    gpooled 
    ## thick ideology
    ## --------------
    leg_title = 'Voter Self-Identification'
    dodge=.5
    gthick = (
        tab
        %>% filter(case=='ideo') 
        %>% mutate(case_value=factor(case_value, levels=c('Left-wing',
                                                          'Center',
                                                          'Right-wing')))
        %>% ggplot(.)
        + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high",
                                    color=fill), height=.3,
                         position = position_dodge(dodge)) 
        + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill), position = position_dodge(dodge))
        + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
        + scale_y_discrete(limits=rev)
        + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
        + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
        + scale_shape_manual(values=c( 25, 23, 24))
        + facet_grid(paste(facet2, " ~ .")) 
        + labs(
              x        = NULL,
              y        = NULL,
              color    = leg_title, 
              fill     = leg_title,
              linetype = NULL,
              shape    = leg_title,
              title    = NULL,
              subtitle = NULL,
              caption  = NULL
          )
        + ggguides(ncol=1)
        + theme(axis.text.y = element_blank(),
                panel.border       = element_rect(colour="black", fill='transparent'))
    )
    gthick 
    ## thick ideology
    ## --------------
    leg_title = 'Populist Attitudes'
    dodge=.3
    gthin = (
        tab
        %>% filter(case=='popmul2c') 
        %>% mutate(case_value=factor(case_value, levels=c('Low', 'High')))
        %>% ggplot(.)
        + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high",
                                    color=fill), height=.3,
                         position = position_dodge(dodge)) 
        + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill), position = position_dodge(dodge))
        + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
        + scale_y_discrete(limits=rev)
        + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
        + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
        + scale_shape_manual(values=c( 25,  24))
        + facet_grid(paste(facet2, " ~ .")) 
        + labs(
              x        = NULL,
              y        = NULL,
              color    = leg_title, 
              fill     = leg_title,
              linetype = NULL,
              shape    = leg_title,
              title    = NULL,
              subtitle = NULL,
              caption  = NULL
          )
        + ggguides(ncol=2)
        + theme(axis.text.y = element_blank(),
                panel.border       = element_rect(colour="black", fill='transparent'),
                )
    )
    xlab = glue("Average Marginal Effect of the Interactin Between\n",
                " Candidate Attributes the Order of the Attribute\n",
                ' in the Conjoint Table.'
                )
    xlab=text_grob(xlab, size = 11.5, face = "bold", hjust=.4, vjust=.1)   #### this worked for me
    ylab = text_grob("Candidates' Attributes", face='bold', rot=90, vjust=1)
    g = gpooled + gthick + gthin + plot_annotation()
    g = gridExtra::grid.arrange(patchwork::patchworkGrob(g),
                                left = ylab, bottom = xlab)
    g
    if (SAVE) {
        fn  = c(glue("{figure}.pdf"),
                glue("{figure}.png"))
        fns = file.path(PATH_SM_FIGURES, fn)
        print(glue("Saving figure {figure}...") )
        for (fn in fns){ggsave(g, filename=fn, width=9.5, height=9 )}
    }
}


## *** Figure G3 (task effect details)

figure = 'fig-g3'
## 
tab = (
    res.task.effect
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Intercep"))  
    %>% filter(str_detect(term, pattern=":"))  
    %>% filter(str_detect(term, pattern="task"))  
    %>% mutate(
            task = (
                term
                %>% str_replace_all(string=., pattern="\\.", replacement="")
                %>% parse_number(term)
            ),
            term = stringr::str_replace_all(string=term,
                                            pattern=":|task|order|[0-9]|`", replacement=""),
        )  
    %>% filter(interaction=='task')   
    %>% filter(case=='pooled')   
    %>% mutate(term = str_replace_all(term, ATT) %>%
                   factor(., levels=ATT))
)
tab
## 
x = "task"
y = "estimate"
color    = NULL
fill     = 'tasks_case'
facet1   = 'term'
facet2   = NULL
leg      = NULL
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .5
ylab     = glue("Effect of the interaction between the task order\n",
                " and the Attribute") 
xlab     = 'Task number'
g = (
    tab
    %>% ggplot(.)
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
    + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high",
                               color=fill), width=.05,
                 position = position_dodge(dodge))
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill),
                 position = position_dodge(dodge))
    + scale_fill_grey(start = 0, end = .7, na.value="red") 
    + scale_color_grey(start = 0, end = .7, na.value="red") 
    + scale_shape_manual(values=c(21,22,23))
    ## + scale_x_continuous(breaks=c(1:7,9:15)) 
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
    + facet_wrap( paste("~", facet1), ncol = , scales='free')  
    + ggguides()
)
g
if (SAVE) {
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_SM_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=9, height=6 )}
}




## *** Figure G4 (thick and thin combined; all tasks, first, and second half)

figure = 'fig-g4'
## 
tab = (
    res.comb
    %>% filter(thin=='popmul2c' & thick=='ideo') 
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Intercep")) 
    %>% mutate(term=str_replace_all(string=term, pattern="`|2", replacement=""),
               term=factor(term, levels=names(ATT), labels=ATT),
               ## 
               thick_value = glue("{thick_value} voters"),
               thick_value = str_replace(string=thick_value,
                                         pattern="Center",
                                         replacement="Centrist"),
               thick_value = factor(thick_value, levels=c("Left-wing voters",
                                                          'Centrist voters',
                                                          'Right-wing voters')),
               thin_value = factor(thin_value, levels=c('Low', 'High'))
               )
)
tab
## 
## plot
## ----
x = "estimate"
y = "term"
color="thin_value"
fill="thin_value"
shape="thin_value"
facet1='thick_value'
facet2='tasks_case'
dodge=.3
xlab = glue("Average Marginal Effect of Candidate ",
            "Attributes on Voter's Support for the Candidate")
ylab = "Candidates' Attributes"
leg_title = "Voters' Populist attitudes"
g = (
    tab
    %>% ggplot(.)
    + geom_errorbarh(aes_string(y=y, xmin="conf.low", xmax="conf.high",
                                color=fill),
                     height=.1, 
                     position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=shape), position = position_dodge(dodge))
    + geom_vline(aes(xintercept=0), linetype="dashed", col="red")
    + scale_y_discrete(limits=rev)
    + scale_colour_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_shape_manual(values=c( 25, 24))
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + facet_grid(paste(facet2, " ~ ", facet1)) 
    + ggguides(ncol=2)
    + theme(panel.border = element_rect(colour="black", fill='transparent'))
)
g
if (SAVE) {
    print(glue("Saving {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_SM_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=8.5, height=8.5 )}
}

## *** Figure G5 (RW vs RWP support; all tasks, first half, second half)


figure = 'fig-g5'
## 
thin = 'popmul2c'
thick = 'ideo'
tab = (
    res.rwp
    %>% filter(thin==!!thin & thick==!!thick) 
    %>% unnest(summ)
    %>% filter(!str_detect(term, pattern="Interc")) 
    %>% filter(!str_detect(term, pattern="LW")) 
    %>% mutate(term = stringr::str_replace_all(string=term,
                                               pattern='`RWP.*`',
                                               replacement=""),
               term = factor(term, levels= CAT_ORDER[['candidates']]),
               ## 
               thick_value = glue("{thick_value} voters"),
               thick_value = str_replace(string=thick_value,
                                         pattern="Center",
                                         replacement="Centrist"),
               thick_value = factor(thick_value, levels=c("Left-wing voters",
                                                          'Centrist voters',
                                                          'Right-wing voters')),
               ## 
               thin_value = factor(thin_value, levels=c('Low', 'High')),
               )
)
tab 
## plot
## ----
x = "term"
y = "estimate"
color="thin_value"
fill="thin_value"
shape="thin_value"
facet1="thick_value"
facet2='tasks_case'
dodge=.3
leg_title='Populist Attitudes'
ylab = glue("Average Marginal Effect of Candidate\n",
            "Attributes on Voters' Support for the Candidate")
g = (
    tab 
    %>% ggplot(.)
    + geom_rect(aes(xmin=0.5, xmax=2.5, ymin=-Inf, ymax=Inf),
                fill="grey", alpha=0.025, inherit_aes = F)
    + geom_rect(aes(xmin=4.5, xmax=6.5, ymin=-Inf, ymax=Inf),
                fill="grey", alpha=0.025, inherit_aes = F)
    + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high",
                               color=fill),
                    width=.05, position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=shape),
                 position=position_dodge(dodge))
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
    + scale_x_discrete(labels = c('<i>RW', '<b>RWP',
                                  '<i>RW', '<b>RWP',
                                  '<i>RW', '<b>RWP'))
    + scale_shape_manual(values=c(25,24))
    + scale_fill_grey(start = 0.7, end = 0,  na.value="red") 
    + scale_color_grey(start = 0.7, end = 0,  na.value="red") 
    + facet_grid2(paste(facet2, "~", facet1), scales='free',
                  independent='y')
    + ggguides(ncol=2)
    ## 
    + geom_text(aes_string(x=1.5, y="-Inf", label="'Anti-\nImmigration'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                ) 
    + geom_text(aes_string(x=3.5, y="-Inf", label="'Social\nconservative'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                ) 
    + geom_text(aes_string(x=5.5, y="-Inf", label="'Econonic\nliberal'"),
                colour="black", show_legend = F, parse=F,
                vjust=-.2, hjust=.50,
                position=position_dodge(0), angle=0, size=3,
                data=tibble(ideo3c='Center')
                )
    + labs(
          x        = NULL,
          y        = ylab,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = leg_title,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + theme(axis.text.x = element_markdown())
)
g
## 
if (SAVE)
{
    print(glue("Saving {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"))
    fns = file.path(PATH_SM_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=9.5, height=9 )}
}

## Difference on anti-immigration support
## --------------------------------------
tab = (
    res.rwp  
    %>% nest(-ntasks) 
    %>% mutate(res=future_map(.x=data, function(.x)
        .x
        %>% filter(thin=='popmul2c') 
        %>% filter(thin_value=='Low') 
        %>% filter(thick=='ideo') 
        %>% filter(!str_detect(thick_value, pattern="Center")) 
        %>% filter(str_detect(candprofile, pattern="anti-")) 
        %>% unnest(summ)
        %>% filter(str_detect(term, pattern="anti-")) 
        %>% filter(str_detect(term, pattern="RW ")) 
        %>% select(term, thin_value, thick_value, estimate)
        %>% tidyr::pivot_wider(id_cols=term,
                               names_from=thick_value,
                               values_from=estimate)
        %>% mutate(ratio_left_right = `Left-wing`/`Right-wing`)
        )) 
    %>% unnest(res)
)
tab

## Percentage of right-wing populists
## ---------------------------------
(
    df
    %>% distinct(rid, .keep_all=TRUE) 
    %>% mutate(rwp_voters = case_when(
                   popmul2c=='High' & ideo=='Right-wing' ~ 'Right-wing populist voters',
                   T ~ 'Others',
               ))
    %>% sjmisc::frq(rwp_voters)
)
## ** Other info

## failed conjoint attention checks
## --------------------------------
(
    dfmerged 
    %>% select(rid, att=attchkcj_keep) 
    %>% distinct(rid, .keep_all=TRUE) 
    %>% frq(att)
)

## * end of script

sessionInfo()


