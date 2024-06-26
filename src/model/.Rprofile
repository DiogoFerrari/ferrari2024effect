    options(warn=-1)

    if (!require("RColorBrewer", quietly=T)) install.packages('RColorBrewer')
    if (!require("ggplot2", quietly=T)) install.packages('ggplot2')
    if (!require("gtsummary", quietly=T))
        install.packages('gtsummary', repos='https://cloud.r-project.org')
    if (!require("ggdark", quietly=T)) install.packages('ggdark', repos='https://cloud.r-project.org')

    library(RColorBrewer, quietly=T)
    library(ggplot2, quietly=T)
    library(gtsummary, quietly=T)
    library(ggdark, quietly=T)


    ## plots
    ## -----
    ## colors
    DISCRETE_COLORS = c(
        ## "black",
        ## "darkgray",
        '#1f77b4', # blue
        '#ff7f0e', # orange
        '#2ca02c',
        '#d62728',
        '#9467bd',
        '#8c564b',
        '#e377c2',
        '#7f7f7f',
        '#bcbd22',
        '#17becf'
        ## "#E41A1C",
        ## "#377EB8",
        ## "#4DAF4A",
        ## "#984EA3",
        ## "#FF7F00"
    )
    DISCRETE_COLORS_GREY = c(
        "black",
        "darkgray",
        '#1f77b4', # blue
        '#ff7f0e', # orange
        '#2ca02c',
        '#d62728',
        '#9467bd',
        '#8c564b',
        '#e377c2',
        '#7f7f7f',
        '#bcbd22',
        '#17becf'
        ## "#E41A1C",
        ## "#377EB8",
        ## "#4DAF4A",
        ## "#984EA3",
        ## "#FF7F00"
    )
    scale_colour_discrete = function(...) scale_colour_manual(..., values=DISCRETE_COLORS)
    scale_fill_discrete   = function(...) scale_fill_manual  (... , values=DISCRETE_COLORS)


    update_geom_defaults("point", list(shape = 21, colour='white', size=3,
                                       fill="#00000044"))

    ## theme
    ggguides = function(ncol=10, leg_title_pos='top',
                        title_hjust=0, title_vjust=.5,
                        keywidth=2.7,
                        keyheight=.9
                        )
    {
        guides(
            colour = guide_legend(title.position = leg_title_pos,
                                  ncol=ncol,
                                  size=8,
                                  keywidth=keywidth,
                                  keyheight=keyheight,
                                  title.vjust = title_vjust,
                                  title.hjust = title_hjust
                                  ),
            fill = guide_legend(title.position = leg_title_pos,
                                ncol=ncol,
                                size=8,
                                keywidth=keywidth,
                                keyheight=keyheight,
                                title.vjust = title_vjust,
                                title_hjust=title_hjust
                                ),
            shape = guide_legend(title.position = leg_title_pos,
                                 ncol=ncol,
                                 size=8,
                                 keywidth=keywidth,
                                 keyheight=keyheight,
                                 title.vjust = title_vjust,
                                 title_hjust=title_hjust
                                 ),
            ## size = guide_legend(title.position = leg_title_pos,
            ##                      ncol=ncol,
            ##                      size=8,
            ##                      keywidth=keywidth,
            ##                      keyheight=keyheight,
            ##                      title.vjust = title_vjust,
            ##                      title_hjust=title_hjust
            ##                      ),
            linetype = guide_legend(title.position = leg_title_pos,
                                    ncol=ncol,
                                    size=8,
                                    keywidth=keywidth,
                                    keyheight=keyheight,
                                    title.vjust = title_vjust,
                                    title_hjust=title_hjust),
            )
    }
    ggtheme <- function(base_size=12,
                        base_family='Helvetica',
                        base_line_size=base_size/24,
                        base_rect_size=base_size/24,
                        border=FALSE,
                        gray=FALSE,
                        ## 
                        color_border = 'black',
                        ## 
                        facet_title_size = 10.5
                        )
    { 
        ## 
        ## font <- "Georgia"   #assign font family up front
        ## 
        return_theme <- theme_bw() %+replace%    #replace elements we want to change
            ## 
            theme(
                ## ----------------------------------------------
                legend.position = "top",
                legend.justification = c(0, .9),
                legend.direction='horizontal',
                legend.title = element_text(size=11, face='bold', hjust = 0),
                legend.title.align = 0,
                legend.background  = element_rect(
                    fill='transparent',
                    colour='transparent',
                    ## to remove box around it, set size to 0
                    linewidth=0),
                ## ----------------------------------------------
                strip.background = element_rect(colour="transparent",
                                                fill='transparent'),
                strip.text.x = element_text(size=facet_title_size ,
                                            face='bold.italic',
                                            color='gray30',
                                            hjust=0
                                            ),
                strip.text.y = element_text(size=9, face="bold", vjust=0,
                                            angle=-90),
                ## ----------------------------------------------
                panel.grid.minor.y = element_line(colour="grey80", linewidth=.4, linetype=3),
                panel.grid.major.y = element_line(colour="grey80", linewidth=.4, linetype=3),
                panel.grid.minor.x = element_line(colour="grey80", linewidth=.4, linetype=3),
                panel.grid.major.x = element_line(colour="grey80", linewidth=.4, linetype=3),
                panel.border      = element_blank(),
                ## ----------------------------------------------
                axis.line.x       = element_line(colour="black", linewidth=.2, linetype=1),
                ## axis.line.y       = element_line(colour="black", size=.2, linetype=1),
                axis.ticks.x        = element_blank(),
                axis.ticks.y        = element_blank(),
                ## ----------------------------------------------
                axis.title.y       = element_text(size=10, angle=90, face='bold', colour='grey13', vjust = +3),
                axis.title.x       = element_text(size=10, angle=0, face='bold', colour='grey13'),
                ## ----------------------------------------------
                plot.title	    = element_text(
                    ## family = font,            #set font family
                    hjust=0.01,
                    size = 13,
                    colour='grey40',
                    face='bold'
                ),
                plot.subtitle	   = element_text(
                    ## family = font,            #set font family
                    hjust=0.01,
                    vjust=-.5,
                    size = 10,
                    colour='grey30'
                ),
                plot.caption = element_text(size = 8.5, hjust = 1,
                                            ## family = "arial",
                                            ## color = "black",
                                            face = "italic")
            ) +
            theme(
                legend.direction = "vertical",
                legend.justification = c(0,0),
                panel.border       = element_blank(),
                axis.line.x.bottom = element_line( color='grey'),
                axis.line.y        = element_blank(),
                axis.line.y.left   = element_blank(),
                axis.line.y.right  = element_blank(),
                panel.grid.major.x=element_blank(),
                panel.grid.minor.x=element_blank()
            ) 
        if (border) {
            return_theme <-
                return_theme +
                ggplot2::theme(panel.border =
                                   ggplot2::element_rect(fill = NA,
                                                         color = color_border,
                                                         linewidth = base_rect_size))
        }
        return(return_theme)
    }
    theme_set(ggtheme())
    ggtheme_dark <- function(base_size=12,
                             base_family='Helvetica',
                             base_line_size=base_size/24,
                             base_rect_size=base_size/24,
                             border=FALSE,
                             gray=FALSE,
                             ## 
                             color_border = 'black',
                             ## 
                             facet_title_size = 10.5
                             )
    { 
        ## 
        ## font <- "Georgia"   #assign font family up front
        ## 
        
        return_theme <- dark_theme_gray() %+replace%    #replace elements we want to change
            ## 
            theme(
                ## ----------------------------------------------
                legend.position = "top",
                legend.justification = c(0, .9),
                legend.direction='horizontal',
                legend.title = element_text(size=11, face='bold', hjust = 0),
                legend.title.align = 0,
                legend.background  = element_rect(
                    fill='transparent',
                    colour='transparent',
                    ## to remove box around it, set size to 0
                    linewidth=0),
                ## ----------------------------------------------
                strip.background = element_rect(colour="transparent",
                                                fill='transparent'),
                strip.text.x = element_text(
                    face='bold.italic',
                    color='white',
                    hjust=0, size=9
                ),
                strip.text.y = element_text(
                    face='bold.italic',
                    size=9,  vjust=0,
                    angle=-90),
                ## ----------------------------------------------
                panel.grid.minor.y = element_line(colour="grey80", linewidth=.1, linetype=3),
                panel.grid.major.y = element_line(colour="grey80", linewidth=.1, linetype=3),
                panel.grid.minor.x = element_line(colour="grey80", linewidth=.1, linetype=3),
                panel.grid.major.x = element_line(colour="grey80", linewidth=.1, linetype=3),
                panel.border      = element_blank(),
                ## ----------------------------------------------
                axis.line.x       = element_line(colour="black", linewidth=.2, linetype=1),
                ## axis.line.y       = element_line(colour="black", size=.2, linetype=1),
                axis.ticks.x        = element_blank(),
                axis.ticks.y        = element_blank(),
                ## ----------------------------------------------
                axis.title.y       = element_text(size=10, angle=90, face='bold', colour='white', vjust = +3),
                axis.title.x       = element_text(size=10, angle=0, face='bold', colour='white'),
                ## ----------------------------------------------
                plot.title	    = element_text(
                    ## family = font,            #set font family
                    hjust=0.01,
                    size = 13,
                    colour='grey40',
                    face='bold'
                ),
                plot.subtitle	   = element_text(
                    ## family = font,            #set font family
                    hjust=0.01,
                    vjust=-.5,
                    size = 10,
                    colour='grey30'
                ),
                plot.caption = element_text(size = 8.5, hjust = 1,
                                            ## family = "arial",
                                            ## color = "black",
                                            face = "italic")
            ) +
            theme(
                legend.direction = "vertical",
                legend.justification = c(0,0),
                panel.border       = element_blank(),
                axis.line.x.bottom = element_line( color='grey'),
                axis.line.y        = element_blank(),
                axis.line.y.left   = element_blank(),
                axis.line.y.right  = element_blank(),
                panel.grid.major.x=element_blank(),
                panel.grid.minor.x=element_blank()
            ) 
        if (border) {
            return_theme <-
                return_theme +
                ggplot2::theme(panel.border =
                                   ggplot2::element_rect(fill = NA,
                                                         color = color_border,
                                                         linewidth = base_rect_size))
        }
        return(return_theme)
    }

    ## tibble
    ## ------
    options(
        warn=-1,
        ## digits
        digits=4,
        scipen = 999,
        ## rows
        tibble.print_min = 6,
        tibble.print_max = 6,
        ## 
        pillar.bold= TRUE,
        pillar.quite= TRUE,
        pillar.min_title_chars=30,
        ## this prevents tibble to print in gray on emacs
        pillar.subtle = FALSE,
        rlang_backtrace_on_error = "none",
        ## columns to print
        pillar.width = 100,
        width = 110,
        tibble.max_extra_cols = 0,
        ## 
        pillar.min_chars=3,
        pillar.advice=FALSE,
        pillar.trunc=T,
        ## 
        tibble.max_footer_lines = 0,
        ## control digits 
        pillar.sigfig = 3,
        ## pillar.subtle_num=TRUE,
        ## control decimal notation
        pillar.max_dec_width=20,
        ##
        knitr.kable.NA = ''
    )
    ## https://pillar.r-lib.org/reference/pillar_options.html
    ## https://tibble.tidyverse.org/articles/numbers.html
    ## https://tibble.tidyverse.org/articles/digits.html

    ## gtsummary
    ## ---------
    ## function helps to execute it quietly
    theme_gtsummary_continuous2(
        statistic = "{mean} ({sd}) {median} ({p25}, {p75})",
        set_theme = TRUE
    )
    theme_gtsummary_compact(set_theme = TRUE, font_size = 5)
    theme_gtsummary_printer(print_engine="huxtable", set_theme = TRUE)
    theme_gtsummary_language(language="en", iqr.sep = NULL,
                             ci.sep = NULL, set_theme = TRUE)
    theme_gtsummary_mean_sd(set_theme = TRUE)
    theme_gtsummary_eda(set_theme = TRUE)

    ## modelsummary
    ## ------------
    options(modelsummary_factory_default = "markdown")

    cat("\n- Settings loaded...\n")

    ## This is to avoid corrupting color after error in emacs REPL
    invisible(addTaskCallback(function(...) {
        if (interactive()) {
                                        # Remember to install crayon
            try(cat(crayon::reset("")), silent = TRUE)
        }
        TRUE
    }, name = "ansi_reset"))

    ## verbose error
    ## options(error = function() {traceback(2, max.lines=10);
    ##     if(!interactive()) quit(save="no", status=1, runLast=T)})
