

DEPVAR = 'chr'
N_TASKS = 15
N_ATTRIBUTES = 8
##
COVARS = c(
    'popmin2c',
    'popadd2c',
    'popmul2c',
    'ptyid',
    'libcons',
    'ideo'
)
COVARS_SM = c(
    "age",
    "agec",
    'inc10c',
    'educ',
    'female.resp',
    'race',
    ## 
    'libcons.raw',
    'ideo.raw',
    'popmul'
    )

ATT = c(
    "cand.rwl" = "Economic liberalism",
    "cand.rwc" = "Social conservatism",
    "cand.rwi" = "Anti-immigration",
    ## 
    "cand.ppl" = "People centrism",
    "cand.ant" = "Anti-elitism",
    "cand.man" = "Manichean outlook",
    "cand.pop" = "Populism",
    ## 
    "cand.out" = "Prior political experience",
    "cand.fem" = "Gender"
)
CAT_ORDER=list(
    'popmin2c'=c('Low', 'High'),
    'popmul2c'=c('Low', 'High'),
    'popadd2c'=c('Low', 'High'),
    # 
    'ptyid'   =c('Democrat', 'Independent', 'Republican'),
    "libcons" = c('Liberal', 'Neither', 'Conservative'),
    "ideo"    = c('Left-wing', 'Center', 'Right-wing'),
    "candidates" = c('RW anti-immigration',
                     'RWP anti-immigration',
                     'RW social conservative',
                     'RWP social conservative',
                     'RW economic liberalism',
                     'RWP economic liberalism'
                     ),
    "candidates_lw" = c('LW pro-immigration',
                        'LWP pro-immigration',
                        'LW social liberal',
                        'LWP social liberal',
                        'LW pro-welfare',
                        'LWP pro-welfare'
                     )
 )
COVARS_RAW = c(
    "gender",
    "age",
    "educ",
    "latino",
    "race",
    "inc",
    "nhh",
    'ppl1',
    'ppl2',
    'ppl3',
    'man1',
    'man2',
    'man3',
    'ant1',
    'ant2',
    'ant3',
    "libcons",
    "ideo",
    "ptyid",
    "ptyid_rep",
    "ptyid_dem",
    "ptyid_ind"
)

