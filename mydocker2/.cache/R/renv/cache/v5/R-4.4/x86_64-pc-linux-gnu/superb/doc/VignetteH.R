## ----echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'-----------
cat("this will be hidden; use for general initializations.\n")
library(superb)
options("superb.feedback" = c("none"))
library(ggplot2)

# generate random data
dta <- GRD(SubjectsPerGroup = 20,
    BSFactors = c("Profile(ProfileA,ProfileB,ProfileC)","Indicator(Visual,Spatial,Verbal,Tactile,Semantic)"),
    Population = list(mean=50,stddev=15),
    Effects = list("Profile*Indicator" = custom(-30,0,30,10,10,10,30,-20,30,0,0,0,0,0,0)))
dta$id = rep(1:60,5)

## ----warning=FALSE, message=FALSE, fig.height=4, fig.width=7, fig.cap = "**Figure 1**. Plot of the 5 indicator variables."----
superb( DV ~ Indicator, data = dta,
        plotStyle   = "pointlinejitter",
        adjustments = list(purpose = "difference")
    ) + theme_bw() + ylim(0,100) + ylab("Score")

## ----warning=FALSE, message=FALSE, fig.height=4, fig.width=7, fig.cap = "**Figure 2**. Plot of the 5 indicator variables and the three profiles."----
superb( DV ~ Indicator + Profile, data = dta,
        plotStyle   = "pointlinejitter",
        adjustments = list(purpose = "difference") 
    ) + theme_bw() + ylim(0,100) + ylab("Score")

## ----warning=FALSE, message=FALSE, fig.height=4, fig.width=7, fig.cap = "**Figure 3**. Plot of the 5 indicator variables and the three profiles in polar coordinate."----
superb( DV ~ Indicator + Profile, data = dta,
        plotStyle = "circularpointlinejitter"         ## only change!
    ) + theme_bw() + ylim(0,100) + ylab("Score")

## ----warning=FALSE, message=FALSE, fig.height=4, fig.width=7, fig.cap = "**Figure 4**. Plot of the 5 indicator variables and the three profiles back in cartesian coordinate."----
superb( DV ~ Indicator + Profile, data = dta,
        plotStyle = "circularpointlinejitter"
    ) + theme_bw() + ylim(0,100) + ylab("Score") + 
    coord_cartesian()                                  ## only change!

## ----warning=FALSE, message=FALSE, fig.height=4, fig.width=7, fig.cap = "**Figure 5**. Plot with some customization"----
library(RColorBrewer)
superb( DV ~ Indicator + Profile, data = dta,
        plotStyle      = "circularpointlinejitter",        
        lineParams     = list(linewidth=1.2, alpha = 0.3 ),
        errorbarParams = list(width=0.3,linewidth=1.2 ),
        jitterParams   = list(size=1,alpha=0.33 ),
    ) + theme_bw() + ylim(0,100) + ylab("Score") +
    theme(panel.border = element_blank(), text = element_text(size = 16) ) +
    scale_color_brewer(palette="Dark2")

## ----warning=FALSE, message=FALSE, fig.height=4, fig.width=7, fig.cap = "**Figure 6**. Plot with some difference adjustment"----
superb( DV ~ Indicator + Profile, data = dta,
        adjustments    = list(purpose = "difference"), ## new addition
        plotStyle      = "circularpointlinejitter",        
        lineParams     = list(linewidth=1.2, alpha = 0.3 ),
        errorbarParams = list(width=0.2,linewidth=1.2 ),
        jitterParams   = list(size=1,alpha=0.33 ),
    ) + theme_bw() + ylim(0,100) + ylab("Score") +
    theme(panel.border = element_blank(), text = element_text(size = 16) ) +
    scale_color_brewer(palette="Dark2")

## ----eval = FALSE, results = 'hide'-------------------------------------------
#      scale_x_continuous(
#              oob = scales::oob_keep,
#              limits =   c(0, 0.00001+NUMBER_OF_CATEGORIES ),
#              n.breaks = NUMBER_OF_CATEGORIES +1
#          )

## ----warning=FALSE, message=FALSE, fig.height=4, fig.width=7, fig.cap = "**Figure 7**. Plot turning counterclockwise"----
superb( DV ~ Indicator + Profile, data = dta,
        adjustments    = list(purpose = "difference"), ## new addition
        plotStyle      = "circularpointlinejitter",        
        lineParams     = list(linewidth=1.2, alpha = 0.3 ),
        errorbarParams = list(width=0.2,linewidth=1.2 ),
        jitterParams   = list(size=1,alpha=0.33 ),
        radarParams    = list( direction = -1, start = 2*pi *4/5)
    ) + theme_bw() + ylim(0,100) + ylab("Score") +
    theme(panel.border = element_blank(), text = element_text(size = 16) ) +
    scale_color_brewer(palette="Dark2")

## ----eval = FALSE, results = 'hide'-------------------------------------------
#  dta <- GRD(
#      SubjectsPerGroup = 20,
#      BSFactors        = c("Profile(ProfileA,ProfileB,ProfileC)","Indicator(Visual,Spatial,Verbal,Tactile,Semantic)"),
#      Population       = list(mean=50,stddev=15),
#      Effects          = list("Profile*Indicator" = custom(-30,0,30,10,10,10,30,-20,30,0,0,0,0,0,0)))

