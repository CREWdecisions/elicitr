
<!-- README.md is generated from README.Rmd. Please edit that file -->

# elicitr

<!-- badges: start -->

[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

<img src="man/figures/elicitr_logo_scaled.png" width="20%" />

Elicitr is an R package, used to aggregate elicitation data from
googlesheets. This package follows two formal elicitation methods:

- Continuous method (adapted from delphi-protocol for expert
  elicitation)
- Categorical method (adapted from Vernet et al. (2020, Biological
  invasion))

## Installation

You can install the development version of elicitr from
[GitHub](https://github.com/) with:

``` r
#install.packages("pak")
pak::pak("sgvignali/elicitr")
```

## Example

This simulated dataset can be used to unterstand how {elicitr} works.

Three types of variables are included here:

- “percentage” i.e. \[0,1\]
- “positive numbers” i.e. \[0,inf)
- “real numbers” i.e. (-inf,inf)

And three types of elicitations:

- One point estimate (var1_best)  
- Three point estimate (var2_min, var2_max, var2_best)  
- Four point estimate (var3_min, var3_max, var3_best, var3_conf)  

<!-- -->

    #>   id var1_best var2_min var2_max var2_best var3_min var3_max var3_best
    #> 1  1       0.8       45       63        53       -1       -6        -1
    #> 2  2       0.9       31       39        35      -18      -22       -17
    #> 3  3       0.7       42       48        46       -5      -19       -13
    #> 4  4       1.0       65       69        67        0       -8        -5
    #> 5  5       0.7       47       63        55      -19      -13       -15
    #> 6  6       1.0       46       54        51        0        0         1
    #>   var3_conf
    #> 1        89
    #> 2        93
    #> 3        98
    #> 4       100
    #> 5        81
    #> 6        60

This is a basic example which shows you how to use {elicitr} to analyse
this data:

``` r
#library(elicitr)
#elicitate(elicit) #function dbd
```

<img src="man/figures/README-plot_elicit_var2-1.png" width="100%" />

    #> List of 136
    #>  $ line                            :List of 6
    #>   ..$ colour       : chr "black"
    #>   ..$ linewidth    : num 0.5
    #>   ..$ linetype     : num 1
    #>   ..$ lineend      : chr "butt"
    #>   ..$ arrow        : logi FALSE
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_line" "element"
    #>  $ rect                            :List of 5
    #>   ..$ fill         : chr "white"
    #>   ..$ colour       : chr "black"
    #>   ..$ linewidth    : num 0.5
    #>   ..$ linetype     : num 1
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
    #>  $ text                            :List of 11
    #>   ..$ family       : chr ""
    #>   ..$ face         : chr "plain"
    #>   ..$ colour       : chr "black"
    #>   ..$ size         : num 11
    #>   ..$ hjust        : num 0.5
    #>   ..$ vjust        : num 0.5
    #>   ..$ angle        : num 0
    #>   ..$ lineheight   : num 0.9
    #>   ..$ margin       : 'margin' num [1:4] 0points 0points 0points 0points
    #>   .. ..- attr(*, "unit")= int 8
    #>   ..$ debug        : logi FALSE
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ title                           : NULL
    #>  $ aspect.ratio                    : NULL
    #>  $ axis.title                      : NULL
    #>  $ axis.title.x                    :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : NULL
    #>   ..$ size         : NULL
    #>   ..$ hjust        : NULL
    #>   ..$ vjust        : num 1
    #>   ..$ angle        : NULL
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : 'margin' num [1:4] 2.75points 0points 0points 0points
    #>   .. ..- attr(*, "unit")= int 8
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ axis.title.x.top                :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : NULL
    #>   ..$ size         : NULL
    #>   ..$ hjust        : NULL
    #>   ..$ vjust        : num 0
    #>   ..$ angle        : NULL
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : 'margin' num [1:4] 0points 0points 2.75points 0points
    #>   .. ..- attr(*, "unit")= int 8
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ axis.title.x.bottom             : NULL
    #>  $ axis.title.y                    :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : NULL
    #>   ..$ size         : NULL
    #>   ..$ hjust        : NULL
    #>   ..$ vjust        : num 1
    #>   ..$ angle        : num 90
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : 'margin' num [1:4] 0points 2.75points 0points 0points
    #>   .. ..- attr(*, "unit")= int 8
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ axis.title.y.left               : NULL
    #>  $ axis.title.y.right              :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : NULL
    #>   ..$ size         : NULL
    #>   ..$ hjust        : NULL
    #>   ..$ vjust        : num 1
    #>   ..$ angle        : num -90
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.75points
    #>   .. ..- attr(*, "unit")= int 8
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ axis.text                       :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : chr "grey30"
    #>   ..$ size         : 'rel' num 0.8
    #>   ..$ hjust        : NULL
    #>   ..$ vjust        : NULL
    #>   ..$ angle        : NULL
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : NULL
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ axis.text.x                     :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : NULL
    #>   ..$ size         : NULL
    #>   ..$ hjust        : NULL
    #>   ..$ vjust        : num 1
    #>   ..$ angle        : NULL
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : 'margin' num [1:4] 2.2points 0points 0points 0points
    #>   .. ..- attr(*, "unit")= int 8
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ axis.text.x.top                 :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : NULL
    #>   ..$ size         : NULL
    #>   ..$ hjust        : NULL
    #>   ..$ vjust        : num 0
    #>   ..$ angle        : NULL
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : 'margin' num [1:4] 0points 0points 2.2points 0points
    #>   .. ..- attr(*, "unit")= int 8
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ axis.text.x.bottom              : NULL
    #>  $ axis.text.y                     :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : NULL
    #>   ..$ size         : NULL
    #>   ..$ hjust        : num 1
    #>   ..$ vjust        : NULL
    #>   ..$ angle        : NULL
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 0points
    #>   .. ..- attr(*, "unit")= int 8
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ axis.text.y.left                : NULL
    #>  $ axis.text.y.right               :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : NULL
    #>   ..$ size         : NULL
    #>   ..$ hjust        : num 0
    #>   ..$ vjust        : NULL
    #>   ..$ angle        : NULL
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.2points
    #>   .. ..- attr(*, "unit")= int 8
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ axis.text.theta                 : NULL
    #>  $ axis.text.r                     :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : NULL
    #>   ..$ size         : NULL
    #>   ..$ hjust        : num 0.5
    #>   ..$ vjust        : NULL
    #>   ..$ angle        : NULL
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 2.2points
    #>   .. ..- attr(*, "unit")= int 8
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ axis.ticks                      :List of 6
    #>   ..$ colour       : chr "grey20"
    #>   ..$ linewidth    : NULL
    #>   ..$ linetype     : NULL
    #>   ..$ lineend      : NULL
    #>   ..$ arrow        : logi FALSE
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_line" "element"
    #>  $ axis.ticks.x                    : NULL
    #>  $ axis.ticks.x.top                : NULL
    #>  $ axis.ticks.x.bottom             : NULL
    #>  $ axis.ticks.y                    : NULL
    #>  $ axis.ticks.y.left               : NULL
    #>  $ axis.ticks.y.right              : NULL
    #>  $ axis.ticks.theta                : NULL
    #>  $ axis.ticks.r                    : NULL
    #>  $ axis.minor.ticks.x.top          : NULL
    #>  $ axis.minor.ticks.x.bottom       : NULL
    #>  $ axis.minor.ticks.y.left         : NULL
    #>  $ axis.minor.ticks.y.right        : NULL
    #>  $ axis.minor.ticks.theta          : NULL
    #>  $ axis.minor.ticks.r              : NULL
    #>  $ axis.ticks.length               : 'simpleUnit' num 2.75points
    #>   ..- attr(*, "unit")= int 8
    #>  $ axis.ticks.length.x             : NULL
    #>  $ axis.ticks.length.x.top         : NULL
    #>  $ axis.ticks.length.x.bottom      : NULL
    #>  $ axis.ticks.length.y             : NULL
    #>  $ axis.ticks.length.y.left        : NULL
    #>  $ axis.ticks.length.y.right       : NULL
    #>  $ axis.ticks.length.theta         : NULL
    #>  $ axis.ticks.length.r             : NULL
    #>  $ axis.minor.ticks.length         : 'rel' num 0.75
    #>  $ axis.minor.ticks.length.x       : NULL
    #>  $ axis.minor.ticks.length.x.top   : NULL
    #>  $ axis.minor.ticks.length.x.bottom: NULL
    #>  $ axis.minor.ticks.length.y       : NULL
    #>  $ axis.minor.ticks.length.y.left  : NULL
    #>  $ axis.minor.ticks.length.y.right : NULL
    #>  $ axis.minor.ticks.length.theta   : NULL
    #>  $ axis.minor.ticks.length.r       : NULL
    #>  $ axis.line                       : list()
    #>   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    #>  $ axis.line.x                     : NULL
    #>  $ axis.line.x.top                 : NULL
    #>  $ axis.line.x.bottom              : NULL
    #>  $ axis.line.y                     : NULL
    #>  $ axis.line.y.left                : NULL
    #>  $ axis.line.y.right               : NULL
    #>  $ axis.line.theta                 : NULL
    #>  $ axis.line.r                     : NULL
    #>  $ legend.background               :List of 5
    #>   ..$ fill         : NULL
    #>   ..$ colour       : logi NA
    #>   ..$ linewidth    : NULL
    #>   ..$ linetype     : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
    #>  $ legend.margin                   : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
    #>   ..- attr(*, "unit")= int 8
    #>  $ legend.spacing                  : 'simpleUnit' num 11points
    #>   ..- attr(*, "unit")= int 8
    #>  $ legend.spacing.x                : NULL
    #>  $ legend.spacing.y                : NULL
    #>  $ legend.key                      : NULL
    #>  $ legend.key.size                 : 'simpleUnit' num 1.2lines
    #>   ..- attr(*, "unit")= int 3
    #>  $ legend.key.height               : NULL
    #>  $ legend.key.width                : NULL
    #>  $ legend.key.spacing              : 'simpleUnit' num 5.5points
    #>   ..- attr(*, "unit")= int 8
    #>  $ legend.key.spacing.x            : NULL
    #>  $ legend.key.spacing.y            : NULL
    #>  $ legend.frame                    : NULL
    #>  $ legend.ticks                    : NULL
    #>  $ legend.ticks.length             : 'rel' num 0.2
    #>  $ legend.axis.line                : NULL
    #>  $ legend.text                     :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : NULL
    #>   ..$ size         : 'rel' num 0.8
    #>   ..$ hjust        : NULL
    #>   ..$ vjust        : NULL
    #>   ..$ angle        : NULL
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : NULL
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ legend.text.position            : NULL
    #>  $ legend.title                    :List of 11
    #>   ..$ family       : NULL
    #>   ..$ face         : NULL
    #>   ..$ colour       : NULL
    #>   ..$ size         : NULL
    #>   ..$ hjust        : num 0
    #>   ..$ vjust        : NULL
    #>   ..$ angle        : NULL
    #>   ..$ lineheight   : NULL
    #>   ..$ margin       : NULL
    #>   ..$ debug        : NULL
    #>   ..$ inherit.blank: logi TRUE
    #>   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    #>  $ legend.title.position           : NULL
    #>  $ legend.position                 : chr "right"
    #>  $ legend.position.inside          : NULL
    #>  $ legend.direction                : NULL
    #>  $ legend.byrow                    : NULL
    #>  $ legend.justification            : chr "center"
    #>  $ legend.justification.top        : NULL
    #>  $ legend.justification.bottom     : NULL
    #>  $ legend.justification.left       : NULL
    #>  $ legend.justification.right      : NULL
    #>  $ legend.justification.inside     : NULL
    #>  $ legend.location                 : NULL
    #>  $ legend.box                      : NULL
    #>  $ legend.box.just                 : NULL
    #>  $ legend.box.margin               : 'margin' num [1:4] 0cm 0cm 0cm 0cm
    #>   ..- attr(*, "unit")= int 1
    #>  $ legend.box.background           : list()
    #>   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    #>  $ legend.box.spacing              : 'simpleUnit' num 11points
    #>   ..- attr(*, "unit")= int 8
    #>   [list output truncated]
    #>  - attr(*, "class")= chr [1:2] "theme" "gg"
    #>  - attr(*, "complete")= logi TRUE
    #>  - attr(*, "validate")= logi TRUE

## Related work

The functioning and application of {elicitr} is related to following R
packages:

- {shelf} : <https://shelf.sites.sheffield.ac.uk/software>
- {prefR} : <https://jlepird.github.io/prefeR/>

## Bibliography

We used R version 4.4.1 (R Core Team 2024) and the following R packages:
devtools v. 2.4.5 (Wickham et al. 2022), knitr v. 1.48 (Xie 2014, 2015,
2024), rmarkdown v. 2.28 (Xie, Allaire, and Grolemund 2018; Xie,
Dervieux, and Riederer 2020; Allaire et al. 2024), tidyverse v. 2.0.0
(Wickham et al. 2019), truncnorm v. 1.0.9 (Mersmann et al. 2023).

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-rmarkdown2024" class="csl-entry">

Allaire, JJ, Yihui Xie, Christophe Dervieux, Jonathan McPherson, Javier
Luraschi, Kevin Ushey, Aron Atkins, et al. 2024.
*<span class="nocase">rmarkdown</span>: Dynamic Documents for r*.
<https://github.com/rstudio/rmarkdown>.

</div>

<div id="ref-truncnorm" class="csl-entry">

Mersmann, Olaf, Heike Trautmann, Detlef Steuer, and Björn Bornkamp.
2023. *<span class="nocase">truncnorm</span>: Truncated Normal
Distribution*. <https://CRAN.R-project.org/package=truncnorm>.

</div>

<div id="ref-base" class="csl-entry">

R Core Team. 2024. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-devtools" class="csl-entry">

Wickham, Hadley, Jim Hester, Winston Chang, and Jennifer Bryan. 2022.
*<span class="nocase">devtools</span>: Tools to Make Developing r
Packages Easier*. <https://CRAN.R-project.org/package=devtools>.

</div>

<div id="ref-knitr2014" class="csl-entry">

Xie, Yihui. 2014. “<span class="nocase">knitr</span>: A Comprehensive
Tool for Reproducible Research in R.” In *Implementing Reproducible
Computational Research*, edited by Victoria Stodden, Friedrich Leisch,
and Roger D. Peng. Chapman; Hall/CRC.

</div>

<div id="ref-knitr2015" class="csl-entry">

———. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca Raton,
Florida: Chapman; Hall/CRC. <https://yihui.org/knitr/>.

</div>

<div id="ref-knitr2024" class="csl-entry">

———. 2024. *<span class="nocase">knitr</span>: A General-Purpose Package
for Dynamic Report Generation in r*. <https://yihui.org/knitr/>.

</div>

<div id="ref-rmarkdown2018" class="csl-entry">

Xie, Yihui, J. J. Allaire, and Garrett Grolemund. 2018. *R Markdown: The
Definitive Guide*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown>.

</div>

<div id="ref-rmarkdown2020" class="csl-entry">

Xie, Yihui, Christophe Dervieux, and Emily Riederer. 2020. *R Markdown
Cookbook*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown-cookbook>.

</div>

</div>
