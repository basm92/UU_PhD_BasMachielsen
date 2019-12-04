



























Possibilities and packages
--------------------------

A small disclaimer..

-   There are various possibilities and packages in R to make tidy,
    well-formatted tables
-   I do not claim to know, let alone be familiar with, even a tiny
    fraction of them
-   I do know how to use a few particular ones, most notable `sjPlot`
    and `stargazer`, which are among the most popular and most-used
    packages for this purpose, whose features I will demonstrate
    throughout this lecture
-   I will also show a couple of alternatives to circumvent the
    shortcomings of these packages.
-   Stargazer has a vignette available
    [**here**](https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf),
    which is an excellent tutorial in its own right, and sjPlot has a
    few available tutorials
    [**here**](http://www.strengejacke.de/sjPlot/articles/)

Why use them?
-------------

-   What is stargazer?

> stargazer is an R package that creates LATEX code, HTML code and ASCII
> text for well-formatted regression tables, with multiple models
> side-by-side, as well as for summary statistics tables, data frames,
> vectors and matrices.

-   Why should you use stargazer? From the vignette:

> Compared to available alternatives, stargazer excels in at least three
> respects: its ease of use, the large number of models it supports, and
> its beautiful aesthetics. These advantages have made it the R-to-LATEX
> package of choice for many satisfied users at research and teaching
> institutions around the world.

Why use them?
-------------

-   What is sjPlot?

> \[a\] collection of plotting and table output functions for data
> visualization. Results of various statistical analyses (that are
> commonly used in social sciences) can be visualized using this
> package, including simple and cross tabulated frequencies, histograms,
> box plots, (generalized) linear models, mixed effects models, PCA and
> correlation matrices, cluster analyses, scatter plots, Likert scales,
> effects plots of interaction terms in regression models, constructing
> index or score variables and much more.

-   sjPlot has way more features than stargazer. We will not focus on
    these features in this session.

Descriptive statistics - stargazer
----------------------------------

We will start out by using stargazer in format. The advantage of a
format is that you can use it right away in a presentation, such as this
one!

    #Load an example dataset
    #the data comes from Hamermesh and Parker (2005)
    #about impact of beauty on teacher evaluations

    data(TeachingRatings)

    stargazer(TeachingRatings, 
              header=FALSE, type='latex',
              summary.stat = c("mean","min","max","n"), 
              font.size = "footnotesize")

Descriptive statistics - stargazer
----------------------------------

-   This is the output you get:

Descriptive statistics - stargazer
----------------------------------

-   The option list of stargazer is very extensive. For example, the
    same output can be achieved by omitting several (non-default)
    statistics:

<!-- -->

    #Load an example dataset
    #the data comes from a Hamermesh and Parker (2005)
    #about impact of beauty on teacher evaluations

    data(TeachingRatings)

    stargazer(TeachingRatings, 
              header=FALSE, type='latex',
              omit.summary.stat = c("sd", "p25", "p75"), 
              font.size = "footnotesize",
              title = "Hello, R Cafe!"
              )

Descriptive statistics - stargazer
----------------------------------

The output you get is the following:

Please refer to the stargazer vignette
[**here**](https://cran.r-project.org/web/packages/stargazer/stargazer.pdf#stargazer_summary_stat_code_list)
to look for the specific summary statistic codes.

Descriptive statistics - stargazer
----------------------------------

-   Hopefully the advantages have been clear. The syntax is amazingly
    simple and the number of options is huge (Have a look at
    `?stargazer`). It automatically filters NA observations. It is also
    easy to integrate these tables into (R)Markdown and documents.

Now, a few disadvantages..

-   You have to specify the option `header = FALSE` in order to bypass
    the default output which contains the credit of the package creator
    (try it!)

-   stargazer automatically extracts the variables which are suitable
    for descriptive statistics out of your dataset. If you want a
    summarise of, e.g., factor variables, you have to convert them.

Descriptive statistics - stargazer in Word
------------------------------------------

-   Stargazer also supports exporting tables to Microsoft Word.

-   It is possible to export tables in one file, and to overwrite or
    append these documents. (This is also supported for .tex files.)

    #Let's try to extract another dataset, CPS1988,
    #about the determinants of wages
    #and summarise the descriptives in a Word table.
    data(CPS1988)

    stargazer(CPS1988[CPS1988$ethnicity == "cauc",],
              header=FALSE, type='latex',
              omit.summary.stat = c("p25", "p75"), 
              font.size = "footnotesize",
              title = "Caucasian")

    stargazer(CPS1988[CPS1988$ethnicity == "afam",],
              header=FALSE, type='latex',
              omit.summary.stat = c("p25", "p75"), 
              font.size = "footnotesize",
              title = "Caucasian")

Descriptive statistics - stargazer in Word
------------------------------------------

This is the output of the two tables:

Descriptive statistics - stargazer in Word
------------------------------------------

-   Exporting tables to Word is possible “natively” in 2 ways:

    1.  First, output to .html, and copy the resulting tables from your
        web browser to Word
    2.  Second, output each separate stargazer command to different .doc
        documents

-   Stargazer does not yet support appending existing documents with new
    tables, and automatically overwrites tables.

Syntax:

    stargazer(CPS1988[CPS1988$ethnicity == "cauc",],
                header=FALSE, omit.summary.stat = c("p25", "p75"),
                font.size = "footnotesize",
                title = "Caucasian",
                type='html', out = "test1.doc") #These 2 arguments are the only 2 you 
                                                #need

    stargazer(CPS1988[CPS1988$ethnicity == "afam",],
              header=FALSE, omit.summary.stat = c("p25", "p75"), 
              font.size = "footnotesize",
              title = "African American",
              type='html', out = "test2.doc") #These 2 arguments are the only 2 you 
                                              #need

Descriptive statistics - stargazer in Word
------------------------------------------

-   However, if you want to append, you can use a little bit of R’s
    power in the following way:

    #Step 1, make a list for all the 
    #partitions of the data, include names
    models <-  list(
        Caucasian = CPS1988[CPS1988$ethnicity == "cauc",],
        AfroAmerican = CPS1988[CPS1988$ethnicity == "afam",],
        Northeast = CPS1988[CPS1988$region == "northeast",],
        Midwest = CPS1988[CPS1988$region == "midwest",],
        South = CPS1988[CPS1988$region == "south",],
        West = CPS1988[CPS1988$region == "west",]
    )

    #Step 2, for loop for every element in the list you want to print
    for (m in 1:length(models)){
      a = vector(length = length(models))
      a[m] = (names(models[m]))
      s = capture.output(stargazer(models[m], type = "html", title = a[m]))
      cat(paste(s,"\n"),file="tables.doc",append=TRUE)
      cat(" ",file="tables.doc",append=TRUE)
    }

Descriptive statistics - stargazer in Word
------------------------------------------

-   One only has to put all the partitions in a list, change the
    filename, and change the desired descriptive statistics accordingly.

-   Let us now look at the [**document in which all tables
    are**](tables.doc)! This is very useful when organizing all tables
    and/or graphs for a paper.

Descriptives in sjPlot
----------------------

-   It is also possible to use `sjPlot` for descriptive statistics.

<!-- -->

    #Let's use a dataset of determinants of GDP Growth
    data(GrowthDJ)
    descriptives <- descr(GrowthDJ, show = 
                            c("type","label","n","mean","sd"))

    kable(descriptives, caption = "Descriptive Statistics", 
          booktabs = TRUE, row.names = FALSE) %>%
      kable_styling(latex_options = "striped")

Descriptives in sjPlot
----------------------

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Descriptive Statistics
</caption>
<thead>
<tr>
<th style="text-align:left;">
var
</th>
<th style="text-align:left;">
type
</th>
<th style="text-align:left;">
label
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
oil
</td>
<td style="text-align:left;">
categorical
</td>
<td style="text-align:left;">
oil
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
1.809917
</td>
<td style="text-align:right;">
0.3939977
</td>
</tr>
<tr>
<td style="text-align:left;">
inter
</td>
<td style="text-align:left;">
categorical
</td>
<td style="text-align:left;">
inter
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
1.619835
</td>
<td style="text-align:right;">
0.4874457
</td>
</tr>
<tr>
<td style="text-align:left;">
oecd
</td>
<td style="text-align:left;">
categorical
</td>
<td style="text-align:left;">
oecd
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
1.181818
</td>
<td style="text-align:right;">
0.3872983
</td>
</tr>
<tr>
<td style="text-align:left;">
gdp60
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
gdp60
</td>
<td style="text-align:right;">
116
</td>
<td style="text-align:right;">
3681.818966
</td>
<td style="text-align:right;">
7492.8776368
</td>
</tr>
<tr>
<td style="text-align:left;">
gdp85
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
gdp85
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
5683.259259
</td>
<td style="text-align:right;">
5688.6708192
</td>
</tr>
<tr>
<td style="text-align:left;">
gdpgrowth
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
gdpgrowth
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
4.094017
</td>
<td style="text-align:right;">
1.8914641
</td>
</tr>
<tr>
<td style="text-align:left;">
popgrowth
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
popgrowth
</td>
<td style="text-align:right;">
107
</td>
<td style="text-align:right;">
2.279439
</td>
<td style="text-align:right;">
0.9987481
</td>
</tr>
<tr>
<td style="text-align:left;">
invest
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
invest
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
18.157025
</td>
<td style="text-align:right;">
7.8533096
</td>
</tr>
<tr>
<td style="text-align:left;">
school
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
school
</td>
<td style="text-align:right;">
118
</td>
<td style="text-align:right;">
5.526271
</td>
<td style="text-align:right;">
3.5320372
</td>
</tr>
<tr>
<td style="text-align:left;">
literacy60
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
literacy60
</td>
<td style="text-align:right;">
103
</td>
<td style="text-align:right;">
48.165048
</td>
<td style="text-align:right;">
35.3542568
</td>
</tr>
</tbody>
</table>

Descriptives in sjPlot
----------------------

-   `sjPlot` has a lot of advantages. It automatically omits NA
    observations, and transforms factor variables to numeric variables
    (although this is dangerous!).
-   It can also make contingency tables, which stargazer cannot
    (readily) do:

<!-- -->

    sjt.xtab(GrowthDJ$oil, GrowthDJ$oecd, 
             use.viewer = TRUE, file = "contingency.doc")

![Figure](contingency.png)

Correlation tables
------------------

-   Both `sjPlot` and `stargazer` can make correlation tables. First,
    let’s try stargazer:

<!-- -->

    data(GrowthDJ)
    cormat <- select_if(GrowthDJ, is.numeric)

    stargazer(cor(cormat[5:7], 
                  use = c('pairwise.complete.obs')),
              header=FALSE, type='latex',
              omit.summary.stat = c("p25", "p75"), 
              font.size = "footnotesize",
              title = "Correlation Matrix")

-   Stargazer has no native stars, a feature which sjPlot does have.

Correlation tables - stargazer
------------------------------

-   This is the output:

-   -users: `stargazer` cannot correctly handle negative numbers when
    generating the code, causing some compilers to have difficulties.
-   Word and -users: you can also use stargazer’s `out`-option to export
    the tables to Word and .tex.

Correlation tables
------------------

-   I can also use the package `xtable`, which surpasses the problem of
    stargazer but creates otherwise identical tables (see .Rmd-file for
    the code).

-   `xtable` also supports output in or html format.

Correlation tables - sjPlot
---------------------------

-   `sjPlot` has a range of functions to visualize correlations. For
    example:

<!-- -->

    sjt.corr(cormat[1:6], file = "cortab.doc")

![Correlation table](cortab.png)

Correlation tables - sjPlot
---------------------------

-   Another example (this uses `ggplot2`)

<!-- -->

    sjp.corr(cormat[1:5], title = "Correlation Matrix")

![](PresentationRCafe---Copy.md_files/figure-markdown_strict/unnamed-chunk-18-1.png)

Correlation tables - with stars
-------------------------------

-   If you would like a correlation function that prints correlation
    tables with stars indicating significance, have a look
    [**here**](http://myowelt.blogspot.com/2008/04/beautiful-correlation-tables-in-r.html)

-   Example:

<!-- -->

    xtable(corstarsl(cormat[,1:6]), 
           caption = "Correlation Table with Stars")

Regression tables
-----------------

-   Now, finally, we can use stargazer’s principal application,
    regression tables.

-   Everything is *very* straightforward: just look at this:

<!-- -->

    data(TeachingRatings) 
    model1 <- lm(data = TeachingRatings, 
                 eval ~ beauty)
    model2 <- lm(data =TeachingRatings, 
                 eval ~ beauty + age)
    model3 <- lm(data =TeachingRatings, 
                 eval ~ beauty + age + gender)
    model4 <- lm(data =TeachingRatings, 
                 eval ~ beauty + age + gender + students)

    stargazer(model1, model2, model3, model4,
              header = FALSE,
              caption = FALSE,
              font.size = "footnotesize",
              column.sep.width = "0pt",
              omit.stat = c("ll", "F","ser"))

Regression tables
-----------------

Regression tables
-----------------

-   `stargazer` can also change styles to fit standard formats required
    by journals in the social sciences, and automatically incorporates
    different dependent variables.

Standard errors
---------------

The issue of standard errors is unrelated to the packages that are use
to *report* your data. The way to go about this is to change the
standard errors in the model list to the appropriate standard errors
calculated by another package, in this case, `sandwich`.

    model1 <- lm(data = GrowthDJ, 
                 gdpgrowth ~ oil + inter + invest + school)

    library(sandwich)

    heterosk_vcov <- vcovHC(model1, type = "HC3")
    h_ac_vcov <- vcovHAC(model1)

    stargazer(model1, 
              coeftest(model1, vcov = heterosk_vcov), 
              coeftest(model1, vcov = vcovHAC(model1) ))

Standard errors
---------------

Conclusion
----------

-   Stargazer and sjPlot are two packages that can save you a lot of
    trouble (as long as you are not looking for correlation tables with
    stars).

-   Supplementary packages, such as xtable and hmisc could also help you
    in reporting the most common statistical analyses.

-   Thank you for your attention! Suggestions?
    <a href="mailto:a.h.machielsen@uu.nl" class="email">a.h.machielsen@uu.nl</a>!
