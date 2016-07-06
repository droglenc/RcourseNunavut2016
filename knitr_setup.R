## knitr custom hooks
# set default plot options
#knit_hooks$set(plot = function(x, options) "")

knit_hooks$set(par1 = function(before, options, envir) {
  if (before) par(mar=c(3.5,3.5,1,1),mgp=c(2.1,0.4,0),tcl=-0.2)
})
knit_hooks$set(par2 = function(before, options, envir) {
  if (before) par(mfrow=c(1,2),mar=c(3.5,3.5,1,1),mgp=c(2.1,0.4,0),tcl=-0.2)
})
## knitr options -- output look
opts_chunk$set(prompt=TRUE,comment='')
## knitr options -- figure handling
opts_chunk$set(fig.path='Figs/', fig.width=3, fig.height=3, par1=TRUE)

## removes spaces between input and output chunks
## this works ok with the haddock theme, not with others
hook1 <- function(x){ gsub("```\n+```\n", "", x) }
knit_hooks$set(document = hook1)

## r options
options(width=100,show.signif.stars=FALSE,continue="  ")
