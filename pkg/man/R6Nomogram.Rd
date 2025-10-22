\name{R6Nomogram}
\alias{R6Nomogram}
\title{
Create an R6 reference class to create, modify, and plot Nomograms
}
\description{
  Nomograms are a type of plot that can be used to explore
  and expain many linear models.  This is an R6 reference
  class creator to help with creating, customizing, and
  plotting nomograms.  Methods in the nomogram object
  allow you to customize parts of the process and there
  are many options that can be changed to make the plot
  easier to understand.
}
\details{
Calling `nom <- R6Nomogram$new(model)` will create an R6 
object with the structure of the model needed to 
construct the nomogram.  You can then generate the plot
with a call like `nom$plot()`.  The first plot is likely
to not be pretty, that is where you will use the methods
or change values in the public variables to customize
how you want the plot to look.  Generally this will be
an iterative process of plotting, then making a change,
 plotting again, etc.
 
The initialization will try to do all the steps (other than
plotting), this works fine for models from `lm` or `glm`, 
but may not work fully for other models.  You can also 
specify how many of the steps should be done automatically,
then call the specific methods for the rest.  For some 
models you may need to manually do some of the steps.

The main steps are:

\enumerate{
  \item Load newdata
  \item PopulateVars: x.names, var.names, orig.x, orig.terms, constant, 
                    total.points, orig.responso, linear.predictor
  \item create.x: x.points, x.vals; translate original into x and points
  \item shift: shift x so that min(points) = 0
  \item scale: scale points so maximum points is max.pnts
  \item prettyify: convert x.points and x.vals into "pretty" versions,
                also total.points, linear.predictor, response
  \item plots and tables
  }
  
  There are one or more methods for each of the above steps
}


\author{
  Greg Snow \email{538280@gmail.com}
}

\seealso{
  \code{nomogram} in the rms package
}
\examples{

fit <- glm(mpg ~ poly(wt,2) + poly(disp,2) + 
           factor(gear)*factor(cyl), 
           data=mtcars, 
           family=gaussian(link="inverse"))

n1 <- R6Nomogram$new(fit)
n1$plot()
}
