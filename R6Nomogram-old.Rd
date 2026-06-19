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
Calling \code{nom <- R6Nomogram$new(model)} will create an R6 
object with the structure of the model needed to 
construct the nomogram.  You can then generate the plot
with a call like \code{nom$plot()}.  The first plot is likely
to not be pretty, that is where you will use the methods
or change values in the public variables to customize
how you want the plot to look.  Generally this will be
an iterative process of plotting, then making a change,
 plotting again, etc.
 
The initialization will try to do all the steps (other than
plotting), this works fine for models from \code{lm} or \code{glm}, 
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

  \section{Methods}
  
  All of the below methods with the exceptions of
  \code{grconvertX} and \code{tables} return the 
  object invisibly allowing chaining of commands.
  
  If you create a new \code{R6Nomogram} object 
  and pass it a fitted model
  by calling \code{R6Nomogram$new(model)} and do 
  not change the \code{steps} argument from its
  default, then the following methods will be
  called in order with their default arguments:
  
  \enumerate{
    \item \code{initialize}
    \item \code{PopulateVars}
    \item \code{create.x}
    \item \code{shift}
    \item \code{scale}
    \item \code{pretty} and \code{pretty.y}
  }
  
  If the \code{steps} argument is set to an integer
  then the above methods up to that value will
  be called.  You can then call specific methods
  with other arguments and set other variables
  in order to customize parts of the
  nomogram.
  
  \itemize{
  
    \item \code{initialize}  This function 
      initialized the object and has the 
      following arguments:
      \itemize{
        \item model
        \item newdata
        \item verbose=TRUE
        \item steps=Inf
        \item type.terms="terms",
        \item type.response="response"
      }
      
    \code{model} is the fitted model used to create
     the nomogram.  Models fit using \code{lm} and 
     \code{`glm`}
     work, other models for which there is a 
     \code{terms} method and a \code{predict} method
     with \code{type='terms'} or similar should work
     as well.  If not, then you can manually fill
     some of the variables to create the nomogram.
     
     The \code{newdata} argument defaults to the data
     frame used to fit the model (if it can be extracted).
     You can also submit a subset of the data frame, or
     other data for help in creating the nomogram.
     
     The \code{verbose} argument controls printing 
     messages indicating the progress.
     
     The \code{steps} argument indicates how many of 
     the steps to run automatically.  You should only
     need to set this if you will be doing important
     steps manually and do not want all of them to 
     be run automatically.
    
     The \code{type.terms} and \code{type.response} 
     arguments will be passed to the \code{predict}
     method for the model as the \code{type} argument.
     Change these if the method requires a different
     "type".
     
     If \code{steps} is greater than or equal to 1
     then \code{model} will be copied into the model
     slot of the object, The terms slot will be a 
     copy of the result of calling the \code{terms}
     function on the model and the newdata slot will
     be either a copy of the data frame passed as the
     newdata argument, or an attempt to get the data
     from the model.
     
    \item \code{PopulateVars}
    
      This method uses the model and newdata to set
      important variable slots for later methods.
      
      The \code{x.names} and \code{x.labels} are
      set to vectors with names corresponding to
      each additive term in the model (main effects
      and interactions) and the \code{var.names} slot
      will be a vector of the names of the variables
      in the original data/newdata.  The x.names slot
      should not be changed, the corresponding values
      in the x.labels slot will be used as the labels
      in the plot and can be changed accordingly.
      
      The \code{orig.x} slot will get a copy of the 
      original x/predictor variables from \code{newdata}.
      
      The \code{constant} slot is set to the constant 
      attribute from the prediction with \code{type="terms"}.
      
      The \code{orig.terms} slot is a list or data frame
      with the results of calling \code{predict} on the
      model and newdata with \code{type="terms"}.
      
      The \code{orig.response} slot will contain a vector
      of the results from calling \code{predict} with the
      model and newdata and \code{type="response"} with
      any duplicate values removed.
      
      The \code{linear predictor} slot will have the
      results from calling \code{predict} with the model
      and newdata and no \code{type} argument.  This is
      usually the linear predictor.  It will be identical
      to the response if there is no link function or if
      the identity link is used.  Otherwise it will be the
      link transform of the response.
      
      The \code{total.points} slot starts with the values
      of the linear.predictor slot minus the constant.
      
    
    \item \code{create.x}
    
      This function has no arguments.  It populates
      the \code{x.vals} and \code{x.points} slots
      with lists corresponding to \code{orig.x} and 
      \code{orig.terms}, but with duplicates removed
      and the individual vectors ordered on the x variable.
      
      Each list will have a component for each element
      of the \code{x.names} slot.
      
      These are the values that will be modified by the 
      \code{shift} and \code{scale} methods to make the
      nomogram.
    
    \item \code{shift}
    
    
    
    \item \code{scale}
    
    \item \code{pretty}
    
    \item \code{pretty.y}
    
    \item \code{plot}
    
    \item \code{grconvertX}
    
    \item \code{tables}
  
  }
  
  
  \section{Variables}
}


\author{
  Greg Snow \email{538280@gmail.com}
}

\seealso{
  \code{nomogram} in the rms package
}
\examples{
mtcars2 <- mtcars
mtcars2$gear <- factor(mtcars2$gear)
mtcars2$cyl <- factor(mtcars2$cyl)

fit <- glm(mpg ~ poly(wt,2) + poly(disp,2) + 
           gear*cyl + vs, 
           data=mtcars2, 
           family=gaussian(link="inverse"))

n1 <- R6Nomogram$new(fit)
n1$plot()

n1$options$tik.len <- 0.4
n1$options$txt.pos <- 1.2
n1$options$signif.digits <- 3
n1$x.pretty.vals$`cyl:gear` <-
  c("Other", "", "", "", "6:4", "", "6:5", "8:5")
n1$pretty.y(seq(60, 200, by=10))
n1$pretty('wt', c(1.5, 2, 3, 3.5, 4, 4.25, 4.5, 
                  4.75, 5, 5.25, 5.4))
n1$pretty('disp', c(75, 100, 125, 150, 175, 200, 250,
                    300, 400, 450))
n1$plot.lp <- TRUE
n1$v.pos.r <- NULL
n1$lp.lab <- "G/M"
n1$resp.lab <- "M/G"
n1$options$text.par[['linear predictor']] <- list(cex=0.8)

n1$plot()

print(mtcars2[1,])
n1$plot(predict=mtcars[1,])
}
