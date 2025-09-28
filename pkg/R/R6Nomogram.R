library(R6)

R6Nomogram <- R6Class("R6Nomogram",
                      public = list(
                        model = list(),
                        newdata = list(),
                        var.names = character(0),
                        x.names = character(0),
                        x.labels = character(0),
                        orig.x = list(),
                        orig.terms = list(),
                        orig.response = list(),
                        x.vals = list(),
                        x.points = list(),
                        x.pretty.vals = list(),
                        x.pretty.points = list(),
                        x.y.offsets = list(),
                        scale.c = 1,
                        constant = 0,
                        total.points = numeric(0),
                        linear.predictor = numeric(0),
                        pretty.total.points = numeric(0),
                        pretty.linear.predictor = numeric(0),
                        pretty.response = numeric(0),
                        plot.lp = FALSE,
                        v.pos.x = numeric(0),
                        v.pos.r = numeric(0),
                        text.par = list(),
                        line.par = list(),
                        tick.par = list(),
                        points.lab = "Points",
                        total.points.lab = "Total Points",
                        lp.lab = "Linear Predictor",
                        resp.lab = "Response",
                        verbose = TRUE
                      )
)

## Initialize ----

R6Nomogram$set("public", "initialize", function(model, newdata, 
                                                verbose=TRUE,
                                                steps=Inf,
                                                type.terms="terms",
                                                type.response="response") {
  self$verbose <- verbose
  if(steps >= 1) {
    if(verbose) cat("1. Loading newdata\n")
    self$model <- model
    if(missing(newdata)) {
      self$newdata <- model$data
    } else {
      self$newdata <- newdata
    }
  }
  
  if(steps >= 2) {
    if(verbose) cat("2. Populating Variables\n")
    self$PopulateVars(model, self$newdata,
                      type.terms, type.response)
  }
  
  if(steps >= 3) {
    if(verbose) cat("3. Creating x varibles\n")
    self$create.x()
  }
  
  if(steps >= 4) {
    if(verbose) cat("4. Shifting x variables\n")
    self$shift()
  }
  
  if(steps >= 5) {
    if(verbose) cat("5. Scaling x variables\n")
    self$scale()
  }
  
  if(steps >= 6) {
    if(verbose) cat("6, Prettying x\n")
    self$pretty()
    if(verbose) cat("   Pretyying y\n")
    self$pretty.y()
  }
  
  if(verbose) cat("Finishing\n")
  return(invisible(self))
})

## PopulateVars ----

R6Nomogram$set("public", "PopulateVars", function(model=self$model, 
                                                  newdata=self$newdata,
                                                  type.terms="terms",
                                                  type.response=
                                                    "response") {
  data.names <- names(newdata)
# look at replacing the following with all.vars and get_all_vars
  # var.names <- character(0)
  # tmp <- as.list(attr(delete.response(terms(model$formula)), 'variables'))
  # while(length(tmp)) {
  #   if(is.call(tmp[[1]])) {
  #     tmp <- c(tmp, as.list(tmp[[1]]))
  #   } 
  #   if(is.name(tmp[[1]])) {
  #     var.names <- c(var.names, as.character(tmp[[1]]))
  #   }
  #   tmp[[1]] <- NULL
  # }
  
  tmp <- delete.response(terms(model$formula))
  var.names <- all.vars(tmp)
  if(length(setdiff(var.names, data.names))) warning(
    "There are variable names in the formula that are not in the data!"
  )
  var.names2 <- apply(attr(tmp, 'factors'), 2, function(x) {
    paste(var.names[as.logical(x)], collapse=":")
  })
  
  self$x.names <- as.vector(var.names2)
  if(!length(self$x.labels)) {
    self$x.labels <- self$x.names
  }
  self$var.names <- var.names
  
  self$orig.x <- get_all_vars(tmp, newdata)
  
  tmp.terms <- predict(model, newdata, type=type.terms)
  self$constant <- attr(tmp.terms, "constant")
  self$orig.terms <- as.data.frame(predict(model, newdata, type=type.terms))
  self$orig.response <- predict(model, newdata, type=type.response)
  self$total.points <- predict(model, newdata)
  self$linear.predictor <- self$total.points
  self$total.points <- self$total.points - self$constant
  
  return(invisible(self))
})

## create.x ----

R6Nomogram$set("public", "create.x", function(){
  tmp <- delete.response(terms(self$model$formula))
  tmp2 <- attr(tmp, 'factors')
  for(i in seq_along(self$orig.terms)) {
    if(i > length(self$var.names)) {
      tmp.df <- self$orig.x[,as.logical(tmp2[,i])]
      if(all(sapply(tmp.df, is.numeric))) {
        x <- Reduce(`*`, tmp.df)
      } else if(all(sapply(tmp.df, is.factor))) {
        x  <- interaction(tmp.df, sep=':')
      } else { # mix, need to improve this eventually
        x <- interaction(tmp.df, sep=':')
      }
    } else {
      x <- self$orig.x[,i]
    }
    
    d <- duplicated(x)
    o <- order(x[!d])
    
    self$x.vals[[i]] <- x[!d][o]
    self$x.points[[i]] <- self$orig.terms[[i]][!d][o]
  }
  names(self$x.vals) <- self$x.names
  names(self$x.points) <- self$x.names
  
  return(invisible(self))
})


## shift ----

R6Nomogram$set("public", "shift", function(w = self$x.names, 
                                           v, 
                                           update.constant=TRUE) {
  if(missing(v)) {
    v <- -sapply(self$x.points[w], min)
  }
  
  if(is.null(names(v))) names(v) <- w
  
  for(i in w) {
    if(update.constant) {
      self$total.points <- self$total.points + v[i]
      self$constant <- self$constant - v[i]
    }
    self$x.points[[i]] <- self$x.points[[i]] + v[i]
  }
  
  return(invisible(self))
})

## scale ----

R6Nomogram$set("public", "scale", function(max.points = 100) {
  m <- max(
    sapply(self$x.points, max)
  )
  
  self$total.points <- self$total.points/m*max.points
  self$scale.c <- self$scale.c*m/max.points
  
  for(i in self$x.names) {
    self$x.points[[i]] <- self$x.points[[i]]/m*max.points
  }
  
  return(invisible(self))
})

## prettyify ----

R6Nomogram$set("public", "pretty", function(w=self$x.names,
                                            v) {
  for(i in w) {
    self$x.y.offsets[[i]] <- NULL
    if(is.factor(self$x.vals[[i]])) {
      self$x.pretty.vals[[i]] <- self$x.vals[[i]]
      self$x.pretty.points[[i]] <- self$x.points[[i]]
      next
    }
    if(missing(v)) {
      vv <- axisTicks(range(self$x.vals[[i]]), FALSE)
    } else if(length(v)==1 && is.numeric(v)) {
      vv <- axisTicks(range(self$x.vals[[i]]), FALSE, nint=v)
    } else if(is.list(v)) {
      vv <- v[[i]]
    } else if(is.numeric(v)) {
      vv <- v
    } else {
      stop("Unable to interpret v")
    }
    
    tmp <- spline(self$x.vals[[i]], self$x.points[[i]], method='natural',
                  xout=vv)
    self$x.pretty.vals[[i]] <- tmp$x
    self$x.pretty.points[[i]] <- tmp$y
  }
  
  return(invisible(self))
})

R6Nomogram$set("public", "pretty.y", function(v) {
  if(missing(v)) {
    vv <- axisTicks(range(self$total.points), FALSE)
  } else if(length(v)==1) {
    vv <- axisTicks(range(self$total.points), FALSE, nint=v)
  } else {
    vv <- v
  }
  
  tmp <- spline(self$total.points, self$orig.response, method='natural',
                xout=vv)
  self$pretty.total.points <- tmp$x
  self$pretty.linear.predictor <- signif(
    self$constant + self$scale.c * tmp$x,
    2)
  self$pretty.response <- signif(tmp$y, 2)
  
  return(invisible(self))
})


## plot ----

R6Nomogram$set("public", "plot", function(plot.x=TRUE, plot.y=TRUE,
                                          predict, ...) {
#  oldpar <- par(c())
#  on.exit(par(oldpar))
  
  par(mar=c(1, max(nchar(self$x.labels)), 1, 0) + 0.1,
      xpd=TRUE)
  par(...)
  
  if(plot.x) {
    if(!length(self$v.pos.x)) {
      self$v.pos.x <- seq(from = length(self$x.labels) + 1, to = 1, by = -1)
    }
    
    max.p <- max(sapply(self$x.points, max))
    
    if(plot.y) {
      if(!length(self$v.pos.r)) {
        if(self$plot.lp) {
          self$v.pos.r <- -(1:3) 
        } else {
          self$v.pos.r <- -(1:2)
        }
      }
      
      plot.new()
      plot.window(xlim = c(0, max.p), 
                  ylim=c(min(self$v.pos.r), max(self$v.pos.x)))
    } else {
      plot.new()
      plot.window(xlim = c(0, max.p), 
                  ylim = c(0, max(self$v.pos.x)))
    }
    
    for(i in seq_along(self$v.pos.x)) {
      if(i==1) { # points
        segments(0, self$v.pos.x[1], max.p)
        tmp <- axisTicks(c(0,max.p), FALSE, nint=10)
        segments(tmp, self$v.pos.x[1], tmp, self$v.pos.x[1]+strheight("M")/5)
        text(tmp, self$v.pos.x[1]+strheight("M")*0.4, tmp)
        
        next
      }
      
      cur.name <- self$x.names[i-1]
    
      if(!( (cur.name %in% names(self$x.y.offsets)) &&
            length(self$x.y.offsets[[cur.name]]) )) {
        if(is.factor(n1$x.vals[[cur.name]])) {
          self$x.y.offsets[[cur.name]] <- rep(c(1,-1), length.out=
              length(self$x.pretty.points[[cur.name]]))[order(order(
                self$x.pretty.points[[cur.name]]
              ))]
        } else {
          tmp.o <- order(self$x.pretty.vals[[cur.name]])
          tmp.x <- self$x.pretty.vals[[cur.name]][tmp.o]
          tmp.p <- self$x.pretty.points[[cur.name]][tmp.o]
          tmp.s <- sign(diff(tmp.p))
          tmp.s <- c(tmp.s, tail(tmp.s,1))
          # if(length(unique(tmp.s))==1) {
          #   tmp.s <- rep(c(1, -1), length.out=length(tmp.o))
          # }
          self$x.y.offsets[[cur.name]] <- tmp.s[order(tmp.o)]
        }
      }
      
      segments(0, self$v.pos.x[i], max(self$x.points[[cur.name]]))
      segments(x0=self$x.pretty.points[[cur.name]], y0=self$v.pos.x[i],
               y1=self$v.pos.x[i] + 
                 sign(self$x.y.offsets[[cur.name]])/5*strheight("M"))
      text(self$x.pretty.points[[cur.name]], 
           self$v.pos.x[i]+self$x.y.offsets[[cur.name]]*strheight("M")*0.4,
           self$x.pretty.vals[[cur.name]])
      
    }
    axis(2, at=self$v.pos.x, labels=c(self$points.lab, self$x.labels),
         tick=FALSE, las=1)
    
    if(!missing(predict)) {
      tot <- 0
      for(i in seq(length(self$x.labels), 1)) {
        cur.name <- self$x.names[i]
        if(!(cur.name %in% names(predict))) {
          tmp <- delete.response(terms(self$model$formula))
          tmp2 <- attr(tmp, 'factors')
          tmp3 <- tmp2[,cur.name]
          tmp.df <- predict[names(tmp3)[tmp3==1]]
          if(all(sapply(tmp.df, is.numeric))) {
            x <- Reduce(`*`, tmp.df)
          } else {
            x  <- interaction(tmp.df, sep=':')
          }
          predict[cur.name] <- x
        }
        if(is.numeric(self$x.vals[[cur.name]])) {
          tmp <- spline(self$x.vals[[cur.name]], 
                        self$x.points[[cur.name]], method='natural',
                        xout=unlist(predict[cur.name]))
        } else if(is.factor(self$x.vals[[cur.name]])) {
          tmp <- list(y= self$x.points[[cur.name]][
            as.numeric(unlist(predict[cur.name]))
          ])
        } else { # character
          tmp <- list(y= self$x.points[[cur.name]][
            match(unlist(predict[cur.name]), 
                  as.character(self$x.vals[[cur.name]]))
          ])
        }
        segments(tmp$y, self$v.pos.x[i+1], y1=self$v.pos.x[1], col=i)
        if(self$verbose) cat(cur.name, ": ", tmp$y, "\n")
        tot <- tot + tmp$y
      }
      if(self$verbose) cat("\nTotal Points: ", tot, "\n\n")
    }
  }
  
  if(plot.y) {
    if(!plot.x) {
      
      max.p <- max(sapply(self$x.points, max))
      
      if(!length(self$v.pos.r)) {
        if(self$plot.lp) {
          self$v.pos.r <- -(1:3) 
        } else {
          self$v.pos.r <- -(1:2)
        }
      }
      
      plot.new()
      plot.window(xlim = c(0, max.p), 
                  ylim=c(min(self$v.pos.r), 0))
      
      if(!missing(predict)) {  # copy of above, should make a function
        
        tot <- 0
        for(i in seq(length(self$x.labels), 1)) {
          cur.name <- self$x.names[i]
          if(!(cur.name %in% names(predict))) {
            tmp <- delete.response(terms(self$model$formula))
            tmp2 <- attr(tmp, 'factors')
            tmp3 <- tmp2[,cur.name]
            tmp.df <- predict[names(tmp3)[tmp3==1]]
            if(all(sapply(tmp.df, is.numeric))) {
              x <- Reduce(`*`, tmp.df)
            } else {
              x  <- interaction(tmp.df, sep=':')
            }
            predict[cur.name] <- x
          }
          if(is.numeric(self$x.vals[[cur.name]])) {
            tmp <- spline(self$x.vals[[cur.name]], 
                          self$x.points[[cur.name]], method='natural',
                          xout=unlist(predict[cur.name]))
          } else if(is.factor(self$x.vals[[cur.name]])) {
            tmp <- list(y= self$x.points[[cur.name]][
              as.numeric(unlist(predict[cur.name]))
            ])
          } else { # character
            tmp <- list(y= self$x.points[[cur.name]][
              match(unlist(predict[cur.name]), 
                    as.character(self$x.vals[[cur.name]]))
            ])
          }
          tot <- tot + tmp$y
        }
      }
    }
    
    i2 <- self$plot.lp + 2
    
    segments(0, self$v.pos.r, max.p)
    tick.pos <- (self$pretty.total.points - min(self$total.points))/
      diff(range(self$total.points))*max.p
    segments(tick.pos, self$v.pos.r[1], tick.pos, self$v.pos.r[1]+
               strheight("M")/5)
    if(self$plot.lp) {
      segments(tick.pos, self$v.pos.r[2], tick.pos, self$v.pos.r[2]+
                 strheight("M")/5)
    }
    segments(tick.pos, self$v.pos.r[i2], tick.pos, self$v.pos.r[i2]-
               strheight("M")/5)
    text(tick.pos, self$v.pos.r[1] + strheight("M")*0.4,
         self$pretty.total.points)
    if(self$plot.lp) {
      text(tick.pos, self$v.pos.r[2] + strheight("M")*0.4,
           self$pretty.linear.predictor)
    }
    text(tick.pos, self$v.pos.r[i2] - strheight("M")*0.4,
         self$pretty.response)
    tmp.lab <- if(self$plot.lp) {
      c(self$total.points.lab, self$lp.lab, self$resp.lab)
    } else {
      c(self$total.points.lab, self$resp.lab)
    }
    axis(2, at=self$v.pos.r, labels= tmp.lab,
         tick=FALSE, las=1)
    
    if(!missing(predict)) {
      segments( (tot-min(self$total.points))/
                  diff(range(self$total.points))*max.p,
                self$v.pos.r[1], y1=self$v.pos.r[i2],
                col='red')
    }
  }
  
  return(invisible(self))
})


## tables ----


## tests ----

#  fit2 <- glm(am ~ poly(mpg, 3) * wt, data=mtcars,
#              family=binomial())

mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)

fit2 <- glm(am ~ poly(mpg,2) + poly(disp,2) + 
              cyl*gear + poly(wt,3),
            data=mtcars)#, family=binomial)


n1 <- R6Nomogram$new(fit2)
n1$plot()

tmp <- mtcars[1,]
#tmp[['cyl:gear']] <- 5
#tmp[['cyl:gear']] <- factor('6:4', levels=levels(n1$x.pretty.vals$`cyl:gear`))
n1$plot(predict=tmp)

n1$plot.lp <- TRUE
n1$v.pos.r <- NULL
n1$plot()
n1$plot(predict=tmp)

n1$plot(predict=mtcars[24,])
n1$plot(predict=mtcars[26,])

R6Nomogram$new(fit2)$plot()

predict(fit2, type='terms') |> head()


fit3 <- lm(n1$orig.response ~ n1$total.points)
fit3

n1$constant
n1$scale.c

coef(fit3)[2] - n1$scale.c
coef(fit3)[1] - n1$constant

all.equal(n1$orig.response, n1$linear.predictor)

tmp1 <- Reduce(`+`, n1$orig.terms) + 0.40625
fit4 <- lm(n1$linear.predictor ~ tmp1)
fit4

fit5 <- lm(mpg ~ wt*cyl, data=mtcars)
head(predict(fit5, type='terms'))

## notes ----
  
steps <- r"(
  1. Load newdata
  2. PopulateVars: x.names, var.names, orig.x, orig.terms, constant, 
                    total.points, orig.responso, linear.predictor
  3. create.x: x.points, x.vals; translate original into x and points
  4. shift: shift x so that min(points) = 0
  5. scale: scale points so maximum points is max.pnts
  6. prettyify: convert x.points and x.vals into "pretty" versions,
                also total.points, linear.predictor, response
  7. plots and tables
)"