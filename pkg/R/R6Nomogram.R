
R6Nomogram <- R6::R6Class("R6Nomogram",
                      public = list(
                        model = list(),
                        terms = list(),
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
                        options = list(
                          tik.len=1/5,
                          txt.pos=1/2,
                          points.nint=10,
                          signif.digits=2,
                          text.par = list(),
                          line.par = list(),
                          tick.par = list()),
                        points.lab = "Points",
                        total.points.lab = "Total Points",
                        lp.lab = "Linear Predictor",
                        resp.lab = "Response",
                        tp.range = numeric(0),
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
    self$terms <- terms(model) %||% terms(model$formula)
    
    if(missing(newdata)) {
      self$newdata <- model$data %||% 
        get(model$call$data) %||%
        model.frame(model)
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
  
  tmp <- delete.response(self$terms)
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
  d <- duplicated(self$orig.response)
  self$orig.response <- self$orig.response[!d,]
  self$total.points <- predict(model, newdata)[!d,]
  self$linear.predictor <- self$total.points
  self$total.points <- self$total.points - self$constant
  
  return(invisible(self))
})

## create.x ----

R6Nomogram$set("public", "create.x", function(){
  tmp <- delete.response(self$terms)
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
    v <- -sapply(self$x.points[w], min, na.rm=TRUE)
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
    sapply(self$x.points, max, na.rm=TRUE), na.rm=TRUE
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
      self$x.pretty.vals[[i]] <- as.character(self$x.vals[[i]])
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
    self$options$signif.digits)
  self$pretty.response <- signif(tmp$y, 
                                 self$options$signif.digits)
  
  return(invisible(self))
})


## plot ----

R6Nomogram$set("public", "plot", function(plot.x=TRUE, plot.y=TRUE,
                                          predict, ...) {
#  oldpar <- par(c())
#  on.exit(par(oldpar))
  
  par(mar=c(1, max(nchar(self$x.labels), na.rm=TRUE), 1, 0) + 0.1,
      xpd=TRUE)
  par(...)
  
  if(plot.x) {
    if(!length(self$v.pos.x)) {
      self$v.pos.x <- seq(from = length(self$x.labels) + 1, to = 1, by = -1)
    }
    
    max.p <- max(sapply(self$x.points, max, na.rm=TRUE), na.rm=TRUE)
    
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
                  ylim=c(min(self$v.pos.r, na.rm=TRUE), 
                         max(self$v.pos.x, na.rm=TRUE)))
    } else {
      plot.new()
      plot.window(xlim = c(0, max.p), 
                  ylim = c(0, max(self$v.pos.x, na.rm=TRUE)))
    }
    
    for(i in seq_along(self$v.pos.x)) {
      if(i==1) { # points
        tmp.args <- self$options$line.par[["points"]] %||%
          self$options$line.par[["default"]] %||%
          list()
        tmp.args <- modifyList(tmp.args, 
                               list(x0=0, y0=self$v.pos.x[1], 
                                    x1=max.p))
        do.call(segments, tmp.args)

        tmp <- axisTicks(c(0,max.p), FALSE, nint=self$options$points.nint)
        tmp.args <- self$options$tick.par[["points"]] %||%
          self$options$tick.par[["default"]] %||%
          list()
        tmp.args <- modifyList(tmp.args,
                               list(x0=tmp, y0=self$v.pos.x[1], 
                                    x1=tmp, 
                                    y1=self$v.pos.x[1]+strheight("M")*self$options$tik.len))
        do.call(segments, tmp.args)
        
        tmp.args <- self$options$text.par[["points"]] %||%
          self$options$text.par[["default"]] %||%
          list()
        tmp.args <- modifyList(tmp.args,
                                list(x=tmp, 
                                     y=self$v.pos.x[1]+strheight("M")*self$options$txt.pos, 
                                     labels=tmp))
        do.call(text, tmp.args)
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
          self$x.y.offsets[[cur.name]] <- tmp.s[order(tmp.o)]
        }
      }
      
      tmp.args <- self$options$line.par[[cur.name]] %||%
        self$options$line.par[["default"]] %||%
        list()
      tmp.args <- modifyList(tmp.args, 
                             list(x0=min(self$x.points[[cur.name]], na.rm=TRUE), 
                                  y0=self$v.pos.x[i], 
                                  x1=max(self$x.points[[cur.name]], na.rm=TRUE)))
      do.call(segments, tmp.args)
      
      tmp.args <- self$options$tick.par[[cur.name]] %||%
        self$options$tick.par[["default"]] %||%
        list()
      tmp.args <- modifyList(tmp.args,
                             list(x0=self$x.pretty.points[[cur.name]], 
                                  y0=self$v.pos.x[i],
                                  y1=self$v.pos.x[i] + 
                                    sign(self$x.y.offsets[[cur.name]])*self$options$tik.len*strheight("M")))
      do.call(segments, tmp.args)
      
      tmp.args <- self$options$text.par[[cur.name]] %||%
        self$options$text.par[["default"]] %||%
        list()
      tmp.args <- modifyList(tmp.args,
                             list(x=self$x.pretty.points[[cur.name]], 
                                  y=self$v.pos.x[i]+self$x.y.offsets[[cur.name]]*strheight("M")*self$options$txt.pos,
                                  labels=self$x.pretty.vals[[cur.name]]))
      do.call(text, tmp.args)
      
    }
    axis(2, at=self$v.pos.x, labels=c(self$points.lab, self$x.labels),
         tick=FALSE, las=1)
    
    if(!missing(predict)) {
      tot <- 0
      for(i in seq(length(self$x.labels), 1)) {
        cur.name <- self$x.names[i]
        if(!(cur.name %in% names(predict))) {
          tmp <- delete.response(self$terms)
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
      
      max.p <- max(sapply(self$x.points, max, na.rm=TRUE), na.rm=TRUE)
      
      if(!length(self$v.pos.r)) {
        if(self$plot.lp) {
          self$v.pos.r <- -(1:3) 
        } else {
          self$v.pos.r <- -(1:2)
        }
      }
      
      plot.new()
      plot.window(xlim = c(0, max.p), 
                  ylim=c(min(self$v.pos.r, na.rm=TRUE), 0))
      
      if(!missing(predict)) {  # copy of above, should make a function
        
        tot <- 0
        for(i in seq(length(self$x.labels), 1)) {
          cur.name <- self$x.names[i]
          if(!(cur.name %in% names(predict))) {
            tmp <- delete.response(self$terms)
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
    
    tmp.args <- self$options$line.par[["response"]] %||%
      self$options$line.par[["default"]] %||%
      list()
    tmp.args <- modifyList(tmp.args, 
                           list(x0=0, 
                                y0=self$v.pos.r, 
                                x1=max.p))
    do.call(segments, tmp.args)
    if(length(self$tp.range) == 0) {
      self$tp.range <- range(self$total.points, na.rm = TRUE)
    }
    tick.pos <- (self$pretty.total.points - self$tp.range[1])/
      diff(self$tp.range)*max.p
    tmp.args <- self$options$tick.par[['total points']] %||%
      self$options$tick.par[['default']] %||%
      list()
    tmp.args <- modifyList(tmp.args, 
                           list(x0=tick.pos, 
                                y0=self$v.pos.r[1], 
                                x1=tick.pos, 
                                y1=self$v.pos.r[1]+
                                  strheight("M")*self$options$tik.len))
    do.call(segments, tmp.args)
    if(self$plot.lp) {
      tmp.args <- self$options$tick.par[['linear predictor']] %||%
        self$options$tick.par[['default']] %||%
        list()
      tmp.args <- modifyList(tmp.args, 
                             list(x0=tick.pos, 
                                  y0=self$v.pos.r[2], 
                                  x1=tick.pos, 
                                  y1=self$v.pos.r[2]+
                                    strheight("M")*self$options$tik.len))
      do.call(segments, tmp.args)
    }
    tmp.args <- self$options$tick.par[['response']] %||%
      self$options$tick.par[['default']] %||%
      list()
    tmp.args <- modifyList(tmp.args, 
                           list(x0=tick.pos, 
                                y0=self$v.pos.r[i2], 
                                x1=tick.pos, 
                                y1=self$v.pos.r[i2]-
                                  strheight("M")*self$options$tik.len))
    do.call(segments, tmp.args)
    
    tmp.args <- self$options$text.par[['total points']] %||%
      self$options$text.par[['default']] %||% 
      list()
    tmp.args <- modifyList(tmp.args,
                           list(x=tick.pos, 
                                y=self$v.pos.r[1] + strheight("M")*self$options$txt.pos,
                                labels=self$pretty.total.points))
    do.call(text, tmp.args)
    
    if(self$plot.lp) {
      tmp.args <- self$options$text.par[['linear predictor']] %||%
        self$options$text.par[['default']] %||%
        list()
      tmp.args <- modifyList(tmp.args,
                             list(x=tick.pos, 
                                  y=self$v.pos.r[2] + strheight("M")*self$options$txt.pos,
                                  labels=self$pretty.linear.predictor))
      do.call(text, tmp.args)
    }
    
    tmp.args <- self$options$text.par[['response']] %||%
      self$options$text.par[['default']] %||%
      list()
    tmp.args <- modifyList(tmp.args,
                           list(x=tick.pos, 
                                y=self$v.pos.r[i2] - strheight("M")*self$options$txt.pos,
                                labels=self$pretty.response))
    do.call(text, tmp.args)
    
    tmp.lab <- if(self$plot.lp) {
      c(self$total.points.lab, self$lp.lab, self$resp.lab)
    } else {
      c(self$total.points.lab, self$resp.lab)
    }
    axis(2, at=self$v.pos.r, labels= tmp.lab,
         tick=FALSE, las=1)
    
    if(!missing(predict)) {
      segments( (tot-self$tp.range[1])/
                  diff(self$tp.range)*max.p,
                self$v.pos.r[1], y1=self$v.pos.r[i2],
                col='red')
    }
  }
  
  return(invisible(self))
})


## grconvertX ----

R6Nomogram$set("public", "grconvertX", function(x, 
                                                from="Total Points",
                                                verbose) {
  if(missing(verbose)) verbose <- self$verbose
  if(from == "Total Points") {
    max.p <- max(sapply(self$x.points, max, na.rm=TRUE), na.rm=TRUE)
    return((x-self$tp.range[1])/
      diff(self$tp.range)*max.p)
  }
  w <- match(from, self$x.names)
  if(is.na(w)) {
    warning("Unable to find a match for ", from)
    return(NA)
  }
  if(is.factor(self$x.vals[[w]])) {
    return(self$x.points[[w]][
      match(x, self$x.vals[[w]])
    ])
  }
  return(spline(
    self$x.vals[[w]], self$x.points[[w]], method='natural',
    xout=x)$y)
})



## tables ----

R6Nomogram$set("public", "tables", function() {
  c(
    sapply(self$x.names, function(w) {
      setNames(data.frame(self$x.pretty.vals[[w]], 
                          self$x.pretty.points[[w]]),
               c(w, self$points.lab)
      )
    }, simplify=FALSE),
    list(Response=setNames(
      data.frame(self$pretty.total.points, 
                 self$pretty.linear.predictor,
                 self$pretty.response),
      c(self$total.points.lab, self$lp.lab,
        self$resp.lab)
    ))
  )
})

