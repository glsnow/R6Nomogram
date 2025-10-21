

## tests ----

#  fit2 <- glm(am ~ poly(mpg, 3) * wt, data=mtcars,
#              family=binomial())

mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)

fit2 <- glm(am ~ poly(mpg,2) + poly(disp,2) + 
              cyl*gear + poly(wt,3),
            data=mtcars)#, family=binomial)


n1 <- R6Nomogram$new(fit2)
n1$plot()$tables()

n2 <- n1$clone()
n2$pretty(v=100)$pretty.y(v=100)
n2$tables()

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



fit <- glm(mpg ~ cyl*gear + poly(wt, 2) + poly(disp,2),
           data=mtcars, family=gaussian(link='inverse'))

n2 <- R6Nomogram$new(fit)

n2$plot()
n2$plot.lp <- TRUE
n2$v.pos.r <- NULL
n2$plot()

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

## old code ----

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
