n = 100; x2 <- 1 : n; x1 = .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n,sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

lr.x1.x2 <- lm(x1~x2)
lr.y.x2 <- lm(y~x2)

resid.x1.x2 <- resid(lr.x1.x2)
resid.y.x2 <- resid(lr.y.x2)

plot(resid.x1.x2,resid.y.x2)


n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2)/2, (runif(n/2)/2)+0.5);
beta0 <- 0; beta1 <- 1; beta2 <- 0; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + x*t*beta2 +  rnorm(n, sd = sigma)

plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

summary(lm(y~factor(t)))
summary(lm(y~factor(t)+x))

lr.t.x <- glm(factor(t)~x,family="binomial")
lr.y.x <- lm(y~x)

resid.t.x <- resid(lr.t.x)
resid.y.x <- resid(lr.y.x)

plot(resid.t.x,resid.y.x)
