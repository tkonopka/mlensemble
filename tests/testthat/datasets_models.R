# synthetic datasets and models used in tests

set.seed(12345)
library(xgboost)


###############################################################################
## dataset 1 for regression
## (2 variables, simple correlation with unit slope)

d_linear_x <- seq(1, 10, length=45)
d_linear <- data.table(index=seq_along(d_linear_x),
                       x1=d_linear_x + rnorm(45),
                       x2=d_linear_x + rnorm(45))
d_linear$y_true <- (d_linear_x)
d_linear$y <- d_linear$y_true + rnorm(nrow(d_linear))
d_linear_1 <- d_linear[seq_len(nrow(d_linear)) %% 3 == 0, ]
d_linear_2 <- d_linear[seq_len(nrow(d_linear)) %% 3 == 1, ]
d_linear_3 <- d_linear[seq_len(nrow(d_linear)) %% 3 == 2, ]

# regression models trained on subsets
m_lm_1 <- lm(y~x1+x2, data=d_linear_1)
m_lm_2 <- lm(y~x1+x2, data=d_linear_2)
m_lm_3 <- lm(y~x1+x2, data=d_linear_3)

# regression models trained on different variables
m_lm_x1 <- lm(y~x1, data=d_linear)
m_lm_x2 <- lm(y~x2, data=d_linear)


###############################################################################
## dataset 2 for regression
## (1 variable, V-shaped pattern)

d_v <- data.table(index=1:40, x=c(seq(-20, -1), seq(1, 20)))
d_v$y_true <- abs(d_v$x)
d_v$y <- abs(d_v$y_true + rnorm(nrow(d_v), 0, 2))
d_v_pos <- d_v[x>0, ]
d_v_neg <- d_v[x<0, ]

# regression models trained on subsets
m_lm_pos <- lm(y~x, data=d_v_pos)
m_lm_neg <- lm(y~x, data=d_v_neg)


###############################################################################
## dataset 3 for regression
## (1 variable, two populations with different slopes)

dpx <- seq(0, 5, length=20)
d_p1 <- data.table(index=1:20, x=dpx)
d_p2 <- data.table(index=101:120, x=dpx)
d_p1$y_true <- d_p1$x
d_p2$y_true <- d_p2$x * 2
d_p1$y <- d_p1$y_true + rnorm(nrow(d_p1))
d_p2$y <- d_p2$y_true + rnorm(nrow(d_p2))
d_pop <- rbind(d_p1, d_p2)

# regression models
m_lm_p1 <- lm(y~x, data=d_p1)
m_lm_p2 <- lm(y~x, data=d_p2)


###############################################################################
## dataset 1 for multi-class classification (points in four quadrants)

temp_0 <- cbind(x=rnorm(60, 1.5, 1),
                y=rnorm(60, 1.5, 1))
temp_1 <- cbind(x=rnorm(60, 1.5, 1),
                y=rnorm(60, -1.5, 1))
temp_2 <- cbind(x=rnorm(60, -1.5, 1),
                y=rnorm(60, -1.5, 1))
temp_3 <- cbind(x=rnorm(60, -1.5, 1),
                y=rnorm(60, 1.5, 1))
d_mc <- data.table(rbind(temp_0, temp_1, temp_2, temp_3))
d_mc$label <- rep(c(0,1,2,3), each=60)
rm(temp_0, temp_1, temp_2, temp_3)
d_mc_1 <- d_mc[seq_len(nrow(d_mc)) %% 3 == 0, ]
d_mc_2 <- d_mc[seq_len(nrow(d_mc)) %% 3 == 1, ]
d_mc_3 <- d_mc[seq_len(nrow(d_mc)) %% 3 == 2, ]

# test dataset with points in a grid
testdata_mc <- data.table(expand.grid(
  list(x=seq(-3, 3, length=12), y=seq(-3, 3, length=12))
))
testdata_mc$label <- 0
testdata_mc[x>0 & y<0, "label"] <- 1
testdata_mc[x<0 & y<0, "label"] <- 2
testdata_mc[x<0 & y>0, "label"] <- 3


# classification models (trained on subsets of complete data)
m_mc_1 <- xgboost(as.matrix(d_mc_1[, c("x", "y")]),
                  label=d_mc_1$label,
                  nrounds=2, num_class=4, verbose=0,
                  objective="multi:softprob")
m_mc_2 <- xgboost(as.matrix(d_mc_2[, c("x", "y")]),
                  label=d_mc_2$label,
                  nrounds=2, num_class=4, verbose=0,
                  objective="multi:softprob")
m_mc_3 <- xgboost(as.matrix(d_mc_3[, c("x", "y")]),
                  label=d_mc_3$label,
                  nrounds=2, num_class=4, verbose=0,
                  objective="multi:softprob")

# classification models (trained on individual variables)
m_mc_x <- xgboost(as.matrix(d_mc[, "x"]),
                  label=d_mc$label,
                  nrounds=2, num_class=4, verbose=0,
                  objective="multi:softprob")
m_mc_y <- xgboost(as.matrix(d_mc[, "y"]),
                  label=d_mc$label,
                  nrounds=2, num_class=4, verbose=0,
                  objective="multi:softprob")


###############################################################################
## dataset 2 for multi-class classification
## items in two classes all mixed up on a plane
## two populations have different proportions of class 0 and class 1

n_pop <- 100
d_mc_pop <- as.data.table(cbind(x=rnorm(n_pop), y=rnorm(2*n_pop)))
d_mc_pop$pop <- rep(c("pop0", "pop1"), each=n_pop)
d_mc_pop$label <- sample(c(0, 1), 2*n_pop, prob=c(1, 5), replace=TRUE)
d_mc_pop$label[seq_len(n_pop)] <- sample(c(0, 1), n_pop, prob=c(5, 1), replace=TRUE)

# classification models (trained on populations)
m_mc_pop0 <- xgboost(as.matrix(d_mc_pop[pop=="pop0", c("x", "y")]),
                     label=d_mc_pop[pop=="pop0"]$label,
                     nrounds=2, num_class=2, verbose=0,
                     objective="multi:softprob")
m_mc_pop1 <- xgboost(as.matrix(d_mc_pop[pop=="pop1", c("x", "y")]),
                     label=d_mc_pop[pop=="pop1"]$label,
                     nrounds=2, num_class=2, verbose=0,
                     objective="multi:softprob")

