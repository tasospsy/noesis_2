require( lavaan )

HF     <- 'visual  =~ l_1*x1 + l_2*x2 + l_3*x3
           textual =~ l_4*x4 + l_5*x5 + l_6*x6
           speed   =~ l_7*x7 + l_8*x8 + l_9*x9
           
           g =~ g1*visual + g2*textual + g3*speed
           
           l1 := g1*l_1
           l2 := g1*l_2
           l3 := g1*l_3
           l4 := g2*l_4
           l5 := g2*l_5
           l6 := g2*l_6           
           l7 := g3*l_7
           l8 := g3*l_8
           l9 := g3*l_9           
          '

BF     <- 'visual  =~ l_1*x1 + l_2*x2 + l_3*x3
           textual =~ l_4*x4 + l_5*x5 + l_6*x6
           speed   =~ l_7*x7 + l_8*x8 + l_9*x9
           
           g =~ l1*x1 + l2*x2 + l3*x3 + l4*x4 + l5*x5 + l6*x6 + l7*x7 + l8*x8 + l9*x9
          '
constraints <- '
               c1 := l_1/l1
               c2 := l_4/l4
               c3 := l_7/l7

               l_2 == c1*l2
               l_3 == c1*l3

               l_5 == c2*l5
               l_6 == c2*l6

               l_8 == c3*l8
               l_9 == c3*l9
               '

fitHF     <- cfa( HF, data = HolzingerSwineford1939, std.lv = TRUE )
fitBF     <- cfa( BF, data = HolzingerSwineford1939, orthogonal = TRUE, std.lv = TRUE )
fitHFasBF <- cfa( BF, data = HolzingerSwineford1939, orthogonal = TRUE, std.lv = TRUE, constraints = constraints )

anova( fitHF,     fitBF     )
anova( fitHFasBF, fitBF     )
anova( fitHF,     fitHFasBF )

library(ockhamSEM)


p <- 9 # number of variables
temp_mat <- diag(p) # identity matrix
dimnames(temp_mat) <- list(paste0("x", seq(1, p)),paste0("x", seq(1, p)))

fitHFt     <- cfa( HF, sample.cov = temp_mat, 
                   sample.nobs = 500, std.lv = TRUE,
                   #optim.force.converged = TRUE
                   )
fitBFt     <- cfa( BF, sample.cov = temp_mat, 
                   sample.nobs = 500, orthogonal = TRUE, std.lv = TRUE,
                   #optim.force.converged = TRUE 
                   )
fitHFasBFt <- cfa( BF, sample.cov = temp_mat, 
                   sample.nobs = 500, orthogonal = TRUE, std.lv = TRUE, 
                   #constraints = constraints ,optim.force.converged = TRUE
                   )

cl <- makeCluster(6)
startt <- Sys.time()
res <- run.fitprop(fitHFt, fitBFt, fitHFasBFt,
                     fit.measure=c("rmsea","cfi", "tli"),
                     rmethod="onion",reps=1000, cluster = cl, 
                   saveR = TRUE, saveModel = TRUE)
endt <- Sys.time()
endt - startt
#Time difference of 3.42861 hours
stopCluster(cl)
plot(res)
summary(res)












