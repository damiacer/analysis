install.packages("randomizeR")
library("randomizeR")

sample(1:370, 10, replace = F)

?sample


N = 370

#Object of class "rarPar"
# design = CR
# groups = A B

N=134
K=2
params <- rarPar(N, K, ratio = rep(1, K))

R <- genSeq(params)
getRandList(R)


myPar <- crPar(370)
random <- genSeq(myPar, 10, seed = 21051986)
genSeq(myPar)
getRandList(random)

#--
install.packages("randomizr")
library("randomizr")

simple_ra(
N = 370,
#prob = ,
#prob_unit = NULL,
prob_each = c(0.05, 0.95),
num_arms = 2,
conditions = c("1", "0"),
check_inputs = TRUE,
simple = TRUE
)
