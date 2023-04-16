library(haven)
setwd(choose.dir())

# -------------- PCA -----------------
data_crimes <- read_sav("PCA.sav")
prin_comp <- prcomp(data_crimes[,-1], scale. = T)
class(data_crimes)
#compute standard deviation of each principal component
std_dev <- prin_comp$sdev
#compute variance
pr_var <- std_dev^2
#check variance of first 10 components
pr_var
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex*100
#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")


# test.data <- predict(prin_comp, newdata = pca.test)
# -------------- FA -----------------
data_medicine <- read_sav("FA.sav")
data_medicine <- data_medicine[1:64,]
factor.computer<-factanal(data_medicine,factors=3,scores="regression")
factor.computer$loadings[,2]
factor.computer$loadings[,3]
factor.computer$loadings[,1]
