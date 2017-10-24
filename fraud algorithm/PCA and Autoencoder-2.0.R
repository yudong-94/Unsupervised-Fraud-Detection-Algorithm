### Chong's code
library(dplyr)
load('ny_ready.rda')
#load('ny_ready_100000.rda')

data=ny_cleaned %>%
  select(-(RECORD:EXTOT))

apply(data,2,mean)
apply(data,2,var)

## Do the z-scale and PCA
pr.out=prcomp(data,scale=TRUE)
# names(pr.out)
pr.out$center
pr.out$scale

## get the result of PCA
# pr.out$rotation
# pr.out$sdev
pr.var=pr.out$sdev^2
# pr.var
pve=pr.var/sum(pr.var)
# pve

## scree plot to understand the smallest number of principla componets required
## focus on the point at which the proportion of variance drops off

plot(pve,xlab='Principal Component',ylan='Proportion of Variance Explained',
     ylim=c(0,0.2),type='b',main='Scree Plot')
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1), type='b')

## PC1-14 are enough 

pca=pr.out$rotation 
pca=data.frame(pca)

## selecting only PC1 to PC14
pcaneed=pca %>%
  select(PC1:PC14)

## Transform the z scaled variables into PC space 
## by multiplying 2 matrix
finaldata=as.matrix(data) %*% as.matrix(pcaneed)
finaldata=data.frame(finaldata)
save(finaldata, file = "finaldata.rda")

## calculate fraud score
mean = colMeans(finaldata)
cov = cov(finaldata)
ma_dist = mahalanobis(finaldata, mean, cov)
ma_dist = data.frame(cbind(1:length(ma_dist), ma_dist))
colnames(ma_dist) = c("Record", "ma_dist")

summary(ma_dist$ma_dist)
quantile(ma_dist$ma_dist, 0.99)

## plot a histogram
ggplot(data = ma_dist, aes(x = ma_dist)) +
    geom_histogram() +
    scale_x_log10()

## find the index of top 10% and 1% records
cut_off = round(dim(ma_dist)[1] * 0.1)
top10_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off]
cut_off2 = round(dim(ma_dist)[1] * 0.01)
top1_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off2]

## Autoencoder using h2o
library(h2o)
localH2O = h2o.init()

finaldata.hex = as.h2o(finaldata)

finaldata.dl = h2o.deeplearning(x = names(finaldata.hex), training_frame = finaldata.hex,
                               autoencoder = TRUE,
                               reproducible = F)
## Reconstruct
finaldata.anon = h2o.anomaly(finaldata.dl, finaldata.hex, per_feature=TRUE)
err = as.data.frame(finaldata.anon)
  # the "err" data frame contains all reconstruction errors of each record

## Plot the reconstructed Squared Error for the first 3 PCs
plot(err$reconstr_PC1.SE, main='Reconstruction Error - PC1', 
     ylab = "Reconstruction Error")
plot(err$reconstr_PC2.SE, main='Reconstruction Error - PC2', 
     ylab = "Reconstruction Error")
plot(err$reconstr_PC3.SE, main='Reconstruction Error - PC3', 
     ylab = "Reconstruction Error")

## Fraud score
save(err, file = "reconstruct_error.rda")

## histogram of the scores
ggplot(data = err, aes(x = reconstr_PC1.SE)) +
    geom_histogram() +
    scale_x_log10()

ggplot(data = err, aes(x = log(reconstr_PC1.SE))) +
    geom_histogram() +
    xlim(-21,0)

## get the index of the top 10% and 1% records
top10_index_auto = order(err$reconstr_PC1.SE, decreasing = TRUE)[1:cut_off]
top1_index_auto = order(err$reconstr_PC1.SE, decreasing = TRUE)[1:cut_off2]

## compare fraud records of the two algorithms
fraud_compare <- function(percentage, cut_off = FALSE) {
    if (!cut_off) {
        cut_off = round(dim(ma_dist)[1] * percentage)
    }
    top_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off]
    top_index_auto = order(err$reconstr_PC1.SE, decreasing = TRUE)[1:cut_off]
    count = 0
    for (i in top_index_auto) {
        if (i %in% top_index_ma) {
            count = count + 1
        }
    }
    print(paste0(round(count/cut_off,2), " of records matched"))
}

fraud_compare(0.1) #top 10%: 21% matched
fraud_compare(0.01) #top 1%: 38% matched
fraud_compare(0.005) #top 0.5%: 35% matched
fraud_compare(0.001) #top 0.1%: 32% matched
fraud_compare(percentage = NULL, cut_off = 100) #top 100: 14 matched
fraud_compare(percentage = NULL, cut_off = 10) #top 10: 4 matched

