library(ggplot2)
library(dplyr)

load("ny_cleaned.rda")
load("finaldata.rda")
load("reconstruct_error.rda")

# PCA + score method

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
    geom_histogram(bins = 50) +
    coord_cartesian(xlim=c(0,500000)) +
    scale_y_log10() +
    ggtitle("Fraud Score of Heuristic Algorithm") +
    xlab("Fraud Score") +
    ylab("count") +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18))

## find the index of top 10% and 1% records
cut_off = round(dim(ma_dist)[1] * 0.1)
top10_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off]
cut_off2 = round(dim(ma_dist)[1] * 0.01)
top1_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off2]


# Autoencoder method

## Fraud score
auto_score = data.frame(rowSums(err))
auto_score = cbind(1:dim(err)[1], auto_score)
colnames(auto_score) = c("Record", "auto_score")

summary(auto_score$auto_score)
quantile(ma_dist$ma_dist, 0.99)

## histogram of the scores
ggplot(data = auto_score, aes(x = auto_score)) +
    geom_histogram(bins = 100) +
    coord_cartesian(xlim=c(1e-10,1)) +
    scale_y_log10() +
    ggtitle("Fraud Score of Autoencoder") +
    xlab("Fraud Score") +
    ylab("count") +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18))

summary(auto_score$auto_score)
quantile(auto_score$auto_score, 0.99)

## get the index of the top 10% and 1% records
top10_index_auto = order(auto_score$auto_score, decreasing = TRUE)[1:cut_off]
top1_index_auto = order(auto_score$auto_score, decreasing = TRUE)[1:cut_off2]

## compare fraud records of the two algorithms
fraud_compare <- function(percentage, cut_off = FALSE) {
    if (!cut_off) {
        cut_off = round(dim(ma_dist)[1] * percentage)
    }
    top_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off]
    top_index_auto = order(auto_score$auto_score, decreasing = TRUE)[1:cut_off]
    count = 0
    for (i in top_index_auto) {
        if (i %in% top_index_ma) {
            count = count + 1
        }
    }
    print(count/cut_off)
    print(paste0(round(count/cut_off,2), " of records matched"))
}

fraud_compare(0.1) #top 10%: 70% matched
fraud_compare(0.01) #top 1%: 69% matched
fraud_compare(0.005) #top 0.5%: 66% matched
fraud_compare(0.001) #top 0.1%: 72% matched
fraud_compare(percentage = NULL, cut_off = 100) #top 100: 70 matched
fraud_compare(percentage = NULL, cut_off = 10) #top 10: 3 matched

## extract the top 1% records of both algorithm
cut_off = round(dim(ma_dist)[1] * 0.01)
top_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off]
top_index_auto = order(auto_score$auto_score, decreasing = TRUE)[1:cut_off]

overlap = ny_property %>%
    filter(RECORD %in% top_index_ma, RECORD %in% top_index_auto)

save(overlap, file = "overlap_1percent.rda")

## extract the top 10 overlap records 
top_10 = NULL
for (i in top_index_auto) {
    if (i %in% top_index_ma) {
        top_10 = c(top_10, i)
    }
    if (length(top_10) == 10) {
        break
    }
}

overlap_top10 = ny_property %>%
    filter(RECORD %in% top_10)

save(overlap_top10, file = "overlap_top10.rda")

