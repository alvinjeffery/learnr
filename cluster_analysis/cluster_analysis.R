
# load required packages
library(shiny); library(d3heatmap); library(scales)
library(rms); library(data.table); library(dplyr); library(lubridate)
library(stringr); library(tidyr); library(gplots); library(DT)
library(cluster); library(fpc); library(mlbench); library(dendextend)
library(mclust)

# load & explore iris data
df <- iris
str(df)
summary(df)

# scale & center
df <- scale(df) # will produce an error
df <- select(df, -Species) # remove classifiers
df$Species <- NULL # if you don't have dplyr package
df <- scale(df)
summary(df)

## k-means/medoids ##
# requires non-missing data in a numerical matrix
# simple but usually not ideal

# Determine number of clusters
wss <- (nrow(df) - 1) * sum(apply(df, 2, var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(df, centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# actual k-means analysis
fit <- kmeans(df, 5) # 5 cluster solution
fit

# get cluster means 
aggregate(df, by=list(fit$cluster), FUN=mean)
# append cluster assignment
df_with_clusters <- data.frame(df, fit$cluster)
df_with_clusters

# if we know we haven't changed the order of anything
iris_with_clusters <- data.frame(iris, fit$cluster)
View(iris_with_clusters)
# consider changing to 3 (e.g., if you had subject-matter knowledge)


# can also be done with pam (k-medoids)
?pam
pam_clusters <- pam(df, 5)
summary(pam_clusters)
plot(pam_clusters)


## hierarchical agglomerative ##

# first, create a dissimilarity matrix 
d <- dist(df, method = 'euclidean')
fit <- hclust(d, method = 'ward.D')
plot(fit)
# view clusters on dendrogram 
rect.hclust(fit, k=5, border = 'red')

# find labels for each row
groups <- cutree(fit, 3)
groups 




## model-based approaches ##
# use MLE and Bayes criteria
fit <- Mclust(df)
plot(fit)
summary(fit)


## plotting & validating ## - where the fun begins!

# k-means 
fit <- kmeans(df, 3)

clusplot(df, 
         fit$cluster, 
         color = TRUE, 
         shade = TRUE, 
         labels = 2, 
         lines = 0)

plotcluster(df, fit$cluster)


fit1 <- kmeans(df, 5)
fit2 <- kmeans(df, 2)

# one option for interpreting/validating
cluster.stats(d, # distance matrix
              fit1$cluster, 
              alt.clustering = fit2$cluster)


# Alvin's approach
agg <- aggregate(df, list(groups), FUN=mean, na.rm=T)
agg

# store aggregated summaries as new variable & melt for ggplot
agg <- as.data.frame(agg) %>% 
  # add group/cluster numbers back
  mutate(Group.1 = seq(1, nrow(agg), by=1)) %>% 
  # melt for heat map
  melt(id.vars = 'Group.1')

# Heat Map of Clusters
p <- ggplot(agg, aes(x=variable, y=Group.1, fill=value)) + geom_tile() 

print(p + 
        scale_fill_gradient(low="ghostwhite", high="deeppink3", na.value="grey") + 
        theme(axis.text.x = element_text(angle=330)) + 
        labs(title=paste("Average Value for Each Cluster/Group"), 
             x="Variables", 
             y="Cluster") 
) # end plot print





# try with some health-related data
getHdata(support)
support_num <- select(support, age, wblc, hrt, resp, temp, crea)
#wblc & crea are missing
median_wblc <- median(support_num$wblc, na.rm = TRUE)
median_crea <- median(support_num$crea, na.rm = TRUE)


support_num$wblc <- ifelse(is.na(support_num$wblc), 
                           median_wblc, 
                           support_num$wblc)
support_num$crea <- ifelse(is.na(support_num$crea), 
                           median_crea, 
                           support_num$crea)
# alternatively
support_num <- apply(support_num, 2, function(x) impute(x, median)) # median/mode imputation

# scale/center data
support_num <- scale(support_num)


# first, create a dissimilarity matrix 
d <- dist(support_num, method = 'euclidean')
fit <- hclust(d, method = 'ward.D')
plot(fit)
# find labels for each row
groups <- cutree(fit, 6)
groups 
# view clusters on dendrogram 
rect.hclust(fit, k=6, border = 'red')

# Alvin's approach
agg <- aggregate(support_num, list(groups), FUN=mean, na.rm=T)
agg

# store aggregated summaries as new variable & melt for ggplot
agg <- as.data.frame(agg) %>% 
  # add group/cluster numbers back
  mutate(Group.1 = seq(1, nrow(agg), by=1)) %>% 
  # melt for heat map
  melt(id.vars = 'Group.1')

# Heat Map of Clusters
p <- ggplot(agg, aes(x=variable, y=Group.1, fill=value)) + geom_tile() 

print(p + 
        scale_fill_gradient(low="ghostwhite", high="steelblue", na.value="grey") + 
        theme(axis.text.x = element_text(angle=330)) + 
        labs(title=paste("Average Value for Each Cluster/Group"), 
             x="Variables", 
             y="Cluster") 
) # end plot print

# what if we DO have an outcome already available? 
with_outcome <- data.frame(support_num, support$hospdead)
# start back at the aggregation level & repeat code...
agg <- aggregate(with_outcome, list(groups), FUN=mean, na.rm=T)
agg

agg <- as.data.frame(agg) %>% 
  mutate(Group.1 = seq(1, nrow(agg), by=1)) %>% 
  # increase scale of death proportion to make easier to see
  mutate(support.hospdead = 3*support.hospdead) %>% 
  melt(id.vars = 'Group.1')

p <- ggplot(agg, aes(x=variable, y=Group.1, fill=value)) + geom_tile() 
print(p + 
        scale_fill_gradient(low="ghostwhite", high="steelblue", na.value="grey") + 
        theme(axis.text.x = element_text(angle=330)) + 
        labs(title=paste("Average Value for Each Cluster/Group"), 
             x="Variables", 
             y="Cluster") 
) # end plot print

### can we please turn this into a function?





### odds & ends for cluster analysis displays ###

# scale & center data for cluster analysis
df <- iris 
df <- select(df, -Species) # remove classifiers
df <- scale(df)


## create colorful 'ladderized' dendorgrams ##

colv <- df %>% 
  t %>% 
  daisy(metric = 'gower') %>% 
  hclust(method='ward.D2') %>% 
  as.dendrogram %>% 
  set("branches_k_color", k=3) %>% 
  set("branches_lwd", 1) %>% 
  ladderize 

rowv <- df %>% 
  #t %>% 
  daisy(metric = 'gower') %>%
  hclust(method='ward.D2') %>% 
  as.dendrogram %>% 
  set("branches_k_color", k=5) %>% 
  set("branches_lwd", 2) %>% 
  ladderize 

my_colors <- colorRampPalette(c('red', 'yellow', 'green')) (n=299)

heatmap.2(as.matrix(df), 
          hclustfun = hclust, 
          Rowv = rowv, 
          Colv = colv, 
          dendrogram = "both", 
          col = my_colors,
          trace = "none", 
          scale = "col", 
          labRow = NULL, 
          labCol = colnames(df), 
          srtCol = -33,
          cexCol = 1
)


## circlize ##
dend <- df %>% 
  daisy(metric='gower') %>% # 
  hclust(method='complete') %>% # complete linkage to find similar clusters
  as.dendrogram %>% 
  set("branches_k_color", k=3) %>% 
  set("branches_lwd", 1) %>% 
  set("labels_colors") %>% 
  set("labels_cex", 0.5)
par(mar=rep(0,4))
circlize_dendrogram(dend)
