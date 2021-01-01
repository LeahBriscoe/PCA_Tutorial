# Load data
data_dir = "~/Documents/Tutorial/DataViz/PCA/"

# data downloaded from https://www.kaggle.com/mrmorj/dataset-of-songs-in-spotify
data_file = "genres_v2.csv"

require(data.table)


# fread to quickly read the data
data = fread(paste0(data_dir,data_file),sep = ",",header = TRUE,stringsAsFactors = FALSE)
data = data.frame(data)

# extract just the numeric features
numerical_features = data[,c("danceability","energy","key","loudness","mode",
                             "speechiness",	"acousticness",	"instrumentalness",	
                             "liveness","valence","tempo","duration_ms","time_signature")]
dim(numerical_features)
# define the groupings we are interested in
genres = as.character(data$genre)
table(genres)


# Perform PCA with prcomp
pca_result = prcomp(numerical_features, scale = TRUE)
dim(pca_result$x)

## plotting function for PCA
plot_pca = function(df_out,key,title_input,pairs1,pairs2){
  # example input
  # df_out = pca_result$x
  # key = genres
  # title_input = "PCs colored by Genres"
  # pairs1 = c(1,3)
  # pairs2 = c(2,4)
  
  df_out=data.frame(df_out)
  colnames(df_out) = paste0("PC",1:ncol(df_out))
  df_out$group = key

  plot_list = list()
  for(j in 1:length(pairs1)){
    df_out$plotx = df_out[,paste0("PC",pairs1[j])]
    df_out$ploty = df_out[,paste0("PC",pairs2[j])]
    
    t1 = Sys.time()
    p<-ggplot(df_out,aes(x=plotx,y=ploty,color=group)) + ggtitle(title_input) 
    p<-p + geom_point() + theme_bw()  + xlab(paste0("PC",pairs1[j])) + ylab(paste0("PC",pairs2[j]))
    p <-p + theme(aspect.ratio=1,text = element_text(size=10),axis.text.x = element_text(size=10),
                  axis.text.y = element_text(size=10))
    plot_list[[j]] = p
    
    print(Sys.time() -t1)
    #return(p)
  }
  return(plot_list)
}

# plot 
p <- plot_pca(pca_result$x,genres,"Musical Genres",c(1,3),c(2,4))

# princomp is similar
# sdev = sdev
# rotation = loadings
# center = center
# scale = scale
# x = score
princom_result = princomp(scale(numerical_features,center = TRUE),scores = TRUE)
# OR
princom_result = princomp(numerical_features,cor=TRUE)

p <- plot_pca(princom_result$scores,genres,"Genres",c(1),c(2))

# SAVE the pca plot to PDF
ggsave(p[[1]],file=paste0(data_dir,"/PCA_MusicalGenres_PC1_2.pdf"),device="pdf")
# 


# get your own eigenvalues
eigenvalues =pca_result$sdev^2
prop_eigen = eigenvalues /sum(eigenvalues)
plot(prop_eigen)

# skree plot eigenvalues
#Load factoextra for visualization
library(factoextra)
#Compute PCA
#Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
fviz_eig(pca_result)

# sampling just some songs
sampling = sample(1:nrow(data),size =1000)
sample_numerical_features = numerical_features[sampling,]
sample_genres = genres[sampling]

dim(numerical_features)
pca_result = prcomp(sample_numerical_features, scale = TRUE)
dim(pca_result$x)
p <- plot_pca(pca_result$x,sample_genres,"PCs colored by Muscial Genres",c(1,3),c(2,4))

# sampling genres

select_genres = c("Underground Rap","Hiphop","techno","RnB")
#select_genres = c("Dark Trap","techno","Hiphop")
#select_genres = c("psytrance","Hiphop")
sampling = which(genres %in% select_genres)
sample_numerical_features = numerical_features[sampling,]
sample_genres = genres[sampling]
dim(sample_numerical_features)
pca_result = prcomp(sample_numerical_features)
dim(pca_result$x)
p <- plot_pca(pca_result$x,sample_genres,"PCs colored by Musical Genres",c(1,3),c(2,4))

# viewing and understanding loadings/linear combination of features for each PC
dim(pca_result$rotation)
loadings = pca_result$rotation
loadings[1:4,1:4]
sort(loadings[,1],decreasing=TRUE)


# fast pca
require(bigstatsr)
t1=Sys.time()  
pca_result = prcomp(numerical_features, scale = TRUE)
print(Sys.time()-t1)

t1=Sys.time()  
myFBM = as_FBM(numerical_features, type = c("double"))
svd_result = big_SVD(myFBM,k=6,fun.scaling = big_scale(center=TRUE,scale=TRUE))
print(Sys.time()-t1)
pca_result$x[1:4,1:4]
pca_score <-  svd_result$u %*% diag(svd_result$d)
pca_score[1:4,1:4] 


# other useful resources 
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/#prcomp-and-princomp-functions
