#=================================================================================================
# THE IMDB PREDICTION CHALLENGE - VISUALIZATION CODE
#=================================================================================================
movies = read.csv("D:/Multivariate_Final_Project/data_final.csv") # original
movies5 = read.csv("D:/Multivariate_Final_Project/data_final_fri.csv")
movies8 = read.csv("D:/Multivariate_Final_Project/data_final_mon.csv") # Last 

detach(movies4)
attach(movies5)
install.packages("ggplot2")
library(dplyr)
library(ggplot2)
library(gridExtra)
install.packages("reshape2")
library(reshape2)

#=================================================================================================
# VISUALIZATION WITH GGPLOT2
#=================================================================================================
# Define default empty plot, layers can be added according to specific needs
a = ggplot(data = movies8) + theme_minimal(base_size = 12) + 
  theme(axis.text = element_text(color = "grey40", face = "bold", 
                                 margin = margin(10, 0, 10, 0)))

b = ggplot(data = movies) + theme_minimal(base_size = 12) + 
  theme(axis.text = element_text(color = "grey40", face = "bold", 
                                 margin = margin(10, 0, 10, 0)))

#These lines allow to save the graphs in specific directories
# To save one single squared graph or squared matrix
ggsave("score_hist.png", plot = score_hist, path = "D:/Multivariate_Final_Project", width = 5, height = 4)

# To save one-row - multiple-columns matrices
ggsave("score_hist_box1.png", plot = score_hist_box, path = "D:/Multivariate_Final_Project", width = 6, height = 3)

#=================================================================================================
# TARGET VARIABLE: imdb_score Complete Dataset
#================================================================================================
# histogram
score_hist = b + geom_histogram(mapping = aes(x = imdb_score), binwidth = .4, 
                                color = "black", fill = "dodgerblue2", alpha = 0.6) + 
  labs(title = "IMDB Score Distribution", subtitle = "Complete Dataset", x = "IMDB Score (stars)", 
       y = "Frequency", caption = "Source: data_final.csv")

# Boxplot
score_box = b + geom_boxplot(mapping = aes(x = "", y = imdb_score), fill = "dodgerblue2", alpha = 0.6, 
                             outlier.color = "firebrick3", outlier.alpha = 0.4, outlier.size = 2) + 
  labs(title = "IMDB Score", subtitle = "Complete Dataset", x = NULL, 
       y = "IMDB Score (stars)", caption = "Source: data_final.csv")

grid.arrange(score_hist, score_box, ncol = 2)

# TARGET VARIABLE: imdb_score Movies after 1995
# histogram
score_hist1 = a + geom_histogram(mapping = aes(x = imdb_score), binwidth = .4, 
                                color = "black", fill = "dodgerblue2", alpha = 0.6) + 
  labs(title = "IMDB Score Distribution", subtitle = "Movies after 1995", x = "IMDB Score (stars)", 
       y = "Frequency", caption = "Source: data_final.csv")

# Boxplot
score_box1 = a + geom_boxplot(mapping = aes(x = "", y = imdb_score), fill = "dodgerblue2", alpha = 0.6, 
                 outlier.color = "firebrick3", outlier.alpha = 0.4, outlier.size = 2) + 
  labs(title = "IMDB Score", subtitle = "Movies after 1995", x = NULL, 
       y = "IMDB Score (stars)", caption = "Source: data_final.csv")

grid.arrange(score_hist1, score_box1, ncol = 2)

#=================================================================================================
# Creating correlation matrices with heatmaps
#================================================================================================
# Filter movies dataset to remove movie Lost Souls

attach(movies)
detach(movies)
# First create a subset of the numeric variables (many ways to do this)
original_numeric1 = movies[, c(3, 5, 6, 7, 8, 12, 14)]
names(original_numeric1)[1] = "IMDB Score"
names(original_numeric1)[2] = "Release Day"
names(original_numeric1)[3] = "Release Year"
names(original_numeric1)[4] = "Duration"
names(original_numeric1)[5] = "Budget"
names(original_numeric1)[6] = "Aspect Ratio"
names(original_numeric1)[7] = "News Articles"

original_numeric2 = movies[, c(3, 16, 18, 20, 22, 24, 26)]
names(original_numeric2)[1] = "IMDB Score"
names(original_numeric2)[2] = "Dir. FB Likes"
names(original_numeric2)[3] = "Act.1 FB Likes"
names(original_numeric2)[4] = "Act.1 Star Mt"
names(original_numeric2)[5] = "Act.2 FB Likes"
names(original_numeric2)[6] = "Act.2 Star Mt"
names(original_numeric2)[7] = "Act.3 FB Likes"

original_numeric3 = movies[, c(3, 28, 31, 33, 34, 35, 37)]
names(original_numeric3)[1] = "IMDB Score"
names(original_numeric3)[2] = "Act.3 Star Mt"
names(original_numeric3)[3] = "Critic reviews"
names(original_numeric3)[4] = "User Votes"
names(original_numeric3)[5] = "Cast FB Likes"
names(original_numeric3)[6] = "Faces in Poster"
names(original_numeric3)[7] = "User Reviews"

original_numeric4 = movies[, c(3, 38, 39, 54, 55, 56, 53)]
names(original_numeric4)[1] = "IMDB Score"
names(original_numeric4)[2] = "Movie Budget"
names(original_numeric4)[3] = "Movie FB Likes"
names(original_numeric4)[4] = "Likes Ratio"
names(original_numeric4)[5] = "Movie Meter"
names(original_numeric4)[6] = "Votes"
names(original_numeric4)[7] = "Total Likes"

# then use the cormat function (native function from R)
cormat1 = round(cor(original_numeric1, use = "complete.obs"), 2)
cormat2 = round(cor(original_numeric2, use = "complete.obs"), 2)
cormat3 = round(cor(original_numeric3, use = "complete.obs"), 2)
cormat4 = round(cor(original_numeric4, use = "complete.obs"), 2)
# Get upper triangle of each correlation matrix
cormat1[lower.tri(cormat1)] <- NA
upper_tri1 = get_upper_tri(cormat1)

cormat2[lower.tri(cormat2)] <- NA
upper_tri2 = get_upper_tri(cormat2)

cormat3[lower.tri(cormat3)] <- NA
upper_tri3 = get_upper_tri(cormat3)

cormat4[lower.tri(cormat4)] <- NA
upper_tri4 = get_upper_tri(cormat4)

# Then met the new correlation matrix (Library Reshape2)
melted_cormat1 = melt(upper_tri1, na.rm = TRUE)
melted_cormat2 = melt(upper_tri2, na.rm = TRUE)
melted_cormat3 = melt(upper_tri3, na.rm = TRUE)
melted_cormat4 = melt(upper_tri4, na.rm = TRUE)

# Create a ggheatmap for each correlation matrix
# ggheatmap 1
ggheatmap1 = ggplot(data = melted_cormat1, aes(Var2, Var1, fill = value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "cyan", high = "dodgerblue4", mid = "dodgerblue", midpoint = 0, 
                       limit = c(-1,1), space = "Lab", name ="Correlation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(color = "grey40", face = "bold", angle = 45, 
                                   vjust = 1, size = 10, hjust = 1), 
        axis.text.y = element_text(color = "grey40", face = "bold")) + 
  coord_fixed()

# ggheatmap 2
ggheatmap2 = ggplot(data = melted_cormat2, aes(Var2, Var1, fill = value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "cyan", high = "dodgerblue4", mid = "dodgerblue", midpoint = 0, 
                       limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(color = "grey40", face = "bold", angle = 45, 
                                   vjust = 1, size = 10, hjust = 1), 
        axis.text.y = element_text(color = "grey40", face = "bold")) + 
  coord_fixed()

# ggheatmap 3
ggheatmap3 = ggplot(data = melted_cormat3, aes(Var2, Var1, fill = value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "cyan", high = "dodgerblue4", mid = "dodgerblue", midpoint = 0, 
                       limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(color = "grey40", face = "bold", angle = 45, 
                                   vjust = 1, size = 10, hjust = 1), 
        axis.text.y = element_text(color = "grey40", face = "bold")) + 
  coord_fixed()

# ggheatmap 4
ggheatmap4 = ggplot(data = melted_cormat4, aes(Var2, Var1, fill = value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "cyan", high = "dodgerblue4", mid = "dodgerblue", midpoint = 0, 
                       limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(color = "grey40", face = "bold", angle = 45, 
                                   vjust = 1, size = 10, hjust = 1), 
        axis.text.y = element_text(color = "grey40", face = "bold")) + 
  coord_fixed()

# Finally, add correlation coefficients on the heatmap
# 1. Use geom_text() to add the correlation coefficients on the graph
# 2. Use a blank theme (remove axis labels, panel grids and background, and axis ticks)
# 3. Use guides() to change the position of the legend title
#Correlation Matrix 1
corm1 = ggheatmap1 + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())

#Correlation Matrix 2
corm2 = ggheatmap2 + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()) 

#Correlation Matrix 3
corm3 = ggheatmap3 + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())  

#Correlation Matrix 4
corm4 = ggheatmap4 + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())

# Arrange all matrices in a grid
grid.arrange(corm1, corm2, corm3, corm4, ncol = 2,
             top = "Correlation Matrices", bottom = "Source: data_final.csv")

# Build correlation matrix for final model
final_numeric = movies8[, c(3, 7, 69, 64, 61, 62)]
names(final_numeric)[1] = "IMDB Score"
names(final_numeric)[2] = "Duration"
names(final_numeric)[3] = "CR Movie FB Likes"
names(final_numeric)[4] = "CR Director FB Likes"
names(final_numeric)[5] = "Log Movie Meter"
names(final_numeric)[6] = "SR News Articles"

# then use the cormat function (native function from R)
cormat_final = round(cor(final_numeric, use = "complete.obs"), 2)
# Get upper triangle of each correlation matrix
cormat_final[lower.tri(cormat_final)] <- NA
upper_tri_final = get_upper_tri(cormat_final)

# Then met the new correlation matrix (Library Reshape2)
melted_cormat_final = melt(upper_tri_final, na.rm = TRUE)

# Create a ggheatmap for correlation matrix of final model
# ggheatmap_final
ggheatmap_final = ggplot(data = melted_cormat_final, aes(Var2, Var1, fill = value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "cyan", high = "dodgerblue4", mid = "dodgerblue", midpoint = 0, 
                       limit = c(-1,1), space = "Lab", name ="Correlation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(color = "grey40", face = "bold", angle = 45, 
                                   vjust = 1, size = 10, hjust = 1), 
        axis.text.y = element_text(color = "grey40", face = "bold")) + 
  coord_fixed()

# Finally, add correlation coefficients on the final heatmap
# 1. Use geom_text() to add the correlation coefficients on the graph
# 2. Use a blank theme (remove axis labels, panel grids and background, and axis ticks)
# 3. Use guides() to change the position of the legend title
#Correlation Matrix Final Model
ggheatmap_final + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()) + 
  labs(title = "Correlation Matrix", subtitle = "Final Model",
       caption = 
         "CR: cubic root transformation\nSR: square root transformation\nLOG: logaritmic transformation")

#================================================================================================
# PREDICTOR: release_year
#================================================================================================
# Histogram based on original data. Use b for complete dataset
year_hist = b + geom_histogram(mapping = aes(x = release_year), 
                               binwidth = .4, 
                               fill = "dodgerblue2", alpha = 0.6) +
  labs(title = "Release Year Distribution", subtitle = "Complete Dataset", 
       x = "Release Year", y = "Frequency", 
       caption = "Source: data_final.csv")

# Scatterplot of release_year and imdb_score (Complete dataset)
year_scat = b + geom_point(mapping = aes(x = release_year, y = imdb_score), 
                           color = "dodgerblue2", shape = 16, alpha = 0.6) + 
  labs(title = "Score & Release Year Relationship", subtitle = "Complete Dataset",
       x = "Year", y = "IMDB Score (stars)", caption = "Source: data_final.csv")

grid.arrange(year_hist, year_scat, ncol = 2)

# Histogram for movies after 1995
year_hist1 = a + geom_histogram(mapping = aes(x = release_year), binwidth = .5, fill = "dodgerblue2", alpha = 0.6) +
  labs(title = "Release Year Distribution", subtitle = "Movies after 1995", x = "Release Year", 
       y = "Frequency", caption = "Source: data_final.csv")

# Scatterplot of release_year and imdb_score (after 1995
year_scat1 = a + geom_point(mapping = aes(x = release_year, y = imdb_score), color = "dodgerblue2", shape = 16, alpha = 0.6) + 
  labs(title = "Score & Release Year Relationship", subtitle = "Movies after 1995",
       x = "Year", y = "IMDB Score (stars)", caption = "Source: data_final.csv")

grid.arrange(year_hist1, year_scat1, ncol = 2)

#=================================================================================================
# PREDICTOR: duration_mins
#================================================================================================
# Histogram based on original data. Use b for complete dataset
duration_hist = b + geom_histogram(mapping = aes(x = duration_mins), 
                               binwidth = .2, color = "dodgerblue2", 
                               fill = "dodgerblue2", alpha = 0.6) +
  labs(title = "Duration Distribution", subtitle = "Complete Dataset", 
       x = "Duration (mins)", y = "Frequency", 
       caption = "Source: data_final.csv")

# Scatterplot of release_year and imdb_score (Complete dataset)
duration_scat = b + geom_point(mapping = aes(x = duration_mins, y = imdb_score), 
                           color = "dodgerblue2", shape = 16, alpha = 0.6) + 
  labs(title = "Score & Duration Relationship", subtitle = "Complete Dataset",
       x = "Duration (mins)", y = "IMDB Score (stars)", caption = "Source: data_final.csv")

grid.arrange(duration_hist, duration_scat, ncol = 2)

# Histogram for movies after 1995
duration_hist1 = a + geom_histogram(mapping = aes(x = duration_mins), 
                                   binwidth = .4, color = "dodgerblue2", 
                                   fill = "dodgerblue2", alpha = 0.6) +
  labs(title = "Duration Distribution", subtitle = "Movies after 1995", 
       x = "Duration (mins)", y = "Frequency", 
       caption = "Source: data_final.csv")


# Scatterplot of release_year and imdb_score (after 1995
duration_scat1 = a + geom_point(mapping = aes(x = duration_mins, y = imdb_score), 
                               color = "dodgerblue2", shape = 16, alpha = 0.6) + 
  labs(title = "Score & Duration Relationship", subtitle = "Movies after 1995",
       x = "Duration (mins)", y = "IMDB Score (stars)", caption = "Source: data_final.csv")

grid.arrange(duration_hist1, duration_scat1, ncol = 2)

#================================================================================================
# PREDICTOR: movie_facebook_likes 
#===============================================================================================
# histogram
movie_fb_hist = a + geom_histogram(mapping = aes(x = movie_facebook_likes), color = "dodgerblue2", 
                   fill = "dodgerblue2", alpha = 0.6) + 
  labs(title = "Movie FB Likes Distribution", subtitle = "Movies after 1995", x = "Movie FB Likes", 
       y = "Frequency", caption = "Source: data_final.csv")

# Boxplot
movie_fb_box = a + geom_boxplot(mapping = aes(x = "", y = movie_facebook_likes), fill = "dodgerblue2", alpha = 0.6, 
                             outlier.color = "firebrick3", outlier.alpha = 0.4, outlier.size = 2) + 
  labs(title = "Movie FB Likes", subtitle = "Movies after 1995", x = NULL, 
       y = "Movie FB Likes", caption = "Source: data_final.csv")

grid.arrange(movie_fb_hist, movie_fb_box, ncol = 2)

#================================================================================================
# PREDICTOR: number_news_articles
#===============================================================================================
# histogram
news_hist = a + geom_histogram(mapping = aes(x = number_news_articles), color = "dodgerblue2", 
                                   fill = "dodgerblue2", alpha = 0.6) + 
  labs(title = "News Articles Distribution", subtitle = "Movies after 1995", x = "News Articles", 
       y = "Frequency", caption = "Source: data_final.csv")

# Boxplot
news_box = a + geom_boxplot(mapping = aes(x = "", y = number_news_articles), fill = "dodgerblue2", alpha = 0.6, 
                                outlier.color = "firebrick3", outlier.alpha = 0.4, outlier.size = 2) + 
  labs(title = "News Articles", subtitle = "Movies after 1995", x = NULL, 
       y = "News Articles", caption = "Source: data_final.csv")

grid.arrange(news_hist, news_box, ncol = 2)

#================================================================================================
# PREDICTOR: director_facebook_likes
#===============================================================================================
# histogram
dir_fb_hist = a + geom_histogram(mapping = aes(x = director_facebook_likes), color = "dodgerblue2", 
                               fill = "dodgerblue2", alpha = 0.6) + 
  labs(title = "Director FB Likes Distribution", subtitle = "Movies after 1995", x = "Director FB Likes", 
       y = "Frequency", caption = "Source: data_final.csv")

# Boxplot
dir_fb_box = a + geom_boxplot(mapping = aes(x = "", y = director_facebook_likes), fill = "dodgerblue2", alpha = 0.6, 
                            outlier.color = "firebrick3", outlier.alpha = 0.4, outlier.size = 2) + 
  labs(title = "Director FB Likes", subtitle = "Movies after 1995", x = NULL, 
       y = "Director FB Likes", caption = "Source: data_final.csv")

grid.arrange(dir_fb_hist, dir_fb_box, ncol = 2)

#================================================================================================
# PREDICTOR: movie_meter_IMDB_pro PENDING - Defining the binwidth may get histograms very slow
#==============================================================================================
# histogram
movie_meter_hist = a + geom_histogram(aes(x = movie_meter_IMDB_pro), color = "dodgerblue2", 
                                 fill = "dodgerblue2", alpha = 0.6) + 
  labs(title = "Movie Meter Distribution", subtitle = "Movies after 1995", x = "Movie Meter", 
       y = "Frequency", caption = "Source: data_final.csv")

a + geom_histogram(aes(movie_meter_IMDB_pro), binwidth = .4)

# Boxplot
movie_meter_box = a + geom_boxplot(aes(x = "", y = movie_meter_IMDB_pro), fill = "dodgerblue2", alpha = 0.6, 
                              outlier.color = "firebrick3", outlier.alpha = 0.4, outlier.size = 2) + 
  labs(title = "Movie Meter", subtitle = "Movies after 1995", x = NULL, 
       y = "Movie Meter", caption = "Source: data_final.csv")

grid.arrange(movie_meter_hist, movie_meter_box, ncol = 2)

#===============================================================================================
# TRANSFORMATION OF 4 PREDICTORS: movie_meter_IMDB_pro      (logaritmic transformation)
#                                 number_news_articles      (square root transformation)
#                                 movie_facebook_likes      (square root transformation)
#                                 director_facebook_likes   (cubic root transformation)
#==============================================================================================
# PREDICTOR: movie_meter_IMDB_pro after log transformation
#============================================================================================
# histogram
movie_meter_hist1 = a + geom_histogram(aes(x = log_movie_meter_IMDB_pro), binwidth = .4, color = "dodgerblue2", 
                                      fill = "dodgerblue2", alpha = 0.6) + 
  labs(title = "Movie Meter Distribution", subtitle = "Movies after 1995 - Log Transform", x = "Movie Meter", 
       y = "Frequency", caption = "Source: data_final.csv")

# Boxplot
movie_meter_box1 = a + geom_boxplot(aes(x = "", y = log_movie_meter_IMDB_pro), fill = "dodgerblue2", alpha = 0.6, 
                                   outlier.color = "firebrick3", outlier.alpha = 0.4, outlier.size = 2) + 
  labs(title = "Movie Meter", subtitle = "Movies after 1995 - Log Transform", x = NULL, 
       y = "Movie Meter", caption = "Source: data_final.csv")

grid.arrange(movie_meter_hist1, movie_meter_box1, ncol = 2)

# Scatterplot of movie_meter_IMDB_pro and imdb_score after log transformation
a + geom_point(aes(x = log_movie_meter_IMDB_pro, y = imdb_score), 
               color = "dodgerblue2", shape = 16, alpha = 0.6) + 
  labs(title = "Score & Movie Meter Relationship", subtitle = "Movies after 1995 - Log Transform",
       x = "Movie Meter", y = "IMDB Score (stars)", caption = "Source: data_final.csv")


#=============================================================================================
#PREDICTOR: number_news_articles after square root transformation
#=============================================================================================
# histogram
news_hist1 = a + geom_histogram(aes(x = sr_number_news_articles), binwidth = .4, color = "dodgerblue2", 
                               fill = "dodgerblue2", alpha = 0.6) + 
  labs(title = "News Articles Distribution", subtitle = "Movies after 1995 - Sqrt Transform", x = "News Articles", 
       y = "Frequency", caption = "Source: data_final.csv")

# Boxplot
news_box1 = a + geom_boxplot(aes(x = "", y = sr_number_news_articles), fill = "dodgerblue2", alpha = 0.6, 
                            outlier.color = "firebrick3", outlier.alpha = 0.4, outlier.size = 2) + 
  labs(title = "News Articles", subtitle = "Movies after 1995 - Sqrt Transform", x = NULL, 
       y = "News Articles", caption = "Source: data_final.csv")

grid.arrange(news_hist1, news_box1, ncol = 2)

# Scatterplot of number_news_articles and imdb_score after sqrt transformation
a + geom_point(aes(x = sr_number_news_articles, y = imdb_score), 
               color = "dodgerblue2", shape = 16, alpha = 0.6) + 
  labs(title = "Score & News Articles Relationship", subtitle = "Movies after 1995 - Sqrt Transform",
       x = "News Articles", y = "IMDB Score (stars)", caption = "Source: data_final.csv")

#==============================================================================================
#PREDICTOR: movie_facebook_likes after cubic root transformation
#=============================================================================================
# histogram
movie_fb_hist1 = a + geom_histogram(aes(x = cr_movie_facebook_likes), binwidth = .4, color = "dodgerblue2", 
                                   fill = "dodgerblue2", alpha = 0.6) + 
  labs(title = "Movie FB Likes Distribution", subtitle = "Movies after 1995 - Cubic Root Transform", x = "Movie FB Likes", 
       y = "Frequency", caption = "Source: data_final.csv")

# Boxplot
movie_fb_box1 = a + geom_boxplot(aes(x = "", y = cr_movie_facebook_likes), fill = "dodgerblue2", alpha = 0.6, 
                                outlier.color = "firebrick3", outlier.alpha = 0.4, outlier.size = 2) + 
  labs(title = "Movie FB Likes", subtitle = "Movies after 1995 - Cubic Root Transform", x = NULL, 
       y = "Movie FB Likes", caption = "Source: data_final.csv")

grid.arrange(movie_fb_hist1, movie_fb_box1, ncol = 2)

# Scatterplot of movie_facebook_likes and imdb_score after sqrt transformation
a + geom_point(aes(x = cr_movie_facebook_likes, y = imdb_score), 
               color = "dodgerblue2", shape = 16, alpha = 0.6) + 
  labs(title = "Score & Movie FB Likes Relationship", subtitle = "Movies after 1995 - Cubic Root Transform",
       x = "Movie FB Likes", y = "IMDB Score (stars)", caption = "Source: data_final.csv")

#===============================================================================================
#PREDICTOR: director_facebook_likes after cubic root transformation
#=============================================================================================
# histogram
dir_fb_hist1 = a + geom_histogram(aes(x = cr_director_facebook_likes), binwidth = .4, color = "dodgerblue2", 
                                 fill = "dodgerblue2", alpha = 0.6) + 
  labs(title = "Director FB Likes Distribution", subtitle = "Movies after 1995 - Cubic root Transform", x = "Director FB Likes", 
       y = "Frequency", caption = "Source: data_final.csv")

# Boxplot
dir_fb_box1 = a + geom_boxplot(aes(x = "", y = cr_director_facebook_likes), fill = "dodgerblue2", alpha = 0.6, 
                              outlier.color = "firebrick3", outlier.alpha = 0.4, outlier.size = 2) + 
  labs(title = "Director FB Likes", subtitle = "Movies after 1995 - Cubic root Transform", x = NULL, 
       y = "Director FB Likes", caption = "Source: data_final.csv")

grid.arrange(dir_fb_hist1, dir_fb_box1, ncol = 2)

# Scatterplot of director_facebook_likes and imdb_score after cubic root transformation
a + geom_point(aes(x = cr_director_facebook_likes, y = imdb_score), 
               color = "dodgerblue2", shape = 16, alpha = 0.6) + 
  labs(title = "Score & Director FB Likes Relationship", subtitle = "Movies after 1995 - Cubic root Transform",
       x = "Director FB Likes", y = "IMDB Score (stars)", caption = "Source: data_final.csv")

#==============================================================================================
# INTRODUCING CATEGORICAL VARIABLES INTO THE MODEL
#==============================================================================================
attach(movies5)
#Investigate genres
#============================================================================================
movies5 %>% group_by(action) %>% summarize(mean(imdb_score))
movies5 %>% group_by(adventure) %>% summarize(mean(imdb_score))
movies5 %>% group_by(scifi) %>% summarize(mean(imdb_score))
movies5 %>% group_by(thriller) %>% summarize(mean(imdb_score))
movies5 %>% group_by(musical) %>% summarize(mean(imdb_score))
movies5 %>% group_by(romance) %>% summarize(mean(imdb_score))
movies5 %>% group_by(western) %>% summarize(mean(imdb_score))
movies5 %>% group_by(sport) %>% summarize(mean(imdb_score))
movies5 %>% group_by(horror) %>% summarize(mean(imdb_score))
movies5 %>% group_by(drama) %>% summarize(mean(imdb_score))
movies5 %>% group_by(war) %>% summarize(mean(imdb_score))
movies5 %>% group_by(animation) %>% summarize(mean(imdb_score))
movies5 %>% group_by(crime) %>% summarize(mean(imdb_score))


genre_unique = c(rep("Action", 2), rep("Adventure", 2), rep("SciFi", 2), rep("Thriller", 2), 
                 rep("Musical", 2), rep("Romance", 2), rep("Western", 2), rep("Sport", 2), 
                 rep("Horror", 2), rep("Drama", 2), rep("War", 2), rep("Animation", 2), 
                 rep("Crime", 2))
is_genre = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
avg_score = c(6.48, 6.23, 6.42, 6.40, 6.44, 6.33, 6.45, 6.34, 6.43, 6.30, 6.43, 6.38, 
              6.42, 6.45, 6.40, 6.73, 6.48, 5.86, 6.11, 6.70, 6.40, 6.93, 6.40, 6.77, 6.39, 6.52)
genre_df = data.frame(genre_unique, is_genre, avg_score)

ggplot(genre_df,aes(genre_unique, avg_score, fill = factor(is_genre))) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, width = .5) + 
  geom_text(aes(label = avg_score), position = position_dodge(0.5), vjust = 0, 
            hjust = .5, size = 3) +
  scale_fill_manual("", values=c("dodgerblue", "dodgerblue4")) +
  theme_minimal(base_size = 12) + 
  theme(axis.text = element_text(color = "grey40", face = "bold", 
                                 margin = margin(10, 0, 10, 0))) + 
  labs(title = "Average Score by Genre", x = "Genres", y = "IMDB Score (stars)")
 
#==============================================================================================
# Adding New variables to reflect other powerful predictors
#============================================================================================
# Adding biography - documentary
movies5 %>% group_by(biography_documentary) %>% summarize(mean(imdb_score))

#Investigate foreign vs. local movies by language and location
movies5 %>% group_by(foreign_loc) %>% summarize(mean(imdb_score))
movies5 %>% group_by(foreign_lan) %>% summarize(mean(imdb_score))

#Adding a "sequel effect" into predictive model
movies5 %>% group_by(sequel) %>% summarize(mean(imdb_score))

# Plot new predictors bar plot
new_predictors = c(rep("Genre Biography\nor Documentary", 2), rep("Foreign Location", 2), 
                   rep("Foreign Language", 2), rep("Sequel", 2))
is_predictor = c(0, 1, 0, 1, 0, 1, 0, 1)
avg_score2 = c(6.36, 7.12, 6.36, 6.67, 6.40, 7.17, 6.42, 6.22)
genre_df2 = data.frame(new_predictors, is_predictor, avg_score2)

ggplot(genre_df2, aes(new_predictors, avg_score2, fill = factor(is_predictor))) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, width = .5) +
  geom_text(aes(label = avg_score2), position = position_dodge(0.5), vjust = 0, 
            hjust = .5) +
    scale_fill_manual("", values = c("dodgerblue", "dodgerblue4")) +
  theme_minimal(base_size = 12) + 
  theme(axis.text = element_text(color = "grey40", face = "bold", 
                                 margin = margin(10, 0, 10, 0))) + 
  labs(title = "Average Score by New Predictor", x = "New Predictors", y = "IMDB Score (stars)")

#=============================================================================================
# REMOVING HETEROSKEDASTICITY - CHANGE IN COEFFICIENTS
#=============================================================================================
attach(movies8)
opt_lm = lm(imdb_score ~ duration_mins + cr_movie_facebook_likes + 
              sr_number_news_articles + cr_director_facebook_likes + 
              log_movie_meter_IMDB_pro)

x_1 = predict(opt_lm)
y_1 = residuals(opt_lm)

summary(opt_lm)
ncvTest(opt_lm)

coeftest(opt_lm, vcov = vcovHC(opt_lm, type = 'HC1'))

Coefficients = c("Intercept", "Duration", "CR Movie FB Likes", "SR News Articles", 
                 "CR Director FB Likes", "Log Movie Meter")
Estimate = c(5.1271593, 0.0169831, 0.0110091, 0.0034412, 0.0153991, -0.0918377)
Significance = c("***", "***", "***", "**", "***", "***")

t_test_coeff = data.frame(Coefficients, Estimate, Significance)

library(gridExtra)
library(grid)

ggplot(movies8, aes(x = x_1, y = y_1)) + 
  geom_point(color = "dodgerblue2", shape = 16, alpha = 0.6) +
  stat_smooth(method = "lm", col = "red") + 
  labs(title = "Plot & Coefficients for a Heteroskedastic Model", 
       subtitle = paste("Intercept: ",signif(opt_lm$coef[[1]],3),"***",
                     " Duration: ",signif(opt_lm$coef[[2]], 3),"***",
                     " Movie FB Likes: ",signif(opt_lm$coef[[3]], 3),"***",
                     " News Articles: ",signif(opt_lm$coef[[4]], 3),"**",
                     "\nDirector FB Likes: ",signif(opt_lm$coef[[5]], 3),"***",
                     " Movie Meter: ",signif(opt_lm$coef[[6]], 3),"***",
                     " Adj R2 = ",signif(summary(opt_lm)$adj.r.squared, 3), 
                     "\nncvTest p-value: 2.22e-16"), 
       x = "Predicted Values", y = "Residuals") + 
  theme_minimal(base_size = 12) + 
  theme(axis.text = element_text(color = "grey40", face = "bold", 
                                 margin = margin(10, 0, 10, 0)))

grid.table(t_test_coeff, rows = NULL,  
           theme = ttheme_default(core=list(fg_params=list(hjust=0, x=0.1)), 
                                  rowhead=list(fg_params=list(hjust=0, x=0))))





#================================================================================================
# THE IMDB PREDICTION CHALLENGE - STARGAZER TABLES CODE
#================================================================================================
movies6 = read.csv("D:/Multivariate_Final_Project/data_final_sun.csv")

library(car)
library(psych)
library(dplyr)
library(splines)
library(caTools)
library(leaps)
library(data.table)
require(lmtest)
require(plm)
library(stargazer)

detach(movies7)
attach(movies8)


#================================================================================================
# FINAL MODEL COMPARISON
#================================================================================================
model1_3 = lm(imdb_score ~ duration_mins * poly(sr_movie_facebook_likes,8) + 
                poly(cr_director_facebook_likes,6) + 
                poly(log_movie_meter_IMDB_pro,5) + 
                duration_mins*poly(sr_number_news_articles,2) + 
                drama + 
                animation + 
                horror + 
                duration_mins * biography_documentary + 
                duration_mins * foreign_lan+foreign_loc+sequel)
stargazer(model1_3, type="html", title="Final Model Comparison", 
          dep.var.caption  = "Regression Results", dep.var.labels=c("IMDB Score"), 
          single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/test_changing_names.htm')

model1_4 = lm(imdb_score ~ duration_mins * poly(sr_movie_facebook_likes, 8) + 
                bs(cr_director_facebook_likes,knots = c(1.817,3.320,4.657,6.702), degree=5) + 
                poly(log_movie_meter_IMDB_pro,5) + 
                bs(sr_number_news_articles,knots = c(5.745,11.106,17.088,25.723,39.393), degree=3) + 
                drama + 
                animation + 
                horror + 
                duration_mins * biography_documentary + 
                duration_mins * foreign_lan + 
                foreign_loc + 
                sequel)

model1_5 = lm(imdb_score ~ duration_mins + 
                bs(cr_movie_facebook_likes,knots = c(0,5.5342,22.2398), degree = 1) + 
                bs(cr_director_facebook_likes,knots=c(1.817,3.320,4.657,6.702),degree=5) + 
                poly(log_movie_meter_IMDB_pro,5) + 
                duration_mins * bs(sr_number_news_articles,knots=c(5.745,11.106,17.088,25.723,39.393),degree=3) + 
                drama + 
                animation + 
                horror + 
                duration_mins * biography_documentary + 
                duration_mins * foreign_lan + 
                foreign_loc + 
                sequel)

stargazer(model1_3, model1_4, model1_5, type="html", title="Final Model Comparison", 
          dep.var.caption  = "Regression Results", dep.var.labels=c("IMDB Score"), 
          single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/final_model_comparison2.htm')

#================================================================================================
# CHANGING COEFFICIENTS NAMES - SAMPLE CODE
#==============================================================================================
model1_5 = lm(imdb_score ~ duration_mins + 
                bs(cr_movie_facebook_likes,knots = c(0,5.5342,22.2398), degree = 1) + 
                bs(cr_director_facebook_likes,knots=c(1.817,3.320,4.657,6.702),degree=5) + 
                poly(log_movie_meter_IMDB_pro,5) + 
                duration_mins * bs(sr_number_news_articles,knots=c(5.745,11.106,17.088,25.723,39.393),degree=3) + 
                drama + 
                animation + 
                horror + 
                duration_mins * biography_documentary + 
                duration_mins * foreign_lan + 
                foreign_loc + 
                sequel)


names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins"] <- "Duration"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_movie_facebook_likes, knots = c(0, 5.5342, 22.2398), degree = 1)1"] <- "Spline 4Knots Movie FB Likes 1"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_movie_facebook_likes, knots = c(0, 5.5342, 22.2398), degree = 1)2"] <- "Spline 4Knots Movie FB Likes 2"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_movie_facebook_likes, knots = c(0, 5.5342, 22.2398), degree = 1)3"] <- "Spline 4Knots Movie FB Likes 3"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_movie_facebook_likes, knots = c(0, 5.5342, 22.2398), degree = 1)4"] <- "Spline 4Knots Movie FB Likes 4"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)1"] <- "PolySpline5 4Knots Director FB Likes 1"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)2"] <- "PolySpline5 4Knots Director FB Likes 2"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)3"] <- "PolySpline5 4Knots Director FB Likes 3"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)4"] <- "PolySpline5 4Knots Director FB Likes 4"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)5"] <- "PolySpline5 4Knots Director FB Likes 5"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)6"] <- "PolySpline5 4Knots Director FB Likes 6"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)7"] <- "PolySpline5 4Knots Director FB Likes 7"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)8"] <- "PolySpline5 4Knots Director FB Likes 8"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)9"] <- "PolySpline5 4Knots Director FB Likes 9"
names(model1_5$coefficients)[names(model1_5$coefficients) == "poly(log_movie_meter_IMDB_pro, 5)1"] <- "Poly5 Movie Meter 1"
names(model1_5$coefficients)[names(model1_5$coefficients) == "poly(log_movie_meter_IMDB_pro, 5)2"] <- "Poly5 Movie Meter 2"
names(model1_5$coefficients)[names(model1_5$coefficients) == "poly(log_movie_meter_IMDB_pro, 5)3"] <- "Poly5 Movie Meter 3"
names(model1_5$coefficients)[names(model1_5$coefficients) == "poly(log_movie_meter_IMDB_pro, 5)4"] <- "Poly5 Movie Meter 4"
names(model1_5$coefficients)[names(model1_5$coefficients) == "poly(log_movie_meter_IMDB_pro, 5)5"] <- "Poly5 Movie Meter 5"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)1"] <- "PolySpline3 5Knots News Articles 1"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)2"] <- "PolySpline3 5Knots News Articles 2"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)3"] <- "PolySpline3 5Knots News Articles 3"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)4"] <- "PolySpline3 5Knots News Articles 4"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)5"] <- "PolySpline3 5Knots News Articles 5"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)6"] <- "PolySpline3 5Knots News Articles 6"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)7"] <- "PolySpline3 5Knots News Articles 7"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)8"] <- "PolySpline3 5Knots News Articles 8"
names(model1_5$coefficients)[names(model1_5$coefficients) == "drama"] <- "Drama"
names(model1_5$coefficients)[names(model1_5$coefficients) == "animation"] <- "Animation"
names(model1_5$coefficients)[names(model1_5$coefficients) == "horror"] <- "Horror"
names(model1_5$coefficients)[names(model1_5$coefficients) == "biography_documentary"] <- "Biography/Documentary"
names(model1_5$coefficients)[names(model1_5$coefficients) == "foreign_lan"] <- "Foreign Language"
names(model1_5$coefficients)[names(model1_5$coefficients) == "foreign_loc"] <- "Foreign Location"
names(model1_5$coefficients)[names(model1_5$coefficients) == "sequel"] <- "Sequel"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)1"] <- "Duration: PolySpline3 5Knots News Articles 1"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)2"] <- "Duration: PolySpline3 5Knots News Articles 2"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)3"] <- "Duration: PolySpline3 5Knots News Articles 3"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)4"] <- "Duration: PolySpline3 5Knots News Articles 4"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)5"] <- "Duration: PolySpline3 5Knots News Articles 5"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)6"] <- "Duration: PolySpline3 5Knots News Articles 6"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)7"] <- "Duration: PolySpline3 5Knots News Articles 7"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)8"] <- "Duration: PolySpline3 5Knots News Articles 8"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:biography_documentary"] <- "Duration: Biography/Documentary"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:foreign_lan"] <- "Duration: Foreign Language"
names(model1_5$coefficients)[names(model1_5$coefficients) == "Constant"] <- "Constant"

stargazer(model1_5, type="html", title="Final Model Summary", 
          dep.var.caption  = "Regression Results", dep.var.labels=c("IMDB Score"), 
          single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/Final_html_format.htm')

stargazer(model1_5, type="text", title="Final Model Summary", 
          dep.var.caption  = "Regression Results", dep.var.labels=c("IMDB Score"), 
          single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/Final_text_format.txt')

# PRINTING ONLY THE DESCRIPTION 
stargazer(model1_5, type="html", keep = c("none"), title="Final Model Description", 
          single.row=TRUE, dep.var.labels=c("IMDB Score"), omit.table.layout = "n",
          out = 'D:/Multivariate_Final_Project/Stargazer/test_1.htm')


#================================================================================================
# INTRODUCING NEW CATEGORICAL VARIABLES INTO THE MODEL
#================================================================================================
# Analyzing Genres
#================================================================================================
genre_score = lm(imdb_score~drama+animation+war+horror+biography_documentary)
summary(genre_score)
outlierTest(genre_score)
residualPlots(genre_score)
#All predictors are individually linear and significant, though the overall model 
# may have a slight non-linearity issue War is not as significant as the others, 
# so we can remove it from the genres in our overall model
stargazer(genre_score, type="html", title="Genre Analysis", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/Genre_Analysis.htm')

#Remove outliers for the genre predictor variables
data_final=data_final[-2367,]
attach(data_final)

#================================================================================================
# Investigate foreign vs. local movies by language and location
#================================================================================================
attach(movies8)
# Foreign Location
foreign_loc = lm(imdb_score~foreign_loc)
summary(foreign_loc)
outlierTest(foreign_loc)
residualPlots(foreign_loc)
#Both predictors are significant,linear and add predictive power to the model. 
# The overall model is linear
stargazer(foreign_loc, type="html", title="Foreign Location Summary", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/Foreign_loc.htm')

# Foreign Language
foreign_lan = lm(imdb_score~foreign_lan)
summary(foreign_lan)
outlierTest(foreign_lan)
residualPlots(foreign_lan)
#Both predictors are significant,linear and add predictive power to the model. 
# The overall model is linear
stargazer(foreign_lan, type="html", title="Foreign Language Summary", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/Foreign_lan.htm')

#=================================================================================================
#Adding a "sequel effect" into predictive model
#=================================================================================================
seq = lm(imdb_score~sequel)
summary(seq)
outlierTest(seq)
residualPlots(seq)
#Not a significant predictor with low predictive power, but can be included in the model 
# to adjust for sequel movies
stargazer(seq, type="html", title="Sequel Summary", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/sequel.htm')

#=================================================================================================
#Adding a Biography/Documentary genre into predictive model
#===============================================================================================
bio_doc = lm(imdb_score~biography_documentary)
summary(bio_doc)
outlierTest(bio_doc)
residualPlots(bio_doc)
#Not a significant predictor with low predictive power, but can be included in the model 
# to adjust for sequel movies
stargazer(seq, type="html", title="Biography/Documentary Summary", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/bio_doc.htm')

#================================================================================================
# PREDICTION OF NEW MOVIES
#===============================================================================================
attach(movies8)
predicted = read.csv("D:/Multivariate_Final_Project/new_data_final.csv") # prediction dataset 

model1_5 = lm(imdb_score ~ duration_mins + 
                bs(cr_movie_facebook_likes,knots = c(0,5.5342,22.2398), degree = 1) + 
                bs(cr_director_facebook_likes,knots=c(1.817,3.320,4.657,6.702),degree=5) + 
                poly(log_movie_meter_IMDB_pro,5) + 
                duration_mins * bs(sr_number_news_articles,knots=c(5.745,11.106,17.088,25.723,39.393),degree=3) + 
                drama + 
                animation + 
                horror + 
                duration_mins * biography_documentary + 
                duration_mins * foreign_lan + 
                foreign_loc + 
                sequel)


predict(model1_5, predicted)

# Build a dataframe with prediction results for the 12 movies

Title = predicted[, "title"]
IMDB_Score = c(7.392776, 7.073886, 6.029955, 6.689221, 6.908786, 6.814569, 6.680304, 7.061288,
               8.268373, 8.281830, 7.315007, 7.694907 )
Language = predicted[, "language"]
Country = predicted[, "country"]

final_predictions = data.frame(Title, IMDB_Score)


# Take final_predictions dataframe to a table
library(gridExtra)
library(grid)
grid.table(final_predictions, rows = NULL,  
           theme = ttheme_default(core=list(fg_params=list(hjust=0, x=0.1)), 
                                  rowhead=list(fg_params=list(hjust=0, x=0))))

# Build horizontal tables
# Table 1


Title = predicted[, "title"]
IMDB_Score = c(7.392776, 7.073886, 6.029955, 6.689221, 6.908786, 6.814569, 6.680304, 7.061288,
               8.268373, 8.281830, 7.315007, 7.694907 )
Language = predicted[, "language"]
Country = predicted[, "country"]

final_predictions = data.frame(Title, IMDB_Score)

# Take final_predictions dataframe to a table
library(gridExtra)
library(grid)
grid.table(final_predictions, rows = NULL,  
           theme = ttheme_default(core=list(fg_params=list(hjust=0, x=0.1)), 
                                  rowhead=list(fg_params=list(hjust=0, x=0))))

# Building the plot for final prediction distribution
library(ggplot2)
ggplot(data = final_predictions) + theme_minimal(base_size = 12) + 
  theme(axis.text = element_text(color = "grey40", face = "bold", 
                                 margin = margin(10, 0, 10, 0))) + 
  geom_histogram(mapping = aes(x = IMDB_Score), binwidth = .6, 
                 color = "dodgerblue2", fill = "dodgerblue2", alpha = 0.6) + 
  labs(title = "Final Predictions Distribution", x = "IMDB Score (stars)", 
       y = "Frequency") +
  coord_cartesian(xlim = c(3, 9), ylim = c(0, 6))
#================================================================================================
# THE IMDB PREDICTION CHALLENGE - STARGAZER TABLES CODE
#================================================================================================
movies6 = read.csv("D:/Multivariate_Final_Project/data_final_sun.csv")

library(car)
library(psych)
library(dplyr)
library(splines)
library(caTools)
library(leaps)
library(data.table)
require(lmtest)
require(plm)
library(stargazer)

detach(movies7)
attach(movies8)


#================================================================================================
# FINAL MODEL COMPARISON
#================================================================================================
model1_3 = lm(imdb_score ~ duration_mins * poly(sr_movie_facebook_likes,8) + 
                poly(cr_director_facebook_likes,6) + 
                poly(log_movie_meter_IMDB_pro,5) + 
                duration_mins*poly(sr_number_news_articles,2) + 
                drama + 
                animation + 
                horror + 
                duration_mins * biography_documentary + 
                duration_mins * foreign_lan+foreign_loc+sequel)
stargazer(model1_3, type="html", title="Final Model Comparison", 
          dep.var.caption  = "Regression Results", dep.var.labels=c("IMDB Score"), 
          single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/test_changing_names.htm')

model1_4 = lm(imdb_score ~ duration_mins * poly(sr_movie_facebook_likes, 8) + 
                bs(cr_director_facebook_likes,knots = c(1.817,3.320,4.657,6.702), degree=5) + 
                poly(log_movie_meter_IMDB_pro,5) + 
                bs(sr_number_news_articles,knots = c(5.745,11.106,17.088,25.723,39.393), degree=3) + 
                drama + 
                animation + 
                horror + 
                duration_mins * biography_documentary + 
                duration_mins * foreign_lan + 
                foreign_loc + 
                sequel)

model1_5 = lm(imdb_score ~ duration_mins + 
                bs(cr_movie_facebook_likes,knots = c(0,5.5342,22.2398), degree = 1) + 
                bs(cr_director_facebook_likes,knots=c(1.817,3.320,4.657,6.702),degree=5) + 
                poly(log_movie_meter_IMDB_pro,5) + 
                duration_mins * bs(sr_number_news_articles,knots=c(5.745,11.106,17.088,25.723,39.393),degree=3) + 
                drama + 
                animation + 
                horror + 
                duration_mins * biography_documentary + 
                duration_mins * foreign_lan + 
                foreign_loc + 
                sequel)

stargazer(model1_3, model1_4, model1_5, type="html", title="Final Model Comparison", 
          dep.var.caption  = "Regression Results", dep.var.labels=c("IMDB Score"), 
          single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/final_model_comparison2.htm')

#================================================================================================
# CHANGING COEFFICIENTS NAMES - SAMPLE CODE
#==============================================================================================
model1_5 = lm(imdb_score ~ duration_mins + 
                bs(cr_movie_facebook_likes,knots = c(0,5.5342,22.2398), degree = 1) + 
                bs(cr_director_facebook_likes,knots=c(1.817,3.320,4.657,6.702),degree=5) + 
                poly(log_movie_meter_IMDB_pro,5) + 
                duration_mins * bs(sr_number_news_articles,knots=c(5.745,11.106,17.088,25.723,39.393),degree=3) + 
                drama + 
                animation + 
                horror + 
                duration_mins * biography_documentary + 
                duration_mins * foreign_lan + 
                foreign_loc + 
                sequel)


names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins"] <- "Duration"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_movie_facebook_likes, knots = c(0, 5.5342, 22.2398), degree = 1)1"] <- "Spline 4Knots Movie FB Likes 1"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_movie_facebook_likes, knots = c(0, 5.5342, 22.2398), degree = 1)2"] <- "Spline 4Knots Movie FB Likes 2"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_movie_facebook_likes, knots = c(0, 5.5342, 22.2398), degree = 1)3"] <- "Spline 4Knots Movie FB Likes 3"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_movie_facebook_likes, knots = c(0, 5.5342, 22.2398), degree = 1)4"] <- "Spline 4Knots Movie FB Likes 4"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)1"] <- "PolySpline5 4Knots Director FB Likes 1"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)2"] <- "PolySpline5 4Knots Director FB Likes 2"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)3"] <- "PolySpline5 4Knots Director FB Likes 3"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)4"] <- "PolySpline5 4Knots Director FB Likes 4"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)5"] <- "PolySpline5 4Knots Director FB Likes 5"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)6"] <- "PolySpline5 4Knots Director FB Likes 6"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)7"] <- "PolySpline5 4Knots Director FB Likes 7"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)8"] <- "PolySpline5 4Knots Director FB Likes 8"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(cr_director_facebook_likes, knots = c(1.817, 3.32, 4.657, 6.702), degree = 5)9"] <- "PolySpline5 4Knots Director FB Likes 9"
names(model1_5$coefficients)[names(model1_5$coefficients) == "poly(log_movie_meter_IMDB_pro, 5)1"] <- "Poly5 Movie Meter 1"
names(model1_5$coefficients)[names(model1_5$coefficients) == "poly(log_movie_meter_IMDB_pro, 5)2"] <- "Poly5 Movie Meter 2"
names(model1_5$coefficients)[names(model1_5$coefficients) == "poly(log_movie_meter_IMDB_pro, 5)3"] <- "Poly5 Movie Meter 3"
names(model1_5$coefficients)[names(model1_5$coefficients) == "poly(log_movie_meter_IMDB_pro, 5)4"] <- "Poly5 Movie Meter 4"
names(model1_5$coefficients)[names(model1_5$coefficients) == "poly(log_movie_meter_IMDB_pro, 5)5"] <- "Poly5 Movie Meter 5"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)1"] <- "PolySpline3 5Knots News Articles 1"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)2"] <- "PolySpline3 5Knots News Articles 2"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)3"] <- "PolySpline3 5Knots News Articles 3"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)4"] <- "PolySpline3 5Knots News Articles 4"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)5"] <- "PolySpline3 5Knots News Articles 5"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)6"] <- "PolySpline3 5Knots News Articles 6"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)7"] <- "PolySpline3 5Knots News Articles 7"
names(model1_5$coefficients)[names(model1_5$coefficients) == "bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)8"] <- "PolySpline3 5Knots News Articles 8"
names(model1_5$coefficients)[names(model1_5$coefficients) == "drama"] <- "Drama"
names(model1_5$coefficients)[names(model1_5$coefficients) == "animation"] <- "Animation"
names(model1_5$coefficients)[names(model1_5$coefficients) == "horror"] <- "Horror"
names(model1_5$coefficients)[names(model1_5$coefficients) == "biography_documentary"] <- "Biography/Documentary"
names(model1_5$coefficients)[names(model1_5$coefficients) == "foreign_lan"] <- "Foreign Language"
names(model1_5$coefficients)[names(model1_5$coefficients) == "foreign_loc"] <- "Foreign Location"
names(model1_5$coefficients)[names(model1_5$coefficients) == "sequel"] <- "Sequel"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)1"] <- "Duration: PolySpline3 5Knots News Articles 1"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)2"] <- "Duration: PolySpline3 5Knots News Articles 2"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)3"] <- "Duration: PolySpline3 5Knots News Articles 3"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)4"] <- "Duration: PolySpline3 5Knots News Articles 4"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)5"] <- "Duration: PolySpline3 5Knots News Articles 5"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)6"] <- "Duration: PolySpline3 5Knots News Articles 6"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)7"] <- "Duration: PolySpline3 5Knots News Articles 7"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:bs(sr_number_news_articles, knots = c(5.745, 11.106, 17.088, 25.723, 39.393), degree = 3)8"] <- "Duration: PolySpline3 5Knots News Articles 8"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:biography_documentary"] <- "Duration: Biography/Documentary"
names(model1_5$coefficients)[names(model1_5$coefficients) == "duration_mins:foreign_lan"] <- "Duration: Foreign Language"
names(model1_5$coefficients)[names(model1_5$coefficients) == "Constant"] <- "Constant"

stargazer(model1_5, type="html", title="Final Model Summary", 
          dep.var.caption  = "Regression Results", dep.var.labels=c("IMDB Score"), 
          single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/Final_html_format.htm')

stargazer(model1_5, type="text", title="Final Model Summary", 
          dep.var.caption  = "Regression Results", dep.var.labels=c("IMDB Score"), 
          single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/Final_text_format.txt')

# PRINTING ONLY THE DESCRIPTION 
stargazer(model1_5, type="html", keep = c("none"), title="Final Model Description", 
          single.row=TRUE, dep.var.labels=c("IMDB Score"), omit.table.layout = "n",
          out = 'D:/Multivariate_Final_Project/Stargazer/test_1.htm')


#================================================================================================
# INTRODUCING NEW CATEGORICAL VARIABLES INTO THE MODEL
#================================================================================================
# Analyzing Genres
#================================================================================================
genre_score = lm(imdb_score~drama+animation+war+horror+biography_documentary)
summary(genre_score)
outlierTest(genre_score)
residualPlots(genre_score)
#All predictors are individually linear and significant, though the overall model 
# may have a slight non-linearity issue War is not as significant as the others, 
# so we can remove it from the genres in our overall model
stargazer(genre_score, type="html", title="Genre Analysis", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/Genre_Analysis.htm')

#Remove outliers for the genre predictor variables
data_final=data_final[-2367,]
attach(data_final)

#================================================================================================
# Investigate foreign vs. local movies by language and location
#================================================================================================
attach(movies8)
# Foreign Location
foreign_loc = lm(imdb_score~foreign_loc)
summary(foreign_loc)
outlierTest(foreign_loc)
residualPlots(foreign_loc)
#Both predictors are significant,linear and add predictive power to the model. 
# The overall model is linear
stargazer(foreign_loc, type="html", title="Foreign Location Summary", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/Foreign_loc.htm')

# Foreign Language
foreign_lan = lm(imdb_score~foreign_lan)
summary(foreign_lan)
outlierTest(foreign_lan)
residualPlots(foreign_lan)
#Both predictors are significant,linear and add predictive power to the model. 
# The overall model is linear
stargazer(foreign_lan, type="html", title="Foreign Language Summary", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/Foreign_lan.htm')

#=================================================================================================
#Adding a "sequel effect" into predictive model
#=================================================================================================
seq = lm(imdb_score~sequel)
summary(seq)
outlierTest(seq)
residualPlots(seq)
#Not a significant predictor with low predictive power, but can be included in the model 
# to adjust for sequel movies
stargazer(seq, type="html", title="Sequel Summary", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/sequel.htm')

#=================================================================================================
#Adding a Biography/Documentary genre into predictive model
#=================================================================================================
bio_doc = lm(imdb_score~biography_documentary)
summary(bio_doc)
outlierTest(bio_doc)
residualPlots(bio_doc)
#Not a significant predictor with low predictive power, but can be included in the model 
# to adjust for sequel movies
stargazer(seq, type="html", title="Biography/Documentary Summary", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/bio_doc.htm')

#=================================================================================================
# Asses the significance of the 4 new variables together
#=================================================================================================
attach(movies8)
new_vars_1 = lm(movies8$imdb_score ~ movies8$biography_documentary + 
                  movies8$foreign_lan + 
                  movies8$foreign_loc + 
                  movies8$sequel)

stargazer(new_vars_1, type="html", report=('vc*p'), title="New Variables Summary", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/new_vars_1.htm')
attach(movies8)
new_vars_1 = lm(movies8$imdb_score ~ movies8$biography_documentary + 
                  movies8$foreign_lan + 
                  movies8$foreign_loc + 
                  movies8$sequel)

stargazer(new_vars_1, type="html", report=('vc*p'), title="New Variables Summary", 
          dep.var.caption  = "Regression Results",
          dep.var.labels=c("IMDB Score"), omit.stat=c("LL","ser","f"), single.row=TRUE, 
          out = 'D:/Multivariate_Final_Project/Stargazer/new_vars_1.htm')
