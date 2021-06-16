library("factoextra")
DrafteeData <- read.csv("~/Documents/term 3 coursework/Project Files/NCAA NBA 2006-2016 Draftee Data.csv")
#r inbuilt function to do PCA
PCADraftee <- prcomp(DrafteeData[,c(2:26)], center = TRUE, scale. = TRUE)
#generating individual PCA data to extract into a new analysible data set
indPCA <- get_pca_ind(PCADraftee)
PostPCADraftee <- indPCA$coord