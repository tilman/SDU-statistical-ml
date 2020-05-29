rm(list = ls()) # clear/reset current environment
dev.off() # clear plots

# since finding the hyperparameters is an tedious task and we wanted to try out a lot of variations to find the fastest and best for the randomforest and the pca,
# we created an hyper parameter grid and tried out 12480 combinations of the following hyper params:
#For PCA:
# rank.
#For RandomForest
# mtry
# nodesize
# sampsize
# ntree
# GOALS:
# * fast test, train time does not matter that much
# * high acc
# HW: google colab CPU instance with 1 core Intel Xeon @2.30GHz and hyperthreading
# 4 colabs in parallel, one started at 10 pca comps, the other started at 120 PCA Components.
# + 4 kaggle in prallel

# first iteration on the hypergrid canceled after:

# - 10PCA: 651 runs, total of 9h 13m, avg runtime of 51s, highest ACC of 78.72% with params: 
#pca_count_i   i      acc pca_count ntree mtry nodesize sampsize time_pcaTrainDuration time_rfTrainDuration time_pcaTestDuration time_rfTestDuration
#          1 561 78.72059        20   500    2       60    48000              3.627728             79.78195            0.3292964            2.579428

# - 120PCA: 90 runs, total of 7h 43m, avg runtime of 308s (5m 8s), highest ACC of 79.12647 with params:
#pca_count_i  i      acc pca_count ntree mtry nodesize sampsize time_pcaTrainDuration time_rfTrainDuration time_pcaTestDuration time_rfTestDuration
#          8 81 79.12647       120   300   12       60    36000              5.238905             308.3567             0.623035            4.422488


# total avg: 82s
# => hypergrid with 12480 combinations to big. Would need (9h13m+7h43m)/(651+90) * 12480 ~= 284h
# need intermidiate evaluation of hypegrid and throw out bad results
# => now make values more aggressiv and see if we will overfit
# => also noticed, pca outside of hypergrid is not good for evaluation
# => reduced to 900 params ~= 15h runtime => use 2 colabs again

#load('/Volumes/GoogleDrive/Meine\ Ablage/machine_learning/R/sdu_stats_ml/hypergrid_results_gpuTrial_28012020-1.RData')
#load('/Volumes/GoogleDrive/Meine\ Ablage/machine_learning/R/sdu_stats_ml/hypergrid_results_copy2.RData')
#load('/Volumes/GoogleDrive/Meine\ Ablage/machine_learning/R/sdu_stats_ml/hypergrid_results.RData')

load('/Volumes/GoogleDrive/Meine\ Ablage/machine_learning/R/sdu_stats_ml/hypergrid_results_450-900_28052020.RData')
load('/Volumes/GoogleDrive/Meine\ Ablage/machine_learning/R/sdu_stats_ml/hypergrid_results_0-450_28052020.RData') #done bis 350
load('/Volumes/GoogleDrive/Meine\ Ablage/machine_learning/R/sdu_stats_ml/hypergrid_results_650-700_28052020.RData')
load('/Volumes/GoogleDrive/Meine\ Ablage/machine_learning/R/sdu_stats_ml/hypergrid_results_350-450_28052020.RData')
load("/Users/Tilman/Downloads/hypergrid_results_700-750_28052020.RData")
load("/Users/Tilman/Downloads/hypergrid_results_750-800_28052020.RData")
load("/Users/Tilman/Downloads/hypergrid_results_800-850_28052020.RData")
load("/Users/Tilman/Downloads/hypergrid_results_850-900_28052020.RData")
#load("/Users/Tilman/Downloads/hypergrid_results_850-900_28052020 (1).RData")
#load("/Users/Tilman/Downloads/hypergrid_results_800-850_28052020 (1).RData")
#load("/Users/Tilman/Downloads/hypergrid_results_750-800_28052020 (1).RData")
#load("/Users/Tilman/Downloads/hypergrid_results_700-750_28052020 (1).RData")

#resId <- do.call(rbind, res[[8]])
#resId <- do.call(rbind, c(res1[1:450],res2[451:749],res3[750:900]))
resId <- do.call(rbind, c(res1[1:349],res8[350:449],res2[450:650],res3[650:699],res4[700:749],res5[750:799],res6[800:849],res7[850:899]))
#resId <- do.call(rbind, c(res1,res2,res3,res4,res5,res6,res7,res8))
resId <- as.data.frame(resId)
nrow(resId)
resId[nrow(resId),]$i

#discrete values:
resId$pca = as.factor(do.call(rbind, resId$pca))
resId$nodesize = as.factor(do.call(rbind, resId$nodesize))
resId$mtry = as.factor(do.call(rbind, resId$mtry))
resId$sampsize = as.factor(do.call(rbind, resId$sampsize))
resId$ntree = as.factor(do.call(rbind, resId$ntree))

#continous values:
#resId$pca_count = do.call(rbind, resId$pca_count) #could be discrete as well
resId$acc = do.call(rbind, resId$acc)
resId$time_pcaTrainDuration = do.call(rbind, resId$time_pcaTrainDuration)
resId$time_rfTrainDuration = do.call(rbind, resId$time_rfTrainDuration)
resId$time_pcaTestDuration = do.call(rbind, resId$time_pcaTestDuration)
resId$time_rfTestDuration = do.call(rbind, resId$time_rfTestDuration)




cat("Total Runtime:",sum(resId$time_pcaTrainDuration) + sum(resId$time_rfTrainDuration)) #47h 53m
cat("Max accuracy:",max(resId$acc)) #83.71471
cat("Best combination:")
print(resId[resId$acc == max(resId$acc),c(2:7,9)])
#theme = theme_bw()
theme = NULL


FILEPATH = "/Users/Tilman/Documents/Programme/R/SDU-statistical-ml/finalProject/RandomForest/hypergrid/plots/"
FILENAME = "run3_28052020_"
FORMAT = "pdf"
SAVE = TRUE
setwd(FILEPATH)


ggplot(resId)+geom_point(aes(x=pca,y=acc))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"pca_acc",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(pca,acc,fill=pca)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"pca_acc_barplot",".",FORMAT,sep = ""),device = FORMAT)

ggplot(resId)+geom_point(aes(x=time_rfTrainDuration,y=acc,color=pca))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"pca_trainDuration",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(pca,time_rfTrainDuration,fill=pca)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"pca_trainDuration_barplot",".",FORMAT,sep = ""),device = FORMAT)

ggplot(resId)+geom_point(aes(x=time_rfTestDuration,y=acc,color=pca))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"pca_testDuration",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(pca,time_rfTestDuration,fill=pca)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"pca_testDuration_barplot",".",FORMAT,sep = ""),device = FORMAT)



ggplot(resId)+geom_point(aes(x=nodesize,y=acc))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"nodesize_acc",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(nodesize,acc,fill=nodesize)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"nodesize_acc_barplot",".",FORMAT,sep = ""),device = FORMAT)

ggplot(resId)+geom_point(aes(x=time_rfTrainDuration,y=acc,color=nodesize))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"nodesize_trainDuration",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(nodesize,time_rfTrainDuration,fill=nodesize)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"nodesize_trainDuration_barplot",".",FORMAT,sep = ""),device = FORMAT)

ggplot(resId)+geom_point(aes(x=time_rfTestDuration,y=acc,color=nodesize))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"nodesize_testDuration",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(nodesize,time_rfTestDuration,fill=nodesize)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"nodesize_testDuration_barplot",".",FORMAT,sep = ""),device = FORMAT)



ggplot(resId)+geom_point(aes(x=mtry,y=acc,color=nodesize))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"mtry_acc",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(mtry,acc,fill=mtry)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"mtry_acc_barplot",".",FORMAT,sep = ""),device = FORMAT)

ggplot(resId)+geom_point(aes(x=time_rfTrainDuration,y=acc,color=mtry))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"mtry_trainDuration",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(mtry,time_rfTrainDuration,fill=mtry)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"mtry_trainDuration_barplot",".",FORMAT,sep = ""),device = FORMAT)

ggplot(resId)+geom_point(aes(x=time_rfTestDuration,y=acc,color=mtry))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"mtry_testDuration",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(mtry,time_rfTestDuration,fill=mtry)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"mtry_testDuration_barplot",".",FORMAT,sep = ""),device = FORMAT)



ggplot(resId)+geom_point(aes(x=sampsize,y=acc,color=nodesize))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"sampsize_acc",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(sampsize,acc,fill=sampsize)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"sampsize_acc_barplot",".",FORMAT,sep = ""),device = FORMAT)

ggplot(resId)+geom_point(aes(x=time_rfTrainDuration,y=acc,color=sampsize))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"sampsize_trainDuration",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(sampsize,time_rfTrainDuration,fill=sampsize)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"sampsize_testDuration_barplot",".",FORMAT,sep = ""),device = FORMAT)

ggplot(resId)+geom_point(aes(x=time_rfTestDuration,y=acc,color=sampsize))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"sampsize_testDuration",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(sampsize,time_rfTestDuration,fill=sampsize)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"sampsize_testDuration_barplot",".",FORMAT,sep = ""),device = FORMAT)



ggplot(resId)+geom_point(aes(x=ntree,y=acc,color=nodesize)) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"ntree_acc",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(ntree,acc,fill=ntree)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"ntree_acc_barplot",".",FORMAT,sep = ""),device = FORMAT)

ggplot(resId)+geom_point(aes(x=time_rfTrainDuration,y=acc,color=ntree))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"ntree_trainDuration",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(ntree,time_rfTrainDuration,fill=ntree)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"ntree_testDuration_barplot",".",FORMAT,sep = ""),device = FORMAT)

ggplot(resId)+geom_point(aes(x=time_rfTestDuration,y=acc,color=ntree))+theme
if(SAVE) ggsave(filename = paste(FILENAME,"ntree_testDuration",".",FORMAT,sep = ""),device = FORMAT)
ggplot(data = resId, aes(ntree,time_rfTestDuration,fill=ntree)) + geom_boxplot()+geom_jitter(width=0,alpha=0.15) + theme
if(SAVE) ggsave(filename = paste(FILENAME,"ntree_testDuration_barplot",".",FORMAT,sep = ""),device = FORMAT)

