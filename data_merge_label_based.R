home = '/home/qiuhao/dunyuzheng/newdata'
setwd(home)
feature = c('wholeset')
splitn = c('split0','../split1','../split2','../split3','../split4','../split5','../split6','../split7')
layer = c('second.csv','third.csv','fourth.csv','fifth.csv','sixth.csv')

data_mer = function(input){
  TP_all = list()
  FN_all = list()
  FP_all = list()
  TN_all = list()
  FPR_all = list()
  TPR_all = list()
  a = c('split0','../split1','../split2','../split3','../split4','../split5','../split6','../split7')
  raw_data = list()
  for (j in 1:length(a)) {
    setwd(a[j])
    result = read.csv(input)#read the name(eg. first_layer.csv)
    TP = list(result$TP)
    FN = list(result$FN)
    FP = list(result$FP)
    TN = list(result$TN)
    TPR = list(result$TPR)
    FPR = list(result$FPR)
    TP_all = append(TP_all, TP)
    FN_all = append(FN_all, FN)
    FP_all = append(FP_all, FP)
    TN_all = append(TN_all, TN)
    TPR_all = append(TPR_all, TPR)
    FPR_all = append(FPR_all, FPR)
  }
  number = length(TP_all)
  length_all = length(TP_all[[1]])
  TP = data.frame(TP_all[[1]])
  TN = data.frame(TN[[1]])
  FP = data.frame(FP_all[[1]])
  FN = data.frame(FN_all[[1]])
  FPR = data.frame(FPR_all[[1]])
  TPR = data.frame(TPR_all[[1]])

  for (i in 2:number) {
    TP[i] = TP_all[[i]]
    TN[i] = TN_all[[i]]
    FP[i] = FP_all[[i]]
    FN[i] = FN_all[[i]]
    FPR[i] = FPR_all[[i]]
    TPR[i] = TPR_all[[i]]
  }

  output = list(TP = rowMeans(TP), FN = rowMeans(FN), FP = rowMeans(FP), TN = rowMeans(TN), TPR = rowMeans(TPR), FPR = rowMeans(FPR))
  return(output)
}

for (k in 1:length(feature)) {
  setwd(feature[k])#full_feature
  for (l in 1:length(layer)) {
    output = data_mer(layer[l])
    setwd(home)
    setwd(feature[k])#full_feature
    write.csv(output, file = layer[l])
  }
}