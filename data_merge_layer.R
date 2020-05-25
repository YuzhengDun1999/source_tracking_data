home = '/data2/public/dunyuzheng'
setwd(home)
feature = c('filter_abundance','../full_feature','../random_forest','../mrmr_feature')
splitn = c('split0','../split1','../split2','../split3','../split4','../split5','../split6','../split7')
layer = c('first_layer.csv','second_layer.csv','third_layer.csv','fourth_layer.csv','fifth_layer.csv','sixth_layer.csv')

data_mer = function(input){
  Pr_all = list()
  Rc_all = list()
  AvgPr_all = list()
  AvgRc_all = list()
  em_all = list()
  f1_all = list()
  s1_all = list()
  FPR_all = list()
  TPR_all = list()
  a = c('split0','../split1','../split2','../split3','../split4','../split5','../split6','../split7')
  raw_data = list()
  for (j in 1:length(a)) {
    setwd(a[j])
    result = read.csv(input)#read the name(eg. first_layer.csv)
    Pr = list(result$Pr)
    Rc = list(result$Rc)
    AvgPr = list(result$AvgPc)
    AvgRc = list(result$AvgRc)
    em = list(result$em)
    f1 = list(result$f1)
    s1 = list(result$s1)
    TPR = list(result$TPR)
    FPR = list(result$FPR)
    Pr_all = append(Pr_all, Pr)
    Rc_all = append(Rc_all, Rc)
    AvgPr_all = append(AvgPr_all, AvgPr)
    AvgRc_all = append(AvgRc_all, AvgRc)
    em_all = append(em_all, em)
    f1_all = append(f1_all, f1)
    s1_all = append(s1_all, s1)
    TPR_all = append(TPR_all, TPR)
    FPR_all = append(FPR_all, FPR)
  }
  number = length(f1_all)
  length_all = length(f1_all[[1]])
  Pr = data.frame(Pr_all[[1]])
  Rc = data.frame(Rc_all[[1]])
  AvgPr = data.frame(AvgPr_all[[1]])
  AvgRc = data.frame(AvgRc_all[[1]])
  em = data.frame(em_all[[1]])
  f1 = data.frame(f1_all[[1]])
  s1 = data.frame(s1_all[[1]])
  FPR = data.frame(FPR_all[[1]])
  TPR = data.frame(TPR_all[[1]])

  for (i in 2:number) {
    Pr[i] = Pr_all[[i]]
    Rc[i] = Rc_all[[i]]
    AvgPr[i] = AvgPr_all[[i]]
    AvgRc[i] = AvgRc_all[[i]]
    em[i] = em_all[[i]]
    f1[i] = f1_all[[i]]
    s1[i] = s1_all[[i]]
    FPR[i] = FPR_all[[i]]
    TPR[i] = TPR_all[[i]]
  }

  output = list(Pr = rowMeans(Pr), Rc = rowMeans(Rc), AvgPr = rowMeans(AvgPr), AvgRc = rowMeans(AvgRc),em =  rowMeans(em),f1 = rowMeans(f1),s1 = rowMeans(s1), TPR = rowMeans(TPR), FPR = rowMeans(FPR))
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