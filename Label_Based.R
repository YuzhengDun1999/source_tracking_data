setwd('G:/source_tracking/evaluation/Label_Based')
data2 = file('data_layered.txt',"r")

alnres = function(filein){
  f = readLines(filein)
  close(filein)
  all_predicted = c()
  all_true = c()
  all_prob = c()
  all_unique = c()
  for (line in f) {
    line = trimws(line, which = c("both", "left", "right"))
    temp1 = unlist(strsplit(line, split = "\\s+"))
    temp2 = unlist(strsplit(temp1[1], split = "\\|"))
    # qid = c(qid, temp2[1])
    temp3 = unlist(strsplit(temp1[2], split = "\\|"))
    # tid = c(tid, temp3[1])
    # seqid = append(seqid, temp1[3])
    # qlabel = c(qlabel, temp2[2])
    # tlabel = c(tlabel, temp3[2])
    predicted = unlist(temp2[2])
    predicted = unlist(strsplit(as.character(predicted), split = ","))##splited predicted label
    tlabel = unlist(temp3[2])
    tlabel = unlist(strsplit(as.character(tlabel), split = ","))##splited true label
    true = rep(0,length(predicted))
    unique_label = predicted[!duplicated(predicted)]
    for (i in 1:length(tlabel)) {
      true[which(predicted == tlabel[i])] = 1
    }
    prob = as.numeric(unlist(strsplit(temp1[3], split = ",")))
    all_predicted = c(all_predicted, predicted)
    all_true = c(all_true, true)
    all_prob = c(all_prob, prob)
    all_unique = c(all_unique, unique_label)
  }
  result = list(true = all_true, predicted = all_predicted, prob = all_prob, unique = all_unique)
  return(result)
}

evaluate = function(cutoff, data_proc, all_label, num_label){
  TPR_all = c()
  FPR_all = c()
  TP_all = c()
  FN_all = c()
  FP_all = c()
  TN_all = c()
  all_true = data_proc$true
  all_predicted = data_proc$predicted
  all_prob = data_proc$prob
  
  for (i in 1:length(all_label)) {
    label = all_label[i]
    true = all_true[which(all_predicted == label)] ###whether label in true
    prob = all_prob[which(all_predicted == label)] ###label probability
    predicted = prob
    predicted[predicted >= cutoff] = 1
    predicted[predicted < cutoff] = 0
    
    TP = sum(predicted[which(true == 1)]) ##in T and P
    FN = sum(true) - TP ## true-TP
    FP = sum(predicted[which(true == 0)]) ##in P not in T
    TN = length(which(true == 0)) - FP ## false sample - FP
    
    if((TP+FN) == 0){
      TPR = 0
    }
    else{
      TPR = TP / (TP + FN)
    }
    
    if((FP + TN) == 0){
      FPR = 0
    }
    else{
      FPR = FP / (FP + TN)
    }
    
    TP_all = c(TP_all, TP)
    FN_all = c(FN_all, FN)
    FP_all = c(FP_all, FP)
    TN_all = c(TN_all, TN)
    TPR_all = c(TPR_all, TPR)
    FPR_all = c(FPR_all, FPR)
  }
  TP_all = TP_all %*% num_label / sum(num_label)
  FN_all = FN_all %*% num_label / sum(num_label)
  FP_all = FP_all %*% num_label / sum(num_label)
  TN_all = TN_all %*% num_label / sum(num_label)
  TPR_all = TPR_all %*% num_label / sum(num_label)
  FPR_all = FPR_all %*% num_label / sum(num_label)
  
  return(list(TP = TP_all, FN = FN_all, FP = FP_all, TN = TN_all, TPR = TPR_all, FPR = FPR_all))
}

data_proc = alnres(data2)
all_label = data_proc$unique[!duplicated(data_proc$unique)]##labels needed to be evluated
num_label = c()
layer_label = c()
output_name = c("first.csv","second.csv","third.csv","fourth.csv","fifth.csv","sixth.csv","seventh.csv","eighth.csv")
for (i in 1:length(all_label)) {
  num_label[i] = length(which(data_proc$unique == all_label[i]))
  layer_label[i]  = length(unlist(strsplit(all_label[i],split = "-")))
}

for(j in 1:max(layer_label)){
  if(j %in% layer_label){
    input_label = all_label[which(layer_label == j)]
    input_num = num_label[which(layer_label == j)]
    TP_all = c()
    FN_all = c()
    FP_all = c()
    TN_all = c()
    FPR_all = c()
    TPR_all = c()
    # for(i in 1:100){
    for(i in 10:10){
      cutoff = i/100
      result = evaluate(cutoff, data_proc, input_label, input_num)
      TP = result$TP
      FN = result$FN
      FP = result$FP
      TN = result$TN
      TPR = result$TPR
      FPR = result$FPR
      TP_all = c(TP_all, TP)
      FN_all = c(FN_all, FN)
      FP_all = c(FP_all, FP)
      TN_all = c(TN_all, TN)
      TPR_all = c(TPR_all, TPR)
      FPR_all = c(FPR_all, FPR)
    }
    output = list(TP = TP_all, FN = FN_all, FP = FP_all, TN = TN_all, TPR = TPR_all, FPR = FPR_all)
    write.csv(output, file = output_name[j])
  }
  else{
    next
  }
}
