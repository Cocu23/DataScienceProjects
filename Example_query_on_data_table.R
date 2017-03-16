#creating data table (for performance reasons)
library (data.table)
testset <- data.table(anomalyTestSetNonAnon)
testset
#query example
# general data table query form:
#Take DT- subset, i.e. all rows satisfyigin condition i
# then do j (selecting, calculating or whatever; here: simple "select")
# group by "by"-statement
testset[
  failure >=1,  # i
  . ( date, days_since_EBK, total.EBK), # j
  by = .(vehiclenr)  #by
  ]