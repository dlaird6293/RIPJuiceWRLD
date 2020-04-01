install.packages("kernlab")



library(kernlab)

library(caret)

data("spam")



tibble::as.tibble(spam)







is.factor(spam$type)



levels(spam$type)







set.seed(42)



spam_idx = sample(nrow(spam), round(nrow(spam) / 2))



spam_idx = sample(nrow(spam), 1000)



spam_trn = spam[spam_idx, ]



spam_tst = spam[-spam_idx, ]







fit_caps = glm(type ~ capitalTotal,
               
               
               
               data = spam_trn, family = binomial)



fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   
                   
                   
                   data = spam_trn, family = binomial)



fit_additive = glm(type ~ .,
                   
                   
                   
                   data = spam_trn, family = binomial)



fit_over = glm(type ~ capitalTotal * (.),
               
               
               
               data = spam_trn, family = binomial, maxit = 50)







# training misclassification rate



mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)



mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)



mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)



mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)







library(boot)



set.seed(2)



cv.glm(spam_trn, fit_caps, K = 100)$delta[1]



cv.glm(spam_trn, fit_selected, K = 100)$delta[1]



cv.glm(spam_trn, fit_additive, K = 100)$delta[1]



cv.glm(spam_trn, fit_over, K = 100)$delta[1]











# Exercise 1



# Part 1



# The ranking of the 4 models from  "most underfit" to "most overfit" is as follows:



# fit_caps, fit_selected, fit_additive, and fit_over



# After changing to 100 folds and setting the seed at 2, the rankings are the same and nothing changed



# Part 2



make_conf_mat = function(predicted, actual) {
  
  
  
  table(predicted = predicted, actual = actual)
  
  
  
}







spam_tst_pred1 = ifelse(predict(fit_additive, spam_tst) > 0,
                        
                        
                        
                        "spam", "nonspam")







(conf_mat_501 = make_conf_mat(predicted = spam_tst_pred1, actual = spam_tst$type))



table(spam_tst$type) / nrow(spam_tst)



#



spam_tst_pred2 = ifelse(predict(fit_caps, spam_tst) > 0,
                        
                        
                        
                        "spam", "nonspam")







(conf_mat_502 = make_conf_mat(predicted = spam_tst_pred2, actual = spam_tst$type))



table(spam_tst$type) / nrow(spam_tst)



#



spam_tst_pred3 = ifelse(predict(fit_over, spam_tst) > 0,
                        
                        
                        
                        "spam", "nonspam")







(conf_mat_503 = make_conf_mat(predicted = spam_tst_pred3, actual = spam_tst$type))



table(spam_tst$type) / nrow(spam_tst)



#



spam_tst_pred4 = ifelse(predict(fit_selected, spam_tst) > 0,
                        
                        
                        
                        "spam", "nonspam")







(conf_mat_504 = make_conf_mat(predicted = spam_tst_pred4, actual = spam_tst$type))



table(spam_tst$type) / nrow(spam_tst)



#       The best model for sensitivity would be model 1, as the sensitivity and specificity are

#  both highest in this # model.  Models 3 and  4 have a higher strict number of

#  nonspam correctly identified, but there is also a higher number of incorrectly

#  identified spam when compared to model 1.  Model 3 has a better specificity, however, as its

#  ratio of correctly identified spam to false positives is the highest of our 4 models. 



#      We would say that the "best" model in this case is model 1.  The sensitivity is slightly

#  lower than models 3 and 4, but the overall balance--along with the relatively successful

#  sensitivity and specificity in the model--makes for the best "overall" model in this case.

#  there is a high enough specificity and sensitivity in total









#Exercise 2



bank <- read.csv("bank.csv")



bank_idx = createDataPartition(bank$yes, p = 0.75, list = FALSE)

bank_trn = bank[bank_idx, ]

bank_tst = bank[-bank_idx, ]

bank_glm_mod = train(
  
  form = y ~ .,
  
  data = bank_trn,
  
  trControl = trainControl(method = "cv", number = 10),
  
  method = "glm",
  
  family = "binomial"
  
)

trainControl(method = "cv", number = 10)[1:3]



bank_glm_mod  



names(bank_glm_mod)



bank_glm_mod$results



bank_glm_mod$finalModel





#age: As age increases, the person is less likely to say yes.

#Job:With all jobs except retired, student, and unknown, the person is less likely to say yes if

#they have the listed job.

#Marital Status:the person is more likely to say yes if they are married vs single, but not by

#a large amount.

#education: The more education a person has, the more likely they are to say yes (the coefficient

#is higher for those with tertiary education than with secondary, and both are higher than

#unknown education.)

#default:If the person has defaulted, they are more likely to say yes.

#balance:The higher the person's current balance, the more likely they are to say yes.

#housing:The person is more likely to say yes if they answered yes to the housing question.

#loan:If the person has received a loan from the bank, they are less likely to say yes.

#contact:The person is more likely to say yes if they have an established mode of contact other than

#"unknown"

#day:There seems to be a correlation between likeliness to say yes and how late in the month the

#person was called.

#month:The best months for yes answers were october, september, march, february, and especially june.

#duration:The Longer the person was on the phone with the bank, the more likely they were to day yes.

#campaign:The more times the person has been called during this campaign, the less likely they are to

#say yes.

#previous:the more times someone had been called by the band previously, the more likely they

#were to say yes.



# Part 4

(conf_matrix = make_conf_mat(predicted = bank_glm_mod, actual = bank_trn$type))



table(bank_trn$type) / nrow(bank_trn)