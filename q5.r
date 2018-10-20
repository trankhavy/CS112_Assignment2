library(foreign)
library(ggplot2)
data = read.dta("nsw.dta")
attach(data)
fit = glm(treat~age+education+black+hispanic+married+nodegree+re75, family="binomial",data=data)
pred_prob = predict(fit,data,type="response")
pred_prob_treat = pred_prob[which(treat==1)]
pred_prob_control = pred_prob[which(treat==0)]

# PLot histogram
treat_hist <- ggplot(,aes(x=pred_prob_treat)) + geom_histogram(color="red",fill="white") +
  ggtitle("Distribution of the treatment group's estimated probabilities")+xlab("Estimated probabilities") +
  ylab("Frequency") + 
  theme(plot.title=element_text(size=11,face="bold"))

control_hist <- ggplot(,aes(x=pred_prob_control)) + geom_histogram(color="blue",fill="white") +
  ggtitle("Distribution of the control group's estimated probabilities") + xlab("Estimated probability") +
  ylab("Frequency") +
  theme(plot.title=element_text(size=11,face="bold"))


# interleaving histogram
Treatment <- rep("NA",length(treat))
Treatment[which(treat==1)] = "Treatment group"
Treatment[which(treat==0)] = "Control group"
df = data.frame(pred_prob,Treatment)
interleave_hist = ggplot(df,aes(x=pred_prob,fill=Treatment,color=Treatment)) + 
  geom_histogram(alpha=0.5) + 
  xlab("Estimated probability")+ 
  ylab("Frequency") +
  ggtitle("Distribution of the estimated probabilities for both treatment and control group") +
  theme(plot.title = element_text(size=10,face="bold"))
