avg.age <- df %>% select(Gender, Age) %>% group_by(Gender) %>% summarize(avg=mean(Age))
avg.age

options(repr.plot.width=8, repr.plot.height=6)
box.attrition <- df %>% select(Attrition, JobSatisfaction, Gender) %>% 
ggplot(aes(x=Attrition, y=JobSatisfaction, fill=Attrition)) + geom_boxplot(color="black") + theme_minimal() + facet_wrap(~Gender) + 
scale_fill_manual(values=c("#FA5858", "#9FF781"))

plot_grid(box.attrition)


options(repr.plot.width=8, repr.plot.height=4)
df$Educational_Levels <-  ifelse(df$Education == 1, "Bachelors.D",
                            ifelse(df$Education == 2 , "Masters D.",
                                  ifelse(df$Education == 3, "College D.",
                                        ifelse(df$Education == 4, "Without College D.", "Phd D."))))
edu.level <- df %>% select(Educational_Levels, Attrition) %>% group_by(Educational_Levels, Attrition) %>% 
summarize(n=n()) %>% 
ggplot(aes(x=fct_reorder(Educational_Levels,n), y=n, fill=Attrition, color=Attrition)) + geom_bar(stat="identity") + facet_wrap(~Attrition) + 
coord_flip() + scale_fill_manual(values=c("#2EF688", "#F63A2E")) + scale_color_manual(values=c("#09C873","#DD1509")) + 
geom_label(aes(label=n, fill = Attrition), colour = "white", fontface = "italic") + 
labs(x="", y="Number of Employees", title="Attrition by Educational Level") + theme_wsj() + 
theme(legend.position="none", plot.title=element_text(hjust=0.5, size=14))
edu.level



options(repr.plot.width=8, repr.plot.height=5)
avg.income <- df %>% select(Department, MonthlyIncome, Attrition) %>% group_by(Attrition, Department) %>%
summarize(avg.inc=mean(MonthlyIncome)) %>%
ggplot(aes(x=reorder(Department, avg.inc), y=avg.inc, fill=Attrition)) + geom_bar(stat="identity", position="dodge") + facet_wrap(~Attrition) + 
theme_minimal() + theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + 
scale_fill_manual(values=c("lightgreen", "tomato2")) + 
labs(y="Average Income", x="Department", title="Average Income by Department \n and Attrition Status") + 
geom_text(aes(x=Department, y=0.01, label= paste0("$ ", round(avg.inc,2))),
            hjust=-0.5, vjust=0, size=3, 
            colour="black", fontface="bold",
         angle=90)
avg.income




df$JobSatisfaction <- as.factor(df$JobSatisfaction)
high.inc <- df %>% select(JobSatisfaction, MonthlyIncome, Attrition) %>% group_by(JobSatisfaction, Attrition) %>%
summarize(med=median(MonthlyIncome)) %>%
ggplot(aes(x=fct_reorder(JobSatisfaction, -med), y=med, color=Attrition)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=JobSatisfaction, 
                   xend=JobSatisfaction, 
                   y=0, 
                   yend=med)) + facet_wrap(~Attrition) + 
  labs(title="Is Income a Reason for Employees to Leave?", 
       subtitle="by Attrition Status",
      y="Median Income",
      x="Level of Job Satisfaction") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6), plot.title=element_text(hjust=0.5), strip.background = element_blank(),
  strip.text = element_blank()) + 
coord_flip() + theme_minimal() + scale_color_manual(values=c("#58FA58", "#FA5858")) + 
geom_text(aes(x=JobSatisfaction, y=0.01, label= paste0("$ ", round(med,2))),
            hjust=-0.5, vjust=-0.5, size=4, 
            colour="black", fontface="italic",
         angle=360)
high.inc



df %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% group_by(Attrition, OverTime) %>%
summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100)


overtime_percent <- df %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% group_by(Attrition, OverTime) %>%
summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100) %>% 
ggplot(aes(x="", y=pct, fill=OverTime)) + 
geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
theme_tufte() + scale_fill_manual(values=c("#2EFE64", "#FE2E2E")) + 
geom_label(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5), colour = "white",  fontface = "italic")+
theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(), 
     plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),                                               axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                                                        axis.title=element_text(colour="white"), 
      legend.background = element_rect(fill="#FFF9F5",
                                       size=0.5, linetype="solid", colour ="black")) + 
labs(title="Level of Attrition by Overtime Status", subtitle="In Percent", x="", y="")
plot_grid(overtime_percent)



attr.job <- df %>% select(JobRole, Attrition) %>% group_by(JobRole, Attrition) %>% summarize(amount=n()) %>%
mutate(pct=round(prop.table(amount),2) * 100) %>% arrange(pct)
nofunc <- colorRampPalette(c("#A9F5A9", "#58FA58", "#01DF01"))
yesfunc <- colorRampPalette(c("#F5A9A9", "#FE2E2E", "#B40404"))
yes.attr <- attr.job %>% filter(Attrition == "Yes") %>% arrange(JobRole) 
no.attr <- attr.job %>% filter(Attrition == "No") %>% arrange(JobRole)
par(mar = pyramid.plot(no.attr$pct, yes.attr$pct, labels = unique(attr.job$JobRole),
                       top.labels=c("No","","Yes"), main = "Attrition by Job Role", 
                       gap=30, show.values = T, rxcol = yesfunc(9), lxcol = nofunc(9)))


sample.mean <- mean(df$MonthlyIncome)
sample.mean


sample.n <- length(df$MonthlyIncome)
sample.sd <- sd(df$MonthlyIncome)
sample.se <- sample.sd/sqrt(sample.n)
sample.se


alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
t.score

margin.error <- t.score * sample.se
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(c(lower.bound,upper.bound))
