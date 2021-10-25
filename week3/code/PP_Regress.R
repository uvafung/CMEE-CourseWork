MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")
require(ggplot2)
require(dplyr)

regress_lm <- lm(Predator.mass ~ Prey.mass, data = MyDF)
summary(regress_lm) # print a summary of the regression results

##### Regression subplots #####
regress_plot <- ggplot(MyDF, aes(x = Prey.mass, y = Predator.mass, colour = Predator.lifestage)) + 
  geom_point(shape = 3) + theme(aspect.ratio = 0.67, legend.position = "bottom", legend.box ="horizontal", plot.margin = unit(c(1,3,1,3), "cm"))+ 
  facet_wrap(Type.of.feeding.interaction ~., dir ="v", ncol = 1, strip.position = 'right') +
  scale_x_log10("Prey Mass in grams") + scale_y_log10("Predator mass in grams") +
  theme_bw() + stat_smooth(method="lm",fullrange=TRUE, se = TRUE, size=.5) + guides(colour=guide_legend(nrow=1))

regress_plot

##### Save figure as a separate pdf file #####
pdf("../results/Regression_subplot.pdf", 11.7, 8.3) 
print(regress_plot)
dev.off()


test1 <- filter(MyDF, Predator.lifestage=="adult", Type.of.feeding.interaction=="insectivorous")
test1

test_lm <- lm(Predator.mass ~ Prey.mass, data = subset(MyDF, Predator.lifestage=="adult", Type.of.feeding.interaction=="insectivorous"))
summary(test_lm)

df_lm <- MyDF %>%
  group_by(Predator.lifestage,Type.of.feeding.interaction) %>%
  do(mod = lm(Predator.mass ~ Prey.mass,data = .))

df_lm

df_coef <- MyDF %>%
  do(data.frame(
    Predator.mass = .$Predator.mass,
    Prey.mass = .$Prey.mass,
    var = names(coef(.$mod)),
    coef(summary(.$mod)))
  )



##### Save regression results as a separate csv #####
write.csv(Regression_Results, "../results/Regress_results.csv")
