library(tidyverse)
library(readxl)
library(gridExtra)
library(grid)
library(ggpubr)
library(writexl)

df1 <- read_excel("/Users/shaheenkabir/eve_all_combined/MASS/MASS_22/results/stripe_summary.xlsx", sheet = "7_Stripes")
df2 <- read_excel("/Users/shaheenkabir/eve_all_combined/MASS/MASS_25/results/stripe_summary.xlsx", sheet = "7_Stripes")
df3 <- read_excel("/Users/shaheenkabir/eve_all_combined/MASS/MASS_29/results/stripe_summary.xlsx", sheet = "7_Stripes")
df1
df1_st1 <- data.frame(Stripe = df1$`Stripe-1`) %>% mutate(condition = "22C")
df1_st2 <- data.frame(Stripe = df1$`Stripe-2`) %>% mutate(condition = "22C")
df1_st3 <- data.frame(Stripe = df1$`Stripe-3`) %>% mutate(condition = "22C")
df1_st4 <- data.frame(Stripe = df1$`Stripe-4`) %>% mutate(condition = "22C")
df1_st5 <- data.frame(Stripe = df1$`Stripe-5`) %>% mutate(condition = "22C")
df1_st6 <- data.frame(Stripe = df1$`Stripe-6`) %>% mutate(condition = "22C")
df1_st7 <- data.frame(Stripe = df1$`Stripe-7`) %>% mutate(condition = "22C")

df2_st1 <- data.frame(Stripe = df2$`Stripe-1`) %>% mutate(condition = "25C")
df2_st2 <- data.frame(Stripe = df2$`Stripe-2`) %>% mutate(condition = "25C")
df2_st3 <- data.frame(Stripe = df2$`Stripe-3`) %>% mutate(condition = "25C")
df2_st4 <- data.frame(Stripe = df2$`Stripe-4`) %>% mutate(condition = "25C")
df2_st5 <- data.frame(Stripe = df2$`Stripe-5`) %>% mutate(condition = "25C")
df2_st6 <- data.frame(Stripe = df2$`Stripe-6`) %>% mutate(condition = "25C")
df2_st7 <- data.frame(Stripe = df2$`Stripe-7`) %>% mutate(condition = "25C")

df3_st1 <- data.frame(Stripe = df3$`Stripe-1`) %>% mutate(condition = "29C")
df3_st2 <- data.frame(Stripe = df3$`Stripe-2`) %>% mutate(condition = "29C")
df3_st3 <- data.frame(Stripe = df3$`Stripe-3`) %>% mutate(condition = "29C")
df3_st4 <- data.frame(Stripe = df3$`Stripe-4`) %>% mutate(condition = "29C")
df3_st5 <- data.frame(Stripe = df3$`Stripe-5`) %>% mutate(condition = "29C")
df3_st6 <- data.frame(Stripe = df3$`Stripe-6`) %>% mutate(condition = "29C")
df3_st7 <- data.frame(Stripe = df3$`Stripe-7`) %>% mutate(condition = "29C")

stripe1.df = rbind(df1_st1, df2_st1, df3_st1)
stripe1.df <- stripe1.df %>% mutate(stripe = "1")
stripe2.df = rbind(df1_st2, df2_st2, df3_st2)
stripe2.df <- stripe2.df %>% mutate(stripe = "2")
stripe3.df = rbind(df1_st3, df2_st3, df3_st3)
stripe3.df <- stripe3.df %>% mutate(stripe = "3")
stripe4.df = rbind(df1_st4, df2_st4, df3_st4)
stripe4.df <- stripe4.df %>% mutate(stripe = "4")
stripe5.df = rbind(df1_st5, df2_st5, df3_st5)
stripe5.df <- stripe5.df %>% mutate(stripe = "5")
stripe6.df = rbind(df1_st6, df2_st6, df3_st6)
stripe6.df <- stripe6.df %>% mutate(stripe = "6")
stripe7.df = rbind(df1_st7, df2_st7, df3_st7)
stripe7.df <- stripe7.df %>% mutate(stripe = "7")

whole_stripe <- rbind(stripe1.df,stripe2.df,stripe3.df,stripe4.df, stripe5.df, stripe6.df, stripe7.df)

comparisons <- list(c("22C", "25C"), c("22C", "29C"), c("25C", "29C"))
colors <- c("22C" = "#cce5ff", "25C" = "#3399ff", "29C" = "#336699") 


plot <- ggplot(whole_stripe, aes(x = condition, y = Stripe, fill = condition)) +
  geom_boxplot(fatten = NULL) +
  stat_summary(fun = mean, geom = "crossbar", 
               width = 0.75, color = "red", size = 0.2) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(breaks = seq(0,100, by = 10))+
  facet_wrap(~stripe, ncol = 1, labeller = labeller(stripe = c("1" = "Stripe 1", "2" = "Stripe 2",
                                                               "3" = "Stripe 3", "4" = "Stripe 4",
                                                               "5" = "Stripe 5", "6" = "Stripe 6",
                                                               "7" = "Stripe 7"))) + 
  geom_pwc(method = "t_test", label = "p.signif", hide.ns = T)+
  theme_bw() +
  labs(x = "Condition", y = "Percent Length (%)") +
  theme(legend.position = "top", legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 12),
        axis.text = element_text(face = 'bold', size = 12),
        strip.text = element_text(face = "bold"),
        legend.key.size = unit(0.9, "cm"), 
        legend.text = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  coord_flip()


#ggsave("/Users/shaheenkabir/eve_all_combined/plots/mass_sig.png", plot = pic, 
#      width = 16, height = 9, dpi = 300, units = "in")
ggsave("/Users/shaheenkabir/eve_all_combined/plots/MASS_sig.pdf", plot = plot, 
       width = 10, height = 6, units = "in")

plot

# Variance Test
var_st1_22vs25 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "1"],
                        whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "1"])$p.val
var_st1_25vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "1"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "1"])$p.val
var_st1_22vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "1"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "1"])$p.val

var_st2_22vs25 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "2"],
                           whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "2"])$p.val
var_st2_25vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "2"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "2"])$p.val
var_st2_22vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "2"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "2"])$p.val


var_st3_22vs25 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "3"],
                           whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "3"])$p.val
var_st3_25vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "3"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "3"])$p.val
var_st3_22vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "3"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "3"])$p.val


var_st4_22vs25 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "4"],
                           whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "4"])$p.val
var_st4_25vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "4"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "4"])$p.val
var_st4_22vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "4"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "4"])$p.val


var_st5_22vs25 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "5"],
                           whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "5"])$p.val
var_st5_25vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "5"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "5"])$p.val
var_st5_22vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "5"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "5"])$p.val


var_st6_22vs25 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "6"],
                           whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "6"])$p.val
var_st6_25vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "6"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "6"])$p.val
var_st6_22vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "6"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "6"])$p.val

var_st7_22vs25 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "7"],
                           whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "7"])$p.val
var_st7_25vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "25C" & whole_stripe$stripe == "7"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "7"])$p.val
var_st7_22vs29 <- var.test(whole_stripe$Stripe[whole_stripe$condition == "22C" & whole_stripe$stripe == "7"],
                           whole_stripe$Stripe[whole_stripe$condition == "29C" & whole_stripe$stripe == "7"])$p.val





var_names <- c(
  "var_st1_22vs25", "var_st1_25vs29", "var_st1_22vs29",
  "var_st2_22vs25", "var_st2_25vs29", "var_st2_22vs29",
  "var_st3_22vs25", "var_st3_25vs29", "var_st3_22vs29",
  "var_st4_22vs25", "var_st4_25vs29", "var_st4_22vs29",
  "var_st5_22vs25", "var_st5_25vs29", "var_st5_22vs29",
  "var_st6_22vs25", "var_st6_25vs29", "var_st6_22vs29",
  "var_st7_22vs25", "var_st7_25vs29", "var_st7_22vs29"
)
var_df <- data.frame(
  stripe = rep(1:7, each = 3),
  comparison = rep(c("22vs25", "25vs29", "22vs29"), times = 7),
  p_value = c(
    var_st1_22vs25, var_st1_25vs29, var_st1_22vs29,
    var_st2_22vs25, var_st2_25vs29, var_st2_22vs29,
    var_st3_22vs25, var_st3_25vs29, var_st3_22vs29,
    var_st4_22vs25, var_st4_25vs29, var_st4_22vs29,
    var_st5_22vs25, var_st5_25vs29, var_st5_22vs29,
    var_st6_22vs25, var_st6_25vs29, var_st6_22vs29,
    var_st7_22vs25, var_st7_25vs29, var_st7_22vs29
  )
)

sig <- var_df %>% 
  dplyr::filter(p_value < 0.05)
sig
write.xlsx(sig, "/Users/shaheenkabir/eve_all_combined/sig/MASS_sig.xlsx")