library(tidyverse)
library(readxl)
library(gridExtra)
library(grid)
library(ggpubr)

df1 <- read_excel("/Users/shaheenkabir/METU/testing/MASS/mass_22/results/stripe_summary.xlsx", sheet = "7_Stripes")
df2 <- read_excel("/Users/shaheenkabir/METU/testing/MASS/mass_25/results/stripe_summary.xlsx", sheet = "7_Stripes")
df3 <- read_excel("/Users/shaheenkabir/METU/testing/MASS/mass_29/results/stripe_summary.xlsx", sheet = "7_Stripes")
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

st12 <- rbind(stripe1.df, stripe2.df)
st34 <- rbind(stripe3.df, stripe4.df)
st567 <- rbind(stripe5.df, stripe6.df, stripe7.df)

ggplot(whole_stripe, aes(x = condition, y = Stripe, fill = condition)) +
  geom_boxplot(fatten = 0, outliers = F) +
  stat_summary(fun = mean, geom = "crossbar",  width = 0.75, color = "red", size = 0.2)+
  stat_boxplot( geom = "errorbar",width = 0.2)+
  facet_wrap(~ stripe, scales = "free_y", nrow = 1) +
  theme_bw() +
  labs(y = "Stripe Value", x = "Condition") +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )

colors <- c("22C" = "#cce5ff", "25C" = "#3399ff", "29C" = "#003366") 
pic1 <- ggplot(st12, aes(x = stripe, y = Stripe, fill = condition)) +
  geom_boxplot(position = position_dodge(width = 0.8), fatten = 0) +
  stat_summary(fun = mean, geom = "crossbar", 
               width = 0.75, color = "red", size = 0.2, 
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2, 
               position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  theme_bw() +
  labs(x = "Stripe Number", y = "Percent Length (%)") +
  theme(legend.position = "top", legend.title = element_blank(),
        axis.title = element_text(face = 'bold',size = 12),
        axis.text = element_text(face = 'bold', size = 12),
        legend.key.size = unit(0.9, "cm"), 
        legend.text = element_text(size = 12))


pic2 <- ggplot(st34, aes(x = stripe, y = Stripe, fill = condition)) +
  geom_boxplot(position = position_dodge(width = 0.8), fatten = 0) +
  stat_summary(fun = mean, geom = "crossbar", 
               width = 0.75, color = "red", size = 0.2, 
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  theme_bw() +
  labs(x = "Stripe Number", y = "Percent Length (%)") +
  theme(legend.position = "top", legend.title = element_blank(),
        axis.title = element_text(face = 'bold',size = 12),
        axis.text = element_text(face = 'bold', size = 12),
        axis.title.y = element_blank(),
        legend.key.size = unit(0.9, "cm"), 
        legend.text = element_text(size = 12))


pic3 <- ggplot(st567, aes(x = stripe, y = Stripe, fill = condition)) +
  geom_boxplot(position = position_dodge(width = 0.8), fatten = 0) +
  stat_summary(fun = mean, geom = "crossbar", 
               width = 0.75, color = "red", size = 0.2, 
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  theme_bw() +
  labs(x = "Stripe Number", y = "Percent Length (%)") +
  theme(legend.position = "top", legend.title = element_blank(), 
        axis.title = element_text(face = 'bold',size = 12),
        axis.text = element_text(face = 'bold', size = 12),
        axis.title.y = element_blank(),
        legend.key.size = unit(0.9, "cm"), 
        legend.text = element_text(size = 12))

pic <- grid.arrange(pic1,pic2,pic3, ncol = 3, top = textGrob("Boxplot of stripe summary for Mass strain",
                                                      gp = gpar(fontsize = 16, fontface = "bold")))
ggsave("/Users/shaheenkabir/METU/testing/MASS/stripe_plot.png", plot = pic, 
       width = 16, height = 9, dpi = 300, units = "in")
ggsave("/Users/shaheenkabir/METU/testing/MASS/stripe_plot.pdf", plot = pic, 
       width = 16, height = 9, units = "in")

