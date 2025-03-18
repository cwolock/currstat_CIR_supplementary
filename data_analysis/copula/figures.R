library(tidyverse)
library(extrafont)
# font_import(prompt = FALSE)
loadfonts(device = "all")
setwd("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis")
main_dat <- readRDS("CIR_results_022425_trunc120_window115_noinconclusives_fixedexpdates_keepinvitedbothyears.rds")
# sens_dat <- readRDS("copula/CIR_copula_030425.rds")
# sens_dat_extra <- readRDS("copula/CIR_copula_030625_1.rds")
# sens_dat_pos <- readRDS("copula/CIR_copula_030625_positivevalues.rds")
# sens_dat_extra <- sens_dat_extra %>% select(-runtime)
# sens_dat_pos <- sens_dat_pos %>% select(-runtime)

sens_dat <- readRDS("copula/CIR_copula_031325_frank.rds")

main_dat$tau = 0
dat <- bind_rows(main_dat, sens_dat) %>%
  filter(t %in% c(30, 60, 90)) %>%
  distinct() %>%
  group_by(t, S_hat_est, tau) %>%
  filter(row_number()==1)

dat <- dat %>% filter(tau >= -0.25 & tau <= 0.25) %>%
  mutate(t = factor(t, levels = c(30, 60, 90),
                    labels = c("30 days", "60 days", "90 days")),
         method = ifelse(tau == 0, "original", "copula"))

# dat$copula <- "clayton"
# dat_save <- dat_save %>% select(-runtime)
# dat_combined <- bind_rows(dat, dat_save)

# dat_combined <- dat_combined %>% mutate(method = ifelse(method == "original", "original",
                                                        # ifelse(copula == "frank", "frank", "clayton")))

p <- dat %>%
  ggplot(aes(x = tau)) +
  geom_point(aes(y = S_hat_est, color = method, shape = method), size = 1.1) +
  geom_errorbar(aes(ymin=S_hat_cil, ymax=S_hat_ciu), width=.0) +
  facet_wrap(~t, ncol = 1, strip.position = "right") +
  theme_bw() +
  xlab(expression("Kendall's "*tau)) +
  ylab("Proportion with symptoms") +
  scale_color_manual(values = c("gray50", "black")) +
  scale_shape_manual(values = c(3, 19)) +
  # ggtitle("Time since positive test") +
  scale_x_continuous(breaks = c(-0.25, -0.20, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25),
                     labels = c("-0.25", "-0.20", "-0.15", "-0.10", "-0.05", "0", "0.05", "0.10",
                                "0.15", "0.20", "0.25"),
                     limits = c(-0.25, 0.25)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Time since positive test",
                                         labels = NULL, breaks = NULL)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        axis.title = element_text(family = "Times New Roman", size = 12),
        axis.text = element_text(family = "Times New Roman", size = 10),
        strip.text = element_text(family = "Times New Roman", size = 10),
        plot.title = element_text(family = "Times New Roman", size = 12, hjust = 0.5),
        legend.position = "none")

ggsave(filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/copula/sensitivity_plot_031325_frank.pdf",
       plot = p,
       device = "pdf",
       width = 7,
       height = 5,
       dpi = 300,
       units = "in")
