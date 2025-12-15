#fect

install.packages("fect")
library(fect)
data(fect)


out.fect <- fect(Y ~ D + X1 + X2, data = simdata, index = c("id","time"),
                 method = "fe", force = "two-way", se =)



out.fect <- fect(Y ~ D + X1 + X2, data = simdata, index = c("id","time"), 
                 method = "fe", force = "two-way", se = TRUE, parallel = TRUE,
                 nboots = 200)
out.fect$att.boot

df <- as_tibble(out.fect$att.boot) %>%
  rename_with(~ paste0("col_", seq_along(.), "_column"))

# Step 2: Compute row standard deviations
df <- df %>%
  rowwise() %>%
  mutate(row_sd = sd(c_across(everything()), na.rm = T)) %>%
  ungroup() |> select(row_sd)

df$att <- out.fect$att

df$time <- out.fect$time

df <- df |> mutate(ci_low = att - row_sd*(qnorm(1-(1-0.95)/2)), 
                      ci_up = att + row_sd*(qnorm(1-(1-0.95)/2)))
ggplot(data = df, 
       mapping = aes(y = att, x = time)) +
  geom_point(size = 2) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  ggtitle("Two-Way Fixed Effects") +
  theme_bw() +
  ylab("Estimated Value (95% C.I.)") + 
  xlab("Time since Treatment Began") +
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = 14, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = 14), 
    plot.title = element_text(size = 14,
                              hjust = 0.5)# X-axis title (e.g., "Event Time")
  )

pdf(paste0("../../results/events/test_judicial_imp/fectfe_test.pdf"))
plot(out.fect, main = "Estimated ATT (FEct)", ylab = "Effect of TreatPost on Y", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, stats = "F.p")
dev.off()

out.ife <- fect(Y ~ D + X1 + X2, data = simdata, index = c("id","time"), 
                force = "two-way", method = "ife", CV = TRUE, r = c(0, 5), 
                se = TRUE, nboots = 200, parallel = TRUE)

plot(out.ife, main = "Estimated ATT (IFEct)", stats = "F.p")


## Normal sun ab: 


### DID 2S 

data("df_het", package = "did2s")
df_het = as.data.frame(df_het)

es <- did2s(df_het,
            yname = "dep_var", first_stage = ~ 0 | state + year,
            second_stage = ~ i(rel_year, ref = Inf), treatment = "treat",
            cluster_var = "state"
)

gard_tibble <- tibble(coef = coef(es), se = se(es), 
                      time = as.numeric(sub(".*::", "", names(coef(es)))))
