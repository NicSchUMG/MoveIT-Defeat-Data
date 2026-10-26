#################################################
### Example Queries for MoveIT Synthetic Data ###
#################################################

## Loading libraries
library(ggplot2)
library(tidyverse)
library(haven)
library(tidyr)
library(patchwork)


## Loading in the data

#Home Computer
data_synth <- read.csv("V:/Nextcloud/Ostfalia/MoveIT/0 data/final/DefeatMoveIT_Synthetic_Raw.csv")


## COVID Status per Group
gruppen <- c("Group_A", "Group_B", "Group_P", "Group_rheuma", "Group_C", "Group_D")

plot_df <- data_synth %>%
  select(all_of(gruppen)) %>%
  mutate(across(everything(), as.numeric)) %>%   # falls die Spalten "labelled" (haven) sind
  pivot_longer(everything(), names_to = "Gruppe", values_to = "Status") %>%
  filter(!is.na(Status)) %>%
  mutate(
    Gruppe = factor(Gruppe,
                    levels = gruppen,
                    labels = c("Gruppe A", "Gruppe B", "Gruppe P",
                               "Gruppe Rheuma", "Gruppe C", "Gruppe D")),
    Status = factor(Status,
                    levels = 0:3,
                    labels = c("No COVID", "Long COVID", "Ex-COVID",
                               "Long COVID\nstatus unclear"))
  )

# Fallzahl pro Gruppe in den Facet-Titel schreiben
n_labels <- plot_df %>%
  count(Gruppe) %>%
  mutate(lab = paste0(Gruppe, " (n = ", n, ")")) %>%
  { setNames(.$lab, .$Gruppe) }

ggplot(plot_df, aes(x = Status, fill = Status)) +
  geom_bar(width = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.4, size = 3.5) +
  facet_wrap(~ Gruppe, nrow = 2, ncol = 3,
             labeller = as_labeller(n_labels)) +
  scale_x_discrete(drop = FALSE) +                          # leere Kategorien trotzdem zeigen
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "COVID-Status nach Gruppe",
       x = "COVID-Status",
       y = "Anzahl Personen") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(face = "bold"),
        panel.grid.major.x = element_blank())


## How many days have you done the following activities in the last 4 weeks? Climbing stairs
plot_df <- data_synth %>%
  filter(!is.na(BSA03f_1_P)) %>%
  mutate(BSA03f_1_P = if (is.labelled(BSA03f_1_P)) as_factor(BSA03f_1_P) else factor(BSA03f_1_P))

ggplot(plot_df, aes(x = BSA03f_1_P)) +
  geom_bar(fill = "steelblue", width = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.4, size = 3.5) +
  scale_y_continuous(name = "Anzahl Personen",
                     breaks = scales::breaks_pretty(n = 8),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(name = "Tage") +
  labs(title = "Bewegungs- und Sportaktivität (Gardening), Kohorte P",
       subtitle = paste0("n = ", nrow(plot_df))) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1))



## How many days between covid vaccination
impf <- data_synth %>%
  mutate(ID = row_number()) %>%          # falls es eine ID-Spalte gibt, diese stattdessen nehmen
  transmute(ID,
            d1 = as.Date(VaccCOVnew1Date_B),
            d2 = as.Date(VaccCOVnew2Date_B),
            d3 = as.Date(VaccCOVnew3Date_B)) %>%
  mutate(abstand_1_2 = as.numeric(d2 - d1),   # Tage zwischen 1. und 2. Impfung
         abstand_2_3 = as.numeric(d3 - d2))   # Tage zwischen 2. und 3. Impfung

summary(impf[, c("abstand_1_2", "abstand_2_3")])

# Plausibilitätscheck: negative Abstände deuten auf Eingabefehler hin
impf %>% filter(abstand_1_2 < 0 | abstand_2_3 < 0)


## Fatigue Assessment Scale Cohort D
fas_items <- c(
  FAS04_D = "I have enough energy for everyday life",
  FAS05_D = "Physically, I feel exhausted",
  FAS06_D = "I have problems to start things",
  FAS07_D = "I have problems to think clearly",
  FAS08_D = "I feel no desire to do anything",
  FAS09_D = "Mentally, I feel exhausted"
)

fas_df <- data_synth %>%
  select(all_of(names(fas_items))) %>%
  mutate(across(everything(), as.numeric)) %>%     # falls gelabelt (haven)
  pivot_longer(everything(), names_to = "Item", values_to = "Antwort") %>%
  filter(!is.na(Antwort)) %>%
  mutate(
    Item = factor(Item, levels = names(fas_items),
                  labels = paste0(names(fas_items), ": ", fas_items)),
    Antwort = factor(Antwort, levels = 1:5,
                     labels = c("Never", "Sometimes", "Regularly", "Often", "Always"))
  )

ggplot(fas_df, aes(x = Antwort, fill = Antwort)) +
  geom_bar(width = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.4, size = 3.5) +
  facet_wrap(~ Item, nrow = 2, ncol = 3,
             labeller = label_wrap_gen(width = 30)) +
  scale_x_discrete(name = "Antwort", drop = FALSE) +
  scale_y_continuous(name = "Anzahl Personen",
                     expand = expansion(mult = c(0, 0.12))) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Fatigue Assessment Scale (FAS), Items 4–9, Kohorte D") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(face = "bold"),
        panel.grid.major.x = element_blank())

## Fatigue Assessment FASgroup_B mit Group_B Covid Status
plot_data <- data_synth %>%
  mutate(
    Group_B = ifelse(
      is.na(Group_B), "NA", as.character(Group_B)
    ),
    FASgroup_B = ifelse(
      is.na(FASgroup_B), "NA", as.character(FASgroup_B)
    ),
    Group_B = factor(
      Group_B,
      levels = c("0", "1", "2", "3", "NA")
    ),
    FASgroup_B = factor(
      FASgroup_B,
      levels = c("0", "1", "NA")
    )
  ) %>%
  count(Group_B, FASgroup_B, .drop = FALSE)

# Balkendiagramm
ggplot(plot_data,
       aes(x = Group_B,
           y = n,
           fill = FASgroup_B)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c(
      "0" = "#4C78A8",
      "1" = "#F58518",
      "NA" = "#B0B0B0"
    )
  ) +
  labs(
    title = "FASgroup_B Wert nach Covid Status (Kohorte B)",
    y = "Anzahl",
    x = NULL,
    fill = "FASgroup_B"
  ) +
  scale_x_discrete(
    labels = c(
      "0" = "No COVID",
      "1" = "Long COVID",
      "2" = "Ex-COVID",
      "3" = "Long COVID status unclear",
      "NA" = "NA"
    )
  ) +
  theme_minimal(base_size = 13) + theme(axis.text.x = element_text(
    angle = 25,
    hjust = 1
  ))
