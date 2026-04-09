
#-------------------------------------------------------------------------------
# Numeric Data------------------------------------------------------------------
# replace_na allows you to replace missing values in vectors or data frames with 
# specified values. 


numeric_pa_df <- df |> 
  filter(RAKE_MAX %in% c(1,3,4,5)) |> 
  mutate(
    RAKE_MAX = as.numeric(RAKE_MAX),
    nitellopsis_obtusa = as.numeric(nitellopsis_obtusa),
    myriophyllum_spicatum = as.numeric(myriophyllum_spicatum),
    myriophyllum_spicatum_x_sibiricum = as.numeric(myriophyllum_spicatum_x_sibiricum)) |> 
  mutate(
    nitellopsis_obtusa_presence = as.numeric(replace_na(nitellopsis_obtusa > 0, FALSE)),
    myriophyllum_spicatum_presence = as.numeric(replace_na(myriophyllum_spicatum > 0, FALSE)),
    myriophyllum_hybrid_presence = as.numeric(replace_na(myriophyllum_spicatum_x_sibiricum > 0, FALSE))) |> 
  select(DOW, sta_nbr, lake_name, SURVEY_START,nitellopsis_obtusa_presence, 
         myriophyllum_spicatum_presence, myriophyllum_hybrid_presence) |> 
  mutate(SURVEY_START = as.Date(SURVEY_START))



ggplot(numeric_pa_df, aes(x = SURVEY_START)) +
  geom_point(aes(y = nitellopsis_obtusa_presence, color = "Starry Stonewort"), 
             size = 3, shape = 16, alpha = 0.8) +
  geom_point(aes(y = myriophyllum_spicatum_presence, color = "Milfoil"), 
             size = 3, shape = 2, alpha = 0.8) +
  geom_point(aes(y = myriophyllum_hybrid_presence, color = "Milfoil Hybrid"), 
             size = 3, shape = 18, alpha = 0.8) +
  scale_color_manual(values = c("Starry Stonewort" = "blue", "Milfoil" = "red", "Milfoil Hybrid" = "lightgreen")) +
  scale_y_continuous(breaks = c(0,1)) +  # y-axis shows only 0/1
  labs(
    x = "Survey Date",
    y = "Presence/Absence (numeric)",
    color = "Species",
    title = "Presence/Absence of Two AIS Species Over Time")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
wetweight_pa_df <- df |>
  filter(RAKE_MAX %in% c(
    "Wet weights (g) instead of rake scores",
    "Our data are wet weights (g) instead of rake scores."
  )) |>
  mutate(
    nitellopsis_obtusa_presence_wt = as.numeric(replace_na(nitellopsis_obtusa > 0, FALSE)),
    myriophyllum_spicatum_presence_wt = as.numeric(replace_na(myriophyllum_spicatum > 0, FALSE)),
    myriophyllum_hybrid_presence_wt = as.numeric(replace_na(myriophyllum_spicatum_x_sibiricum > 0, FALSE))) |>
  mutate(method = "Wet Weight") |>
  select(DOW, sta_nbr, lake_name, SURVEY_START,
         nitellopsis_obtusa_presence_wt, myriophyllum_spicatum_presence_wt, myriophyllum_hybrid_presence_wt, method) |>
  mutate(SURVEY_START = as.Date(SURVEY_START))

ggplot(wetweight_pa_df, aes(x = SURVEY_START)) +
  geom_point(aes(y = nitellopsis_obtusa_presence_wt, color = "Starry Stonewort"), size = 3, shape = 16) +
  geom_point(aes(y = myriophyllum_spicatum_presence_wt, color = "Milfoil"), size = 3, shape = 2) +
  geom_point(aes(y = myriophyllum_hybrid_presence_wt, color = "Milfoil Hybrid"), size = 3, shape = 18, alpha = 0.8) +
  scale_color_manual(values = c("Starry Stonewort" = "blue", "Milfoil" = "red", "Milfoil Hybrid" = "lightgreen")) +
  scale_y_continuous(breaks = c(0,1)) +   # y-axis shows only 0 (absent) and 1 (present)
  labs(x = "Survey Date", y = "Presence/Absence", color = "Species",
       title = "Presence/Absence of Two AIS Species Over Time (Wet Weight Data)")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

unknown_pa_df <- df |> 
  filter(RAKE_MAX == "Unknown") |> 
  mutate(across(all_of(species_cols), as.numeric)) |> 
  mutate(
    nitellopsis_obtusa_presence_un = replace_na(nitellopsis_obtusa > 0, FALSE),
    myriophyllum_spicatum_presence_un = replace_na(myriophyllum_spicatum > 0, FALSE),
    myriophyllum_hybrid_presence_un = replace_na(myriophyllum_spicatum_x_sibiricum > 0, FALSE)) |> 
  select(DOW, sta_nbr, lake_name, SURVEY_START,
         nitellopsis_obtusa_presence_un, myriophyllum_spicatum_presence_un, myriophyllum_hybrid_presence_un) |> 
  mutate(SURVEY_START = as.Date(SURVEY_START))

#head(unknown_df)

ggplot(unknown_pa_df, aes(x = SURVEY_START)) +
  geom_point(aes(y = nitellopsis_obtusa_presence_un, color = "Starry Stonewort"), 
             size = 3, shape = 16) +
  geom_point(aes(y = myriophyllum_spicatum_presence_un, color = "Milfoil"), 
             size = 3, shape = 2) +
  geom_point(aes(y = myriophyllum_hybrid_presence_un, color = "Milfoil Hybrid"), size = 3, shape = 18, alpha = 0.8) +
  scale_color_manual(values = c("Starry Stonewort" = "blue", "Milfoil" = "red", "Milfoil Hybrid" = "lightgreen")) +
  scale_y_continuous(breaks = c(0, 1)) +  # ensures y-axis only shows 0 and 1
  labs(x = "Survey Date", y = "Presence/Absence", color = "Species",
       title = "Presence/Absence of Two AIS Species Over Time: Unknown")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Combining Data sets 
stt_pa_df <- numeric_pa_df |>  # from your numeric_df pipeline
  rename(
    nitellopsis_presence = nitellopsis_obtusa_presence,
    myriophyllum_presence = myriophyllum_spicatum_presence,
    myriophyllum_hybrid_presence = myriophyllum_hybrid_presence
  ) |> 
  mutate(method = "Rake") |> 
  select(DOW, sta_nbr, lake_name, SURVEY_START,
         nitellopsis_presence, myriophyllum_presence, myriophyllum_hybrid_presence, method)

# Wet weight numeric presence/absence
ww_pa_df <- wetweight_pa_df |>  # from wetweight numeric pipeline
  rename(
    nitellopsis_presence = nitellopsis_obtusa_presence_wt,
    myriophyllum_presence = myriophyllum_spicatum_presence_wt,
    myriophyllum_hybrid_presence = myriophyllum_hybrid_presence_wt
  ) |> 
  mutate(method = "Wet Weight") |> 
  select(DOW, sta_nbr, lake_name, SURVEY_START,
         nitellopsis_presence, myriophyllum_presence, myriophyllum_hybrid_presence, method)

# Unknown numeric presence/absence
unkn_pa_df <- unknown_pa_df |>  # from unknown numeric presence/absence pipeline
  rename(
    nitellopsis_presence = nitellopsis_obtusa_presence_un,
    myriophyllum_presence = myriophyllum_spicatum_presence_un,
    myriophyllum_hybrid_presence = myriophyllum_hybrid_presence_un
  ) |> 
  mutate(method = "Unknown") |> 
  select(DOW, sta_nbr, lake_name, SURVEY_START,
         nitellopsis_presence, myriophyllum_presence, myriophyllum_hybrid_presence, method)

# Combine all three
combined_pa_df <- bind_rows(stt_pa_df, ww_pa_df, unkn_pa_df)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# UN-faceted Plot 
ggplot(combined_pa_df, aes(x = SURVEY_START)) +
  geom_point(aes(y = nitellopsis_presence, color = "Starry Stonewort"), 
             size = 3, alpha = 0.8) +
  geom_point(aes(y = myriophyllum_presence, color = "Milfoil"), 
             size = 3, shape = 2) +
  geom_point(aes(y = myriophyllum_hybrid_presence, color = "Milfoil Hybrid"), 
             size = 3, shape = 18, alpha = 0.8) +
  scale_color_manual(values = c("Starry Stonewort" = "blue", "Milfoil" = "red", "Milfoil Hybrid" = "lightgreen")) +
  labs(
    x = "Survey Date",
    y = "Abundance",
    color = "Species",
    title = "Abundance of Two AIS Species Over Time"
  )

combined_long <- combined_pa_df |>
  pivot_longer(
    cols = c(nitellopsis_presence, myriophyllum_presence, myriophyllum_hybrid_presence),
    names_to = "species",
    values_to = "presence")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

ggplot(combined_long, aes(x = SURVEY_START, y = presence, color = method)) +
  geom_point(size = 3, alpha = 0.8) +
  facet_wrap(~ species) +
  scale_y_continuous(breaks = c(0,1)) +
  labs(x = "Survey Date",
       y = "Presence/Absence",
       color = "Method",
       title = "Presence/Absence of AIS Species Over Time by Method")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# isolated just the year
combined_long <- combined_long |>
  mutate(year = as.numeric(format(SURVEY_START, "%Y")))

combined_y_l <- combined_long |> 
  group_by(lake_name, DOW, year)|>
summarise(n_sites = n())|> 
ungroup()
combined_y_l <- combined_y_l |> 
  group_by(DOW)|> 
  summarise(n_years = n())|> 
  filter(n_years > 5)

# make vector with DOW that have more than specified amount of surveys 
# 4 years 
# pick if there are multiple surveys per year, pick ones around the same time 
# make df with a row for each lake, year, survey -> how often does that happen?
# figuring out how many have multiple--> count of dates in one row

#-----------
# 
#----------

# Step 1: find lakes (DOW) sampled more than once
lakes_sampled <- combined_pa_df |>
  group_by(DOW) |>
  summarise(n_samples = n(), .groups = "drop") |>
  filter(n_samples > 5)

# Step 2: filter the long-format data to only those lakes
station_long_filtered <- combined_long |>
  filter(DOW %in% combined_y_l$DOW)

# Step 3: plot
ggplot(station_long_filtered, aes(x = year, y = presence, color = species)) +
  geom_point(alpha = 0.7, size = 3, position = position_jitter(height = 0.05)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  scale_y_continuous(breaks = c(0,1), limits = c(-0.1, 1.1)) +
  labs(
    x = "Year",
    y = "Presence / Absence",
    color = "Species",
    title = "Presence/Absence Over Time (Repeatedly Sampled Lakes)"
  ) +
  theme_minimal()
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
station_long_filtered <- combined_long |>
  group_by(sta_nbr) |>
  filter(n() > 2) |>
  ungroup()

ggplot(station_long_filtered, aes(x = year, y = presence)) +
  geom_point(alpha = 0.7, size = 3, position = position_jitter(height = 0.05)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "blue") +
  scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +
  facet_wrap(~ species) +
  labs(x = "Year", y = "Presence / Absence", title = "Presence/Absence of Invasive Species") +
  theme_minimal()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
dow_counts <- combined_pa_df |>
  group_by(DOW, lake_name) |>
  summarise(n_samples = n(), .groups = "drop") |>
  arrange(desc(n_samples))

# View top lakes
head(dow_counts)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

