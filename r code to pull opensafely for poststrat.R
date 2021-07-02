
library(dplyr)
library(ggplot2)
# read in data from opensafely github:

age_50_54_ethn <- read.csv("https://raw.githubusercontent.com/opensafely/nhs-covid-vaccination-coverage/master/released-outputs/machine_readable_outputs/figure_csvs/Cumulative%20vaccination%20percent%20among%2050-54%20population%20by%20ethnicity%206%20groups_tpp.csv") 
age_50_54_ethn$age_group <- "50-54"
age_55_59_ethn <- read.csv("https://raw.githubusercontent.com/opensafely/nhs-covid-vaccination-coverage/master/released-outputs/machine_readable_outputs/figure_csvs/Cumulative%20vaccination%20percent%20among%2055-59%20population%20by%20ethnicity%206%20groups_tpp.csv")
age_55_59_ethn$age_group <- "55-59"
age_60_64_ethn <- read.csv("https://raw.githubusercontent.com/opensafely/nhs-covid-vaccination-coverage/master/released-outputs/machine_readable_outputs/figure_csvs/Cumulative%20vaccination%20percent%20among%2060-64%20population%20by%20ethnicity%206%20groups_tpp.csv")
age_60_64_ethn$age_group <- "60-64"
age_65_69_ethn <- read.csv("https://raw.githubusercontent.com/opensafely/nhs-covid-vaccination-coverage/master/released-outputs/machine_readable_outputs/figure_csvs/Cumulative%20vaccination%20percent%20among%2065-69%20population%20by%20ethnicity%206%20groups_tpp.csv")
age_65_69_ethn$age_group <- "65-69"
age_70_79_ethn <- read.csv("https://raw.githubusercontent.com/opensafely/nhs-covid-vaccination-coverage/master/released-outputs/machine_readable_outputs/figure_csvs/Cumulative%20vaccination%20percent%20among%2070-79%20population%20by%20ethnicity%206%20groups_tpp.csv")
age_70_79_ethn$age_group <- "70-79"
age_80over_ethn <- read.csv("https://raw.githubusercontent.com/opensafely/nhs-covid-vaccination-coverage/master/released-outputs/machine_readable_outputs/figure_csvs/Cumulative%20vaccination%20percent%20among%2080%2B%20population%20by%20ethnicity%206%20groups_tpp.csv")
age_80over_ethn$age_group <- "80plus"

most_recent_date <- max(as.Date(as.character(age_50_54_ethn$covid_vacc_date), format="%Y-%m-%d"))

# combine this: 
opensafely_ages_50plus <- rbind(age_50_54_ethn, age_55_59_ethn, age_60_64_ethn, age_65_69_ethn,
                                age_70_79_ethn, age_80over_ethn)

opensafely_ages_50plus$covid_vacc_date <- as.Date(as.character(opensafely_ages_50plus$covid_vacc_date), format="%Y-%m-%d")
names(opensafely_ages_50plus)
opensafely_ages_50plus$Black_perc <- opensafely_ages_50plus$Black / opensafely_ages_50plus$Black_total
opensafely_ages_50plus$Mixed_perc <- opensafely_ages_50plus$Mixed / opensafely_ages_50plus$Mixed_total
opensafely_ages_50plus$Other_perc <- opensafely_ages_50plus$Other / opensafely_ages_50plus$Other_total
opensafely_ages_50plus$South.Asian_perc <- opensafely_ages_50plus$South.Asian / opensafely_ages_50plus$South.Asian_total
opensafely_ages_50plus$White_perc <- opensafely_ages_50plus$White / opensafely_ages_50plus$White_total

# check whether ratios uptake between ethnicities are more or less stable over age (or a pattern exists):
# do it with white as reference category: 
opensafely_ages_50plus$Black_vs_white <- opensafely_ages_50plus$Black_perc / opensafely_ages_50plus$White_perc
opensafely_ages_50plus$Mixed_vs_white <- opensafely_ages_50plus$Mixed_perc / opensafely_ages_50plus$White_perc
opensafely_ages_50plus$Other_vs_white <- opensafely_ages_50plus$Other_perc / opensafely_ages_50plus$White_perc
opensafely_ages_50plus$South.Asian_vs_white <- opensafely_ages_50plus$South.Asian_perc / opensafely_ages_50plus$White_perc

opensafely_ages_50plus%>%filter(covid_vacc_date=="2021-01-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
opensafely_ages_50plus%>%filter(covid_vacc_date=="2021-02-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
opensafely_ages_50plus%>%filter(covid_vacc_date=="2021-03-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
opensafely_ages_50plus%>%filter(covid_vacc_date=="2021-04-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
opensafely_ages_50plus%>%filter(covid_vacc_date=="2021-05-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
opensafely_ages_50plus%>%filter(covid_vacc_date=="2021-06-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)

# so in first months it is uncertain, but in recent data uptake ratio becomes worse by age compared to white:
# BUT, shielding individuals not included in age-groups <70, leading to underestimation of uptake in those groups. 
# HOWEVER, proportion shielding for each group won't increase with decreasing age (the reverse must be true), so age-effect likely still there. 
# for simplicity assume that uptake 

shielding_16_69_ethn <- read.csv("https://raw.githubusercontent.com/opensafely/nhs-covid-vaccination-coverage/master/released-outputs/machine_readable_outputs/figure_csvs/Cumulative%20vaccination%20percent%20among%20shielding%20(aged%2016-69)%20population%20by%20ethnicity%206%20groups_tpp.csv")
shielding_16_69_ethn$age_group <- "shielding_16_69"

shielding_16_69_ethn$Black_perc <- shielding_16_69_ethn$Black / shielding_16_69_ethn$Black_total
shielding_16_69_ethn$Mixed_perc <- shielding_16_69_ethn$Mixed / shielding_16_69_ethn$Mixed_total
shielding_16_69_ethn$Other_perc <- shielding_16_69_ethn$Other / shielding_16_69_ethn$Other_total
shielding_16_69_ethn$South.Asian_perc <- shielding_16_69_ethn$South.Asian / shielding_16_69_ethn$South.Asian_total
shielding_16_69_ethn$White_perc <- shielding_16_69_ethn$White / shielding_16_69_ethn$White_total
# check whether ratios uptake between ethnicities are more or less stable over age (or a pattern exists):
# do it with white as reference category: 
shielding_16_69_ethn$Black_vs_white <- shielding_16_69_ethn$Black_perc / shielding_16_69_ethn$White_perc
shielding_16_69_ethn$Mixed_vs_white <- shielding_16_69_ethn$Mixed_perc / shielding_16_69_ethn$White_perc
shielding_16_69_ethn$Other_vs_white <- shielding_16_69_ethn$Other_perc / shielding_16_69_ethn$White_perc
shielding_16_69_ethn$South.Asian_vs_white <- shielding_16_69_ethn$South.Asian_perc / shielding_16_69_ethn$White_perc

shielding_16_69_ethn%>%filter(covid_vacc_date=="2021-01-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
shielding_16_69_ethn%>%filter(covid_vacc_date=="2021-02-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
shielding_16_69_ethn%>%filter(covid_vacc_date=="2021-03-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
shielding_16_69_ethn%>%filter(covid_vacc_date=="2021-04-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
shielding_16_69_ethn%>%filter(covid_vacc_date=="2021-05-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
shielding_16_69_ethn%>%filter(covid_vacc_date=="2021-06-01")%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
# so shielding more similar to 70-79 or 80+

# can't use below unfortunately, as don't have underlying population totals:
all_eligible <- read.csv("https://raw.githubusercontent.com/opensafely/nhs-covid-vaccination-coverage/master/released-outputs/machine_readable_outputs/figure_csvs/Cumulative%20vaccination%20figures%20among%20each%20eligible%20group_tpp.csv")
other_16_49 <- read.csv("https://raw.githubusercontent.com/opensafely/nhs-covid-vaccination-coverage/master/released-outputs/machine_readable_outputs/table_csvs/Cumulative%20vaccination%20figures%20among%2016-49%2C%20not%20in%20other%20eligible%20groups%20shown%20population_tpp.csv")
# can't 
# opensafely uses only highest risk-group, so if in care-home not also in age_80over. 

# so it isn't perfect as shielding takes priority, but ignore for now just to see whether it can solve anything: 
# take most recent data for simplicity for age*ethnicity interaction: 
check <- opensafely_ages_50plus%>%filter(covid_vacc_date==most_recent_date)%>%select(age_group, Black_vs_white, Mixed_vs_white, Other_vs_white, South.Asian_vs_white)
check$rownumber <- 1:dim(check)[1]
summary(check$Black_vs_white)
ggplot(check, aes(x=rownumber, y=Black_vs_white)) + 
  geom_line()
ggplot(check, aes(x=rownumber, y=Mixed_vs_white)) + 
  geom_line()
ggplot(check, aes(x=rownumber, y=Other_vs_white)) + 
  geom_line()
ggplot(check, aes(x=rownumber, y=South.Asian_vs_white)) + 
  geom_line()

# keep South.Asian vs white stable at: 
Asian_vs_white <- (sum(opensafely_ages_50plus$South.Asian[opensafely_ages_50plus$covid_vacc_date==most_recent_date]) / 
  sum(opensafely_ages_50plus$South.Asian_total[opensafely_ages_50plus$covid_vacc_date==most_recent_date])) / 
  (sum(opensafely_ages_50plus$White[opensafely_ages_50plus$covid_vacc_date==most_recent_date]) / 
     sum(opensafely_ages_50plus$White_total[opensafely_ages_50plus$covid_vacc_date==most_recent_date]))

##############################################################################################################################################
# use coronavirus dashboard: 
# https://coronavirus.data.gov.uk/details/vaccinations?areaType=ltla&areaName=Bassetlaw


#check <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage&format=csv")
#check2 <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage&format=csv")
age_england <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv")
age_ltla <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=vaccinationsAgeDemographics&format=csv")
age_region <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=vaccinationsAgeDemographics&format=csv")
#########################################################################################################################################################

# for others assume a relationship with age based on above, 
# for simplicity assume a linear relationship with each 5 years from rownumber 4 (65-69) and downwards and see where I end up with: 
check4 <- check%>%filter(rownumber<5)

temp <- data.frame(age=sort(unique(as.character(age_ltla$age))),
                   rownumber=1:length(unique(as.character(age_ltla$age))))
temp
# so 4 (65-79) should be 10 if I go down by ages of 5 (put 16 within 18_24):
check4$rownumber <- check4$rownumber + 6

# change - to _: 
check4$age <- gsub("-","_", check4$age_group)

model_black <- lm(Black_vs_white ~ rownumber, data=check4)
model_mixed <- lm(Mixed_vs_white ~ rownumber, data=check4)
model_other <- lm(Other_vs_white ~ rownumber, data=check4)
model_asian <- lm(South.Asian_vs_white ~ rownumber, data=check4)

# predict what it is at other age_groups: 
predictions <- data.frame(age=sort(unique(as.character(age_ltla$age))),
                          rownumber=1:length(unique(as.character(age_ltla$age))),
                          Black_vs_white=predict(model_black, newdata = temp),
                          Mixed_vs_white=predict(model_mixed, newdata = temp),
                          Other_vs_white=predict(model_other, newdata = temp),
                          South.Asian_vs_white=predict(model_asian, newdata = temp))
  



####################################################################################################################################################
# somehow combine this: 
# start with england as easier to oversee, but same principles will apply to ltla and region: 
head(age_ltla)

age_ethn_ltla <- age_ltla%>%select(areaCode,areaName, date, age, VaccineRegisterPopulationByVaccinationDate, cumPeopleVaccinatedFirstDoseByVaccinationDate)
age_ethn_ltla$date <- as.Date(as.character(age_ethn_ltla$date), format="%Y-%m-%d")
age_ethn_ltla$vaccinated_prop <- age_ethn_ltla$cumPeopleVaccinatedFirstDoseByVaccinationDate / age_ethn_ltla$VaccineRegisterPopulationByVaccinationDate

sort(unique(age_ethn_ltla$age))


# maybe because of the issues with opensafely making distinction between those that do and don't have 
# clear indications <69, just base it on average observed in 70-79, but time-specific: 

opensafely_70over <- opensafely_ages_50plus%>%filter(age_group %in% c("70-79","80plus"))%>%
  group_by(covid_vacc_date)%>%summarise(
  Black = sum(Black), Mixed = sum(Mixed), Other=sum(Other), South.Asian = sum(South.Asian), White = sum(White),
  Black_total = sum(Black_total), Mixed_total = sum(Mixed_total), Other_total = sum(Other_total),
  South.Asian_total = sum(South.Asian_total), White_total = sum(White_total))

opensafely_70over <- opensafely_70over%>%mutate(Black_perc = Black / Black_total, 
                                                Mixed_perc = Mixed / Mixed_total, 
                                                Other_perc = Other / Other_total,
                                                South.Asian_perc = South.Asian / South.Asian_total,
                                                White_perc = White / White_total)

opensafely_70over <- opensafely_70over%>%mutate(Black_vs_white = Black_perc / White_perc,
                                                Mixed_vs_white = Mixed_perc / White_perc,
                                                Other_vs_white = Other_perc / White_perc, 
                                                South.Asian_vs_white = South.Asian_perc / White_perc)
# NaN make 0: 
is.nan.data.frame <- function(x) do.call(cbind,lapply(x, is.nan))
opensafely_70over[is.nan.data.frame(opensafely_70over)] <- 0

###############################################################################################################
# ok let's go for this for now, what I really need is within each day, the percentage of vaccinated that
# is in each ehtnicity category: 
opensafely_70over <- opensafely_70over%>%mutate(total_vaccinated = Black + Mixed + Other + 
                                                  South.Asian + White)
opensafely_70over <- opensafely_70over%>%mutate(black_vac = Black / total_vaccinated,
                                                mixed_vac = Mixed / total_vaccinated, 
                                                other_vac = Other / total_vaccinated,
                                                asian_vac = South.Asian / total_vaccinated, 
                                                white_vac = White / total_vaccinated,
                                                non_white = (Black + Mixed + Other + South.Asian) / 
                                                  total_vaccinated)
# NaN make 0: 
is.nan.data.frame <- function(x) do.call(cbind,lapply(x, is.nan))
opensafely_70over[is.nan.data.frame(opensafely_70over)] <- 0

# get in long format to make it possible to merge: 
date_specific_70over <- opensafely_70over%>%dplyr::select(covid_vacc_date,
                                                          black_vac,
                                                          mixed_vac,
                                                          other_vac,
                                                          asian_vac,
                                                          white_vac,
                                                          non_white)
names(date_specific_70over) <- c("date","Black","Mixed","Other","South.Asian","White","Nonwhite")
date_specific_70over <- date_specific_70over%>%tidyr::pivot_longer(!date,
                                                            names_to="ethnicity",
                                                            values_to="vac_prop_date_specific")


# just an overall percentage for 70+, how precise do we really now this anyway...:
overall_70over <- data.frame(ethnicity=c("Black","Mixed","Other","South.Asian","White","Nonwhite"),
  
  vac_prop = c(sum(opensafely_70over$Black) / sum(opensafely_70over$total_vaccinated),
sum(opensafely_70over$Mixed) / sum(opensafely_70over$total_vaccinated),
sum(opensafely_70over$Other) / sum(opensafely_70over$total_vaccinated),
sum(opensafely_70over$South.Asian) / sum(opensafely_70over$total_vaccinated),
sum(opensafely_70over$White) / sum(opensafely_70over$total_vaccinated),

sum(opensafely_70over$Black, opensafely_70over$Mixed, opensafely_70over$Other,
    opensafely_70over$South.Asian) / sum(opensafely_70over$total_vaccinated)
))

###################################################################################################################
##########################################################################################################

head(age_ethn_ltla)
summary(age_ethn_ltla$age)

class(age_ethn_ltla$date)
class(opensafely_70over$covid_vacc_date)

# create 5 rows for each existing row
test_5level <- age_ethn_ltla[rep(seq_len(nrow(age_ethn_ltla)), each=6),]
nrow(test_5level)/nrow(age_ethn_ltla)
test_5level$ethnicity <- rep(c("Black","Mixed","Other","South.Asian","White","Nonwhite"), times=nrow(age_ethn_ltla))

levels(factor(test_5level$ethnicity))==levels(factor(overall_70over$ethnicity))
# stable percentage: 
test_5level <- merge(test_5level, overall_70over, by="ethnicity")
# date specific percentage: 
test_5level <- merge(test_5level, date_specific_70over, by=c("ethnicity","date"), all.x = TRUE)

head(test_5level)

# need to do locf by ethinicity: 
test_5level <- test_5level %>% group_by(ethnicity) %>% mutate(vac_prop_date_specific=zoo::na.locf(vac_prop_date_specific, na.rm=FALSE))

summary(test_5level$vaccinated_prop)

test_5level[is.nan.data.frame(test_5level)] <- 0

length(unique(test_5level$areaCode)) # ah ltla actually is just LAD, it's the same thing after all...

########################################################################################################
# read in lookup ltla (which is lad) to CIS: 

geo_lookup <- read.csv("C:/Users/Dhr. K. Pouwels/Downloads/Local_Authority_District__May_2018__to_Covid_Infection_Survey__October_2020__Lookup_in_England.csv")
unique(test_5level$areaCode[!test_5level$areaCode %in% geo_lookup$LAD18CD])

geo_lookup <- read.csv("C:/Users/Dhr. K. Pouwels/Downloads/Local_Authority_District__December_2019__to_Covid_Infection_Survey__October_2020__Lookup_for_Great_Britain.csv")
unique(test_5level$areaCode[!test_5level$areaCode %in% geo_lookup$LAD19CD])
unique(geo_lookup$LAD19CD[!geo_lookup$LAD19CD %in% test_5level$areaCode])
# so can link all, there is one area in geo_lookup that is not in test_5level: E06000053
geo_lookup%>%filter(LAD19CD=="E06000053") # that's Isles of Scilly, so can safely ignore (put together with cornwall)
geo_lookup <- geo_lookup%>%filter(LAD19CD!="E06000053")%>%dplyr::select(LAD19CD, CIS20CD)

test_5level <- merge(x=test_5level, y=geo_lookup, by.x="areaCode", by.y = "LAD19CD")

head(test_5level)
# group by CIS_area: 
test_5level_CIS <- test_5level%>%group_by(CIS20CD, date, age, ethnicity)%>%summarise(
  VaccineRegisterPopulationByVaccinationDate = sum(VaccineRegisterPopulationByVaccinationDate),
  cumPeopleVaccinatedFirstDoseByVaccinationDate = sum(cumPeopleVaccinatedFirstDoseByVaccinationDate),
  vaccinated_prop = sum(cumPeopleVaccinatedFirstDoseByVaccinationDate) / sum(VaccineRegisterPopulationByVaccinationDate),
  vac_prop = mean(vac_prop), # as not area-specific
  vac_prop_date_specific = mean(vac_prop_date_specific) # as not area-specific
)

test_5level_CIS[is.nan.data.frame(test_5level_CIS)] <- 0


test_5level_CIS <- test_5level_CIS%>%mutate(timefixed_vac_prop=vaccinated_prop*vac_prop,
                                            timevarying_vac_prop=vaccinated_prop*vac_prop_date_specific)

write.csv(test_5level_CIS, "C:/Users/Dhr. K. Pouwels/Documents/COVID19/OPENSAFELY vaccination/CIS_age_ethn_vacuptake.csv")
