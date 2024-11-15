library(tidyverse)
library(robotoolbox)


# Download the data
l <- kobo_asset_list()

uid <- l$uid[2]

asset <- kobo_asset(uid)

asset

full_data <- kobo_submissions(asset)

main_data <- full_data$main|> 
  rename(
    "recount_f05" = "recount_vot"
  )

amend_data <- full_data$amend_party |>
  mutate(
    amend_party_name = case_when(
      str_detect(amend_party_name, "გახარი") ~ "25 გახარია საქართველოსთვის",
      amend_party_name %in% c('გირჩი', '"გირჩი"', '“გირჩი"', '"გირჩი" 36') ~ "36 გირჩი",
      str_detect(amend_party_name, "ევროპელი დემოკრატები|ევროპული დემოკრატები") ~ "6 ევროპელი დემოკრატები",
      str_detect(amend_party_name, "ნაციონალური მოძრაობა") ~ "5 ერთიანობა ნაციონალური მოძრაობა",
      str_detect(amend_party_name, "მემარცხენე ალიანსი") ~ "26 მემარცხენე ალიანსი",
      str_detect(amend_party_name, "შეცვალე საქართველო") ~ "16 შეცვალე საქართველო",
      str_detect(amend_party_name, "ჩვენი გაერთიანებული საქართველო") ~ "12 ჩვენი გაერთიანებული საქართველო",
      str_detect(amend_party_name, '"ჩვენ"') ~ "23 ჩვენ",
      str_detect(amend_party_name, "ძლიერი საქართველო") ~ "9 ძლიერი საქართველო, ლელო, ხალხისთვის, თავისუფლებისთვის",
      str_detect(amend_party_name, "ქართული ოცნება") ~ "41 ქართული ოცნება",
      str_detect(amend_party_name, "თავისუფალი საქართველო") ~ "20 თავისუფალი საქართველო",
      str_detect(amend_party_name, "კოალიცია ცვლილებებისთვის") ~ "4 კოალიცია ცვლილებებისთვის",
      str_detect(amend_party_name, "ტრიბუნა") ~ "21 ტრიბუნა",
      str_detect(amend_party_name, "ლეიბორ") ~ "10 საქართველოს ლეიბორისტული პარტია",
      str_detect(amend_party_name, "პატრიოტ") ~ "8 საქართველოს პატრიოტთა ალიანსი",
      str_detect(amend_party_name, " ერთობა") ~ "27 ქართველ ერთობა",
      amend_party_name == "ჩვენ" ~ "23 ჩვენ",
      amend_party_name == "საქართველო" ~ "17 საქართველო",
      amend_party_name == "კოალიცია ცვლილებისთვის" ~ "4 კოალიცია ცვლილებებისთვის",
      T ~ as.character(amend_party_name)
    )
  ) |>
  separate(
    amend_party_name, into = c("amend_party_name"), sep = " |\\."
  ) |> 
  select(
    c(amend_party_name, amend_party_fixed, `_parent_index`)
  ) |>
  mutate(
    amend_party_name = paste0("amend_p_", amend_party_name)
  ) |>
  pivot_wider(
    names_from = amend_party_name, values_from = amend_party_fixed
  ) |> 
  rename(
    "_index" = "_parent_index"
  )

box_data <- full_data$box_party |>
  mutate(
    box_party_name = case_when(
      str_detect(box_party_name, "გახარი") ~ "25 გახარია საქართველოსთვის",
      box_party_name %in% c('გირჩი', '"გირჩი"', '“გირჩი"', '"გირჩი" 36') ~ "36 გირჩი",
      str_detect(box_party_name, "ევროპელი დემოკრატები|ევროპული დემოკრატები") ~ "6 ევროპელი დემოკრატები",
      str_detect(box_party_name, "ნაციონალური მოძრაობა") ~ "5 ერთიანობა ნაციონალური მოძრაობა",
      str_detect(box_party_name, "მემარცხენე ალიანსი") ~ "26 მემარცხენე ალიანსი",
      str_detect(box_party_name, "შეცვალე საქართველო") ~ "16 შეცვალე საქართველო",
      str_detect(box_party_name, "ჩვენი გაერთიანებული საქართველო") ~ "12 ჩვენი გაერთიანებული საქართველო",
      str_detect(box_party_name, '"ჩვენ"') ~ "23 ჩვენ",
      str_detect(box_party_name, "ძლიერი საქართველო") ~ "9 ძლიერი საქართველო, ლელო, ხალხისთვის, თავისუფლებისთვის",
      str_detect(box_party_name, "ქართული ოცნება") ~ "41 ქართული ოცნება",
      str_detect(box_party_name, "თავისუფალი საქართველო") ~ "20 თავისუფალი საქართველო",
      str_detect(box_party_name, "კოალიცია ცვლილებებისთვის") ~ "4 კოალიცია ცვლილებებისთვის",
      str_detect(box_party_name, "კოალიცია ცვლილებისთვის გვარამია მელია გირჩი დროა 4") ~ "4 კოალიცია ცვლილებებისთვის",
      str_detect(box_party_name, "ტრიბუნა") ~ "21 ტრიბუნა",
      str_detect(box_party_name, "ლეიბორ") ~ "10 საქართველოს ლეიბორისტული პარტია",
      str_detect(box_party_name, "პატრიოტ") ~ "8 საქართველოს პატრიოტთა ალიანსი",
      str_detect(box_party_name, " ერთობა") ~ "27 ქართველ ერთობა",
      box_party_name == "ჩვენ" ~ "23 ჩვენ",
      box_party_name == "საქართველო" ~ "17 საქართველო",
      box_party_name == "კოალიცია ცვლილებისთვის" ~ "4 კოალიცია ცვლილებებისთვის",
      T ~ as.character(box_party_name)
    )
  ) |> 
  separate(
    box_party_name, into = c("box_party_name"), sep = " |\\."
  ) |> 
  select(
    c(box_party_name, box_party_fixed, `_parent_index`)
  ) |>
  mutate(
    box_party_name = paste0("box_p_", box_party_name)
  ) |> 
  pivot_wider(
    names_from = box_party_name, values_from = box_party_fixed
  ) |> 
  rename(
    "_index" = "_parent_index"
  )




duplicate_documents <- c(
  "uuid:e6c6713c-7229-40fb-baa1-3a39afb2defe", "uuid:7362f950-e997-4b01-8fd4-12f14b503367", "uuid:9d35f520-3f2f-4a2f-9bc5-2995c175c4dc", "uuid:23e0197b-8bd5-46ac-b1ad-6ae167e71b82", "uuid:12523a70-d5fc-4297-ae3a-ef43f5a18bec", "uuid:a7ce56e5-448a-442b-ae51-9193a26a0631", "uuid:3997fcba-469e-444c-b95b-fae4bb052823", "uuid:ec63ef7e-40d1-4deb-99ad-c72543fe98c7", "uuid:79f90c7f-b75c-45b5-ad57-6e5dea5cd9ea", "uuid:0db2f62e-f71e-4414-b1ef-5a6aa24b9fd0", "uuid:8f64e7c6-83c5-4ebe-922a-debb4ec75e1f", "uuid:3268b61d-e79b-4079-a500-621fa293c18b", "uuid:1940f464-8aac-42ba-8001-8be746ef5bef", "uuid:aa7c0008-2b45-4680-ad7c-da42f175bd81", "uuid:c3f5e34c-a0a8-4b3f-addc-8629de8b003d", "uuid:4c7bade4-a753-45be-b3e3-b15e31c7ad0a", "uuid:c0a8d1c4-1945-4ea0-9db0-a0642bffad0b", "uuid:314efa1d-6ca5-46c9-8985-eaa7cbc73aa2", "uuid:e48977e8-9574-4597-99aa-b95ec4e91aa3", "uuid:3047999c-101d-48e7-a91b-e4f3929d2262", "uuid:e2b3e5e1-6e5d-485f-b871-be05f1aea579", "uuid:6692b14a-3a09-4332-a398-88565a81b4fd", "uuid:1521db90-a4d7-44da-a50e-c9ee3cd6c260", "uuid:71b968c2-51a6-4967-958a-e9da9718745b", "uuid:71b968c2-51a6-4967-958a-e9da9718745b", "uuid:352c71b6-4c7f-4a82-98de-aae33678d34f", "uuid:460f3464-e38f-42d0-a3d9-881b09588e61", "uuid:678507db-e4fe-46be-b977-aa6913b5c4f5", "uuid:04a0c21b-0fdd-4e7f-be26-b5267e1f80f8", "uuid:d5d69d84-2cfe-4238-9bb9-f6100faa692b", "uuid:fb5d7046-2e05-43c2-a6df-1b719cd8b94a", "uuid:3a2bbac8-62c1-4f50-a586-9a0fc3bbe198", "uuid:127d6750-2449-4d6b-b035-c16e6c3e79e0", "uuid:0c0833eb-92ec-4282-9fff-a7fd412dc0a7", "uuid:a1ad1080-9fac-4476-9020-1d6a0d569091", "uuid:0c08a7e7-b179-4ae1-a323-a999b6ca7b2a", "uuid:12e6dbaf-1ee0-4a54-b1c7-6debbb8d2388", "uuid:3a99f2d4-8160-4141-94d4-7d80ef66beeb", "uuid:337e552e-fcb3-4878-86c6-4657cc304a72"
)

main_data |> 
  # dplyr::filter(document_type == "1", (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved") ) |> 
  mutate(
    approved_result_protocol = case_when(
      (`_validation_status` == "validation_status_not_approved") ~ 0,
      instanceID %in% c(duplicate_documents) ~ 0,
      T ~ 1
    ),
    district = as.numeric(district),
    precinct = as.numeric(precinct),
    prec_id = as.numeric(district)*1000+as.numeric(precinct)
  ) -> only_approved


only_approved |> 
  filter(
    approved_result_protocol == 0
  ) |> 
  select(
    district, precinct, prec_id
  ) |> distinct() -> duplicates

only_approved |>
  filter(
    approved_result_protocol == 1
  ) |>
  select(
    district, precinct, prec_id, document_type, approved_result_protocol
  ) |>
  pivot_wider(names_from = document_type, values_from = approved_result_protocol, values_fn  = sum, values_fill = 0) |>
  arrange(
    prec_id
  ) %>%
  mutate(
    n_docs = rowSums(select(., -c(district, precinct, prec_id)), na.rm = TRUE)
  ) |>
  distinct() -> number_of_documents


sum(only_approved$f_06)/sum(only_approved$f_05, na.rm= T)

all_precincts <- readxl::read_excel("../data_cleaning/georgia_parliamentary_2024.xlsx")

nrow(all_precincts)



all_precincts |> 
  select(
    district_name, district, precinct
  ) |> 
  left_join(only_approved, by = c("district", "precinct")) |> 
  mutate(
    digitized = ifelse(!is.na(document_type), "Digitized", "Not digitized")
  ) |> 
  # group_by(
  #   district_name, district, precinct, digitized
  # ) |>
  group_by(digitized) |>
  count()
  # arrange(district, precinct) |>
  # # filter(district == "Not ") |> print(n = Inf)
  # # filter(digitized == "Not digitized") |> 
  # # group_by(district) |> 
  # count() |> 
  # print(n = Inf)

all_precincts |> 
  select(
    district_name, district, precinct
  ) |> 
  left_join(only_approved, by = c("district", "precinct")) |>
  mutate(
    digitized = ifelse(!is.na(document_type), "Digitized", "Not digitized")
  ) |> 
  # group_by(
  #   district_name, district, precinct, digitized
  # ) |>
#   group_by(digitized) |>
#   count()
# arrange(district, precinct) |>
  # filter(district == "Not ") |> print(n = Inf)
  # filter(digitized == "Not digitized") |>
  group_by(district, digitized) |>
  count() |> 
  pivot_wider(names_from = digitized, values_from = n, values_fill = 0) |>
  print(n = Inf)

all_precincts |> 
  select(
    district_name, district, precinct
  ) |> 
  left_join(only_approved, by = c("district", "precinct")) |>
  mutate(
    digitized = ifelse(!is.na(document_type), "Digitized", "Not digitized")
  ) |> 
  filter(district == 87) |> 
  select(district_name, district, precinct, digitized) |> 
  print(n = Inf)
    
# უბნის კოდებში არ უნდა იყოს დუბლირებები
# უბნები არ უნდა იყოს გამორჩენილი
# პარტიების ჯამი უნდა ემთხვეოდეს ბათილს + ნამდვილი
# აქტივობა უნდა იყოს მეტი 12 და 17 საათზე აქტივობის
# მისული ნაკლები უნდა იყოს ამომრ სია + სპეცსია
# კომენტარები რაც აქვს, ყველაფერი უნდა იყოს საბოლოო ბაზაში, მათ შორის - ამოღებული ჩანაწერებიდან


names(only_approved)


table(only_approved[only_approved$approved_result_protocol== 1, ]$document_type)
table(only_approved$district)
table(only_approved$precinct)
only_approved |> group_by(code) |> count() |> arrange(desc(n))
only_approved |> filter(
  document_type == "1", (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved") 
) |>  group_by(code) |> count() |> arrange(desc(n))


# only_approved |> group_by(attached) |> count() |> arrange(desc(n)) |> View()
# only_approved |> group_by(f_01) |> count() |> arrange(desc(n)) |> View()
# only_approved |> group_by(f_02) |> count() |> arrange(desc(n)) |> View()
only_approved |> filter(f_02 == "0")
only_approved |> filter(f_02 == "50")

# 12:00 an 17:00 აქტივობა მეტია ან უდრის ერთმანეთს
only_approved |>
  filter(document_type == "1", (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved")) |> 
  filter(f_04a >= f_04b) |>
  select(district, precinct, f_03, f_04a, f_04b, f_05) |> 
  arrange(
    district, precinct
  )

# 17:00 აქტივობა მეტია ან უდრის 20 საათის აქტივობას
only_approved |>
  filter(document_type == "1", (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved")) |> 
  filter(f_04b >= f_05) |>
  select(district, precinct, f_03, f_04a, f_04b, f_05) |> 
  arrange(
    district, precinct
  )

# პარტიების ჯამი უნდა იყოს მონაწილეებს მინუს ბათილი

only_approved %>%
  mutate(
    valid = f_05 - f_06,
    total = rowSums(select(., starts_with("p_")), na.rm = T)
  ) |> 
  filter(valid != total) |> 
  select(district, precinct, f_05, f_06, valid, total, p_3:p_41) |> 
  arrange(
    district, precinct
  ) |> 
  print(n = Inf)

## მონაცემების რედაქტირება

## შესწორებები

only_approved |>
  filter(
  document_type == "2" & (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved")
  ) |> 
  left_join(
    amend_data, by = "_index"
  ) |>
  filter(instanceID != "uuid:1521db90-a4d7-44da-a50e-c9ee3cd6c260") |> 
  select(district, precinct, amendment_001:opinion_a, amend_p_4:amend_p_12) |>
  distinct() -> amendments

## გადათვლები

only_approved |>
  filter(
    document_type == "3", (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved")
  ) |> 
  select(district, precinct, recount_reason:recount_f_06) |>
  distinct() |> 
  group_by(district, precinct) |>
  mutate(
    ## add count by group
    dups = n() > 1
  ) |> 
  filter(
    dups == FALSE
  ) |> 
  select(-dups) -> recounts

## ყუთი

only_approved |>
  filter(
    document_type == "2" & (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved")
  ) |>
  left_join(
    box_data, by = "_index"
  ) |> 
  # filter(instanceID != "uuid:1521db90-a4d7-44da-a50e-c9ee3cd6c260") |> 
  select(district, precinct, box_5_should_be, box_6_should_be, box_p_4:box_p_3) |>
  distinct() -> box


# აქ გვჭირდება გადათვლის და შესწორების ოქმების მონაცემების ავტომატურად ჩანაცვლება



only_approved |>
  filter(
    document_type == "1", (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved")
  ) |> 
  select(
    -c(
      amendment_001:opinion_a, recount_reason:recount_f_06, box_5_should_be, box_6_should_be
    )
  ) |> 
  left_join(
    amendments, by = c("district", "precinct")
  ) |> 
  left_join(
    recounts, by = c("district", "precinct")
  ) |> 
  left_join(
    box, by = c("district", "precinct")
  ) |> 
  mutate(
    attached = case_when(
      attached == "0" ~ NA_character_,
      T ~ as.character(attached)
    ),
    f_01 = case_when(
      district == 3 & precinct == 85 ~ 1369,
      district == 5 & precinct == 52 ~ 2979,
      district == 6 & precinct == 52 ~ 2943,
      district == 6 & precinct == 56 ~ 1491,
      district == 9 & precinct == 81 ~ 2614,
      district == 10 & precinct == 27 ~ 2596,
      district == 10 & precinct == 35 ~ 1494,
      district == 10 & precinct == 58 ~ 965,
      district == 14 & precinct == 19 ~ 483,
      district == 22 & precinct == 45 ~ 346,
      district == 28 & precinct == 45 ~ 526,
      district == 30 & precinct == 5 ~ 2186,
      district == 33 & precinct == 19 ~ 2378,
      district == 44 & precinct == 4 ~ 491,
      district == 48 & precinct == 28 ~ 619,
      district == 59 & precinct == 51 ~ 1236,
      district == 60 & precinct == 37 ~ 1312,
      district == 60 & precinct == 46 ~ 0,
      district == 61 & precinct == 14 ~ 806,
      district == 61 & precinct == 22 ~ 918,
      district == 63 & precinct == 19 ~ 485,
      district == 64 & precinct == 31 ~ 647,
      district == 81 & precinct == 12 ~ 1045,
      district == 81 & precinct == 48 ~ 976,
      district == 84 & precinct == 1 ~ 1614,
      district == 87 & precinct == 65 ~ 0,
      amend_1_should_be != "" & !is.na(amend_1_should_be) & f_01 != amend_1_should_be ~ amend_1_should_be,
      T ~ as.double(f_01)
    ),
    f_02 = case_when(
      amend_2_should_be != "" & !is.na(amend_2_should_be) & as.character(f_02) != as.character(amend_2_should_be) ~ as.character(amend_2_should_be),
      f_02 == "0" ~ NA_character_, # ამ უბანზე ოქმს ჩამოჭრილი აქვს შესაბამისი ნაწილი
      T ~ as.character(f_02)
    ),
    f_03 = case_when(
      amend_3_should_be != "" & !is.na(amend_3_should_be) & f_03 != amend_3_should_be ~ amend_3_should_be,
      T ~ as.double(f_03)
    ),
    f_04a = case_when(
      district == "55" & precinct == "23" & document_type == "1" ~ 35,
      district == "3" & precinct == "66" & document_type == "1" ~ 345,
      district == "37" & precinct == "23" & document_type == "1" ~ 211,
      district == "49" & precinct == "20" & document_type == "1" ~ 320,
      district == "87" & precinct == "66" & document_type == "1" ~ 150, # შეყვანის შეცდომა
      amend_4a_should_be != "" & !is.na(amend_4a_should_be) & f_04a != amend_4a_should_be ~ amend_4a_should_be,
      T ~ as.numeric(f_04a)
    ),
    f_04b = case_when(
      district == "55" & precinct == "23"  & document_type == "1" ~ 48,
      district == "3" & precinct == "66"  & document_type == "1" ~ 808,
      district == "48" & precinct == "16"  & document_type == "1" ~ 87,
      district == "59" & precinct == "62"  & document_type == "1" ~ 1046,
      district == "87" & precinct == "38"  & document_type == "1" ~ 651,
      district == "10" & precinct == "28"  & document_type == "1" ~ 1310, # შეყვანის შეცდომა (გაურკვევლად წერია და დიდი ალბათობით, 3-ია)
      district == "3" & precinct == "7"  & document_type == "1" ~ 857, # გვაქვს შესწორების ოქმი
      district == "87" & precinct == "66" & document_type == "1" ~ 310, # შეყვანის შეცდომა
      amend_4b_should_be!="" & !is.na(amend_4b_should_be) & f_04b != amend_4b_should_be ~ amend_4b_should_be,
      T ~ as.double(f_04b)
    ),
    f_05 = case_when(
      district == 82 & precinct == 40  & document_type == "1" ~ 178, # შესწორების ოქმი
      district == 81 & precinct == 43  & document_type == "1" ~ 1074, # შესწორების ოქმი
      district == 81 & precinct == 42  & document_type == "1" ~ 1235, # შესწორების ოქმი
      district == 80 & precinct == 294  & document_type == "1" ~ 368, # შესწორების ოქმი
      district == 79 & precinct == 54  & document_type == "1" ~ 1061, # შესწორების ოქმი
      district == 68 & precinct == 8  & document_type == "1" ~ 472, # შესწორების ოქმი
      district == 67 & precinct == 23  & document_type == "1" ~ 1272, # შესწორების ოქმი
      district == 67 & precinct == 13  & document_type == "1" ~ 789, # შესწორების ოქმი
      district == 65 & precinct == 28  & document_type == "1" ~ 479, # შესწორების ოქმი
      district == 59 & precinct == 66  & document_type == "1" ~ 844, # შესწორების ოქმი
      district == 59 & precinct == 8  & document_type == "1" ~ 1550, # შესწორების ოქმი
      district == 58 & precinct == 20  & document_type == "1" ~ 250, # შესწორების ოქმი
      district == 55 & precinct == 20  & document_type == "1" ~ 681, # შესწორების ოქმი
      district == 55 & precinct == 4  & document_type == "1" ~ 744, # შესწორების ოქმი
      district == 55 & precinct == 1  & document_type == "1" ~ 818, # შესწორების ოქმი
      district == 52 & precinct == 2  & document_type == "1" ~ 590, # შესწორების ოქმი
      district == 46 & precinct == 11  & document_type == "1" ~ 117, # შესწორების ოქმი
      district == 43 & precinct == 6  & document_type == "1" ~ 97, # შესწორების ოქმი
      district == 40 & precinct == 41  & document_type == "1" ~ 166, # შესწორების ოქმი
      district == 38 & precinct == 5  & document_type == "1" ~ 699, # შესწორების ოქმი
      district == 33 & precinct == 34  & document_type == "1" ~ 896, # შესწორების ოქმი
      district == 32 & precinct == 56  & document_type == "1" ~ 949, # შესწორების ოქმი
      district == 30 & precinct == 18  & document_type == "1" ~ 412, # შესწორების ოქმი
      district == 27 & precinct == 18  & document_type == "1" ~ 843, # შესწორების ოქმი
      district == 23 & precinct == 2  & document_type == "1" ~ 1526, # შესწორების ოქმი
      district == 23 & precinct == 1  & document_type == "1" ~ 1258, # შესწორების ოქმი
      district == 22 & precinct == 49  & document_type == "1" ~ 587, # შესწორების ოქმი
      district == 22 & precinct == 25  & document_type == "1" ~ 444, # შესწორების ოქმი
      district == 22 & precinct == 23  & document_type == "1" ~ 424, # შესწორების ოქმი
      district == 22 & precinct == 7  & document_type == "1" ~ 1772, # შესწორების ოქმი
      district == 12 & precinct == 26  & document_type == "1" ~ 657, # შესწორების ოქმი
      district == 12 & precinct == 28  & document_type == "1" ~ 896, # შესწორების ოქმი
      district == 10 & precinct == 59  & document_type == "1" ~ 775, # შესწორების ოქმი
      district == 10 & precinct == 25  & document_type == "1" ~ 790, # შესწორების ოქმი
      district == 10 & precinct == 18  & document_type == "1" ~ 1233, # შესწორების ოქმი
      district == 10 & precinct == 4  & document_type == "1" ~ 1282, # შესწორების ოქმი
      district == 9 & precinct == 68  & document_type == "1" ~ 875, # შესწორების ოქმი
      district == 9 & precinct == 67  & document_type == "1" ~ 873, # შესწორების ოქმი
      district == 9 & precinct == 63  & document_type == "1" ~ 871, # შესწორების ოქმი
      district == 9 & precinct == 57  & document_type == "1" ~ 886, # შესწორების ოქმი
      district == 9 & precinct == 48  & document_type == "1" ~ 885, # შესწორების ოქმი
      district == 9 & precinct == 39  & document_type == "1" ~ 1530, # შესწორების ოქმი
      district == 9 & precinct == 36  & document_type == "1" ~ 938, # შესწორების ოქმი
      district == 9 & precinct == 32  & document_type == "1" ~ 836, # შესწორების ოქმი
      district == 9 & precinct == 9  & document_type == "1" ~ 864, # შესწორების ოქმი
      district == 9 & precinct == 6  & document_type == "1" ~ 758, # შესწორების ოქმი
      district == 7 & precinct == 37  & document_type == "1" ~ 850, # შესწორების ოქმი
      district == 7 & precinct == 18  & document_type == "1" ~ 1008, # შესწორების ოქმი (ყუთის)
      district == 7 & precinct == 6  & document_type == "1" ~ 854, # შესწორების ოქმი
      district == 6 & precinct == 69  & document_type == "1" ~ 593, # შესწორების ოქმი
      district == 6 & precinct == 46  & document_type == "1" ~ 649, # შესწორების ოქმი
      district == 6 & precinct == 4  & document_type == "1" ~ 1363, # შესწორების ოქმი
      district == 5 & precinct == 39  & document_type == "1" ~ 1290, # გადათვლის ოქმი
      district == 4 & precinct == 5  & document_type == "1" ~ 1276, # შესწორების ოქმი
      district == 32 & precinct == 15  & document_type == "1" ~ 1370, # შესწორების ოქმი
      district == 3 & precinct == 89  & document_type == "1" ~ 1734, # შესწორების ოქმი
      district == 3 & precinct == 47  & document_type == "1" ~ 1007, # შესწორების ოქმი
      district == 3 & precinct == 18  & document_type == "1" ~ 948, # შესწორების ოქმი
      district == 3 & precinct == 11  & document_type == "1" ~ 846, # შესწორების ოქმი
      district == 3 & precinct == 10  & document_type == "1" ~ 1000, # შესწორების ოქმი
      district == 3 & precinct == 9  & document_type == "1" ~ 867, # შესწორების ოქმი
      district == 2 & precinct == 62  & document_type == "1" ~ 1772, # შესწორების ოქმი
      district == 2 & precinct == 46  & document_type == "1" ~ 1157, # შესწორების ოქმი
      district == 2 & precinct == 21  & document_type == "1" ~ 1124, # შესწორების ოქმი
      district == 2 & precinct == 15  & document_type == "1" ~ 1277, # შესწორების ოქმი
      district == 2 & precinct == 3  & document_type == "1" ~ 1153, # შესწორების ოქმი
      district == 47 & precinct == 20  & document_type == "1" ~ 327, # შეყვანის შეცდომა
      district == 83 & precinct == 21  & document_type == "1" ~ 873, # შეყვანის შეცდომა
      district == 79 & precinct == 46  & document_type == "1" ~ 1262, # შესწორების ოქმი
      district == 65 & precinct == 28  & document_type == "1" ~ 479, # გადათვლის ოქმი
      district == 62 & precinct == 2  & document_type == "1" ~ 456, # შეყვანის შეცდომა
      district == 61 & precinct == 22  & document_type == "1" ~ 585, # გვაქვს შესწორების ოქმი
      district == 60 & precinct == 43  & document_type == "1" ~ 510, # გვაქვს შესწორების ოქმი
      district == 60 & precinct == 3  & document_type == "1" ~ 1276, # გვაქვს შესწორების ოქმი და კომისიის თავ-რეს გამოეცხადა გაფრთხილება
      district == 59 & precinct == 51  & document_type == "1" ~ 661, # გვაქვს შესწორების ოქმი
      district == 59 & precinct == 18  & document_type == "1" ~ 1148, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 58 & precinct == 3  & document_type == "1" ~ 1271, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 54 & precinct == 25  & document_type == "1" ~ 1248, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 48 & precinct == 36  & document_type == "1" ~ 144, # გვაქვს გადათვლის ოქმი
      district == 48 & precinct == 27  & document_type == "1" ~ 458, # შეყვანის შეცდომა
      district == 48 & precinct == 23  & document_type == "1" ~ 341, # შეყვანის შეცდომა
      district == 27 & precinct == 40  & document_type == "1" ~ 365, # არის ახსნა-განმარტება, რომ გადათვლის შემდეგ შეეშალათ
      district == 27 & precinct == 2  & document_type == "1" ~ 1343, # შეყვანის შეცდომა
      district == 24 & precinct == 35  & document_type == "1" ~ 132, # შეყვანის შეცდომა
      district == 22 & precinct == 42  & document_type == "1" ~ 720, # გვაქვს ახსნა-განმარტება, თუმცა შესწორების ოქმი არაა ატვირთული
      district == 11 & precinct == 28  & document_type == "1" ~ 355, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 918, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 7  & document_type == "1" ~ 1484, # გვაქვს შესწორების ოქმი
      district == "9" & precinct == "81"  & document_type == "1" ~ 1525, # გვაქვს შესწორების ოქმი
      district == "9" & precinct == "26"  & document_type == "1" ~ 863, # გვაქვს შესწორების ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 1561, # გვაქვს შესწორების ოქმი
      district == "55" & precinct == "23" & document_type == "1" ~ 50, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "66" & document_type == "1" ~ 943, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 1088, # გვაქვს შესწორების ოქმი
      district == "1" & precinct == "13" & document_type == "1" ~ 782,  # გვაქვს შესწორების ოქმი
      district == "2" & precinct == "1" & document_type == "1" ~ 1124,  # გვაქვს შესწორების ოქმი
      district == "2" & precinct == "68" & document_type == "1" ~ 997,  # გვაქვს შესწორების ოქმი
      district == "2" & precinct == "58" & document_type == "1" ~ 1163,  # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "7"  & document_type == "1" ~ 984, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "34"  & document_type == "1" ~ 969, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "60"  & document_type == "1" ~ 885, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "72"  & document_type == "1" ~ 2241, # გვაქვს შესწორების ოქმი
      district == 5 & precinct == 1  & document_type == "1" ~ 1561, # შეყვანის შეცდომა
      district == 5 & precinct == 35  & document_type == "1" ~ 447, # გვაქვს შესწორების ოქმი
      district == 5 & precinct == 55  & document_type == "1" ~ 1397, # გვაქვს გადათვლის ოქმი
      district == 7 & precinct == 27  & document_type == "1" ~ 1253, # გვაქვს გადათვლის ოქმი
      district == 21 & precinct == 32  & document_type == "1" ~ 629, # გვაქვს გადათვლის ოქმი (არ იყო ხელმოწერები შეყვანილი)
      district == 22 & precinct == 25  & document_type == "1" ~ 444, # გვაქვს გადათვლის ოქმი (არ იყო ხელმოწერები შეყვანილი)
      district == 32 & precinct == 15  & document_type == "1" ~ 1393, # გვაქვს შესწორების ოქმი
      district == 33 & precinct == 23  & document_type == "1" ~ 279, # შეყვანის შეცდომა
      district == 40 & precinct == 18  & document_type == "1" ~ 191, # გვაქვს შესწორების ოქმი
      district == 57 & precinct == 13  & document_type == "1" ~ 183, # გვაქვს გადათვლის ოქმი (არ იყო ხელმოწერები შეყვანილი)
      district == 58 & precinct == 41  & document_type == "1" ~ 69, # გვაქვს გადათვლის ოქმი (არ იყო ხელმოწერები შეყვანილი)
      district == 60 & precinct == 30  & document_type == "1" ~ 531, # შეყვანის შეცდომა
      district == 62 & precinct == 6  & document_type == "1" ~ 572, # გვაქვს შესწორების ოქმი
      district == "87" & precinct == "66" & document_type == "1" ~ 398, # შეყვანის შეცდომა
      district == 6 & precinct == 2 & document_type == "1" ~ 1367, # გვაქვს გადათვლის ოქმი
      district == 6 & precinct == 32 & document_type == "1" ~ 1437, # გვაქვს გადათვლის ოქმი
      district == 6 & precinct == 58 & document_type == "1" ~ 1395, # გვაქვს გადათვლის ოქმი
      district == "7" & precinct == "4" & document_type == "1" ~ 936, # გვაქვს გადათვლის ოქმი
      district == "7" & precinct == "23" & document_type == "1" ~ 1625, # გვაქვს გადათვლის ოქმი
      amend_5_should_be != "" & !is.na(amend_5_should_be) & f_05 != amend_5_should_be ~ amend_5_should_be,
      !is.na(recount_f05) & recount_f05 != f_05 ~ recount_f05, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_5_should_be) & box_5_should_be != f_05 ~ box_5_should_be, # ყუთის შედეგები, თუკი განსხვავებულია

      T ~ as.double(f_05)
    ),
    p_3 = case_when(
      district == 65 & precinct == 5  & document_type == "1" ~ 0, # შესწორების ოქმი
      district == 62 & precinct == 4  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 60 & precinct == 38  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 9 & precinct == 63  & document_type == "1" ~ 3, # შესწორების ოქმი
      district == 80 & precinct == 15  & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == 55 & precinct == 12  & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == 47 & precinct == 1  & document_type == "1" ~ 3, # შეყვანის შეცდომა
      district == 22 & precinct == 25  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 20 & precinct == 45  & document_type == "1" ~ 2, # გვაქვს გადათვლის და შესწორების ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 0, # გვაქვს გადათვლის ოქმი
      district == "9" & precinct == "26"  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == "8" & precinct == "27"  & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "61" & document_type == "1" ~ 2, # გვაქვს შესწორების ოქმი
      amend_p_3 != "" & !is.na(amend_p_3) & amend_p_3 != p_3 ~ amend_p_3, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_3) & recount_p_3 != p_3 ~ recount_p_3, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_3) & box_p_3 != p_3 ~ box_p_3, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_3)
    ),
    p_4 = case_when(
      district == 81 & precinct == 42  & document_type == "1" ~ 87, # შესწორების ოქმი
      district == 80 & precinct == 29  & document_type == "1" ~ 23, # შესწორების ოქმი
      district == 70 & precinct == 17  & document_type == "1" ~ 106, # შესწორების ოქმი
      district == 67 & precinct == 13  & document_type == "1" ~ 156, # შესწორების ოქმი
      district == 62 & precinct == 4  & document_type == "1" ~ 38, # შესწორების ოქმი
      district == 59 & precinct == 8  & document_type == "1" ~ 215, # შესწორების ოქმი
      district == 58 & precinct == 20  & document_type == "1" ~ 22, # შესწორების ოქმი
      district == 55 & precinct == 4  & document_type == "1" ~ 81, # შესწორების ოქმი
      district == 49 & precinct == 23  & document_type == "1" ~ 57, # შესწორების ოქმი
      district == 46 & precinct == 11  & document_type == "1" ~ 2, # შესწორების ოქმი
      district == 43 & precinct == 22  & document_type == "1" ~ 12, # შესწორების ოქმი
      district == 43 & precinct == 6  & document_type == "1" ~ 24, # შესწორების ოქმი
      district == 40 & precinct == 41  & document_type == "1" ~ 5, # შესწორების ოქმი
      district == 38 & precinct == 5  & document_type == "1" ~ 24, # შესწორების ოქმი
      district == 32 & precinct == 56  & document_type == "1" ~ 64, # შესწორების ოქმი
      district == 27 & precinct == 18  & document_type == "1" ~ 88, # შესწორების ოქმი
      district == 22 & precinct == 7  & document_type == "1" ~ 15, # შესწორების ოქმი
      district == 20 & precinct == 38  & document_type == "1" ~ 183, # შესწორების ოქმი
      district == 17 & precinct == 43  & document_type == "1" ~ 28, # შესწორების ოქმი
      district == 14 & precinct == 18  & document_type == "1" ~ 35, # შესწორების ოქმი
      district == 10 & precinct == 59  & document_type == "1" ~ 126, # შესწორების ოქმი
      district == 10 & precinct == 25  & document_type == "1" ~ 126, # შესწორების ოქმი
      district == 10 & precinct == 18  & document_type == "1" ~ 158, # შესწორების ოქმი
      district == 10 & precinct == 4  & document_type == "1" ~ 171, # შესწორების ოქმი
      district == 9 & precinct == 68  & document_type == "1" ~ 151, # შესწორების ოქმი
      district == 9 & precinct == 67  & document_type == "1" ~ 125, # შესწორების ოქმი
      district == 9 & precinct == 63  & document_type == "1" ~ 130, # შესწორების ოქმი
      district == 9 & precinct == 57  & document_type == "1" ~ 121, # შესწორების ოქმი
      district == 9 & precinct == 48  & document_type == "1" ~ 103, # შესწორების ოქმი
      district == 9 & precinct == 39  & document_type == "1" ~ 185, # შესწორების ოქმი
      district == 9 & precinct == 36  & document_type == "1" ~ 124, # შესწორების ოქმი
      district == 7 & precinct == 18  & document_type == "1" ~ 119, # შესწორების ოქმი
      district == 7 & precinct == 6  & document_type == "1" ~ 93, # შესწორების ოქმი
      !is.na(box_p_4) & box_p_4 != p_4 ~ box_p_4, # ყუთის შედეგები, თუკი განსხვავებულია
      district == 5 & precinct == 39  & document_type == "1" ~ 193, # გადათვლის ოქმი
      district == 3 & precinct == 47  & document_type == "1" ~ 163, # შესწორების ოქმი
      district == 3 & precinct == 18  & document_type == "1" ~ 169, # შესწორების ოქმი
      district == 3 & precinct == 9  & document_type == "1" ~ 147, # შესწორების ოქმი
      district == 2 & precinct == 62  & document_type == "1" ~ 304, # შესწორების ოქმი
      district == 2 & precinct == 15  & document_type == "1" ~ 192, # შესწორების ოქმი
      district == 83 & precinct == 41  & document_type == "1" ~ 29, # შეყვანის შეცდომა
      district == 80 & precinct == 15  & document_type == "1" ~ 15, # გვაქვს შესწორების ოქმი
      district == 65 & precinct == 28  & document_type == "1" ~ 66, # გადათვლის ოქმი
      district == 59 & precinct == 18  & document_type == "1" ~ 176, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 58 & precinct == 7  & document_type == "1" ~ 100, # გვაქვს გადათვლის ოქმი
      district == 58 & precinct == 3  & document_type == "1" ~ 164, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 54 & precinct == 25  & document_type == "1" ~ 70, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 22 & precinct == 61  & document_type == "1" ~ 21, # შეყვანის შეცდომა
      district == 22 & precinct == 25  & document_type == "1" ~ 9, # შეყვანის შეცდომა
      district == 11 & precinct == 39  & document_type == "1" ~ 9, # გვაქვს გადათვლის ოქმი
      district == 11 & precinct == 22  & document_type == "1" ~ 5, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 159, # გვაქვს გადათვლის ოქმი
      district == "9" & precinct == "15"  & document_type == "1" ~ 89, # შეყვანის შეცდომა
      district == "9" & precinct == "26"  & document_type == "1" ~ 101, # გვაქვს შესწორების ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 251, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 149, # გვაქვს შესწორების ოქმი
      district == "2" & precinct == "58" & document_type == "1" ~ 210, # გვაქვს შესწორების ოქმი
      district == "6" & precinct == "63" & document_type == "1" ~ 164, # გვაქვს შესწორების ოქმი
      district == "7" & precinct == "23" & document_type == "1" ~ 249, # გვაქვს გადათვლის ოქმი
      !is.na(amend_p_4) & amend_p_4 != p_4 ~ amend_p_4, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_4) & recount_p_4 != p_4 ~ recount_p_4, # გადათვლის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_4)
    ),
    p_5 = case_when(
      district == 81 & precinct == 42  & document_type == "1" ~ 196, # შესწორების ოქმი
      district == 80 & precinct == 29  & document_type == "1" ~ 35, # შესწორების ოქმი
      district == 70 & precinct == 17  & document_type == "1" ~ 66, # შესწორების ოქმი
      district == 68 & precinct == 8  & document_type == "1" ~ 90, # შესწორების ოქმი
      district == 67 & precinct == 23  & document_type == "1" ~ 158, # შესწორების ოქმი
      district == 62 & precinct == 4  & document_type == "1" ~ 16, # შესწორების ოქმი
      district == 61 & precinct == 2  & document_type == "1" ~ 103, # შესწორების ოქმი
      district == 59 & precinct == 8  & document_type == "1" ~ 169, # შესწორების ოქმი
      district == 58 & precinct == 20  & document_type == "1" ~ 33, # შესწორების ოქმი
      district == 57 & precinct == 2  & document_type == "1" ~ 26, # შესწორების ოქმი
      district == 55 & precinct == 20  & document_type == "1" ~ 81, # შესწორების ოქმი
      district == 55 & precinct == 4  & document_type == "1" ~ 70, # შესწორების ოქმი
      district == 52 & precinct == 2  & document_type == "1" ~ 78, # შესწორების ოქმი
      district == 40 & precinct == 41  & document_type == "1" ~ 2, # შესწორების ოქმი
      district == 33 & precinct == 34  & document_type == "1" ~ 99, # შესწორების ოქმი
      district == 32 & precinct == 56  & document_type == "1" ~ 133, # შესწორების ოქმი
      district == 30 & precinct == 23  & document_type == "1" ~ 65, # შესწორების ოქმი
      district == 27 & precinct == 18  & document_type == "1" ~ 101, # შესწორების ოქმი
      district == 23 & precinct == 2  & document_type == "1" ~ 104, # შესწორების ოქმი
      district == 22 & precinct == 23  & document_type == "1" ~ 145, # შესწორების ოქმი
      district == 21 & precinct == 16  & document_type == "1" ~ 121, # შესწორების ოქმი
      district == 17 & precinct == 43  & document_type == "1" ~ 82, # შესწორების ოქმი
      district == 16 & precinct == 24  & document_type == "1" ~ 109, # შესწორების ოქმი
      district == 12 & precinct == 28  & document_type == "1" ~ 108, # შესწორების ოქმი
      district == 12 & precinct == 26  & document_type == "1" ~ 36, # შესწორების ოქმი
      district == 10 & precinct == 59  & document_type == "1" ~ 64, # შესწორების ოქმი
      district == 10 & precinct == 25  & document_type == "1" ~ 54, # შესწორების ოქმი
      district == 10 & precinct == 4  & document_type == "1" ~ 86, # შესწორების ოქმი
      district == 9 & precinct == 57  & document_type == "1" ~ 46, # შესწორების ოქმი
      district == 9 & precinct == 48  & document_type == "1" ~ 68, # შესწორების ოქმი
      district == 9 & precinct == 36  & document_type == "1" ~ 72, # შესწორების ოქმი
      district == 9 & precinct == 9  & document_type == "1" ~ 56, # შესწორების ოქმი
      district == 7 & precinct == 18  & document_type == "1" ~ 65, # შესწორების ოქმი
      district == 6 & precinct == 4  & document_type == "1" ~ 128, # შესწორების ოქმი
      district == 4 & precinct == 5  & document_type == "1" ~ 111, # შესწორების ოქმი
      district == 3 & precinct == 11  & document_type == "1" ~ 48, # შესწორების ოქმი
      district == 2 & precinct == 62  & document_type == "1" ~ 112, # შესწორების ოქმი
      district == 2 & precinct == 21  & document_type == "1" ~ 74, # შესწორების ოქმი
      district == 2 & precinct == 15  & document_type == "1" ~ 98, # შესწორების ოქმი
      district == 2 & precinct == 3  & document_type == "1" ~ 79, # შესწორების ოქმი
      district == 84 & precinct == 35  & document_type == "1" ~ 35, # გვაქვს შესწორების ოქმი
      district == 80 & precinct == 15  & document_type == "1" ~ 59, # გვაქვს შესწორების ოქმი
      district == 65 & precinct == 28  & document_type == "1" ~ 33, # გადათვლის ოქმი
      district == 59 & precinct == 18  & document_type == "1" ~ 115, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 58 & precinct == 11  & document_type == "1" ~ 57, # შეყვანის შეცდომა (ცუდი ხარისხის ფოტოა)
      district == 58 & precinct == 7  & document_type == "1" ~ 48, # გვაქვს გადათვლის ოქმი
      district == 58 & precinct == 3  & document_type == "1" ~ 195, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 55 & precinct == 8  & document_type == "1" ~ 114, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 54 & precinct == 25  & document_type == "1" ~ 127, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 48 & precinct == 1  & document_type == "1" ~ 83, # გვაქვს შესწორების ოქმი
      district == 41 & precinct == 33  & document_type == "1" ~ 4, # გვაქვს შესწორების ოქმი
      district == 41 & precinct == 19  & document_type == "1" ~ 42, # გვაქვს შესწორების ოქმი
      district == 41 & precinct == 12  & document_type == "1" ~ 36, # შეყვანის შეცდომა
      district == 41 & precinct == 7  & document_type == "1" ~ 17, # შეყვანის შეცდომა
      district == 37 & precinct == 3  & document_type == "1" ~ 76, # გვაქვს შესწორების ოქმი
      district == 36 & precinct == 20  & document_type == "1" ~ 112, # გვაქვს შესწორების ოქმი
      district == 36 & precinct == 14  & document_type == "1" ~ 22, # გვაქვს შესწორების ოქმი
      district == 36 & precinct == 9  & document_type == "1" ~ 53, # გვაქვს შესწორების ოქმი
      district == 30 & precinct == 22  & document_type == "1" ~ 124, # გვაქვს შესწორების ოქმი
      district == 26 & precinct == 29  & document_type == "1" ~ 4, # შეყვანის შეცდომა
      district == 22 & precinct == 61  & document_type == "1" ~ 10, # შეყვანის შეცდომა
      district == 22 & precinct == 25  & document_type == "1" ~ 134, # შეყვანის შეცდომა
      district == 20 & precinct == 58  & document_type == "1" ~ 11, # გვაქვს გადათვლის ოქმი
      district == 12 & precinct == 29  & document_type == "1" ~ 47, # გვაქვს გადათვლის ოქმი
      district == 11 & precinct == 28  & document_type == "1" ~ 90, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 73, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 7  & document_type == "1" ~ 107, # გვაქვს შესწორების ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 92, # გვაქვს შესწორების ოქმი
      district == "9" & precinct == "18"  & document_type == "1" ~ 47, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 49, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "60" & document_type == "1" ~ 86, # გვაქვს შესწორების ოქმი
      district == "5" & precinct == "24" & document_type == "1" ~ 69, # შეყვანის შეცდომა
      district == "6" & precinct == "20" & document_type == "1" ~ 118, # შეყვანის შეცდომა
      district == "6" & precinct == "55" & document_type == "1" ~ 62, # შეყვანის შეცდომა
      district == "7" & precinct == "23" & document_type == "1" ~ 100, # გვაქვს გადათვლის ოქმი
      district == "8" & precinct == "16" & document_type == "1" ~ 70, # გვაქვს გადათვლის ოქმი
      !is.na(amend_p_4) & amend_p_4 != p_4 ~ amend_p_4, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_5) & recount_p_5 != p_5 ~ recount_p_5, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_5) & box_p_5 != p_5 ~ box_p_5, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_5)
    ),
    p_6 = case_when(
      district == 62 & precinct == 4  & document_type == "1" ~ 4, # შესწორების ოქმი
      district == 48 & precinct == 16  & document_type == "1" ~ 0, # შესწორების ოქმი
      district == 40 & precinct == 41  & document_type == "1" ~ 0, # შესწორების ოქმი
      district == 84 & precinct == 35  & document_type == "1" ~ 2, # გვაქვს შესწორების ოქმი
      district == 80 & precinct == 15  & document_type == "1" ~ 3, # გვაქვს შესწორების ოქმი
      district == 59 & precinct == 18  & document_type == "1" ~ 6, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 43 & precinct == 5  & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == 37 & precinct == 21  & document_type == "1" ~ 3, # შეყვანის შეცდომა
      district == 32 & precinct == 94  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 27 & precinct == 2  & document_type == "1" ~ 6, # შეყვანის შეცდომა
      district == 24 & precinct == 20  & document_type == "1" ~ 1, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 3, # გვაქვს გადათვლის ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 5, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "63" & document_type == "1" ~ 3, # გვაქვს შესწორების ოქმი
      district == "5" & precinct == "24" & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == "2" & precinct == "66" & document_type == "1" ~ 2, # გვაქვს შესწორების ოქმი
      !is.na(amend_p_6) & amend_p_6 != p_6 ~ amend_p_6, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_6) & recount_p_6 != p_6 ~ recount_p_6, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_6) & box_p_6 != p_6 ~ box_p_6, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_6)
    ),
    p_8 = case_when(
      district == 65 & precinct == 5  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 62 & precinct == 4  & document_type == "1" ~ 16, # შესწორების ოქმი
      district == 61 & precinct == 31  & document_type == "1" ~ 16, # შესწორების ოქმი
      district == 55 & precinct == 1  & document_type == "1" ~ 22, # შესწორების ოქმი
      district == 48 & precinct == 16  & document_type == "1" ~ 4, # შესწორების ოქმი
      district == 32 & precinct == 56  & document_type == "1" ~ 63, # შესწორების ოქმი
      district == 30 & precinct == 23  & document_type == "1" ~ 12, # შესწორების ოქმი
      district == 27 & precinct == 18  & document_type == "1" ~ 28, # შესწორების ოქმი
      district == 12 & precinct == 26  & document_type == "1" ~ 29, # შესწორების ოქმი
      district == 10 & precinct == 25  & document_type == "1" ~ 24, # შესწორების ოქმი
      district == 10 & precinct == 18  & document_type == "1" ~ 44, # შესწორების ოქმი
      district == 9 & precinct == 67  & document_type == "1" ~ 33, # შესწორების ოქმი
      district == 9 & precinct == 39  & document_type == "1" ~ 69, # შესწორების ოქმი
      district == 9 & precinct == 36  & document_type == "1" ~ 36, # შესწორების ოქმი
      district == 9 & precinct == 9  & document_type == "1" ~ 21, # შესწორების ოქმი
      district == 9 & precinct == 6  & document_type == "1" ~ 23, # შესწორების ოქმი
      district == 7 & precinct == 18  & document_type == "1" ~ 43, # შესწორების ოქმი
      district == 6 & precinct == 46  & document_type == "1" ~ 27, # შესწორების ოქმი
      district == 3 & precinct == 11  & document_type == "1" ~ 16, # შესწორების ოქმი
      district == 84 & precinct == 35  & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == 84 & precinct == 29  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 80 & precinct == 15  & document_type == "1" ~ 5, # გვაქვს შესწორების ოქმი
      district == 62 & precinct == 15  & document_type == "1" ~ 1, # შეყვანის შეცდომა (გაურკვევლად წერია)
      district == 58 & precinct == 11  & document_type == "1" ~ 26, # შეყვანის შეცდომა
      district == 43 & precinct == 5  & document_type == "1" ~ 3, # გვაქვს შესწორების ოქმი
      district == 30 & precinct == 21  & document_type == "1" ~ 13, # გვაქვს გადათვლის ოქმი, გადათვლისას შეეშალათ და დაწერეს ახსნა-განმარტება
      district == 24 & precinct == 20  & document_type == "1" ~ 3, # გვაქვს გადათვლის ოქმი
      district == 22 & precinct == 25  & document_type == "1" ~ 3, # შეყვანის შეცდომა
      district == 18 & precinct == 18  & document_type == "1" ~ 34, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 24, # გვაქვს გადათვლის ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 56, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 46, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "60" & document_type == "1" ~ 31, # გვაქვს შესწორების ოქმი
      district == "7" & precinct == "4" & document_type == "1" ~ 30, # გვაქვს გადათვლის ოქმი
      !is.na(amend_p_8) & amend_p_8 != p_8 ~ amend_p_8, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_8) & recount_p_8 != p_8 ~ recount_p_8, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_8) & box_p_8 != p_8 ~ box_p_8, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_8)
    ),
    p_9 = case_when(
      district == 81 & precinct == 42  & document_type == "1" ~ 66, # შესწორების ოქმი
      district == 67 & precinct == 23  & document_type == "1" ~ 59, # შესწორების ოქმი
      district == 62 & precinct == 4  & document_type == "1" ~ 39, # შესწორების ოქმი
      district == 61 & precinct == 31  & document_type == "1" ~ 75, # შესწორების ოქმი
      district == 61 & precinct == 2  & document_type == "1" ~ 108, # შესწორების ოქმი
      district == 59 & precinct == 66  & document_type == "1" ~ 78, # შესწორების ოქმი
      district == 58 & precinct == 20  & document_type == "1" ~ 20, # შესწორების ოქმი
      district == 55 & precinct == 20  & document_type == "1" ~ 47, # შესწორების ოქმი
      district == 55 & precinct == 9 & document_type == "1" ~ 10, # შესწორების ოქმი
      district == 55 & precinct == 1  & document_type == "1" ~ 46, # შესწორების ოქმი
      district == 52 & precinct == 2  & document_type == "1" ~ 42, # შესწორების ოქმი
      district == 48 & precinct == 16  & document_type == "1" ~ 11, # შესწორების ოქმი
      district == 43 & precinct == 6  & document_type == "1" ~ 5, # შესწორების ოქმი
      district == 23 & precinct == 2  & document_type == "1" ~ 86, # შესწორების ოქმი
      district == 22 & precinct == 25  & document_type == "1" ~ 20, # შესწორების ოქმი
      district == 10 & precinct == 25  & document_type == "1" ~ 83, # შესწორების ოქმი
      district == 10 & precinct == 18  & document_type == "1" ~ 137, # შესწორების ოქმი
      district == 10 & precinct == 4  & document_type == "1" ~ 117, # შესწორების ოქმი
      district == 9 & precinct == 68  & document_type == "1" ~ 100, # შესწორების ოქმი
      district == 9 & precinct == 67  & document_type == "1" ~ 109, # შესწორების ოქმი
      district == 9 & precinct == 63  & document_type == "1" ~ 91, # შესწორების ოქმი
      district == 9 & precinct == 57  & document_type == "1" ~ 114, # შესწორების ოქმი
      district == 9 & precinct == 32  & document_type == "1" ~ 120, # შესწორების ოქმი
      district == 9 & precinct == 9  & document_type == "1" ~ 101, # შესწორების ოქმი
      district == 9 & precinct == 6  & document_type == "1" ~ 94, # შესწორების ოქმი
      district == 7 & precinct == 6  & document_type == "1" ~ 118, # შესწორების ოქმი
      district == 6 & precinct == 46  & document_type == "1" ~ 54, # შესწორების ოქმი
      district == 5 & precinct == 39  & document_type == "1" ~ 150, # გადათვლის ოქმი
      district == 3 & precinct == 18  & document_type == "1" ~ 149, # შესწორების ოქმი
      district == 3 & precinct == 11  & document_type == "1" ~ 114, # შესწორების ოქმი
      district == 2 & precinct == 62  & document_type == "1" ~ 314, # შესწორების ოქმი
      district == 2 & precinct == 46  & document_type == "1" ~ 172, # შესწორების ოქმი
      district == 2 & precinct == 21  & document_type == "1" ~ 210, # შესწორების ოქმი
      district == 2 & precinct == 15  & document_type == "1" ~ 171, # შესწორების ოქმი
      district == 84 & precinct == 35  & document_type == "1" ~ 15, # გვაქვს შესწორების ოქმი
      district == 84 & precinct == 29  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 80 & precinct == 15  & document_type == "1" ~ 28, # გვაქვს შესწორების ოქმი
      district == 66 & precinct == 24  & document_type == "1" ~ 21, # შეყვანის შეცდომა (გადათვლის ოქმში გაურკვევლად წერია)
      district == 59 & precinct == 18  & document_type == "1" ~ 100, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 58 & precinct == 12  & document_type == "1" ~ 12, # შეყვანის შეცდომა
      district == 58 & precinct == 1  & document_type == "1" ~ 93, # შეყვანის შეცდომა (ცუდი ხარისხის ფოტოა)
      district == 55 & precinct == 21  & document_type == "1" ~ 16, # გვაქვს შესწორების ოქმი
      district == 54 & precinct == 25  & document_type == "1" ~ 75, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 47 & precinct == 27  & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == 46 & precinct == 14  & document_type == "1" ~ 11, # შეყვანის შეცდომა
      district == 37 & precinct == 3  & document_type == "1" ~ 64, # გვაქვს შესწორების ოქმი
      district == 24 & precinct == 35  & document_type == "1" ~ 14, # შეყვანის შეცდომა
      district == 22 & precinct == 61  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 22 & precinct == 39  & document_type == "1" ~ 18, # შეყვანის შეცდომა
      district == 10 & precinct == 10  & document_type == "1" ~ 103, # გვაქვს გადათვლის ოქმი
      district == "9" & precinct == "26"  & document_type == "1" ~ 112, # გვაქვს შესწორების ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 186, # გვაქვს შესწორების ოქმი
      district == "7" & precinct == "1" & document_type == "1" ~ 137, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 156, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "13" & document_type == "1" ~ 137, # გვაქვს გადათვლის ოქმი
      district == "3" & precinct == "53" & document_type == "1" ~ 178, # შეყვანის შეცდომა
      district == "3" & precinct == "60" & document_type == "1" ~ 112, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "72" & document_type == "1" ~ 298, # გვაქვს შესწორების ოქმი
      district == "6" & precinct == "71" & document_type == "1" ~ 121, # გვაქვს შესწორების ოქმი
      district == "7" & precinct == "4" & document_type == "1" ~ 116, # გვაქვს შესწორების ოქმი
      district == "7" & precinct == "41" & document_type == "1" ~ 113, # შეყვანის შეცდომა (საშინელი)
      !is.na(amend_p_9) & amend_p_9 != p_9 ~ amend_p_9, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_9) & recount_p_9 != p_9 ~ recount_p_9, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_9) & box_p_9 != p_9 ~ box_p_9, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_9)
    ),
    p_10 = case_when(
      district == 65 & precinct == 13  & document_type == "1" ~ 3, # შესწორების ოქმი
      district == 62 & precinct == 4  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 61 & precinct == 2  & document_type == "1" ~ 8, # შესწორების ოქმი
      district == 59 & precinct == 66  & document_type == "1" ~ 15, # შესწორების ოქმი
      district == 22 & precinct == 25  & document_type == "1" ~ 0, # შესწორების ოქმი
      district == 10 & precinct == 25  & document_type == "1" ~ 8, # შესწორების ოქმი
      district == 80 & precinct == 15  & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == 46 & precinct == 14  & document_type == "1" ~ 0, # შეყვანის შეცდომა
      district == 22 & precinct == 25  & document_type == "1" ~ 20, # შეყვანის შეცდომა
      district == 20 & precinct == 31  & document_type == "1" ~ 18, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 21, # გვაქვს გადათვლის ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 21, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 11, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "90" & document_type == "1" ~ 8, # გვაქვს შესწორების ოქმი
      district == "7" & precinct == "4" & document_type == "1" ~ 13, # გვაქვს შესწორების ოქმი
      district == "7" & precinct == "23" & document_type == "1" ~ 25, # გვაქვს გადათვლის ოქმი
      !is.na(amend_p_10) & amend_p_10 != p_10 ~ amend_p_10, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_10) & recount_p_10 != p_10 ~ recount_p_10, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_10) & box_p_10 != p_10 ~ box_p_10, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_10)
    ),
    p_12 = case_when(
      district == 84 & precinct == 29  & document_type == "1" ~ 0, # შესწორების ოქმი
      district == 52 & precinct == 7  & document_type == "1" ~ 0, # შესწორების ოქმი
      district == 7 & precinct == 6  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 84 & precinct == 1  & document_type == "1" ~ 2, # შეყვანის შეცდომა
      district == 80 & precinct == 15  & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == 58 & precinct == 7  & document_type == "1" ~ 0, # გვაქვს გადათვლის ოქმი
      district == 58 & precinct == 3  & document_type == "1" ~ 3, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 36 & precinct == 20  & document_type == "1" ~ 2, # გვაქვს შესწორების ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 0, # გვაქვს გადათვლის ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "60" & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == "7" & precinct == "4" & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      !is.na(amend_p_12) & amend_p_12 != p_12 ~ amend_p_12, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_12) & recount_p_12 != p_12 ~ recount_p_12, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_12) & box_p_12 != p_12 ~ box_p_12, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_12)
    ),
    p_16 = case_when(
      district == 84 & precinct == 29  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 59 & precinct == 8  & document_type == "1" ~ 19, # შესწორების ოქმი
      district == 52 & precinct == 7  & document_type == "1" ~ 5, # შესწორების ოქმი
      district == 44 & precinct == 22  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 7 & precinct == 18  & document_type == "1" ~ 5, # შესწორების ოქმი
      district == 4 & precinct == 5  & document_type == "1" ~ 10, # შესწორების ოქმი
      district == 80 & precinct == 15  & document_type == "1" ~ 2, # გვაქვს შესწორების ოქმი
      district == 61 & precinct == 27  & document_type == "1" ~ 3, # შეყვანის შეცდომა
      district == 58 & precinct == 4  & document_type == "1" ~ 17, # შეყვანის შეცდომა
      district == 20 & precinct == 30  & document_type == "1" ~ 11, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 6, # გვაქვს გადათვლის ოქმი
      district == "9" & precinct == "26"  & document_type == "1" ~ 15, # გვაქვს შესწორების ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 5, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 11, # გვაქვს შესწორების ოქმი
      district == "05" & precinct == "18" & document_type == "1" ~ 11, # შეყვანის შეცდომა
      district == "7" & precinct == "4" & document_type == "1" ~ 5, # გვაქვს შესწორების ოქმი
      !is.na(amend_p_16) & amend_p_16 != p_16 ~ amend_p_16, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_16) & recount_p_16 != p_16 ~ recount_p_16, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_16) & box_p_16 != p_16 ~ box_p_16, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_16)
    ),
    p_17 = case_when(
      district == 67 & precinct == 13  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 59 & precinct == 66  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 10 & precinct == 25  & document_type == "1" ~ 4, # შესწორების ოქმი
      district == 9 & precinct == 48  & document_type == "1" ~ 4, # შესწორების ოქმი
      district == 80 & precinct == 15  & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == 58 & precinct == 14  & document_type == "1" ~ 2, # შეყვანის შეცდომა
      district == 58 & precinct == 7  & document_type == "1" ~ 2, # გვაქვს გადათვლის ოქმი
      district == 32 & precinct == 94  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 18 & precinct == 18  & document_type == "1" ~ 0, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 2, # გვაქვს გადათვლის ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 4, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      !is.na(amend_p_17) & amend_p_17 != p_17 ~ amend_p_17, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_17) & recount_p_17 != p_17 ~ recount_p_17, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_17) & box_p_17 != p_17 ~ box_p_17, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_17)
    ),
    p_20 = case_when(
      district == 36 & precinct == 4  & document_type == "1" ~ 2, # შესწორების ოქმი
      district == 23 & precinct == 2  & document_type == "1" ~ 3, # შესწორების ოქმი
      district == 2 & precinct == 62  & document_type == "1" ~ 5, # შესწორების ოქმი
      district == 84 & precinct == 35  & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == 80 & precinct == 15  & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == 66 & precinct == 20  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 10 & precinct == 10  & document_type == "1" ~ 0, # გვაქვს გადათვლის ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 2, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 4, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "72" & document_type == "1" ~ 3, # შეყვანის შეცდომა
      district == "5" & precinct == "51" & document_type == "1" ~ 4, # გვაქვს შესწორების ოქმი
      !is.na(amend_p_20) & amend_p_20 != p_20 ~ amend_p_20, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_20) & recount_p_20 != p_20 ~ recount_p_20, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_20) & box_p_20 != p_20 ~ box_p_20, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_20)
    ),
    p_21 = case_when(
      district == 57 & precinct == 2  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 36 & precinct == 4  & document_type == "1" ~ 0, # შესწორების ოქმი
      district == 84 & precinct == 35  & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == 80 & precinct == 15  & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == 58 & precinct == 7  & document_type == "1" ~ 1, # გვაქვს გადათვლის ოქმი
      district == 49 & precinct == 7  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 10 & precinct == 71  & document_type == "1" ~ 0, # შეყვანის შეცდომა
      district == 10 & precinct == 10  & document_type == "1" ~ 2, # გვაქვს გადათვლის ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 2, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 2, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "54" & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == "5" & precinct == "51" & document_type == "1" ~ 7, # გვაქვს შესწორების ოქმი
      !is.na(amend_p_21) & amend_p_21 != p_21 ~ amend_p_21, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_21) & recount_p_21 != p_21 ~ recount_p_21, # გადათვლის შედეგები, თუკი განსხვავებულია
      # !is.na(box_p_21) & box_p_21 != p_21 ~ box_p_21, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_21)
    ),
    p_23 = case_when(
      district == 79 & precinct == 54  & document_type == "1" ~ 3, # შესწორების ოქმი
      district == 44 & precinct == 17  & document_type == "1" ~ 0, # შესწორების ოქმი
      district == 43 & precinct == 22  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 80 & precinct == 15  & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == 60 & precinct == 44  & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == 58 & precinct == 7  & document_type == "1" ~ 0, # გვაქვს გადათვლის ოქმი
      district == 27 & precinct == 40  & document_type == "1" ~ 1, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 3, # გვაქვს გადათვლის ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "54" & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "60" & document_type == "1" ~ 2, # შეყვანის შეცდომა
      district == "5" & precinct == "39" & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == "5" & precinct == "51" & document_type == "1" ~ 2, # გვაქვს შესწორების ოქმი
      district == "7" & precinct == "17" & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      !is.na(amend_p_23) & amend_p_23 != p_23 ~ amend_p_23, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_23) & recount_p_23 != p_23 ~ recount_p_23, # გადათვლის შედეგები, თუკი განსხვავებულია
      # !is.na(box_p_23) & box_p_23 != p_23 ~ box_p_23, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_23)
    ),
    p_25 = case_when(
      district == 62 & precinct == 4  & document_type == "1" ~ 15, # შესწორების ოქმი
      district == 61 & precinct == 31  & document_type == "1" ~ 20, # შესწორების ოქმი
      district == 61 & precinct == 2  & document_type == "1" ~ 101, # შესწორების ოქმი
      district == 58 & precinct == 20  & document_type == "1" ~ 4, # შესწორების ოქმი
      district == 55 & precinct == 1  & document_type == "1" ~ 41, # შესწორების ოქმი
      district == 44 & precinct == 17  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 43 & precinct == 22  & document_type == "1" ~ 26, # შესწორების ოქმი
      district == 27 & precinct == 18  & document_type == "1" ~ 61, # შესწორების ოქმი
      district == 9 & precinct == 39  & document_type == "1" ~ 179, # შესწორების ოქმი
      district == 9 & precinct == 36  & document_type == "1" ~ 106, # შესწორების ოქმი
      district == 9 & precinct == 9  & document_type == "1" ~ 97, # შესწორების ოქმი
      district == 5 & precinct == 6  & document_type == "1" ~ 118, # შესწორების ოქმი და თავდაპირველი ოქმი განსხვავებულია, ისევე, როგორც ცხრილი
      district == 3 & precinct == 89  & document_type == "1" ~ 247, # შესწორების ოქმი
      district == 3 & precinct == 10  & document_type == "1" ~ 124, # შესწორების ოქმი
      district == 2 & precinct == 62  & document_type == "1" ~ 194, # შესწორების ოქმი
      district == 2 & precinct == 46  & document_type == "1" ~ 150, # შესწორების ოქმი
      district == 2 & precinct == 21  & document_type == "1" ~ 124, # შესწორების ოქმი
      district == 2 & precinct == 15  & document_type == "1" ~ 175, # შესწორების ოქმი
      district == 80 & precinct == 15  & document_type == "1" ~ 34, # გვაქვს შესწორების ოქმი
      district == 65 & precinct == 28  & document_type == "1" ~ 123, # გადათვლის ოქმი
      district == 60 & precinct == 44  & document_type == "1" ~ 13, # გვაქვს შესწორების ოქმი
      district == 58 & precinct == 51  & document_type == "1" ~ 13, # შეყვანის შეცდომა
      district == 58 & precinct == 47  & document_type == "1" ~ 19, # შეყვანის შეცდომა
      district == 58 & precinct == 10  & document_type == "1" ~ 14, # შეყვანის შეცდომა
      district == 58 & precinct == 9  & document_type == "1" ~ 15, # გვაქვს გადათვლის ოქმი
      district == 58 & precinct == 4  & document_type == "1" ~ 75, # შეყვანის შეცდომა
      district == 49 & precinct == 7  & document_type == "1" ~ 20, # შეყვანის შეცდომა
      district == 41 & precinct == 2  & document_type == "1" ~ 13, # შეყვანის შეცდომა
      district == 39 & precinct == 1  & document_type == "1" ~ 54, # შეყვანის შეცდომა
      district == 37 & precinct == 3  & document_type == "1" ~ 55, # შეყვანის შეცდომა
      district == 32 & precinct == 48  & document_type == "1" ~ 48, # გვაქვს გადათვლის ოქმი
      district == 22 & precinct == 40  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 22 & precinct == 27  & document_type == "1" ~ 33, # შეყვანის შეცდომა
      district == 20 & precinct == 45  & document_type == "1" ~ 111, # გვაქვს გადათვლის და შესწორების ოქმი
      district == 11 & precinct == 26  & document_type == "1" ~ 10, # შეყვანის შეცდომა
      district == 10 & precinct == 10  & document_type == "1" ~ 103, # გვაქვს გადათვლის ოქმი
      district == "9" & precinct == "26"  & document_type == "1" ~ 89, # გვაქვს შესწორების ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 195, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 121, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "54" & document_type == "1" ~ 121, # გვაქვს შესწორების ოქმი
      district == "5" & precinct == "51" & document_type == "1" ~ 197, # გვაქვს შესწორების ოქმი
      district == "6" & precinct == "59" & document_type == "1" ~ 153, # შეყვანის შეცდომა
      district == "7" & precinct == "17" & document_type == "1" ~ 108, # გვაქვს შესწორების ოქმი
      !is.na(amend_p_25) & amend_p_25 != p_25 ~ amend_p_25, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_25) & recount_p_25 != p_25 ~ recount_p_25, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_25) & box_p_25 != p_25 ~ box_p_25, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_25)
    ),
    p_26 = case_when(
      district == 62 & precinct == 4  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 44 & precinct == 17  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 43 & precinct == 22  & document_type == "1" ~ 0, # შესწორების ოქმი
      district == 3 & precinct == 10  & document_type == "1" ~ 2, # შესწორების ოქმი
      district == 83 & precinct == 25  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 80 & precinct == 15  & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == 60 & precinct == 44  & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == 49 & precinct == 7  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 10 & precinct == 10  & document_type == "1" ~ 0, # გვაქვს გადათვლის ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 2, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "54" & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == "5" & precinct == "51" & document_type == "1" ~ 3, # გვაქვს შესწორების ოქმი
      district == "7" & precinct == "17" & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      !is.na(amend_p_26) & amend_p_26 != p_26 ~ amend_p_26, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_26) & recount_p_26 != p_26 ~ recount_p_26, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_26) & box_p_26 != p_26 ~ box_p_26, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_26)
    ),
    p_27 = case_when(
      district == 81 & precinct == 42  & document_type == "1" ~ 4, # შესწორების ოქმი
      district == 70 & precinct == 11  & document_type == "1" ~ 5, # შესწორების ოქმი
      district == 62 & precinct == 4  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 57 & precinct == 2  & document_type == "1" ~ 0, # შესწორების ოქმი
      district == 19 & precinct == 9  & document_type == "1" ~ 2, # შეყვანის შეცდომა
      district == 87 & precinct == 64  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 80 & precinct == 15  & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == 49 & precinct == 7  & document_type == "1" ~ 0, # შეყვანის შეცდომა
      district == 10 & precinct == 10  & document_type == "1" ~ 1, # გვაქვს გადათვლის ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 7, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 3, # გვაქვს შესწორების ოქმი
      district == 1 & precinct == 10 & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == "2" & precinct == "24" & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "54" & document_type == "1" ~ 1, # გვაქვს შესწორების ოქმი
      district == "4" & precinct == "21" & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == "5" & precinct == "51" & document_type == "1" ~ 4, # გვაქვს შესწორების ოქმი
      !is.na(amend_p_27) & amend_p_27 != p_27 ~ amend_p_27, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_27) & recount_p_27 != p_27 ~ recount_p_27, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_27) & box_p_27 != p_27 ~ box_p_27, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_27)
    ),
    p_36 = case_when(
      district == 65 & precinct == 28  & document_type == "1" ~ 3, # შესწორების ოქმი
      district == 62 & precinct == 4  & document_type == "1" ~ 12, # შესწორების ოქმი
      district == 55 & precinct == 9 & document_type == "1" ~ 2, # შესწორების ოქმი
      district == 52 & precinct == 2  & document_type == "1" ~ 15, # შესწორების ოქმი
      district == 46 & precinct == 11  & document_type == "1" ~ 0, # შესწორების ოქმი
      district == 43 & precinct == 22  & document_type == "1" ~ 6, # შესწორების ოქმი
      district == 12 & precinct == 26  & document_type == "1" ~ 8, # შესწორების ოქმი
      district == 83 & precinct == 32  & document_type == "1" ~ 11, # შეყვანის შეცდომა
      district == 83 & precinct == 28  & document_type == "1" ~ 7, # შეყვანის შეცდომა
      district == 83 & precinct == 24  & document_type == "1" ~ 16, # შეყვანის შეცდომა
      district == 80 & precinct == 15  & document_type == "1" ~ 4, # გვაქვს შესწორების ოქმი
      district == 58 & precinct == 4  & document_type == "1" ~ 19, # შეყვანის შეცდომა
      district == 49 & precinct == 7  & document_type == "1" ~ 6, # შეყვანის შეცდომა
      district == 37 & precinct == 17  & document_type == "1" ~ 14, # შეყვანის შეცდომა
      district == 22 & precinct == 25  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 11 & precinct == 26  & document_type == "1" ~ 1, # შეყვანის შეცდომა
      district == 10 & precinct == 22  & document_type == "1" ~ 51, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 10  & document_type == "1" ~ 54, # გვაქვს გადათვლის ოქმი
      district == "9" & precinct == "75"  & document_type == "1" ~ 43, # გვაქვს შესწორების ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 104, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 67, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "54" & document_type == "1" ~ 63, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "60" & document_type == "1" ~ 61, # გვაქვს შესწორების ოქმი
      district == "5" & precinct == "51" & document_type == "1" ~ 89, # გვაქვს შესწორების ოქმი
      !is.na(amend_p_36) & amend_p_36 != p_36 ~ amend_p_36, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_36) & recount_p_36 != p_36 ~ recount_p_36, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_36) & box_p_36 != p_36 ~ box_p_36, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_36)
    ),
    p_41 = case_when(
      district == 82 & precinct == 40  & document_type == "1" ~ 98, # შესწორების ოქმი
      district == 81 & precinct == 43  & document_type == "1" ~ 621, # შესწორების ოქმი
      district == 81 & precinct == 42  & document_type == "1" ~ 703, # შესწორების ოქმი
      district == 80 & precinct == 29  & document_type == "1" ~ 227, # შესწორების ოქმი
      district == 70 & precinct == 17  & document_type == "1" ~ 345, # შესწორების ოქმი
      district == 67 & precinct == 23  & document_type == "1" ~ 632, # შესწორების ოქმი
      district == 67 & precinct == 13  & document_type == "1" ~ 410, # შესწორების ოქმი
      district == 65 & precinct == 5  & document_type == "1" ~ 189, # შესწორების ოქმი
      district == 62 & precinct == 4  & document_type == "1" ~ 328, # შესწორების ოქმი
      district == 61 & precinct == 2  & document_type == "1" ~ 758, # შესწორების ოქმი
      district == 59 & precinct == 66  & document_type == "1" ~ 364, # შესწორების ოქმი
      district == 59 & precinct == 8  & document_type == "1" ~ 739, # შესწორების ოქმი
      district == 58 & precinct == 20  & document_type == "1" ~ 161, # შესწორების ოქმი
      district == 55 & precinct == 20  & document_type == "1" ~ 420, # შესწორების ოქმი
      district == 55 & precinct == 4  & document_type == "1" ~ 464, # შესწორების ოქმი
      district == 55 & precinct == 1  & document_type == "1" ~ 520, # შესწორების ოქმი
      district == 52 & precinct == 2  & document_type == "1" ~ 327, # შესწორების ოქმი
      district == 44 & precinct == 22  & document_type == "1" ~ 89, # შესწორების ოქმი
      district == 43 & precinct == 9  & document_type == "1" ~ 108, # შესწორების ოქმი
      district == 43 & precinct == 6  & document_type == "1" ~ 61, # შესწორების ოქმი
      district == 38 & precinct == 5  & document_type == "1" ~ 553, # შესწორების ოქმი
      district == 37 & precinct == 15  & document_type == "1" ~ 292, # შესწორების ოქმი
      district == 33 & precinct == 34  & document_type == "1" ~ 587, # შესწორების ოქმი
      district == 32 & precinct == 56  & document_type == "1" ~ 483, # შესწორების ოქმი
      district == 30 & precinct == 23  & document_type == "1" ~ 232, # შესწორების ოქმი
      district == 30 & precinct == 18  & document_type == "1" ~ 308, # შესწორების ოქმი
      district == 27 & precinct == 18  & document_type == "1" ~ 450, # შესწორების ოქმი
      district == 23 & precinct == 2  & document_type == "1" ~ 1023, # შესწორების ოქმი
      district == 23 & precinct == 1  & document_type == "1" ~ 846, # შესწორების ოქმი
      district == 22 & precinct == 49  & document_type == "1" ~ 499, # შესწორების ოქმი
      district == 22 & precinct == 23  & document_type == "1" ~ 244, # შესწორების ოქმი
      district == 22 & precinct == 7  & document_type == "1" ~ 932, # შესწორების ოქმი
      district == 20 & precinct == 7  & document_type == "1" ~ 379, # შესწორების ოქმი
      district == 12 & precinct == 28  & document_type == "1" ~ 552, # შესწორების ოქმი
      district == 12 & precinct == 26  & document_type == "1" ~ 434, # შესწორების ოქმი
      district == 10 & precinct == 59  & document_type == "1" ~ 294, # შესწორების ოქმი
      district == 10 & precinct == 25  & document_type == "1" ~ 327, # შესწორების ოქმი
      district == 10 & precinct == 18  & document_type == "1" ~ 553, # შესწორების ოქმი
      district == 10 & precinct == 4  & document_type == "1" ~ 578, # შესწორების ოქმი
      district == 9 & precinct == 68  & document_type == "1" ~ 359, # შესწორების ოქმი
      district == 9 & precinct == 67  & document_type == "1" ~ 362, # შესწორების ოქმი
      district == 9 & precinct == 63  & document_type == "1" ~ 315, # შესწორების ოქმი
      district == 9 & precinct == 57  & document_type == "1" ~ 347, # შესწორების ოქმი
      district == 9 & precinct == 48  & document_type == "1" ~ 408, # შესწორების ოქმი
      district == 9 & precinct == 39  & document_type == "1" ~ 665, # შესწორების ოქმი
      district == 9 & precinct == 36  & document_type == "1" ~ 405, # შესწორების ოქმი
      district == 9 & precinct == 32  & document_type == "1" ~ 355, # შესწორების ოქმი
      district == 9 & precinct == 9  & document_type == "1" ~ 406, # შესწორების ოქმი
      district == 9 & precinct == 6  & document_type == "1" ~ 322, # შესწორების ოქმი
      district == 7 & precinct == 37  & document_type == "1" ~ 401, # შესწორების ოქმი
      district == 7 & precinct == 18  & document_type == "1" ~ 492, # შესწორების ოქმი
      district == 7 & precinct == 13  & document_type == "1" ~ 468, # შესწორების ოქმი
      district == 7 & precinct == 6  & document_type == "1" ~ 399, # შესწორების ოქმი
      district == 6 & precinct == 69  & document_type == "1" ~ 257, # შესწორების ოქმი
      district == 6 & precinct == 46  & document_type == "1" ~ 290, # შესწორების ოქმი
      district == 6 & precinct == 4  & document_type == "1" ~ 605, # შესწორების ოქმი
      district == 4 & precinct == 5  & document_type == "1" ~ 679, # შესწორების ოქმი
      district == 3 & precinct == 89  & document_type == "1" ~ 587, # შესწორების ოქმი
      district == 3 & precinct == 47  & document_type == "1" ~ 397, # შესწორების ოქმი
      district == 3 & precinct == 18  & document_type == "1" ~ 348, # შესწორების ოქმი
      district == 3 & precinct == 11  & document_type == "1" ~ 357, # შესწორების ოქმი
      district == 3 & precinct == 10  & document_type == "1" ~ 370, # შესწორების ოქმი
      district == 3 & precinct == 9  & document_type == "1" ~ 337, # შესწორების ოქმი
      district == 2 & precinct == 62  & document_type == "1" ~ 598, # შესწორების ოქმი
      district == 2 & precinct == 46  & document_type == "1" ~ 396, # შესწორების ოქმი
      district == 2 & precinct == 21  & document_type == "1" ~ 436, # შესწორების ოქმი
      district == 2 & precinct == 15  & document_type == "1" ~ 514, # შესწორების ოქმი
      district == 2 & precinct == 3  & document_type == "1" ~ 489, # შესწორების ოქმი
      district == 87 & precinct == 29  & document_type == "1" ~ 114, # შეყვანის შეცდომა
      district == 84 & precinct == 35  & document_type == "1" ~ 174, # გვაქვს შესწორების ოქმი
      district == 80 & precinct == 15  & document_type == "1" ~ 350, # გვაქვს შესწორების ოქმი
      district == 65 & precinct == 28  & document_type == "1" ~ 228, # გადათვლის ოქმი
      district == 63 & precinct == 6  & document_type == "1" ~ 322, # შეყვანის შეცდომა
      district == 62 & precinct == 16  & document_type == "1" ~ 417, # შეყვანის შეცდომა
      district == 59 & precinct == 18  & document_type == "1" ~ 530, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 58 & precinct == 10  & document_type == "1" ~ 159, # შეყვანის შეცდომა
      district == 58 & precinct == 3  & document_type == "1" ~ 603, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 54 & precinct == 25  & document_type == "1" ~ 837, # გვაქვს ყუთის შედეგების დასამატებელი ოქმი
      district == 49 & precinct == 7  & document_type == "1" ~ 213, # შეყვანის შეცდომა
      district == 48 & precinct == 9  & document_type == "1" ~ 351, # შეყვანის შეცდომა
      district == 47 & precinct == 33  & document_type == "1" ~ 64, # გვაქვს გადათვლის ოქმის შესწორების ოქმი
      district == 47 & precinct == 28  & document_type == "1" ~ 86, # შეყვანის შეცდომა
      district == 44 & precinct == 24  & document_type == "1" ~ 153, # შეყვანის შეცდომა
      district == 41 & precinct == 34  & document_type == "1" ~ 350, # შეყვანის შეცდომა
      district == 37 & precinct == 3  & document_type == "1" ~ 458, # გვაქვს შესწორების ოქმი
      district == 36 & precinct == 20  & document_type == "1" ~ 511, # გვაქვს შესწორების ოქმი
      district == 24 & precinct == 35  & document_type == "1" ~ 97, # შეყვანის შეცდომა
      district == 24 & precinct == 1  & document_type == "1" ~ 472, # გვაქვს გადათვლის ოქმი
      district == 22 & precinct == 61  & document_type == "1" ~ 471, # შეყვანის შეცდომა
      district == 22 & precinct == 25  & document_type == "1" ~ 276, # შეყვანის შეცდომა
      district == 22 & precinct == 15  & document_type == "1" ~ 359, # შეყვანის შეცდომა
      district == 14 & precinct == 11  & document_type == "1" ~ 813, # შეყვანის შეცდომა
      district == 13 & precinct == 1  & document_type == "1" ~ 560, # გვაქვს გადათვლის ოქმი
      district == 11 & precinct == 28  & document_type == "1" ~ 218, # გვაქვს გადათვლის ოქმი
      district == 11 & precinct == 26  & document_type == "1" ~ 213, # შეყვანის შეცდომა
      district == 10 & precinct == 10  & document_type == "1" ~ 358, # გვაქვს გადათვლის ოქმი
      district == "9" & precinct == "26"  & document_type == "1" ~ 361, # გვაქვს შესწორების ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 614, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 455, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "60" & document_type == "1" ~ 361, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "72" & document_type == "1" ~ 760, # გვაქვს შესწორების ოქმი
      district == "7" & precinct == "4" & document_type == "1" ~ 378, # გვაქვს გადათვლის ოქმი
      district == "7" & precinct == "23" & document_type == "1" ~ 695, # გვაქვს გადათვლის ოქმი
      !is.na(amend_p_41) & amend_p_41 != p_41 ~ amend_p_41, # შესწორების შედეგები, თუკი განსხვავებულია
      !is.na(recount_p_41) & recount_p_41 != p_41 ~ recount_p_41, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_p_41) & box_p_41 != p_41 ~ box_p_41, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(p_41)
    ),
    f_06 = case_when(
      district == 81 & precinct == 43  & document_type == "1" ~ 28, # შესწორების ოქმი
      district == 79 & precinct == 54  & document_type == "1" ~ 66, # შესწორების ოქმი
      district == 70 & precinct == 17  & document_type == "1" ~ 10, # შესწორების ოქმი
      district == 68 & precinct == 8  & document_type == "1" ~ 6, # შესწორების ოქმი
      district == 67 & precinct == 23  & document_type == "1" ~ 28, # შესწორების ოქმი
      district == 65 & precinct == 5  & document_type == "1" ~ 6, # შესწორების ოქმი
      district == 62 & precinct == 4  & document_type == "1" ~ 15, # შესწორების ოქმი
      district == 61 & precinct == 31  & document_type == "1" ~ 3, # შესწორების ოქმი
      district == 61 & precinct == 2  & document_type == "1" ~ 21, # შესწორების ოქმი
      district == 59 & precinct == 8  & document_type == "1" ~ 22, # შესწორების ოქმი
      district == 57 & precinct == 2  & document_type == "1" ~ 7, # შესწორების ოქმი
      district == 55 & precinct == 20  & document_type == "1" ~ 19, # შესწორების ოქმი
      district == 55 & precinct == 4  & document_type == "1" ~ 17, # შესწორების ოქმი
      district == 55 & precinct == 1  & document_type == "1" ~ 26, # შესწორების ოქმი
      district == 43 & precinct == 9  & document_type == "1" ~ 1, # შესწორების ოქმი
      district == 43 & precinct == 6  & document_type == "1" ~ 4, # შესწორების ოქმი
      district == 40 & precinct == 41  & document_type == "1" ~ 2, # შესწორების ოქმი
      district == 38 & precinct == 5  & document_type == "1" ~ 11, # შესწორების ოქმი
      district == 37 & precinct == 15  & document_type == "1" ~ 8, # შესწორების ოქმი
      district == 30 & precinct == 23  & document_type == "1" ~ 5, # შესწორების ოქმი
      district == 30 & precinct == 18  & document_type == "1" ~ 11, # შესწორების ოქმი
      district == 23 & precinct == 2  & document_type == "1" ~ 26, # შესწორების ოქმი
      district == 22 & precinct == 7  & document_type == "1" ~ 51, # შესწორების ოქმი
      district == 20 & precinct == 7  & document_type == "1" ~ 15, # შესწორების ოქმი
      district == 17 & precinct == 43  & document_type == "1" ~ 5, # შესწორების ოქმი
      district == 16 & precinct == 24  & document_type == "1" ~ 11, # შესწორების ოქმი
      district == 12 & precinct == 28  & document_type == "1" ~ 18, # შესწორების ოქმი
      district == 10 & precinct == 59  & document_type == "1" ~ 11, # შესწორების ოქმი
      district == 10 & precinct == 25  & document_type == "1" ~ 8, # შესწორების ოქმი
      district == 10 & precinct == 18  & document_type == "1" ~ 18, # შესწორების ოქმი
      district == 10 & precinct == 4  & document_type == "1" ~ 16, # შესწორების ოქმი
      district == 9 & precinct == 67  & document_type == "1" ~ 9, # შესწორების ოქმი
      district == 9 & precinct == 63  & document_type == "1" ~ 12, # შესწორების ოქმი
      district == 9 & precinct == 57  & document_type == "1" ~ 16, # შესწორების ოქმი
      district == 9 & precinct == 48  & document_type == "1" ~ 10, # შესწორების ოქმი
      district == 9 & precinct == 32  & document_type == "1" ~ 5, # შესწორების ოქმი
      district == 9 & precinct == 9  & document_type == "1" ~ 7, # შესწორების ოქმი
      district == 7 & precinct == 37  & document_type == "1" ~ 7, # შესწორების ოქმი
      district == 32 & precinct == 15  & document_type == "1" ~ 23, # შესწორების ოქმი
      district == 7 & precinct == 18  & document_type == "1" ~ 14, # შესწორების ოქმი
      district == 83 & precinct == 25  & document_type == "1" ~ 43, # შეყვანის შეცდომა
      district == 80 & precinct == 15  & document_type == "1" ~ 21, # გვაქვს შესწორების ოქმი
      district == 79 & precinct == 46  & document_type == "1" ~ 45, # შესწორების ოქმი
      district == 65 & precinct == 28  & document_type == "1" ~ 6, # გადათვლის ოქმი
      district == 55 & precinct == 24  & document_type == "1" ~ 0, # არსებობს ახსნა-განმარტება
      district == 49 & precinct == 7  & document_type == "1" ~ 10, # შეყვანის შეცდომა
      district == 41 & precinct == 19  & document_type == "1" ~ 11, # შეყვანის შეცდომა
      district == 22 & precinct == 61  & document_type == "1" ~ 9, # შეყვანის შეცდომა
      district == 18 & precinct == 18  & document_type == "1" ~ 16, # გვაქვს გადათვლის ოქმი
      district == 14 & precinct == 11  & document_type == "1" ~ 7, # შეყვანის შეცდომა
      district == 11 & precinct == 26  & document_type == "1" ~ 2, # შეყვანის შეცდომა
      district == 10 & precinct == 10  & document_type == "1" ~ 6, # გვაქვს გადათვლის ოქმი
      district == 10 & precinct == 7  & document_type == "1" ~ 25, # გვაქვს შესწორების ოქმი
      district == "8" & precinct == "27"  & document_type == "1" ~ 14, # გვაქვს შესწორების ოქმი
      district == "3" & precinct == "83" & document_type == "1" ~ 10, # გვაქვს შესწორების ოქმი
      district == "2" & precinct == "16" & document_type == "1" ~ 11, # გვაქვს შესწორების ოქმი
      district == "2" & precinct == "70" & document_type == "1" ~ 1, # გააფუჭეს/გააბათილეს ერთი ბიულეტინი, რადგან აპარატმა არ მიიღო
      district == "3" & precinct == "13" & document_type == "1" ~ 6, # ხელახალი გადათვლა: შემცირდა ბათილი ბიულეტინები
      district == "3" & precinct == "83" & document_type == "1" ~ 10, # ხელახალი გადათვლა: დაემატა ბათილი ბიულეტინები
      district == "87" & precinct == "12" & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == "7" & precinct == "4" & document_type == "1" ~ 14, # გვაქვს გადათვლის ოქმი
      district == "7" & precinct == "23" & document_type == "1" ~ 23, # გვაქვს გადათვლის ოქმი
      !is.na(amend_6_should_be) & f_06 != amend_6_should_be ~ amend_6_should_be,
      !is.na(recount_f_06) & recount_f_06 != f_06 ~ recount_f_06, # გადათვლის შედეგები, თუკი განსხვავებულია
      !is.na(box_6_should_be) & box_6_should_be != f_06 ~ box_6_should_be, # ყუთის შედეგები, თუკი განსხვავებულია
      T ~ as.double(f_06)
    ),
    day = case_when(
      district == 79 & precinct == 8  & document_type == "1" ~ 27, # 12 da 24 saati uweriat
      district == "87" & precinct == "64"  & document_type == "1" ~ 27, # გვაქვს შესწორების ოქმი
      district == "9" & precinct == "26"  & document_type == "1" ~ 27, # გვაქვს შესწორების ოქმი
      day == 16 ~ 26,
      day == 20 ~ 26,
      day == 22 ~ 26,
      day == 23 ~ 26,
      day == 24 ~ 26,
      day == 25 ~ 26,
      day == 29 ~ 26,
      day == 59 ~ 26,
      T ~ as.double(day)
    ),
    month = 10,
    year = 2024,
    hour = case_when(
      district == 87 & precinct == 7  & document_type == "1" ~ 5, # ამერიკის უბანი, დიდი რიგები იყო და გვიან მორჩნენ
      district == 27 & precinct == 13  & document_type == "1" ~ 22, # 21:60 უწერიათ მართლა
      district == 79 & precinct == 8  & document_type == "1" ~ 0, # 12 da 24 saati uweriat
      district == "58" & precinct == "1"  & document_type == "1" ~ 0, # გვაქვს შესწორების ოქმი
      district == "28" & precinct == "35"  & document_type == "1" ~ 21, # გვაქვს შესწორების ოქმი
      T ~ as.double(hour)
    ),
    minute = case_when(
      district == 27 & precinct == 13  & document_type == "1" ~ 0, # 21:60 უწერიათ მართლა
      district == 87 & precinct == 7  & document_type == "1" ~ 11, # ამერიკის უბანი, დიდი რიგები იყო და გვიან მორჩნენ
      T ~ as.double(minute)
    )
  ) -> only_approved_processed

# 45 7: 12:00-ზე უფრო მეტი ადამიანი მივიდა, ვიდრე 17:00-ზე (შეყვანა სწორია)
# 45 26: 12:00-ზე უფრო მეტი ადამიანი მივიდა, ვიდრე 17:00-ზე (შეყვანა სწორია)
# 62 13: 12:00-ზე უფრო მეტი ადამიანი მივიდა, ვიდრე 17:00-ზე (შეყვანა სწორია)
# 22 73: 12:00-ზე უფრო მეტი ადამიანი მივიდა, ვიდრე 17:00-ზე (შეყვანა სწორია)

# 3 54: 17:00-ზე უფრო მეტი ადამიანი მივიდა, ვიდრე 20:00-ზე (შეყვანა სწორია)



# 1 8: ბიულეტენი გადამიყვაო, თუმცა ეს მაინც არ ხსნის დისბალანსს
# 1 16: სხვა რამეზე უწერიათ ახსნა-განმარტება

# 2 16: გადასატან ყუთში აღმოჩნდა ორი ბიულეტენი, ამიტომ ვერ დასვეს ბალანსი
# 2 24: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული
# 2 37: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული
# 2 19: ბალანსი არ ჯდება, რადგან ბიულეტინი გადაუყვა რეგისტრატორს

# 3 8: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული
# 3 23: ვერიფიცირება გაუკეთეს ამომრჩეველს ვადაგასული დოკუმენტით, თუმცა ხმა არ მიაცემინეს
# 3 50: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული
# 3 64: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული
# 3 82: დაწერილია აქტი, რომ ყუთიდან ამოვიდა ერთი "მეტი ბიულეტინი", თუმცა ბალანსი მაინც არ ჯდება და სხვა დამატებითი დოკუმენტი არაა ატვირთული
# 3 84: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული

# 5 6: ბალანსი მაინც არ ჯდება მიუხედავად იმისა, რომ გადაითვალეს და შეასწორეს
# 5 18: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული
# 5 30: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული
# 5 40: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული
# 5 49: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული

# 6 2: ბალანსი მაინც არ ჯდება იმის მიუხედავად, რომ არის შესწორების და გადათვლის ოქმები
# 6 11: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული
# 6 21: ბალანსი არ ჯდება, რადგან ამომრჩეველმა ერთი ბიულეტენი აღნიშვნის გარეშე ჩააგდო ყუთში და მეორე მისცეს
# 6 42: ბალანსი არ ჯდება, რადგან რეგისტრატორს გადაუყვა ორი ერთმანეთზე დაკრული ბიულეტინი.
# 6 45: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული
# 6 48: ბალანსი მაინც არ ჯდება, თუმცა დამატებითი დოკუმენტი არაა ატვირთული
# 6 59: ბალანსი მაინც არ ჯდება, რადგან ამომრჩეველმა მეორე ბიულეტინი მოითხოვა (ასეა ახსნილი)

# 7 4: ბალანსი მაინც არ ჯდება, იმის მიუხედავად, რომ შეასწორეს და გადაითვალეს ბიულეტენები

# 8 1: ბალანსი არ ჯდება, რადგან ამომრჩეველმა, ბიულეტინის მიღების შემდეგ, უარი თქვა ხმის მიცემაზე
# 8 24: ბალანსი არ ჯდება, რადგან ამომრჩეველმა ბიულეტინი გაიტანა უბნიდან

# 9 4: ბალანსი არ ჯდება და არც დამატებითი დოკუმენტია ატვირთული
# 9 26: ბალანსი მაინც არ ჯდება, იმის მიუხედავად, რომ შეასწორეს და გადაითვალეს ბიულეტენები
# 9 28: ბალანსი არ ჯდება, რადგან ამომრჩეველმა ცარიელი ბიულეტენის გატანა სცადა უბნიდან
# 9 46: ბალანსი არ ჯდება, რადგან ორმა ამომრჩეველმა რეგისტრაცია გაიარა ვადაგასული პირადობით, თუმცა დისბალანსი ამ შემთხვევაში 2 უნდა იყოს
# 9 76: ბალანსი არ ჯდება, რადგან ამომრჩეველს გაუფუჭდა ბიულეტენი
# 9 79: ბალანსი არ ჯდება, რადგან მარკირებული ამომრჩეველი შემოეპარათ, რომელმაც რეგიტრაცია გაიარა, თუმცა ხმა არ მიუცია
# 10 22: ბალანსი არ ჯდება იმის მიუხედავად, რომ უბანი გახსნეს და გადათვალეს
# 10 48: ბალანსი არ ჯდება იმიტომ, რომ სკანერმა ერთი ბიულეტინი არ მიიღო და შემდეგ შეურიეს ძირითად ნაწილს
# 10 63: ბალანსი არ ჯდება იმის მიუხედავად, რომ უბანი გახსნეს და გადათვალეს

# 11 2: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 11 20: ბალანსი არ ჯდება, იმის მიუხედავად, რომ შესწორების ოქმი ატვირთულია (სხვა რამეზე)
# 11 28: ბალანსი არ ჯდება, გამორჩათ ერთი ამომრჩევლის ხელმოწერა.
# 11 29: ბალანსი არ ჯდება, გამორჩათ ორი ამომრჩევლის ხელმოწერა.
# 11 35: ბალანსი არ ჯდება, გამორჩათ ერთი ამომრჩევლის ხელმოწერა.

# 12 2: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 12 8: ბალანსი არ ჯდება, ატვირთულია დიდი ახსნა-განმარტება, რომელიც ვერ ხსნის 2-იან დისბალანსს
# 12 17: ბალანსი არ ჯდება, ალბათ ერთი ბიულეტენი გადაგვიყვაო
# 12 20: ბალანსი არ ჯდება, რადგან აპარატმა ბიულეტენი არ მიიღო
# 12 40: ბალანსი არ ჯდება, ალბათ სამი ბიულეტენი გადაგვიყვანო ყუთის გადატანის დროსო (დისბალანსი არის 1)

# 13 6: ბალანსი არ ჯდება, რადგან ორი ამომრჩევლის ბიულეტენი არ მიიღო აპარატმა და მათი ხმები გაბათილდა

# 14 1: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 15 4: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 15 11: ბალანსი არ ჯდება იმის მიუხედავად, რომ უბანი გახსნეს და გადაითვალეს
# 15 13: ბალანსი არ ჯდება: ყუთის დამტარებელს გადაყვა ერთი ბიულეტენი
# 15 18: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 15 28: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 16 6: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 18 3: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 18 4: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 18 7: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 18 18: ბალანსი არ ჯდება, იმის მიუხედავად, რომ უბანი გახსნეს და თავიდან გადათვალეს

# 19 9: ბალანსი არ ჯდება, იმის მიუხედავად, რომ უბანი გახსნეს და თავიდან გადათვალეს

# 20 15: ბალანსი არ ჯდება: ჩარჩო-კონვერტში აღმოჩნდა 2 ბიულეტენი
# 20 29: ბალანსი არ ჯდება: მოქალაქეს მეორეჯერ ჰქონდა გავლილი რეგისტრაცია (არ მიაცემინეს ხმა)
# 20 33: ბალანსი არ ჯდება: ყუთიდან ამოვიდა ორი ზედმეტი ბიულეტენი (თუმცა სხვაობა 1-ია)
# 20 38: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 20 45: ბალანსი არ ჯდება, იმის მიუხედავად, რომ უბანი გახსნეს და თავიდან გადათვალეს

# 21 16: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 21 34: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 21 49: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 22 18: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 22 28: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 22 30: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 22 36: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 22 37: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 22 58: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 22 71: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 22 79: ბალანსი არ ჯდება, რადგან ერთმა ამომრჩეველმა არ ჩააგდო ბიულეტენი მთვლელში

# 23 10: ბალანსი არ ჯდება, რადგან ერთ ამომრჩეველს ბიულეტენი გაუფუჭდა და ახლის აღებას აღარ დაელოდა
# 23 26: ბალანსი არ ჯდება, რადგან გადასატან ყუთში აღმოჩნდა დადგენილზე მეტი (2) ბიულეტენი
# 23 49: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული, მიუხედავად იმისა, რომ უბანი გახსნეს და გადათვალეს

# 24 22: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 26 6: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 26 7: ბალანსი არ ჯდება, რადგან ამომრჩეველი გააძევეს უბნიდან

# 27 3: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 27 4: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 27 16: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული


# 28 28: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 30 5: ბალანსი არ ჯდება, ბიულეტენები ერთმანეთს მიეწებაო, თუმცა დისბალანსს სრულად არ ხსნის
# 30 11: ბალანსი არ ჯდება, სამაგიდე სიაში ერთი ხელმოწერა გამორჩათ
# 30 32: ბალანსი არ ჯდება, რადგან ორი ბიულეტენი ერთმანეთს მიეწება

# 32 12: ბალანსი არ ჯდება, რადგან გადასატანი ყუთიდან ამოვიდა ორი ერთმანეთზე მიწებებული ბიულეტენი
# 32 34: ბალანსი არ ჯდება, რადგან ამომრჩეველი არ დაელოდა ბიულეტენის დადასტურებას და დატოვა უბანი
# 32 60: ბალანსი არ ჯდება, რადგან გადასატანი ყუთიდან ამოვიდა ორი ერთმანეთზე მიწებებული ბიულეტენი
# 32 66: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 32 69: ბალანსი არ ჯდება, იმის მიუხედავად, რომ შესწორების ოქმი ატვირთულია
# 32 79: ბალანსი არ ჯდება, რადგან ამომრჩეველი არ დაელოდა ბიულეტენის დადასტურებას და დატოვა უბანი
# 32 80: ბალანსი არ ჯდება, რადგან ამომრჩეველი არ დაელოდა ბიულეტენის დადასტურებას და დატოვა უბანი, ასევე მეორე ამომრჩეველზე გადაყვათ ბიულეტენი
# 32 91: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული


# 33 26: ბალანსი არ ჯდება, რადგან გადასატანი ყუთიდან ამოვიდა 4 ბიულეტენი (დისბალანსი არის ორი)
# 33 27: ბალანსი არ ჯდება, რადგან ბიულეტენი არ მიიღო მთვლელმა და ამომრჩეველმა კი არ აიღო დამატებითი ბიულეტენი

# 35 7: ბალანსი არ ჯდება, რადგან შემთხვევით გასცეს ორი ბიულეტენი

# 36 4: ბალანსი არ ჯდება, რადგან შემთხვევით გასცეს ორი ბიულეტენი
# 36 11: ბალანსი არ ჯდება, რადგან შემთხვევით გასცეს ორი ბიულეტენი
# 36 13: ბალანსი არ ჯდება, რადგან ბიულეტენი არ მიიღო მთვლელმა და ამომრჩეველმა კი არ აიღო დამატებითი ბიულეტენი

# 38 4: ბალანსი არ ჯდება, რადგან გადასატანი ყუთიდან ამოვიდა 2 ბიულეტენი

# 41 34: ბალანსი არ ჯდება იმის მიუხედავად, რომ უბანი გახსნეს და გადათვალეს

# 44 25: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 45 29: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 46 12: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 46 22: ბალანსი არ ჯდება, მიუხედავად იმისა, რომ მონაცემები გადათვალეს და შეასწორეს

# 47 3: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 47 16: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 48 19: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 48 20: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 48 30: ბალანსი არ ჯდება, იმის მიუხედავად, რომ უბანი გახსნეს და გადათვალეს

# 49 2: ბალანსი არ ჯდება, იმის მიუხედავად, რომ უბანი გახსნეს და გადათვალეს
# 49 7: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 49 12: ბალანსი არ ჯდება, რადგან გადასატანი ყუთიდან ამოვიდა 2 ბიულეტენი
# 49 27: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 50 26: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 50 29: ბალანსი არ ჯდება, რადგან გადასატანი ყუთიდან ამოვიდა 2 ბიულეტენი

# 50 2: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 50 32: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 52 7: ბალანსი არ ჯდება, იმის მიუხედავად, რომ უბანი გახსნეს და გადათვალეს

# 53 2: ბალანსი არ ჯდება, ოქმი დაწერილია ყუთის დაზიანებაზე
# 53 4: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 53 12: ბალანსი არ ჯდება, ახსნა-განმარტება დაწერილია 1 გადაყოლილ ბიულეტენზე.

# 54 7: ბალანსი არ ჯდება, რადგან ამომრჩეველი რეგისტრაციის შემდეგ აღარ დაელოდა ახალ ბიულეტენს (დააღვია ფარულობა)
# 54 25: ბალანსი არ ჯდება, რადგან ერთი ზედმეტი ბიულეტენი გადაყვათ


# 55 2: ბალანსი არ ჯდება, რადგან ორი ზედმეტი ბიულეტენი ყუთიდან ამოვიდა და ორიც ამომრჩეველმა დატოვა
# 55 5: ბალანსი არ ჯდება, რადგან ამომრჩეველს გაუფუჭდა ორი ბიულეტენი და შემდეგ როგორც ჩანს, აღარ მიუცია ხმა
# 55 8: ბალანსი არ ჯდება, რადგან ამომრჩეველმა უბანი დატოვა და ხმა არ მიუცია
# 55 16: ბალანსი არ ჯდება, რადგან ამომრჩევლის ორი ბიულეტენი არ მიიღო აპარატმა (სხვაობა არის 1)

# 56 6: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 56 27: ბალანსი არ ჯდება, რადგან ამომრჩეველმა უბანი დატოვა მანამ, სანამ აპარატი მიიღებდა ხმას (აპარატმა არ მიიღო)
# 56 36: ბალანსი არ ჯდება, რადგან ამომრჩეველს გაუფუჭდა სამი ბიულეტენი და შემდეგ როგორც ჩანს, აღარ მიუცია ხმა (სხვაობა ერთია)

# 57 1: ბალანსი არ ჯდება, რადგან ერთი ზედმეტი ბიულეტენი გადაყვათ
# 57 2: ბალანსი არ ჯდება, იმის მიუხედავად, რომ უბანი გახსნეს და გადათვალეს
# 57 6: ბალანსი არ ჯდება, რადგან გადასატანი ყუთიდან ამოვიდა 2 ბიულეტენი
# 57 27: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული

# 58 3: ბალანსი მაინც არ ჯდება, იმის მიუხედავად, რომ უბანი გახსნეს და გადათვალეს
# 58 42: ბალანსი არ ჯდება და არც რაიმე დოკუმენტია ატვირთული
# 58 52: ბალანსი არ ჯდება, რადგან გადასატანი ყუთიდან ორი ბიულეტენი ამოვიდა

# 59 18: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 59 27: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 59 42: ბალანსი არ ჯდება, რადგან ამომრჩეველმა ბიულეტენი არ ჩააგდო, ისე დატოვა უბანი
# 59 62: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი

# 60 2: ბალანსი არ ჯდება, რადგან გადასატანი ყუთიდან ამოვიდა 2 ბიულეტენი
# 60 9: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 60 13: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 60 18: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 60 25: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 60 28: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 60 37: ბალანსი არ ჯდება, რადგან გადასატან ყუთში იყო ორი ზედმეტი ბიულეტენი

# 61 8: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 61 22: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი, შესწორების ოლქი სხვა რამეზეა

# 62 33: ბალანსი არ ჯდება. ბათილებში არაფერი წერია და სავარაუდოდ, გამორჩენილი აქვთ

# 63 1: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი

# 64 18: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 64 29: ბალანსი არ ჯდება, რადგან ორი ამომრჩევლისთვის ხელის მოწერინება გამორჩათ

# 65 1: ბალანსი არ ჯდება, რადგან აპარატმა არ მიიღო ბიულეტენი და ამ დროს ამომრჩეველი უკვე გასული იყო უბნიდან
# 65 7: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 65 13: ბალანსი არ ჯდება, რადგან ამომრჩეველმა ბიულეტენი დააზიანა და გავიდა უბნიდან
# 65 20: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი 
# 65 29: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი, ოლქი გახსნილია და გადათვლილია
# 65 33: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი 

# 66 15: ბალანსი არ ჯდება, რადგან ამომრჩევლის ბიულეტენი სამჯერ არ მიიღო აპარატმა და ამ პიროვნებამ ვერ შეძლო ხმის მიცემა

# 67 1: ბალანსი არ ჯდება, გადასატანი ყუთიდან ამოვიდა 1 ზედმეტი ბიულეტენი
# 67 14: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი 
# 67 15: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი 
# 67 16: ბალანსი არ ჯდება, რადგან სამი ამომრჩევლის ბიულეტენი ვერ დასკანერდა
# 67 18: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი 
# 67 32: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 67 41: ბალანსი არ ჯდება, გადასატანი ყუთიდან ამოვიდა 1 ზედმეტი ბიულეტენი
# 67 51: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი

# 68 1: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი

# 70 10: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 70 11: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი

# 79 5: ბალანსი არ ჯდება, წერია, რომ ერთი ამომრჩეველი არ დაელოდა აპარატის დასტურს, თუმცა დისბალანსი მეტია
# 79 10: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 79 12: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 79 13: ბალანსი არ ჯდება, ქვითარი ამოიბეჭდა, თუმცა ამომრჩეველი ხმის მიცემის გარეშე გავიდა უბნიდან
# 79 25: ბალანსი არ ჯდება, რადგან ვერიფიკაციისას შეცდომა დაუშვეს
# 79 31: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 79 52: ბალანსი არ ჯდება, ქვითარი ამოიბეჭდა, თუმცა ამომრჩეველი ხმის მიცემის გარეშე გავიდა უბნიდან
# 79 53: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 79 54: ბალანსი არ ჯდება, გადასატანი ყუთიდან ამოვიდა 2 ზედმეტი ბიულეტენი
# 79 58: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 79 59: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 79 62: ბალანსი არ ჯდება, რადგან ქვითრის ბეჭდვის დროს ქაღალდი გათავდა, ხოლო ამომრჩეველი არ დაელოდა ბიულეტენს
# 79 72: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი

# 81 1: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 81 10: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი, უბანი გახსნილი და გადათვლილია
# 81 15: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 81 19: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 81 28: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 81 33: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 81 36: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 81 44: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 81 52: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი

# 82 45: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 82 48: ბალანსი არ ჯდება, რადგან კონვერტში იყო ზედმეტი ბიულეტენი

# 83 8: ბალანსი არ ჯდება, გადასატანი ყუთიდან ამოვიდა 2 ზედმეტი ბიულეტენი
# 83 39: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი

# 84 19: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი

# 87 13: ბალანსი არ ჯდება, რადგან სამაგიდო სიაში გამორჩენილია ერთი ხელმოწერა
# 87 29: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 87 36: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 87 47: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი
# 87 67: ბალანსი არ ჯდება და რაიმე ახსნაც არაა მოტანილი


# 40 60: დათვლის დროდ უწერიათ 21 ოქტომბრის 31 საათი (?)

# გადამოწმება
only_approved_processed |>
  filter(document_type == "1", (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved")) |> 
  filter(f_04a >= f_04b) |>
  select(district, precinct, f_03, f_04a, f_04b, f_05) |> 
  arrange(
    district, precinct
  )

only_approved_processed |>
  filter(document_type == "1", (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved")) %>%
  mutate(
    valid = f_05 - f_06,
    total = rowSums(select(., starts_with("p_")), na.rm = T)
  ) |>
  filter(f_04b > f_05) |>
  select(district, precinct, f_01, f_02, f_03, f_04a, f_04b, f_05, total, valid) |> 
  arrange(
    district, precinct
  ) |> 
  print(n = Inf)


only_approved_processed |>
  filter(document_type == "1", (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved")) %>%
  mutate(
    valid = f_05 - f_06,
    total = rowSums(select(., starts_with("p_")), na.rm = T)
  ) |>
  filter(f_04a >= f_05) |>
  select(district, precinct, f_01, f_02, f_03, f_04a, f_04b, f_05, total, valid) |> 
  arrange(
    district, precinct
  ) |> 
  print(n = Inf)

only_approved_processed %>%
  filter(document_type == "1", (is.na(`_validation_status`) | `_validation_status` == "validation_status_approved")) %>%
  mutate(
    valid = f_05 - f_06,
    total = rowSums(select(., starts_with("p_")), na.rm = T)
  ) |> 
  filter(valid != total) |> 
  select(district, precinct, f_05, f_06, valid, total, p_3:p_41) |> 
  arrange(
    district, precinct
  ) |>
  print(n = Inf)

table(only_approved_processed$day)
table(only_approved_processed$minute)

## Compare with CEC

read_csv("cec_dashboard/electronic_machines_vs_hand_count.csv") |> 
  separate(
    prec_id, into = c("state", "district", "precinct"), sep = "\\.", convert = T
  ) |> 
  select(
    district, precinct, number, votes
  ) -> cec_data_hand_count

cec_data_hand_count |> 
  select(district, precinct) |> distinct() |> anti_join(
    only_approved_processed |> select(district, precinct) |> distinct()
  )

only_approved_processed |>
  select(
    district, precinct, p_3:p_41
  ) |> 
  pivot_longer(
    cols = p_3:p_41, names_to = "number", values_to = "votes"
  ) |> 
  mutate(
    number = str_remove(number, "p_")
  ) |>
  type_convert() |> 
  arrange(district, precinct, number) |> distinct() -> georgia_protocols_long

georgia_protocols_long |> 
  left_join(cec_data_hand_count, by = c("district", "precinct", "number")) |>
  mutate(
    diff = votes.x - votes.y
  ) |> 
  filter(diff != 0)

cec_data_hand_count |> 
  group_by(number) |>
  summarize(
    votes = sum(votes)
  )

georgia_protocols_long |> 
  group_by(number) |>
  summarize(
    votes = sum(votes)
  )

## საბოლოო ბაზა

readxl::read_excel("notes.xlsx", sheet = 1) |> 
  type_convert() -> notes

readxl::read_excel("notes.xlsx", sheet = 2) |> 
  type_convert() -> definitions

# combine if there are more than one note per district/precinct


notes |> 
  group_by(district, precinct) |>
  summarise(
    note = paste0(note, collapse = "; ")
  ) -> notes_collapsed


only_approved_processed |> 
  select(
    district, precinct, code, attached, f_01:f_06, stamp:opinion, day:minute, note
  ) |> 
  left_join(notes_collapsed, by = c("district", "precinct")) |> 
  mutate(
    note = paste0(note.x, "; ", note.y),
    note = str_replace_all(note, "NA|NA;|;NA|^\\;$", ""),
    # replace ; with empty
    note = str_replace_all(note, ";", ""),
    across(
      c(stamp:secretary, opinion), ~case_when(
        . == "1" ~ "დიახ",
        . == "0" ~ "არა",
        TRUE ~ as.character(.)
      )
    )
  ) |>
  select(-note.x, -note.y) |> 
  arrange(
    district, precinct
  )-> georgia_parliamentary_elections


openxlsx::createWorkbook() -> geo_parl_elections_wb

openxlsx::addWorksheet(geo_parl_elections_wb, "შედეგები")

openxlsx::addWorksheet(geo_parl_elections_wb, "განმარტებები")

openxlsx::addWorksheet(geo_parl_elections_wb, "შენიშვნა")

additional_text <- tibble(
  text = c(
    "ხმების ჯამი 1-ით მეტია ცესკო-ს მიერ 2024 წლის 14 ნოემბრისთვის არსებულ შედეგებს (მონაცემების შეყვანაში შეცდომის გამო). სხვა დისბალანსი უმეტეს შემთხვევაში, გამოვლენილი და დოკუმენტირებულია. თუ დამატებით რაიმეს იპოვით, დამიკავშირდით ელ.ფოსტის მეშვეობით: david@sichinava.ge",
    "წინამდებარე მონაცემები დიგიტალიზებულია 87 მოხალისის მიერ. გამოყენებული ტექნოლოგიები: KoboToolbox, R. მონაცემების გამოყენება ნებისმიერ მსურველს შეუძლია გვერდზე: https://github.com/davidsichinava/georgia_elections_data",
    "მონაცემების განახლების თარიღი: 2024 წლის 15 ნოემბერი, თბილისის დროით 15:44"
           )
)

openxlsx::writeData(geo_parl_elections_wb, sheet = "შედეგები", x = georgia_parliamentary_elections)
openxlsx::writeData(geo_parl_elections_wb, sheet = "განმარტებები", x = definitions)
openxlsx::writeData(geo_parl_elections_wb, sheet = "შენიშვნა", x = additional_text)

openxlsx::saveWorkbook(geo_parl_elections_wb, "georgia_parliamentary_elections_nov15_2024.xlsx", overwrite = T)

