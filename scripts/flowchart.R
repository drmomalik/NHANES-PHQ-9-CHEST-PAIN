library(flowchart)

df <- subsetd

fc1 <- df %>% 
  as_fc(label = "NHANES Cardiovascular Health Survey participants between 2005-2020 (pre-pandemic)
        (All participants 40 years of age or older)", text_fs = 9, text_padding = 2) %>% 
  fc_filter(N = 9576, text_padding = 1.5, text_fs = 9, text_fs_exc = 9, offset_exc = 0.15, text_padding_exc = 1,
            label="Total respondents who have ever experienced chest pain", 
            text_pattern_exc = "Responded 'No' to experiencing chest pain: 25656 (72.8%)
            Refused to respond: 3 (<0.001%)
            Missing response: 24 (<0.001%)"
            , show_exc = TRUE, just_exc = "left", text_fface_exc = 2) %>% 
  fc_filter(N = 1880, text_padding = 1.5, text_fs = 9, text_fs_exc = 9, offset_exc = 0.15, text_padding_exc = 1,
            label = "Total respondents with 'exertional' chest pain", 
            label_exc = 
            "Responded 'no' to experiencing chest pain 
            when walking uphill, in a hurry and at an ordinary pace: 7170 (74.9%)
            Never exerts themselves: 450 (4.7%)
            Refused to respond: 1 (<0.001%) 
            Don't know: 75 (0.01%)",
            text_pattern_exc= "{label_exc}", show_exc = TRUE, text_fface_exc = 2, just_exc = "left") %>% 
  fc_split(N = c(1269, 611), label = c("Respondents who have history of CAD/MI/Angina or Rose Criteria Grade 1 or 2 Angina", 
                                       "No history of CAD/MI/Angina or Rose Criteria Grade 1 or 2 Angina"),
           text_fs = 9, text_padding = 2) %>% 
  fc_draw(title = "NHANES Chest-Pain and PHQ-9 Survey Population", arrow_type = "open", title_y = 0.95)




fc2 <- as_fc(N = 7000, label = "each respondent localized pain in up to 8 different body regions",
             text_pattern = "For n = 1531, {label}",
             text_fs = 12, text_padding = 1.5) %>% 
  fc_filter(N = 2487, label = "Total observations of chest pain by location", text_padding = 1.5,
            text_color_exc = "red", border_color_exc = "red", direction_ex = "right", 
            show_exc = TRUE, just = "centre", just_exc = "left", text_pattern = "{label} 
            n* = {n}",
            text_pattern_exc = "{label_exc}",
            offset_exc = 0.15, text_padding_exc = 1, text_fs_exc = 10, text_fs = 12,
            label_exc = "# of respondents with # of body regions selected:
                  n = 916 with 1 body region,
                  n = 381 with 2 body regions,
                  n = 164 with 3 body regions,
                  n = 51 with 4 body regions,
                  n = 10 with 5 body regions, 
                  n = 4 with 6 body regions, 
                  n = 1 with 7 body regions,
                  n = 4 with 8 body regions", border_color = "red",text_color = "red"
  ) %>% 
  fc_split(N = c(124, 285, 153, 883, 299, 572, 126, 45), 
         label = c("Right Arm","Right Chest",
                   "Neck","Upper Sternum","Lower Sternum",
                   "Left Chest", "Left arm", "Epigastric"), text_fs = 12
      , text_padding = 0.5) %>% 
  fc_draw(title = "Total Observations of Chest Pain by location in NHANES sample",
          arrow_type = "open", title_y = 0.95)


  
