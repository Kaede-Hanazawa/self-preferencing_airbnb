df <- readxl::read_xlsx(here::here("99_data", "住宅宿泊事業の届出_東京都", "23区_住宅宿泊事業_届出情報_URL.xlsx"))

View(df)
df <- df |>
    filter(city_ids != "23区外") |>
    mutate(
        `Last Updated` = いつ
    ) |>
    select(city_ids, 届出数, `Last Updated`, content,) |>
    arrange(desc(届出数))  # 届出数で降順に並べ替え


df |>
  kbl(booktabs = TRUE, format = "latex", caption = "Sample Table") |>
  kable_styling(latex_options = c("hold_position", "striped"))

df |>
    kbl(booktabs = TRUE, caption = "Sample Table") |>
    kable_styling(latex_options = c("hold_position", "striped")) |>
    kable_classic(full_width = F)

library(kableExtra)

# データフレームの出力をkableExtraを使ってLaTeXのtableに変換
df |>
  kbl(booktabs = TRUE, format = "latex", caption = "Sample Table") |>
  kable_styling(latex_options = c("hold_position", "striped")) |>
  kable_classic(full_width = F)
