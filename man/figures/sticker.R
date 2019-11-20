library(hexSticker)
library(usethis)

hexSticker::sticker("inst/icon.png",
                    package="rtapas",
                    s_x=1,
                    s_y=1,
                    s_width=0.55,
                    s_height=0.55,
                    p_color = "white",
                    p_size = 6,
                    p_x = 1,
                    p_y = 1.7,
                    h_fill = "#EE3223",
                    h_color = "#AE3927",
                    url = "https://github.com/avalcarcel9/rtapas",
                    u_size = 1.2,
                    u_color = "white",
                    filename = "inst/sticker.png")

usethis::use_build_ignore(
  c("inst/icon.png", 
    "inst/icon.ai", 
    "inst/sticker.R", 
    "inst/sticker.png"))
