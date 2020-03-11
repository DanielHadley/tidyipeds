require(tidyverse)
require(siverse)
require(hexSticker)

img <- "https://www.naicu.edu/getmedia/4c12bbda-f945-4b04-81ea-6065474e69c0/IPEDS-2.png"

sticker(img, package = "tidyipeds",
        h_fill = "white",
        p_color = "black",
        p_family = "Roboto",
        h_color = "gold",
        s_x = 1,
        s_y = .75,
        s_width = .5)
