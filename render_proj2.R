#Render Code

library(tidyverse)
library(rmarkdown)


##create vector of channel names. Could automate this further.

channels<-c("data_channel_is_lifestyle",
            "data_channel_is_entertainment",
            "data_channel_is_bus",
            "data_channel_is_socmed", 
            "data_channel_is_tech",
            "data_channel_is_world")  

##Create names for .md files 
md_files <- paste0(channels, ".md")

#create report for each  channel by rendering and passing the channel parameter

for (i in 1:length(channels)){  
  rmarkdown::render(input="Project2Code.RMD", 
                    output_format = "github_document",
                    output_file = md_files[i],
                    params=list(channel=channels[i]), 
                    #envir=new.env(),
                    output_options = list(#html_preview = FALSE,
                                          toc=TRUE)
                     )   
}


  