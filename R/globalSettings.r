## @knitr globalSettings
opts_knit$set(concordance=TRUE)                     #% generate concordance
opts_chunk$set(background='white')                 #% background color of code chunks.
opts_chunk$set(out.width="45%")           #% images are .45*linewidth by default
opts_chunk$set(dev="png")                            #% images are png by default
opts_chunk$set(fig.align='center')                   #% center images
opts_chunk$set(fig.path='figures/')
opts_chunk$set(fig.pos="!hbtp")
opts_chunk$set(message=FALSE)                        #% do not print out messages
opts_chunk$set(warning=FALSE)                        #% do not print out warnings
opts_chunk$set(cache=TRUE)                           #% cache chunks
#opts_knit$set(self.contained=FALSE)
opts_chunk$set(comment="##", prompt=FALSE)             #% Whether to use prompts or comments
options(left.brace.newline = TRUE)                  #% put the curly brace of functions on a new line where they belong
options(width = 80)