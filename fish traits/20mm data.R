# Pete Nelson, PhD
# Department of Water Resources
# purpose: acquire 20mm survey data from EDI

# drought/trait study ----

# Package ID: edi.535.4 Cataloging System:https://pasta.edirepository.org.
# Data set title: Interagency Ecological Program San Francisco Estuary 20mm Survey 1995 - 2021.
# Data set creator:    - Interagency Ecological Program (IEP) 
# Data set creator:  Lauren Damon - California Department of Fish and Wildlife 
# Data set creator:  Adam Chorazyczewski - California Department of Fish and Wildlife 
# Contact:  Adam Chorazyczewski -  California Department of Fish and Wildlife  - Adam.chorazyczewski@wildlife.ca.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/535/4/671a005532337c078b2bbf95c1df6e2d" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Year",     
                 "qry_AMC_EDI_01.Survey.Survey",     
                 "SampleDate",     
                 "TowTime",     
                 "qry_AMC_EDI_01.Station.Station",     
                 "Latitude",     
                 "Longitude",     
                 "Duration",     
                 "TowNum",     
                 "GearCode",     
                 "Volume",     
                 "Tide",     
                 "BottomDepth",     
                 "Temp",     
                 "TopEC",     
                 "BottomEC",     
                 "Secchi",     
                 "Turbidity",     
                 "Comments",     
                 "American.Shad",     
                 "Arrow.Goby",     
                 "Bay.Goby",     
                 "Bay.Pipefish",     
                 "Bigscale.Logperch",     
                 "Black.Bullhead",     
                 "Black.Crappie",     
                 "Bluegill.Sunfish",     
                 "Brown.Bullhead",     
                 "Brown.Rockfish",     
                 "California.Tonguefish",     
                 "Carp",     
                 "Catfish..paren.Unid.paren.",     
                 "Centrarchids..paren.Unid.paren.",     
                 "Chameleon.Goby",     
                 "Channel.Catfish",     
                 "Cheekspot.Goby",     
                 "Chinook.Salmon",     
                 "Cyprinids..paren.Unid.paren.",     
                 "Delta.Smelt",     
                 "English.Sole",     
                 "Gobies..paren.Unid.paren.",     
                 "Golden.Shiner",     
                 "Goldfish",     
                 "Hitch",     
                 "Inland.Silverside",     
                 "Jacksmelt",     
                 "Lampreys..paren.Unid.paren.",     
                 "Largemouth.Bass",     
                 "Longfin.Smelt",     
                 "Longjaw.Mudsucker",     
                 "Mosquitofish",     
                 "Northern.Anchovy",     
                 "Pacific.Herring",     
                 "Pacific.Lamprey",     
                 "Pacific.Staghorn.Sculpin",     
                 "Plainfin.Midshipman",     
                 "Prickly.Sculpin",     
                 "Rainwater.Killifish",     
                 "Redear.Sunfish",     
                 "River.Lamprey",     
                 "Sacramento.Blackfish",     
                 "Sacramento.Pikeminnow",     
                 "Sacramento.Sucker",     
                 "Sculpins..paren.Unid.paren.",     
                 "Shimofuri.Goby",     
                 "Shiner.Perch",     
                 "Shokihaze.Goby",     
                 "Silversides..paren.Unid.paren.",     
                 "Smallmouth.Bass",     
                 "Smelt..paren.Unid.paren.",     
                 "Speckled.Sanddab",     
                 "Splittail",     
                 "Spotted.Bass",     
                 "Starry.Flounder",     
                 "Striped.Bass",     
                 "Sturgeon..paren.Unid.paren.",     
                 "Threadfin.Shad",     
                 "Threespine.Stickleback",     
                 "Topsmelt",     
                 "Tridentiger.spp_",     
                 "Tule.Perch",     
                 "Unknown",     
                 "Wakasagi",     
                 "White.Catfish",     
                 "White.Crappie",     
                 "White.Croaker",     
                 "White.Sturgeon",     
                 "Yellowfin.Goby"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Year)=="factor") dt1$Year <-as.numeric(levels(dt1$Year))[as.integer(dt1$Year) ]               
if (class(dt1$Year)=="character") dt1$Year <-as.numeric(dt1$Year)
if (class(dt1$qry_AMC_EDI_01.Survey.Survey)!="factor") dt1$qry_AMC_EDI_01.Survey.Survey<- as.factor(dt1$qry_AMC_EDI_01.Survey.Survey)                                   
# attempting to convert dt1$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1SampleDate<-as.Date(dt1$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1SampleDate) == length(tmp1SampleDate[!is.na(tmp1SampleDate)])){dt1$SampleDate <- tmp1SampleDate } else {print("Date conversion failed for dt1$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1SampleDate) 
if (class(dt1$qry_AMC_EDI_01.Station.Station)!="factor") dt1$qry_AMC_EDI_01.Station.Station<- as.factor(dt1$qry_AMC_EDI_01.Station.Station)
if (class(dt1$Latitude)!="factor") dt1$Latitude<- as.factor(dt1$Latitude)
if (class(dt1$Longitude)!="factor") dt1$Longitude<- as.factor(dt1$Longitude)
if (class(dt1$Duration)=="factor") dt1$Duration <-as.numeric(levels(dt1$Duration))[as.integer(dt1$Duration) ]               
if (class(dt1$Duration)=="character") dt1$Duration <-as.numeric(dt1$Duration)
if (class(dt1$TowNum)!="factor") dt1$TowNum<- as.factor(dt1$TowNum)
if (class(dt1$GearCode)!="factor") dt1$GearCode<- as.factor(dt1$GearCode)
if (class(dt1$Volume)=="factor") dt1$Volume <-as.numeric(levels(dt1$Volume))[as.integer(dt1$Volume) ]               
if (class(dt1$Volume)=="character") dt1$Volume <-as.numeric(dt1$Volume)
if (class(dt1$Tide)!="factor") dt1$Tide<- as.factor(dt1$Tide)
if (class(dt1$BottomDepth)=="factor") dt1$BottomDepth <-as.numeric(levels(dt1$BottomDepth))[as.integer(dt1$BottomDepth) ]               
if (class(dt1$BottomDepth)=="character") dt1$BottomDepth <-as.numeric(dt1$BottomDepth)
if (class(dt1$Temp)=="factor") dt1$Temp <-as.numeric(levels(dt1$Temp))[as.integer(dt1$Temp) ]               
if (class(dt1$Temp)=="character") dt1$Temp <-as.numeric(dt1$Temp)
if (class(dt1$TopEC)=="factor") dt1$TopEC <-as.numeric(levels(dt1$TopEC))[as.integer(dt1$TopEC) ]               
if (class(dt1$TopEC)=="character") dt1$TopEC <-as.numeric(dt1$TopEC)
if (class(dt1$BottomEC)=="factor") dt1$BottomEC <-as.numeric(levels(dt1$BottomEC))[as.integer(dt1$BottomEC) ]               
if (class(dt1$BottomEC)=="character") dt1$BottomEC <-as.numeric(dt1$BottomEC)
if (class(dt1$Secchi)=="factor") dt1$Secchi <-as.numeric(levels(dt1$Secchi))[as.integer(dt1$Secchi) ]               
if (class(dt1$Secchi)=="character") dt1$Secchi <-as.numeric(dt1$Secchi)
if (class(dt1$Turbidity)=="factor") dt1$Turbidity <-as.numeric(levels(dt1$Turbidity))[as.integer(dt1$Turbidity) ]               
if (class(dt1$Turbidity)=="character") dt1$Turbidity <-as.numeric(dt1$Turbidity)
if (class(dt1$Comments)!="factor") dt1$Comments<- as.factor(dt1$Comments)
if (class(dt1$American.Shad)=="factor") dt1$American.Shad <-as.numeric(levels(dt1$American.Shad))[as.integer(dt1$American.Shad) ]               
if (class(dt1$American.Shad)=="character") dt1$American.Shad <-as.numeric(dt1$American.Shad)
if (class(dt1$Arrow.Goby)=="factor") dt1$Arrow.Goby <-as.numeric(levels(dt1$Arrow.Goby))[as.integer(dt1$Arrow.Goby) ]               
if (class(dt1$Arrow.Goby)=="character") dt1$Arrow.Goby <-as.numeric(dt1$Arrow.Goby)
if (class(dt1$Bay.Goby)=="factor") dt1$Bay.Goby <-as.numeric(levels(dt1$Bay.Goby))[as.integer(dt1$Bay.Goby) ]               
if (class(dt1$Bay.Goby)=="character") dt1$Bay.Goby <-as.numeric(dt1$Bay.Goby)
if (class(dt1$Bay.Pipefish)=="factor") dt1$Bay.Pipefish <-as.numeric(levels(dt1$Bay.Pipefish))[as.integer(dt1$Bay.Pipefish) ]               
if (class(dt1$Bay.Pipefish)=="character") dt1$Bay.Pipefish <-as.numeric(dt1$Bay.Pipefish)
if (class(dt1$Bigscale.Logperch)=="factor") dt1$Bigscale.Logperch <-as.numeric(levels(dt1$Bigscale.Logperch))[as.integer(dt1$Bigscale.Logperch) ]               
if (class(dt1$Bigscale.Logperch)=="character") dt1$Bigscale.Logperch <-as.numeric(dt1$Bigscale.Logperch)
if (class(dt1$Black.Bullhead)=="factor") dt1$Black.Bullhead <-as.numeric(levels(dt1$Black.Bullhead))[as.integer(dt1$Black.Bullhead) ]               
if (class(dt1$Black.Bullhead)=="character") dt1$Black.Bullhead <-as.numeric(dt1$Black.Bullhead)
if (class(dt1$Black.Crappie)=="factor") dt1$Black.Crappie <-as.numeric(levels(dt1$Black.Crappie))[as.integer(dt1$Black.Crappie) ]               
if (class(dt1$Black.Crappie)=="character") dt1$Black.Crappie <-as.numeric(dt1$Black.Crappie)
if (class(dt1$Bluegill.Sunfish)=="factor") dt1$Bluegill.Sunfish <-as.numeric(levels(dt1$Bluegill.Sunfish))[as.integer(dt1$Bluegill.Sunfish) ]               
if (class(dt1$Bluegill.Sunfish)=="character") dt1$Bluegill.Sunfish <-as.numeric(dt1$Bluegill.Sunfish)
if (class(dt1$Brown.Bullhead)=="factor") dt1$Brown.Bullhead <-as.numeric(levels(dt1$Brown.Bullhead))[as.integer(dt1$Brown.Bullhead) ]               
if (class(dt1$Brown.Bullhead)=="character") dt1$Brown.Bullhead <-as.numeric(dt1$Brown.Bullhead)
if (class(dt1$Brown.Rockfish)=="factor") dt1$Brown.Rockfish <-as.numeric(levels(dt1$Brown.Rockfish))[as.integer(dt1$Brown.Rockfish) ]               
if (class(dt1$Brown.Rockfish)=="character") dt1$Brown.Rockfish <-as.numeric(dt1$Brown.Rockfish)
if (class(dt1$California.Tonguefish)=="factor") dt1$California.Tonguefish <-as.numeric(levels(dt1$California.Tonguefish))[as.integer(dt1$California.Tonguefish) ]               
if (class(dt1$California.Tonguefish)=="character") dt1$California.Tonguefish <-as.numeric(dt1$California.Tonguefish)
if (class(dt1$Carp)=="factor") dt1$Carp <-as.numeric(levels(dt1$Carp))[as.integer(dt1$Carp) ]               
if (class(dt1$Carp)=="character") dt1$Carp <-as.numeric(dt1$Carp)
if (class(dt1$Catfish..paren.Unid.paren.)=="factor") dt1$Catfish..paren.Unid.paren. <-as.numeric(levels(dt1$Catfish..paren.Unid.paren.))[as.integer(dt1$Catfish..paren.Unid.paren.) ]               
if (class(dt1$Catfish..paren.Unid.paren.)=="character") dt1$Catfish..paren.Unid.paren. <-as.numeric(dt1$Catfish..paren.Unid.paren.)
if (class(dt1$Centrarchids..paren.Unid.paren.)=="factor") dt1$Centrarchids..paren.Unid.paren. <-as.numeric(levels(dt1$Centrarchids..paren.Unid.paren.))[as.integer(dt1$Centrarchids..paren.Unid.paren.) ]               
if (class(dt1$Centrarchids..paren.Unid.paren.)=="character") dt1$Centrarchids..paren.Unid.paren. <-as.numeric(dt1$Centrarchids..paren.Unid.paren.)
if (class(dt1$Chameleon.Goby)=="factor") dt1$Chameleon.Goby <-as.numeric(levels(dt1$Chameleon.Goby))[as.integer(dt1$Chameleon.Goby) ]               
if (class(dt1$Chameleon.Goby)=="character") dt1$Chameleon.Goby <-as.numeric(dt1$Chameleon.Goby)
if (class(dt1$Channel.Catfish)=="factor") dt1$Channel.Catfish <-as.numeric(levels(dt1$Channel.Catfish))[as.integer(dt1$Channel.Catfish) ]               
if (class(dt1$Channel.Catfish)=="character") dt1$Channel.Catfish <-as.numeric(dt1$Channel.Catfish)
if (class(dt1$Cheekspot.Goby)=="factor") dt1$Cheekspot.Goby <-as.numeric(levels(dt1$Cheekspot.Goby))[as.integer(dt1$Cheekspot.Goby) ]               
if (class(dt1$Cheekspot.Goby)=="character") dt1$Cheekspot.Goby <-as.numeric(dt1$Cheekspot.Goby)
if (class(dt1$Chinook.Salmon)=="factor") dt1$Chinook.Salmon <-as.numeric(levels(dt1$Chinook.Salmon))[as.integer(dt1$Chinook.Salmon) ]               
if (class(dt1$Chinook.Salmon)=="character") dt1$Chinook.Salmon <-as.numeric(dt1$Chinook.Salmon)
if (class(dt1$Cyprinids..paren.Unid.paren.)=="factor") dt1$Cyprinids..paren.Unid.paren. <-as.numeric(levels(dt1$Cyprinids..paren.Unid.paren.))[as.integer(dt1$Cyprinids..paren.Unid.paren.) ]               
if (class(dt1$Cyprinids..paren.Unid.paren.)=="character") dt1$Cyprinids..paren.Unid.paren. <-as.numeric(dt1$Cyprinids..paren.Unid.paren.)
if (class(dt1$Delta.Smelt)=="factor") dt1$Delta.Smelt <-as.numeric(levels(dt1$Delta.Smelt))[as.integer(dt1$Delta.Smelt) ]               
if (class(dt1$Delta.Smelt)=="character") dt1$Delta.Smelt <-as.numeric(dt1$Delta.Smelt)
if (class(dt1$English.Sole)=="factor") dt1$English.Sole <-as.numeric(levels(dt1$English.Sole))[as.integer(dt1$English.Sole) ]               
if (class(dt1$English.Sole)=="character") dt1$English.Sole <-as.numeric(dt1$English.Sole)
if (class(dt1$Gobies..paren.Unid.paren.)=="factor") dt1$Gobies..paren.Unid.paren. <-as.numeric(levels(dt1$Gobies..paren.Unid.paren.))[as.integer(dt1$Gobies..paren.Unid.paren.) ]               
if (class(dt1$Gobies..paren.Unid.paren.)=="character") dt1$Gobies..paren.Unid.paren. <-as.numeric(dt1$Gobies..paren.Unid.paren.)
if (class(dt1$Golden.Shiner)=="factor") dt1$Golden.Shiner <-as.numeric(levels(dt1$Golden.Shiner))[as.integer(dt1$Golden.Shiner) ]               
if (class(dt1$Golden.Shiner)=="character") dt1$Golden.Shiner <-as.numeric(dt1$Golden.Shiner)
if (class(dt1$Goldfish)=="factor") dt1$Goldfish <-as.numeric(levels(dt1$Goldfish))[as.integer(dt1$Goldfish) ]               
if (class(dt1$Goldfish)=="character") dt1$Goldfish <-as.numeric(dt1$Goldfish)
if (class(dt1$Hitch)=="factor") dt1$Hitch <-as.numeric(levels(dt1$Hitch))[as.integer(dt1$Hitch) ]               
if (class(dt1$Hitch)=="character") dt1$Hitch <-as.numeric(dt1$Hitch)
if (class(dt1$Inland.Silverside)=="factor") dt1$Inland.Silverside <-as.numeric(levels(dt1$Inland.Silverside))[as.integer(dt1$Inland.Silverside) ]               
if (class(dt1$Inland.Silverside)=="character") dt1$Inland.Silverside <-as.numeric(dt1$Inland.Silverside)
if (class(dt1$Jacksmelt)=="factor") dt1$Jacksmelt <-as.numeric(levels(dt1$Jacksmelt))[as.integer(dt1$Jacksmelt) ]               
if (class(dt1$Jacksmelt)=="character") dt1$Jacksmelt <-as.numeric(dt1$Jacksmelt)
if (class(dt1$Lampreys..paren.Unid.paren.)=="factor") dt1$Lampreys..paren.Unid.paren. <-as.numeric(levels(dt1$Lampreys..paren.Unid.paren.))[as.integer(dt1$Lampreys..paren.Unid.paren.) ]               
if (class(dt1$Lampreys..paren.Unid.paren.)=="character") dt1$Lampreys..paren.Unid.paren. <-as.numeric(dt1$Lampreys..paren.Unid.paren.)
if (class(dt1$Largemouth.Bass)=="factor") dt1$Largemouth.Bass <-as.numeric(levels(dt1$Largemouth.Bass))[as.integer(dt1$Largemouth.Bass) ]               
if (class(dt1$Largemouth.Bass)=="character") dt1$Largemouth.Bass <-as.numeric(dt1$Largemouth.Bass)
if (class(dt1$Longfin.Smelt)=="factor") dt1$Longfin.Smelt <-as.numeric(levels(dt1$Longfin.Smelt))[as.integer(dt1$Longfin.Smelt) ]               
if (class(dt1$Longfin.Smelt)=="character") dt1$Longfin.Smelt <-as.numeric(dt1$Longfin.Smelt)
if (class(dt1$Longjaw.Mudsucker)=="factor") dt1$Longjaw.Mudsucker <-as.numeric(levels(dt1$Longjaw.Mudsucker))[as.integer(dt1$Longjaw.Mudsucker) ]               
if (class(dt1$Longjaw.Mudsucker)=="character") dt1$Longjaw.Mudsucker <-as.numeric(dt1$Longjaw.Mudsucker)
if (class(dt1$Mosquitofish)=="factor") dt1$Mosquitofish <-as.numeric(levels(dt1$Mosquitofish))[as.integer(dt1$Mosquitofish) ]               
if (class(dt1$Mosquitofish)=="character") dt1$Mosquitofish <-as.numeric(dt1$Mosquitofish)
if (class(dt1$Northern.Anchovy)=="factor") dt1$Northern.Anchovy <-as.numeric(levels(dt1$Northern.Anchovy))[as.integer(dt1$Northern.Anchovy) ]               
if (class(dt1$Northern.Anchovy)=="character") dt1$Northern.Anchovy <-as.numeric(dt1$Northern.Anchovy)
if (class(dt1$Pacific.Herring)=="factor") dt1$Pacific.Herring <-as.numeric(levels(dt1$Pacific.Herring))[as.integer(dt1$Pacific.Herring) ]               
if (class(dt1$Pacific.Herring)=="character") dt1$Pacific.Herring <-as.numeric(dt1$Pacific.Herring)
if (class(dt1$Pacific.Lamprey)=="factor") dt1$Pacific.Lamprey <-as.numeric(levels(dt1$Pacific.Lamprey))[as.integer(dt1$Pacific.Lamprey) ]               
if (class(dt1$Pacific.Lamprey)=="character") dt1$Pacific.Lamprey <-as.numeric(dt1$Pacific.Lamprey)
if (class(dt1$Pacific.Staghorn.Sculpin)=="factor") dt1$Pacific.Staghorn.Sculpin <-as.numeric(levels(dt1$Pacific.Staghorn.Sculpin))[as.integer(dt1$Pacific.Staghorn.Sculpin) ]               
if (class(dt1$Pacific.Staghorn.Sculpin)=="character") dt1$Pacific.Staghorn.Sculpin <-as.numeric(dt1$Pacific.Staghorn.Sculpin)
if (class(dt1$Plainfin.Midshipman)=="factor") dt1$Plainfin.Midshipman <-as.numeric(levels(dt1$Plainfin.Midshipman))[as.integer(dt1$Plainfin.Midshipman) ]               
if (class(dt1$Plainfin.Midshipman)=="character") dt1$Plainfin.Midshipman <-as.numeric(dt1$Plainfin.Midshipman)
if (class(dt1$Prickly.Sculpin)=="factor") dt1$Prickly.Sculpin <-as.numeric(levels(dt1$Prickly.Sculpin))[as.integer(dt1$Prickly.Sculpin) ]               
if (class(dt1$Prickly.Sculpin)=="character") dt1$Prickly.Sculpin <-as.numeric(dt1$Prickly.Sculpin)
if (class(dt1$Rainwater.Killifish)=="factor") dt1$Rainwater.Killifish <-as.numeric(levels(dt1$Rainwater.Killifish))[as.integer(dt1$Rainwater.Killifish) ]               
if (class(dt1$Rainwater.Killifish)=="character") dt1$Rainwater.Killifish <-as.numeric(dt1$Rainwater.Killifish)
if (class(dt1$Redear.Sunfish)=="factor") dt1$Redear.Sunfish <-as.numeric(levels(dt1$Redear.Sunfish))[as.integer(dt1$Redear.Sunfish) ]               
if (class(dt1$Redear.Sunfish)=="character") dt1$Redear.Sunfish <-as.numeric(dt1$Redear.Sunfish)
if (class(dt1$River.Lamprey)=="factor") dt1$River.Lamprey <-as.numeric(levels(dt1$River.Lamprey))[as.integer(dt1$River.Lamprey) ]               
if (class(dt1$River.Lamprey)=="character") dt1$River.Lamprey <-as.numeric(dt1$River.Lamprey)
if (class(dt1$Sacramento.Blackfish)=="factor") dt1$Sacramento.Blackfish <-as.numeric(levels(dt1$Sacramento.Blackfish))[as.integer(dt1$Sacramento.Blackfish) ]               
if (class(dt1$Sacramento.Blackfish)=="character") dt1$Sacramento.Blackfish <-as.numeric(dt1$Sacramento.Blackfish)
if (class(dt1$Sacramento.Pikeminnow)=="factor") dt1$Sacramento.Pikeminnow <-as.numeric(levels(dt1$Sacramento.Pikeminnow))[as.integer(dt1$Sacramento.Pikeminnow) ]               
if (class(dt1$Sacramento.Pikeminnow)=="character") dt1$Sacramento.Pikeminnow <-as.numeric(dt1$Sacramento.Pikeminnow)
if (class(dt1$Sacramento.Sucker)=="factor") dt1$Sacramento.Sucker <-as.numeric(levels(dt1$Sacramento.Sucker))[as.integer(dt1$Sacramento.Sucker) ]               
if (class(dt1$Sacramento.Sucker)=="character") dt1$Sacramento.Sucker <-as.numeric(dt1$Sacramento.Sucker)
if (class(dt1$Sculpins..paren.Unid.paren.)=="factor") dt1$Sculpins..paren.Unid.paren. <-as.numeric(levels(dt1$Sculpins..paren.Unid.paren.))[as.integer(dt1$Sculpins..paren.Unid.paren.) ]               
if (class(dt1$Sculpins..paren.Unid.paren.)=="character") dt1$Sculpins..paren.Unid.paren. <-as.numeric(dt1$Sculpins..paren.Unid.paren.)
if (class(dt1$Shimofuri.Goby)=="factor") dt1$Shimofuri.Goby <-as.numeric(levels(dt1$Shimofuri.Goby))[as.integer(dt1$Shimofuri.Goby) ]               
if (class(dt1$Shimofuri.Goby)=="character") dt1$Shimofuri.Goby <-as.numeric(dt1$Shimofuri.Goby)
if (class(dt1$Shiner.Perch)=="factor") dt1$Shiner.Perch <-as.numeric(levels(dt1$Shiner.Perch))[as.integer(dt1$Shiner.Perch) ]               
if (class(dt1$Shiner.Perch)=="character") dt1$Shiner.Perch <-as.numeric(dt1$Shiner.Perch)
if (class(dt1$Shokihaze.Goby)=="factor") dt1$Shokihaze.Goby <-as.numeric(levels(dt1$Shokihaze.Goby))[as.integer(dt1$Shokihaze.Goby) ]               
if (class(dt1$Shokihaze.Goby)=="character") dt1$Shokihaze.Goby <-as.numeric(dt1$Shokihaze.Goby)
if (class(dt1$Silversides..paren.Unid.paren.)=="factor") dt1$Silversides..paren.Unid.paren. <-as.numeric(levels(dt1$Silversides..paren.Unid.paren.))[as.integer(dt1$Silversides..paren.Unid.paren.) ]               
if (class(dt1$Silversides..paren.Unid.paren.)=="character") dt1$Silversides..paren.Unid.paren. <-as.numeric(dt1$Silversides..paren.Unid.paren.)
if (class(dt1$Smallmouth.Bass)=="factor") dt1$Smallmouth.Bass <-as.numeric(levels(dt1$Smallmouth.Bass))[as.integer(dt1$Smallmouth.Bass) ]               
if (class(dt1$Smallmouth.Bass)=="character") dt1$Smallmouth.Bass <-as.numeric(dt1$Smallmouth.Bass)
if (class(dt1$Smelt..paren.Unid.paren.)=="factor") dt1$Smelt..paren.Unid.paren. <-as.numeric(levels(dt1$Smelt..paren.Unid.paren.))[as.integer(dt1$Smelt..paren.Unid.paren.) ]               
if (class(dt1$Smelt..paren.Unid.paren.)=="character") dt1$Smelt..paren.Unid.paren. <-as.numeric(dt1$Smelt..paren.Unid.paren.)
if (class(dt1$Speckled.Sanddab)=="factor") dt1$Speckled.Sanddab <-as.numeric(levels(dt1$Speckled.Sanddab))[as.integer(dt1$Speckled.Sanddab) ]               
if (class(dt1$Speckled.Sanddab)=="character") dt1$Speckled.Sanddab <-as.numeric(dt1$Speckled.Sanddab)
if (class(dt1$Splittail)=="factor") dt1$Splittail <-as.numeric(levels(dt1$Splittail))[as.integer(dt1$Splittail) ]               
if (class(dt1$Splittail)=="character") dt1$Splittail <-as.numeric(dt1$Splittail)
if (class(dt1$Spotted.Bass)=="factor") dt1$Spotted.Bass <-as.numeric(levels(dt1$Spotted.Bass))[as.integer(dt1$Spotted.Bass) ]               
if (class(dt1$Spotted.Bass)=="character") dt1$Spotted.Bass <-as.numeric(dt1$Spotted.Bass)
if (class(dt1$Starry.Flounder)=="factor") dt1$Starry.Flounder <-as.numeric(levels(dt1$Starry.Flounder))[as.integer(dt1$Starry.Flounder) ]               
if (class(dt1$Starry.Flounder)=="character") dt1$Starry.Flounder <-as.numeric(dt1$Starry.Flounder)
if (class(dt1$Striped.Bass)=="factor") dt1$Striped.Bass <-as.numeric(levels(dt1$Striped.Bass))[as.integer(dt1$Striped.Bass) ]               
if (class(dt1$Striped.Bass)=="character") dt1$Striped.Bass <-as.numeric(dt1$Striped.Bass)
if (class(dt1$Sturgeon..paren.Unid.paren.)=="factor") dt1$Sturgeon..paren.Unid.paren. <-as.numeric(levels(dt1$Sturgeon..paren.Unid.paren.))[as.integer(dt1$Sturgeon..paren.Unid.paren.) ]               
if (class(dt1$Sturgeon..paren.Unid.paren.)=="character") dt1$Sturgeon..paren.Unid.paren. <-as.numeric(dt1$Sturgeon..paren.Unid.paren.)
if (class(dt1$Threadfin.Shad)=="factor") dt1$Threadfin.Shad <-as.numeric(levels(dt1$Threadfin.Shad))[as.integer(dt1$Threadfin.Shad) ]               
if (class(dt1$Threadfin.Shad)=="character") dt1$Threadfin.Shad <-as.numeric(dt1$Threadfin.Shad)
if (class(dt1$Threespine.Stickleback)=="factor") dt1$Threespine.Stickleback <-as.numeric(levels(dt1$Threespine.Stickleback))[as.integer(dt1$Threespine.Stickleback) ]               
if (class(dt1$Threespine.Stickleback)=="character") dt1$Threespine.Stickleback <-as.numeric(dt1$Threespine.Stickleback)
if (class(dt1$Topsmelt)=="factor") dt1$Topsmelt <-as.numeric(levels(dt1$Topsmelt))[as.integer(dt1$Topsmelt) ]               
if (class(dt1$Topsmelt)=="character") dt1$Topsmelt <-as.numeric(dt1$Topsmelt)
if (class(dt1$Tridentiger.spp_)=="factor") dt1$Tridentiger.spp_ <-as.numeric(levels(dt1$Tridentiger.spp_))[as.integer(dt1$Tridentiger.spp_) ]               
if (class(dt1$Tridentiger.spp_)=="character") dt1$Tridentiger.spp_ <-as.numeric(dt1$Tridentiger.spp_)
if (class(dt1$Tule.Perch)=="factor") dt1$Tule.Perch <-as.numeric(levels(dt1$Tule.Perch))[as.integer(dt1$Tule.Perch) ]               
if (class(dt1$Tule.Perch)=="character") dt1$Tule.Perch <-as.numeric(dt1$Tule.Perch)
if (class(dt1$Unknown)=="factor") dt1$Unknown <-as.numeric(levels(dt1$Unknown))[as.integer(dt1$Unknown) ]               
if (class(dt1$Unknown)=="character") dt1$Unknown <-as.numeric(dt1$Unknown)
if (class(dt1$Wakasagi)=="factor") dt1$Wakasagi <-as.numeric(levels(dt1$Wakasagi))[as.integer(dt1$Wakasagi) ]               
if (class(dt1$Wakasagi)=="character") dt1$Wakasagi <-as.numeric(dt1$Wakasagi)
if (class(dt1$White.Catfish)=="factor") dt1$White.Catfish <-as.numeric(levels(dt1$White.Catfish))[as.integer(dt1$White.Catfish) ]               
if (class(dt1$White.Catfish)=="character") dt1$White.Catfish <-as.numeric(dt1$White.Catfish)
if (class(dt1$White.Crappie)=="factor") dt1$White.Crappie <-as.numeric(levels(dt1$White.Crappie))[as.integer(dt1$White.Crappie) ]               
if (class(dt1$White.Crappie)=="character") dt1$White.Crappie <-as.numeric(dt1$White.Crappie)
if (class(dt1$White.Croaker)=="factor") dt1$White.Croaker <-as.numeric(levels(dt1$White.Croaker))[as.integer(dt1$White.Croaker) ]               
if (class(dt1$White.Croaker)=="character") dt1$White.Croaker <-as.numeric(dt1$White.Croaker)
if (class(dt1$White.Sturgeon)=="factor") dt1$White.Sturgeon <-as.numeric(levels(dt1$White.Sturgeon))[as.integer(dt1$White.Sturgeon) ]               
if (class(dt1$White.Sturgeon)=="character") dt1$White.Sturgeon <-as.numeric(dt1$White.Sturgeon)
if (class(dt1$Yellowfin.Goby)=="factor") dt1$Yellowfin.Goby <-as.numeric(levels(dt1$Yellowfin.Goby))[as.integer(dt1$Yellowfin.Goby) ]               
if (class(dt1$Yellowfin.Goby)=="character") dt1$Yellowfin.Goby <-as.numeric(dt1$Yellowfin.Goby)

# Convert Missing Values to NA for non-dates

dt1$Year <- ifelse((trimws(as.character(dt1$Year))==trimws("NA")),NA,dt1$Year)               
suppressWarnings(dt1$Year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Year))==as.character(as.numeric("NA"))),NA,dt1$Year))
dt1$qry_AMC_EDI_01.Survey.Survey <- as.factor(ifelse((trimws(as.character(dt1$qry_AMC_EDI_01.Survey.Survey))==trimws("NA")),NA,as.character(dt1$qry_AMC_EDI_01.Survey.Survey)))
dt1$qry_AMC_EDI_01.Station.Station <- as.factor(ifelse((trimws(as.character(dt1$qry_AMC_EDI_01.Station.Station))==trimws("NA")),NA,as.character(dt1$qry_AMC_EDI_01.Station.Station)))
dt1$Latitude <- as.factor(ifelse((trimws(as.character(dt1$Latitude))==trimws("NA")),NA,as.character(dt1$Latitude)))
dt1$Longitude <- as.factor(ifelse((trimws(as.character(dt1$Longitude))==trimws("NA")),NA,as.character(dt1$Longitude)))
dt1$Duration <- ifelse((trimws(as.character(dt1$Duration))==trimws("NA")),NA,dt1$Duration)               
suppressWarnings(dt1$Duration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Duration))==as.character(as.numeric("NA"))),NA,dt1$Duration))
dt1$TowNum <- as.factor(ifelse((trimws(as.character(dt1$TowNum))==trimws("NA")),NA,as.character(dt1$TowNum)))
dt1$Volume <- ifelse((trimws(as.character(dt1$Volume))==trimws("NA")),NA,dt1$Volume)               
suppressWarnings(dt1$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Volume))==as.character(as.numeric("NA"))),NA,dt1$Volume))
dt1$BottomDepth <- ifelse((trimws(as.character(dt1$BottomDepth))==trimws("NA")),NA,dt1$BottomDepth)               
suppressWarnings(dt1$BottomDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BottomDepth))==as.character(as.numeric("NA"))),NA,dt1$BottomDepth))
dt1$Temp <- ifelse((trimws(as.character(dt1$Temp))==trimws("NA")),NA,dt1$Temp)               
suppressWarnings(dt1$Temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Temp))==as.character(as.numeric("NA"))),NA,dt1$Temp))
dt1$TopEC <- ifelse((trimws(as.character(dt1$TopEC))==trimws("NA")),NA,dt1$TopEC)               
suppressWarnings(dt1$TopEC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TopEC))==as.character(as.numeric("NA"))),NA,dt1$TopEC))
dt1$BottomEC <- ifelse((trimws(as.character(dt1$BottomEC))==trimws("NA")),NA,dt1$BottomEC)               
suppressWarnings(dt1$BottomEC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BottomEC))==as.character(as.numeric("NA"))),NA,dt1$BottomEC))
dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("NA")),NA,dt1$Secchi)               
suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("NA"))),NA,dt1$Secchi))
dt1$Turbidity <- ifelse((trimws(as.character(dt1$Turbidity))==trimws("NA")),NA,dt1$Turbidity)               
suppressWarnings(dt1$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Turbidity))==as.character(as.numeric("NA"))),NA,dt1$Turbidity))
dt1$American.Shad <- ifelse((trimws(as.character(dt1$American.Shad))==trimws("NA")),NA,dt1$American.Shad)               
suppressWarnings(dt1$American.Shad <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$American.Shad))==as.character(as.numeric("NA"))),NA,dt1$American.Shad))
dt1$Arrow.Goby <- ifelse((trimws(as.character(dt1$Arrow.Goby))==trimws("NA")),NA,dt1$Arrow.Goby)               
suppressWarnings(dt1$Arrow.Goby <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Arrow.Goby))==as.character(as.numeric("NA"))),NA,dt1$Arrow.Goby))
dt1$Bay.Goby <- ifelse((trimws(as.character(dt1$Bay.Goby))==trimws("NA")),NA,dt1$Bay.Goby)               
suppressWarnings(dt1$Bay.Goby <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Bay.Goby))==as.character(as.numeric("NA"))),NA,dt1$Bay.Goby))
dt1$Bay.Pipefish <- ifelse((trimws(as.character(dt1$Bay.Pipefish))==trimws("NA")),NA,dt1$Bay.Pipefish)               
suppressWarnings(dt1$Bay.Pipefish <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Bay.Pipefish))==as.character(as.numeric("NA"))),NA,dt1$Bay.Pipefish))
dt1$Bigscale.Logperch <- ifelse((trimws(as.character(dt1$Bigscale.Logperch))==trimws("NA")),NA,dt1$Bigscale.Logperch)               
suppressWarnings(dt1$Bigscale.Logperch <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Bigscale.Logperch))==as.character(as.numeric("NA"))),NA,dt1$Bigscale.Logperch))
dt1$Black.Bullhead <- ifelse((trimws(as.character(dt1$Black.Bullhead))==trimws("NA")),NA,dt1$Black.Bullhead)               
suppressWarnings(dt1$Black.Bullhead <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Black.Bullhead))==as.character(as.numeric("NA"))),NA,dt1$Black.Bullhead))
dt1$Black.Crappie <- ifelse((trimws(as.character(dt1$Black.Crappie))==trimws("NA")),NA,dt1$Black.Crappie)               
suppressWarnings(dt1$Black.Crappie <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Black.Crappie))==as.character(as.numeric("NA"))),NA,dt1$Black.Crappie))
dt1$Bluegill.Sunfish <- ifelse((trimws(as.character(dt1$Bluegill.Sunfish))==trimws("NA")),NA,dt1$Bluegill.Sunfish)               
suppressWarnings(dt1$Bluegill.Sunfish <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Bluegill.Sunfish))==as.character(as.numeric("NA"))),NA,dt1$Bluegill.Sunfish))
dt1$Brown.Bullhead <- ifelse((trimws(as.character(dt1$Brown.Bullhead))==trimws("NA")),NA,dt1$Brown.Bullhead)               
suppressWarnings(dt1$Brown.Bullhead <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Brown.Bullhead))==as.character(as.numeric("NA"))),NA,dt1$Brown.Bullhead))
dt1$Brown.Rockfish <- ifelse((trimws(as.character(dt1$Brown.Rockfish))==trimws("NA")),NA,dt1$Brown.Rockfish)               
suppressWarnings(dt1$Brown.Rockfish <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Brown.Rockfish))==as.character(as.numeric("NA"))),NA,dt1$Brown.Rockfish))
dt1$California.Tonguefish <- ifelse((trimws(as.character(dt1$California.Tonguefish))==trimws("NA")),NA,dt1$California.Tonguefish)               
suppressWarnings(dt1$California.Tonguefish <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$California.Tonguefish))==as.character(as.numeric("NA"))),NA,dt1$California.Tonguefish))
dt1$Carp <- ifelse((trimws(as.character(dt1$Carp))==trimws("NA")),NA,dt1$Carp)               
suppressWarnings(dt1$Carp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Carp))==as.character(as.numeric("NA"))),NA,dt1$Carp))
dt1$Catfish..paren.Unid.paren. <- ifelse((trimws(as.character(dt1$Catfish..paren.Unid.paren.))==trimws("NA")),NA,dt1$Catfish..paren.Unid.paren.)               
suppressWarnings(dt1$Catfish..paren.Unid.paren. <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Catfish..paren.Unid.paren.))==as.character(as.numeric("NA"))),NA,dt1$Catfish..paren.Unid.paren.))
dt1$Centrarchids..paren.Unid.paren. <- ifelse((trimws(as.character(dt1$Centrarchids..paren.Unid.paren.))==trimws("NA")),NA,dt1$Centrarchids..paren.Unid.paren.)               
suppressWarnings(dt1$Centrarchids..paren.Unid.paren. <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Centrarchids..paren.Unid.paren.))==as.character(as.numeric("NA"))),NA,dt1$Centrarchids..paren.Unid.paren.))
dt1$Chameleon.Goby <- ifelse((trimws(as.character(dt1$Chameleon.Goby))==trimws("NA")),NA,dt1$Chameleon.Goby)               
suppressWarnings(dt1$Chameleon.Goby <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Chameleon.Goby))==as.character(as.numeric("NA"))),NA,dt1$Chameleon.Goby))
dt1$Channel.Catfish <- ifelse((trimws(as.character(dt1$Channel.Catfish))==trimws("NA")),NA,dt1$Channel.Catfish)               
suppressWarnings(dt1$Channel.Catfish <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Channel.Catfish))==as.character(as.numeric("NA"))),NA,dt1$Channel.Catfish))
dt1$Cheekspot.Goby <- ifelse((trimws(as.character(dt1$Cheekspot.Goby))==trimws("NA")),NA,dt1$Cheekspot.Goby)               
suppressWarnings(dt1$Cheekspot.Goby <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Cheekspot.Goby))==as.character(as.numeric("NA"))),NA,dt1$Cheekspot.Goby))
dt1$Chinook.Salmon <- ifelse((trimws(as.character(dt1$Chinook.Salmon))==trimws("NA")),NA,dt1$Chinook.Salmon)               
suppressWarnings(dt1$Chinook.Salmon <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Chinook.Salmon))==as.character(as.numeric("NA"))),NA,dt1$Chinook.Salmon))
dt1$Cyprinids..paren.Unid.paren. <- ifelse((trimws(as.character(dt1$Cyprinids..paren.Unid.paren.))==trimws("NA")),NA,dt1$Cyprinids..paren.Unid.paren.)               
suppressWarnings(dt1$Cyprinids..paren.Unid.paren. <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Cyprinids..paren.Unid.paren.))==as.character(as.numeric("NA"))),NA,dt1$Cyprinids..paren.Unid.paren.))
dt1$Delta.Smelt <- ifelse((trimws(as.character(dt1$Delta.Smelt))==trimws("NA")),NA,dt1$Delta.Smelt)               
suppressWarnings(dt1$Delta.Smelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Delta.Smelt))==as.character(as.numeric("NA"))),NA,dt1$Delta.Smelt))
dt1$English.Sole <- ifelse((trimws(as.character(dt1$English.Sole))==trimws("NA")),NA,dt1$English.Sole)               
suppressWarnings(dt1$English.Sole <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$English.Sole))==as.character(as.numeric("NA"))),NA,dt1$English.Sole))
dt1$Gobies..paren.Unid.paren. <- ifelse((trimws(as.character(dt1$Gobies..paren.Unid.paren.))==trimws("NA")),NA,dt1$Gobies..paren.Unid.paren.)               
suppressWarnings(dt1$Gobies..paren.Unid.paren. <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Gobies..paren.Unid.paren.))==as.character(as.numeric("NA"))),NA,dt1$Gobies..paren.Unid.paren.))
dt1$Golden.Shiner <- ifelse((trimws(as.character(dt1$Golden.Shiner))==trimws("NA")),NA,dt1$Golden.Shiner)               
suppressWarnings(dt1$Golden.Shiner <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Golden.Shiner))==as.character(as.numeric("NA"))),NA,dt1$Golden.Shiner))
dt1$Goldfish <- ifelse((trimws(as.character(dt1$Goldfish))==trimws("NA")),NA,dt1$Goldfish)               
suppressWarnings(dt1$Goldfish <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Goldfish))==as.character(as.numeric("NA"))),NA,dt1$Goldfish))
dt1$Hitch <- ifelse((trimws(as.character(dt1$Hitch))==trimws("NA")),NA,dt1$Hitch)               
suppressWarnings(dt1$Hitch <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Hitch))==as.character(as.numeric("NA"))),NA,dt1$Hitch))
dt1$Inland.Silverside <- ifelse((trimws(as.character(dt1$Inland.Silverside))==trimws("NA")),NA,dt1$Inland.Silverside)               
suppressWarnings(dt1$Inland.Silverside <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Inland.Silverside))==as.character(as.numeric("NA"))),NA,dt1$Inland.Silverside))
dt1$Jacksmelt <- ifelse((trimws(as.character(dt1$Jacksmelt))==trimws("NA")),NA,dt1$Jacksmelt)               
suppressWarnings(dt1$Jacksmelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Jacksmelt))==as.character(as.numeric("NA"))),NA,dt1$Jacksmelt))
dt1$Lampreys..paren.Unid.paren. <- ifelse((trimws(as.character(dt1$Lampreys..paren.Unid.paren.))==trimws("NA")),NA,dt1$Lampreys..paren.Unid.paren.)               
suppressWarnings(dt1$Lampreys..paren.Unid.paren. <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Lampreys..paren.Unid.paren.))==as.character(as.numeric("NA"))),NA,dt1$Lampreys..paren.Unid.paren.))
dt1$Largemouth.Bass <- ifelse((trimws(as.character(dt1$Largemouth.Bass))==trimws("NA")),NA,dt1$Largemouth.Bass)               
suppressWarnings(dt1$Largemouth.Bass <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Largemouth.Bass))==as.character(as.numeric("NA"))),NA,dt1$Largemouth.Bass))
dt1$Longfin.Smelt <- ifelse((trimws(as.character(dt1$Longfin.Smelt))==trimws("NA")),NA,dt1$Longfin.Smelt)               
suppressWarnings(dt1$Longfin.Smelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Longfin.Smelt))==as.character(as.numeric("NA"))),NA,dt1$Longfin.Smelt))
dt1$Longjaw.Mudsucker <- ifelse((trimws(as.character(dt1$Longjaw.Mudsucker))==trimws("NA")),NA,dt1$Longjaw.Mudsucker)               
suppressWarnings(dt1$Longjaw.Mudsucker <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Longjaw.Mudsucker))==as.character(as.numeric("NA"))),NA,dt1$Longjaw.Mudsucker))
dt1$Mosquitofish <- ifelse((trimws(as.character(dt1$Mosquitofish))==trimws("NA")),NA,dt1$Mosquitofish)               
suppressWarnings(dt1$Mosquitofish <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Mosquitofish))==as.character(as.numeric("NA"))),NA,dt1$Mosquitofish))
dt1$Northern.Anchovy <- ifelse((trimws(as.character(dt1$Northern.Anchovy))==trimws("NA")),NA,dt1$Northern.Anchovy)               
suppressWarnings(dt1$Northern.Anchovy <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Northern.Anchovy))==as.character(as.numeric("NA"))),NA,dt1$Northern.Anchovy))
dt1$Pacific.Herring <- ifelse((trimws(as.character(dt1$Pacific.Herring))==trimws("NA")),NA,dt1$Pacific.Herring)               
suppressWarnings(dt1$Pacific.Herring <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Pacific.Herring))==as.character(as.numeric("NA"))),NA,dt1$Pacific.Herring))
dt1$Pacific.Lamprey <- ifelse((trimws(as.character(dt1$Pacific.Lamprey))==trimws("NA")),NA,dt1$Pacific.Lamprey)               
suppressWarnings(dt1$Pacific.Lamprey <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Pacific.Lamprey))==as.character(as.numeric("NA"))),NA,dt1$Pacific.Lamprey))
dt1$Pacific.Staghorn.Sculpin <- ifelse((trimws(as.character(dt1$Pacific.Staghorn.Sculpin))==trimws("NA")),NA,dt1$Pacific.Staghorn.Sculpin)               
suppressWarnings(dt1$Pacific.Staghorn.Sculpin <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Pacific.Staghorn.Sculpin))==as.character(as.numeric("NA"))),NA,dt1$Pacific.Staghorn.Sculpin))
dt1$Plainfin.Midshipman <- ifelse((trimws(as.character(dt1$Plainfin.Midshipman))==trimws("NA")),NA,dt1$Plainfin.Midshipman)               
suppressWarnings(dt1$Plainfin.Midshipman <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Plainfin.Midshipman))==as.character(as.numeric("NA"))),NA,dt1$Plainfin.Midshipman))
dt1$Prickly.Sculpin <- ifelse((trimws(as.character(dt1$Prickly.Sculpin))==trimws("NA")),NA,dt1$Prickly.Sculpin)               
suppressWarnings(dt1$Prickly.Sculpin <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Prickly.Sculpin))==as.character(as.numeric("NA"))),NA,dt1$Prickly.Sculpin))
dt1$Rainwater.Killifish <- ifelse((trimws(as.character(dt1$Rainwater.Killifish))==trimws("NA")),NA,dt1$Rainwater.Killifish)               
suppressWarnings(dt1$Rainwater.Killifish <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Rainwater.Killifish))==as.character(as.numeric("NA"))),NA,dt1$Rainwater.Killifish))
dt1$Redear.Sunfish <- ifelse((trimws(as.character(dt1$Redear.Sunfish))==trimws("NA")),NA,dt1$Redear.Sunfish)               
suppressWarnings(dt1$Redear.Sunfish <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Redear.Sunfish))==as.character(as.numeric("NA"))),NA,dt1$Redear.Sunfish))
dt1$River.Lamprey <- ifelse((trimws(as.character(dt1$River.Lamprey))==trimws("NA")),NA,dt1$River.Lamprey)               
suppressWarnings(dt1$River.Lamprey <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$River.Lamprey))==as.character(as.numeric("NA"))),NA,dt1$River.Lamprey))
dt1$Sacramento.Blackfish <- ifelse((trimws(as.character(dt1$Sacramento.Blackfish))==trimws("NA")),NA,dt1$Sacramento.Blackfish)               
suppressWarnings(dt1$Sacramento.Blackfish <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Sacramento.Blackfish))==as.character(as.numeric("NA"))),NA,dt1$Sacramento.Blackfish))
dt1$Sacramento.Pikeminnow <- ifelse((trimws(as.character(dt1$Sacramento.Pikeminnow))==trimws("NA")),NA,dt1$Sacramento.Pikeminnow)               
suppressWarnings(dt1$Sacramento.Pikeminnow <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Sacramento.Pikeminnow))==as.character(as.numeric("NA"))),NA,dt1$Sacramento.Pikeminnow))
dt1$Sacramento.Sucker <- ifelse((trimws(as.character(dt1$Sacramento.Sucker))==trimws("NA")),NA,dt1$Sacramento.Sucker)               
suppressWarnings(dt1$Sacramento.Sucker <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Sacramento.Sucker))==as.character(as.numeric("NA"))),NA,dt1$Sacramento.Sucker))
dt1$Sculpins..paren.Unid.paren. <- ifelse((trimws(as.character(dt1$Sculpins..paren.Unid.paren.))==trimws("NA")),NA,dt1$Sculpins..paren.Unid.paren.)               
suppressWarnings(dt1$Sculpins..paren.Unid.paren. <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Sculpins..paren.Unid.paren.))==as.character(as.numeric("NA"))),NA,dt1$Sculpins..paren.Unid.paren.))
dt1$Shimofuri.Goby <- ifelse((trimws(as.character(dt1$Shimofuri.Goby))==trimws("NA")),NA,dt1$Shimofuri.Goby)               
suppressWarnings(dt1$Shimofuri.Goby <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Shimofuri.Goby))==as.character(as.numeric("NA"))),NA,dt1$Shimofuri.Goby))
dt1$Shiner.Perch <- ifelse((trimws(as.character(dt1$Shiner.Perch))==trimws("NA")),NA,dt1$Shiner.Perch)               
suppressWarnings(dt1$Shiner.Perch <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Shiner.Perch))==as.character(as.numeric("NA"))),NA,dt1$Shiner.Perch))
dt1$Shokihaze.Goby <- ifelse((trimws(as.character(dt1$Shokihaze.Goby))==trimws("NA")),NA,dt1$Shokihaze.Goby)               
suppressWarnings(dt1$Shokihaze.Goby <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Shokihaze.Goby))==as.character(as.numeric("NA"))),NA,dt1$Shokihaze.Goby))
dt1$Silversides..paren.Unid.paren. <- ifelse((trimws(as.character(dt1$Silversides..paren.Unid.paren.))==trimws("NA")),NA,dt1$Silversides..paren.Unid.paren.)               
suppressWarnings(dt1$Silversides..paren.Unid.paren. <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Silversides..paren.Unid.paren.))==as.character(as.numeric("NA"))),NA,dt1$Silversides..paren.Unid.paren.))
dt1$Smallmouth.Bass <- ifelse((trimws(as.character(dt1$Smallmouth.Bass))==trimws("NA")),NA,dt1$Smallmouth.Bass)               
suppressWarnings(dt1$Smallmouth.Bass <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Smallmouth.Bass))==as.character(as.numeric("NA"))),NA,dt1$Smallmouth.Bass))
dt1$Smelt..paren.Unid.paren. <- ifelse((trimws(as.character(dt1$Smelt..paren.Unid.paren.))==trimws("NA")),NA,dt1$Smelt..paren.Unid.paren.)               
suppressWarnings(dt1$Smelt..paren.Unid.paren. <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Smelt..paren.Unid.paren.))==as.character(as.numeric("NA"))),NA,dt1$Smelt..paren.Unid.paren.))
dt1$Speckled.Sanddab <- ifelse((trimws(as.character(dt1$Speckled.Sanddab))==trimws("NA")),NA,dt1$Speckled.Sanddab)               
suppressWarnings(dt1$Speckled.Sanddab <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Speckled.Sanddab))==as.character(as.numeric("NA"))),NA,dt1$Speckled.Sanddab))
dt1$Splittail <- ifelse((trimws(as.character(dt1$Splittail))==trimws("NA")),NA,dt1$Splittail)               
suppressWarnings(dt1$Splittail <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Splittail))==as.character(as.numeric("NA"))),NA,dt1$Splittail))
dt1$Spotted.Bass <- ifelse((trimws(as.character(dt1$Spotted.Bass))==trimws("NA")),NA,dt1$Spotted.Bass)               
suppressWarnings(dt1$Spotted.Bass <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Spotted.Bass))==as.character(as.numeric("NA"))),NA,dt1$Spotted.Bass))
dt1$Starry.Flounder <- ifelse((trimws(as.character(dt1$Starry.Flounder))==trimws("NA")),NA,dt1$Starry.Flounder)               
suppressWarnings(dt1$Starry.Flounder <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Starry.Flounder))==as.character(as.numeric("NA"))),NA,dt1$Starry.Flounder))
dt1$Striped.Bass <- ifelse((trimws(as.character(dt1$Striped.Bass))==trimws("NA")),NA,dt1$Striped.Bass)               
suppressWarnings(dt1$Striped.Bass <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Striped.Bass))==as.character(as.numeric("NA"))),NA,dt1$Striped.Bass))
dt1$Sturgeon..paren.Unid.paren. <- ifelse((trimws(as.character(dt1$Sturgeon..paren.Unid.paren.))==trimws("NA")),NA,dt1$Sturgeon..paren.Unid.paren.)               
suppressWarnings(dt1$Sturgeon..paren.Unid.paren. <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Sturgeon..paren.Unid.paren.))==as.character(as.numeric("NA"))),NA,dt1$Sturgeon..paren.Unid.paren.))
dt1$Threadfin.Shad <- ifelse((trimws(as.character(dt1$Threadfin.Shad))==trimws("NA")),NA,dt1$Threadfin.Shad)               
suppressWarnings(dt1$Threadfin.Shad <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Threadfin.Shad))==as.character(as.numeric("NA"))),NA,dt1$Threadfin.Shad))
dt1$Threespine.Stickleback <- ifelse((trimws(as.character(dt1$Threespine.Stickleback))==trimws("NA")),NA,dt1$Threespine.Stickleback)               
suppressWarnings(dt1$Threespine.Stickleback <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Threespine.Stickleback))==as.character(as.numeric("NA"))),NA,dt1$Threespine.Stickleback))
dt1$Topsmelt <- ifelse((trimws(as.character(dt1$Topsmelt))==trimws("NA")),NA,dt1$Topsmelt)               
suppressWarnings(dt1$Topsmelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Topsmelt))==as.character(as.numeric("NA"))),NA,dt1$Topsmelt))
dt1$Tridentiger.spp_ <- ifelse((trimws(as.character(dt1$Tridentiger.spp_))==trimws("NA")),NA,dt1$Tridentiger.spp_)               
suppressWarnings(dt1$Tridentiger.spp_ <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Tridentiger.spp_))==as.character(as.numeric("NA"))),NA,dt1$Tridentiger.spp_))
dt1$Tule.Perch <- ifelse((trimws(as.character(dt1$Tule.Perch))==trimws("NA")),NA,dt1$Tule.Perch)               
suppressWarnings(dt1$Tule.Perch <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Tule.Perch))==as.character(as.numeric("NA"))),NA,dt1$Tule.Perch))
dt1$Unknown <- ifelse((trimws(as.character(dt1$Unknown))==trimws("NA")),NA,dt1$Unknown)               
suppressWarnings(dt1$Unknown <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Unknown))==as.character(as.numeric("NA"))),NA,dt1$Unknown))
dt1$Wakasagi <- ifelse((trimws(as.character(dt1$Wakasagi))==trimws("NA")),NA,dt1$Wakasagi)               
suppressWarnings(dt1$Wakasagi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Wakasagi))==as.character(as.numeric("NA"))),NA,dt1$Wakasagi))
dt1$White.Catfish <- ifelse((trimws(as.character(dt1$White.Catfish))==trimws("NA")),NA,dt1$White.Catfish)               
suppressWarnings(dt1$White.Catfish <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$White.Catfish))==as.character(as.numeric("NA"))),NA,dt1$White.Catfish))
dt1$White.Crappie <- ifelse((trimws(as.character(dt1$White.Crappie))==trimws("NA")),NA,dt1$White.Crappie)               
suppressWarnings(dt1$White.Crappie <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$White.Crappie))==as.character(as.numeric("NA"))),NA,dt1$White.Crappie))
dt1$White.Croaker <- ifelse((trimws(as.character(dt1$White.Croaker))==trimws("NA")),NA,dt1$White.Croaker)               
suppressWarnings(dt1$White.Croaker <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$White.Croaker))==as.character(as.numeric("NA"))),NA,dt1$White.Croaker))
dt1$White.Sturgeon <- ifelse((trimws(as.character(dt1$White.Sturgeon))==trimws("NA")),NA,dt1$White.Sturgeon)               
suppressWarnings(dt1$White.Sturgeon <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$White.Sturgeon))==as.character(as.numeric("NA"))),NA,dt1$White.Sturgeon))
dt1$Yellowfin.Goby <- ifelse((trimws(as.character(dt1$Yellowfin.Goby))==trimws("NA")),NA,dt1$Yellowfin.Goby)               
suppressWarnings(dt1$Yellowfin.Goby <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Yellowfin.Goby))==as.character(as.numeric("NA"))),NA,dt1$Yellowfin.Goby))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Year)
summary(qry_AMC_EDI_01.Survey.Survey)
summary(SampleDate)
summary(TowTime)
summary(qry_AMC_EDI_01.Station.Station)
summary(Latitude)
summary(Longitude)
summary(Duration)
summary(TowNum)
summary(GearCode)
summary(Volume)
summary(Tide)
summary(BottomDepth)
summary(Temp)
summary(TopEC)
summary(BottomEC)
summary(Secchi)
summary(Turbidity)
summary(Comments)
summary(American.Shad)
summary(Arrow.Goby)
summary(Bay.Goby)
summary(Bay.Pipefish)
summary(Bigscale.Logperch)
summary(Black.Bullhead)
summary(Black.Crappie)
summary(Bluegill.Sunfish)
summary(Brown.Bullhead)
summary(Brown.Rockfish)
summary(California.Tonguefish)
summary(Carp)
summary(Catfish..paren.Unid.paren.)
summary(Centrarchids..paren.Unid.paren.)
summary(Chameleon.Goby)
summary(Channel.Catfish)
summary(Cheekspot.Goby)
summary(Chinook.Salmon)
summary(Cyprinids..paren.Unid.paren.)
summary(Delta.Smelt)
summary(English.Sole)
summary(Gobies..paren.Unid.paren.)
summary(Golden.Shiner)
summary(Goldfish)
summary(Hitch)
summary(Inland.Silverside)
summary(Jacksmelt)
summary(Lampreys..paren.Unid.paren.)
summary(Largemouth.Bass)
summary(Longfin.Smelt)
summary(Longjaw.Mudsucker)
summary(Mosquitofish)
summary(Northern.Anchovy)
summary(Pacific.Herring)
summary(Pacific.Lamprey)
summary(Pacific.Staghorn.Sculpin)
summary(Plainfin.Midshipman)
summary(Prickly.Sculpin)
summary(Rainwater.Killifish)
summary(Redear.Sunfish)
summary(River.Lamprey)
summary(Sacramento.Blackfish)
summary(Sacramento.Pikeminnow)
summary(Sacramento.Sucker)
summary(Sculpins..paren.Unid.paren.)
summary(Shimofuri.Goby)
summary(Shiner.Perch)
summary(Shokihaze.Goby)
summary(Silversides..paren.Unid.paren.)
summary(Smallmouth.Bass)
summary(Smelt..paren.Unid.paren.)
summary(Speckled.Sanddab)
summary(Splittail)
summary(Spotted.Bass)
summary(Starry.Flounder)
summary(Striped.Bass)
summary(Sturgeon..paren.Unid.paren.)
summary(Threadfin.Shad)
summary(Threespine.Stickleback)
summary(Topsmelt)
summary(Tridentiger.spp_)
summary(Tule.Perch)
summary(Unknown)
summary(Wakasagi)
summary(White.Catfish)
summary(White.Crappie)
summary(White.Croaker)
summary(White.Sturgeon)
summary(Yellowfin.Goby) 
# Get more details on character variables

summary(as.factor(dt1$qry_AMC_EDI_01.Survey.Survey)) 
summary(as.factor(dt1$qry_AMC_EDI_01.Station.Station)) 
summary(as.factor(dt1$Latitude)) 
summary(as.factor(dt1$Longitude)) 
summary(as.factor(dt1$TowNum)) 
summary(as.factor(dt1$GearCode)) 
summary(as.factor(dt1$Tide)) 
summary(as.factor(dt1$Comments))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/535/4/1c8ab3bcd3aec0ed06bf86e696cdfa6a" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Station",     
                 "Lat",     
                 "Long"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Station)!="factor") dt2$Station<- as.factor(dt2$Station)
if (class(dt2$Lat)!="factor") dt2$Lat<- as.factor(dt2$Lat)
if (class(dt2$Long)!="factor") dt2$Long<- as.factor(dt2$Long)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Station)
summary(Lat)
summary(Long) 
# Get more details on character variables

summary(as.factor(dt2$Station)) 
summary(as.factor(dt2$Lat)) 
summary(as.factor(dt2$Long))
detach(dt2)            

