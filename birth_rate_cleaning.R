library(janitor)
library(readxl)
library(fs)
library(scales)
library(sorensonimpact)
library(tidyverse)
library(purrr)
library(tidycensus)

<<<<<<< HEAD
=======
# i am so sorry, i couldnt figure out how to automate ... :( -- Lily
>>>>>>> f088333559d906ab0f8277bc6b9cf094baeab871
natl1988 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1988.csv")
natl1989 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1989.csv")
natl1990 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1990.csv")
natl1997 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1997.csv")
natl1996 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1996.csv")
natl1995 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1995.csv")
natl1994 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1994.csv")
natl1993 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1993.csv")
natl1992 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1992.csv")
natl1998 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1998.csv")
natl1999 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1999.csv")
natl2000 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2000.csv")
natl2001 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2001.csv")
natl2002 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2002.csv")
natl2003 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2003.csv")
natl2004 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2004.csv")

# supposedly these do not have state data.... WHYYYYYYY
# natl2005 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2005.csv")
# natl2006 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2006.csv")
# natl2007 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2007.csv") ##
# natl2008 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2008.csv")
# natl2009 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2009.csv")
# natl2010 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2010.csv")
# natl2011 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2011.csv")
# natl2012 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2012.csv")
# natl2013 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2013.csv")
# natl2014 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2014.csv")
# natl2015 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2015.csv")
# natl2016 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2016.csv")
# natl2017 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2017.csv")

fips <- fips_codes %>% 
  # select(state_name, state) %>%
  select(state_code, state) %>% 
  unique()

births88 <- natl1988 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1988, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births89 <- natl1989 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1989, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births90 <- natl1990 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1990, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births97 <- natl1997 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1997, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births96 <- natl1996 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1996, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births95 <- natl1995 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1995, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births94 <- natl1994 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1994, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births93 <- natl1993 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1993, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births92 <- natl1992 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1992, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births98 <- natl1998 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1998, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births99 <- natl1999 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1999, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

<<<<<<< HEAD
write_rds(births88, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births88.rds")
write_rds(births89, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births89.rds")
write_rds(births90, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births90.rds")
write_rds(births97, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births97.rds")
write_rds(births96, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births96.rds")
write_rds(births95, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births95.rds")
write_rds(births94, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births94.rds")
write_rds(births93, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births93.rds")
write_rds(births92, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births92.rds")
write_rds(births98, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births98.rds")
write_rds(births99, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births99.rds")

=======
births00 <- natl2000 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2000, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births01 <- natl2001 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2001, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births02 <- natl2002 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2002, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births03 <- natl2003 %>% 
  group_by(ostate) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2003, state = ostate) %>% 
  select(-c(ostate))

births04 <- natl2004 %>% 
  group_by(ostate) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2004, state = ostate) %>% 
  select(-c(ostate))

# errant NA in row 52 ??????
births04 <- births04[-c(52),]

# -----------
# births05 <- natl2005 %>% 
#   group_by(xostate) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2005, state_code = xostate) %>% 
#   select(-c(xostate))
# 
# births06 <- natl2006 %>% 
#   group_by(ostate) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2006, state_code = ostate) %>% 
#   select(-c(ostate))
# 
# births07 <- natl2007 %>% 
#   group_by(ostate) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2007, state_code = ostate) %>% 
#   select(-c(ostate))
# 
# births08 <- natl2008 %>% 
#   group_by(stoccfip) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2008, state_code = stoccfip) %>% 
#   select(-c(stoccfip)) %>% 
#   left_join(fips)
# 
# births09 <- natl2009 %>% 
#   group_by(stoccfip) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2009, state_code = stoccfip) %>% 
#   select(-c(stoccfip)) %>% 
#   left_join(fips)
# 
# births10 <- natl2010 %>% 
#   group_by(stoccfip) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2010, state_code = stoccfip) %>% 
#   select(-c(stoccfip)) %>% 
#   left_join(fips)
# 
# births11 <- natl2011 %>% 
#   group_by(stoccfip) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2011, state_code = stoccfip) %>% 
#   select(-c(stoccfip)) %>% 
#   left_join(fips)
# 
# births12 <- natl2012 %>% 
#   group_by(stoccfip) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2012, state_code = stoccfip) %>% 
#   select(-c(stoccfip)) %>% 
#   left_join(fips)
# 
# births13 <- natl2013 %>% 
#   group_by(stoccfip) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2013, state_code = stoccfip) %>% 
#   select(-c(stoccfip)) %>% 
#   left_join(fips)
# 
# births14 <- natl2014 %>% 
#   group_by(stoccfip) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2014, state_code = stoccfip) %>% 
#   select(-c(stoccfip)) %>% 
#   left_join(fips)
# 
# births15 <- natl2015 %>% 
#   group_by(stoccfip) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2015, state_code = stoccfip) %>% 
#   select(-c(stoccfip)) %>% 
#   left_join(fips)
# 
# births15 <- natl2015 %>% 
#   group_by(stoccfip) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2015, state_code = stoccfip) %>% 
#   select(-c(stoccfip)) %>% 
#   left_join(fips)
# 
# births16 <- natl2016 %>% 
#   group_by(stoccfip) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2016, state_code = stoccfip) %>% 
#   select(-c(stoccfip)) %>% 
#   left_join(fips)
# 
# births17 <- natl2017 %>% 
#   group_by(stoccfip) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(datayear = 2017, state_code = stoccfip) %>% 
#   select(-c(stoccfip)) %>% 
#   left_join(fips) -----------

write_rds(births88, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births88.rds")
write_rds(births89, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births89.rds")
write_rds(births90, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births90.rds")
write_rds(births97, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births97.rds")
write_rds(births96, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births96.rds")
write_rds(births95, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births95.rds")
write_rds(births94, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births94.rds")
write_rds(births93, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births93.rds")
write_rds(births92, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births92.rds")
write_rds(births98, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births98.rds")
write_rds(births99, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births99.rds")
write_rds(births00, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births00.rds")
write_rds(births01, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births01.rds")
write_rds(births02, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births02.rds")
write_rds(births03, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births03.rds")
write_rds(births04, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births04.rds")

# 2005-2017, state data not available on the national dataset, have to go to the PDF description, copy, paste, and scrape... URGH NBER AND CDC URGH
# each year has a PDF doc on the NBER website. Table A has the state counts. We have been using the occurrence counts. example :http://www.nber.org/natality/2017/natl2017.pdf

births_2005 = "Alabama 59,300 60,453
Alaska 10,365 10,459
Arizona 95,687 96,199
Arkansas 38,381 39,208
California 549,100 548,882
Colorado 69,205 68,944
Connecticut 42,133 41,718
Delaware 12,265 11,643
District of Columbia 14,311 7,971
Florida 226,415 226,240
Georgia 143,476 142,200
Hawaii 17,911 17,924
Idaho 22,522 23,062
Illinois 175,714 179,020
Indiana 87,843 87,193
Iowa 39,337 39,311
Kansas 40,737 39,888
Kentucky 54,590 56,444
Louisiana 60,461 60,937
Maine 13,975 14,112
Maryland 71,292 74,980
Massachusetts 77,820 76,865
Michigan 126,498 127,706
Minnesota 70,933 70,919
Mississippi 41,175 42,395
Missouri 79,523 78,618
Montana 11,551 11,583
Nebraska 26,350 26,145
Nevada 36,950 37,268
New Hampshire 13,968 14,420
New Jersey 110,800 113,776
New Mexico 28,291 28,835
New York 247,901 246,351
North Carolina 123,943 123,096
North Dakota 9,621 8,390
Ohio 148,876 148,388
Oklahoma 50,656 51,801
Oregon 46,712 45,922
Pennsylvania 144,908 145,383
Rhode Island 13,481 12,697
South Carolina 55,321 57,711
South Dakota 11,957 11,462
Tennessee 87,072 81,747
Texas 387,856 385,915
Utah 52,555 51,556
Vermont 5,932 6,295
Virginia 102,646 104,555
Washington 82,336 82,703
West Virginia 21,150 20,836
Wisconsin 69,769 70,984
Wyoming 6,778 7,239"

births_2006 <- "Alabama 62,100 63,232
Alaska 10,899 10,996
Arizona 103,142 102,429
Arkansas 39,746 40,961
California 563,522 562,440
Colorado 71,157 70,751
Connecticut 42,187 41,820
Delaware 12,418 11,989
District of Columbia 14,592 8,523
Florida 237,499 236,802
Georgia 149,920 148,633
Hawaii 18,986 18,982
Idaho 23,719 24,184
Illinois 177,234 180,572
Indiana 89,178 88,631
Iowa 40,620 40,607
Kansas 41,946 40,968
Kentucky 56,646 58,250
Louisiana 63,479 63,376
Maine 14,010 14,151
Maryland 74,082 77,494
Massachusetts 78,508 77,676
Michigan 126,393 127,483
Minnesota 73,474 73,525
Mississippi 44,863 46,056
Missouri 82,458 81,385
Montana 12,490 12,508
Nebraska 26,892 26,727
Nevada 39,690 40,027
New Hampshire 14,070 14,378
New Jersey 111,930 115,020
New Mexico 29,337 29,936
New York 251,948 250,104
North Carolina 128,999 127,859
North Dakota 9,875 8,621
Ohio 151,341 150,593
Oklahoma 53,039 54,016
Oregon 49,090 48,689
Pennsylvania 148,518 149,090
Rhode Island 13,179 12,372
South Carolina 59,571 62,171
South Dakota 12,386 11,919
Tennessee 89,429 84,355
Texas 405,869 399,603
Utah 54,528 53,504
Vermont 6,114 6,511
Virginia 105,890 107,817
Washington 86,799 86,876
West Virginia 21,134 20,931
Wisconsin 71,236 72,340
Wyoming 7,093 7,672"

births_2007 <- "Alabama 63,995 64,804
Alaska 10,955 11,052
Arizona 103,797 102,981
Arkansas 40,168 41,378
California 567,527 566,414
Colorado 71,225 70,809
Connecticut 42,265 41,660
Delaware 12,527 12,170
District of Columbia 14,824 8,864
Florida 239,389 239,165
Georgia 152,068 151,137
Hawaii 19,151 19,134
Idaho 24,436 25,019
Illinois 177,450 180,836
Indiana 90,572 89,864
Iowa 40,988 40,886
Kansas 42,938 42,004
Kentucky 57,424 59,368
Louisiana 66,336 66,301
Maine 13,974 14,120
Maryland 74,940 78,095
Massachusetts 78,723 77,967
Michigan 124,124 125,261
Minnesota 73,595 73,735
Mississippi 45,509 46,491
Missouri 82,823 81,930
Montana 12,400 12,439
Nebraska 27,117 26,934
Nevada 40,766 41,181
New Hampshire 13,937 14,168
New Jersey 112,883 116,063
New Mexico 29,905 30,616
New York 255,429 253,451
North Carolina 132,188 131,037
North Dakota 10,152 8,840
Ohio 151,342 150,879
Oklahoma 54,154 55,065
Oregon 49,873 49,378
Pennsylvania 149,999 150,713
Rhode Island 13,192 12,376
South Carolina 60,204 62,875
South Dakota 12,815 12,261
Tennessee 92,049 86,711
Texas 414,170 407,625
Utah 56,319 55,130
Vermont 6,210 6,513
Virginia 107,262 108,884
Washington 88,944 88,978
West Virginia 21,917 21,994
Wisconsin 71,741 72,784
Wyoming 7,317 7,893"

births_2008 <- "Alabama 63,450 64,546
Alaska 11,329 11,442
Arizona 100,089 99,442
Arkansas 39,502 40,669
California 552,618 551,779
Colorado 70,527 70,031
Connecticut 40,930 40,399
Delaware 12,545 12,090
District of Columbia 14,499 9,130
Florida 231,652 231,445
Georgia 147,799 146,603
Hawaii 19,463 19,484
Idaho 24,676 25,149
Illinois 173,410 176,795
Indiana 89,345 88,742
Iowa 40,281 40,224
Kansas 42,568 41,833
Kentucky 56,621 58,375
Louisiana 65,073 65,268
Maine 13,500 13,609
Maryland 74,615 77,289
Massachusetts 77,543 77,022
Michigan 120,172 121,127
Minnesota 72,220 72,421
Mississippi 44,139 44,947
Missouri 81,992 80,963
Montana 12,551 12,594
Nebraska 27,082 26,989
Nevada 39,192 39,506
New Hampshire 13,630 13,683
New Jersey 109,703 112,710
New Mexico 29,572 30,173
New York 252,360 250,383
North Carolina 132,106 130,839
North Dakota 10,312 8,938
Ohio 149,346 148,821
Oklahoma 53,720 54,781
Oregon 49,499 49,096
Pennsylvania 148,460 149,273
Rhode Island 12,812 12,048
South Carolina 60,401 63,071
South Dakota 12,631 12,071
Tennessee 90,885 85,560
Texas 412,127 405,554
Utah 56,787 55,634
Vermont 5,957 6,339
Virginia 104,990 106,686
Washington 90,318 90,321
West Virginia 21,441 21,501
Wisconsin 71,272 72,261
Wyoming 7,444 8,038"

births_2009 <- "Alabama 61,317 62,475
Alaska 11,202 11,324
Arizona 93,320 92,798
Arkansas 38,768 39,808
California 527,847 527,020
Colorado 69,036 68,628
Connecticut 39,481 38,896
Delaware 11,989 11,559
District of Columbia 14,200 9,040
Florida 221,635 221,394
Georgia 142,686 141,377
Hawaii 18,888 18,887
Idaho 23,253 23,737
Illinois 167,659 171,163
Indiana 87,520 86,673
Iowa 39,640 39,701
Kansas 42,512 41,396
Kentucky 55,594 57,551
Louisiana 65,108 64,973
Maine 13,354 13,470
Maryland 72,590 75,059
Massachusetts 75,445 75,016
Michigan 116,236 117,294
Minnesota 70,426 70,646
Mississippi 41,978 42,901
Missouri 79,593 78,905
Montana 12,203 12,257
Nebraska 27,198 26,936
Nevada 37,296 37,612
New Hampshire 13,389 13,377
New Jersey 107,086 110,331
New Mexico 28,315 29,000
New York 250,029 248,110
North Carolina 128,173 126,845
North Dakota 10,275 9,001
Ohio 145,517 144,841
Oklahoma 53,650 54,553
Oregon 47,685 47,132
Pennsylvania 145,812 146,434
Rhode Island 12,230 11,442
South Carolina 57,884 60,620
South Dakota 12,479 11,934
Tennessee 87,141 82,211
Texas 408,391 401,977
Utah 55,144 53,887
Vermont 5,776 6,110
Virginia 103,061 105,059
Washington 89,200 89,313
West Virginia 21,299 21,268
Wisconsin 70,090 70,843
Wyoming 7,236 7,881"

births_2010 <- "Alabama 58,783 60,050
Alaska 11,366 11,471
Arizona 88,090 87,477
Arkansas 37,536 38,540
California 510,980 510,198
Colorado 66,822 66,355
Connecticut 38,539 37,708
Delaware 11,682 11,364
District of Columbia 13,790 9,165
Florida 214,962 214,590
Georgia 135,411 133,947
Hawaii 18,948 18,988
Idaho 22,799 23,198
Illinois 161,758 165,200
Indiana 84,794 83,940
Iowa 38,574 38,719
Kansas 41,598 40,649
Kentucky 53,565 55,784
Louisiana 62,531 62,379
Maine 12,814 12,970
Maryland 71,739 73,801
Massachusetts 73,275 72,865
Michigan 113,509 114,531
Minnesota 68,269 68,610
Mississippi 39,177 40,036
Missouri 77,588 76,759
Montana 12,066 12,060
Nebraska 26,242 25,918
Nevada 35,671 35,934
New Hampshire 13,032 12,874
New Jersey 103,932 106,922
New Mexico 27,021 27,850
New York 246,081 244,375
North Carolina 123,468 122,350
North Dakota 10,470 9,104
Ohio 139,858 139,128
Oklahoma 52,347 53,238
Oregon 45,904 45,540
Pennsylvania 142,724 143,321
Rhode Island 11,843 11,177
South Carolina 55,599 58,342
South Dakota 12,382 11,811
Tennessee 84,533 79,495
Texas 392,764 386,118
Utah 53,395 52,258
Vermont 5,775 6,223
Virginia 101,202 103,002
Washington 86,507 86,539
West Virginia 20,757 20,470
Wisconsin 67,719 68,487
Wyoming 6,914 7,556"

births_2011 <- "Alabama 57,891 59,354
Alaska 11,339 11,456
Arizona 86,103 85,543
Arkansas 37,855 38,715
California 503,018 502,120
Colorado 65,514 65,055
Connecticut 37,988 37,281
Delaware 11,631 11,257
District of Columbia 13,836 9,295
Florida 213,716 213,414
Georgia 133,828 132,409
Hawaii 18,971 18,956
Idaho 21,859 22,305
Illinois 157,737 161,312
Indiana 84,582 83,701
Iowa 38,040 38,214
Kansas 40,472 39,642
Kentucky 53,344 55,370
Louisiana 61,983 61,888
Maine 12,564 12,704
Maryland 70,958 73,093
Massachusetts 73,499 73,166
Michigan 113,054 114,008
Minnesota 68,115 68,409
Mississippi 38,938 39,860
Missouri 77,136 76,117
Montana 12,018 12,069
Nebraska 26,095 25,720
Nevada 35,009 35,296
New Hampshire 13,080 12,851
New Jersey 103,033 105,883
New Mexico 26,414 27,289
New York 242,864 241,312
North Carolina 121,557 120,389
North Dakota 10,772 9,527
Ohio 138,581 137,918
Oklahoma 51,315 52,272
Oregon 45,480 45,155
Pennsylvania 142,527 143,178
Rhode Island 11,729 10,960
South Carolina 54,837 57,393
South Dakota 12,470 11,846
Tennessee 84,412 79,588
Texas 384,116 377,445
Utah 52,326 51,223
Vermont 5,702 6,078
Virginia 101,032 102,652
Washington 86,954 86,976
West Virginia 20,959 20,717
Wisconsin 67,255 67,810
Wyoming 6,712 7,399"

births_2012 <- "Alabama 56,941 58,448
Alaska 11,054 11,187
Arizona 87,207 86,441
Arkansas 37,342 38,347
California 504,634 503,755
Colorado 65,643 65,187
Connecticut 37,294 36,539
Delaware 11,381 11,023
District of Columbia 13,972 9,399
Florida 213,402 213,148
Georgia 131,921 130,280
Hawaii 18,986 18,980
Idaho 22,482 22,963
Illinois 155,813 159,160
Indiana 84,201 83,227
Iowa 38,427 38,702
Kansas 41,174 40,341
Kentucky 53,370 55,758
Louisiana 62,585 62,642
Maine 12,594 12,798
Maryland 70,417 72,883
Massachusetts 72,827 72,439
Michigan 112,159 113,091
Minnesota 68,053 68,772
Mississippi 37,787 38,669
Missouri 76,412 75,446
Montana 12,072 12,118
Nebraska 26,282 25,942
Nevada 34,622 34,911
New Hampshire 12,578 12,352
New Jersey 101,632 104,230
New Mexico 26,150 27,068
New York 242,254 240,916
North Carolina 121,138 119,831
North Dakota 11,508 10,106
Ohio 139,071 138,483
Oklahoma 51,758 52,751
Oregon 45,567 45,067
Pennsylvania 142,009 142,514
Rhode Island 11,652 10,926
South Carolina 54,258 57,155
South Dakota 12,713 12,104
Tennessee 85,600 80,371
Texas 389,896 382,727
Utah 52,516 51,465
Vermont 5,687 6,009
Virginia 101,412 103,013
Washington 87,358 87,463
West Virginia 21,152 20,827
Wisconsin 66,975 67,295
Wyoming 6,858 7,572"

births_2013 <- "Alabama 56,727 58,167
Alaska 11,345 11,446
Arizona 86,538 85,600
Arkansas 36,804 37,832
California 495,571 494,705
Colorado 65,529 65,007
Connecticut 36,851 36,085
Delaware 11,201 10,831
District of Columbia 14,157 9,288
Florida 215,657 215,407
Georgia 130,150 128,748
Hawaii 18,976 18,987
Idaho 22,008 22,383
Illinois 153,325 156,931
Indiana 83,982 83,102
Iowa 38,790 39,094
Kansas 39,777 38,839
Kentucky 53,438 55,686
Louisiana 63,225 63,201
Maine 12,594 12,776
Maryland 69,315 71,953
Massachusetts 72,191 71,788
Michigan 112,591 113,489
Minnesota 68,469 69,159
Mississippi 37,647 38,634
Missouri 76,390 75,296
Montana 12,328 12,377
Nebraska 26,415 26,095
Nevada 34,822 35,030
New Hampshire 12,571 12,396
New Jersey 100,547 102,575
New Mexico 25,284 26,354
New York 238,168 236,980
North Carolina 120,341 119,002
North Dakota 11,978 10,599
Ohio 139,694 138,936
Oklahoma 52,350 53,369
Oregon 45,592 45,155
Pennsylvania 140,048 140,921
Rhode Island 11,506 10,809
South Carolina 53,970 56,795
South Dakota 12,925 12,248
Tennessee 84,974 79,992
Texas 395,067 387,340
Utah 51,861 50,957
Vermont 5,654 5,975
Virginia 100,618 102,147
Washington 86,415 86,577
West Virginia 21,120 20,825
Wisconsin 66,320 66,649
Wyoming 6,948 7,644"

births_2014 <- "Alabama 57,798 59,422
Alaska 11,246 11,392
Arizona 88,028 86,887
Arkansas 37,788 38,511
California 503,889 502,879
Colorado 66,379 65,830
Connecticut 37,649 36,285
Delaware 11,486 10,972
District of Columbia 14,500 9,509
Florida 220,148 219,991
Georgia 132,078 130,946
Hawaii 18,573 18,550
Idaho 22,489 22,876
Illinois 154,806 158,556
Indiana 84,876 84,080
Iowa 39,466 39,687
Kansas 40,145 39,223
Kentucky 53,796 56,170
Louisiana 64,518 64,497
Maine 12,533 12,698
Maryland 70,916 73,921
Massachusetts 72,510 71,908
Michigan 113,436 114,375
Minnesota 69,166 69,904
Mississippi 37,955 38,736
Missouri 76,725 75,360
Montana 12,397 12,432
Nebraska 27,113 26,794
Nevada 35,509 35,861
New Hampshire 12,313 12,302
New Jersey 101,282 103,305
New Mexico 24,990 26,052
New York 239,435 238,773
North Carolina 122,542 120,975
North Dakota 12,839 11,359
Ohio 140,514 139,467
Oklahoma 51,926 53,339
Oregon 46,098 45,556
Pennsylvania 141,394 142,268
Rhode Island 11,358 10,823
South Carolina 54,503 57,627
South Dakota 12,956 12,283
Tennessee 87,174 81,602
Texas 408,213 399,766
Utah 52,162 51,154
Vermont 5,846 6,130
Virginia 101,867 103,300
Washington 88,420 88,585
West Virginia 20,548 20,301
Wisconsin 66,884 67,161
Wyoming 6,993 7,696"

births_2015 <- "Alabama 58,034 59,657
Alaska 11,176 11,282
Arizona 86,399 85,351
Arkansas 37,571 38,886
California 492,759 491,748
Colorado 67,162 66,581
Connecticut 37,242 35,746
Delaware 11,511 11,166
District of Columbia 14,615 9,578
Florida 224,530 224,269
Georgia 132,678 131,404
Hawaii 18,453 18,420
Idaho 22,474 22,827
Illinois 154,193 158,116
Indiana 84,995 84,040
Iowa 39,491 39,482
Kansas 40,238 39,154
Kentucky 53,515 55,971
Louisiana 64,774 64,692
Maine 12,385 12,607
Maryland 70,314 73,616
Massachusetts 71,993 71,492
Michigan 112,309 113,312
Minnesota 69,092 69,834
Mississippi 37,589 38,394
Missouri 76,170 75,061
Montana 12,523 12,583
Nebraska 27,120 26,679
Nevada 35,951 36,298
New Hampshire 12,494 12,433
New Jersey 100,883 103,127
New Mexico 24,296 25,816
New York 237,993 237,274
North Carolina 122,670 120,843
North Dakota 12,839 11,314
Ohio 140,050 139,264
Oklahoma 51,900 53,122
Oregon 46,101 45,655
Pennsylvania 140,130 141,047
Rhode Island 11,552 10,993
South Carolina 54,754 58,139
South Dakota 12,969 12,336
Tennessee 87,432 81,685
Texas 412,672 403,618
Utah 51,723 50,778
Vermont 5,720 5,903
Virginia 102,182 103,303
Washington 88,905 88,990
West Virginia 20,422 19,805
Wisconsin 66,677 67,041
Wyoming 7,113 7,765"

births_2016 <- "Alabama 57,647 59,151
Alaska 11,095 11,209
Arizona 85,573 84,520
Arkansas 36,913 38,274
California 489,976 488,827
Colorado 67,183 66,613
Connecticut 37,467 36,015
Delaware 11,415 10,992
District of Columbia 14,847 9,858
Florida 225,260 225,022
Georgia 130,961 130,042
Hawaii 18,057 18,059
Idaho 22,179 22,482
Illinois 150,789 154,445
Indiana 83,983 83,091
Iowa 39,094 39,403
Kansas 39,317 38,053
Kentucky 53,062 55,449
Louisiana 63,277 63,178
Maine 12,477 12,705
Maryland 69,832 73,136
Massachusetts 71,935 71,317
Michigan 112,349 113,315
Minnesota 68,831 69,749
Mississippi 37,139 37,928
Missouri 75,864 74,705
Montana 12,280 12,282
Nebraska 27,101 26,589
Nevada 35,918 36,260
New Hampshire 12,350 12,267
New Jersey 100,379 102,647
New Mexico 23,322 24,692 
New York 234,861 234,283
North Carolina 122,780 120,779
North Dakota 13,025 11,383
Ohio 138,570 138,085
Oklahoma 51,319 52,592
Oregon 45,973 45,535
Pennsylvania 138,637 139,409
Rhode Island 11,430 10,798
South Carolina 53,810 57,342
South Dakota 12,910 12,275
Tennessee 86,540 80,807
Texas 406,945 398,047
Utah 51,521 50,464
Vermont 5,567 5,756
Virginia 101,216 102,460
Washington 90,301 90,505
West Virginia 19,887 19,079
Wisconsin 66,238 66,615
Wyoming 6,710 7,386"

births_2017 <- "Alabama 57,469 58,941
Alaska 10,353 10,445
Arizona 82,829 81,872
Arkansas 36,179 37,520
California 472,820 471,658
Colorado 64,964 64,382
Connecticut 36,712 35,221
Delaware 11,265 10,855
District of Columbia 14,602 9,560
Florida 223,601 223,630
Georgia 130,183 129,243
Hawaii 17,519 17,517
Idaho 21,825 22,181
Illinois 145,700 149,390
Indiana 83,122 82,170
Iowa 38,284 38,430
Kansas 38,013 36,519
Kentucky 52,410 54,752
Louisiana 61,203 61,018
Maine 12,072 12,298
Maryland 68,154 71,641
Massachusetts 71,426 70,702
Michigan 110,449 111,426
Minnesota 67,545 68,595
Mississippi 36,563 37,357
Missouri 73,869 73,034
Montana 11,752 11,799
Nebraska 26,237 25,821
Nevada 35,466 35,756
New Hampshire 12,064 12,116
New Jersey 98,952 101,250
New Mexico 22,343 23,767
New York 230,358 229,737
North Carolina 122,103 120,125
North Dakota 12,391 10,737
Ohio 137,460 136,832
Oklahoma 48,833 50,214
Oregon 44,161 43,631
Pennsylvania 136,916 137,745
Rhode Island 11,198 10,638
South Carolina 53,579 57,029
South Dakota 12,809 12,134
Tennessee 86,721 81,016
Texas 390,053 382,050
Utah 49,651 48,585
Vermont 5,518 5,655
Virginia 99,475 100,391
Washington 87,331 87,562
West Virginia 19,252 18,675
Wisconsin 64,728 64,975
Wyoming 6,272 6,903"

birth_cleaning_funx <- function(births_file, year){
  str_split(births_file, "\\n", simplify = TRUE) %>%
    as_tibble() %>%
    gather(key = "key", value = "value") %>%
    select(-c(key)) %>%
    extract(value, into = c("state_name", "n", "residence"), regex = "([A-z\\s]*)\\s([\\d\\,]+)\\s([\\d\\,]+)") %>% 
    mutate(n = as.integer(gsub(",","",n))) %>% 
    left_join(fips) %>% 
    select(-c(residence, state_name)) %>% 
    mutate(datayear = year)
}

births05 <- birth_cleaning_funx(births_2005, 2005)
births06 <- birth_cleaning_funx(births_2006, 2006)
births07 <- birth_cleaning_funx(births_2007, 2007)
births08 <- birth_cleaning_funx(births_2008, 2008)
births09 <- birth_cleaning_funx(births_2009, 2009)
births10 <- birth_cleaning_funx(births_2010, 2010)
births11 <- birth_cleaning_funx(births_2011, 2011)
births12 <- birth_cleaning_funx(births_2012, 2012)
births13 <- birth_cleaning_funx(births_2013, 2013)
births14 <- birth_cleaning_funx(births_2014, 2014)
births15 <- birth_cleaning_funx(births_2015, 2015)
births16 <- birth_cleaning_funx(births_2016, 2016)
births17 <- birth_cleaning_funx(births_2017, 2017)

write_rds(births05, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births05.rds")
write_rds(births06, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births06.rds")
write_rds(births07, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births07.rds")
write_rds(births08, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births08.rds")
write_rds(births09, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births09.rds")
write_rds(births10, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births10.rds")
write_rds(births11, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births11.rds")
write_rds(births12, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births12.rds")
write_rds(births13, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births13.rds")
write_rds(births14, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births14.rds")
write_rds(births15, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births15.rds")
write_rds(births16, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births16.rds")
write_rds(births17, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/cleaned data/births17.rds")
>>>>>>> f088333559d906ab0f8277bc6b9cf094baeab871

