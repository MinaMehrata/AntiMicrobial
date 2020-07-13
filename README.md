# AntiMicrobial

#objective
The objective of this work was to analyze approximately 3 years worth of data
(collected every second), to determine:
1-Number of door opening in each room per day, per week, per month and
per year
2- Number of completed disinfection cycles per day, per week, per month and
per year
3-Number of non-completed disinfection cycle for 1 min, 2 min, 3min and 4
min time periods per day, per week, per month and per year. 

#How the code works

The code opens one csv file at a time, performs operations on the data from
that opened file and then saves information on door opening and UV activation
in a separate data table. More specifically, it performs checks to see if the door
is open or closed, the time that it is open, and whether or not the closing of
the door initiates the UV. When the door signal goes from 1 to 0, a counter is
initiated which increments until the door signal returns to 1. UV is recorded as
being turned on if the current signal goes above 5 mA. A counter is initiated
when the UV signal goes above 5 mA and increments the time until the signal
falls back below 5 mA. To check if a door closing causes UV activation, the code
looks to data points 40 seconds after the door signal goes from 0 to 1 (open to
close) and then checks to see if the current signal is greater than 5 mA.

#Issues with Data
One issue that arose with the data was missing times and dates. A second issue
that was found was inconsistent .csv fileles. Row headers sometimes appeared af-
ter the start of the data collection. In other instances, the last row of data was in-
complete (in diverenterent ways). Another issue involved files ST3 700101000605.csv,
ST3 700102063651.csv from 5-124 Utility Room, and ST4 700101000345.csv,
ST4 700102063417.csv from 5-110 Bathroom, which had time stamps of 1970.