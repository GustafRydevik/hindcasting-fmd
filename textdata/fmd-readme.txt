Dear Gustaf,

Here is the csv file

The data are roughly as follows

1377 random sample cross sectional = sample =1
195 animals in herds followed over time but not same animals = sample = 2
68 animals in a single herds at one time = sample=3

“hcode” - unique herd identifier listing leading 0s if you want to make a proper identifier
“ancode” - animal id repeated over each herd so need both to get a unique identifier for animal is also missing leading 0s

"age”    - years
“agecat” - 1 is <2 yo and 2 is >=2
"sample”      - see above
"FMD_cELISA”  - non structural ELISA
"FMD_CHEKIT"  - non structural ELISA
"FMD_EITB"  - non structural ELISA
   "FMD_iELISA"  - non structural ELISA these 4 tests are in Bronsvoort et all 3ABC paper

"FMD_VNT_A”   - serotype specific test
"FMD_VNT_O"   - serotype specific test
"FMD_VNT_SAT2" - serotype specific test these are antibody serotype specific see Bronsvoort geographic 2004

"PbP-A”   - virus recovered by probang and which serotype
"PbP-O"  - virus recovered by probang and which serotype
"PbP-SAT2"    - virus recovered by probang and which serotype

“monlast” - how many months ago did the herdsmen believe he last had an outbreak see morgan and bronsvoort EID 2014

"Date”   - animal sampled

"FMDS_O”   - VNT as binary
"FMDS_A"     - VNT as binary  
"FMDS_SAT2"   - VNT as binary

Hope this helps.

subset sample = 1 gets you the cross sectional stuff
subset sample = 2 should be the longitudinal


