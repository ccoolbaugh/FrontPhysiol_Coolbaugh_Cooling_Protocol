## DATA DICTIONARY
The tidy data folder contains the processed data files needed to recreate the tables, figures, and statistical analyses in the *Frontiers Integrative Physiology* Methods paper: "An Individualized, Perception-Based Cooling Protocol to Investigate Brown Adipose Tissue Physiology". 

### Tidy Data Files 
The following comma-separated-value (.csv) files are imported and analyzed in the R markdown code:

1. data\_tab3\_subject\_demographics.csv
2. data\_fig2\_water\_tgui\_variability.csv
3. data\_fig3\_shiver\_variability.csv
4. data\_fig4\_skintemp\_tgui\_relation.csv
5. data\_stats\_skintemp\_prepost.csv
6. data\_fig6a\_EMGtGUI\_similarity.csv
7. data\_fig6bcd\_EMGtGUI\_similarity.csv

### Variable Naming Conventions
Variable naming conventions and units for each data file are as follows: 

#### data\_tab3\_subject\_demographics
* Columns: 11
* Rows: 13
* Variables:
    * ID - Subject Identification #
    * Sex - Subject Sex - "M" or "F" (male / female)
    * Age - Subject Age - (years)
    * Height - Subject Height - (centimeters)
    * Mass - Subject Mass - (kilograms)
    * BMI - Subject Body Mass Index (BMI) - (kilograms/meters^2)
    * WC - Subject Waist Circumference (WC) - (centimeters)
    * TempOut - outdoor temperature at the start of the session - (°C)
    * HumOut - outdoor humidity at the start of the session - (%)
    * TempIn - indoor temperature at the start of the session - (°C)
    * HumIn - indoor humidity at the start of the session - (%)

#### data\_fig2\_water\_tgui\_variability
* Columns: 4
* Rows: 37
* Variables:
    * ID - Subject Identification #
    * IdxType - Physiological events - VC (vasoconstriction index); S\_On (onset of shivering); S\_End (sustained shivering) 
    * H20 - water temperature - (°C)
    * tGUI - perception of cooling integer value - (arbitrary units)
        * 0 = "Very Cold"; 10 = "Cold"; 20 = "Slightly Cold"; 30 = "Cool"; 40 = "Slightly Cool"; 50 = "Neutral"

#### data\_fig3\_shiver\_variability
* Columns: 4
* Rows: 846
* Variables:
    * ID - Subject Identification #
    * time - cooling protocol time (0 = start of cooling) - HH:MM:ss
    * shiver\_event - tGUI self reported shiver event - 0 (shiver = no); 1 (shiver = yes)
    * group - shiver event type - 'first' (onset of shivering); 'shiver' (shiver event); 'mid' (50% of total shivers); 'last' (sustained shivering)

#### data\_fig4\_skintemp\_tgui\_relation
* Columns: 7
* Rows: 5691
* Variables:
    * ID - Subject Identification #
    * time - interpolated time vector - (seconds)
    * tGUI - interpolated perception of cooling integer value - (arbitrary units)
        * 0 = "Very Cold"; 10 = "Cold"; 20 = "Slightly Cold"; 30 = "Cool"; 40 = "Slightly Cool"; 50 = "Neutral"
    * CR - relative change in clavicle skin temperature - (°C)
    * AR - relative change in arm skin temperature - (°C)
    * FR - relative change in finger skin temperature - (°C)
    * VC - arm to finger temperature gradient - (°C)
        
#### data\_stats\_skintemp\_prepost
* Columns: 4
* Rows: 81
* Variables:
    * ID - Subject Identification #
    * Location - skin temperature probe location - Clav (clavicle); Arm (arm); Fing (finger); VC (arm to finger temperature gradient)
    * Condition - time point of measurement - Pre (end of thermoneutral phase); Post (end of cooling)
    * Temp - skin temperature or gradient value - (°C)
    
#### data\_fig6a\_EMGtGUI\_similarity
* Columns: 3
* Rows: 25
* Variables:
    * ID - Subject Identification #
    * Type - shiver measurement tool - EMG (surface electromyography); tGUI (self-reported shiver)
    * FoA - fraction of agreement - arbitrary units (0 = no agreement; 1 = complete agreement)

#### data\_fig6bcd\_EMGtGUI\_similarity
* Columns: 4
* Rows: 1404
* Variables:
    * ID - Subject Identification #
    * Type - shiver measurement tool - EMG (surface electromyography); tGUI (self-reported shiver)
    * On - shiver start time - time of day (HH:MM:ss)
    * Off - shiver end time - time of day (HH:MM:ss)



