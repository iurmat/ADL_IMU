# ADL_IMU
This repository contains IMU data related to shoulder biomechanics during the execution of simulated Activities of Daily Living (ADL).

## Folders
The repository contains two folders:
<ul>
  <li>Angular_data: contains .xslx files with normalized and resampled data related to Roll, Pitch and Yaw angles for each movement, and the script LMM.R that performs statistical analysis and produces figures with results</li>
  <li>Differential_data: contains .xslx files with point-to-points differences in time of normalized and resampled data of Roll, Pitch and Yaw angles for each movement, and the script LMM_diff.R that performs statistical analysis and produces figures with results</li>
</ul>

## File naming conventions
In each folder, the files are named based on the following convention:

XXYYZ.xslx

where

<ul>
  <li>XX refers to the type of movement and can assume two different values: NS (Non-Standardized version of movement) or ST (Standardized version of movement)</li>
  <li>YY refers to the movement performed and can assume six different values: UC (Upper Care), MC (Medium Care), LC (Lower Care), FR (Frontal Reaching), UR (Upper Reaching), DR(Driving)</li>
  <li>Z refers to the specific angle considered and can assume three different values: R (Roll), P (Pitch), Y (Yaw)</li>
</ul>

In the Differential_data folder, a suffix _diff is appended to the file names, resulting in XXYYZ_diff.xslx.

## Files structure
The .xslx files are structured so that the analysis of the signals through Linear Mixed Effect Models (LMM) is possible. 
Therefore, the files are organized in the following columns:
<ul>
  <li>Angle: contains the angular value normalized with respect to the Range of Motion (RoM), or - in the Differential_data folder - the value of the angular difference between the associated Timestamp and the previous one</li>
  <li>Timestamp: the timestamp associated with the Angle values; because signals are resampled at 100 samples, the timestamp values range in a (0,100], representing percentage progression of the movement interval</li>
  <li>Age: represents the classification of the datum as belonging to the young or elderly population; it can assume two values: 0 for the young population, and 1 for the elderly population</li>
  <li>Subject: it represent the identification number associated with the participant</li>
  <li>Repetition: it represents the movement repetition to which the datum belongs to; it can assume a value in the range [1,10], since each participant was performing 10 repetions of each movement</li>
</ul>
