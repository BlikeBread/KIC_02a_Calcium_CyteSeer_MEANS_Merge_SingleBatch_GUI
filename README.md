# KIC Spheroid Calcium Pipeline (Script 02a/03)

This repository contains an R script for merging and filtering **WHOLE-IMAGE MEANS summary tables** generated from the **CyteSeer Cytometer (Vala Sciences)** and processed in **Script 01** of the KIC Spheroid Calcium pipeline.

This script represents the second step (**02a/03**) of a modular **KIC Spheroid Calcium analysis pipeline**.

The pipeline consolidates per-well MEANS datasets from a **single experiment/batch**, applies peak-range filtering, assigns experimental group labels, and exports a standardized dataset ready for cross-batch merging and downstream statistical analysis.

---

## What the pipeline does

Starting from the **MEANS-only summary CSV files** generated in Script 01, the script:

- Recursively scans an input folder for `*MEANS*.csv` files  
- Reads and merges all MEANS tables into a unified batch-level dataset  
- Flags wells based on:
  - Valid plate format (**A01–H12**)  
  - `Num.Peaks` within a user-defined range (e.g., 1 Hz validation window)  
- Allows two processing modes:
  - **Filtered mode** – removes invalid wells and out-of-range peak counts  
  - **Flag-only mode** – retains all wells and adds logical flags  
- Assigns **Group labels** based on user-defined column rules  
  - Example: `C1:1-3`, `P1:7-9`  
- Optionally assigns **RowGroup labels** based on plate row rules  
  - Example: `D0:A,B`, `D7:C,D`  
- Appends a **Batch identifier** to all rows  
- Exports a standardized `merged.csv` file  

All file paths and settings are selected interactively via GUI dialogs (`tcltk`).

---

## Required inputs

### Step 01 output

- One or multiple `*MEANS*.csv` files  
- Files must originate from the `/summary` folder generated in **Script 01**  
- Nested folder structures are supported (recursive search enabled)

Required columns:

- `Well`  
- `Num.Peaks`  

---

## Cleaned merged output

The script generates:

- `merged.csv`

This file contains:

- Original MEANS summary statistics  
- Logical flags:
  - `keep_1hz`  
  - `valid_well`  
- Optional filtered dataset (if selected)  
- Assigned `Group`  
- Optional `RowGroup`  
- `Batch` identifier  
- Source file tracking  

The dataset is standardized and ready for:

- Cross-batch merging (**Script 02b**)  
- Statistical analysis  
- Group-based comparisons  
- Timepoint aggregation  
- Downstream visualization (**Script 03**)  

---

## Typical use cases

- Single-batch experiment consolidation  
- Peak-range quality control  
- Plate-based experimental grouping  
- Preparation for multi-experiment integration  
- Standardized preprocessing before statistical modeling  

---

## Position in the KIC Pipeline

This script is **Script 02a** of a structured workflow:

- Script 01 – Raw CyteSeer CSV processing and MEANS summary generation  
- **Script 02a (this repository)** – Single-batch MEANS merging and filtering  
- Script 02b – Cross-batch merging  
- Script 03 – Statistical analysis and visualization  

---

## Methods Description

WHOLE-IMAGE MEANS summary tables generated from CyteSeer calcium transient measurements were merged using a custom R-based GUI pipeline. Wells were flagged based on plate validity and peak-count thresholds, and experimental group labels were assigned using user-defined plate mapping rules. The resulting standardized dataset was exported for cross-batch integration and downstream statistical analysis.

---

## Authorship

This script was developed by **Michele Buono, Talitha Spanjersberg, Nikki Scheen, Nina van der Wilt** and can be used freely for research purposes, provided appropriate citation of the authors.

The overall workflow, structure, and clarity of the pipeline were iteratively refined with assistance from **OpenAI – ChatGPT 5.2**, which was used as a tool to improve code organization, documentation, and usability.
