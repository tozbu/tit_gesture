# Ontogenic shift in the usage and function of a symbolic bird gesture

[![BioRxiv](https://img.shields.io/badge/bioRxiv-10.64898%2F2026.01.07.698152v1-red)](https://www.biorxiv.org/content/10.64898/2026.01.07.698152v1-v1)

This repository contains the data files and R scripts for the statistical analyses presented in this study of symbolic gestural communication in Great Tits (*Parus major*).

---

## 📝 Abstract
Gestures play a crucial role in human communication, and studies examining pointing (deictic) and meaningful (symbolic) gestures in non-human primates have highlighted the origins of our communication systems. Recent research on Japanese tits (Parus minor) revealed that individuals use a symbolic wing-fluttering gesture to encourage their mating partner to enter the nest first, suggesting that such gestures may be more ancestral than previously recognised. However, the presence of this gesture in related species, its association with feeding contexts, and how gestural use varies with development remain unclear. To address these gaps, we investigated the wing-fluttering gesture in great tits (P. major), observing fledglings and adults in different contexts both around and away from nests in their natural habitat. Our results indicated that females primarily used wing-fluttering near nests in the presence of their mate, ceasing the gesture when the mate entered the nest. Similar to findings in Japanese tits, this suggests that wing fluttering functions as a symbolic gesture prompting mates to enter the nest and feed nestlings. In contrast, fledglings used the gesture to solicit food from parents, stopping after being fed. This study highlights the evolutionary roots of symbolic communication and adaptive shifts in gestural use across development.

---

## 📂 Repository Structure

The repository is organized to ensure that each statistical model is easily reproducible with its corresponding dataset.

### 📜 `/scripts`
Contains **6 R scripts**, one for each statistical model used in the paper.
* `Script for Model 1 effect of mate presence and sex on gesturing.R` to `Script for Model 6 Influence of parent’s feeding behaviour on gesture interruption in fledglings.R`

### 📊 `/data_files`
Contains **6 data files** (CSV) corresponding to the scripts.
* `dataset Model 1 gesture and mate presence.csv` ... `dataset Model 6 stop gesture when fed.csv`

> **Note:** The numbering of the data files directly matches the numbering of the scripts (e.g., `Script for Model 1 effect of mate presence and sex on gesturing`
> is the script to analyse the datafile `dataset Model 1 gesture and mate presence.csv`).

---

## 🛠 Usage
To replicate the analyses:
1. Clone the repository:
   ```bash
   git clone [https://github.com/tozbu/tit_gesture](https://github.com/tozbu/tit_gesture)

## 💻 Software & Package Requirements

To run these analyses, you will need **R** (version 4.0.0 or higher recommended) and the following Bayesian modeling package:

* **[brms](https://paul-buerkner.github.io/brms/):** Bayesian Regression Models using 'Stan'.

You can install the required package by running the following command in your R console:

```r
install.packages("brms")
