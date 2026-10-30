# Evaluating Ratings from Large Language Models: Analysis Code and Data

This repository contains the code and simulation results for

> Angelike, T., & Heck, D. W. (2026). *Evaluating Ratings from Large Language Models:
> A Simulation-Based Workflow*. Preprint: https://osf.io/preprints/psyarxiv/xphm9

- Preregistration (original): https://osf.io/8e6gf
- Preregistration (revision, additional analyses): https://osf.io/28ja9/
- ADEMP template for LLM simulation studies: https://github.com/TImA97/ADEMP-PreReg-for-LLMs
- Archived version of this repository: https://doi.org/10.5281/zenodo.XXXXXXX

## Repository structure

The repository contains two folders: `llm_simulation_valence_arousal/` (all code and results reported in the paper) and `pilot_study/` (files used for developing the workflow, not part of the paper; see below).

### LLM simulation and analyses (`llm_simulation_valence_arousal/`)

| File / folder | Content |
|---|---|
| `simulation_code.R` | Functions for running the LLM simulation (prompt construction, Ollama calls via `ellmer`, validity checks) |
| `run_simulations.R` | Main LLM simulation for valence and arousal (64 conditions × 96 expressions × 100 iterations) |
| `temperature_simulation.R` | Supplementary check of the effective temperature setting (valence only; 4 models × 8 prompt conditions × 20 iterations × 3 temperature settings) |
| `analyze_results.R` | All analyses reported in the paper: missing values, G and D studies, human-rating reliability, correlations with human ratings, split-half analysis, aggregation analysis, and the valence–arousal comparison |
| `computational_environment.txt` | `sessionInfo()` output for (1) the LLM simulation, (2) the analysis of the results, and (3) the temperature simulation |
| `llm_simulation_valence_arousal.Rproj` | RStudio project file (open it to work with the scripts in this folder) |
| `sim_results/` | Simulation results, derived correlation tables, and run logs (see below) |


## Reproducing the analyses

1. Download the four data files by Martínez et al. (2024) listed below and place them in the `llm_simulation_valence_arousal/` folder.
2. Open R in the `llm_simulation_valence_arousal/` folder.
3. Run `analyze_results.R` with `construct <- "valence"`, then with `construct <- "arousal"`.

**Note on the valence–arousal comparison:** the last part of `analyze_results.R` compares the results of both simulations and therefore requires both `sim_results/correlations_valence.csv` and `sim_results/correlations_arousal.csv`. Both files are part of this repository, so the script runs completely for either construct, in any order. Each run overwrites the correlation file of the analyzed construct with identical values.

Recommended batch call (writes a log file to `sim_results/`):

    R CMD BATCH --vanilla analyze_results.R sim_results/analysis_valence_documentation.Rout

For arousal, set `construct <- "arousal"` and write the log to `sim_results/analysis_arousal_documentation.Rout` instead.

The figures are saved as PDF files to `sim_results/` (they are not included in this repository). Runtime: about 30–45 minutes per construct.

### Re-running the LLM simulation (optional)

Requires Ollama 0.12.9 with the models `gemma3:4b`, `gemma3:12b`, `gemma3:27b`, `llama3.1:8b`,
`phi3:3.8b`, `phi3:14b`, `phi4:14b`, and `gpt-oss:20b` (pulled on 28/01/2026) and a GPU with ≥ 32 GB VRAM
(we used an RTX 5090 with CUDA 13.0 on Kubuntu 24.04). The main simulation took about 7.5 days and the
temperature simulation about 16.5 hours. Because model weights may change and gpt-oss (20b) is not fully
deterministic even with a fixed seed, re-running the simulation may not reproduce the ratings exactly;
the saved per-call seeds are included in the results.

    systemd-inhibit R CMD BATCH --vanilla run_simulations.R sim_results/sim_results_documentation.Rout

## Simulation results (`sim_results/`)

- `sim_results_valence.RData`, `sim_results_arousal.RData`: one row per LLM call (614,400 per construct) with the columns
  - `format`: response format (1 = 0–100, 2 = 1–9)
  - `example`: prompt examples (1 = none, 2 = lower anchor, 3 = upper anchor, 4 = both anchors)
  - `models`: Ollama model tag
  - `scale_min`, `scale_max`: bounds of the response scale
  - `iteration`: simulation iteration (1–100)
  - `stimulus_id`: index of the multi-word expression (1–96, order as in the Martínez et al. stimulus file)
  - `rating_with_invalid`: raw integer returned by the model (`NA` if Ollama returned an error)
  - `rating`: rating after setting values outside the admissible range to `NA`
  - `seed`: seed passed with the request
  - `system_prompt`: index of the (empty) system prompt
- `correlations_valence.csv`, `correlations_arousal.csv`: one row per condition (`models`, `format`, `example`) with
  - `correlation`: mean correlation between LLM and human ratings across iterations
  - `cor_lb`, `cor_ub`: 95% bounds based on the Monte Carlo standard error (MCSE)
  - `cor_uncertainty_lower`, `cor_uncertainty_upper`: 95% confidence bounds of the correlation per iteration
    (reflecting the 96 expressions), averaged across iterations
- `analysis_valence_documentation.Rout`, `analysis_arousal_documentation.Rout`: `R CMD BATCH` console logs from running
  `analyze_results.R` for each construct (session information, all printed results, and run time at the end)
- `sim_results_documentation.Rout`: console log of the main LLM simulation (`run_simulations.R`)
- `sim_results_documentation_temperature_analysis.Rout`: console log of the temperature simulation (`temperature_simulation.R`)

## External data (Martínez et al., 2024)

Stimuli and human ratings are from Martínez et al. (2024, https://doi.org/10.3758/s13428-024-02515-z), available at https://osf.io/k5a4x (folder "Results validation Studies 4-5"). They are not redistributed here.

- `96_selected_stimuli_human_validation_valence.xlsx`
- `96_selected_stimuli_human_validation_arousal.xlsx`
- `Evaluation validity valence with human raters on Prolific for R analysis.xlsx`
- `Evaluation validity arousal with human raters on Prolific for R analysis.xlsx`

## Pilot study (`pilot_study/`)

Files of an initial pilot study used for developing the simulation workflow; they are not part of the paper.
The files have the same structure as the files used for the LLM simulation of valence and arousal:

- `simulation_code.R`: functions for running the LLM simulation (pilot variant of the simulation code)
- `run_complexity_simulation.R`: script that loads the data and executes the LLM simulation of syntactic complexity ratings
- `analyze_sim_results.R`: script used for analyzing the pilot-study simulation results
- `pilot_study.Rproj`: RStudio project file (open it to work with the scripts in this folder)

The results of the pilot simulation are not included in this repository. To reproduce the pilot analysis, first run the simulation yourself. It requires Ollama with the same eight models as the main simulation (see above) and covers 8 models × 2 response scales × 4 example conditions × 100 iterations:

    systemd-inhibit R CMD BATCH --vanilla run_complexity_simulation.R sim_results/sim_results_documentation.Rout

The script creates the folder `pilot_study/sim_results/` and saves the results to `sim_results/simulation_results.RData`, which is then loaded by `analyze_sim_results.R`. Run both scripts with `pilot_study/` as the working directory.

### External data (Schmidt & Heck, 2024)

The material used for the pilot study is from Schmidt & Heck (2024, https://doi.org/10.1016/j.concog.2023.103623), available at https://osf.io/vp2nu. It is not redistributed here and has to be placed in the `pilot_study/` folder to run the scripts:

- `study_3_statements-statistics.csv`
- `mixed_models_item_filtered_study_3.RData`

## Citation

Please cite the paper above.
