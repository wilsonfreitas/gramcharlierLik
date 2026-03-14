# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

R research project exploring Maximum Likelihood estimators for skewness and kurtosis using Gram-Charlier expansions applied to Brazilian financial time series (IBOVESPA, PETR4). The project compares ML estimators with bootstrap sample estimators and extends to GARCH models with Gram-Charlier distributed innovations and option pricing via Corrado & Su method.

**Language:** R | **Author:** Wilson Freitas | **Code comments/variables:** Portuguese

## Setup and Running

This is an interactive R research project (not a package). Open `gramcharlierLik.Rproj` in RStudio.

```bash
# Install all required R packages
Rscript setup.R

# Render all R Markdown papers (PDF by default)
Rscript render.R                   # render all files as PDF
Rscript render.R html              # render all files as HTML
Rscript render.R all               # render all files as PDF and HTML
Rscript render.R pdf pricing       # render pricing paper as PDF
Rscript render.R pdf estimadores   # render estimadores paper as PDF

# Source individual analysis scripts in R
source("functions.R")
source("garchEstimate.R")
source("garchgcEstimate.R")
source("bootstrap-moments.R")
```

## Key Dependencies

`boot`, `cowplot`, `DEoptim`, `fGarch`, `ggplot2`, `kableExtra`, `knitr`, `nloptr`, `PDQutils`, `PerformanceAnalytics`, `profvis`, `rbcb`, `rmarkdown`, `tidyverse`, `timeDate`, `xts`, `zoo`

## Architecture

### Core Library: `functions.R`

All reusable functions live here. Key components:

- **Gram-Charlier density:** `dgramcharlier(x, mu3, mu4)` and `dgramcharlier_adj()` — polynomial expansion of normal distribution capturing skewness/kurtosis
- **Likelihood functions:** `gclogLik(x)`, `gcUnconstrainedlogLik(x)` — negative log-likelihood for ML estimation
- **Constraint region:** `uncons_regionD()`, `regionD()` — maps unconstrained parameters to valid (skewness, kurtosis) region using logistic transforms, ensuring density non-negativity
- **GARCH functions:** `garch_fit()`, `var.garch()`, `garch_sim()` — GARCH(1,1) fitting and simulation
- **GARCH-GC likelihood:** `.garch_gc_likelihood(rets)`, `.garch_gc_likelihood2(rets)` — hybrid GARCH + Gram-Charlier models

### Key Mathematical Pattern

The Gram-Charlier density is `p(x) = φ(x) × ψ(x)` where φ is the standard normal and ψ is a polynomial correction involving Hermite polynomials parameterized by skewness (mu3) and excess kurtosis (mu4). The valid parameter space is constrained to ensure non-negative density — this is handled by transforming to an unconstrained space rather than using explicit bounds.

### Research Papers (R Markdown)

- `estimadores-assimetria-curtose/` — Main paper on ML estimators for skewness and kurtosis
- `pricing-options-garch-gram-charlier/` — Application to option pricing and volatility surfaces

### Data

- IBOVESPA index data (1993-2014) in `.csv` and `.RData` formats
- PETR4 (Petrobras) daily returns in `.csv`
- Recent code uses `rbcb` package for fetching Brazilian Central Bank data

## Linting

Uses `lintr` for static analysis. Configuration in `.lintr`.

```bash
# Lint all R files
Rscript lint.R

# Lint specific files
Rscript lint.R functions.R garchEstimate.R
```

A Claude Code hook (`.claude/hooks.json`) automatically runs `lintr` on every R file after Write/Edit operations.

**MANDATORY: Before considering any task complete, run `Rscript lint.R` on all modified R files and fix every reported issue. A task is only done when the linter reports zero problems.**

Disabled linters (to accommodate mathematical notation style): `infix_spaces_linter`, `spaces_inside_linter`, `spaces_left_parentheses_linter`, `quotes_linter`, `object_name_linter`, `commented_code_linter`.

## Style

- 2-space indentation (configured in `.Rproj`)
- UTF-8 encoding
- Use `<-` for assignment (not `=`)
- Use `FALSE`/`TRUE` (not `F`/`T`)
- No trailing whitespace
- Space after commas
- Max 120 character line length
- Multiple optimization methods compared: `nlminb()`, `optim()`, `nloptr` (NLOPT_LD_MMA), `DEoptim`
