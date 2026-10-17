# BikeFlow QC — Bikeshare Fleet Monitoring

**Team Plasma | Six Sigma Hackathon 2026 | Urban Systems: Bikeshare Fleets**

## Problem and purpose

Bikeshare customers cannot rent a bike at an empty station or return one at a full station. During peak periods, these events reduce service availability. BikeFlow QC is an interactive **R Shiny** demonstration that helps dispatchers identify unusable stations, compare service levels between neighborhoods, and prioritize bicycle redistribution using transparent rules.

**Important:** All data are **synthetic and simulated**. The application is not connected to a real bikeshare operator, live telemetry, or dispatch system.

## What the dashboard does

- Shows station count, service availability, currently full/empty stations, and unusable station-hours for a selected day.
- Plots bicycle inventory over time for a selected station.
- Displays the percentage of unusable station-observations by hour.
- Classifies stations as **EMPTY**, **FULL**, **WATCH**, or **OK**. The warning threshold is adjustable.
- Produces a dispatch priority table that ranks critical stations by consecutive unusable observation duration, then the magnitude of a proposed adjustment toward half capacity.
- Compares neighborhood-level service availability and unusable station-hours.
- Provides an **exploratory** p-chart and exports the synthetic dataset as CSV.

## Synthetic data and assumptions

- **20 stations**, each with a randomly sampled capacity of **15–30 docks**.
- **Seven days** of observations, recorded every **15 minutes** (13,440 station-observations in total).
- A fixed random seed (`set.seed(5300)`) makes the simulated dataset reproducible.
- Three neighborhood classifications: **Residential, Downtown, and Mixed**.
- Assumed weekday commuting patterns influence simulated Poisson rental and return counts; demand patterns vary by neighborhood.
- Scheduled redistribution to approximately half capacity occurs at **04:00** each day.
- Rentals are limited by available bikes and returns by available docks; consequently **bikes + open docks = capacity** at each snapshot.
- Movement quantities are recommendations in bikes, not optimized van routes or proof that physical transfers are feasible across the fleet.

## Statistics and definitions

### Station usability

A station is **unusable** for an observation if there are **zero bikes** or **zero open docks**. Otherwise, it is usable.

### Availability

\[
\text{Availability} = \frac{\text{number of usable station-observations}}{\text{total station-observations}} \times 100\%.
\]

The dashboard calculates this for the selected day and for each neighborhood. This is a **service-availability metric**, **not** the process-capability indices Cp or Cpk.

### Unusable station-hours

\[
\text{Unusable station-hours} = \text{number of unusable 15-minute observations} \times 0.25\text{ h}.
\]

This is summed across stations. It is **not** elapsed wall-clock time for the entire network.

### Current outage duration

The dashboard counts each station's consecutive unusable 15-minute observations ending at the selected day's final snapshot and multiplies the count by **0.25 hours**. This is a **sample-based estimate**, not a continuously measured outage timer. A run can continue across midnight.

### Exploratory p-chart

Each hourly sample contains 20 station observations. The proportion unusable is `p = number unusable / 20`. The overall mean proportion is `p_bar`. Illustrative bounds are

\[
\text{LCL} = \max(0,\bar p-3\sqrt{\bar p(1-\bar p)/20}), \quad
\text{UCL} = \min(1,\bar p+3\sqrt{\bar p(1-\bar p)/20}).
\]

**Statistical limitation:** Classic p-chart limits assume appropriate independent binomial sampling and a stable underlying event probability. This simulation contains time-of-day patterns and repeated correlated station observations, so its plotted limits are **descriptive screening references**, not formal evidence of statistical control or causal special causes. No inferential claim is made from limit crossings.

## How to run locally

Requires **R** and the **shiny** R package. In Positron or an R console:

```r
install.packages("shiny") # first time only
shiny::runApp("bikeflow-qc/shinyapp") # from the team-plasma repository root
```

Alternatively, set the working directory to this `shinyapp` folder and run `shiny::runApp(".")`.

For reproducibility, the simulation is regenerated with the same random seed each time the application starts. The code includes data-invariant checks and the interface has a **Download simulated CSV** button.

## Repository and submission

- Source code: https://github.com/lel223/team-plasma/tree/main/bikeflow-qc/shinyapp
- Main Shiny entry point: `app.R`
- Deployment manifest: `manifest.json`
- Live Posit Connect deployment URL: **to be added once deployed**

## Limitations and future work

This is a decision-support demonstration using synthetic data. It does **not** consume live station feeds, model actual repair incidents, estimate causal effects, optimize vehicle routes, calculate response costs, or forecast service interruptions. Possible future work includes ingesting actual dock-status records, adjusting for daily seasonality, tracking actual maintenance events, and evaluating interventions with verified operational data.
