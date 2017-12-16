# Head dimorphism

Analysis of European eel head dimorphism in the Scheldt River basin

## Project structure

### Data-analysis

* `/src:`
    1. `Preprocessing.R`: upload eel data and change column names
    2. `Determine_headwidth.R`: Determine head width categories (narrow, broad and intermediate) and add head width data to the dataset
    3. `Condition.R`: Calculate relative condition factor according to Le Cren
    4. `Analysis_condition.R`: analyse if the relative condition differs according to head width, nested within maturation stages
    5. `Analysis_stadia.R`: analyse if number of narrow and broad heads differ between maturation stadia
    6. `Link_onset_duration.R`: Link the onset and duration of migration to the dataset
    7. `Analysis_onset.R`: analyse if number of narrow and broad heads differ between months of onset
