# k-means clustering in R

Segmenting a film catalogue on budget and gross revenue to decide which 100 titles a company should license.

> Written in 2020 during my MSc Data Analytics at London Metropolitan University.

## The question

Given a catalogue of films with budget and box-office figures, which cluster represents the titles worth adding to a company's library?

## Approach

- **Distance measure:** Euclidean, over budget and gross
- **Choosing k:** within-cluster sum of squares plotted against k, with the elbow used to pick the optimal value
- **Sensitivity:** the analysis is then re-run at a higher k, because the elbow is a heuristic and it is worth seeing whether the story survives a finer partition
- **Outcome:** the two analyses together are used to select 100 films, saved to [`Selected 100 movies.csv`](Selected%20100%20movies.csv)

Re-running at a second k is the step that turns this from "the elbow said 4" into an actual decision.

## Reports

| Report | What it covers |
| --- | --- |
| [k-means in R, explained](K-means-R-explanation-.pdf) | The method walked through alongside the R code |
| [Full analysis](k-means%20clustering.pdf) | Complete analysis, cluster interpretation and the resulting selection |

## Data

`movie_metadata.csv` — the Kaggle film metadata set.

## Stack

R · ggplot2 · R Markdown

---

For current work, see [rag-eval-harness](https://github.com/ChinduSahid/rag-eval-harness) and [medallion-duckdb](https://github.com/ChinduSahid/medallion-duckdb).
