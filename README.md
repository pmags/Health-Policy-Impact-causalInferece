# Measuring the impact of the introduction of a mandatory pre-triage to emergency services in Portugal

## Motivation

The high pressure on emergency rooms has long been a significant concern for the Portuguese National Health System. This issue becomes particularly problematic during periods of increased demand, such as the winter season, when the pressure is exacerbated by the seasonal flu and other similar illnesses. In response to these challenges, public health authorities introduced a mandatory telephone pre-triage system, which was initially trialed in December 2023 at a limited number of locations. Alongside this, a new policy was implemented to refer patients to private hospitals. The mandatory pre-triage system is now promoted as a central component of the government’s strategy to alleviate emergency room congestion.

This study is motivated by an initial analysis conducted by Professor Pedro Pita Barros, an economics professor at Nova SBE and a recognized expert in health economics. His initial findings, published as a blog on December 18, 2024, examined emergency occurrences at the Unidade de Saúde de Vila do Conde (ULS Vila do Conde), one of the trial locations, and tracked the evolution of emergency room visits following the introduction of the mandatory pre-triage system in January 2024. In his study, the city of Barcelos was used as a comparison group to assess the potential impact of the intervention [see @barros2024]. From this initial work he concludes that there evidence to support that the introduction of this new policy lead to a 10% drop in emergencies.

This study goal is to use traditional time series statistics to study the same data and compare if the results of a model driven analysis are aligned with the initial findings suggested by [@barros2024]. For this ARMA models will be used and later expanded using dynamic time series models to incorporate the effect of a reference series. This study approach focus on using a counter-factual to simulate a series without any intervention and later compare to real measurements. The goal is not to challenge or further support the 10% drop claim, instead, given the number of assumptions taken, is to conclude if indeed this intervention had any significant impact.

This analysis uses the exact same assumptions as [@barros2024] original post, namely the use of total emergencies as the relevant indicator and the assumption of ULS Barcelos as a reference series. In the conclusions some additional questions are risen regarding impacts and assumptions, namely if total emergencies is most appropriate indicator.


## How to rende the report

The final report was produced using the `quarto` package. To render the report, you need to have the `quarto` package installed. You can install it using the following command:

```
quarto render notebooks/report.qmd --to pdf
```