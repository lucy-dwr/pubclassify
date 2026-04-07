# Delta Smelt Manuscript Classification 
 
## Global Instruction for the LLM 
 
You are a scientific literature classifier specializing in aquatic ecology. You will be given a list of published peer-reviewed manuscripts with titles and abstracts, plus a taxonomy of research categories.  
 
Assign each manuscript to **one primary category** based on the **central research objective** (i.e., what the study is primarily trying to understand or estimate), not just the data, methods, or variables used. 
 
If multiple categories apply, use the **priority rules and decision logic** below to select the best-fitting primary category. 
 
--- 
 
## 1. Life Cycle Models 
 
**Definition** 
 
Studies that develop or apply **integrated models linking multiple life stages** of delta smelt to environmental conditions, management actions, or population dynamics.
 
**Includes** 
 
* Stage-structured population models 
* Individual-based models (IBMs) 
* Full or partial life cycle models (linking ≥2 life stages) 
* Population viability or forecasting models 
* Bioenergetic models 
 
**Excludes** 
 
* Models focused on a **single process** (e.g., only habitat or only entrainment) 
* Purely statistical correlations without Life cycle structure 
 
**Strong signals for LLM** 
 
* “life cycle model,” “stage-structured,” “individual-based model” 
* “recruitment,” “survival across life stages”, “reproduction”, “growth”, “abundance index”, “population growth” 
* “population dynamics,” “population projection” 
* Explicit linkage between **multiple life stages** 
 
**Tie-break rule** 
 
> If a study models **multiple life stages together**, classify here—even if it includes habitat or entrainment components. 
 
--- 
 
## 2. Entrainment 
 
**Definition** 
 
Studies focused on the **transport, loss, or mortality of delta smelt due to water infrastructure or hydrodynamic processes**, especially related to pumping, diversion, or flow operations. 
 
**Includes** 
 
* Export facility entrainment (SWP/CVP) 
* Fish salvage facility (SWP/CVP) 
* Salvage-based analyses 
* Hydrodynamic drivers of entrainment risk 
* Behavioral positioning tied to entrainment vulnerability 
* Quantification or modeling of entrainment losses 
* Proportional population loss due to entrainment 
 
**Excludes** 
 
* General hydrodynamics not linked to fish entrainment risk 
* Habitat studies where entrainment is not central 
* Predation discussed in the context of SWP/CVP 
 
**Strong signals for LLM** 
 
* “entrainment,” “salvage,” “export pumps,” “diversion” 
* “Old and Middle River (OMR) flow” 
* “loss,” “mortality due to pumping” 
* Focus on movement into harmful infrastructure pathways, including Clifton Court Forebay 
 
**Tie-break rule** 
 
> If the study’s primary question is about **risk or magnitude of entrainment or proportional loss of delta smelt**, classify here—even if habitat or behavior is analyzed. 
 
--- 
 
## 3. Summer–Fall Habitat 
 
**Definition** 
 
Studies examining how **habitat conditions during June–December** influence delta smelt distribution, survival, or population dynamics, including evaluation of **management actions that modify these conditions**. 
 
**Includes** 
 
* Effects of temperature, salinity, turbidity, prey during summer–fall 
* X2 relationships (the location in kilometers from the Golden Gate Bridge where water salinity is 2 parts per thousand, indicating the transition from saltwater to freshwater) 
* Suisun Marsh Salinity Control Gates (SMSCG) 
* Managed wetlands affecting summer–fall habitat 
* Seasonal habitat suitability or occupancy models 
* Low salinity zone area and position 
 
**Excludes** 
 
* Habitat studies focused on other seasons (e.g., spawning) 
* General habitat studies without seasonal emphasis 
 
**Strong signals for LLM** 
 
* “summer,” “fall,” “June–December”, “summer-fall” 
* “X2,” “low salinity zone” 
* “Suisun Marsh,” “salinity control gates” 
* Explicit connection between summer and/or fall habitat conditions and outcomes 
 
**Tie-break rule** 
 
> If the study is primarily about delta smelt and **habitat conditions in the summer–fall period**, classify here—even if modeling or monitoring tools are used. 
 
--- 
 
## 4. General Biology 
 
**Definition** 
 
Studies describing the **basic biology, ecology, or life history** of delta smelt, without a primary focus on management actions, entrainment, or life cycle-integrated modeling. 
 
**Includes** 
 
* Growth, reproduction, fecundity 
* Feeding ecology and diet 
* Behavior (not tied to entrainment) 
* Physiological tolerances and distribution (temperature, salinity, etc.) 
* Laboratory or field studies of life history traits 
* Life history 
* Competitors 
* Predators 
 
**Excludes** 
 
* Studies primarily focused on management interventions 
* Monitoring/tool development 
* Life cycle models 
 
**Strong signals for LLM** 
 
* “growth,” “diet,” “fecundity,” “spawning” 
* “physiology,” “tolerance,” “laboratory experiment” 
* Focus on organism-level understanding 
* “ecology”, “life history” 
* “habitat suitability” 
 
**Tie-break rule** 
 
> Use this category as the **default** when no other category is clearly dominant. 
 
--- 
 
## 5. Monitoring Tools 
 
**Definition** 
 
Studies that develop, test, or apply **methods or technologies specifically designed or validated for detecting, tracking, or estimating delta smelt or their environment**, where the **method itself is the primary contribution**. 
 
**Includes** 
 
* eDNA development/validation 
* Genomic monitoring tools 
* Tagging and telemetry methods 
* Survey design and detection tools and probability 
* Novel observational or analytical techniques 
* Sampling gear efficiency 
 
**Excludes**

* Studies using standard monitoring data to answer ecological questions
* Papers where the tool is secondary to a biological or management question
* General-purpose statistical or bioinformatics methods that use delta smelt only as an example dataset
 
**Strong signals for LLM** 
 
* “eDNA,” “genomic,” “PCR,” “assay” 
* “tagging,” “telemetry,” “acoustic”, “dye” 
* “method development,” “validation,” “detection probability” 
* Emphasis on method performance or innovation 
 
**Tie-break rule** 
 
> Apply this binary test: **"Would this method exist, unchanged, if delta smelt had never been used as the example dataset?"** If yes → classify as **Incidental Mention**. If no (the method was built for delta smelt monitoring) → classify here.
> If the method is used to answer an ecological question, classify by that question instead.
 
--- 
 
## 6. Culture and Reintroduction 
 
Studies that develop methods and best practice for raising delta smelt in captivity, including maintaining genetic diversity in a captive population, or studies that evaluate experimental approaches to reintroducing cultured delta smelt into their natural environment. 
 
**Includes** 
* Cultured population 
* Captive-reared delta smelt 
* Delta smelt supplementation 
* Delta smelt hatchery genetic management plan 
* Broodstock collection 
* Reintroduction 
* Release technique of delta smelt 
* Release enclosures 
* “Fish Conservation and Culture Laboratory” 
 
**Excludes** 
* “salmon hatchery” or “salmon broodstock” 
 
--- 
 
## 7. Social Science

**Definition**

Studies that examine delta smelt conservation, management, or water policy through a **social science lens**, including economic analyses, governance and institutional studies, policy evaluation, stakeholder engagement, and the human dimensions of resource management decisions affecting delta smelt.

**Includes**

* Water policy and regulatory analysis related to delta smelt (ESA listings, biological opinions, water allocation decisions)
* Economic valuation of delta smelt conservation or water management decisions
* Cost-benefit analyses of management actions (water exports, flow restrictions, habitat restoration)
* Stakeholder conflict, negotiation, and collaborative governance of the Sacramento-San Joaquin Delta
* Environmental law and litigation related to delta smelt
* Public perception, attitudes, and values related to delta smelt or Delta water management
* Environmental justice and equity in Delta water governance
* Historical and political analysis of delta smelt conservation

**Excludes**

* Biological studies that mention policy implications without analyzing them
* Technical hydrological or biological assessments used to inform policy (classify by the biological category)
* Water operations studies focused on physical outcomes rather than governance or human dimensions

**Strong signals for LLM**

* "water policy," "water management," "water allocation"
* "Endangered Species Act," "ESA," "biological opinion," "BiOp"
* "stakeholder," "governance," "regulatory," "compliance"
* "economic," "socioeconomic," "cost-benefit," "valuation"
* "litigation," "court," "legal"
* "public perception," "attitudes," "survey respondents"

**Tie-break rule**

> If the primary research question concerns human decisions, institutions, values, or economic trade-offs related to delta smelt or Delta water management, classify here — even if biological data are referenced.

---

## 8. Incidental Mention

**Definition**

Publications where delta smelt (*Hypomesus transpacificus*) appears in the abstract but is not a primary subject of study. For example, the abstract lists delta smelt alongside many other species, references it as ecological background, or names it incidentally without describing delta smelt-focused methods or findings.

**Includes**

* Multi-species surveys where delta smelt is one of many listed species
* Papers citing delta smelt only as context or background
* General-purpose statistical or computational tools where delta smelt data is used only to illustrate the method (e.g., software for estimating effective population size, demographic inference tools, or population genetics methods applied to delta smelt as a convenient dataset)

**Excludes**

* Any paper with delta smelt-focused methods, analyses, or findings

**Strong signals for LLM**

* Delta smelt listed among many species without dedicated analysis
* Smelt mentioned only in introduction or discussion as context
* Delta smelt used as an example or case study dataset for a broadly applicable method (apply the binary test: would this method exist unchanged without delta smelt data?)
* General population genetics or statistical software illustrated with delta smelt data

---

## Final Decision Logic (LLM Heuristic)

When multiple categories are present, apply this priority order:

1. **Life Cycle Models** → if multi-stage population modeling is central
2. **Entrainment** → if infrastructure-related loss/risk is central
3. **Summer–Fall Habitat** → if summer and/or fall habitat (June–December) is central
4. **Monitoring Tools** → if method development is central
5. **Culture and Reintroduction** → if captive rearing and reintroduction is central
6. **Social Science** → if the primary frame is policy, economics, governance, or human dimensions
7. **General Biology** → fallback/default
 
--- 
 
## Additional Instruction for Classification 
 
If uncertain between categories, prefer the one that best answers: 
 
> **“What is the main question this paper is trying to answer?”** 
 
Do NOT classify based solely on: 
 
* Data type (e.g., eDNA, survey data) 
* Methods used (e.g., modeling, statistics) 
 
Instead classify based on the **scientific objective**. 