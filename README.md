# article_trait_conf_comms_dyads
This repository contains the anonymised data and R scripts supporting the article: <br>
 <br>
[Blanchard et al. (2026) How Trait Confidence and Communication Shape Dyadic Decision Outcomes and Decision-Specific Confidence Matching](https://osf.io/preprints/psyarxiv/rft7q_v1) <br>
 <br>
 <br>
There are three R scripts to reproduce the main analyses:<br>
- **functions.R**: loads the packages and custom functions required to run the other three scripts<br>
- **1_descriptive_stats.R**: calculates the descriptive statistics and related plots<br>
- **2_analyses_acc_conf.R**: fits the models and produces plots for decision accuracy and decision confidence<br>
- **3_analyses_conf_match.R**: fits the models and produces plots for decision-specific confidence matching<br>
<br>
<br>
The data file contains the following information: <br>
**part_id**: the unique ID for each participant, which is group_id and member_id combined <br>
**group_id**: the unique ID for each dyad <br>
**member_id**: the unique ID for each member within a dyad (a or b) <br>
**conf_cond**: the trait confidence condition (Low, Mixed, or High) <br>
**comm_cond**: the communication condition (Isolated, Passive, or Active) <br>
**itemnum**: the unique item number for each general knowledge question <br>
**grouping**: indicates whether a response was given by an individual or dyad <br>
**resp**: the submitted answer for an item <br>
**resp_rt**: the response time for the answer (resp) <br>
**conf**: the confidence rating for an item <br>
**conf_rt**: the response time for the confidence rating (conf) <br>
**correct_resp**: the ground truth for an item <br>
**resp_acc**: the accuracy of the response (TRUE or FALSE) <br>
**item_mean**: mean individual accuracy for an item across participants <br>
**consensus**: indicates whether dyad members agreed or disagreed for their individual responses to an item <br>
**sex**: the sex of a participant (0 = female, 1 = male) <br>
**education**: higher scores indicate more education (1 = bachelor, 2 = graduate diploma, 3 = honours, 4 = masters, 5 = phd, 0 = not currently studying, 99 = other) <br>
**eng_fl**: indicates whether english is a participant's first language (0 = no, 1 = yes) <br>
**age**: age in years of a participant <br>
**trait_acc**: mean accuracy for accuracy scores on the two cognitive ability tests (RAPM and EAT) <br>
**trait_conf**: mean confidence for the confidence ratings on the two cognitive ability tests (RAPM and EAT) <br>
**eat_acc**: mean accuracy on the esoteric analogies cognitive ability test <br>
**eat_conf**: mean confidence on esoteric analogies cognitive ability test <br>
**rapm_acc**: mean accuracy on Raven's advanced progressive matrices cognitive ability test <br>
**rapm_conf**: mean confidence on Raven's advanced progressive matrices cognitive ability test <br>
**agreeableness**: mean score across items for the Big Five personality trait <br>
**conscientiousness**: mean score across items for the Big Five personality trait <br>
**extraversion**: mean score across items for the Big Five personality trait <br>
**intellect**: mean score across items for the Big Five personality trait <br>
**neuroticism**: mean score across items for the Big Five personality trait <br>





