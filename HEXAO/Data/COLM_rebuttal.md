## R1
Dear reviewer 3FvA,
Thank you for your comments and deep thinking on this research question. 

* *What would really improve this paper a lot is spending some time discussing construct validity and external validity. Why are we testing models for these kinds of biases? What do we conclude about them when they get these results?*

We agree with your opinion and believe that discussing construct validity and external validity will improve this article. **We will add the relevant discussions to the final version.**
We think that LMs might be more structured and consistent in their decision-making processes because most models follow clear algorithms and rules to process data and generate outputs. However, LMs face challenges when dealing with unseen situations or those outside the scope of the training data, which is an issue of external validity. How can this problem be addressed? In psychology, common methods to improve external validity include improving sample diversity and improving fidelity. Therefore, when we collect such a dataset, we try to provide diverse contexts for each type of representativeness heuristic and more realistic descriptions. **We test the model’s performance on these diverse and high-fidelity questions in our dataset to infer its performance in broader external scenarios.** Additionally, in psychology, there are stable traits and behavior patterns between individuals, as well as changes in behavior and psychological states within individuals across different times and contexts. **Similarly, here we want to measure the consistency between models rather than the variability within a model.**

* *The base rate fallacy picture doesn't seem to be a great example of the fallacy. Would be nice if you could illustrate it there too. Unless I'm missing something?*

Thank you for the advice regarding the base rate fallacy figure. We agree with your point that although the current image also reflects the Bayesian theorem of the base rate fallacy, it is not a good example to demonstrate the counterintuitive aspect of the base rate fallacy. If the blue dots within the large circle but outside the small circle change to red, and vice versa the red dots will make the figure better. Most of the blue dots represent the majority of people who have symptoms, but only a small portion are actually sick. Therefore, while a high proportion of the sick have symptoms, it does not mean that a high proportion of those with symptoms are sick. **We will make the necessary changes to the figure accordingly.**

* *The caption on the Disjunction Fallacy image is ungrammatical.*

**Thank you for the reminder about the grammatical error, the new caption is “A or B. A. Therefore, if A, then not B. However, if A is a subset of B, this conclusion would be incorrect.”**


## R2
Dear reviewer f6wA,
We appreciate your effort in providing positive feedback on our manuscript. Below we would like to answer your questions by presenting additional experiments.

* *Do you think more sophistic prompting strategies such as multi-agent debating can bring larger improvements?*

**We agree that more complex prompting strategies can bring improvements, similar to how humans, when faced with heuristic questions, think deeply and invest more mental resources to avoid heuristic biases.** And we provide the following experiment…

* *It would be better to provide experiments of testing models performance across different scales.*

**To test model performance across different scales, we provide an additional experiment to compare the performance of LLaMA7b to LLaMA70b.**

* *Is there any results on average human accuracy on the proposed testset?*

After the completion of IRB approval from the institution, we are very glad to present the human evaluation results of our proposed ReHeAT dataset. Through the Prolific platform, we recruit 153 participants to answer questions. Participants are asked to answer 3-4 types of representative heuristic questions. The human results are close to those of LLaMA2-70B, while the GPT series performs better than the human results. **We will incorporate these new results into our final version and aim to provide more comprehensive outcomes.**

| Type           | Base Rate Fallacy | Conjunction Fallacy | Disjunction Fallacy | Insensitivity to Sample Size | Misconceptions of Chance | Regression Fallacy | Average |
|----------------|-------------------|---------------------|---------------------|------------------------------|--------------------------|--------------------|---------|
| Human          | 27.7              | 24.4                | 12.5                | 33.3                         | 50.0                     | 85.7               | 27.2    |
| GPT-3.5        | 31.9              | 31.1                | 22.9                | 45.1                         | 50.0                     | 71.4               | 34.7    |
| GPT-4          | 34.0              | 20.0                | 6.3                 | 54.9                         | 50.0                     | 71.4               | 31.2    |
| PaLM 2         | 19.1              | 2.2                 | 2.3                 | 22.0                         | 50.0                     | 71.4               | 14.8    |
| LLaMA2-70B     | 23.4              | 20.0                | 10.4                | 43.1                         | 75.0                     | 71.4               | 27.2    |


## R3
Dear reviewer eSG3,
We sincerely thank your insightful comments and valuable suggestions. Below, we would like to address your concerns raised in reasons to reject part:

* *This paper aims to investigate whether LLMs make representativeness heuristic mistakes similar to humans, but understanding the results is challenging without human performance for comparison. It would be helpful if the authors could conduct similar human studies or, at the very least, provide references from previous studies, given that some questions are directly drawn from prior research.*

After the completion of IRB approval from the institution, we are very glad to present the human evaluation results of our proposed ReHeAT dataset. Through the Prolific platform, we recruit 153 participants to answer questions. Each participant was required to answer 3-4 different types of representative heuristic questions. **We will incorporate these new results into our final version and aim to provide more comprehensive outcomes.**

| Type           | Base Rate Fallacy | Conjunction Fallacy | Disjunction Fallacy | Insensitivity to Sample Size | Misconceptions of Chance | Regression Fallacy | Average |
|----------------|-------------------|---------------------|---------------------|------------------------------|--------------------------|--------------------|---------|
| Human          | 27.7              | 24.4                | 12.5                | 33.3                         | 50.0                     | 85.7               | 27.2    |
| GPT-3.5        | 31.9              | 31.1                | 22.9                | 45.1                         | 50.0                     | 71.4               | 34.7    |
| GPT-4          | 34.0              | 20.0                | 6.3                 | 54.9                         | 50.0                     | 71.4               | 31.2    |
| PaLM 2         | 19.1              | 2.2                 | 2.3                 | 22.0                         | 50.0                     | 71.4               | 14.8    |
| LLaMA2-70B     | 23.4              | 20.0                | 10.4                | 43.1                         | 75.0                     | 71.4               | 27.2    |

* *The paper mentions “49 questions drawn directly from previous research and 153 new questions”, but the curation process for the new data is unclear. Also, how diverse are the new questions?*

The 153 new questions were organized to retain the essence of the representativeness heuristic being tested while changing the situations and contexts of the original items. **Since each type of representativeness heuristic question shares the same statistical prototype, we aimed to display diversity in the contexts of the questions.** There are approximately 15 to 18 different contexts for each type of question.

And for the questions:

* *The experiments on IQs and SPQs appear to only use one example per question type, but it remains unclear whether this performance would remain consistent with more examples. It would be better if the authors could include results using more examples.*

Thank you for your advice. Each type of representativeness heuristic has the same statistical prototype. Therefore, only one SPQ is needed for each type of representativeness heuristic, and we collected the majority of answers for each SPQ through self-consistency. While IQs could indeed benefit from more examples for demonstration, considering that one of the purposes of IQs is to directly compare with SPQs, we adopted the same test format as the SPQs.

* *How are the ICL examples selected in section 5.1?*

**The examples in Section 5.1 for IC are randomly selected from diverse backgrounds.**

*Missing reference that compares human and LLMs reasoning, e.g. [1]
[1] Ishita Dasgupta, Andrew K. Lampinen, Stephanie C. Y. Chan, Hannah R. Sheahan, Antonia Creswell, Dharshan Kumaran, James L. McClelland, Felix Hill. Language models show human-like content effects on reasoning tasks.*

Thank you for recommending the missing references. **We will add this reference in our final version.**


## R4
Dear reviewer 4fxrR,
Thank you for your comments and for recognizing the value of our work.

* *Change Subtitle 5.3: "Improving Performance by Hinting (that LLMs to ?) use Their Knowledge"*

**For the subtitle 5.3, thank you very much for your suggestion. We have changed the title accordingly to “Improving Performance by Hinting that LLMs Use Their Knowledge.”**

* *Discussing your results in light of human base rates would be interesting.*

Following the completion of IRB approval from the institution, we are pleased to present the human evaluation results of our proposed ReHeAT dataset. Through the Prolific platform, we recruited 153 participants to answer these questions. Each participant was required to respond to 3-4 different types of representative heuristic questions. **We will integrate these new results into the final version of our study to provide more comprehensive outcomes.**

| Type           | Base Rate Fallacy | Conjunction Fallacy | Disjunction Fallacy | Insensitivity to Sample Size | Misconceptions of Chance | Regression Fallacy | Average |
|----------------|-------------------|---------------------|---------------------|------------------------------|--------------------------|--------------------|---------|
| Human          | 27.7              | 24.4                | 12.5                | 33.3                         | 50.0                     | 85.7               | 27.2    |
| GPT-3.5        | 31.9              | 31.1                | 22.9                | 45.1                         | 50.0                     | 71.4               | 34.7    |
| GPT-4          | 34.0              | 20.0                | 6.3                 | 54.9                         | 50.0                     | 71.4               | 31.2    |
| PaLM 2         | 19.1              | 2.2                 | 2.3                 | 22.0                         | 50.0                     | 71.4               | 14.8    |
| LLaMA2-70B     | 23.4              | 20.0                | 10.4                | 43.1                         | 75.0                     | 71.4               | 27.2    |


* *I see the note on page 8 that human base rates are around 10 to 50%. In a way, should we expect LLMs to exhibit this bias, in that they are trained to replicate the judgments of a population of humans?*

Thank you so much for asking such a good question. **We do observe similar performance between humans and LMs. However, regarding whether we should expect LLMs to exhibit the representativeness heuristic, we think it depends on the LMs' potential use.** When LLMs are used to aid in decision-making, we certainly do not want them to display such biases, as this cognitive trap can lead to incorrect judgments. From another viewpoint, this heuristic is a simplification strategy that helps people make quick decisions, which can be beneficial. If our goal is for LLMs to mimic human behavior closely, we should expect them to exhibit the representativeness heuristic. In reality, however, our aim extends beyond merely replicating human-like behavior; we also hope that LLMs will surpass human performance in areas prone to error.

