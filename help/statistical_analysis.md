# Statistical Analysis

## Batch effects

Biological data typically consist of measurement "batches".

Typically, a batch is defined by data that are "comparable"; this could be due to
- cells from same passage number are used
- antibody stainings are from same "mix"
- data was acquired in same microscopy session

In high-throughput screening such a measurement batch typically is considered to be one multi-well plate.

As above conditions have a high probability to change between different batches one should (has to) normalize data to control measurements from the same batch.

Statistically this is referred to as "handling of batch effects".

The HTM Explorer gives you several outputs that handle batch effects in different ways (see below). 


## Multiplicative or additive effect of treatments

Before you start the actual analysis of your treatment effects you have to decide whether your treatment effects are multiplicative or additive to your readout.

### Example:

Let's neglect batch effects for now and assume you have only one batch of measurements. Let's also not think about statistical significance for now (otherwise we would need too many numbers). Say your readout is the number of a certain type of vesicles in the cells. 
Your control measurement gave you: 
	
	c = 20

and you have two independent measurements for your treatment, namely
	
	m1 = 10
	m2 = 40

#### Additive interpretation: 
If you adopt an "additive interpretation" of your data you would say: In measurement one the number of vesicles was decreased by 10 (10-20=-10) and in measurement two  the number of vesicles was increased by 20  (40-20=+20).

Thus, on average the treatment led to an increase in the number of vesicles, because the mean difference of control samples and treated samples is: mean(-10, +20) = +5. In other words, the expected number of vesicles in treated samples is c + 5 = 25.

Mathematically this looks like this:

	c = 20; m1 = 10; m2 = 40	
	d1 = m1 - c = -10
	d2 = m2 - c = +20
	mean(d1,d2) = +5 
	=> the treatment increases the readout



#### Multiplicative interpretation:
If you adopt an "multiplicative interpretation" of your data you would say: 
In measurement one the number of vesicles decreased by a factor of 2 (10/20=1/2) and in measurement two the number of vesicles increased by factor of 2 from (40/20=2). As you got the exact opposite effect in your two measurements you would say that on average there was no effect of the treatment.

In order to get this result mathematically one needs to take the logarithm (e.g. the log2) of the data before computing the differences:

	c = 20; m1 = 10; m2 = 40	
	log2(c) = log2(20) = 4.321928
	log2(m1) = log2(10) = 3.321928
	log2(m2) = log2(40) = 5.321928
	d1 = log(m1) - log(c) = 3.321928 - 4.321928 = -1
	d2 = log(m2) - log(c) = 5.321928 - 4.321928 = +1
	mean(d1,d2) = 0 
	=> no effect of the treatment

One can also compute a "factor change" from the final result by taking it to the power of 2:
	
	2^(mean(d1,d2)) = 2^(0) = 1

In words: on average the treatment changes the number of vesicles by a factor of 1, i.e. there is no change.

##### Note:

Normalizing the data by dividing by the mean of the controls does __not__ give you this answer:

	c = 20; m1 = 10; m2 = 40	
	f1 = m1/c = 10/20 = 0.5
	f2 = m1/c = 40/20 = 2
	mean(f1,f2) = 1.25 
	=> the treatment increases the readout

In fact if you compute the expected number of vesicles in treated sample you get

	1.25 * c = 1.25 * 20 = 25 

This is the same answer as in the additive interpretation!


## Parameters

### Data transformation


## `Median__z_score` (position based score)

For each batch (plate), the data of all images within one position are averaged (see above) such that we have one number per position. 
Then a z-score is computed for each position as Z = (value - mean(ctrls)) / sd(ctrls)
Where the mean and sd are computed across all positions that contain the selected control measurements.

To take into account the fact that you measured multiple batches the median z-score of all the batches is computed and given in the treatmentSummary table. The logic of taking the median is: "at least in half of the batches (replicates) there should have been a significant effect."; significant typically being Z>2 (increase) or Z<-2 (decrease).

Issues:
- the problem with this score is that the estimation of sd(ctrls) is very noisy if there are not enough control measurements. A wrong estimation of sd(ctrls) can readily render all of your measurements in the same batch as hits or non-hits. 
- does not give you a statistical significance; for instance the value does not reflect how often you repeated your measurement
 
## `Median__robust_z_score` (position based score)

Same as Median__z_score, but for each batch (plate) a z-score is computed as Z =  (treated - median(ctrls)) / mad(ctrls), where mad is the so called median average deviation (a median based analog to the standard deviation).

This score can make sense if you want to remove outliers in your controls, e.g. you could take the whole plate (all treatments) as controls if you expect only a limited number of hits (reference: Butros and Huber).

Issues:
- same as `Median__z_score`


## `t_test__positions` (position based score)

In a first step the batch effect is dealt with by subtracting the mean of the control positions within each batch. 
Afterwards, for one specific treatment all batches are identified that contained this treatment and all treatment and control measurements from these batches are pooled; a t-test is performed of the treated positions against the control positions.
Note: the t-test will compute a common variance of the batch-corrected controls across all batches that contained the treatment under consideration. This assumes that the variance of the controls is similar from batch to batch. The HTM Explorer will provide you with a graph that allows you to visually check this assummption.

Issues:
- assumes similar (within factor of 2-3) variance of controls across batches.

### output

`t_test__positions__p_value`


`t_test__positions__estimate`

The estimate is the difference of treated positions and control positions (after batch correction).
If you chose the log2 data transformation this difference gives the fold-change of treatment vs. control (in log2 scale). To compute the actual fold change you can use this formula: 2^estimate





## `t_test__images` (images based score)

As `t_test__positions` but the images (sub-positions) are used for the computations.


Notes:

- For multi-well data we often observe that - for the same treatment - the images tend to give quite similar results within one well while from well to well there can be strong variations; this indicates that the variation comes from the wells ("positions") and one should not pool all the images from different positions together (as it is done in this image based score), but rather use the positions based score..
(think more about this; make an example)


## Statistical significance code

p_value<0.001 "***"
p_value<0.01 "**"

                                                      ifelse(t_test__images__p_value<0.05,"*",
                                                              ifelse(t_test__images__p_value<0.1,"."," "
                                                              ))))


