# Statistical Analysis

## Normalisation

### Data features to be analyzed

Select (add) all measurements that you want to analyze.

### Data transformation

Before you start the actual analysis of your treatment effects you have to decide whether your treatment effects are multiplicative or additive to your readout.

Basically, if you are interested in statements as "my treatment changed something two-fold" you should consider using the __log2__ transformation. On the other hand, if you are interested in statements as"my treatment add twenty units to my measurement" you should not transform your data and leave "None selected"

#### Example:

Let's neglect batch effects for now and assume you have only one batch of measurements. Let's also not think about statistical significance for now (otherwise we would need too many numbers). Say your readout is the number of a certain type of vesicles in the cells. 
Your control measurement gave you: 
	
	c = 20

and you have two independent measurements for your treatment, namely
	
	m1 = 10
	m2 = 40

##### Additive interpretation: 
If you adopt an "additive interpretation" of your data you would say: In measurement one the number of vesicles was decreased by 10 (10-20=-10) and in measurement two  the number of vesicles was increased by 20  (40-20=+20).

Thus, on average the treatment led to an increase in the number of vesicles, because the mean difference of control samples and treated samples is: mean(-10, +20) = +5. In other words, the expected number of vesicles in treated samples is c + 5 = 25.

Mathematically this looks like this:

	c = 20; m1 = 10; m2 = 40	
	d1 = m1 - c = -10
	d2 = m2 - c = +20
	mean(d1,d2) = +5 
	=> the treatment increases the readout

##### Multiplicative interpretation:
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

###### Note:

Normalizing the data by dividing by the mean of the controls does __not__ give you this answer:

	c = 20; m1 = 10; m2 = 40	
	f1 = m1/c = 10/20 = 0.5
	f2 = m1/c = 40/20 = 2
	mean(f1,f2) = 1.25 
	=> the treatment increases the readout

In fact if you compute the expected number of vesicles in treated sample you get

	1.25 * c = 1.25 * 20 = 25 

This is the same answer as in the additive interpretation!

## Negative control

Select the negative control treatment. All your data will be normalised against it.
You need to have (several) negative control data points in each experimental batch (see below).

A special case is to select **"all treatments"**; in this case one assumes that only few treatments in each experiment
cause an effect, so normalising against all treatments, e.g. using the median-based **robust_z_score**, is an option (add reference).

## Batch-wise normalisation against negative control

### Batch effects

Biological data typically consist of measurement "batches".

Typically, a batch is defined by data that are "comparable"; this could be due to
- cells from same passage number are used
- antibody stainings are from same "mix"
- data was acquired in same microscopy session

In high-throughput screening such a measurement batch typically is considered to be one multi-well plate.

As above conditions often change between different batches one should (has to) normalize data to control measurements from the same batch.

Statistically this is referred to as "handling of batch effects".

The HTM Explorer gives you several outputs that handle batch effects in different ways (see below). 

### subtract_mean_ctrl

From all data points in each experimental batch (e.g., multi-well plate) you subtract the mean of all control data points in the respective batch.
Combining this with a **log2** transformation gives you the log2-fold change of the treatments with respect to the controls;
often this is a very useful readout as it has a nice biological interpretation (the statistical significance of this fold-change is computed in **[Analysis > Treatment summary]**)
                      
### z_score

Each data point is replaced by z-score against the negative controls in the same batch: 
z_score = (raw_value - mean(ctrls)) / sd(ctrls)

Pro:
- you take into account the variation (sd) of your controls; you are in some sense looking for treatments that are "significantly" different from the negative control. 
Con:
- the problem with this score is that the estimation of sd(ctrls) is very noisy if there are only a handful control measurements in each batch. A wrong estimation of sd(ctrls) can readily render all of your measurements in the same batch as "hits"" or "non-hits". 

### robust_z_score

Each data point is replaced by the robust z-score against the negative controls in the same batch: 
z_score = (raw_value - median(ctrls)) / mad(ctrls)

Pro:
- useful if **all_treatments** are selected as negative controls and you have only few treatments (outliers) that cause an effcet; the median based values will efficiently ignore the outliers and use all the other treatment values a base line. 
Con:
- the problem with this score is that the estimation of mad(ctrls) is very noisy if there are only a handful control measurements in each batch. A wrong estimation of mad(ctrls) can readily render all of your measurements in the same batch as "hits"" or "non-hits".
- if you have only a handful of negative controls, the median is not a very good representation of the true average of the negative control distribution (find reference).

### divide_by_mean_ctrl

You divide all data points in each batch by the mean of all control data points in the respective batch.
If you think this is a good idea you should consider to rather perform a **log2** transform in combination with **subtract_mean_ctrl** (see dissucion in above section batch effects).

### subtract_median_ctrl

As **subtract_mean_ctrl**, but using the median of the control measurements. This is particularly useful if you use **all_treatments** as negative control (see **robust_z_score**).

### divide_by_median_ctrl

As **divide_by_mean_ctrl**, but using the median of the control measurements. This is particularly useful if you use **all_treatments** as negative control (see **robust_z_score**).

## Options of Data Normalisation

### Batch-wise spatial gradient correction

Sometimes there are spatial gradients in your experiments; 
for instance if you perform an antibody staining in a 96-well plate experiment chances are high that the antibody staining is uneven
across the plate.

Here are different methods to normalise each data point to the data points in its local neighborhood.




## Parameters

### Data transformation

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


