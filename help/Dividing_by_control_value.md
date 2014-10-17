# Statistical Analysis

## Dividing by the controls

### Method 1

One way of normalizing the data could be to divide by control value like this:

	c = 20; m1 = 10; m2 = 40	
	f1 = m1/c = 10/20 = 0.5
	f2 = m1/c = 40/20 = 2
	mean(f1,f2) = 1.25 
	=> the treatment increases the readout

In fact, if you now compute the expected number of cells in treated sample you get

	1.25 * c = 1.25 * 20 = 25 

This is the same answer as in the additive interpretation!


### Method 2

In contrast to the previous method you could (and probably should) take the geometric mean instead of the simple average:

	c = 20; m1 = 10; m2 = 40	
	f1 = m1/c = 10/20 = 0.5
	f2 = m1/c = 40/20 = 2
	geo.mean(f1,f2) = (f1*f2)^(1/2) = 1
	=> no effect of the treatment

This is the same answer as when you did your analysis on the log scale.

### Draft...to be worked on


In fact, you can work out that working with geometric means and ratios is the same as working on the log scale...

Say V is a bunch of values then:

	geo.mean(V) = 2^(mean(log2(V)))

Example for V={v1,v2,v3}:

	2^(1/3(log2(v1)+log2(v2)+log2(v3))) = 2^( log2( (v1*v2*v3)^(1/3)) ) ) = (v1*v2*v3)^(1/3)    



