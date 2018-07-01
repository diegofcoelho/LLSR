# Data Analysis of Liquid-liquid systems using R


The idea started after I decided to start coding my apps for handling experimental data but with no use of commercial software.

For academic reasons, I had only a weekend to develop the concept, code and test a small algorithm to fit data from experiments to determine binodal curves for Aqueous Two-Phase Systems (ATPS). After a brief research I found an article describing the use of an empyrical equation to fit data and estimate tieline's composition.


* Merchuk[1]:	> Y = P1*exp(P2*(X^(0.5))-P3*(X^3)) 		
* Murugesan[2]:	> Y = A+B*(XC)^0.5+C*XC

I chose *R* for obvious reasons: It is undoubtly the best open source statistical plataform available and its library have thousands of packages which functionalities could support my script and allow me to design specific-use functions.

At that time I coded a whole script that would read a worksheet, fit the data and create a report in which it would insert plots and tables with the statistical data obtained through the script.

I could share the script and let the users tied to the way I wrote it but I thought it would give bigger functionality if I wrap it into a R package and allow each researcher develop their own methodology to handle their data.

### LLSR 0.0.1.9000 (dev1)

That is what *Data Analysis of Liquid-liquid systems using R* is (formally only LLSR).

This beta version only brings a few methods to introduce the package for those who want such tool but my plan is to expand it continuously. LLSR's documentation describe the methods and give examples to test. Besides, I am including a small data.frame to allow you give a go with the package.

## Available methods in version 0.0.1.9000

* Merchuk's method
* Murugesan's method
* Package Methods
* mrchk.default( )
* mrgsn.default( )
* gsnchk()
* mrchk.plot( )
* mrgsn.plot( )
* peg4kslt - dataset
* mrchk.tielines( )
* mrgsn.othmer( )
* jnCs( )
* mrchk.crpt( )
* mrchk.bancroft( )


## References

1. Merchuk, J.C., B.A. Andrews, and J.A. Asenjo, Aqueous two-phase systems for protein separation: Studies on phase inversion. Journal of Chromatography B: Biomedical Sciences and Applications, 1998. 711(1–2): p. 285-293.

2. Murugesan, T. and M. Perumalsamy, Liquid−Liquid Equilibria of Poly(ethylene glycol) 2000 + Sodium Citrate + Water at (25, 30, 35, 40, and 45) °C. Journal of Chemical & Engineering Data, 2005. 50(4): p. 1392-1395.