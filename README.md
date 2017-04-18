# k-anonymity

This program anonymizes database so that we cannot narrow down candidates to k or fewer.

Anonymization executed in this program do not follow regular definition of k-anonymity because of speeding up.

Real k-anonymity equalizes dataset to make clusters composed of k or more rows having same contents in dataset.
But this program make clusters composed of  just only k rows. 


## Installation and Build

preparation for use this program.

### 1.Install Stack

This project uses build tool 'Stack'.

Installation for OSX
```
brew install haskell-stack
```
Installation for Linux
```
url -sSL https://get.haskellstack.org/ | sh
```


### 2.Build
After installing Stack| please install packages for this program with following command.
```
stack build
```

## Usage

### 1.Edit config.yaml
Please write settings to satisfy your requirement.

* solution: "optimal" or "suboptimal"
    * optimal: Usefulness of anonymized data is maximum| but result is returned late.
    * suboptimal: Usefulness of anonymized data is not maximum| but result is returned fast.   
* k_size: Int 
    * number of rows that you want to equalize
* assoc_id: Int
    * number of columns that you want to anonymize
* filepath: String
    * path of raw data file 
    
### 2. Execute

This program is executed by following command.

```
stack exec -- k-anonymity +RTS -Nn
```

`stack exec -- k-anonymity` execute k-anonymity itself.

`-- +RTS -Nn` is options to parallel execution.

'n' should be replaced any Int number.
n is a number of threads that you allow this program to use.
 
example:
```
stack exec -- k-anonymity -- +RTS -N2
```

## Example

### Input

Raw data should be saved in form of CSV.
Sample is like this.

```
47677,25,Amnesia
47611,38,Insomnia
47525,27,Leukemia
47676,31,Leukemia
47582,32,Amnesia
47523,39,Amnesia
47581,24,Insomnia
47618,26,Leukemia
```

That is,

|post number|age|disease|
|:--:|:--:|:--:|
|47677|25|Amnesia|
|47611|38|Insomnia|
|47525|27|Leukemia|
|47676|31|Leukemia|
|47582|32|Amnesia|
|47523|39|Amnesia|
|47581|24|Insomnia|
|47618|26|Leukemia|

That data anonymize with this setting.
```
solution: "optimal"	    
k_size: 2	  		        
assoc_id: 2			        
filepath: "data/sample"	    
```




 
### Output
This program print result as CSV form like below in stdout.
```
476**,2*,Leukemia
476**,2*,Amnesia
476**,3*,Insomnia
476**,3*,Leukemia
475**,2*,Leukemia
475**,2*,Insomnia
475**,3*,Amnesia
475**,3*,Amnesia
```

That is,

|post number|age|disease|
|:--:|:--:|:--:|
|476**|2*|Leukemia|
|476**|2*|Amnesia|
|476**|3*|Insomnia|
|476**|3*|Leukemia|
|475**|2*|Leukemia|
|475**|2*|Insomnia|
|475**|3*|Amnesia|
|475**|3*|Amnesia|

