---
title: "lecture12"
output: 
  html_document: 
    keep_md: yes
---
title: "lecture11"
output: 
  html_document: 
    keep_md: yes
---




```r
#import 1hsg pdb file
library(bio3d)
```

```
## Warning: package 'bio3d' was built under R version 3.4.4
```

```r
file.name <- get.pdb("1hsg")
```

```
## Warning in get.pdb("1hsg"): ./1hsg.pdb exists. Skipping download
```

```r
hiv <- read.pdb(file.name)
```


```r
#get a quick summary of the pdb structure
hiv
```

```
## 
##  Call:  read.pdb(file = file.name)
## 
##    Total Models#: 1
##      Total Atoms#: 1686,  XYZs#: 5058  Chains#: 2  (values: A B)
## 
##      Protein Atoms#: 1514  (residues/Calpha atoms#: 198)
##      Nucleic acid Atoms#: 0  (residues/phosphate atoms#: 0)
## 
##      Non-protein/nucleic Atoms#: 172  (residues: 128)
##      Non-protein/nucleic resid values: [ HOH (127), MK1 (1) ]
## 
##    Protein sequence:
##       PQITLWQRPLVTIKIGGQLKEALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYD
##       QILIEICGHKAIGTVLVGPTPVNIIGRNLLTQIGCTLNFPQITLWQRPLVTIKIGGQLKE
##       ALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYDQILIEICGHKAIGTVLVGPTP
##       VNIIGRNLLTQIGCTLNF
## 
## + attr: atom, xyz, seqres, helix, sheet,
##         calpha, remark, call
```

Before proceeding with docking, we have to first extract just the protein atoms

```r
prot <- trim.pdb(hiv, "protein") #get protein portion
lig <- trim.pdb(hiv, "ligand") #get ligand portion

write.pdb(prot, file="1hsg_protein.pdb") #write protein portion into pdb file
write.pdb(lig, "1hsg_ligand.pdb") #write ligand portion into pdb file
```


## Use AutoDockTools to set up protein docking input
Docking algorithms require atoms to have charge and atom type info, which PDB structures don't have

We have to prep the protein and ligand files to include these info

Use AutoDock Tools (ADT) to do so
Note: window's computer did not download ADT successfully
Refer to worksheet for instructions for using ADT

Visualize the docks and compare to crystal conformation of the ligand

```r
#use bio3d
library(bio3d)

#all.pdbqt file contains the docked modes
res <- read.pdb("all.pdbqt", multi=TRUE)
write.pdb(res, "results.pdb")
```
Now we can load both original 1hsg.pdb and results.pdb file into VMD for visualization

To assess the results quantitatively, calculate RMSD (root mean square distance) between each of the docking results and known crystal structure

```r
res <- read.pdb("all.pdbqt", multi=T)
ori <- read.pdb("ligand.pdbqt")
rmsd(ori, res)
```

```
##  [1]  0.590 11.163 10.531  4.364 11.040  3.682  5.741  3.864  5.442 10.920
## [11]  4.318  6.249 11.084  8.929
```








































