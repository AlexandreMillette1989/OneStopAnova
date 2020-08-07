# SCI1018 One-Way Anova | Two-Way Anova

**OneStopAnova** est un package réalisé dans le cadre du cours **SCI1018 - Statistiques avec R** de la TÉLUQ pour les cours traitant d'Anova. 

L'objectif de ce package est de réduire substantiellement le nombre d'étapes de codification pour les futur(e)s étudiant(e)s.

En date d'août 2020, le package offre deux fonctions, soit: **OneStopAnova()** et **OneStopAnova_IC()**. 

# Brève description des opérations effectuées par le package

**OneStopAnova():**

  - Vérification des suppositions d'homoscédasticité et de normalité des résidus:
  
    - Supposition d'homoscédasticité:
    
        - Test de Levene pour l'homogénéité de la variance avec une explication textuelle vous signifiant si les conditions sont respectées.
        
        - Graphique résidus vs valeurs prédites.
        
    - Supposition de normalité des résidus:
    
        - Test de normalité d'Anderson-Darling avec une explication textuelle vous signifiant si les conditions sont respectées. 
        
        - Graphique quantile-quantile.  
        
  - Création d'un tableau sommaire des résultats du test Anova.
  
  - (OPTIONNEL) Création d'un tableau sommaire pour le test de Tukey.
  
  - (OPTIONNEL) Transformation logarithmique de la variable quantitative.
  
  
**OneStopAnova_IC():**

  - Fonction permettant d'obtenir directement l'intervalle de confiance.

# Installation du package OneStopAnova

Pour installer ce packages, vous devrez d'abord installer le package devtools:
```
install.packages("devtools")
  
library(devtools)
```

Par la suite, vous serez en mesure d'importer des packages à partir de GitHub:
```
install_github("OneStopAnova")
  
library(OneStopAnova)
```
  
Par défaut, le package devrait vous offrir l'option de télécharger ou de mettre à jour les dépendences nécessaires à son fonctionnement. Vous pouvez ignorer les mises à jour en sélectionnant l'option 3 (None).

**Si vous rencontrez une erreur lors de l'installation ou de la mise à jour d'une des dépendences, vous pouvez effectuer le code suivant:**
```
packages = c("knitr", "dplyr", "car", "nortest")
             
package.check <- lapply(
  packages,
  FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
  install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
  }
 }
)
```
# Exemple 7.1 (Module 7) pages 7 à 24
```
calcium = read.table(file.choose(), header = TRUE)
```
**Two-Way Anova | Log | Tukey**

**OneStopAnova()**
```
OneStopAnova(Quantitative = calcium$Concentration,
             Qualitative = calcium$Trait,
             Qualitative2 = calcium$Sexe,
             var_names = c(Quantitative = "Concentration",
                           Qualitative = "Trait",
                           Qualitative2 = "Sexe"),
             Log = TRUE,
             Tukey = TRUE)
```
**Cliquez sur les images pour agrandir**

Le *Graphique 1* présente un exemple fictif de vérification des suppositions. 

Lorsque les tests présentent des résultats P <= 0.05, un message contenant la mention **Warning** apparaît pour signaler un problème au niveau des suppositions.
Lorsque les test présentent des résultats P > 0.05, un message sans avertissement apparaît pour signaler que vous respectez les suppositions.

**Graphique 1**
![Vérification des suppositions](Images/OneStopAnova_1.jpg)

Le *Graphique 2* présente un sommaire du test Anova (Log) ainsi que les résultats du test de Tukey.

**Graphique 2**
![Sommaire Anova et Tukey](Images/OneStopAnova_2.jpg)

**OneStopAnova_IC()**
```
calcium$log.concentration = log(calcium$Concentration)
aov.log = aov(log.concentration ~ Trait + Sexe + Trait:Sexe, data = calcium)
OneStopAnova_IC(aov = aov.log, p = 0.025)
```
**Cliquez sur l'image pour agrandir**



![Intervalle de confiance](Images/OneStopAnova_3.jpg)

**Pour obtenir de l'aide quant aux arguments de la fonction, vous pouvez faire:**
```
?OneStopAnova

?OneStopAnova_IC
```
