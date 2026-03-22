Algorithme MCMC pour BSTS : Détail des Étapes
1. Initialisation

Paramètres : On initialise les variances des innovations (σμ2,στ2,σs2,σϵ2\sigma^2_\mu, \sigma^2_\tau, \sigma^2_s, \sigma^2_\epsilonσμ2​,στ2​,σs2​,σϵ2​) à des valeurs plausibles (ex. 1 ou estimées par un modèle simple).
États latents : On initialise les composantes (μt,τt,st\mu_t, \tau_t, s_tμt​,τt​,st​) à zéro ou via une décomposition classique (ex. STL).
Coefficients : Les β\betaβ sont initialisés à zéro ou via une régression linéaire classique.

2. Itération MCMC
À chaque itération, on met à jour successivement :
  a. Mise à jour des états latents (composantes)

Méthode : On utilise un filtre de Kalman avant/arrière (forward filtering/backward smoothing) pour échantillonner les états latents conditionnellement aux données et aux autres paramètres.
Pourquoi ? : Le BSTS peut être vu comme un modèle d’espace d’état, où chaque composante est un processus latent. Le filtre de Kalman permet d’échantillonner efficacement ces états.
b. Mise à jour des variances des innovations

Prior : On suppose généralement un prior inverse-gamma pour chaque σi2\sigma^2_iσi2​ :
  σi2∼Inverse-Gamma(α,β)  \sigma^2_i \sim \text{Inverse-Gamma}(\alpha, \beta)σi2​∼Inverse-Gamma(α,β)

Postérieur : La distribution conditionnelle de σi2\sigma^2_iσi2​ est aussi une inverse-gamma, mise à jour avec les innovations estimées.
c. Mise à jour des coefficients des régresseurs (β\betaβ)

Prior : Souvent un prior normal ou de Laplace (pour la parcimonie) :
  βj∼Normal(0,σβ2)  \beta_j \sim \text{Normal}(0, \sigma^2_\beta)βj​∼Normal(0,σβ2​)

Postérieur : On échantillonne β\betaβ via un pas de Gibbs (si le prior est conjugué) ou un Metropolis-Hastings (sinon), en utilisant la vraisemblance conditionnelle aux autres paramètres.

3. Vérification de la Convergence

Diagnostics :
  
  Trace plots : Visualiser l’évolution des chaînes pour détecter des tendances ou des sauts.
Statistique de Gelman-Rubin (R^\hat{R}R^) : Comparer plusieurs chaînes ; R^≈1\hat{R} \approx 1R^≈1 indique la convergence.
Autocorrélation : Vérifier que les échantillons ne sont pas trop corrélés.

Burn-in : On élimine les premières itérations (ex. 50%) pour éviter l’effet des valeurs initiales.

4. Résultats

Estimation : Les paramètres sont estimés par la moyenne ou la médiane des échantillons post-burn-in.
Incertitude : Les intervalles de crédibilité (ex. 95%) sont calculés à partir des quantiles des échantillons.

Exemple Pratique (R avec bsts)
Voici un extrait de code R illustrant l’estimation :
  r
Copier

library(bsts)

# Simulation de données
set.seed(1)
n <- 100
x <- rnorm(n)
y <- 1 + 0.5 * x + rnorm(n, sd = 0.5)

# Spécification du modèle
ss <- AddLocalLinearTrend(list(), y)
ss <- AddRegression(ss, x)

# Estimation MCMC
model <- bsts(y, ss, niter = 1000, ping = 0, seed = 1)

# Résumé des paramètres
print(model)


Sortie : Le modèle affiche les estimations des composantes, des coefficients, et des variances, avec leurs intervalles de crédibilité.

Points Clés à Retenir

Efficacité : Le filtre de Kalman rend l’échantillonnage des états latents très efficace, même pour de longues séries.
Flexibilité : On peut ajouter/supprimer des composantes ou des régresseurs sans changer fondamentalement l’algorithme.
Diagnostics : Toujours vérifier la convergence et l’autocorrélation des chaînes.

As-tu une série temporelle spécifique à modéliser, ou veux-tu des précisions sur l’implémentation en Python/Stan ?
  
  
  