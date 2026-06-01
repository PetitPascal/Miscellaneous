#--------------------------------------------------------
## Script permettant de calculer le montant des mensualités d'un prêt (ex. : bancaire) en cas de remboursement anticipé
#--------------------------------------------------------


#--------------------------------------------------------
## Informations sur le prêt

capital_initial<-286094       # Montant initial du prêt en euros
taux_annuel<-3.10/100         # Taux annuel du prêt
duree_annees<-26              # Durée du prêt en années
mensualite_initiale<-1482.24  # Mensualité en euros
remboursement_anticipé<-50000 # Montant du remboursement anticipé en euros
mois_annee<-12                # Nombre de mois dans une année
mois_ecoules<-24              # Nombre de mois ecoules à la date de remboursement anticipe

#--------------------------------------------------------
## Création de fonctions

# Fonction pour calculer le capital restant dû après un certain nombre de mois
capital_restant_du<-function(capital_initial, taux_mensuel, mensualite_initiale, mois_ecoules) {
  capital_restant<-capital_initial
  for (mois in 1:mois_ecoules) {
    interet_mensuel<-capital_restant * taux_mensuel
    principal_mensuel<-mensualite_initiale - interet_mensuel
    capital_restant<-capital_restant - principal_mensuel
  }
  return(capital_restant)
}

# Fonction pour calculer la nouvelle mensualité avec le capital restant dû après remboursement anticipé
nouvelle_mensualite<-function(capital_restant, taux_mensuel, nombre_mois_restants) {
  mensualite<-capital_restant * (taux_mensuel * (1 + taux_mensuel)^nombre_mois_restants)/((1 + taux_mensuel)^nombre_mois_restants - 1)
  return(mensualite)
}

#--------------------------------------------------------
## Calculs des informations/variables pertinentes

# Calcul du taux d'intérêt mensuel
taux_mensuel<-taux_annuel/mois_annee

# Nombre total de mois
nombre_mois_total<-duree_annees * mois_annee

# Calcul du capital restant à la date de remboursement
capital_restant_date_rembour<-capital_restant_du(capital_initial, taux_mensuel, mensualite_initiale, mois_ecoules)

# Calcul du nouveau capital après remboursement anticipé
nouveau_capital<-capital_restant_date_rembour - remboursement_anticipé

# Nombre de mois restants après la date de remboursement
mois_restants<-nombre_mois_total - mois_ecoules

# Calcul de la nouvelle mensualité
nouvelle_mensualite_calculée<-nouvelle_mensualite(nouveau_capital, taux_mensuel, mois_restants)

#--------------------------------------------------------
## Résultats de la simulation

cat("Capital restant dû après date : ", capital_restant_date_rembour, "€\n")
cat("Capital restant dû après remboursement anticipé de ",remboursement_anticipé,"€ : ", nouveau_capital, "€\n")
cat("Nouvelle mensualité après remboursement anticipé : ", nouvelle_mensualite_calculée, "€\n")
