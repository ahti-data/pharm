#' AHTI Branding Metadata
#' Bron: Kleurgebruik Stijlgids

ahti_branding <- list(
  
  # Hoofdkleuren
  colors = list(
    fris_rood     = "#EE3124", # Gebruikt voor logo, volvlakken, CTA's en links[cite: 1]
    helder_blauw  = "#009DDC", # Gebruikt voor logo, volvlakken, CTA's en links[cite: 1]
    grijs_blauw   = "#336A88", # Gebruikt voor logo, volvlakken, CTA's en links[cite: 1]
    
    # Tintvarianten (zoals vermeld in stijlgids)[cite: 1]
    rood_dark_25  = "#82241A", # +25% black[cite: 1]
    rood_dark_75  = "#380C09", # +75% black[cite: 1]
    blauw_light_75 = "#CCDAEI", # +75% white[cite: 1]
    blauw_dark_25  = "#0075A4", # +25% black[cite: 1]
    blauw_dark_75  = "#002737", # +75% black[cite: 1]
    grijs_blauw_dark_25 = "#264F65", # +25% black[cite: 1]
    grijs_blauw_dark_75 = "#0C1A22", # +75% black[cite: 1]
    
    # Steunkleuren[cite: 1]
    fris_groen    = "#00A55D", # C100 M0 Y44 K35[cite: 1]
    diep_paars    = "#20153E", # C95 M95 Y40 K50[cite: 1]
    
    # Grijstinten[cite: 1]
    donker_grijs  = "#272727", # K85[cite: 1]
    midden_grijs  = "#524F50", # K80[cite: 1]
    licht_grijs   = "#F4F4F4"  # C3 M2 Y2 K0[cite: 1]
  ),
  
  # Aanbevolen Kleurverdeling[cite: 1]
  # Gebaseerd op de 50/20/10/10/10 verdeling in de gids[cite: 1]
  distribution = c(
    primary   = 0.50,
    secondary = 0.20,
    accent_1  = 0.10,
    accent_2  = 0.10,
    accent_3  = 0.10
  ),
  
  # Metadata voor plotting (bijv. ggplot2)
  scale_discrete = c("#EE3124", "#009DDC", "#336A88", "#00A55D", "#20153E")
)

# Tip: Gebruik 'ahti_branding$colors$fris_rood' in je Shiny UI of ggplot calls.