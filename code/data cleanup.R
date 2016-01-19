rm(list=ls())

# Libraries
library(taxize)
library(XML)
library(tools)

# Load Data
paper <- read.csv("C:/Users/klevan/Documents/GitHub/beetleBot/data/Larochelle & Lariviere 2001.txt", stringsAsFactors=FALSE)

#Functions
getLinks = function() { 
  links = character()
  names = character()
  list(a = function(node, ...) { 
    links <<- c(links, xmlGetAttr(node, "href"))
    node 
  }, 
  links = function()links)
}
h1 = getLinks()

# Format scientific names
colnames(paper)[1] <- "scientificName"
for (i in 1:dim(paper)[1]){
  paper$genus[i] <- strsplit(paper$scientificName[i],split = " ")[[1]][1]
  paper$species[i] <- strsplit(paper$scientificName[i],split = " ")[[1]][2]
  paper$subspecies[i] <- strsplit(paper$scientificName[i],split = " ")[[1]][3]
  if(substr(paper$subspecies[i],1,1)==toupper(substr(paper$subspecies[i],1,1))){
    paper$subspecies[i] <- NA
  }
  if(is.na(paper$subspecies[i])==FALSE){
    paper$scientificName[i] <- paste(paper$genus[i],paper$species[i],paper$subspecies[i],sep = " ")  
  } else {
    paper$scientificName[i] <- paste(paper$genus[i],paper$species[i],sep = " ")  
  }
  tmp <- try(paper$valid[i] <- getitistermsfromscientificname(paper$scientificName[i])$nameusage)
  if(class(tmp)=="try-error"){
    paper$valid[i] <- "untested"
  }
}
paper <- paper[c("scientificName","Ecology","Biology","Dispersal","valid")]

for(i in 1:dim(paper)[1]){
  # Verify names from itis.gov
  if(paper$valid[i]=="invalid"){
    paper$scientificName[i] <- getacceptednamesfromtsn(getitistermsfromscientificname(paper$scientificName[i])$tsn)$acceptedName 
    paper$valid[i] <- getitistermsfromscientificname(paper$scientificName[i])$nameusage[1] 
  }
}

# Collate facts about beetles
for (i in 1:dim(paper)[1]){
  paper$Ecology[i] <- substr(paper$Ecology[i],10,nchar(paper$Ecology[i]))
  paper$Biology[i] <- substr(paper$Biology[i],10,nchar(paper$Biology[i]))
  paper$Dispersal[i] <- substr(paper$Dispersal[i],18,nchar(paper$Dispersal[i]))
  paper$conservationStatus[i] <- ""
  if(length(strsplit(tolower(paper$Dispersal[i]),split = " conservation status. ")[[1]])>1){
    paper$conservationStatus[i] <- strsplit(tolower(paper$Dispersal[i]),split = " conservation status. ")[[1]][2]
    paper$Dispersal[i] <- strsplit(tolower(paper$Dispersal[i]),split = " conservation status. ")[[1]][1]
  }
  paper$wings[i] <- strsplit(paper$Dispersal[i],split = "\\. ")[[1]][1]
  paper$Dispersal[i] <- substr(paper$Dispersal[i],nchar(paper$wings[i])+3,nchar(paper$Dispersal[i]))
}
# Found in the day or the night
paper$dayOrNight[grepl("iurnal",paper$Ecology)] <- "Diurnal"
paper$dayOrNight[grepl("octurnal",paper$Ecology)] <- "Nocturnal"
paper$dayOrNight[grepl("repuscular",paper$Ecology)] <- "Crepuscular"
paper$dayOrNight[is.na(paper$dayOrNight)==TRUE] <- "Unknown"
paper$predatory[grepl("redaceous",paper$Biology)] <- "Predaceous"

paper$wings <- toTitleCase(paper$wings)
paper$conservationStatus <- toTitleCase(paper$conservationStatus)


paper$predators <- ""; paper$prey <- ""
paper$prey[grepl("Adult food, in the field: ",paper$Biology)] <- 'stuff'
for (i in 1:dim(paper)[1]){
  if(length(strsplit(tolower(paper$Biology[27]),split = " predators: ")[[1]])>1){
    paper$predators[i] <- substr(strsplit(strsplit(tolower(paper$Biology[i]),split = " predator")[[1]][2],
                                   split = "\\. ")[[1]][1],
                                 4,
                                 nchar(strsplit(strsplit(tolower(paper$Biology[i]),split = " predator")[[1]][2],
                                                split = "\\. ")[[1]][1]))
  }
  if(paper$prey[i]=="stuff"){
    paper$prey[i] <- strsplit(strsplit(tolower(paper$Biology[i]),split = "adult food, in the field: ")[[1]][2],
                              split = "\\. ")[[1]][1]
  }
}
paper$prey[grepl('\\.',paper$prey)] <- substr(paper$prey[grepl('\\.',paper$prey)],1,nchar(paper$prey[grepl('\\.',paper$prey)])-1) 

paper$predators[grepl("various birds.",paper$predators)] <- "birds"
paper$predators[grepl("undetermined bird species",paper$predators)] <- "birds"
paper$predators[grepl("ants ",paper$predators)] <- "ants"
paper$predators[grepl('\\.',paper$predators)] <- substr(paper$predators[grepl('\\.',paper$predators)],1,nchar(paper$predators[grepl('\\.',paper$predators)])-1)

for (i in 1:dim(paper)[1]){
  if(is.na(paper$predators[i])==FALSE){
    if(length(strsplit(paper$predators[i],split=", and ")[[1]])>1){
      paper$predators[i] <- paste(strsplit(paper$predators[i],split=", and ")[[1]][1],
                                  strsplit(paper$predators[i],split=", and ")[[1]][2],
                                  sep=", ")
    }
    if(length(strsplit(paper$predators[i],split=" and ")[[1]])>1){
      paper$predators[i] <- paste(strsplit(paper$predators[i],split=" and ")[[1]][1],
                                  strsplit(paper$predators[i],split=" and ")[[1]][2],
                                  sep=", ")
    }
  }
  if(is.na(paper$prey[i])==FALSE){
    if(length(strsplit(paper$prey[i],split=", and ")[[1]])>1){
      paper$prey[i] <- paste(strsplit(paper$prey[i],split=", and ")[[1]][1],
                                  strsplit(paper$prey[i],split=", and ")[[1]][2],
                                  sep=", ")
    }
    if(length(strsplit(paper$prey[i],split=" and ")[[1]])>1){
      paper$prey[i] <- paste(strsplit(paper$prey[i],split=" and ")[[1]][1],
                                  strsplit(paper$prey[i],split=" and ")[[1]][2],
                                  sep=", ")
    }
  }
}
paper <- paper[c("scientificName","Ecology","Biology","Dispersal","conservationStatus","wings","dayOrNight",
               "predatory","predators","prey")]

# Is there a photo of that beetle?
paper$beetle_photo <- ""
for (i in 1:dim(paper)[1]){
  photo_locale <- list.files("C:/Users/klevan/Documents/GitHub/beetleBot/images")
  a <- ""
  sub <- ""
  if(length(strsplit(paper$scientificName[i],split = " ")[[1]])==3){
     a <- paste(tolower(strsplit(paper$scientificName[i],split = " ")[[1]][1]),
                       strsplit(paper$scientificName[i],split = " ")[[1]][2],
                       strsplit(paper$scientificName[i],split = " ")[[1]][3],
                       sep="_")
     sub <- "YES"
  }
  if(length(strsplit(paper$scientificName[i],split = " ")[[1]])==2){
    a <- paste(tolower(strsplit(paper$scientificName[i],split = " ")[[1]][1]),
                      strsplit(paper$scientificName[i],split = " ")[[1]][2],
                      sep="_")
  }
  if(sub=="YES" & length(photo_locale[grepl(a,photo_locale)])==0){
    a <- paste(tolower(strsplit(paper$scientificName[i],split = " ")[[1]][1]),
               strsplit(paper$scientificName[i],split = " ")[[1]][2],
               sep="_")
    if(length(photo_locale[grepl(a,photo_locale)])==1){
      a <- sample(photo_locale[grepl(a,photo_locale)],1)  
    }
    
  }
  if(length(photo_locale[grepl(a,photo_locale)])==1){
    paper$beetle_photo[grepl(paper$scientificName[i],paper$scientificName)] <- sample(photo_locale[grepl(a,photo_locale)],1)
  }
}
# Beetle Photo Sources
denverlist <- c("cicindela_amargosae_nyensis","cicindela_columbica","cicindela_hemorrhagica_arizonae","cicindela_hemorrhagica_hemorrhagica",
                "cicindela_hirticollis_shelfordi","cicindela_hornii_hornii","cicindela_obsoleta_santaclarae","cicindela_pugetana","cicindela_willistoni_psuedosenilis",
                "cicindela_willistoni_willistoni","ellipsoptera_hamata_monti","ellipsoptera_macra_macra","habroscelimorpha_californica_mojavi",
                "habroscelimorpha_californica_pseudoerronea","habroscelimorpha_circumpicta_pembina","habroscelimorpha_dorsalis_saulcyi",
                "habroscelimorpha_dorsalis_venusta","habroscelimorpha_fulgoris_erronea","habroscelimorpha_gabbii")

erwin <- c("amblycheila_baroni","amblycheila_cylindriformis","amblycheila_picolominii","amblycheila_schwarzi","omus_audouini_1",
           "omus_californicus_angustocylindricus","omus_californicus_californicus","omus_californicus_intermedius","omus_cazieri","omus_dejeanii","tetracha_angustata",
           "tetracha_carolina_carolina","tetracha_floridana","tetracha_virginica")

BitB <- c("cicindela_albissima","cicindela_arenicola","cicindela_denverensis","cicindela_formosa_gibsoni",
          "cicindela_formosa_formosa","cylindera_cursitans","cicindela_lengi_lengi","cicindela_willistoni_estancia")

goulet <- c("cicindela_ancocisconensis","cicindela_denikei","cicindela_duodecimguttata",
            "cicindela_hirticollis_rhodensis","cicindela_limbalis","cicindela_limbata_labradorensis",
            "cicindela_marginipennis","ellipsoptera_lepida","ellipsoptera_marginata","ellipsoptera_puritana",
            "omus_audouini")

kippenhan <- c("cicindela_decemnotata","cylindera_lunalonga")

knisley <- c("cicindela_hirticollis_corpuscula","cicindela_hirticollis_gravida","cicindela_hirticollis_siuslawensis",
             "cicindela_patruela_consentanea","cicindela_pimeriana","cicindela_politula_barbaraannae",
             "cicindela_politula_viridimonticola","cicindela_rufiventris_heutzii",
             "cicindela_theatina","cicindela_tranquebarica_viridissima","cicindela_willistoni_sulfontis",
             "ellipsoptera_nevadica_citata","ellipsoptera_nevadica_olmosa","eunota_togata_fascinans",
             "microthylax_olivaceus")

catling <- c("cicindela_limbata_hyperborea","cicindela_limbata_nympha","cicindela_longilabris_longilabris",
             "cicindela_oregona_guttifera","cicindela_tranquebarica_kirbyi","cylindera_terricola_terricola")

anichtchenko <- c("cicindela_scabrosa")

onh <- c("cicindela_bellissima_bellissima","cicindela_bellissima_frechini")

ubc <- c("cicindela_hirticollis_couleensis")

bold <- c("cicindela_fulgida_pseudowillistoni","cicindela_fulgida_westbournei","ellipsoptera_rubicunda")

alexWild <- c("cicindela_lemniscata_lemniscata")

mentionlists <- list(denverlist,erwin,BitB,goulet,kippenhan,
                     knisley,catling,anichtchenko,onh,ubc,bold,
                     alexWild)
authorlist <- c("@DenverMuseumNS", #DMNS
                "#TerryErwin", #Professor
                "@tcmacrae", #Ted C. Macrae of BitB
                "#HenriGoulet", #henri goulet photos from carabidae.org
                "#MKippenhan",
                "DOI:10.1163/18749836-07021077", #Knisley et al.
                "#PMCatling", #catling 2006
                "#AAnichtchenko",
                "http://onh.eugraph.com",
                "@UBC",
                "#BOLDsystems",
                "@Myrmecos")
paper$mentions <- ""

for (i in mentionlists){
  for (j in i){
    paper$mentions[grepl(j,paper$beetle_photo)] <- authorlist[grepl(i[1],mentionlists)]
  }
}

  
write.csv(paper,"C:/Users/klevan/Documents/GitHub/beetleBot/data/lifehist.csv",row.names = FALSE)


