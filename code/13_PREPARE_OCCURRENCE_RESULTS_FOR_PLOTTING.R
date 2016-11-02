##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< PREPARE OCCURRENCE RESULTS FOR PLOTTING (RENAMING; ETC)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

wd.current <- getwd()
setwd(wd.final)

results.occ.final <- read.csv(file.occurrence.results, row.names = 1)


##+ RENAME CATEGORIES AND SELECT CATEGORY SETS FOR PLOTTING---------------------------------

#change category names to thesaurus style with angle quotes
row.names(results.occ.final) <- gsub("--","><",row.names(results.occ.final) )
row.names(results.occ.final) <- paste0("<",row.names(results.occ.final) )
row.names(results.occ.final) <- paste0(row.names(results.occ.final), ">" )


##+ THESAURUS 1 - SOCIAL SYSTEM
categories.exclude <- grep("<fed_state><ger>", rownames(results.occ.final), ignore.case = T)
results.occ.final <- results.occ.final[-categories.exclude,]

categories.select1 <- grep("<1>", rownames(results.occ.final), ignore.case = T)
results.occ.social <- results.occ.final[categories.select1,]
##~

##+ THESAURUS 2 - ENERGY SYSTEM
categories.select2 <- grep("<2>|<economy>|<industry>|<commerce>|<mobility_sector>|<local_administration_bodies>|<infrastructure>|<Food>|<Residents>", rownames(results.occ.final), ignore.case = T)
results.occ.energy <- results.occ.final[categories.select2,]

#rename categories that have been included from <1> into <2>
row.names(results.occ.energy) <- gsub("<1><SOC>","",row.names(results.occ.energy))
row.names(results.occ.energy) <- gsub("<2><ENG>","",row.names(results.occ.energy))

#exclude some categories not suitable or important for analysis
categories.exclude <- grep("<carrier>|<rental_of_houses>|<social_groups>|<general_reference><saving>|<building_parts_materials>|<Energy><unspecific_reference>|<Energy_Form><unspecific_reference>|<Economy><commerce><sector_unspecific>|<Infrastructure><disposal_unspecific>|<detailed_PPtechnology>|<alternative_wo_bike>|<tariffs_standing_orders>|<Economy><general_reference><employment_workplace>|<Infrastructure><unspecific>|<Infrastructure><supply_unspecific>|<Economy><general_reference><economic_viability>|<Economy><service><personal_services_crafting>", rownames(results.occ.energy), ignore.case = T)
results.occ.energy <- results.occ.energy[-categories.exclude,]

##+  THESAURUS 3 - SUSTAINABILITY
categories.select3 <- grep("<3>|biofuels|renewable|pedestrian|insulation|car_sharing|public_transport|storage", rownames(results.occ.final), ignore.case = T)
results.occ.sustainability <- results.occ.final[categories.select3,]
##~

##~-----------------------------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "PREPARE_OCCURRENCE_RESULTS_FOR_PLOTTING"
setwd(wd.current)

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< PREPARE OCCURRENCE RESULTS FOR PLOTTING (RENAMING; ETC)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
