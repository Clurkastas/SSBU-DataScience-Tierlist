#generate vector with fighter names

# Writing Down Original Fighters Names:
#sortierung teilweise wegen zuerst löschung:
#"bsp: Dr. Mario muss vor Mario ausglesen werden"

fighter <- c("Dr. Mario","Mario","Luigi","Peach",
             "Rosalina & Luma", 
             "Bowser Jr.", "Bowser", "Yoshi",
             "Donkey Kong","Diddy Kong","Zelda","Ganondorf",
             "Sheik","Young Link", "Toon Link", "Link",
             "Dark Samus", "Zero Suit Samus", "Samus", "Kirby","Meta Knight",
             "King Dedede", "Fox", 
             "Captain Falcon", 
             "Falco", "Wolf", 
             "Pikachu", "Jigglypuff","Pichu","Mewtwo",
             "Pokémon Trainer",
             "Lucario","Greninja","Ness",
             "Lucas","Ice Climbers","Marth","Roy",
             "Ike","Lucina","Robin","Corrin",
             "Mr. Game & Watch",
             "Palutena","Dark Pit", "Pit",
             "Wario","Olimar","R.O.B.","Villager",
             "Wii Fit Trainer", "Little Mac",
             "Shulk", "Duck Hunt",
             "Snake","Sonic","Mega Man",
             "Pac-Man",
             "Ryu", "Cloud", "Bayonetta",
             "Mii Brawler",
             "Mii Swordfighter", 
             "Mii Gunner",
             "Daisy", "Piranha Plant", 
             "King K. Rool", "Ridley",
             "Incineroar", 
             "Chrom", 
             "Isabelle",
             "Inkling", "Ken", "Simon", "Richter",
             "Joker", "Hero", "Banjo & Kazooie")
length(fighter)
is.character(fighter)
is.factor(fighter)
write.table(fighter, "fighter_names.csv")