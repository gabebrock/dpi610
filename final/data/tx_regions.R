library(tibble)

tx_regions <- tibble(
  county_Name = c(
    # Region 1 - Lubbock (41 counties)
    "Armstrong", "Bailey", "Briscoe", "Carson", "Castro", "Childress",
    "Cochran", "Collingsworth", "Crosby", "Dallam", "Deaf Smith", "Dickens",
    "Donley", "Floyd", "Garza", "Gray", "Hale", "Hall", "Hansford", "Hartley",
    "Hemphill", "Hockley", "Hutchinson", "King", "Lamb", "Lipscomb", "Lubbock",
    "Lynn", "Moore", "Motley", "Ochiltree", "Oldham", "Parmer", "Potter",
    "Randall", "Roberts", "Sherman", "Swisher", "Terry", "Wheeler", "Yoakum",
    
    # Region 2 - Abilene (30 counties)
    "Archer", "Baylor", "Brown", "Callahan", "Clay", "Coleman", "Comanche",
    "Cottle", "Eastland", "Fisher", "Foard", "Hardeman", "Haskell", "Jack",
    "Jones", "Kent", "Knox", "Mitchell", "Montague", "Nolan", "Runnels",
    "Scurry", "Shackelford", "Stephens", "Stonewall", "Taylor", "Throckmorton",
    "Wichita", "Wilbarger", "Young",
    
    # Region 3 - Arlington (19 counties)
    "Collin", "Cooke", "Dallas", "Denton", "Ellis", "Erath", "Fannin",
    "Grayson", "Hood", "Hunt", "Johnson", "Kaufman", "Navarro", "Palo Pinto",
    "Parker", "Rockwall", "Somervell", "Tarrant", "Wise",
    
    # Region 4 - Tyler (23 counties)
    "Anderson", "Bowie", "Camp", "Cass", "Cherokee", "Delta", "Franklin",
    "Gregg", "Harrison", "Henderson", "Hopkins", "Lamar", "Marion", "Morris",
    "Panola", "Rains", "Red River", "Rusk", "Smith", "Titus", "Upshur",
    "Van Zandt", "Wood",
    
    # Region 5 - Beaumont (15 counties)
    "Angelina", "Hardin", "Houston", "Jasper", "Jefferson", "Nacogdoches",
    "Newton", "Orange", "Polk", "Sabine", "San Augustine", "San Jacinto",
    "Shelby", "Trinity", "Tyler",
    
    # Region 6 - Houston (13 counties)
    "Austin", "Brazoria", "Chambers", "Colorado", "Fort Bend", "Galveston",
    "Harris", "Liberty", "Matagorda", "Montgomery", "Walker", "Waller",
    "Wharton",
    
    # Region 7 - Austin (30 counties)
    "Bastrop", "Bell", "Blanco", "Bosque", "Brazos", "Burleson", "Burnet",
    "Caldwell", "Coryell", "Falls", "Fayette", "Freestone", "Grimes",
    "Hamilton", "Hays", "Hill", "Lampasas", "Lee", "Leon", "Limestone",
    "Llano", "Madison", "McLennan", "Milam", "Mills", "Robertson", "San Saba",
    "Travis", "Washington", "Williamson",
    
    # Region 8 - San Antonio (28 counties)
    "Atascosa", "Bandera", "Bexar", "Calhoun", "Comal", "De Witt", "Dimmit",
    "Edwards", "Frio", "Gillespie", "Goliad", "Gonzales", "Guadalupe",
    "Jackson", "Karnes", "Kendall", "Kerr", "Kinney", "La Salle", "Lavaca",
    "Maverick", "Medina", "Real", "Uvalde", "Val Verde", "Victoria", "Wilson",
    "Zavala",
    
    # Region 9 - Midland (30 counties)
    "Andrews", "Borden", "Coke", "Concho", "Crane", "Crockett", "Dawson",
    "Ector", "Gaines", "Glasscock", "Howard", "Irion", "Kimble", "Loving",
    "Martin", "Mason", "McCulloch", "Menard", "Midland", "Pecos", "Reagan",
    "Reeves", "Schleicher", "Sterling", "Sutton", "Terrell", "Tom Green",
    "Upton", "Ward", "Winkler",
    
    # Region 10 - El Paso (6 counties)
    "Brewster", "Culberson", "El Paso", "Hudspeth", "Jeff Davis", "Presidio",
    
    # Region 11 - Edinburg (19 counties)
    "Aransas", "Bee", "Brooks", "Cameron", "Duval", "Hidalgo", "Jim Hogg",
    "Jim Wells", "Kenedy", "Kleberg", "Live Oak", "McMullen", "Nueces",
    "Refugio", "San Patricio", "Starr", "Webb", "Willacy", "Zapata"
  ),
  county_Region = c(
    rep("1",    41),
    rep("2",    30),
    rep("3",  19),
    rep("4",      23),
    rep("5",   15),
    rep("6",    13),
    rep("7",     30),
    rep("8",28),
    rep("9",    30),
    rep("10",    6),
    rep("11",  19)
  )
)