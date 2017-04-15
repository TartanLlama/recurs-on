module World where

-- Things which do something to an object and update the game state
type Action  = Object -> GameData -> (GameData, String)

-- Things which just update the game state
type Command = GameData -> (GameData, String)

-- Direction data type - for moving to different rooms.
data Direction = North
               | Northeast
               | East
               | Southeast
               | South
               | Southwest
               | West
               | Northwest
               | Up
               | Down
    deriving (Eq, Show, Read)
-- End of directions





-- GameData - the game state
data GameData = GameData {
                           -- where player is:
                           location_id :: String,
                           -- list of rooms/room ids:
                           world :: [(String, Room)],
                           -- objects player has:
                           inventory :: [Object],
                           -- is coffee poured:
                           poured :: Bool,
                           -- has coffee been drunk:
                           caffeinated :: Bool,
                           -- been to the entrance room
                           visited_entrance :: Bool,
                           --given code to God
                           given_code :: Bool,
                           --given pet to God
                           given_pet :: Bool,
                           --given food to God
                           given_food :: Bool,
                           --has the power to filter
                           has_filter :: Bool,
                           -- the power of recursion
                           has_recursion :: Bool,
                           --if the user has used xyzzy succesfully
                           xyzzy_used :: Bool,
                           --has the player spoken to lucas
                           spoken_lucas :: Bool,
                           --has the player spoken to mancomb
                           spoken_mancomb :: Bool,
                           --has the player ironed out the bug
                           debugged :: Bool,
                           -- set to True at the end
                           finished :: Bool,
                           spoken_native :: Bool,
                           completed_india :: Bool,
                           pressed_button :: Bool,
                           spoken_guide :: Bool,
                           fixed_wheel :: Bool
                         }
    deriving (Show, Read)

-- won - a function to determine from the game state if the game has been won.
won :: GameData -> Bool
won gd = location_id gd == "dungeon"

getRoom :: [(String,Room)] -> String -> Room
--getRoom [] _ = Nothing
getRoom ((x,y):xys) name | x == name = y
                         | otherwise = getRoom (xys) name

getExitData :: [Exit] -> String
getExitData [] = ""
getExitData (x:xs) = exit_desc x ++ "\n" ++ getExitData xs   

getObjData :: [Object] -> String
getObjData [] = ""
getObjData (x:xs) = obj_longname x ++ "\n" ++ getObjData xs


getRoomDescription :: String -> String
getRoomDescription roomName = rmdescription ++ "\n" ++ getExitData rmexits ++
                                  "\n" ++ getObjData rmobjects
                              where
                                  room = (getRoom gameworld roomName)
                                  rmdescription = room_desc room
                                  rmexits = exits room
                                  rmobjects = objects room

-- For converting the game state into a string
getString gd = 
        rm_desc ++ "\n" ++ concatMap exit_desc rm_exits ++ showInv rm_objs
        where
            rm_data = getRoomData gd
            rm_desc = room_desc rm_data
            rm_exits = exits rm_data
            rm_objs = objects rm_data
            showInv [] = ""
            showInv xs = "\n\nYou can see: " ++ showInv' xs
            showInv' [x] = (obj_longname x)
            --RECURSIVE
            showInv' (x:xs) = (obj_longname x) ++ ", " ++ (showInv' xs)

-- List of rooms in the world
gameworld = [("bedroom", bedroom),
             ("kitchen", kitchen),
             ("hall", hall),
             ("cycle path", blocked_cycle_path),
             ("playing fields", playing_fields),
             ("car park", car_park),
             ("entrance", entrance),
             ("arena", arena),
             ("dungeon", dungeon),
             ("office", office),
             ("m1", m1),
             ("m2", m2),
             ("m3", m3),
             ("m4", m4),
             ("m5", m5),
             ("m6", m6),
             ("m7", m7),
             ("m8", m8),
             ("m9", m9),
             ("m10", m10),
             ("m11", m11),
             ("m12", m12),
             ("m13", m13),
             ("m14", m14),
             ("mg", mg),
             ("sam", sam), 
             ("monkey", monkey),
             ("portal_room", portal_room),
             ("small_room", small_room),
             ("prison", prison),
             ("beach", beach),
             ("north_beach", north_beach),
             ("sea", sea),
             ("submarine", submarine),
             ("golfcourse", golfcourse),
             ("scores", scores),
             ("north_street", north_street),
             ("cinema_outside", cinema_outside),
             ("cinema", cinema),
             ("science_area", science_area),
             ("sand_dunes", sand_dunes),
             ("castle_entrance", castle_entrance),
             ("castle", castle),
             ("mine", mine),
             ("fridge", fridge),
             ("subkitchen", subkitchen),
             ("portal1",portal1),
             ("portal2",portal2),
             ("indiaentrance",indiaentrance),
             ("taj",taj),
             ("indiacenter",indiacenter),
             ("indiawest",indiawest),
             ("indiaeast",indiaeast),
             ("slum1",slum1),
             ("slum2",slum2),
             ("slum3",slum3),
             ("slum4",slum4),
             ("slum5",slum5),
             ("slum6",slum6),
             ("computer_room", computer_room)]

-- The initial game state
initState :: GameData
initState = GameData "bedroom" gameworld [] False False False False False False
                     False False False False False False False False False
                     False False False
-- End of GameData code





-- Room - an area in the game
data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
    deriving (Eq, Show, Read)

-- Exit - an exit from a room
data Exit = Exit { exit_dir :: Direction,
                   exit_desc :: String,
                   room :: String }
    deriving (Eq, Show, Read)

-- Return the room the player is currently in.
getRoomData :: GameData -> Room
getRoomData state = room
    where
        loc_id = location_id state
        [(_, room)] = filter (\ (id, _) -> id == loc_id) (world state)
        
-- Rooms in the game:
bedroom, kitchen, hall, blocked_cycle_path, car_park :: Room

bedroom = Room "You are in your bedroom. "
               [Exit North "To the north is a kitchen. " "kitchen"]
               [mug, keycard]

bedroom_exit = Exit East "To the east is a portal leading back to India" "indiaeast"

kitchen = Room "You are in the kitchen. "
               [Exit South "To the south is your bedroom. " "bedroom",
                Exit West "To the west is a hallway. " "hall"]
               [coffeepot]

kitchen_exit = Exit North "Your fridge to the north looks different than before. " "fridge"

hall = Room "You are in the hallway. "
            [Exit East "To the east is a kitchen. " "kitchen"]
            [closed_door]

hall_door_exit = Exit North "You can go outside to the north. " "cycle path"
        

blocked_cycle_path = Room ("You are on a long cycle path, leading to the Jack Cole building. "
                        ++ "There is an exit to the north, but it is blocked by a zombie.")
                     [Exit South "A doorway to the south leads into your house. " "hall",
                      Exit East "You can go east to the playing fields. " "playing fields"]
                     [zombie, fiver]

cycle_path_exit = Exit North "The path continues north towards the Jack Cole building. " "car park"
cycle_path_new_desc = "You are on a long cycle path. "

playing_fields = Room "You are in a large playing field. "
                 [Exit West "A gap in the hedges leads back to the cycle path in the west. " "cycle path"]
                 [hockey_stick, bum, shed]

science_area = Room ("You are on the North Haugh, where all of the science buildings are situated."
                  ++ " Zombies block the road to the Jack Cole Building.")
               [Exit East "To the east is north street. " "north_street"]
               [zombie_horde]
science_area_exit = Exit Southeast "To the south-east is the car park for the jack cole building. "
                         "car park"
science_area_new_desc = "You are on the North Haugh, where all of the science buildings are situated."

car_park = Room ("You are in a small car park outside the Jack Cole Building. Zombies block the road to"
              ++ " the rest of the science buildings.")
           [Exit South "A long cycle path extends to the south. " "cycle path"]
           [closed_door, zombie_horde]
car_park_exit = Exit North "There is a doorway to the north leading into the Jack Cole Building. "
                     "entrance"
car_park_exit_2 = Exit Northwest ("To the north-west is the rest of the North Haugh, containing other"
                               ++ " science buildings. ") "science_area"
car_park_new_desc = "You are in a small car park outside the Jack Cole Building."

entrance = Room "You are in the entrance to the Jack Cole building. " 
           [Exit South "You can see the car park through the door to the south. " "car park",
            Exit Down "Dark stairs lead down to a small office. " "office",
            Exit West "A computer room can be seen to the west. " "computer_room",
            Exit East "A very narrow corridor can be seen to the east. " "portal_room"]
           [func_god, closed_door]
entrance_exit = Exit North ("An open double door to the north beckons you towards what looks like some"
                         ++ " kind of arena.") "arena"

arena = Room ("You have walked into a large, circular, symmetrical room. Standing in the centre,"
           ++ " waiting for you, is the Java-monster.")
        [Exit South "You can get back to the entrance through the door to the south.  " "entrance"]
        [java_monster_with_defences]
arena_exit = Exit North "With the Java-monster out of the way, you can proceed north to Haskell's dungeon!"
                  "dungeon"
arena_new_desc = "You are in an arena. What used to be the terrible Java-monster is now a bunch of"
              ++ " scrambled-up lines of code."

dungeon = Room "" [] []

office = Room "You are in a small office. "
        [Exit Up "Stairs lead back up to the building entrance. " "entrance",
         Exit Down "Stairs lead down to a maze of sorts" "m1"]
        [journal]

m1 = Room "You are asdk ewr maze qweiy twisty litte asdfslkg, all comarable"
     [Exit Up "" "office",
      Exit Southwest "" "m14",
      Exit East "" "m2",
      Exit Southeast "" "m3"]
     []

m2 = Room "You msbd in a maze of eaiuf little passages, all compaable"
     [Exit West "" "m1",
      Exit Northeast "" "m6"]
     []

m3 = Room "You are lhwe a maze of twisty poiufd passages, all coparable"
     [Exit Northwest "" "m1",
      Exit East "" "m4"]
     []

m4 = Room "You zziyc in a maze of youisd little passages, all cmparable"
     [Exit West "" "m3",
      Exit Northeast "" "m10"]
     []

m5 = Room "You are in a maze of twisty little sdcvoiu, all comparabe"
     [Exit Up "" "m6"]
     []

m6 = Room "You are in a maze of twisty little passages, uidy comparable"
     [Exit Down "" "m5",
      Exit Southwest "" "m2",
      Exit East "" "m7"]
     []

m7 = Room "You are in yxza maze of vhifr little zixugc, all comarable"
     [Exit West "" "m6",
      Exit Up "" "m8",
      Exit Southeast "" "m11"]
     []

m8 = Room "You qwyt in a maze of twisty little passages, fdsae comparabl"
     [Exit Northwest "" "m9",
      Exit Down "" "m7"]
     []

m9 = Room "Hiouyweq are in nzdf maze of twisty little passages, all comarable"
     [Exit North "" "m14",
      Exit Southeast "" "m8"]
     []

m10 = Room "You are iuye wgeqtg maze kjhe twisty little passages, riuyse comarable"
     [Exit Southwest "" "m4",
      Exit Down "" "m12",
      Exit Southeast "" "m13",
      Exit East "" "m11"]
     []

m11 = Room "You are in a maze of beibw uihewr passages, all comparble"
     [Exit West "" "m10",
      Exit Northwest "" "m7"]
     []

m12 = Room "You are in a maze of twisty little kjwhrr, all comparabe"
     [Exit Up "" "m10"]
     []

m13 = Room "You are in a maze of twisty duiqew passages, seiourg comparabl"
     [Exit Northwest "" "m10",
      Exit Down "" "mg"]
     []

m14 = Room "You hklaw in a qliueg of twisty little passages, all comparale"
     [Exit South "" "m9",
      Exit Northeast "" "m1"]
     []

mg = Room "You are in what looks and smells like someone's mum's basement."
     [Exit Up "Stairs lead back up to the maze. I hope you mapped your route! " "m13",
      Exit East "There is an orange portal to the east. " "sam",
      Exit Southeast "There is a purple portal to the south east. " "monkey"]
     [lucas]

sam = Room "You are in what appears to be a fireman's office."
      [Exit West "You can go back through the orange portal to the west. " "mg"]
      [code, cockroach, sam_note]

monkey = Room "You appear to be on an island surrounded by a big, blue sea."
         [Exit Northwest "You can go back through the purple portal to the north west. " "mg"]
         [mancomb, coconut, monkey_note]


portal_room = Room "A strange, impossible looking room. There are several one-way portals in this room."
              [Exit West "You can see the entrance to the building down a narrow corridor to the west. "
                    "entrance",
               Exit Northeast "To the north-east is a green portal. " "bedroom",
               Exit East "A blue portal lies to the east. " "beach",
               Exit Southeast "There is a red portal in the south-east. " "small_room"]
              []

small_room = Room ("You find yourself in a small room with nothing but a metal door hooked up to some"
                ++ " kind of computer, and a trapdoor.")
             [Exit Down "There is a trapdoor below you. " "portal_room"]
             [closed_door]
small_room_exit = Exit South "The big metal door to the south has been opened. " "prison"

prison = Room ("You are in an ugly room with an industrial feel to it; it looks like a strange"
            ++ " futuristic prison. There are various people and objects from the computer science"
            ++ " department in the jail.")
         [Exit North "The small room you came from can be found to the north. " "small_room"]
         [prison_force_field]

beach = Room ("You are on the west sands beach.\n"
           ++ "  Judging by the waste left lying around, "
             ++ "there must be object-oriented programmers nearby.")
        [Exit East "To the east is the sea. " "sea",
         Exit South "Just south of your position, you can see the golf course. " "golfcourse",
         Exit North "The beach is quite extensive, and it carries on for quite a bit to the north. "
              "north_beach",
         Exit West "To the west, there are some sand dunes which look fun to climb on. " "sand_dunes"]
        [oop_stuff]

north_beach = Room ("You are at the north of the beach.")
                    [Exit South "You can go back along the beach to the south. " "beach",
                     Exit Southwest "There are sand dunes to the southwest. " "sand_dunes"]
                    [programmers]

sand_dunes = Room ("You are in some sand dunes, You wonder what kind of cool stuff could be lying around"
                ++ " up here.")
             [Exit East "To the east is the main part of the beach. " "beach",
              Exit Northeast "If you go northeast, you'll end up on the north of the beach. "
                   "north_beach"]
             [shotgun]

sea = Room "You are swimming in the sea, it is freezing cold and you wonder why you decided to do this."
      [Exit Down "A dark shape underwater makes you nervous... " "submarine",
       Exit West "You can swim west, back to the shore. " "beach"]
      []

submarine = Room "You have found a submarine!"
            [Exit Up "You can exit the submarine and swim back up to the surface. " "sea"]
            [recursion]

golfcourse = Room ("You are on the golf course, near the 18th hole.")
             [Exit North "To the north is west sands. " "beach"]
             [zombie_horde]

golfcourse_exit_1 = Exit South "You can go south, to north street. " "north_street"
golfcourse_exit_2 = Exit East "To the east is the scores. " "scores"
golfcourse_new_desc = "You are near the 18th hole of the golf course. Everything is "
                   ++ "peaceful except for the spent shotgun shells and zombie guts. "

scores = Room ("You are on the scores, you have a beautiful view of the sea from here.")
         [Exit West "If you go west, you can get back to the golf course. " "golfcourse",
          Exit East "If you continue east, you will reach the castle. " "castle_entrance"]
         []

castle_entrance = Room ("You are outside the entrance to the castle.")
                  [Exit West "If you go west, you will get back to the scores. " "scores"]
                  [castle_force_field]

castle_entrance_exit = Exit North "You can go in the castle to the north. " "castle"
castle_entrance_new_desc = "You are outside the castle entrance, the force field has been disabled. "

castle = Room ("You are in the castle; A hollow voice asks \"What's the magic word?\".")
         [Exit South "You can exit the castle to the south. " "castle_entrance",
          Exit Down "There is a passage downwards." "mine"]
         []
castle_new_desc = "You are in the castle, Nothing to see here except history."

mine   = Room ("You crawl through a narrow tunnel, and end up in a mine.")
         [Exit Up "You can crawl back up to the castle." "castle"]
         [pickaxe]

north_street = Room ("You are on north street.")
       [Exit North "To the north is the golf course. " "golfcourse",
       Exit West "If you go west, you'll end up back around the science buildings. " "science_area",
       Exit East "If you go further down north street, to the east, you will find the cinema. "
            "cinema_outside"]
       []

cinema_outside = Room ("You are outside the cinema. You see on the board that Monty Python and the"
                    ++ " Holy Grail is currently playing.")
                 [Exit North "The cinema entrace is to the north. " "cinema",
                  Exit West "North street stretches back down to the west. " "north_street"]
                 []

cinema = Room ("You are in the cinema. You vaguely notice some characters on the screen talking about"
            ++ " african and european swallows.")
         [Exit South "The main entrance is behind you, to the south. " "cinema_outside"]
         [python]
       
fridge = Room "You open the fridge and discover a secret room"
         [Exit South "A thin passage leads back to the entrance to the South. " "kitchen",
          Exit Down "A small hole in the ground with a ladder that gives you the shivers. " "subkitchen"]
         [matches]

subkitchen = Room "It is pitch black, but you are unlikely to be eaten by a grue."
             [Exit Up "The ladder you came down leads back to the fridge. " "fridge"]
             []

lights_desc = "Ah, thanks to the matches I can see this room, its a professional kitchen! "
lights_exit =  Exit South "You can see a portal to the south. " "portal1"
lights_obj = [grinder]

portal1 = Room "A voice asks you: What is roughly the population of India?"
          [Exit Up "Up - Less than 1 million. " "portal1",
           Exit Down "Down - Between 1 and 10 million. " "portal1",
           Exit East "East - Between 10 and 99 million. " "portal1",
           Exit West "West - Between 100 million and a billion. " "portal1",
           Exit South "South - Between a billion and 2 billion. " "portal2", --exit South
           Exit North "North - More than 2 billion. " "portal1"]
          []

portal2 = Room ("To be allowed entrance into India, you need to prove your intelligence.\n"
             ++ "What year was Haskell Curry born?")
          [Exit Up "Up -> 1900 " "indiaentrance", --exit
           Exit Down "Down -> 1990. " "portal2",
           Exit East "East -> 1890 " "portal2",
           Exit West "West -> 1321 " "portal2",
           Exit South "South -> 6BC " "portal2",
           Exit North "North -> Over 9000 years ago " "portal2"]
          [dvd]


indiaentrance = Room ("You appear to be in a rural area of northern India. A sign points towards"
                   ++ " Agra to the west, but you don't think you could make it on foot.")
                []
                [guide_init]

indiaentrance_exit = Exit West "You can go west on your guide's rickshaw" "taj"

taj = Room "You are stopped in front of the Taj Mahal. You wonder why you're not moving."
           []
           [guide_stopped, wheel]

taj_desc = "You are stopped in front of the Taj Mahal. Drink in the sights!"

taj_exit = Exit South "Your guide will take you south to your destination" "indiacenter"

indiacenter = Room "You are in a clearing of a dense forest . "
              [Exit West "There is a swamp to the West, looks potentially dangerous. " "indiawest",
               Exit East "You can see a small village to the East, looks welcoming. " "indiaeast"]
              []
            
indiawest = Room "You are in a nasty swamp. There are mosquitos everywhere and you fear catching malaria."
            [Exit East "You can go back to the clearing east. " "indiacenter",
             Exit West "There is a dirty looking slum to the west. It looks quite forboding" "slum1"]
            []

slum1 = Room "This place doesn't look friendly. I wonder who lives here... "
        [Exit East "The swamps are to the East. " "indiawest",
         Exit West "A dark road leading deep into the slums is to the west. " "slum2"]
        []

slum2 = Room "You are at a crossroads. "
        [Exit South "There is a dead end to the south. " "slum3",
         Exit North "You can go north to what looks like another dead end. " "slum4",
         Exit West " busy market to the West, doesn't look like a normal market. " "slum5",
         Exit East "Going east will lead you back to the swamp. " "slum1"]
        []

slum3 = Room "This isn't were you should stay, there's only one way back and you don't feel safe. "
        [Exit North "North leads you back to the crossroads. " "slum2"]
        [rice]

slum4 = Room "This won't get you anywhere, you don't feel too well. You should really be heading back. "
        [Exit South "Going south will bring you back to the crossroads. " "slum2"]
        [pan]

slum5 = Room "This busy market doesn't seem safe at all.  "
        [Exit East "You can head back to the east " "slum2"]
        [closed_door]

slum5_exit = Exit South "You can see what looks like a stash room to the south" "slum6"

slum6 = Room "This seems to be a secret stash room. "
        [Exit North "The exit to the north leads back into the market. " "slum5"]
        [thief, cookbook, herbs]


indiaeast = Room "You are in a small village. Perhaps this is where you will find the secret ingredient!"
            [Exit West "To the west is the entrance of the village and the swamp. " "indiacenter",
             Exit South "There is a sparkling portal to the south. You can see your bedroom through it!"
                  "bedroom"]
            [native]
        
computer_room = Room "You are in a computer room. "
                [Exit East "You can go back to the building entrance to the east. " "entrance"]
                [elf, svn, button]
-- End of Room code





-- Object - a game object such as a mug.
data Object = Obj { obj_name :: ObjName,
                    obj_longname :: String,
                    obj_desc :: String,
                    gettable :: Bool,
                    open_state :: Maybe OpenState,
                    filterable :: Bool,
                    awesome :: Bool, --checking if the curry has IngredientX
                    person :: Bool
                  }
    deriving (Eq, Show, Read)

-- default_obj - default object will values such as gettable set to false.
default_obj = Obj { obj_name = Mug,
                    obj_longname = "",
                    obj_desc = "",
                    gettable = False,
                    open_state = Nothing,
                    filterable = False,
                    awesome = False,
                    person = False }

data OpenState = Opened | Closed
               deriving (Eq, Show, Read)

-- Names for objects
data ObjName = Mug | Coffee | Zombie | Door | KeyCard | Fiver | HockeyStick
             | Bum | Shed | Crowbar | FuncGod | Paper | Journal | OOPStuff
             | Recursion | ZombieHorde | Shotgun | ForceField | Programmers
             | Password | Python | Grinder | Herbs | Matches | Pan | SamNote
             | MonkeyNote | Coconut | Lucas | Cockroach | Iron | Code 
             | Mancomb | Slash | Pickaxe | DVD | GuideStopped | GuideInit | Native | Cookbook
             | Paste | Curryobj | Prisoners | EncryptionKey | Haddock | Sauce | Rice | Thief | Nativedoc
             | JavaMonster | IngreX | Wheel | Button | Svn | Elf | XCurry
             deriving (Eq, Show, Read)

-- Return the data constructor for the given object string
transformObject :: String -> GameData -> Maybe ObjName
transformObject s state
    | s == "mug" || s == "coffee" && poured state                                  = Just Mug
    | s == "coffee" && not (poured state) || s == "pot" || s == "coffeepot"        = Just Coffee
    | s == "door"                                                                  = Just Door
    | s == "card" || s == "keycard"                                                = Just KeyCard
    | s == "shed"                                                                  = Just Shed
    | s == "bum" || s == "man" && location_id state == "playing fields"            = Just Bum
    | s == "stick"                                                                 = Just HockeyStick
    | s == "money" || s == "fiver"                                                 = Just Fiver
    | s == "zombie"                                                                = Just Zombie
    | s == "zombies" || s == "horde" || s == "zombiehorde"                         = Just ZombieHorde
    | s == "crowbar"                                                               = Just Crowbar
    | s == "larry" || s == "god" || s == "man" && location_id state == "entrance"  = Just FuncGod
    | s == "paper" || s == "sheet"                                                 = Just Paper
    | s == "journal"                                                               = Just Journal
    | s == "crap"                                                                  = Just OOPStuff
    | s == "game" || s == "recursion" || s == "recurs!on" || s == "computergame"
        || s == "computer"                                                         = Just Recursion
    | s == "gun" || s == "shotgun" || s == "pumpactionshotgun"                     = Just Shotgun
    | s == "field" || s == "forcefield" || s == "force-field"                      = Just ForceField
    | s == "programmers"                                                           = Just Programmers
    | s == "password"                                                              = Just Password
    | s == "python"                                                                = Just Python
    | s == "grinder"                                                               = Just Grinder
    | s == "herbs"                                                                 = Just Herbs
    | s == "matches" || s == "match"                                               = Just Matches
    | s == "pan"                                                                   = Just Pan
    | s == "message" && location_id state == "sam"                                 = Just SamNote
    | s == "message" && location_id state == "monkey"                              = Just MonkeyNote
    | s == "programmer" || s == "lucas" || s == "man" && location_id state == "mg" = Just Lucas
    | s == "mancomb" || s == "pirate" || s == "man" && location_id state == "monkey"  = Just Mancomb
    | s == "bug" || s == "cockroach" || s == "roach"                               = Just Cockroach
    | s == "iron"                                                                  = Just Iron
    | s == "code"                                                                  = Just Code
    | s == "coconut"                                                               = Just Coconut
    | s == "slash" || s == "/"                                                     = Just Slash
    | s == "pick" || s == "axe" || s == "pickaxe"                                  = Just Pickaxe
    | s == "dvd"                                                                   = Just DVD
    | (s == "guide" || s == "indian" || s == "man") 
      && location_id state == "indiaentrance"                                      = Just GuideInit
    | (s == "guide" || s == "indian" || s == "man") 
      && location_id state == "taj"                                                = Just GuideStopped
    | s == "native"                                                                = Just Native
    | s == "cookbook" || s == "book"                                               = Just Cookbook
    | s == "paste"                                                                 = Just Paste
    | s == "sauce"                                                                 = Just Sauce
    | s == "rice"                                                                  = Just Rice
    | s == "thief" || s == "boy" || s == "child"                                   = Just Thief
    | s == "curry"                                                                 = Just Curryobj
    | s == "prisoners" || s == "people"                                            = Just Prisoners
    | s == "haddock" || s == "fish" || s == "tank" || s == "fishtank"              = Just Haddock
    | s == "encryption" || s == "key" || s == "encryption-key"                     = Just EncryptionKey
    | s == "monster" || s == "java" || s == "javamonster" || s == "java-monster"   = Just JavaMonster
    | s == "ingredient" || s == "ingredientx"                                      = Just IngreX
    | s == "wheel" || s == "cartwheel"                                             = Just Wheel
    | s == "button" || s == "commit"                                               = Just Button
    | s == "console"                                                               = Just Svn
    | s == "elf" || s == "bob"                                                     = Just Elf
    | otherwise                                                                    = Nothing

-- Game objects:
mug, fullmug, coffeepot, emptypot, zombie, closed_door, open_door :: Object
keycard, bum, hockey_stick, fiver, shed, crowbar, grinder         :: Object
mug           = default_obj { obj_name = Mug,
                              obj_longname = "A coffee mug",
                              obj_desc = "An empty coffee mug",
                              gettable = True }

fullmug       = default_obj { obj_name = Mug,
                              obj_longname = "A full coffee mug",
                              obj_desc = "A coffee mug containing freshly brewed coffee. ",
                              gettable = True }

coffeepot     = default_obj { obj_name = Coffee,
                              obj_longname = "A coffee pot",
                              obj_desc = "A pot containing freshly brewed coffee. ",
                              gettable = True }

emptypot      = default_obj { obj_name = Coffee,
                              obj_longname = "A coffee pot",
                              obj_desc = "A sad looking coffee pot with no coffee in it. ", 
                              gettable = True }

zombie        = default_obj { obj_name = Zombie,
                              obj_longname = "A scary zombie",
                              obj_desc = "Mindless zombie, can be heard muttering about object-oriented"
                                      ++ " programming, eats brains." }

zombie_horde  = default_obj { obj_name = ZombieHorde,
                              obj_longname = "A horde of terrifying zombies blocking the road!",
                              obj_desc = "A horde of terrifying mindless zombies, just like the one you"
                                      ++ " saw earlier!" }

closed_door   = default_obj { obj_name = Door,
                              obj_longname = "A closed door",
                              obj_desc = "A closed door. ",
                              open_state = Just Closed }

open_door     = default_obj { obj_name = Door,
                              obj_longname = "An open door",
                              obj_desc = "An open door. ",
                              open_state = Just Opened }

keycard       = default_obj { obj_name = KeyCard,
                              obj_longname = "A keycard",
                              obj_desc = "A keycard. ",
                              gettable = True }

bum           = default_obj { obj_name = Bum, 
                              obj_longname = "A bum",
                              obj_desc = "An old, smelly bum. ", 
                              person = True }

hockey_stick  = default_obj { obj_name = HockeyStick, 
                              obj_longname = "A hockey stick",
                              obj_desc = "A run-of-the-mill wooden field-hockey stick. ",
                              gettable = True }

fiver         = default_obj { obj_name = Fiver, 
                              obj_longname = "A five pound note",
                              obj_desc = "A five pound note. ", 
                              gettable = True }

shed          = default_obj { obj_name = Shed, 
                              obj_longname = "A shed",
                              obj_desc = "An old wooden shed made of twisted, rotten wood. ",
                              open_state = Just Closed }

crowbar       = default_obj { obj_name = Crowbar,
                              obj_longname = "A crowbar",
                              obj_desc = "A heavy metal crowbar",
                              gettable = True }

func_god      = default_obj { obj_name = FuncGod,
                              obj_longname = "The God of Functional Programming",
                              obj_desc = "A blue man who claims to be the God of Functional Programming. ", 
                              person = True }

paper         = default_obj { obj_name = Paper,
                              obj_longname = "A blank piece of paper",
                              obj_desc = "Just a piece of paper filled with white space. ",
                              gettable = True }

journal      = default_obj { obj_name = Journal,
                             obj_longname = "A small journal",
                             obj_desc = "It's all blank except for one message:\n\n"
                                     ++ "\"The maze...the maze...wrong is valid...missing is right..."
                                     ++ "letters...words are directions...\". ",
                             gettable = True }

oop_stuff    = default_obj { obj_name = OOPStuff,
                             obj_longname = "A bunch of object-oriented crap",
                             obj_desc = "A bunch of object oriented crap:\n"
                                     ++ "  Some useless classes and objects,\n"
                                     ++ "  interfaces which specify one thing and are only used once,\n"
                                     ++ "  and factories which produce factories for some reason." }

recursion    = default_obj { obj_name = Recursion,
                             obj_longname = "A computer running the game \"Recurs!on\"",
                             obj_desc = "A computer running the new blockbuster game \"Recurs!on\"\n"
                                     ++ "The storyline of this game looks oddly familiar, and you wonder"
                                     ++ " how it got here."}

shotgun      = default_obj { obj_name = Shotgun,
                             obj_longname = "A pump-action shotgun",
                             obj_desc = "Built for kicking ass.",
                             gettable = True }

castle_force_field = default_obj { obj_name = ForceField,
                                   obj_longname = "A powerful forcefield surrounding the castle",
                                   obj_desc = "The force field shimmers a translucent blue colour.\n"
                                           ++ "You wonder how you will ever get past it.",
                                   filterable = True }

prison_force_field = default_obj { obj_name = ForceField,
                                   obj_longname = "A powerful forcefield, trapping various objects"
                                               ++ " and people from the computer science department.",
                                   obj_desc = "The force field shimmers a translucent blue colour.\n"
                                           ++ "You wonder how you will ever get past it.",
                                   filterable = True }

programmers = default_obj { obj_name = Programmers,
                            obj_longname = "A collection of object-oriented programmers.",
                            obj_desc = "A bunch of object-oriented programmers, "
                                    ++ "You should interrogate them for information.",
                            person = True }

password    = default_obj { obj_name = Password,
                            obj_longname = "A password written down on a piece of paper.",
                            obj_desc = "It says \"the password is 'password'\".",
                            gettable = True }

python      = default_obj { obj_name = Python,
                            obj_longname = "Python, a good friend of you and Haskell.",
                            obj_desc = "Neatly and dynamically presented.", 
                            person = True}

prisoners      = default_obj { obj_name = Prisoners,
                               obj_longname = "The prisoners from the jail cell, now freed.",
                               obj_desc = "They are very relieved to get out.", 
                               person = True}
                             
grinder         = default_obj { obj_name = Grinder,
                             obj_longname = "A herb grinder",
                             obj_desc = "A herb grinder" }

herbs         = default_obj { obj_name = Herbs,
                             obj_longname = "Some herbs",
                             obj_desc = "A suspicious looking plastic bag of dried herbs"
                                     ++ " - doesn't smell too bad, a tad spicy",
                             gettable = True }
                             
matches         = default_obj { obj_name = Matches,
                 obj_longname = "Some matches",
                 obj_desc = "some matches, how convinient I can burn something or maybe illuminate a room",
                 gettable = True }

pan         = default_obj { obj_name = Pan,
                 obj_longname = "An Asian style pan",
                 obj_desc = "A large wok",
                 gettable = True }


sam_note  = default_obj { obj_name = SamNote,
                            obj_longname = "A message scratched into the wall",
                          obj_desc = "This is the game with the bug in it. It's called Max & Sam."
                                  ++ " It's about a hare and a wolf who are freelance firemen." }

monkey_note  = default_obj { obj_name = MonkeyNote,
                               obj_longname = "A message scratched into the sand",
                             obj_desc = "This is a game I'm working on called Gorrila island."
                                     ++ " You play as Mancomb Seepgood and need to defeat the"
                                     ++ " evil werewolf pirate, LeThrow" }

coconut  = default_obj { obj_name = Coconut,
                           obj_longname = "A coconut",
                         obj_desc = "A fragrant, fresh coconut",
                         gettable = True }

lucas  = default_obj { obj_name = Lucas,
                         obj_longname = "An unhappy looking programmer",
                       obj_desc = "A very fat, cross programmer", 
                       person = True }

cockroach  = default_obj { obj_name = Cockroach,
                             obj_longname = "A cockroach",
                           obj_desc = "A slimy cockroach. Ewww!" }

iron  = default_obj { obj_name = Iron,
                        obj_longname = "A portable iron",
                      obj_desc = "An iron that seems to run off your processing power.",
                      gettable = True }

code  = default_obj { obj_name = Code,
                        obj_longname = "A block of code",
                      obj_desc = "An unused piece of code. Perhaps you can get something out of it?" }

mancomb  = default_obj { obj_name = Mancomb,
                   obj_longname = "A skinny blonde pirate",
                         obj_desc = "A skinny blonde pirate wearing a fine leather jacket", 
                         person = True }

slash = default_obj { obj_name = Slash,
                      obj_longname = "A forward slash",
                      obj_desc = "&#47",
                      gettable = True }

pickaxe = default_obj { obj_name = Pickaxe,
                        obj_longname = "A pickaxe",
                        obj_desc = "A sharp-looking pickaxe",
                        gettable = True }
                        
dvd     = default_obj { obj_name = DVD,
                        obj_longname = "A DVD of One foot in the Grave",
                        obj_desc = "All of the Series of One Foot in the Grave starring Richard Wilson",
                        gettable = True }
            
guide_init = default_obj { obj_name = GuideInit,
                           obj_longname = "A very smiley Indian man",
                           obj_desc = "An Indian man who is smiling intently at you.", 
                           person = True }
            
guide_stopped = default_obj { obj_name = GuideStopped,
                              obj_longname = "Your very friendly Indian tour guide",
                              obj_desc = "Your Indian tour guide, he looks worried about something",
                              person = True}
                     
native = default_obj { obj_name = Native,
                       obj_longname = "An Indian native, what's he doing here?",
                       obj_desc = "A native old man, seems wise with his bald and toothless mouth" ,
                       person = True}


cookbook = default_obj { obj_name = Cookbook,
                         obj_longname = "An old indian cookbook",
                         obj_desc = "Pages upon pages of indian meals to cook which all take less"
                                 ++ " than 25min. Perhaps you should study it.",
                         gettable = True }
                     
paste = default_obj { obj_name = Paste,
                      obj_longname = "A yellow paste",
                      obj_desc = "A yellowish paste, smells of spices",
                      gettable = True }

sauce = default_obj { obj_name = Sauce,
                      obj_longname = "A curry sauce",
                      obj_desc = "A delicious curry sauce, now I need to cook it with something" }

rice = default_obj { obj_name = Rice,
                     obj_longname = "Some plain rice",
                     obj_desc = "Some rice, just the sort that would fit into a curry",
                     gettable = True } 

thief = default_obj { obj_name = Thief,
                      obj_longname = "A child thief",
                      obj_desc = "a child thief who looks like he's planning something", 
                      person = True }
                    

curryobj = default_obj { obj_name = Curryobj,
                         obj_longname = "Curry",
                         obj_desc = "It's only curry but still, it's amazing",
                         gettable = True }

xcurry_desc = "The perfect curry!"

haddock = default_obj { obj_name = Haddock,
                        obj_longname = "The computer science fish tank, containing a haddock",
                        obj_desc = "Since when did the computer science department have a pet haddock?",
                        gettable = True }

encryption_key = default_obj { obj_name = EncryptionKey,
                               obj_longname = "A powerful 1024-bit encryption key",
                               obj_desc = "You can use this to rescue haskell!",
                               gettable = True }

java_monster_with_defences = default_obj { obj_name = JavaMonster,
                                           obj_longname = "A terrifying Java monster, with a powerful"
                                                       ++ " encapsulation force field",
                                           obj_desc = "Does not support deep recursion.",
                                           filterable = True }

java_monster = default_obj { obj_name = JavaMonster,
                             obj_longname = "A Java monster with weakened defenses",
                             obj_desc = "Does not support deep recursion." }

ingreX = default_obj { obj_name = IngreX,
                       obj_longname = "IngredientX",
                       obj_desc = "Looks like a root vegetable of some kind. It's shaped almost"
                               ++ " exactly like a thingy", 
                       gettable = True }

wheel = default_obj { obj_name = Wheel,
                      obj_longname = "A cartwheel",
                      obj_desc = "A wooden cartwheel", 
                      gettable = True }
                       
elf = default_obj { obj_name = Elf,
                    obj_longname = "A little elf",
                    obj_desc = "A small man with pointy ears and a funny hat", 
                    person = True }

svn = default_obj { obj_name = Svn,
                    obj_longname = "A console",
                    obj_desc = "A console screen which says:\n"
                            ++ "BobTheElf@Console:~/kitchen$"
                            ++ " svn commit --message \"Added portals to India\"" }
      
button = default_obj { obj_name = Button,
                       obj_longname = "A commit button",
                       obj_desc = "A big button which says \"commit\"" }
         
-- End of Object code
