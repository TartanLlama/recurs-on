module Actions where

import World

actions :: GameData -> Cmd -> (GameData, String)
actions state (Use obj0 obj1)      = use obj0 obj1 state
actions state (OpenWith obj0 obj1) = open [obj0, obj1] state
actions state (Give obj0 obj1)     = give obj0 obj1 state 
actions state (HitWith obj0 obj1)  = hit [obj0, obj1] state
actions state (Cook obj0 obj1)     = cook obj0 obj1 state
actions state (Hit obj)            = hit [obj] state
actions state (Go dir)             = go dir state
actions state (Get obj)            = get obj state
actions state (Drop obj)           = put obj state
actions state (Pour obj)           = pour obj state
actions state (Examine obj)        = examine obj state
actions state (Drink obj)          = drink obj state
actions state (Open obj)           = open [obj] state
actions state (Close obj)          = close obj state
actions state (SuicideWith obj)    = suicide [obj] state
actions state (Light obj)          = light obj state
actions state (Watch obj)          = watch obj state
actions state (Talk obj)           = talk obj state
actions state (Grind obj)          = grind obj state
actions state (Eat obj)            = eat obj state
actions state (Study obj)          = study obj state
actions state Quit                 = quit state
actions state Look                 = look state
actions state Inventory            = inv state
actions state Help                 = help state
actions state Xyzzy                = xyzzy state
actions state Plugh                = plugh state
actions state (Play obj)           = play obj state
actions state (Filter obj)         = filterCmd obj state
actions state (IronCmd obj)        = ironCmd obj state
actions state (Recurse obj)        = recurseCmd obj state
actions state (Push obj)           = push obj state

data Cmd = Use Object Object
         | OpenWith Object Object
         | Give Object Object
         | HitWith Object Object
         | Hit Object
         | Go Direction
         | Get Object
         | Drop Object
         | Pour Object
         | Examine Object
         | Drink Object
         | Open Object
         | Close Object
         | SuicideWith Object
         | Light Object
         | Grind Object
         | Watch Object
         | Study Object
         | Talk Object
         | Eat Object
         | Suicide
         | Look
         | Quit
         | Inventory
         | Help
         | Xyzzy
         | Plugh
         | Play Object
         | Filter Object
         | IronCmd Object
         | Recurse Object
         | Cook Object Object
         | Push Object
           

-- Given a direction and a room to move from, return the room id in
-- that direction, if it exists.
move :: Direction -> Room -> Maybe String
move direction rm
    | null exit_list = Nothing
    | otherwise = Just (room exit)
    where
        exit_list = filter (\ x -> exit_dir x == direction) (exits rm)
        exit = head exit_list


-- Gives the description of the current room
look :: Command
look state = (state, getString state)


-- True if the object appears in the specified room, false otherwise.    
objectHere :: Object -> Room -> Bool
objectHere obj rm
    | null obj_list = False
    | otherwise     = True
    where
        obj_list = filter (\ x -> x == obj) (objects rm)

addExit :: Room -> Exit -> Room
addExit rm ex = rm{exits = (exits rm) ++ [ex]}

-- Given a game state and a room id, replace the old room information with
-- new data. If the room id does not already exist, add it.
-- This could possibly break things if used incorrectly; e.g. do not update a
-- room in which an object has been dropped, without including the dropped
-- object in the new room data.
updateRoom :: GameData -> String -> Room -> GameData
updateRoom state rmid rmdata =
    state { world = (rmid, rmdata):rooms }
    where
        rooms = filter (\ (id, rm) -> id /= rmid) (world state)

-- Play
play :: Action
play obj state =
    if obj == recursion
        then
            (state, "You play recurs!on, and find that it is awesome.\n  ...Get it?")
        else
            (state, "You play with it for a bit, giggle like a child, then realise that you're an idiot.")

-- Filter
filterCmd :: Action
filterCmd obj state =
    if has_filter state
        then if filterable obj && objectHere obj (getRoomData state)
            then if obj == castle_force_field
                then (new_state_castle, "You use your power, and filter the force field away.")
                else if obj == prison_force_field
                    then (new_prison_state, ("You use the power of filter to lower the forcefield, and"
                                         ++ " free the occupants of the prison!") )
                    else if obj == java_monster_with_defences
                        then (new_java_monster_state, ("The monster's defences are significantly damaged"
                                                    ++ " by the filter!") )
                        else (state, "Oh no! This hasn't been coded!")
            else (state, "You can't filter that!")
        else (state, "You don't know how to filter!")
    where
        new_castle_entrance = addExit (getRoomData state) castle_entrance_exit
        new_castle_desc = new_castle_entrance{room_desc = castle_entrance_new_desc}
        new_state_castle = newState state (removeObject castle_force_field new_castle_desc)
        new_prison = removeObject prison_force_field (getRoomData state)
        new_prison_objects = addObject prisoners (addObject haddock new_prison)
        new_prison_state = newState state new_prison_objects
        new_java_monster_state = newState state
                                          (addObject java_monster
                                                     (removeObject java_monster_with_defences
                                                                   (getRoomData state)))

recurseCmd :: Action
recurseCmd obj state =
    if has_recursion state
        then if obj == java_monster
            then (no_java_monster_state, ("You use recursion on the Java-monster, it can't handle it!\n"
                                       ++ "\"Aaaargh! StackOverFlowErrrroooorrr! Nooooooo!\""))
            else if obj == java_monster_with_defences
                then (state, "Your recursion fails, the Java-monster's defenses are still up!")
                else (state, "You can't use recursion on that.")
        else (state, "You don't know how to use recursion!")
    where
        new_arena = removeObject java_monster (getRoomData state)
        new_arena_with_exit = addExit new_arena arena_exit
        new_arena_desc = new_arena_with_exit{ room_desc = arena_new_desc }
        no_java_monster_state = newState state new_arena_desc

-- help - doesn't change the state, just gives a message listing commands etc.
help :: Command
help state =
    (state, message)
    where
        message = "\n"
               ++ "--------Recurs!on Help--------\n"
               ++ "\n"
               ++ "\n"
               ++ "Command Syntax:\n"
               ++ "\n"
               ++ "    \"command parameter\" - A command being used on an object.\n"
               ++ "        For example: \"open door\".\n"
               ++ "\n"
               ++ "    \"command\" - A command with no parameters.\n"
               ++ "        For example: \"look\".\n"
               ++ "\n"
               ++ "    \"command with object\" - To carry out a command with a specific object.\n"
               ++ "        For example: \"hit bum with stick\".\n"
               ++ "\n"
               ++ "    \"command then another_command\" - To carry out a sequence of commands.\n"
               ++ "        For example: \"pour coffee then drink coffee\".\n"
               ++ "                     \"go north then look\".\n"
               ++ "\n"
               ++ "\n"
               ++ "Command List:\n"
               ++ "\n"
               ++ "    save filename - To save the game with a certain filename.\n"
               ++ "    load filename - To load a game with the specified filename.\n"
               ++ "        Games will be stored in \"recursion_saves\", with \".rsav\" extension.\n"
               ++ "\n"
               ++ "    go direction/direction - To go in a certain direction.\n"
               ++ "    look/l - To examine your surroundings.\n"
               ++ "        Direction can be north, south, e, w, northeast, se, up, down, etc.\n"
               ++ "\n"
               ++ "    i/inv/inventory - To examine your inventory.\n"
               ++ "    get/take/g - To pick up an object.\n"
               ++ "    drop/d - To drop an object from your inventory.\n"
               ++ "    give object to object - To give something to another object.\n"
               ++ "    examine/x object - To examine an object.\n"
               ++ "    play object - To play with an object.\n"
               ++ "    use object with object - To use two items in conjunction\n"
               ++ "        you can complete the game without using this command, it is just a fallback.\n"
               ++ "        Generally try and use a different command).\n"
               ++ "\n"
               ++ "    talk to/talk to object - To talk to something.\n"
               ++ "\n"
               ++ "    open [with object]/o - To open an object such as a door [with an object].\n"
               ++ "    close/c - To close an object.\n"
               ++ "\n"
               ++ "    pour object - To pour something.\n"
               ++ "    drink object - To drink something.\n"
               ++ "    eat object - To eat something.\n"
               ++ "    light object - to set something on fire.\n"
               ++ "    watch object - to watch a suitable object like a DVD.\n"
               ++ "    grind object - grind an object to get a new object.\n"
               ++ "    cook object with object - To cook two things together.\n"
               ++ "    light object - To attempt to light an object.\n"
               ++ "    study object - To study a book.\n"
               ++ "    iron object - To iron something.\n"
               ++ "    push object - To push something like a button.\n"
               ++ "\n"
               ++ "    hit/attack/shoot object - To attack something.\n"
               ++ "    filter object - To filter something.\n"
               ++ "    recurse object - To call the power of recursion on something.\n"
               ++ "\n"
               ++ "    xyzzy - A mysterious magical word.\n"
               ++ "    plugh - A mysterious magical word.\n"
               ++ "\n"
               ++ "    commit suicide [with object] - To kill yourself [with an object].\n"
               ++ "    q/quit/exit - To exit the game.\n"
               ++ "    help - To get this help text.\n"
               ++ "\n"
               ++ "\n"
               ++ "--------Recurs!on Help--------\n"

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

open :: [Object] -> GameData -> (GameData, String)
open [obj0, obj1] state | location_id state == "car park"
                          && obj0 == closed_door
                          && obj1 == keycard = 
                              do let current_room = getRoomData state
                                 let added_exit = addExit current_room car_park_exit
                                 let changed_door = replaceObject added_exit closed_door open_door
                                 (newState state changed_door, "The door swings open automagically")
                        | location_id state == "small_room"
                          && obj0 == closed_door
                          && obj1 == password = 
                              do let current_room = getRoomData state
                                 let added_exit = addExit current_room small_room_exit
                                 let changed_door = replaceObject added_exit closed_door open_door
                                 (newState state changed_door, ("You hear the lound thunk of a heavy bolt"
                                                             ++ " shifting, and the door swings open."))
                        | location_id state == "entrance"
                          && obj0 == closed_door
                          && obj1 == encryption_key =
                              do let current_room = getRoomData state
                                 let added_exit = addExit current_room entrance_exit
                                 let changed_door = replaceObject added_exit closed_door open_door
                                 (newState state changed_door, "The door menacingly swings open...")

                        | location_id state == "slum5"
                          && obj0 == closed_door
                          && obj1 == crowbar = 
                              do let current_room = getRoomData state
                                 let added_exit = addExit current_room slum5_exit
                                 let changed_door = replaceObject added_exit closed_door open_door
                                 (newState state changed_door, "You wrench the door open.")
                         | location_id state == "slum5"
                          && obj0 == closed_door
                          && obj1 == shotgun = 
                              do let current_room = getRoomData state
                                 let added_exit = addExit current_room slum5_exit
                                 let changed_door = replaceObject added_exit closed_door open_door
                                 (newState state changed_door, "You shoot the door open.")
                        | location_id state == "slum5"
                          && obj0 == closed_door = (state, "You can't get enough leverage")
                        | otherwise = (state, "Can't do that")
                                                  
open [obj] state
    | objectHere obj (getRoomData state) 
      && obj == shed 
      && open_state shed == Just Closed  
          = if (objectHere bum (getRoomData state))
            then (state, "The bum shouts at you in gibberish and chases you away")
            else (newState state
                           (replaceObject (addObject crowbar (getRoomData state))
                                          shed
                                          shed{open_state=Just Opened}),
                           "The shed door creaks open")

    | objectHere obj (getRoomData state)
      && location_id state == "hall"
      && obj == closed_door = if caffeinated state
                              then do let current_room = getRoomData state
                                      let added_exit   = addExit current_room hall_door_exit
                                      let changed_door = replaceObject added_exit closed_door open_door
                                      (newState state changed_door, "Opened door.")
                              else (state, "Cannot leave house without caffeine!")
    | objectHere obj (getRoomData state)
      && location_id state == "car park"
      && obj == closed_door = (state, "The door seems to be locked")
    | location_id state == "small_room"
      && obj == closed_door  = (state, "The computer beeps and says \"ENTER PASSWORD:\"")      
    | location_id state == "slum5"
      && obj == closed_door = (state, "It seems to be stuck.")
    | open_state obj == Just Opened     = (state, "That's already open!")
    | otherwise                         = (state, ("You try to open it, but look rather silly and stop"
                                                ++ " your foolishness"))

{- a quicker way of using updateRoom -}
newState :: GameData -> Room -> GameData
newState state new_room = updateRoom state (location_id state) new_room

close :: Action
close obj state 
      | objectHere obj (getRoomData state)
        && obj == shed{open_state = Just Opened} = (state, "The shed door seems to be stuck open now")
      | objectHere obj (getRoomData state)
        && location_id state == "hall"
        && obj == open_door = do
            let current_room = getRoomData state
            let removed_exit = current_room{exits = filter (\x -> x /= hall_door_exit) (exits current_room)}
            let changed_door = replaceObject removed_exit open_door closed_door
            (newState state changed_door, "Closed the door. Wouldn't want it to get cold!")
      | objectHere obj (getRoomData state)
        && location_id state == "car park"
        && obj == open_door = (state, "You can't seem to close it.")
      | open_state obj == Just Closed            = (state, "That's already closed!")
      | otherwise                                = (state, "You can't close that!")


{- Given an object and a room, return the room without that object in it -}

removeObject :: Object -> Room -> Room
removeObject o rm = rm {objects = filter (/=o) (objects rm)}

{- Given an object and a room description, return a new room description
   with that object added -}

addObject :: Object -> Room -> Room
addObject o rm = rm {objects = (objects rm)++[o]}

{- Returns the object for the given object string if it is in the room or the inventory-}
--RECURSIVE
getObject :: String -> GameData -> Maybe Object
getObject s gd = findObj s gd (objects (getRoomData gd) ++ (inventory gd))
    where
      findObj s gd [] = Nothing
      findObj s gd (x:xs) | Just (obj_name x) == (transformObject s gd) = Just x
                          | otherwise = findObj s gd xs

{- Add an object to the player's inventory -}

addInv :: GameData -> Object -> GameData
addInv gd obj = gd {inventory = (inventory gd) ++ [obj]}

{- Remove object from inventory Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> Object -> GameData
removeInv gd obj = gd {inventory = filterInv (inventory gd) obj}
                   where
                     filterInv inv obj = filter (\x -> x /= obj) inv

{- Replaces the first object in the inventory with the second -}

replaceInv :: GameData -> Object -> Object -> GameData
replaceInv gd obj0 obj1 = addInv (removeInv gd obj0) obj1

replaceObject :: Room -> Object -> Object -> Room
replaceObject rm obj0 obj1 = addObject obj1 (removeObject obj0 rm)

{- Does the inventory in the game state contain the given object? -}

carrying :: GameData -> Object -> Bool
carrying gd obj = elem obj (inventory gd)

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Direction -> GameData -> (GameData, String)
go dir state = do let curr_room = getRoomData state
                  processExits (exits curr_room)
                      where
                        processExits [] | dir == Up   = (state, "You jump and down like a fool")
                                        | dir == Down = (state, ("You claw at the ground like a madman,"
                                                              ++ " but to no avail"))
                                        | otherwise   = (state, "You look for an exit but can't find one")
                        
                        processExits (x:xs) | (exit_dir x) == dir = (new_state x, getString (new_state x))
                                            | otherwise = processExits xs
                        
                        new_state x = state {location_id = room x}

give :: Object -> Object -> GameData -> (GameData, String)
give obj reciever state
    | carrying state obj
      && obj == fiver 
      && reciever == bum =
        (newState no_fiver_state
                  (removeObject bum (getRoomData no_fiver_state)),
         "The bum takes the money, grunts and hobbles off.")
    | reciever == func_god && carrying state obj =
        if obj == paper
            then do
                let removed_state = (removeInv state paper){given_code = True}
                let message = ("The God says \"Ah! My program! Whitespace is one of my favourite languages."
                            ++ " Thank you very much.\" and takes the code.")
                addKey removed_state message
            else if obj == haddock
                then do
                    let removed_state = (removeInv state haddock){given_pet = True}
                    let message = "The God says \"I promise to love it forever!\""
                               ++ " and takes the pet haddock."
                    addKey removed_state message
                else if obj_name obj == Curryobj && awesome obj == True
                    then do
                        let removed_state = (removeInv state curryobj){given_food = True}
                        let message = "The God says \"Curry! With ingredientX! My favourite!\""
                                   ++ " and takes the curry."
                        addKey removed_state message
                    else if obj_name obj  == Curryobj
                        then (state, "This curry isn't good enough!")
                        else (state, "I don't want it!")
    | carrying state obj && obj == slash && reciever == mancomb =
        (newState (addInv state iron) (removeObject mancomb (getRoomData state)),
         "Mancomb says \"Ah! A root! I'm afraid all I have to give you in return is this iron."
         ++ " I have no idea why my programmer decided to give me this (he's a bit of a ditz)\""
         ++ " and promptly dissapears. You guess Lucas didn't code this far into the game...")
    | carrying state obj && obj == wheel && reciever == guide_stopped =
        (newState (removeInv state wheel){fixed_wheel = True}
                  ((addExit (getRoomData state) taj_exit)){room_desc = taj_desc},
         "The guide takes the wheel, fixes the rickshaw and says \"Okay, ready to go\"")
    | otherwise = (state, "Why would you want to do that?")
    where
        no_fiver_state = removeInv state fiver
        addKey state message =
            if (given_code state) && (given_food state) && (given_pet state)
                then (((addInv state encryption_key){has_recursion = True}),
                      (message ++ "\n\"You have proved your worth, you must next fight the Java-monster;"
                               ++ " take this, the power of recursion! If you recurse the Java-monster,"
                               ++ " it'll have no chance!\nNow take this key and save haskell!\""))
                else (state, message)

talk :: Action
talk obj state
    | obj == prisoners =
        (state, ("The prisoners thank you for freeing them, and compliment you on your excellent"
              ++ " use of filter!"))
    | obj == haddock   = (state, ("You attempt to have a conversation, but it isn't very talkative."))
    | obj == python && not (has_filter state) =
        ((state{ has_filter = True }), ("You tell Python of the problem; to help you, he teaches you"
                                     ++ " the power of filter. You can use this to bring down even"
                                     ++ " the most powerful defenses!"))
    | obj == python = (state, "Python tells you that he has assisted you as much as he can.")
    | obj == programmers = (state, "The programmers refuse to reveal information!")
    | obj == bum = (state, "The bum shouts nonsense at you")
    | obj == zombie = (state, "The zombie mumbles a question about method encapsulation")
    | obj == lucas && debugged state =
        (newState (addInv state paper) (removeObject lucas (getRoomData state)),
         ("Lucas says \"Wow, thanks for getting that bug out for me. I'm afraid that my games"
       ++ " aren't doing very well commercially, so all I can give you is this piece of paper."
       ++ " I'm going to go into my development environment now. Bye!\" and gets sucked into the wall."))
    | obj == lucas =
        if spoken_lucas state
            then (state, "Please help me iron out these bugs")
            else (state {spoken_lucas = True},
                  ("Hi, I'm Lucas. I used to work for Lucasarts back in the day. Now I just sit and try"
                ++ " and write adventure games of my own; they're really original! I'm having some"
                ++ " trouble with bugs in one of the programs through the portals. Maybe you can help"
                ++ " me iron them out? I think I left some useful things lying around..."))
    | obj == mancomb = if spoken_mancomb state
        then (state, "Please, I really need that root!")
        else (state {spoken_mancomb = True},
              ("Hello there! My name is Mancomb Seepgood. I'm in a bit of a pickle. I need root beer"
            ++ " to defeat the evil werewolf pirate, LeThrow, but I can't seem to find any root anywhere."
            ++ " If you get some, let me have it please."))
    | obj == func_god && (given_code state) && (given_food state) && (given_pet state) =
        (state, "You have proved your worth, now run along and save Haskell!")
    | obj == func_god = do
        let poss_items = [("Some source code\n", given_code state),
                          ("A pet\n", given_pet state),
                          ("Some food\n", given_food state)] 
        let items = [x | (x, y) <- poss_items, y == False]
        let message = listToString items
        (state, "The god says \"You still need to bring me:\n" ++ message)
    | obj == native
    && spoken_native state
    && carrying state cookbook
    && carrying state pan
    && carrying state rice 
    && carrying state herbs =
        (state, ("The native says \"I see that you have recovered my supplies, fantastic! I have created"
              ++ " a portal to teleport you back home.\""))
    | obj == native && spoken_native state = (state, "The native says \"" ++ still_need ++ "\"")
    | obj == native
    && carrying state cookbook
    && carrying state pan
    && carrying state rice 
    && carrying state herbs =
        (update_india,
         ("The native says \"Greetings traveller. I understand that you have come in search of"
       ++ " ingredient X? You have come to the right place. I see that you have also recovered"
       ++ " my supplies, fantastic! I have created a portal to teleport you back to your home."
       ++ " Here is IngredientX\" He hands you a funny shaped object."))
    | obj == native =
        (update_india, 
         ("The native says \"Greetings traveller. I understand that"
       ++ " you have come in search of ingredient X? You have come to the right place."
       ++ " Unfortunately, my cookbook and other supplies have been stolen. You should"
       ++ " recover them from the slums in the west before going back home. " ++ still_need
       ++ "Here is IngredientX\""))
    | obj == guide_init && not (spoken_guide state) =
        (newState state{spoken_guide = True} (addExit (getRoomData state) indiaentrance_exit),
         ("The man says \"Hello my friend! I was told you were coming. Come, I will take you to"
       ++ " your destination\""))
    | obj == guide_init = (state, "I'm ready when you are")
    | obj == guide_stopped && not (fixed_wheel state) =
        (state,
         ("He says \"The wheel for my rickshaw is broken. You'll need to find another one if"
       ++ " you want to keep going\""))
    | obj == guide_stopped = (state, "Hello!")
    | obj == thief = (state,"I'm sorry! I won't steal again!")              
    | obj == elf = (state, ("So Larry's hungry, eh? Well he'll only eat food which contains IngredientX;"
                         ++ " a rare vegetable from India. Unfortunately I'm all out, so you'll need to"
                         ++ " go there yourself. I've re-coded your house with a portal to India, you"
                         ++ " just need to commit the changes on the console over there."))
    | otherwise = (state, "Wow, you really are wrong in the head")
    where
        update_india = do
            let gd = (addInv state ingreX){spoken_native = True, completed_india=True}
            let rm = getRoom (world gd) "bedroom"
            let add_exit2 = addExit rm bedroom_exit
            updateRoom gd "bedroom" add_exit2
        listToString xs = foldr (++) "" xs
        still_need = do
            let poss_items = [("Some rice\n", carrying state rice),
                              ("Some herbs\n", carrying state herbs),
                              ("A pan\n", carrying state pan),
                              ("A cookbook\n", carrying state cookbook)]
            let items = [x | (x, y) <- poss_items, y == False]
            "You still need:\n" ++ listToString items ++ "\n"
                                     
                       
cook :: Object -> Object -> GameData -> (GameData, String)
cook obj0 obj1 state
    | (obj0 == paste || obj1 == paste) && (obj0 == coconut || obj1 == coconut) 
    && got_pan =
        (addInv no_paste sauce, "Wow, some curry sauce. you're almost there") 
    | (obj0 == rice || obj1 == rice) && (obj0 == sauce || obj1 == sauce) && got_pan =
        (addInv no_sauce curryobj, "Finally, you've made myself a curry dish") 
    | (obj0 == curryobj || obj1 == curryobj) && (obj0 == ingreX || obj1 == ingreX) =
        (with_curry, "Wow, the ultimate curry!")
    | person obj0 || person obj1 = (state, "You sick, sick person")
    | otherwise = (state, "You can only cook certain ingredients, you should find and read a cooking book")
    where
        no_paste = removeInv no_coco paste
        no_coco = removeInv state coconut
        no_sauce = removeInv no_rice sauce
        no_rice = removeInv state rice
        no_curry = removeInv state curryobj
        with_curry = addInv no_curry curryobj{awesome = True, obj_desc = xcurry_desc}
        got_pan = carrying state pan
                       

transformDirection :: String -> Maybe Direction
transformDirection s | s == "n"  || s == "north"     = Just North
                     | s == "ne" || s == "northeast" || s == "north-east" = Just Northeast
                     | s == "e"  || s == "east"                           = Just East
                     | s == "se" || s == "southeast" || s == "south-east" = Just Southeast
                     | s == "s"  || s == "south"                          = Just South
                     | s == "sw" || s == "southwest" || s == "south-west" = Just Southwest
                     | s == "w"  || s == "west"                           = Just West
                     | s == "nw" || s == "northwest" || s == "north-west" = Just Northwest
                     | s == "u"  || s == "up"                             = Just Up
                     | s == "d"  || s == "down"                           = Just Down 
                     | otherwise                                          = Nothing
              
study :: Object -> GameData -> (GameData,String)
study obj state
    | obj == cookbook = (state,bookdata)
    | otherwise = (state, "Can't study that")
    where 
        bookdata =
            "\nXtreme Curry Recipe\n" ++
            "\nTo make curry make sure you are in a suitable place!\n" ++
            "Start off by making sure you have all the required equipment: \n"++
            "1) Herbs \n2) A pan\n3) A herb grinder\n4) A coconut\n5) Some rice\n6)IngredientX\n"++
            "The first part is to make a curry paste, to do this grind up the herbs.\n"++
            "Now to make sauce...\nCook the paste with the coconut (make sure you have a pan)\n"++
            "This will result in a lovely rich sauce, cook the rice with this sauce and you're done" ++
            " making your first curry dish\n"++
            "Not, if you want to make an Xtreme curry, simply cook the curry with IngredientX. This" ++
            " will result in the most awesome curry known to man\n" ++
            "Grinding isn't hard, all it requires is grinding a single object so not to confuse the" ++
            " beginner chef\n"++
            "Cooking requires a pan and two ingredients, you can cook x with y.\n"++
            "Simples!\n"

{- This function is nowhere near exhaustive and is just a small fallback -}                      
use :: Object -> Object -> GameData -> (GameData, String)    
use obj0 obj1 state | obj0 == coffeepot
                      && obj1 == mug = pour coffeepot state
                    | obj0 == crowbar
                      && obj1 == zombie = hit [zombie, crowbar] state
                    | obj0 == keycard
                      && obj1 == closed_door = open [closed_door, keycard] state
                    | obj0 == crowbar
                      && obj1 == closed_door = open [closed_door, crowbar] state
                    | obj0 == shotgun
                      && obj1 == closed_door = open [closed_door, shotgun] state
                    | obj0 == iron
                      && obj1 == cockroach = ironCmd cockroach state
                    | obj0 == shotgun
                      && obj1 == zombie_horde = hit [zombie_horde, shotgun] state
                    | obj0 == pickaxe
                      && obj1 == code = hit [code, pickaxe] state
                    | obj0 == password
                      && obj1 == closed_door = open [closed_door, password] state
                    | otherwise = (state, "Try a more specific command")

{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

   Hints: you will need to take care to update things in the right order here!
    * create a new state with the updated inventory (use 'objectData')
    * create a new room without the object (use 'removeObject')
    * update the game state with this new room in the current location
      (use 'location_id' to find where the player is)
-}

get :: Action
get obj state | objectHere obj (getRoomData state) = if gettable obj
                                                     then update obj state
                                                     else (state, "Can't pick that up")
              | otherwise = (state, "Object not found in current room")
              where
                update :: Action
                update obj state = do let new_state = addInv state obj
                                      let new_room = removeObject obj (getRoomData new_state)
                                      (updateRoom new_state (location_id new_state)  new_room,
                                       "Picked up object")
                                                    
 
{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}

put :: Action
put obj state | carrying state obj = update obj state
              | otherwise = (state, "Object not in inventory")
              where
                update :: Action
                update obj state = do let new_state = removeInv state obj
                                      let new_room = addObject obj (getRoomData new_state)
                                      (updateRoom new_state (location_id new_state) new_room,
                                       "Dropped object")

{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: Action
examine obj state = (state, obj_desc obj)
                    

{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: Action
pour obj state | obj == coffeepot
                 && carrying state obj
                 && carrying state mug =
                    ((replaceInv (replaceInv state mug fullmug) coffeepot emptypot){poured = True},
                     "Poured coffee")
               | obj == emptypot       = (state, "There's no coffee left...")
               | obj == fullmug        = (state, "Why would you waste perfectly good coffee?")
               | otherwise             = (state, "You want me to pour what?!")
                             

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!

   The correct object will be passed to this method so you don't need to worry about that.
   All you need to check is that the object is the fullmug and that it's in your inventory (carrying).

   You should then update the inventory to change the fullmug to just a mug. This involves first
   removing the fullmug from the inventory, then adding the mug to the inventory, look at pour for
   an example of how to do this
-}

drink :: Action
drink obj state | obj == fullmug 
                  && carrying state obj =
                      (state {caffeinated = True,inventory = inventory (replaceMug state)},
                       "Caffeinated!!")
                | obj == coffeepot
                  && poured state = (state, "Try drinking the mug")
                | otherwise = (state, "I find it hard to drink that") 
                              where 
                                replaceMug state = replaceInv state fullmug mug

eat :: Action
eat obj state
    | obj == zombie =
        (state{finished = True}, "You have a brain eating contest with the zombie. You lose.")
    | obj == func_god || obj == lucas || obj == mancomb || obj == bum =
        (state, "I really don't think he'd appreciate that")
    | (obj == pickaxe || obj == hockey_stick || obj == shotgun || obj == crowbar) && carrying state obj =
        (state, "You attem- wait, what?! No.")
    | obj == herbs && carrying state obj = (state, "You want to save them for later")
    | obj == coconut && carrying state obj = (state, "You want to save it for later")
    | obj == haddock && carrying state obj = (state, "Why would you want to eat the cute little fishy?")
    | carrying state obj = (state, "You don't think that it would be very tasty")
    | person obj = (state, "You don't think they would appreciate you nibbling on them")
    | otherwise = (state, "You don't have that object")

{- 
Allow the user to end the game by killing themselves with an object
-}                    
suicide :: [Object] -> GameData -> (GameData, String)
suicide [] state =
    (state{finished = True}, "You are fed up of this world and bludgeon yourself to death with your shoe.")
suicide [obj] state
    |(obj == mug  && has_obj) || (obj == fullmug && has_obj) =
        (dead_state,
         "You beat yourself over the head with a mug until you kill your one remaining brain cell.")
    | obj == shotgun && has_obj =
        (dead_state, "You blow your head off. Nice going.")
    | obj == hockey_stick && has_obj =
        (dead_state, "You beat yourself to death with the hockey stick.")
    | obj == pickaxe && has_obj =
        (dead_state, "You disembowl yourself with a pickaxe. Congratulations, you are a mental case.")
    | obj == matches && has_obj =
        (dead_state, "You light yourself on fire, and burn yourself to a crisp. What did you expect?")
    | obj == crowbar && has_obj =
        (dead_state, ("You commit atrocities to yourself with a piece of metal."
                   ++ " I don't want to you play me anymore."))
    | has_obj = (state, "You think another object would be more effective")
    | person obj = (state, "Eh?")
    | otherwise = (state, "You don't have that object")
    where 
        dead_state = state {finished=True}
        has_obj = carrying state obj

light :: Object -> GameData -> (GameData, String)
light obj state
    | obj == matches && location_id state == "subkitchen" = do
        let new_desc = (getRoomData state){room_desc = lights_desc}
        let added_exit = addExit new_desc lights_exit
        let added_objects = added_exit{objects = lights_obj}
        (newState state added_objects, "Let there be light!")
    | obj /= matches =
        (state, "I should really take away your fire making capabilities if you're going to be an idiot.")
    | otherwise = (state, "WOW FIRE, mhh this is useless")

watch :: Object -> GameData -> (GameData, String)
watch obj state
    | obj == dvd && location_id state == "cinema" =
        (state, "You Don't believe it!!!\nYou've been carrying this DVD for no reason. (Moan)")
    | obj /= dvd = (state, "You stare at it lovingly. You are special.")
    | obj == dvd =
        (state,"The cover seems funny, if you find a machine to play it on, you could actually watch it")
    | otherwise = (state, "What?")

grind :: Object -> GameData -> (GameData, String)
grind obj state
    | obj == herbs && location_id state == "subkitchen" =
        (addInv no_herb paste,"The grinder made a nice paste out of the herbs")
    | otherwise = (state,"Hahaha no.")
    where
        no_herb = removeInv state herbs

hit :: [Object] -> GameData -> (GameData, String)
hit [target, object] state
    | target == programmers && (object == crowbar || object == hockey_stick) =
        (state, ("You knock one of the programmers out with a blow to the head. " ++ programmer_message))
    | target == programmers && object == shotgun =
        (state, ("You shoot one of the programmers in the leg. " ++ programmer_message))
    | target == zombie && object == crowbar = do
        let current_room = getRoomData state
        let without_zombie = removeObject zombie current_room
        let added_exit = addExit without_zombie cycle_path_exit
        let changed_desc = added_exit{room_desc = cycle_path_new_desc}
        (newState state changed_desc, "You smash the zombie with the crowbar and he disintegrates")
    | target == zombie_horde && object == shotgun && location_id state == "golfcourse" = do
        let current_room = getRoomData state
        let without_zombies = removeObject zombie_horde current_room
        let added_exit_1 = addExit without_zombies golfcourse_exit_1
        let added_exit_2 = addExit added_exit_1 golfcourse_exit_2
        let changed_desc = added_exit_2{ room_desc = golfcourse_new_desc }
        let updated_state = newState state changed_desc
        (updated_state, "You fire several shells into the zombie horde, leaving no survivors.")
    | target == zombie_horde && object == shotgun && location_id state == "science_area" = do
        let science_removed_zombies = removeObject zombie_horde (getRoomData state)
        let science_added_exit = addExit science_removed_zombies science_area_exit
        let science_changed_desc = science_added_exit{ room_desc = science_area_new_desc }
        let state_updated_science = newState state science_changed_desc
        let carpark_removed_zombies = removeObject zombie_horde (getRoom (world state_updated_science)
                                                                         "car park")
        let carpark_added_exit = addExit carpark_removed_zombies car_park_exit_2
        let carpark_changed_desc = carpark_added_exit{ room_desc = car_park_new_desc }
        let new_state = updateRoom state_updated_science "car park" carpark_changed_desc
        (new_state, "You fire several shells into the zombie horde, leaving no survivors.")
    | target == zombie && object == hockey_stick =
        (state, "The zombie reels back a bit, but doesn't seem to be too phased")
    | target == zombie_horde && (object == hockey_stick || object == crowbar) =
        (state,
         "You can't run into a horde of zombies with that puny thing, you need" ++
         " something which packs a bit more punch!")
    | target == func_god  = (state, "He turns you into a newt, but you get better")
    | target == guide_init || target == guide_stopped =
        (state,"You can't bring youself to attack such a happy person.")
    | target == native = (state,"You have a feeling he'd work his voodoo on you, so stop.")
    | target == code && object == pickaxe =
        (newState state (addObject slash (removeObject code (getRoomData state))),
         "The code crumbles away, but seems to leave something useful behind.")
    | target == code && object == shotgun = (state, "You don't want to completely destroy it!")
    | target == code       = (state, "Your going to need something sharper than that")
    | target == cockroach && object == iron =
        (newState state{debugged = True} (removeObject cockroach (getRoomData state)), "The bug melts away")
    | target == cockroach  = (state, "He seems to be impervious to you attack")   
    | person target        = (state, "That's not very nice of you")
    | otherwise            =
        (state, "After a while, you realise that you are being randomly destructive for fun, and stop.")

hit [target] state
    | target == programmers    = (state, ("You punch one of the programmers in the face. "
                                          ++ programmer_message))
    | target == zombie         = (state, "You hurt your hand. Perhaps try using something heavier.")
    | target == zombie_horde   = (state, "You can't go near that with nothing but your bare hands!.")
    | target == func_god       = (state, "He turns you into a newt, but you get better")
    | target == guide_init || target == guide_stopped =
        (state, "You can't bring youself to hit such a happy person.")    
    | target == native         = (state,"You have a feeling he'd work his voodoo on you, so stop.")
    | person target            = (state, "That's not very nice of you")
    | otherwise                = (state, "You hurt your hand.")

     
programmer_message = "At the threat of violence; the other programmers cave in and reveal to you"
                  ++ " that their leader, the terrible Java-monster, has hidden something in the"
                  ++ " castle, and it is protected by a mysterious magical word."
{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "Your pockets are as empty as your head"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         --RECURSIVE
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")

xyzzy :: Command
xyzzy state
    | (location_id state == "castle") && not (xyzzy_used state) =
        (new_state, "An object appears before you...")
    | otherwise = (state, "Nothing happens.")
    where
        new_state = newState (state{ xyzzy_used = True })
                             ((addObject password (getRoomData state)){room_desc = castle_new_desc})

plugh :: Command
plugh state = (state, "A hollow voice says \"Stop messing around and mark the project!\"")

ironCmd :: Action
ironCmd obj state
    | carrying state iron && obj == cockroach =
        (newState state{debugged = True} (removeObject cockroach (getRoomData state)), "The bug melts away")
    | carrying state iron = (state, "You can't iron that!")
    | person obj = (state, "You think they look fine without a burn mark")
    | otherwise = (state, "You don't even have an iron, do you plan to use your thick skull instead?")

push :: Action
push obj state
    | obj == button && not (pressed_button state) = do
        let kit = getRoom (world state) "kitchen"
        let added_exit = addExit kit kitchen_exit
        let pressed = state{pressed_button = True}
        (updateRoom pressed "kitchen" added_exit, "Committed changes")
    | obj == button = (state, "Nothing happens")
    | person obj = (state, "I can see why you have no friends...")
    | otherwise = (state, "You can't push that")
