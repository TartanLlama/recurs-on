module Main where

import World
import Actions

import Control.Exception
import Data.Char
import System.IO
import System.Exit
import System.Directory




-- isNothing - helper method to determine if a Maybe something is Nothing.
isNothing :: Maybe a -> Bool
isNothing value =
    case value of
        Just a_value -> False
        Nothing -> True

-- main - program start/initialisation.
main :: IO ()
main = do putStrLn "Welcome to recurs!on. Go to your lecture, you lazy slob."
          putStrLn "Type \"help\" for a list of commands.\n"
          putStrLn (getString initState)
          repl initState
          return ()




-------------------------------------------------------------------------------
-- processCommands code - processes a list of commands in sequence ------------
-------------------------------------------------------------------------------

-- Given a game state, and a Maybe list of commands, return the new state after
-- processing each command in turn, and a message for the user.
-- If the Maybe list is nothing, returns the current state and a message saying
-- "I don't understand"
processCommands :: Maybe [Cmd] -> GameData -> (GameData, String)
-- Case where there is one command in the list:
processCommands (Just [command]) state =
    actions state command
-- Case where the list is Nothing:
processCommands Nothing state =
    (state, "I don't understand")
-- Case where there is a list of multiple commands:
--RECURSIVE
processCommands (Just command_list) old_state =
    -- Process the rest (tail) of commands using the new state from processing
    -- the first command in the list.
    processCommands (Just (tail command_list)) new_state
    where
        -- Get the new state from processing only the first command in the list
        (new_state, _) = processCommands (Just [head command_list]) old_state

-------------------------------------------------------------------------------
--End of processCommands code -------------------------------------------------
-------------------------------------------------------------------------------




-------------------------------------------------------------------------------
-- getCmd code - turns a list of strings into a list of commands --------------
-------------------------------------------------------------------------------

-- getCmd - gets a list of "Maybe" valid commands or "Nothing" if one or more
-- commands are invalid.
getCmd :: [String] -> GameData -> Maybe [Cmd]
getCmd [] _ = Nothing
getCmd word_list state =
    getCommands (split word_list "then") state
    where
        -- getCommands - to get a Cmd list from a list of components.
        -- Gives "Just" a list of commands, or "Nothing", if one or more
        -- commands are invalid.
        getCommands :: [[String]] -> GameData -> Maybe [Cmd]
        getCommands [] state = Just []
        getCommands components state
            | isNothing current = Nothing
            | isNothing rest = Nothing
            | otherwise = Just (cmd : cmds)
            where
                -- Creates a Maybe executable command from the first component
                current = getCmdFunction (head components) state
                -- Extracts the command from the "Maybe" - only used when it's
                -- verified to not be "Nothing".
                Just cmd = current
                -- We need the new state in order to get future commands
                (new_state, _) = processCommands (Just [cmd]) state
                -- getCommands for the rest of the component list.
                rest = getCommands (tail components) new_state
                -- Extracts commands from the "Maybe"
                Just cmds = rest

split :: [String] -> String -> [[String]]
split [] delim = []
split str_lst delim =
    getComponents str_lst delim
    where
        -- getComponents - to turn a list of strings into a list of lists of
        -- strings, separated by "then".
        getComponents :: [String] -> String -> [[String]]
        getComponents str_lst delim
            -- An empty word list becomes an empty list
            | str_lst == [] = []
            -- Parse the words for one command, add it to the list of
            -- components for the rest of the list
            | not (head str_lst == delim) = current_cmd : (getComponents
                                                           rest delim)
            -- If the first word is "then", get components from the rest of
            -- the word list, i.e. ignore the "then".
            | otherwise = getComponents (tail str_lst) delim
            where
                -- current_cmd is the first component in the list.
                -- rest is the list of words after the "then" which terminated
                -- the first command.
                (current_cmd, rest) = getFirstComponent str_lst delim
        
        -- getFirstComponent - to get (from a list of strings) a list of all
        -- strings before the first occurance of "then", and a list of all
        -- strings after.
        getFirstComponent :: [String] -> String -> ([String], [String])
        getFirstComponent str_lst delim
            -- If the word list is empty
            | str_lst == [] = ([], [])
            -- If we have found a "then", this is the end of the current
            -- component, and the rest of the list is simply the tail (i.e.
            -- missing out "then".
            | head str_lst == delim = ([], (tail str_lst))
            -- The first word in the list is not "then", so add this to
            -- next_words, which comes from calling this function on the
            -- tail of the word list.
            | otherwise = ((head str_lst : next_words), rest)
            where
                -- Call the same function on the rest of the list.
                (next_words, rest) = getFirstComponent (tail str_lst) delim

-------------------------------------------------------------------------------
--End getCmd code -------------------------------------------------------------
-------------------------------------------------------------------------------




-------------------------------------------------------------------------------
-- getCmdFunction - gets the function for a single command --------------------
-------------------------------------------------------------------------------

-- getCmdFunction - gets a Maybe Cmd from a list of strings
getCmdFunction :: [String] -> GameData -> Maybe Cmd

getCmdFunction [cmd, arg0, arg1, arg2] state
    | cmd == "use" && (arg1 == "with" || arg1 == "on")
        = doubleObjCmd Use arg0 arg2 state
    | cmd == "open" && (arg1 == "with" || arg1 == "using")
        = doubleObjCmd OpenWith arg0 arg2 state
    | (cmd == "attack" 
        || (cmd == "hit"
            && (transformObject arg2 state) /= Just Shotgun)
        || (cmd == "shoot"
            && (transformObject arg2 state) == Just Shotgun))
      && arg1 == "with"
            = doubleObjCmd HitWith arg0 arg2 state
    | cmd == "give" && arg1 == "to"
        = doubleObjCmd Give arg0 arg2 state
    | cmd == "cook" && arg1 == "with" = doubleObjCmd Cook arg0 arg2 state 
    | cmd == "commit" && arg0 == "suicide" && arg1 == "with"  = case getObject arg2 state of
                                                                  Just obj -> Just (SuicideWith obj)
                                                                  Nothing  -> Nothing
    | otherwise
        = Nothing
    where
        doubleObjCmd cmd arg0 arg1 state =
            case (getObject arg0 state, getObject arg1 state) of
                (Just obj0, Just obj1) -> Just (cmd obj0 obj1)
                (_, _)                 -> Nothing

getCmdFunction [cmd, arg0, arg1] state
    | cmd == "talk" && arg0 == "to" || arg1 == "with" = case getObject arg1 state of
                                                          Just obj -> Just (Talk obj)
                                                          Nothing  -> Nothing

-- If the command is a command and argument:
getCmdFunction [cmd,arg] state
    | cmd == "go"                                    = checkDirection arg
    | cmd == "pour"                                  = objCmd Pour arg state
    | cmd == "filter"                                = objCmd Filter arg state
    | cmd == "recurse"                               = objCmd Recurse arg state
    | cmd == "light" || cmd == "strike"              = objCmd Light arg state
    | cmd == "watch"                                 = objCmd Watch arg state
    | cmd == "grind"                                 = objCmd Grind arg state
    | cmd == "study"                                 = objCmd Study arg state 
    | cmd == "drink"                                 = objCmd Drink arg state
    | cmd == "commit" && arg == "suicide"            = Just Suicide  
    | cmd == "examine" || cmd == "x" || cmd == "read"= objCmd Examine arg state
    | cmd == "drop"    || cmd == "d"                 = objCmd Drop arg state
    | cmd == "open"    || cmd == "o"                 = objCmd Open arg state
    | cmd == "close"   || cmd == "c"                 = objCmd Close arg state
    | cmd == "push"    || cmd == "press"             = objCmd Push arg state
    | cmd == "attack" 
        || (cmd == "hit"
            && (transformObject arg state) /= Just Shotgun)
        || (cmd == "shoot"
            && (transformObject arg state) == Just Shotgun) = objCmd Hit arg state
    | cmd == "take"    || cmd == "get" || cmd == "g" = objCmd Get arg state
    | cmd == "play"                                  = objCmd Play arg state
    | cmd == "eat" || cmd == "nom"                   = objCmd Eat arg state
    | cmd == "iron"                                  = objCmd IronCmd arg state
    | otherwise                                      = Nothing
        where
            objCmd cmd arg state = case getObject arg state of
                Just obj -> Just (cmd obj)
                Nothing  -> Nothing

-- If it is a one word command:
getCmdFunction [cmd] state
    | cmd == "xyzzy"                                     = Just Xyzzy
    | cmd == "plugh"                                     = Just Plugh
    | cmd == "l"  || cmd == "look"                       = Just Look
    | cmd == "q"  || cmd == "quit" || cmd == "exit"      = Just Quit
    | cmd == "i"  || cmd == "inv"  || cmd == "inventory" = Just Inventory
    | cmd == "help"                                      = Just Help
    | otherwise                                          = checkDirection cmd

-- If the command is an invalid format:
getCmdFunction _ state =
    Nothing

-- To check if it's a valid direction and return Command: go direction
checkDirection :: String -> Maybe Cmd
checkDirection direction = case transformDirection direction of
                                         Just dir -> Just (Go dir)
                                         Nothing -> Nothing

-------------------------------------------------------------------------------
--End getCmdFunction code -----------------------------------------------------
-------------------------------------------------------------------------------




-------------------------------------------------------------------------------
-- save and load functions - for the user to save progress --------------------
-------------------------------------------------------------------------------

-- Saves the game data to "recursion_saves/specified_filename.rsav"
save :: GameData -> String -> IO ()
save state filename = do
    createDirectoryIfMissing True "recursion_saves"
    -- Try to write data, call "failure" if an exception is caught
    catch
        -- Try to write data:
        (write_data ("recursion_saves/" ++ filename ++ ".rsav") state)
        -- If an exception is caught:
        failure
    return ()
    where
        -- For writing data to the file
        write_data :: String -> GameData -> IO ()
        write_data filename state = do
            -- Call show to serialize game data as a string.
            let data_str = show state
            writeFile filename data_str
            putStrLn "Wrote data succesfully :)"
            return ()
        -- To be called if write fails
        failure :: IOError -> IO ()
        failure e = do
            putStrLn "Could not write data :("

-- Loads game data from a file "recursion_saves/specified_filename.rsav"
load :: GameData -> String -> IO GameData
load state filename = do
    createDirectoryIfMissing True "recursion_saves"
    -- Try to read data, call "failure" if an exception is caught
    new_state <- catch
        -- Try to read data:
        (read_data ("recursion_saves/" ++ filename ++ ".rsav"))
        -- If an exception is caught:
        failure
    -- New state if load was succesful, old if not.
    return new_state
    where
        -- read_data - to read in game data from a properly formatted file.
        read_data :: String -> IO GameData
        read_data filename = do
            data_str <- readFile filename
            -- read the string from the file as GameData
            let new_state = (read data_str)::GameData
            putStrLn "Read data succesfully :)"
            return new_state
        -- failure - to be called if read operation fails.
        failure :: IOError -> IO GameData
        failure e = do
            putStrLn "Could not read data :("
            -- Return old state
            return state

-------------------------------------------------------------------------------
--End of save/load code -------------------------------------------------------
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- Recursive game loop --------------------------------------------------------
-------------------------------------------------------------------------------

-- Game loop
repl :: GameData -> IO GameData
repl state | finished state = return state
repl state = do
    putStrLn ""
    putStr "> "
    hFlush stdout
    -- Get command from user
    input_string <- getLine
    putStrLn "\n"
    -- Turn command into a list of strings for easier processing.
    let user_commands = words (map toLower input_string)
    if user_commands == []
       then do putStrLn "You stand about like an idiot for a while"
               repl state
       else
       -- Check if the user is saving/loading the game
       -- This requires file IO, so must be handled separately to normal commands
        if (length user_commands) == 2
                && (head user_commands) == "save"
                || (head user_commands) == "saveas"
                || (head user_commands) == "savegame"
            then do
                save state (head (tail user_commands))
                repl state
            else if (length user_commands) == 2
                    && (head user_commands) == "load"
                    || (head user_commands) == "loadgame"
                then do
                    new_state <- load state (head (tail user_commands))
                    repl new_state
                else do
                    -- Turn the list of strings into a list of commands,
                    -- using "then" to separate.
                    let command_list = getCmd user_commands state
                    -- Process the command and update the state.
                    let (new_state, msg) = processCommands command_list state
                    putStrLn msg
                    if getRoomData new_state == entrance
                       && not (visited_entrance new_state)
                        then do putStrLn god_message
                                repl new_state{visited_entrance = True}
                        else if (won new_state)
                            then do
                                putStrLn win_message
                                return new_state
                            else
                                -- Game has not been won, so repeat with the new state.
                                repl new_state

-------------------------------------------------------------------------------
-- End of main game loop ------------------------------------------------------
-------------------------------------------------------------------------------

win_message = "You enter Haskell's dungeon, to find him tied up and unable to move. "
           ++ "You quickly untie him, and recount the tales of your adventure. "
           ++ "Together, you use the power of functional programming to undo all of"
           ++ " the damage caused by the cult, and cure the inexplicable"
           ++ " (completely unrelated) zombie uprising."
           ++ "\nCongratulations, you've won the game!"

god_message = "\nYou instantly realise that something is wrong. The walls are"
           ++ " warped into impossible shapes and seem to slide around the"
           ++ " room, the doors are all in the wrong place and seem to be"
           ++ " pulsating and there is a large, blue man in the centre of it"
           ++ " all.\n\nThe man approaches you and says\n\"I am the God of"
           ++ " Functional Programming! My name is Larry. Your best friend,"
           ++ " Haskell has been imprisoned by an evil cult of object"
           ++ " orientated programmers! They've also messed around with the"
           ++ " code for the building and unleashed a terrible Java monster!\n"
           ++ "I shall allow you to try and save your friend if you bring to"
           ++ " me these items three:\n"
           ++ "A pet, as it gets lonely being a God\n"
           ++ "Some food, but I'm a very picky eater. Talk to Bob to the west"
           ++ " for details.\n"
           ++ "Some source code, I wrote a really important program, but I"
           ++ " can't seem to find it.\nWell, hop to it!\"" 

