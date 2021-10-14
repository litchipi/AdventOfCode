module Main where

import qualified System.Environment as Environment

test_input = "389125467"
test_result10 = "92658374"
test_result100 = "67384529"

data GameState = 
	GameState { current_cup :: Int }

get_init_state :: String -> GameState
-- TODO		Get initial state from input String
get_init_state input = GameState { current_cup = 0 }

get_result_from_state :: GameState -> String
-- TODO		Get result output from final state
get_result_from_state state = "1234567890"

compute_game :: Int -> GameState -> GameState
compute_game nrounds state = final_state
	where
		-- TODO		Perform modifications on state here
		new_state = GameState { current_cup = (current_cup state) + 1 }
		final_state = compute_game (nrounds - 1) new_state

play_test_game :: Int -> IO ()
play_test_game rounds = putStrLn ("Expected " ++ exp ++ " got " ++ got ++ " -> " ++ result)
	where
		exp = case rounds of 
		  10 -> test_result10
		  100 -> test_result100

		got = get_result_from_state (compute_game rounds (get_init_state test_input))
		result = if got == exp then "Success" else "Failure"

main :: IO ()
main = do {
		x <- Environment.getArgs;
		if length x < 2
			then putStrLn help_msg
		else
			case (x !! 0) of
				"test" -> case (x !! 1) of
					"10" -> play_test_game 10
					"100" -> play_test_game 100
					_ -> putStrLn ("Only 10 or 100 rounds are allowed")

				"input" -> putStrLn ("Result: " ++ (show (get_result_from_state (compute_game 100 (get_init_state (x !! 1))))))

				_ -> putStrLn help_msg
	  }

help_msg = "Arguments expected: \n\
				\    test <10 / 100>: Perform a test on 10 or 100 rounds \n\
				\    input <input>: Perform the computation on the given input"
