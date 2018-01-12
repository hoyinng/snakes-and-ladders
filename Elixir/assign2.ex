defmodule Assign2 do
	#--------------------------
	# ?%%% object(s) %%%?
	# object player  | player object is a map containing {:loc, :powerup}
	# object powerup | powerup object is a map containing{:av=>false,:double=>false, 
	# %%%%%
	# ?%%% maps %%%?
	# player_map contains a map that stores player object , suppose playername is A, {:A=> %{:loc=>0,:powerup=>{:escalator=>false,...so on}}}
	# snake_map|ladder is a map containing the endpoint, it will return an integer 
	# board is a map that given an index of the board, returns a playerName or nil
	# escalator|antivenom|double is a map given an index return true and false
	#-------------------------
	def readFrom(aString) do
		gameState = %{:board=>%{},:snakes=>%{},:powerup=>%{}, :players=>%{}, :ladder=>%{},:diceSeq=>[], :diceLen=>0,:diceCounter=>0, :nPlayers=>0,
			:escalator=>%{}, :antivenom=>%{}, :double =>%{}, :victory=>false}
		gameState = Enum.reduce(String.split(aString,"\n"), gameState, &processLine/2)
		processGame(gameState)
	end
	def processLine(aString,gameState) do
		#Map.put(gameState,:result,gameState[:result] <> aString)
		processCommands(String.split(aString," "), gameState)
	end
	def processCommands(["board"|[x,y]],gameState) do
		gameState = Map.put(gameState, :board_x, String.to_integer(x))
		gameState = Map.put(gameState, :board_y, String.to_integer(y))
		Map.put(gameState, :board, %{})
	end
	def processCommands(["snake"|[x,y]],gameState) do
		temp_map = gameState[:snakes]
		x = (String.to_atom(x))
		y = (String.to_atom(y))
		temp_map = Map.put(temp_map,x,y)
		Map.put(gameState,:snakes,temp_map)
	end
	def processCommands(["ladder"|[x,y]],gameState) do
		x = (String.to_atom(x))
		y = (String.to_atom(y))
		Map.put(gameState,:ladder,Map.put(gameState[:ladder],x,y))
	end
	def processCommands(["powerup", aType| cell_list],gameState ) do
		aType = String.to_atom(aType) 
		aMap = Enum.reduce(cell_list,gameState[aType],fn(x,map)->Map.put(map,String.to_atom(x),true)end )
		Map.put(gameState, aType, aMap)	
	end
	def processCommands(["dice"|x], gameState) do
		gameState = Map.put(gameState,:diceLen, length(x))
		Map.put(gameState, :diceSeq, Enum.map(x,fn(x)->String.to_integer(x) end))
	end
	def processCommands(["players",n],gameState) do
		Map.put(gameState, :nPlayers, String.to_integer(n))
	end
	def processCommands(["turns",x], gameState) do
		Map.put(gameState, :turn, String.to_integer(x))
	end
	def processGame(gameState) do
		#initalize board, player
		gameState=initalizePlayer(gameState)
		if (gameState[:turn] != nil) do
			gameState = Enum.reduce(1.. gameState[:turn], gameState, fn(_,state)->
				state = Enum.reduce(1..state[:nPlayers],state, fn(p,temp_state)->
						if (temp_state[:victory] == false) do
							playerName = << 64+p :: utf8>> ;
							player_key = String.to_atom(playerName)
							seed = Enum.fetch!(temp_state[:diceSeq],temp_state[:diceCounter]);
							if (temp_state[:players][player_key][:double] == true) do
								temp_state = %{temp_state| :players=> Map.put(temp_state[:players],player_key,%{temp_state[:players][player_key]| :double=>false})};
								seed = seed * 2;
							end
							temp_state = movePlayer(temp_state,playerName,seed + temp_state[:players][String.to_atom(playerName)][:loc]) ;
							temp_state= %{temp_state|:diceCounter=> rem(temp_state[:diceCounter]+1,temp_state[:diceLen])};
						end
						temp_state;
					end
					)
				end
			)
		
		end
		gameState 
	end
	#--------Player
	def initalizePlayer (gameState) do
		number_of_player = gameState[:nPlayers]
		gameState =
			Enum.reduce(1..number_of_player, gameState,
				fn(counter,gs)-> 
					playerName = << 64+counter :: utf8>> ;
					current_player = %{:loc=>0,:escalator=>false,:antivenom=>false,:double=>false}
					player_map = Map.put(gs[:players], String.to_atom(playerName), current_player)		#put player into the player_map
					gs = Map.put(gs,:players,player_map)
					gs = movePlayer(gs,playerName,1)
				end
			)
		gameState
	end
	def movePlayer(gameState,playerName,dst) do
		board = gameState[:board]
		player_map = gameState[:players]
		dest_key = String.to_atom(Integer.to_string(dst))
		player_key = String.to_atom(playerName)
		#------SNAKES & LADDERS -----#
		if (gameState[:snakes][dest_key] != nil) do
			if gameState[:players][String.to_atom(playerName)][:antivenom] == true do
				#--STATUS QUO
				gameState = %{gameState| :players=> Map.put(gameState[:players],player_key,%{gameState[:players][player_key]| :antivenom=>false})}
			else
				dk = gameState[:snakes][dest_key]
				dst = String.to_integer(Atom.to_string(dk))
				# I COPY ANY PASTE THIS BECAUSE I RAN OUT OF TIME TO FIGURE OUT WHAT THE PROBLEM IS, PICKERING POWER OUTAGE MADE ME HAVE TO RUSH THIS
				# PLZ SPARE ME ( THERE IS A PROBLEM WHEN SNAKE LAND THE PLAYER BACK, IT WOULDNT CHECK FOR POWER UP, THIS IS CAUSE BY DIFFERENT KEY
				#------PICKUP POWERUPS ------#
				if (gameState[:escalator][dk] != nil) do
					gameState = %{gameState| :players=> Map.put(gameState[:players],player_key,%{gameState[:players][player_key]| :escalator=>true})}
				end 
				if (gameState[:double][dk] != nil) do
					gameState = %{gameState| :players=> Map.put(gameState[:players],player_key,%{gameState[:players][player_key]| :double=>true})}
				end 
				if (gameState[:antivenom][dk] != nil) do
					gameState = %{gameState| :players=> Map.put(gameState[:players],player_key,%{gameState[:players][player_key]| :antivenom=>true})}
				end 
			end
		end 
		#------LADDERS
		if (gameState[:ladder][dest_key] != nil) do
			dest_key = gameState[:ladder][dest_key]
			ladder_endpoint = String.to_integer(Atom.to_string(dest_key))
			if gameState[:players][String.to_atom(playerName)][:escalator] == true do
				dst = dst + 2 * (ladder_endpoint-dst)	
				gameState = %{gameState| :players=> Map.put(gameState[:players],player_key,%{gameState[:players][player_key]| :escalator=>false})}
			else
				dst = ladder_endpoint
			end
		end 
		#------PICKUP POWERUPS ------#
		if (gameState[:escalator][dest_key] != nil) do
			gameState = %{gameState| :players=> Map.put(gameState[:players],player_key,%{gameState[:players][player_key]| :escalator=>true})}
		end 
		if (gameState[:double][dest_key] != nil) do
			gameState = %{gameState| :players=> Map.put(gameState[:players],player_key,%{gameState[:players][player_key]| :double=>true})}
		end 
		if (gameState[:antivenom][dest_key] != nil) do
			gameState = %{gameState| :players=> Map.put(gameState[:players],player_key,%{gameState[:players][player_key]| :antivenom=>true})}
		end 
		#--- WIN CONDITION ----#
		if (dst >= gameState[:board_x] * gameState[:board_y]) do
			index = gameState[:players][String.to_atom(playerName)][:loc]
			index_atom = String.to_atom(Integer.to_string(index))
			#REMOVE WHERE THE PLAYER CURRENTLY IS
			if index != nil and gameState[:board][index_atom] == playerName do
				{_,aMap} = Map.pop(gameState[:board], index_atom)
				gameState = %{gameState|:board=>aMap}	
			end
			#PUT PLAYER TO LAST SPOT
			gameState = putPlayer(gameState,playerName,gameState[:board_x]*gameState[:board_y])
			gameState = Map.put(gameState,:victory,true)
		else
			if (board[dest_key] == nil) do	
				#--Remove the player from the board map in its current location if no one bumped him
				index = gameState[:players][String.to_atom(playerName)][:loc]
				index_atom = String.to_atom(Integer.to_string(index))
				if index != nil and gameState[:board][index_atom] == playerName do
					{_,aMap} = Map.pop(gameState[:board], index_atom)
					gameState = %{gameState|:board=>aMap}	
				end
				#--put the player in destination 
				gameState = putPlayer(gameState,playerName,dst)
			else
				#--FIND WHOEVER IS OCCUPYING
				occupant = board[dest_key]
				#--REMOVE CURRENTPLAYER
				index = gameState[:players][String.to_atom(playerName)][:loc]
				index_atom = String.to_atom(Integer.to_string(index))
				if index != nil and gameState[:board][index_atom] == playerName do
					{_,aMap} = Map.pop(gameState[:board], index_atom)
					gameState = %{gameState|:board=>aMap}	
				end
				#--PUT CURRENTPLAYER
				gameState = putPlayer(gameState,playerName,dst)
				#--MOVE PLAYER
				gameState = movePlayer(gameState,occupant,dst+1)
			end	
		end
		gameState
	end
	def putPlayer(gameState,playerName,dst) do
		dst_atom = String.to_atom(Integer.to_string(dst))
		p_atom = String.to_atom(playerName)
		%{ gameState | 
			:board   => Map.put(gameState[:board],dst_atom,playerName),
			:players => Map.put(gameState[:players],p_atom,%{gameState[:players][p_atom]|:loc =>dst})
		}

	end 
	def print(gameState) do
		board = gameState[:board]
		maxx = gameState[:board_x]
		maxy = gameState[:board_y]
		max_block = maxx*maxy+1
		buffer = ""
		
		bar = Enum.reduce(1..maxx,"+",fn(_,str) -> str<>"---+" end)
		[_,_,buffer] = Enum.reduce(1..maxy,[max_block,0,buffer],fn(y,[block_number,fp,bf])->
			if fp == 1, do: (range = block_number-maxx.. block_number-1; fp = 0), else: (range = block_number-1..block_number - maxx; fp=1) ;
			bf=bf<> bar <> "\n" <> 
				Enum.reduce(range,"|",fn(cell,aStr)-> 
					str_block = Integer.to_string(cell );
					if (String.length(str_block) <3), do: ( str_block = Enum.reduce(String.length(str_block)..2,"",fn(_,acc)->acc <>" " end) <>str_block );
					aStr = aStr <> str_block <> "|"
					end
					) <> "\n" <> Enum.reduce(range,"|",fn(cell,str_block)->
				cell_atom = String.to_atom(Integer.to_string(cell))
				if (board[cell_atom] == nil), do: str_block = str_block <> " ", else: ( str_block = str_block<> board[cell_atom])
				if (gameState[:escalator][cell_atom] != nil), do: ( str_block = str_block<> "e"),else: 
					(if (gameState[:antivenom][cell_atom]!= nil), do: ( str_block = str_block<> "a"),else: (
						if (gameState[:double][cell_atom]!= nil), do: ( str_block = str_block<> "d"),else: ( 
							str_block = str_block <> " "
							)))
				
				if (gameState[:snakes][cell_atom] != nil), do: ( str_block = str_block<> "S"),else: 
					(if (gameState[:ladder][cell_atom]!= nil), do: ( str_block = str_block<> "L"),else: (
							str_block = str_block <> " "
							))
				
				str_block = str_block <> "|"
				end
			)<>"\n"	
			block_number = block_number - maxx ;
			[block_number,fp,bf];
			end
		)
		buffer = buffer <> bar
		if gameState[:victory] == true do
			last_block_atom = (String.to_atom(Integer.to_string(maxx*maxy)))
			buffer = buffer <> "Player " <> gameState[:board][last_block_atom] <> " won"
		end
		buffer
	end
end

Assign2.print(Assign2.readFrom("board 3 4
players 2
dice 1
turns 5"))
